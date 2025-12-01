import express from 'express';
import multer from 'multer';
import path from 'path';
import fs from 'fs/promises';
import { 
  generatePDFWithCorrectionsList, 
  generatePDFWithCorrectionsFromContext, 
  applyCorrectionsDirectly, 
  parseCorrections,
  generateCorrections,
  testAiCoreConnection
} from '../services/pdfCorrectionService.js';
import pdfVisualAnalyzer from '../services/pdfVisualAnalyzer.js';
import { highlightErrorsInPDF, parseErrorsFromAIResponse } from '../services/pdfHighlightService.js';
import { storeErrorsForPliego, getErrorsForPliego, generateAggregatedReport, formatAggregatedReport, deleteErrorsForPliego, clearAllErrors } from '../services/pliegoErrorsService.js';
import loggerService from '../services/loggerService.js';
import { convertDocxToPdf, convertDocxBufferToPdf, isDocxFile, detectFileTypeFromBuffer } from '../services/docToPdfConverter.js';

const router = express.Router();

// Configurar multer para subida de archivos
const storage = multer.diskStorage({
  destination: async (req, file, cb) => {
    const uploadDir = 'uploads/pdf-corrections';
    try {
      await fs.mkdir(uploadDir, { recursive: true });
      cb(null, uploadDir);
    } catch (error) {
      cb(error);
    }
  },
  filename: (req, file, cb) => {
    const uniqueSuffix = Date.now() + '-' + Math.round(Math.random() * 1E9);
    cb(null, file.fieldname + '-' + uniqueSuffix + path.extname(file.originalname));
  }
});

const upload = multer({
  storage: storage,
  fileFilter: (req, file, cb) => {
    const isPdf = file.mimetype === 'application/pdf';
    const isDocx = isDocxFile(file.mimetype, file.originalname);
    
    if (isPdf || isDocx) {
      cb(null, true);
    } else {
      cb(new Error('Solo se permiten archivos PDF, DOC o DOCX'), false);
    }
  },
  limits: {
    fileSize: 2048 * 1024 * 1024, // 50MB máximo para archivos
    fieldSize: 2048 * 1024 * 1024  // 50MB máximo para campos de texto (base64)
  }
});

/**
 * POST /api/pdf-correction/generate-list
 * Genera un PDF con el contenido original + lista de correcciones
 * Soporta tres formatos:
 * 1. Multipart/form-data con campo 'pdf' (archivo PDF o DOC/DOCX)
 * 2. JSON con campo 'pdfBase64' (string base64 de PDF o DOC/DOCX)
 * 
 * Los archivos DOC/DOCX se convierten automáticamente a PDF antes de procesarse.
 * La detección del tipo de archivo se hace por magic bytes, no por extensión.
 * 
 * MEJORAS:
 * - Soporte completo para DOC/DOCX (mejor detección de errores en formato Word)
 * - Análisis visual automático del documento
 * - Almacenamiento automático en base de datos de errores
 * - Logging detallado para debugging
 * - Limpieza automática de archivos temporales
 */
router.post('/generate-list', upload.single('pdf'), async (req, res) => {
  const startTime = Date.now();
  let tempFilePath = null;
  let convertedPdfPath = null;
  
  try {
    // Determinar origen del documento: archivo o base64
    let pdfPath;
    let fileName;
    let fileSize = 0;

    if (req.file) {
      // Caso 1: Archivo subido via multipart/form-data
      fileName = req.file.originalname;
      fileSize = req.file.size;
      
      // Verificar si es un archivo DOC/DOCX que necesita conversión
      if (isDocxFile(req.file.mimetype, req.file.originalname)) {
        loggerService.info('PDF-CORRECTION-API', 'Archivo DOC/DOCX recibido', { fileName, size: fileSize });
        console.log(`[PDF-CORRECTION] 📄 Convirtiendo DOC/DOCX a PDF: ${fileName}`);
        
        // Convertir DOCX a PDF
        const pdfBuffer = await convertDocxToPdf(req.file.path);
        
        // Guardar PDF convertido temporalmente
        const uploadDir = 'uploads/pdf-corrections';
        await fs.mkdir(uploadDir, { recursive: true });
        
        const uniqueSuffix = Date.now() + '-' + Math.round(Math.random() * 1E9);
        convertedPdfPath = path.join(uploadDir, `converted-${uniqueSuffix}.pdf`);
        await fs.writeFile(convertedPdfPath, pdfBuffer);
        
        pdfPath = convertedPdfPath;
        fileName = fileName.replace(/\.(docx?|DOCX?)$/, '.pdf');
        
        loggerService.info('PDF-CORRECTION-API', 'DOC/DOCX convertido a PDF', { fileName, size: pdfBuffer.length });
        console.log(`[PDF-CORRECTION] ✅ Conversión completada: ${fileName} (${pdfBuffer.length} bytes)`);
      } else {
        // Es un PDF normal
        pdfPath = req.file.path;
        loggerService.info('PDF-CORRECTION-API', 'Archivo PDF recibido', { fileName, size: fileSize });
        console.log(`[PDF-CORRECTION] 📄 Procesando PDF: ${fileName} (${fileSize} bytes)`);
      }
    } else if (req.body.pdfBase64) {
      // Caso 2: Documento en base64 (PDF o DOC/DOCX)
      console.log(`[PDF-CORRECTION] 📦 Procesando documento desde base64`);
      
      // Crear archivo temporal desde base64
      const uploadDir = 'uploads/pdf-corrections';
      await fs.mkdir(uploadDir, { recursive: true });
      
      const uniqueSuffix = Date.now() + '-' + Math.round(Math.random() * 1E9);
      
      // Decodificar base64
      const fileBuffer = Buffer.from(req.body.pdfBase64, 'base64');
      fileSize = fileBuffer.length;
      
      // Detectar tipo de archivo por magic bytes
      const fileType = detectFileTypeFromBuffer(fileBuffer);
      console.log(`[PDF-CORRECTION] 🔍 Tipo de archivo detectado: ${fileType}`);
      
      if (fileType === 'docx' || fileType === 'doc') {
        // Es un DOC/DOCX, convertir a PDF
        loggerService.info('PDF-CORRECTION-API', 'DOC/DOCX base64 recibido', { size: fileSize });
        console.log(`[PDF-CORRECTION] 📄 Convirtiendo DOC/DOCX base64 a PDF...`);
        
        const pdfBuffer = await convertDocxBufferToPdf(fileBuffer);
        
        // Guardar PDF convertido temporalmente
        convertedPdfPath = path.join(uploadDir, `converted-base64-${uniqueSuffix}.pdf`);
        await fs.writeFile(convertedPdfPath, pdfBuffer);
        
        pdfPath = convertedPdfPath;
        fileName = req.body.fileName ? req.body.fileName.replace(/\.(docx?|DOCX?)$/, '.pdf') : `documento-${uniqueSuffix}.pdf`;
        
        loggerService.info('PDF-CORRECTION-API', 'DOC/DOCX base64 convertido a PDF', { fileName, size: pdfBuffer.length });
        console.log(`[PDF-CORRECTION] ✅ Conversión base64 completada: ${fileName}`);
      } else if (fileType === 'pdf') {
        // Es un PDF normal
        tempFilePath = path.join(uploadDir, `pdf-base64-${uniqueSuffix}.pdf`);
        await fs.writeFile(tempFilePath, fileBuffer);
        
        pdfPath = tempFilePath;
        fileName = req.body.fileName || `documento-${uniqueSuffix}.pdf`;
        loggerService.info('PDF-CORRECTION-API', 'PDF base64 recibido y guardado', { fileName, size: fileSize });
        console.log(`[PDF-CORRECTION] 📄 PDF base64 guardado: ${fileName}`);
      } else {
        // Tipo de archivo no soportado
        throw new Error(`Tipo de archivo no soportado. Se esperaba PDF, DOC o DOCX pero se detectó: ${fileType}`);
      }
    } else {
      loggerService.warn('PDF-CORRECTION-API', 'Request sin archivo');
      return res.status(400).json({
        success: false,
        error: 'Debes proporcionar un archivo PDF/DOC/DOCX (campo "pdf") o un string base64 (campo "pdfBase64")'
      });
    }

    // Extraer parámetros de la solicitud
    const pliegoId = req.body.pliegoId || `PLIEGO_${Date.now()}`;
    const contextId = req.body.contextId || null;
    const customPrompt = req.body.customPrompt || req.body.prompt || null;
    const username = req.username || req.body.username || 'anonymous';
    const skipVisualAnalysis = req.body.skipVisualAnalysis === 'true' || req.body.skipVisualAnalysis === true;

    loggerService.info('PDF-VALIDATION', `Iniciando validación de pliego: ${pliegoId}`, {
      username,
      fileName,
      contextId,
      pliegoId,
      fileSize
    });

    let visualAnalysis = null;
    let visualReport = null;

    // 1. Análisis visual (opcional, puede deshabilitarse para mayor velocidad)
    if (!skipVisualAnalysis) {
      console.log(`[PDF-CORRECTION] 🔍 Ejecutando análisis visual del documento...`);
      try {
        visualAnalysis = await pdfVisualAnalyzer.analyzeAll(pdfPath);
        visualReport = pdfVisualAnalyzer.generateVisualErrorsReport(visualAnalysis);
        console.log(`[PDF-CORRECTION] ✅ Análisis visual completado`);
      } catch (visualError) {
        console.warn(`[PDF-CORRECTION] ⚠️ Error en análisis visual (continuando sin él):`, visualError.message);
        // Continuar sin análisis visual
      }
    } else {
      console.log(`[PDF-CORRECTION] ⏭️ Análisis visual omitido (skipVisualAnalysis=true)`);
    }
    
    // 2. Ejecutar análisis de IA con contexto visual (si existe)
    console.log(`[PDF-CORRECTION] 🤖 Ejecutando análisis de IA...`);
    const aiResult = await generatePDFWithCorrectionsList(
      pdfPath,
      customPrompt,
      contextId,
      visualReport // Pasar errores visuales a la IA (puede ser null)
    );

    if (!aiResult.success) {
      throw new Error('Error en análisis de IA: ' + (aiResult.error || 'Unknown error'));
    }

    // 3. Almacenar errores en la base de datos
    console.log(`[PDF-CORRECTION] 💾 Almacenando errores para pliego: ${pliegoId}...`);
    const storeResult = await storeErrorsForPliego(
      pliegoId,
      aiResult.correctionsList,
      {
        fileName: fileName,
        contextId: contextId,
        username: username,
        fileSize: fileSize
      }
    );
    console.log(`[PDF-CORRECTION] ✅ Almacenados: ${storeResult.criticalErrors} errores críticos, ${storeResult.warnings} advertencias`);

    const processingTime = Date.now() - startTime;

    loggerService.success('PDF-VALIDATION', `Validación completada: ${pliegoId}`, {
      username,
      pliegoId,
      criticalErrors: storeResult.criticalErrors,
      warnings: storeResult.warnings,
      fileName,
      processingTime: `${processingTime}ms`
    });

    // 4. Preparar respuesta con metadata adicional
    const pdfBufferSize = aiResult.pdfBuffer.length;
    
    const combinedResult = {
      ...aiResult,
      visualAnalysis: visualAnalysis,
      errorStorage: {
        pliegoId: pliegoId,
        stored: true,
        criticalErrors: storeResult.criticalErrors,
        warnings: storeResult.warnings
      },
      metadata: {
        fileName: fileName,
        fileSize: fileSize,
        pdfSize: pdfBufferSize,  // Tamaño del PDF generado
        processingTime: processingTime,
        timestamp: new Date().toISOString()
      }
    };

    // Limpiar archivos temporales
    const filesToClean = [
      req.file?.path,
      tempFilePath,
      convertedPdfPath
    ].filter(Boolean);
    
    for (const fileToClean of filesToClean) {
      try {
        await fs.unlink(fileToClean);
        console.log(`[PDF-CORRECTION] 🗑️ Archivo temporal limpiado: ${path.basename(fileToClean)}`);
      } catch (cleanupError) {
        console.warn('[PDF-CORRECTION] ⚠️ Error limpiando archivo temporal:', cleanupError.message);
      }
    }

    console.log(`[PDF-CORRECTION] ✅ Proceso completado en ${processingTime}ms`);
    
    // Configurar headers para descarga (comportamiento original)
    res.set({
      'Content-Type': 'application/pdf',
      'Content-Disposition': `attachment; filename="correcciones-${fileName}"`,
      'Content-Length': pdfBufferSize,
      'fileSize': pdfBufferSize.toString(),  // ← Header fileSize para SAP
      'X-File-Size': pdfBufferSize,
      'X-File-Name': fileName,
      'X-Pliego-Id': pliegoId,
      'X-Critical-Errors': storeResult.criticalErrors,
      'X-Warnings': storeResult.warnings
    });
    
    console.log(`[PDF-CORRECTION] 📄 Enviando PDF directo con header fileSize: ${pdfBufferSize}`);
    res.send(combinedResult.pdfBuffer);

  } catch (error) {
    const processingTime = Date.now() - startTime;
    console.error(`[PDF-CORRECTION] ❌ Error en generate-list (${processingTime}ms):`, error);
    
    loggerService.error('PDF-VALIDATION', 'Error en validación', {
      error: error.message,
      stack: error.stack,
      processingTime: `${processingTime}ms`
    });
    
    // Limpiar archivos temporales en caso de error
    const filesToClean = [
      req.file?.path,
      tempFilePath,
      convertedPdfPath
    ].filter(Boolean);
    
    for (const fileToClean of filesToClean) {
      try {
        await fs.unlink(fileToClean);
      } catch (cleanupError) {
        console.warn('[PDF-CORRECTION] ⚠️ Error limpiando archivo temporal:', cleanupError.message);
      }
    }

    res.status(500).json({
      success: false,
      error: error.message,
      processingTime: processingTime
    });
  }
});

/**
 * POST /api/pdf-correction/apply-corrections
 * Aplica correcciones directamente al PDF usando replace
 */
router.post('/apply-corrections', upload.single('pdf'), async (req, res) => {
  try {
    if (!req.file) {
      return res.status(400).json({
        success: false,
        error: 'No se proporcionó archivo PDF'
      });
    }

    const { corrections } = req.body;
    if (!corrections) {
      return res.status(400).json({
        success: false,
        error: 'No se proporcionaron correcciones'
      });
    }

    console.log(`[PDF-CORRECTION] Aplicando correcciones a: ${req.file.originalname}`);

    // Parsear correcciones si vienen como string
    let parsedCorrections;
    if (typeof corrections === 'string') {
      parsedCorrections = parseCorrections(corrections);
    } else if (Array.isArray(corrections)) {
      parsedCorrections = corrections;
    } else {
      throw new Error('Formato de correcciones inválido');
    }

    const result = await applyCorrectionsDirectly(req.file.path, parsedCorrections);

    // Configurar headers para descarga
    res.set({
      'Content-Type': 'application/pdf',
      'Content-Disposition': `attachment; filename="corregido-${req.file.originalname}"`,
      'Content-Length': result.pdfBuffer.length
    });

    // Limpiar archivo temporal
    try {
      await fs.unlink(req.file.path);
    } catch (cleanupError) {
      console.warn('[PDF-CORRECTION] Error limpiando archivo temporal:', cleanupError.message);
    }

    res.send(result.pdfBuffer);

  } catch (error) {
    console.error('[PDF-CORRECTION] Error en apply-corrections:', error);
    
    // Limpiar archivo temporal en caso de error
    if (req.file?.path) {
      try {
        await fs.unlink(req.file.path);
      } catch (cleanupError) {
        console.warn('[PDF-CORRECTION] Error limpiando archivo temporal:', cleanupError.message);
      }
    }

    res.status(500).json({
      success: false,
      error: error.message
    });
  }
});

/**
 * POST /api/pdf-correction/generate-corrections
 * Genera solo la lista de correcciones sin crear PDF
 */
router.post('/generate-corrections', upload.single('pdf'), async (req, res) => {
  try {
    if (!req.file) {
      return res.status(400).json({
        success: false,
        error: 'No se proporcionó archivo PDF'
      });
    }

    console.log(`[PDF-CORRECTION] Generando correcciones para: ${req.file.originalname}`);

    const result = await generateCorrections(req.file.path);

    // Limpiar archivo temporal
    try {
      await fs.unlink(req.file.path);
    } catch (cleanupError) {
      console.warn('[PDF-CORRECTION] Error limpiando archivo temporal:', cleanupError.message);
    }

    res.json(result);

  } catch (error) {
    console.error('[PDF-CORRECTION] Error en generate-corrections:', error);
    
    // Limpiar archivo temporal en caso de error
    if (req.file?.path) {
      try {
        await fs.unlink(req.file.path);
      } catch (cleanupError) {
        console.warn('[PDF-CORRECTION] Error limpiando archivo temporal:', cleanupError.message);
      }
    }

    res.status(500).json({
      success: false,
      error: error.message
    });
  }
});

/**
 * POST /api/pdf-correction/test-workflow
 * Endpoint de prueba para el flujo completo
 */
router.post('/test-workflow', upload.single('pdf'), async (req, res) => {
  try {
    if (!req.file) {
      return res.status(400).json({
        success: false,
        error: 'No se proporcionó archivo PDF'
      });
    }

    const { workflow = 'list' } = req.body;
    console.log(`[PDF-CORRECTION] Flujo de prueba '${workflow}' para: ${req.file.originalname}`);

    let result;

    if (workflow === 'list') {
      // Generar PDF con lista de correcciones
      result = await generatePDFWithCorrectionsList(req.file.path);
      
      res.set({
        'Content-Type': 'application/pdf',
        'Content-Disposition': `attachment; filename="correcciones-${req.file.originalname}"`,
        'Content-Length': result.pdfBuffer.length
      });

      // Limpiar archivo temporal
      try {
        await fs.unlink(req.file.path);
      } catch (cleanupError) {
        console.warn('[PDF-CORRECTION] Error limpiando archivo temporal:', cleanupError.message);
      }

      res.send(result.pdfBuffer);

    } else if (workflow === 'apply') {
      // Generar correcciones y aplicarlas
      const corrections = await generateCorrections(req.file.path);
      
      if (corrections.corrections.length === 0) {
        // Limpiar archivo temporal
        try {
          await fs.unlink(req.file.path);
        } catch (cleanupError) {
          console.warn('[PDF-CORRECTION] Error limpiando archivo temporal:', cleanupError.message);
        }

        return res.json({
          success: true,
          message: 'No se encontraron errores ortográficos',
          corrections: corrections.corrections
        });
      }

      result = await applyCorrectionsDirectly(req.file.path, corrections.corrections);
      
      res.set({
        'Content-Type': 'application/pdf',
        'Content-Disposition': `attachment; filename="corregido-${req.file.originalname}"`,
        'Content-Length': result.pdfBuffer.length
      });

      // Limpiar archivo temporal
      try {
        await fs.unlink(req.file.path);
      } catch (cleanupError) {
        console.warn('[PDF-CORRECTION] Error limpiando archivo temporal:', cleanupError.message);
      }

      res.send(result.pdfBuffer);

    } else {
      throw new Error(`Flujo de trabajo desconocido: ${workflow}`);
    }

  } catch (error) {
    console.error('[PDF-CORRECTION] Error en test-workflow:', error);
    
    // Limpiar archivo temporal en caso de error
    if (req.file?.path) {
      try {
        await fs.unlink(req.file.path);
      } catch (cleanupError) {
        console.warn('[PDF-CORRECTION] Error limpiando archivo temporal:', cleanupError.message);
      }
    }

    res.status(500).json({
      success: false,
      error: error.message
    });
  }
});

/**
 * GET /api/pdf-correction/health
 * Endpoint de salud para el servicio de corrección
 */
router.get('/health', (req, res) => {
  res.json({
    service: 'PDF Correction Service',
    status: 'healthy',
    timestamp: new Date().toISOString(),
    supportedFormats: ['PDF', 'DOC', 'DOCX'],
    features: [
      'Conversión automática DOC/DOCX a PDF',
      'Análisis visual de documentos',
      'Detección de errores con IA',
      'Subrayado de errores en amarillo',
      'Almacenamiento en base de datos',
      'Generación de reportes'
    ],
    endpoints: {
      generateList: {
        method: 'POST',
        path: '/api/pdf-correction/generate-list',
        description: 'Genera PDF con lista de correcciones (soporta PDF/DOC/DOCX)',
        params: {
          pdf: 'Archivo (multipart) o pdfBase64 (string)',
          pliegoId: 'ID del pliego (opcional)',
          contextId: 'ID del contexto RAG (opcional)',
          customPrompt: 'Prompt personalizado (opcional)',
          skipVisualAnalysis: 'Omitir análisis visual para mayor velocidad (opcional)'
        },
        response: {
          type: 'Binary PDF (application/pdf)',
          headers: {
            'Content-Type': 'application/pdf',
            'Content-Length': 'number (tamaño del PDF)',
            'fileSize': 'number (tamaño en bytes - para SAP)',
            'X-File-Size': 'number (tamaño en bytes)',
            'X-File-Name': 'string (nombre del archivo)',
            'X-Pliego-Id': 'string (ID del pliego)',
            'X-Critical-Errors': 'number (errores críticos)',
            'X-Warnings': 'number (advertencias)'
          }
        }
      },
      highlightErrors: {
        method: 'POST',
        path: '/api/pdf-correction/highlight-errors',
        description: 'Subraya errores en amarillo (soporta PDF/DOC/DOCX)',
        params: {
          pdf: 'Archivo (multipart)',
          pliegoId: 'ID del pliego (opcional)',
          contextId: 'ID del contexto RAG (opcional)'
        }
      },
      generateListFromContext: 'POST /api/pdf-correction/generate-list-from-context',
      applyCorrections: 'POST /api/pdf-correction/apply-corrections',
      generateCorrections: 'POST /api/pdf-correction/generate-corrections',
      testWorkflow: 'POST /api/pdf-correction/test-workflow',
      testAiCore: 'GET /api/pdf-correction/test-ai-core',
      getErrors: 'GET /api/pdf-correction/errors/:pliegoId',
      deleteErrors: 'DELETE /api/pdf-correction/errors/:pliegoId',
      clearAllErrors: 'DELETE /api/pdf-correction/errors',
      getRawDatabase: 'GET /api/pdf-correction/errors/database/raw',
      aggregatedReport: 'GET /api/pdf-correction/report/aggregated?lastN=5'
    }
  });
});

/**
 * POST /api/pdf-correction/generate-list-from-context
 * Genera un PDF con lista de correcciones basado en contexto RAG
 */
router.post('/generate-list-from-context', async (req, res) => {
  try {
    const { prompt, contextId, pliegoId } = req.body;

    if (!prompt || !contextId) {
      return res.status(400).json({
        success: false,
        error: 'Se requieren prompt y contextId'
      });
    }

    console.log(`[PDF-CORRECTION] Generando lista desde contexto: ${contextId}`);
    
    const generatedPliegoId = pliegoId || `PLIEGO_CONTEXT_${Date.now()}`;

    const result = await generatePDFWithCorrectionsFromContext(
      prompt,
      contextId
    );

    // Almacenar errores en la base de datos si hay correctionsList
    if (result.correctionsList) {
      console.log(`[PDF-CORRECTION] Almacenando errores para pliego: ${generatedPliegoId}...`);
      const storeResult = await storeErrorsForPliego(
        generatedPliegoId,
        result.correctionsList,
        {
          fileName: `contexto-${contextId}`,
          contextId: contextId
        }
      );
      console.log(`[PDF-CORRECTION] ✅ Almacenados: ${storeResult.criticalErrors} errores críticos, ${storeResult.warnings} advertencias`);
    }

    // Configurar headers para descarga
    res.set({
      'Content-Type': 'application/pdf',
      'Content-Disposition': `attachment; filename="correcciones-contexto-${contextId}.pdf"`,
      'Content-Length': result.pdfBuffer.length
    });

    res.send(result.pdfBuffer);

  } catch (error) {
    console.error('[PDF-CORRECTION] Error en generate-list-from-context:', error);

    res.status(500).json({
      success: false,
      error: error.message
    });
  }
});
router.get('/test-ai-core', async (req, res) => {
  try {
    console.log('[PDF-CORRECTION] Probando conexión SAP AI Core...');
    
    const result = await testAiCoreConnection();
    
    if (result.success) {
      res.json({
        success: true,
        message: 'Conexión con SAP AI Core exitosa',
        ...result
      });
    } else {
      res.status(500).json({
        success: false,
        message: 'Error conectando con SAP AI Core',
        ...result
      });
    }
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error en test-ai-core:', error);
    res.status(500).json({
      success: false,
      error: error.message,
      message: 'Error interno probando SAP AI Core'
    });
  }
});

/**
 * POST /api/pdf-correction/highlight-errors
 * Subraya errores detectados en amarillo sobre el PDF original
 * Soporta archivos PDF, DOC y DOCX (se convierten automáticamente)
 */
router.post('/highlight-errors', upload.single('pdf'), async (req, res) => {
  const startTime = Date.now();
  let convertedPdfPath = null;
  
  try {
    console.log('[PDF-CORRECTION] Iniciando subrayado de errores...');
    
    if (!req.file) {
      return res.status(400).json({
        success: false,
        error: 'No se proporcionó ningún archivo (PDF/DOC/DOCX)'
      });
    }
    
    let pdfPath;
    let fileName = req.file.originalname;
    const contextId = req.body.contextId || null;
    const pliegoId = req.body.pliegoId || `PLIEGO_${Date.now()}`;
    
    console.log(`[PDF-CORRECTION] Archivo recibido: ${fileName}`);
    console.log(`[PDF-CORRECTION] Pliego ID: ${pliegoId}`);
    console.log(`[PDF-CORRECTION] Contexto: ${contextId || 'ninguno'}`);
    
    // Verificar si es un archivo DOC/DOCX que necesita conversión
    if (isDocxFile(req.file.mimetype, req.file.originalname)) {
      console.log(`[PDF-CORRECTION] Archivo DOC/DOCX detectado, convirtiendo a PDF...`);
      
      // Convertir DOCX a PDF
      const pdfBuffer = await convertDocxToPdf(req.file.path);
      
      // Guardar PDF convertido temporalmente
      const uploadDir = 'uploads/pdf-corrections';
      await fs.mkdir(uploadDir, { recursive: true });
      
      const uniqueSuffix = Date.now() + '-' + Math.round(Math.random() * 1E9);
      convertedPdfPath = path.join(uploadDir, `converted-highlight-${uniqueSuffix}.pdf`);
      await fs.writeFile(convertedPdfPath, pdfBuffer);
      
      pdfPath = convertedPdfPath;
      fileName = fileName.replace(/\.(docx?|DOCX?)$/, '.pdf');
      
      console.log(`[PDF-CORRECTION] ✅ Conversión completada: ${fileName}`);
    } else {
      // Es un PDF normal
      pdfPath = req.file.path;
    }
    
    // 1. Primero, detectar errores con la IA (reutilizamos la función existente)
    console.log('[PDF-CORRECTION] Paso 1: Detectando errores con IA...');
    const detectionResult = await generatePDFWithCorrectionsList(
      pdfPath,
      null, // customPrompt
      contextId,
      null  // visualErrors (opcional)
    );
    
    if (!detectionResult.success) {
      throw new Error('Error detectando errores con la IA');
    }
    
    console.log('[PDF-CORRECTION] Errores detectados por la IA');
    
    // 2. Almacenar errores en la base de datos
    console.log('[PDF-CORRECTION] Paso 2: Almacenando errores...');
    const storeResult = await storeErrorsForPliego(
      pliegoId,
      detectionResult.correctionsList,
      {
        fileName: fileName,
        contextId: contextId
      }
    );
    console.log(`[PDF-CORRECTION] ✅ Almacenados: ${storeResult.criticalErrors} errores críticos, ${storeResult.warnings} advertencias`);
    
    // 3. Parsear errores de la respuesta de la IA para subrayado
    console.log('[PDF-CORRECTION] Paso 3: Parseando errores para subrayado...');
    const errors = parseErrorsFromAIResponse(detectionResult.correctionsList);
    console.log(`[PDF-CORRECTION] ${errors.length} errores parseados`);
    
    // 4. Subrayar errores en el PDF
    console.log('[PDF-CORRECTION] Paso 4: Subrayando errores en PDF...');
    const highlightedPdfBuffer = await highlightErrorsInPDF(pdfPath, errors);
    
    // 5. Guardar PDF subrayado
    const outputFilename = `highlighted-${Date.now()}-${fileName}`;
    const outputPath = path.join('uploads/pdf-corrections', outputFilename);
    await fs.writeFile(outputPath, highlightedPdfBuffer);
    
    console.log(`[PDF-CORRECTION] PDF subrayado guardado: ${outputPath}`);
    
    // 6. Limpiar archivos temporales
    const filesToClean = [
      req.file?.path,
      convertedPdfPath
    ].filter(Boolean);
    
    for (const fileToClean of filesToClean) {
      try {
        await fs.unlink(fileToClean);
        console.log(`[PDF-CORRECTION] Archivo temporal limpiado: ${fileToClean}`);
      } catch (cleanupError) {
        console.warn('[PDF-CORRECTION] Error limpiando archivo temporal:', cleanupError.message);
      }
    }
    
    const processingTime = Date.now() - startTime;
    
    // 7. Devolver PDF subrayado
    res.setHeader('Content-Type', 'application/pdf');
    res.setHeader('Content-Disposition', `attachment; filename="${outputFilename}"`);
    res.send(highlightedPdfBuffer);
    
    console.log(`[PDF-CORRECTION] ✅ Subrayado completado en ${processingTime}ms`);
    
    // Limpiar archivo de salida después de enviarlo
    setTimeout(async () => {
      try {
        await fs.unlink(outputPath);
        console.log('[PDF-CORRECTION] Archivo de salida limpiado');
      } catch (cleanupError) {
        console.warn('[PDF-CORRECTION] Error limpiando archivo de salida:', cleanupError);
      }
    }, 5000);
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error en highlight-errors:', error);
    
    // Limpiar archivos temporales en caso de error
    const filesToClean = [
      req.file?.path,
      convertedPdfPath
    ].filter(Boolean);
    
    for (const fileToClean of filesToClean) {
      try {
        await fs.unlink(fileToClean);
      } catch (cleanupError) {
        console.warn('[PDF-CORRECTION] Error limpiando archivo temporal:', cleanupError);
      }
    }
    
    res.status(500).json({
      success: false,
      error: error.message,
      message: 'Error subrayando errores en documento'
    });
  }
});

/**
 * GET /api/pdf-correction/errors/:pliegoId
 * Obtiene errores de un pliego específico
 */
router.get('/errors/:pliegoId', async (req, res) => {
  try {
    const { pliegoId } = req.params;
    
    console.log(`[PDF-CORRECTION] Obteniendo errores para pliego: ${pliegoId}`);
    
    const errors = await getErrorsForPliego(pliegoId);
    
    if (!errors) {
      return res.status(404).json({
        success: false,
        error: `No se encontraron errores para el pliego: ${pliegoId}`
      });
    }
    
    res.json({
      success: true,
      pliegoId,
      ...errors
    });
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error obteniendo errores:', error);
    res.status(500).json({
      success: false,
      error: 'Error obteniendo errores del pliego',
      message: error.message
    });
  }
});

/**
 * GET /api/pdf-correction/report/aggregated
 * Genera reporte agregado de los últimos N pliegos
 * Query params: lastN (default: 5)
 */
router.get('/report/aggregated', async (req, res) => {
  try {
    const lastN = parseInt(req.query.lastN) || 5;
    
    console.log(`[PDF-CORRECTION] Generando reporte agregado de últimos ${lastN} pliegos...`);
    
    const report = await generateAggregatedReport(lastN);
    
    res.json(report);
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error generando reporte:', error);
    res.status(500).json({
      success: false,
      error: 'Error generando reporte agregado',
      message: error.message
    });
  }
});

/**
 * GET /api/pdf-correction/report/aggregated/text
 * Genera reporte agregado en formato texto
 * Query params: lastN (default: 5)
 */
router.get('/report/aggregated/text', async (req, res) => {
  try {
    const lastN = parseInt(req.query.lastN) || 5;
    
    console.log(`[PDF-CORRECTION] Generando reporte de texto de últimos ${lastN} pliegos...`);
    
    const report = await generateAggregatedReport(lastN);
    const formattedText = formatAggregatedReport(report);
    
    res.setHeader('Content-Type', 'text/plain; charset=utf-8');
    res.send(formattedText);
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error generando reporte de texto:', error);
    res.status(500).json({
      success: false,
      error: 'Error generando reporte de texto',
      message: error.message
    });
  }
});

/**
 * DELETE /api/pdf-correction/errors/:pliegoId
 * Elimina errores de un pliego específico
 */
router.delete('/errors/:pliegoId', async (req, res) => {
  try {
    const { pliegoId } = req.params;
    
    console.log(`[PDF-CORRECTION] Eliminando errores del pliego: ${pliegoId}`);
    
    const result = await deleteErrorsForPliego(pliegoId);
    
    res.json({
      success: true,
      message: `Errores del pliego ${pliegoId} eliminados correctamente`
    });
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error eliminando errores:', error);
    res.status(500).json({
      success: false,
      error: 'Error eliminando errores del pliego',
      message: error.message
    });
  }
});

/**
 * DELETE /api/pdf-correction/errors
 * Limpia toda la base de datos de errores
 */
router.delete('/errors', async (req, res) => {
  try {
    console.log('[PDF-CORRECTION] Limpiando toda la base de datos de errores...');
    
    const result = await clearAllErrors();
    
    res.json({
      success: true,
      message: 'Base de datos de errores limpiada correctamente'
    });
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error limpiando base de datos:', error);
    res.status(500).json({
      success: false,
      error: 'Error limpiando base de datos de errores',
      message: error.message
    });
  }
});

/**
 * GET /api/pdf-correction/errors/database/raw
 * Visualiza el contenido completo del archivo pliego_errors.json
 */
router.get('/errors/database/raw', async (req, res) => {
  try {
    console.log('[PDF-CORRECTION] Obteniendo base de datos completa...');
    
    const dbPath = path.join(process.cwd(), 'data', 'pliego_errors.json');
    
    // Verificar si existe el archivo
    try {
      await fs.access(dbPath);
    } catch {
      return res.json({
        success: true,
        message: 'Base de datos vacía (archivo no existe aún)',
        data: {},
        totalPliegos: 0
      });
    }
    
    // Leer el archivo
    const rawData = await fs.readFile(dbPath, 'utf-8');
    const database = JSON.parse(rawData);
    
    const totalPliegos = Object.keys(database).length;
    
    res.json({
      success: true,
      totalPliegos,
      data: database
    });
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error leyendo base de datos:', error);
    res.status(500).json({
      success: false,
      error: 'Error leyendo base de datos',
      message: error.message
    });
  }
});

export default router;
