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
    if (file.mimetype === 'application/pdf') {
      cb(null, true);
    } else {
      cb(new Error('Solo se permiten archivos PDF'), false);
    }
  },
  limits: {
    fileSize: 50 * 1024 * 1024 // 50MB máximo
  }
});

/**
 * POST /api/pdf-correction/generate-list
 * Genera un PDF con el contenido original + lista de correcciones
 */
router.post('/generate-list', upload.single('pdf'), async (req, res) => {
  try {
    if (!req.file) {
      return res.status(400).json({
        success: false,
        error: 'No se proporcionó archivo PDF'
      });
    }

    console.log(`[PDF-CORRECTION] Procesando: ${req.file.originalname}`);
    
    const pliegoId = req.body.pliegoId || `PLIEGO_${Date.now()}`;
    const contextId = req.body.contextId || null;

    // 1. Primero ejecutar análisis visual
    console.log(`[PDF-CORRECTION] Ejecutando análisis visual del PDF...`);
    const visualAnalysis = await pdfVisualAnalyzer.analyzeAll(req.file.path);
    
    // 2. Generar reporte de errores visuales para pasarlo a la IA
    const visualReport = pdfVisualAnalyzer.generateVisualErrorsReport(visualAnalysis);
    
    // 3. Ejecutar análisis de IA incluyendo los errores visuales como contexto
    console.log(`[PDF-CORRECTION] Ejecutando análisis de IA con errores visuales como contexto...`);
    const aiResult = await generatePDFWithCorrectionsList(
      req.file.path,
      req.body.customPrompt || null,
      contextId,
      visualReport // Pasar errores visuales a la IA
    );

    // 4. Almacenar errores en la base de datos
    console.log(`[PDF-CORRECTION] Almacenando errores para pliego: ${pliegoId}...`);
    const storeResult = await storeErrorsForPliego(
      pliegoId,
      aiResult.correctionsList,
      {
        fileName: req.file.originalname,
        contextId: contextId
      }
    );
    console.log(`[PDF-CORRECTION] ✅ Almacenados: ${storeResult.criticalErrors} errores críticos, ${storeResult.warnings} advertencias`);

    // 5. El resultado ya incluye los errores visuales procesados por la IA
    const combinedResult = {
      ...aiResult,
      visualAnalysis: visualAnalysis,
      errorStorage: {
        pliegoId: pliegoId,
        stored: true,
        criticalErrors: storeResult.criticalErrors,
        warnings: storeResult.warnings
      }
    };

    // Configurar headers para descarga
    res.set({
      'Content-Type': 'application/pdf',
      'Content-Disposition': `attachment; filename="correcciones-${req.file.originalname}"`,
      'Content-Length': combinedResult.pdfBuffer.length
    });

    // Limpiar archivo temporal
    try {
      await fs.unlink(req.file.path);
    } catch (cleanupError) {
      console.warn('[PDF-CORRECTION] Error limpiando archivo temporal:', cleanupError.message);
    }

    res.send(combinedResult.pdfBuffer);

  } catch (error) {
    console.error('[PDF-CORRECTION] Error en generate-list:', error);
    
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
    endpoints: {
      generateList: 'POST /api/pdf-correction/generate-list',
      generateListFromContext: 'POST /api/pdf-correction/generate-list-from-context',
      applyCorrections: 'POST /api/pdf-correction/apply-corrections',
      generateCorrections: 'POST /api/pdf-correction/generate-corrections',
      testWorkflow: 'POST /api/pdf-correction/test-workflow',
      testAiCore: 'GET /api/pdf-correction/test-ai-core'
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
 */
router.post('/highlight-errors', upload.single('pdf'), async (req, res) => {
  const startTime = Date.now();
  
  try {
    console.log('[PDF-CORRECTION] Iniciando subrayado de errores...');
    
    if (!req.file) {
      return res.status(400).json({
        success: false,
        error: 'No se proporcionó ningún archivo PDF'
      });
    }
    
    const pdfPath = req.file.path;
    const contextId = req.body.contextId || null;
    const pliegoId = req.body.pliegoId || `PLIEGO_${Date.now()}`;
    
    console.log(`[PDF-CORRECTION] Archivo recibido: ${req.file.originalname}`);
    console.log(`[PDF-CORRECTION] Pliego ID: ${pliegoId}`);
    console.log(`[PDF-CORRECTION] Contexto: ${contextId || 'ninguno'}`);
    
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
        fileName: req.file.originalname,
        contextId: contextId
      }
    );
    console.log(`[PDF-CORRECTION] ✅ Almacenados: ${storeResult.criticalErrors} errores críticos, ${storeResult.warnings} advertencias`);
    
    // 3. Parsear errores de la respuesta de la IA para subrayado
    console.log('[PDF-CORRECTION] Paso 3: Parseando errores para subrayado...');
    const errors = parseErrorsFromAIResponse(detectionResult.correctionsList);
    console.log(`[PDF-CORRECTION] ${errors.length} errores parseados`);
    
    // 3. Subrayar errores en el PDF
    console.log('[PDF-CORRECTION] Paso 3: Subrayando errores en PDF...');
    const highlightedPdfBuffer = await highlightErrorsInPDF(pdfPath, errors);
    
    // 4. Guardar PDF subrayado
    const outputFilename = `highlighted-${Date.now()}-${req.file.originalname}`;
    const outputPath = path.join('uploads/pdf-corrections', outputFilename);
    await fs.writeFile(outputPath, highlightedPdfBuffer);
    
    console.log(`[PDF-CORRECTION] PDF subrayado guardado: ${outputPath}`);
    
    // 5. Limpiar archivo original
    try {
      await fs.unlink(pdfPath);
    } catch (cleanupError) {
      console.warn('[PDF-CORRECTION] Error limpiando archivo temporal:', cleanupError);
    }
    
    const processingTime = Date.now() - startTime;
    
    // 6. Devolver PDF subrayado
    res.setHeader('Content-Type', 'application/pdf');
    res.setHeader('Content-Disposition', `attachment; filename="${outputFilename}"`);
    res.send(highlightedPdfBuffer);
    
    console.log(`[PDF-CORRECTION] Subrayado completado en ${processingTime}ms`);
    
    // Limpiar archivo de salida después de enviarlo
    setTimeout(async () => {
      try {
        await fs.unlink(outputPath);
        console.log('[PDF-CORRECTION] Archivo temporal limpiado');
      } catch (cleanupError) {
        console.warn('[PDF-CORRECTION] Error limpiando archivo de salida:', cleanupError);
      }
    }, 5000);
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error en highlight-errors:', error);
    
    // Limpiar archivo temporal en caso de error
    if (req.file?.path) {
      try {
        await fs.unlink(req.file.path);
      } catch (cleanupError) {
        console.warn('[PDF-CORRECTION] Error limpiando archivo temporal:', cleanupError);
      }
    }
    
    res.status(500).json({
      success: false,
      error: error.message,
      message: 'Error subrayando errores en PDF'
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
