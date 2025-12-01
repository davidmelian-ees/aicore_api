import express from "express";
import multer from "multer";
import path from "path";
import fs from "fs/promises";
import { 
  indexDocument, 
  searchContext, 
  generateRAGResponse,
  listDocuments,
  getDocumentInfo,
  deleteDocument,
  getRAGStats,
  clearRAGIndex,
  createContext,
  listContexts,
  getContextInfo,
  deleteContext,
  processPliegoWithPrompt
} from "../services/ragService.js";
import { processDocument } from '../services/documentProcessor.js';
import { persistenceManager } from '../services/persistenceManager.js';
import { backupService } from '../services/backupService.js';
import { logoDetectionService } from '../services/logoDetectionService.js';

const router = express.Router();

// Configurar multer para subida de archivos
const storage = multer.diskStorage({
  destination: async (req, file, cb) => {
    const uploadDir = './uploads';
    try {
      await fs.mkdir(uploadDir, { recursive: true });
      cb(null, uploadDir);
    } catch (error) {
      cb(error);
    }
  },
  filename: (req, file, cb) => {
    const uniqueSuffix = Date.now() + '-' + Math.round(Math.random() * 1E9);
    const sanitizedName = file.originalname.replace(/[^a-zA-Z0-9.-]/g, '_');
    cb(null, `${uniqueSuffix}-${sanitizedName}`);
  }
});

const upload = multer({ 
  storage,
  limits: {
    fileSize: 50 * 1024 * 1024 // 50MB límite
  },
  fileFilter: (req, file, cb) => {
    // Tipos de archivo permitidos
    const allowedTypes = [
      'text/plain',
      'application/vnd.openxmlformats-officedocument.wordprocessingml.document', // .docx
      'application/msword', // .doc
      'application/vnd.ms-word', // .doc (alternativo)
      'text/markdown',
      'application/json',
      'text/csv',
      'application/pdf',
      'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', // .xlsx
      'application/vnd.ms-excel' // .xls
    ];
    
    const allowedExtensions = ['.txt', '.doc', '.docx', '.md', '.json', '.csv', '.pdf', '.xlsx', '.xls'];
    
    const fileExtension = path.extname(file.originalname).toLowerCase();
    
    if (allowedTypes.includes(file.mimetype) || allowedExtensions.includes(fileExtension)) {
      cb(null, true);
    } else {
      cb(new Error(`Tipo de archivo no soportado: ${file.mimetype}. Permitidos: txt, doc, docx, md, json, csv, pdf, xlsx, xls`));
    }
  }
});

// Configuración específica para archivos de base de datos
const uploadDB = multer({ 
  storage,
  limits: {
    fileSize: 500 * 1024 * 1024 // 500MB límite para bases de datos (aumentado para backups grandes)
  },
  fileFilter: (req, file, cb) => {
    // Permitir archivos .db y SQLite
    const allowedTypes = [
      'application/octet-stream',
      'application/x-sqlite3',
      'application/vnd.sqlite3'
    ];

    const allowedExtensions = ['.db', '.sqlite', '.sqlite3'];
    
    const fileExtension = path.extname(file.originalname).toLowerCase();
    
    if (allowedTypes.includes(file.mimetype) || allowedExtensions.includes(fileExtension)) {
      cb(null, true);
    } else {
      cb(new Error(`Tipo de archivo no soportado para base de datos: ${file.mimetype}. Permitidos: .db, .sqlite, .sqlite3`));
    }
  }
});

// Configuración específica para procesamiento de pliegos (solo PDFs)
const uploadPliego = multer({ 
  storage,
  limits: {
    fileSize: 100 * 1024 * 1024 // 100MB límite para pliegos
  },
  fileFilter: (req, file, cb) => {
    const allowedTypes = ['application/pdf'];
    const ext = path.extname(file.originalname).toLowerCase();
    
    if (allowedTypes.includes(file.mimetype) || ext === '.pdf') {
      cb(null, true);
    } else {
      cb(new Error(`Solo se permiten archivos PDF para pliegos. Recibido: ${file.mimetype}`));
    }
  }
});

/**
 * POST /api/rag/contexts
 * Crea un nuevo contexto
 */
router.post("/contexts", async (req, res) => {
  try {
    const { name, description } = req.body;

    if (!name || typeof name !== 'string' || name.trim().length === 0) {
      return res.status(400).json({ 
        success: false,
        error: "El nombre del contexto es requerido" 
      });
    }

    const context = await createContext(name.trim(), description?.trim());

    res.json({
      success: true,
      message: "Contexto creado exitosamente",
      context
    });

  } catch (error) {
    console.error("[RAG API] Error en /contexts:", error);
    res.status(500).json({ 
      success: false,
      error: "Error creando contexto",
      details: error.message 
    });
  }
});

/**
 * GET /api/rag/contexts
 * Lista todos los contextos
 */
router.get("/contexts", async (req, res) => {
  try {
    const contexts = await listContexts();

    res.json({
      success: true,
      contexts,
      count: contexts.length
    });

  } catch (error) {
    console.error("[RAG API] Error en /contexts:", error);
    res.status(500).json({ 
      success: false,
      error: "Error listando contextos",
      details: error.message 
    });
  }
});

/**
 * GET /api/rag/contexts/:contextId
 * Obtiene información de un contexto específico
 */
router.get("/contexts/:contextId", async (req, res) => {
  try {
    const { contextId } = req.params;
    const context = await getContextInfo(contextId);

    if (!context) {
      return res.status(404).json({
        success: false,
        error: "Contexto no encontrado"
      });
    }

    res.json({
      success: true,
      context
    });

  } catch (error) {
    console.error("[RAG API] Error en /contexts/:id:", error);
    res.status(500).json({ 
      success: false,
      error: "Error obteniendo contexto",
      details: error.message 
    });
  }
});

/**
 * DELETE /api/rag/contexts/:contextId
 * Elimina un contexto y todos sus documentos
 */
router.delete("/contexts/:contextId", async (req, res) => {
  try {
    const { contextId } = req.params;
    const result = await deleteContext(contextId);

    if (!result.deleted) {
      return res.status(404).json({
        success: false,
        error: "Contexto no encontrado"
      });
    }

    res.json({
      success: true,
      message: "Contexto eliminado exitosamente",
      result
    });

  } catch (error) {
    console.error("[RAG API] Error en DELETE /contexts:", error);
    res.status(500).json({ 
      success: false,
      error: "Error eliminando contexto",
      details: error.message 
    });
  }
});

/**
 * POST /api/rag/upload
 * Sube y indexa un documento usando SAP AI Core
 */
router.post("/upload", upload.single('document'), async (req, res) => {
  try {
    if (!req.file) {
      return res.status(400).json({ 
        success: false,
        error: "No se proporcionó ningún archivo" 
      });
    }

    console.log(`[RAG API] Procesando archivo: ${req.file.originalname}`);

    // Detectar logos si es un PDF
    let logoDetection = null;
    let logoReport = null;
    let logoDescription = null;
    
    if (req.file.mimetype === 'application/pdf' || path.extname(req.file.originalname).toLowerCase() === '.pdf') {
      console.log('[RAG API] 🔍 Detectando logos en PDF...');
      
      try {
        logoDetection = await logoDetectionService.detectLogosInPDF(req.file.path);
        logoReport = logoDetectionService.generateLogoValidationReport(logoDetection, req.file.originalname);
        logoDescription = logoDetectionService.generateLogoDescription(logoDetection, req.file.originalname);
        
        console.log(`[RAG API] Logo obligatorio: ${logoDetection.analysis.hasRequiredLogo ? '✅ SÍ' : '❌ NO'}`);
      } catch (logoError) {
        console.warn('[RAG API] ⚠️ Error detectando logos:', logoError.message);
        // Continuar sin detección de logos
      }
    }

    // Preparar metadatos enriquecidos con información de logos
    const metadata = {
      originalName: req.file.originalname,
      uploadedBy: req.body.uploadedBy || 'anonymous',
      uploadedAt: new Date().toISOString(),
      tags: req.body.tags ? req.body.tags.split(',').map(t => t.trim()) : [],
      contextId: req.body.contextId || 'default',
      // Información de logos
      hasLogos: logoDetection?.hasLogos || false,
      hasRequiredLogo: logoDetection?.analysis.hasRequiredLogo || false,
      logoConfidence: logoDetection?.analysis.confidence || 'unknown',
      totalImages: logoDetection?.totalImages || 0,
      headerImages: logoDetection?.headerImages?.length || 0,
      footerImages: logoDetection?.footerImages?.length || 0
    };

    // Si es PDF con detección de logos, crear archivo temporal con texto + análisis
    let filePathToIndex = req.file.path;
    
    if (logoDescription && req.file.mimetype === 'application/pdf') {
      // Crear archivo de texto temporal con el análisis de logos
      // Este texto se añadirá al contenido del PDF para que la IA lo aprenda
      const tempTextPath = req.file.path + '_logo_analysis.txt';
      
      try {
        // Escribir el análisis de logos en un archivo temporal
        await fs.writeFile(tempTextPath, logoDescription, 'utf-8');
        
        // Añadir referencia al análisis en metadatos
        metadata.logoAnalysisFile = tempTextPath;
        metadata.logoAnalysis = logoDescription;
        
        console.log('[RAG API] 📝 Análisis de logos guardado para indexación');
      } catch (writeError) {
        console.warn('[RAG API] ⚠️ No se pudo guardar análisis de logos:', writeError.message);
      }
    }

    // Indexar el documento usando SAP AI Core
    const result = await indexDocument(
      filePathToIndex,
      req.file.mimetype,
      metadata
    );

    // Opcional: eliminar el archivo temporal después de indexarlo
    try {
      await fs.unlink(req.file.path);
    } catch (unlinkError) {
      console.warn(`[RAG API] No se pudo eliminar archivo temporal: ${unlinkError.message}`);
    }

    res.json({
      success: true,
      message: "Documento indexado exitosamente con SAP AI Core",
      document: result,
      logoDetection: logoReport // Incluir reporte de logos en la respuesta
    });

  } catch (error) {
    console.error("[RAG API] Error en /upload:", error);
    
    // Limpiar archivo en caso de error
    if (req.file?.path) {
      try {
        await fs.unlink(req.file.path);
      } catch (unlinkError) {
        console.warn(`[RAG API] No se pudo limpiar archivo tras error: ${unlinkError.message}`);
      }
    }
    
    res.status(500).json({ 
      success: false,
      error: "Error procesando el documento",
      details: error.message 
    });
  }
});

/**
 * POST /api/rag/process-pliego
 * Procesa un pliego PDF con un prompt personalizado sin almacenarlo
 */
router.post("/process-pliego", uploadPliego.single('pliego'), async (req, res) => {
  try {
    if (!req.file) {
      return res.status(400).json({ 
        success: false,
        error: "No se proporcionó ningún archivo PDF" 
      });
    }

    const { prompt } = req.body;
    
    if (!prompt || typeof prompt !== 'string' || prompt.trim().length === 0) {
      return res.status(400).json({ 
        success: false,
        error: "El prompt personalizado es requerido" 
      });
    }

    console.log(`[RAG API] Procesando pliego: ${req.file.originalname} con prompt personalizado`);

    // Procesar el pliego con el prompt personalizado
    const result = await processPliegoWithPrompt(
      req.file.path,
      prompt.trim(),
      {
        originalName: req.file.originalname,
        fileSize: req.file.size,
        uploadedAt: new Date().toISOString()
      }
    );

    // Limpiar el archivo temporal inmediatamente
    try {
      await fs.unlink(req.file.path);
      console.log(`[RAG API] Archivo temporal eliminado: ${req.file.path}`);
    } catch (unlinkError) {
      console.warn(`[RAG API] No se pudo eliminar archivo temporal: ${unlinkError.message}`);
    }

    const response = {
      success: true,
      message: "Pliego procesado exitosamente",
      analysis: result.analysis,
      metadata: result.metadata
    };

    // Incluir texto corregido si está disponible
    if (result.correctedText) {
      response.correctedText = result.correctedText;
      response.message = "Pliego procesado exitosamente con correcciones ortográficas";
    }

    // Incluir PDF corregido como base64 si está disponible
    if (result.correctedPdfBuffer) {
      // Asegurar que es un Buffer y convertir a base64
      const pdfBuffer = Buffer.isBuffer(result.correctedPdfBuffer) 
        ? result.correctedPdfBuffer 
        : Buffer.from(result.correctedPdfBuffer);
        
      response.correctedPdf = {
        data: pdfBuffer.toString('base64'),
        filename: `${result.metadata.fileName.replace('.pdf', '')}_corregido.pdf`,
        mimeType: 'application/pdf',
        size: pdfBuffer.length
      };
      response.message = "Pliego procesado exitosamente con correcciones ortográficas y PDF corregido";
    }

    res.json(response);

  } catch (error) {
    console.error("[RAG API] Error en /process-pliego:", error);
    
    // Limpiar archivo en caso de error
    if (req.file?.path) {
      try {
        await fs.unlink(req.file.path);
      } catch (unlinkError) {
        console.warn(`[RAG API] No se pudo limpiar archivo tras error: ${unlinkError.message}`);
      }
    }
    
    res.status(500).json({ 
      success: false,
      error: "Error procesando el pliego",
      details: error.message 
    });
  }
});

/**
 * POST /api/rag/chat
 * Chat con contexto RAG usando SAP AI Core
 */
router.post("/chat", async (req, res) => {
  try {
    const { 
      message, 
      topK = 5, 
      includeContext = true,
      documentId = null,
      contextId = 'default',
      model = "gpt-4o"
    } = req.body;

    if (!message || typeof message !== 'string' || message.trim().length === 0) {
      return res.status(400).json({ 
        success: false,
        error: "El mensaje es requerido y debe ser un texto válido" 
      });
    }

    console.log(`[RAG API] Consulta RAG: "${message.substring(0, 100)}..."`);

    // Generar respuesta con RAG usando SAP AI Core
    const result = await generateRAGResponse(message, {
      topK: Math.min(Math.max(topK, 1), 20), // Limitar entre 1 y 20
      includeContext,
      documentId,
      contextId,
      model
    });

    res.json({
      success: true,
      query: message,
      answer: result.answer,
      context: result.context,
      metadata: result.metadata
    });

  } catch (error) {
    console.error("[RAG API] Error en /chat:", error);
    res.status(500).json({ 
      success: false,
      error: "Error generando respuesta",
      details: error.message 
    });
  }
});

/**
 * POST /api/rag/search
 * Busca contexto relevante sin generar respuesta
 */
router.post("/search", async (req, res) => {
  try {
    const { 
      query, 
      topK = 5,
      documentId = null,
      contextId = 'default',
      minSimilarity = 0.1
    } = req.body;

    if (!query || typeof query !== 'string' || query.trim().length === 0) {
      return res.status(400).json({ 
        success: false,
        error: "La consulta es requerida y debe ser un texto válido" 
      });
    }

    const results = await searchContext(query, {
      topK: Math.min(Math.max(topK, 1), 50),
      documentId,
      contextId,
      minSimilarity: Math.max(minSimilarity, 0)
    });

    res.json({
      success: true,
      query,
      results,
      count: results.length,
      metadata: {
        searchedAt: new Date().toISOString(),
        parameters: { topK, documentId, minSimilarity }
      }
    });

  } catch (error) {
    console.error("[RAG API] Error en /search:", error);
    res.status(500).json({ 
      success: false,
      error: "Error buscando contexto",
      details: error.message 
    });
  }
});

/**
 * GET /api/rag/documents
 * Lista todos los documentos indexados (opcionalmente filtrados por contexto)
 */
router.get("/documents", async (req, res) => {
  try {
    const { contextId } = req.query;
    const documents = await listDocuments(contextId);

    res.json({
      success: true,
      documents,
      count: documents.length,
      contextId: contextId || 'all',
      retrievedAt: new Date().toISOString()
    });

  } catch (error) {
    console.error("[RAG API] Error en /documents:", error);
    res.status(500).json({ 
      success: false,
      error: "Error listando documentos",
      details: error.message 
    });
  }
});

/**
 * GET /api/rag/documents/:documentId
 * Obtiene información detallada de un documento específico
 */
router.get("/documents/:documentId", async (req, res) => {
  try {
    const { documentId } = req.params;

    if (!documentId) {
      return res.status(400).json({
        success: false,
        error: "ID de documento requerido"
      });
    }

    const documentInfo = await getDocumentInfo(documentId);

    if (!documentInfo) {
      return res.status(404).json({
        success: false,
        error: "Documento no encontrado",
        documentId
      });
    }

    res.json({
      success: true,
      document: documentInfo
    });

  } catch (error) {
    console.error("[RAG API] Error en GET /documents/:id:", error);
    res.status(500).json({ 
      success: false,
      error: "Error obteniendo información del documento",
      details: error.message 
    });
  }
});

/**
 * DELETE /api/rag/documents/:documentId
 * Elimina un documento del índice
 */
router.delete("/documents/:documentId", async (req, res) => {
  try {
    const { documentId } = req.params;

    if (!documentId) {
      return res.status(400).json({
        success: false,
        error: "ID de documento requerido"
      });
    }

    const result = await deleteDocument(documentId);

    if (result.deleted) {
      res.json({
        success: true,
        message: "Documento eliminado exitosamente",
        result
      });
    } else {
      res.status(404).json({
        success: false,
        error: "Documento no encontrado",
        documentId
      });
    }

  } catch (error) {
    console.error("[RAG API] Error en DELETE /documents:", error);
    res.status(500).json({ 
      success: false,
      error: "Error eliminando documento",
      details: error.message 
    });
  }
});

/**
 * GET /api/rag/stats
 * Obtiene estadísticas del sistema RAG
 */
router.get("/stats", async (req, res) => {
  try {
    const stats = await getRAGStats();

    res.json({
      success: true,
      stats
    });

  } catch (error) {
    console.error("[RAG API] Error en /stats:", error);
    res.status(500).json({ 
      success: false,
      error: "Error obteniendo estadísticas",
      details: error.message 
    });
  }
});

/**
 * DELETE /api/rag/clear
 * Limpia todo el índice RAG (usar con precaución)
 */
router.delete("/clear", async (req, res) => {
  try {
    const { confirm } = req.body;

    if (confirm !== 'DELETE_ALL') {
      return res.status(400).json({
        success: false,
        error: "Para confirmar la eliminación, envía { \"confirm\": \"DELETE_ALL\" }"
      });
    }

    const result = await clearRAGIndex();

    res.json({
      success: true,
      message: "Índice RAG limpiado completamente",
      result
    });

  } catch (error) {
    console.error("[RAG API] Error en /clear:", error);
    res.status(500).json({ 
      success: false,
      error: "Error limpiando índice RAG",
      details: error.message 
    });
  }
});

/**
 * GET /api/rag/health
 * Endpoint de salud del sistema RAG
 */
router.get("/health", async (req, res) => {
  try {
    const stats = await getRAGStats();
    
    res.json({
      success: true,
      status: "healthy",
      timestamp: new Date().toISOString(),
      summary: {
        totalDocuments: stats.totalDocuments,
        totalChunks: stats.totalChunks,
        embeddingDimension: stats.embeddingDimension,
        integrityCheck: stats.integrity?.isValid || false
      }
    });

  } catch (error) {
    console.error("[RAG API] Error en /health:", error);
    res.status(500).json({ 
      success: false,
      status: "unhealthy",
      error: error.message,
      timestamp: new Date().toISOString()
    });
  }
});

/**
 * POST /api/rag/backup
 * Crea backup manual de datos RAG
 */
router.post('/backup', async (req, res) => {
  try {
    console.log('[RAG API] 🔄 Creando backup manual...');
    
    const backupData = await persistenceManager.manualBackup();
    
    res.json({
      success: true,
      message: 'Backup creado exitosamente',
      backup: backupData,
      timestamp: new Date().toISOString()
    });
  } catch (error) {
    console.error('[RAG API] ❌ Error creando backup:', error);
    res.status(500).json({
      success: false,
      error: 'Error creando backup',
      details: error.message,
      timestamp: new Date().toISOString()
    });
  }
});

/**
 * GET /api/rag/download-db
 * Descarga la base de datos SQLite completa
 * Query params:
 *   - compress: true/false (opcional, comprime con gzip)
 */
router.get('/download-db', async (req, res) => {
  try {
    console.log('[RAG API] 📥 Descargando base de datos...');
    
    const dbPath = './data/rag_vectors.db';
    const compress = req.query.compress === 'true';
    const fsModule = await import('fs');
    const fs = fsModule.default;
    const fsPromises = await import('fs/promises');
    
    // Verificar que el archivo existe
    try {
      await fsPromises.access(dbPath);
    } catch {
      return res.status(404).json({
        success: false,
        error: 'Base de datos no encontrada',
        timestamp: new Date().toISOString()
      });
    }

    // Obtener información del archivo
    const stats = await fsPromises.stat(dbPath);
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
    const sizeInMB = (stats.size / 1024 / 1024).toFixed(2);
    
    console.log(`[RAG API] 📊 Tamaño de BD: ${sizeInMB} MB`);

    if (compress) {
      // Descargar comprimido con gzip
      const zlib = await import('zlib');
      const filename = `rag_vectors_backup_${timestamp}.db.gz`;
      
      res.setHeader('Content-Type', 'application/gzip');
      res.setHeader('Content-Disposition', `attachment; filename="${filename}"`);
      res.setHeader('Content-Encoding', 'gzip');
      
      const readStream = fs.createReadStream(dbPath);
      const gzip = zlib.createGzip({ level: 9 }); // Máxima compresión
      
      readStream.pipe(gzip).pipe(res);
      
      readStream.on('end', () => {
        console.log(`[RAG API] ✅ Base de datos comprimida descargada: ${filename}`);
      });
      
      readStream.on('error', (error) => {
        console.error('[RAG API] ❌ Error en stream de lectura:', error);
      });
      
    } else {
      // Descargar sin comprimir usando streaming
      const filename = `rag_vectors_backup_${timestamp}.db`;
      
      res.setHeader('Content-Type', 'application/octet-stream');
      res.setHeader('Content-Disposition', `attachment; filename="${filename}"`);
      res.setHeader('Content-Length', stats.size);
      
      const readStream = fs.createReadStream(dbPath);
      readStream.pipe(res);
      
      readStream.on('end', () => {
        console.log(`[RAG API] ✅ Base de datos descargada: ${filename} (${sizeInMB} MB)`);
      });
      
      readStream.on('error', (error) => {
        console.error('[RAG API] ❌ Error en stream de lectura:', error);
      });
    }

  } catch (error) {
    console.error('[RAG API] ❌ Error descargando base de datos:', error);
    res.status(500).json({
      success: false,
      error: 'Error descargando base de datos',
      details: error.message,
      timestamp: new Date().toISOString()
    });
  }
});

/**
 * POST /api/rag/upload-db
 * Sube y restaura una base de datos SQLite
 */
router.post('/upload-db', uploadDB.single('database'), async (req, res) => {
  try {
    console.log('[RAG API] 📤 Restaurando base de datos...');
    
    if (!req.file) {
      return res.status(400).json({
        success: false,
        error: 'No se proporcionó archivo de base de datos'
      });
    }

    const fs = await import('fs/promises');
    const dbPath = './data/rag_vectors.db';
    const backupPath = `./data/rag_vectors_backup_${Date.now()}.db`;

    // Obtener estadísticas ANTES de restaurar
    let statsBefore = null;
    try {
      statsBefore = await fs.stat(dbPath);
      console.log(`[RAG API] 📊 BD actual: ${statsBefore.size} bytes`);
    } catch {
      console.log('[RAG API] 📝 No hay base de datos previa');
    }

    // Obtener estadísticas del archivo a restaurar
    const uploadStats = await fs.stat(req.file.path);
    console.log(`[RAG API] 📤 Archivo a restaurar: ${uploadStats.size} bytes`);

    // Crear backup de la BD actual si existe
    try {
      await fs.access(dbPath);
      await fs.copyFile(dbPath, backupPath);
      console.log(`[RAG API] 💾 Backup creado: ${backupPath}`);
    } catch {
      console.log('[RAG API] 📝 No hay base de datos previa para respaldar');
    }

    // CERRAR conexiones SQLite antes de reemplazar
    console.log('[RAG API] 🔒 Cerrando conexiones SQLite...');
    const { sqliteVectorStore } = await import('../services/sqliteVectorStore.js');
    sqliteVectorStore.close();
    
    // Copiar nueva base de datos
    console.log(`[RAG API] 🔄 Copiando ${req.file.path} → ${dbPath}`);
    await fs.copyFile(req.file.path, dbPath);
    console.log('[RAG API] ✅ Archivo copiado');
    
    // Limpiar archivo temporal
    await fs.unlink(req.file.path);
    console.log('[RAG API] 🗑️ Archivo temporal eliminado');

    // Obtener estadísticas de la nueva BD
    const stats = await fs.stat(dbPath);
    console.log(`[RAG API] 📊 BD después de restaurar: ${stats.size} bytes`);
    
    // REINICIALIZAR SQLite con la nueva base de datos
    console.log('[RAG API] 🔄 Reinicializando SQLite...');
    await sqliteVectorStore.reinitialize();
    console.log('[RAG API] ✅ SQLite reinicializado con nueva base de datos');
    
    // VALIDAR que la base de datos se puede leer correctamente
    console.log('[RAG API] 🔍 Validando integridad de la base de datos...');
    try {
      const dbStats = sqliteVectorStore.getStats();
      console.log(`[RAG API] ✅ Base de datos válida - Documentos: ${dbStats.totalDocuments}, Chunks: ${dbStats.totalChunks}`);
    } catch (validationError) {
      console.error('[RAG API] ❌ Error validando base de datos:', validationError);
      throw new Error(`Base de datos corrupta o inválida: ${validationError.message}`);
    }
    
    // SINCRONIZAR CONTEXTOS desde SQLite
    console.log('[RAG API] 🔄 Sincronizando contextos desde SQLite...');
    let syncedContexts = 0;
    let contextsList = [];
    try {
      const { contextPersistence } = await import('../services/contextPersistence.js');
      const contextsFromDB = sqliteVectorStore.getAllContexts();
      contextsList = contextsFromDB;
      
      // Convertir array a Map
      const contextsMap = new Map();
      for (const ctx of contextsFromDB) {
        contextsMap.set(ctx.id, ctx);
      }
      
      // Guardar contextos sincronizados
      await contextPersistence.saveContexts(contextsMap);
      syncedContexts = contextsFromDB.length;
      console.log(`[RAG API] ✅ Sincronizados ${syncedContexts} contextos desde SQLite`);
    } catch (syncError) {
      console.warn('[RAG API] ⚠️ Error sincronizando contextos:', syncError.message);
      // No lanzar error, continuar con la restauración
    }

    res.json({
      success: true,
      message: 'Base de datos restaurada y validada exitosamente',
      database: {
        size: stats.size,
        restored_at: new Date().toISOString(),
        backup_created: backupPath,
        documents: sqliteVectorStore.getStats().totalDocuments,
        chunks: sqliteVectorStore.getStats().totalChunks,
        contexts_synced: syncedContexts,
        contexts: contextsList
      },
      timestamp: new Date().toISOString()
    });

    console.log(`[RAG API] ✅ Base de datos restaurada: ${stats.size} bytes`);
  } catch (error) {
    console.error('[RAG API] ❌ Error restaurando base de datos:', error);
    
    // Limpiar archivo temporal en caso de error
    if (req.file?.path) {
      try {
        await import('fs/promises').then(fs => fs.unlink(req.file.path));
      } catch {}
    }

    res.status(500).json({
      success: false,
      error: 'Error restaurando base de datos',
      details: error.message,
      timestamp: new Date().toISOString()
    });
  }
});

/**
 * POST /api/rag/sync-contexts
 * Sincroniza contextos desde SQLite a contexts.json
 */
router.post('/sync-contexts', async (req, res) => {
  try {
    console.log('[RAG API] 🔄 Sincronizando contextos desde SQLite...');
    
    const { sqliteVectorStore } = await import('../services/sqliteVectorStore.js');
    const { contextPersistence } = await import('../services/contextPersistence.js');
    
    // Obtener contextos desde SQLite
    const contextsFromDB = sqliteVectorStore.getAllContexts();
    
    // Convertir array a Map
    const contextsMap = new Map();
    for (const ctx of contextsFromDB) {
      contextsMap.set(ctx.id, ctx);
    }
    
    // Guardar contextos sincronizados
    await contextPersistence.saveContexts(contextsMap);
    
    console.log(`[RAG API] ✅ Sincronizados ${contextsFromDB.length} contextos`);
    
    res.json({
      success: true,
      message: 'Contextos sincronizados exitosamente',
      contexts_synced: contextsFromDB.length,
      contexts: contextsFromDB,
      timestamp: new Date().toISOString()
    });
    
  } catch (error) {
    console.error('[RAG API] ❌ Error sincronizando contextos:', error);
    res.status(500).json({
      success: false,
      error: 'Error sincronizando contextos',
      details: error.message,
      timestamp: new Date().toISOString()
    });
  }
});

/**
 * GET /api/rag/db-info
 * Información sobre la base de datos actual
 */
router.get('/db-info', async (req, res) => {
  try {
    console.log('[RAG API] 📊 Obteniendo información de base de datos...');
    
    const dbPath = './data/rag_vectors.db';
    const fs = await import('fs/promises');
    
    try {
      const stats = await fs.stat(dbPath);
      const documents = await listDocuments();
      const ragStats = await getRAGStats();

      res.json({
        success: true,
        database: {
          path: dbPath,
          size: stats.size,
          size_mb: (stats.size / (1024 * 1024)).toFixed(2),
          modified: stats.mtime,
          created: stats.birthtime
        },
        content: {
          total_documents: documents.length,
          total_chunks: ragStats.totalDocuments || 0,
          contexts: ragStats.contexts || []
        },
        timestamp: new Date().toISOString()
      });
    } catch {
      res.status(404).json({
        success: false,
        error: 'Base de datos no encontrada',
        timestamp: new Date().toISOString()
      });
    }
  } catch (error) {
    console.error('[RAG API] ❌ Error obteniendo info de BD:', error);
    res.status(500).json({
      success: false,
      error: 'Error obteniendo información de base de datos',
      details: error.message,
      timestamp: new Date().toISOString()
    });
  }
});

/**
 * POST /api/rag/backup/create
 * Crea un backup manual de la base de datos
 */
router.post('/backup/create', async (req, res) => {
  try {
    console.log('[RAG API] 📦 Creando backup manual...');
    
    const { compress = true } = req.body;
    const result = await backupService.createBackup(compress);
    
    res.json({
      success: true,
      backup: result,
      message: 'Backup creado correctamente'
    });

  } catch (error) {
    console.error('[RAG API] ❌ Error creando backup:', error);
    res.status(500).json({
      success: false,
      error: 'Error creando backup',
      details: error.message
    });
  }
});

/**
 * GET /api/rag/backup/list
 * Lista todos los backups disponibles
 */
router.get('/backup/list', async (req, res) => {
  try {
    console.log('[RAG API] 📋 Listando backups...');
    
    const backups = await backupService.listBackups();
    const stats = await backupService.getBackupStats();
    
    res.json({
      success: true,
      backups,
      stats,
      count: backups.length
    });

  } catch (error) {
    console.error('[RAG API] ❌ Error listando backups:', error);
    res.status(500).json({
      success: false,
      error: 'Error listando backups',
      details: error.message
    });
  }
});

/**
 * GET /api/rag/backup/download/:filename
 * Descarga un backup específico
 */
router.get('/backup/download/:filename', async (req, res) => {
  try {
    const { filename } = req.params;
    console.log(`[RAG API] 📥 Descargando backup: ${filename}`);
    
    const backupPath = `./data/backup/${filename}`;
    const fsModule = await import('fs');
    const fs = fsModule.default;
    const fsPromises = await import('fs/promises');
    
    // Verificar que el archivo existe
    try {
      await fsPromises.access(backupPath);
    } catch {
      return res.status(404).json({
        success: false,
        error: 'Backup no encontrado'
      });
    }

    const stats = await fsPromises.stat(backupPath);
    
    res.setHeader('Content-Type', 'application/octet-stream');
    res.setHeader('Content-Disposition', `attachment; filename="${filename}"`);
    res.setHeader('Content-Length', stats.size);
    
    const readStream = fs.createReadStream(backupPath);
    readStream.pipe(res);
    
    readStream.on('end', () => {
      console.log(`[RAG API] ✅ Backup descargado: ${filename}`);
    });

  } catch (error) {
    console.error('[RAG API] ❌ Error descargando backup:', error);
    res.status(500).json({
      success: false,
      error: 'Error descargando backup',
      details: error.message
    });
  }
});

/**
 * DELETE /api/rag/backup/:filename
 * Elimina un backup específico
 */
router.delete('/backup/:filename', async (req, res) => {
  try {
    const { filename } = req.params;
    console.log(`[RAG API] 🗑️ Eliminando backup: ${filename}`);
    
    const result = await backupService.deleteBackup(filename);
    
    res.json({
      success: true,
      ...result
    });

  } catch (error) {
    console.error('[RAG API] ❌ Error eliminando backup:', error);
    res.status(500).json({
      success: false,
      error: 'Error eliminando backup',
      details: error.message
    });
  }
});

/**
 * POST /api/rag/validate-logos
 * Valida la presencia de logos en un PDF sin subirlo al RAG
 */
router.post('/validate-logos', uploadPliego.single('pdf'), async (req, res) => {
  try {
    if (!req.file) {
      return res.status(400).json({ 
        success: false,
        error: "No se proporcionó ningún archivo PDF" 
      });
    }

    console.log(`[RAG API] 🔍 Validando logos en: ${req.file.originalname}`);

    // Detectar logos
    const logoDetection = await logoDetectionService.detectLogosInPDF(req.file.path);
    const logoReport = logoDetectionService.generateLogoValidationReport(logoDetection, req.file.originalname);
    const logoDescription = logoDetectionService.generateLogoDescription(logoDetection, req.file.originalname);

    // Limpiar archivo temporal
    try {
      await fs.unlink(req.file.path);
    } catch (unlinkError) {
      console.warn(`[RAG API] No se pudo eliminar archivo temporal: ${unlinkError.message}`);
    }

    res.json({
      success: true,
      message: "Validación de logos completada",
      report: logoReport,
      description: logoDescription,
      details: {
        totalImages: logoDetection.totalImages,
        headerImages: logoDetection.headerImages.length,
        footerImages: logoDetection.footerImages.length,
        pagesWithImages: logoDetection.pagesWithImages
      }
    });

  } catch (error) {
    console.error("[RAG API] Error en /validate-logos:", error);
    
    // Limpiar archivo en caso de error
    if (req.file?.path) {
      try {
        await fs.unlink(req.file.path);
      } catch (unlinkError) {
        console.warn(`[RAG API] No se pudo limpiar archivo tras error: ${unlinkError.message}`);
      }
    }
    
    res.status(500).json({ 
      success: false,
      error: "Error validando logos",
      details: error.message 
    });
  }
});

/**
 * POST /api/rag/backup/restore/:filename
 * Restaura la base de datos desde un backup
 */
router.post('/backup/restore/:filename', async (req, res) => {
  try {
    const { filename } = req.params;
    console.log(`[RAG API] 🔄 Restaurando desde backup: ${filename}`);
    
    const result = await backupService.restoreBackup(filename);
    
    res.json({
      success: true,
      ...result,
      warning: 'La aplicación debe reiniciarse para que los cambios tengan efecto'
    });

  } catch (error) {
    console.error('[RAG API] ❌ Error restaurando backup:', error);
    res.status(500).json({
      success: false,
      error: 'Error restaurando backup',
      details: error.message
    });
  }
});

/**
 * GET /api/rag/backup/stats
 * Obtiene estadísticas de los backups
 */
router.get('/backup/stats', async (req, res) => {
  try {
    console.log('[RAG API] 📊 Obteniendo estadísticas de backups...');
    
    const stats = await backupService.getBackupStats();
    
    res.json({
      success: true,
      stats
    });

  } catch (error) {
    console.error('[RAG API] ❌ Error obteniendo estadísticas:', error);
    res.status(500).json({
      success: false,
      error: 'Error obteniendo estadísticas de backups',
      details: error.message
    });
  }
});

export default router;
