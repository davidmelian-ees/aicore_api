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
      'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
      'text/markdown',
      'application/json',
      'text/csv',
      'application/pdf',
      'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', // .xlsx
      'application/vnd.ms-excel' // .xls
    ];
    
    const allowedExtensions = ['.txt', '.docx', '.md', '.json', '.csv', '.pdf', '.xlsx', '.xls'];
    
    const fileExtension = path.extname(file.originalname).toLowerCase();
    
    if (allowedTypes.includes(file.mimetype) || allowedExtensions.includes(fileExtension)) {
      cb(null, true);
    } else {
      cb(new Error(`Tipo de archivo no soportado: ${file.mimetype}. Permitidos: txt, docx, md, json, csv, pdf, xlsx, xls`));
    }
  }
});

// Configuración específica para archivos de base de datos
const uploadDB = multer({ 
  storage,
  limits: {
    fileSize: 100 * 1024 * 1024 // 100MB límite para bases de datos
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

    // Indexar el documento usando SAP AI Core
    const result = await indexDocument(
      req.file.path,
      req.file.mimetype,
      {
        originalName: req.file.originalname,
        uploadedBy: req.body.uploadedBy || 'anonymous',
        uploadedAt: new Date().toISOString(),
        tags: req.body.tags ? req.body.tags.split(',').map(t => t.trim()) : [],
        contextId: req.body.contextId || 'default'
      }
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
      document: result
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
 */
router.get('/download-db', async (req, res) => {
  try {
    console.log('[RAG API] 📥 Descargando base de datos...');
    
    const dbPath = './data/rag_vectors.db';
    const fs = await import('fs/promises');
    
    // Verificar que el archivo existe
    try {
      await fs.access(dbPath);
    } catch {
      return res.status(404).json({
        success: false,
        error: 'Base de datos no encontrada',
        timestamp: new Date().toISOString()
      });
    }

    // Obtener información del archivo
    const stats = await fs.stat(dbPath);
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
    const filename = `rag_vectors_backup_${timestamp}.db`;

    // Configurar headers para descarga
    res.setHeader('Content-Type', 'application/octet-stream');
    res.setHeader('Content-Disposition', `attachment; filename="${filename}"`);
    res.setHeader('Content-Length', stats.size);

    // Enviar archivo
    const fileBuffer = await fs.readFile(dbPath);
    res.send(fileBuffer);

    console.log(`[RAG API] ✅ Base de datos descargada: ${filename} (${stats.size} bytes)`);
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

    res.json({
      success: true,
      message: 'Base de datos restaurada exitosamente',
      database: {
        size: stats.size,
        restored_at: new Date().toISOString(),
        backup_created: backupPath
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

export default router;
