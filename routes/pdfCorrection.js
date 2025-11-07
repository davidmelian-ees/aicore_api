import express from 'express';
import multer from 'multer';
import path from 'path';
import fs from 'fs/promises';
import { 
  generatePDFWithCorrectionsList, 
  applyCorrectionsDirectly, 
  parseCorrections,
  generateCorrections,
  testAiCoreConnection
} from '../services/pdfCorrectionService.js';

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

    const result = await generatePDFWithCorrectionsList(
      req.file.path,
      req.body.customPrompt || null,
      req.body.contextId || null
    );

    // Configurar headers para descarga
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
    const { prompt, contextId } = req.body;

    if (!prompt || !contextId) {
      return res.status(400).json({
        success: false,
        error: 'Se requieren prompt y contextId'
      });
    }

    console.log(`[PDF-CORRECTION] Generando lista desde contexto: ${contextId}`);

    const result = await generatePDFWithCorrectionsFromContext(
      prompt,
      contextId
    );

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

export default router;
