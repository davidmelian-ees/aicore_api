import express from 'express';
import multer from 'multer';
import { pliegoValidationService } from '../services/pliegoValidationService.js';
import { processDocument } from '../services/documentProcessor.js';
import fs from 'fs/promises';
import path from 'path';

const router = express.Router();

// Configuración de multer para subida de archivos
const upload = multer({
  dest: 'uploads/',
  limits: {
    fileSize: 10 * 1024 * 1024 // 10MB
  },
  fileFilter: (req, file, cb) => {
    const allowedTypes = [
      'application/pdf',
      'application/msword',
      'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
      'text/plain'
    ];
    
    if (allowedTypes.includes(file.mimetype)) {
      cb(null, true);
    } else {
      cb(new Error('Tipo de archivo no soportado. Use PDF, DOC, DOCX o TXT.'));
    }
  }
});

/**
 * POST /api/pliego-validation/validate
 * Valida un pliego estructuralmente contra un contexto específico
 * 
 * Parámetros (multipart/form-data):
 * - documento: Archivo del pliego (obligatorio)
 * - contextId: ID del contexto RAG para validación (opcional)
 * - uploadedBy: Usuario que sube el documento (opcional, default: 'anonymous')
 * - tags: Tags del documento, separados por comas o array (opcional)
 */
router.post('/validate', upload.single('documento'), async (req, res) => {
  let tempFilePath = null;
  
  try {
    console.log('[PLIEGO-VALIDATION] 📄 Nueva solicitud de validación');
    
    if (!req.file) {
      return res.status(400).json({
        success: false,
        error: 'No se proporcionó ningún archivo'
      });
    }
    
    tempFilePath = req.file.path;
    const originalName = req.file.originalname;
    const mimeType = req.file.mimetype;
    
    console.log(`[PLIEGO-VALIDATION] 📁 Procesando: ${originalName} (${mimeType})`);
    
    // Extraer texto del documento
    const textoExtraido = await processDocument(tempFilePath, mimeType, {
      fileName: originalName,
      uploadedAt: new Date().toISOString()
    });
    
    if (!textoExtraido || textoExtraido.trim().length === 0) {
      return res.status(400).json({
        success: false,
        error: 'No se pudo extraer texto del documento o el documento está vacío'
      });
    }
    
    console.log(`[PLIEGO-VALIDATION] 📝 Texto extraído: ${textoExtraido.length} caracteres`);
    
    // Obtener metadatos adicionales del request
    const { contextId, uploadedBy, tags } = req.body;
    
    const metadatos = {
      fileName: originalName,
      fileSize: req.file.size,
      mimeType: mimeType,
      uploadedAt: new Date().toISOString(),
      uploadedBy: uploadedBy || 'anonymous',
      tags: tags ? (Array.isArray(tags) ? tags : tags.split(',').map(t => t.trim())) : [],
      userAgent: req.headers['user-agent'],
      ip: req.ip,
      contextId: contextId || null,
      ...req.body // Incluir cualquier metadato adicional del formulario
    };
    
    // Ejecutar validación completa
    const reporteValidacion = await pliegoValidationService.generarReporteValidacion(
      textoExtraido,
      metadatos
    );
    
    console.log(`[PLIEGO-VALIDATION] ✅ Validación completada - ID: ${reporteValidacion.id}`);
    
    res.json({
      success: true,
      reporte: reporteValidacion,
      mensaje: 'Validación completada exitosamente'
    });
    
  } catch (error) {
    console.error('[PLIEGO-VALIDATION] ❌ Error en validación:', error);
    
    res.status(500).json({
      success: false,
      error: 'Error interno del servidor durante la validación',
      details: process.env.NODE_ENV === 'development' ? error.message : undefined
    });
    
  } finally {
    // Limpiar archivo temporal
    if (tempFilePath) {
      try {
        await fs.unlink(tempFilePath);
        console.log(`[PLIEGO-VALIDATION] 🧹 Archivo temporal eliminado: ${tempFilePath}`);
      } catch (cleanupError) {
        console.warn(`[PLIEGO-VALIDATION] ⚠️ Error limpiando archivo temporal: ${cleanupError.message}`);
      }
    }
  }
});

/**
 * POST /api/pliego-validation/validate-text
 * Valida texto directo (sin archivo) contra un contexto específico
 * 
 * Body JSON:
 * - texto: Contenido del pliego a validar (obligatorio)
 * - contextId: ID del contexto RAG para validación (opcional)
 * - uploadedBy: Usuario que valida el documento (opcional, default: 'anonymous')
 * - tags: Tags del documento como array o string separado por comas (opcional)
 * - metadatos: Metadatos adicionales (opcional)
 */
router.post('/validate-text', async (req, res) => {
  try {
    const { texto, metadatos = {}, contextId, uploadedBy, tags } = req.body;
    
    if (!texto || typeof texto !== 'string' || texto.trim().length === 0) {
      return res.status(400).json({
        success: false,
        error: 'Se requiere el campo "texto" con contenido válido'
      });
    }
    
    console.log(`[PLIEGO-VALIDATION] 📝 Validando texto directo: ${texto.length} caracteres`);
    
    const metadatosCompletos = {
      source: 'text_input',
      uploadedAt: new Date().toISOString(),
      uploadedBy: uploadedBy || 'anonymous',
      tags: tags ? (Array.isArray(tags) ? tags : tags.split(',').map(t => t.trim())) : [],
      textLength: texto.length,
      userAgent: req.headers['user-agent'],
      ip: req.ip,
      contextId: contextId || null,
      ...metadatos
    };
    
    const reporteValidacion = await pliegoValidationService.generarReporteValidacion(
      texto,
      metadatosCompletos
    );
    
    console.log(`[PLIEGO-VALIDATION] ✅ Validación de texto completada - ID: ${reporteValidacion.id}`);
    
    res.json({
      success: true,
      reporte: reporteValidacion,
      mensaje: 'Validación de texto completada exitosamente'
    });
    
  } catch (error) {
    console.error('[PLIEGO-VALIDATION] ❌ Error en validación de texto:', error);
    
    res.status(500).json({
      success: false,
      error: 'Error interno del servidor durante la validación',
      details: process.env.NODE_ENV === 'development' ? error.message : undefined
    });
  }
});

/**
 * GET /api/pliego-validation/reportes
 * Lista reportes de validación generados
 */
router.get('/reportes', async (req, res) => {
  try {
    const { limit = 10, offset = 0 } = req.query;
    
    // Leer directorio de reportes
    const reportesDir = './validation_results';
    
    try {
      const archivos = await fs.readdir(reportesDir);
      const reportesArchivos = archivos
        .filter(archivo => archivo.startsWith('reporte_validacion_') && archivo.endsWith('.json'))
        .sort((a, b) => b.localeCompare(a)) // Ordenar por fecha (más reciente primero)
        .slice(parseInt(offset), parseInt(offset) + parseInt(limit));
      
      const reportes = [];
      
      for (const archivo of reportesArchivos) {
        try {
          const contenido = await fs.readFile(path.join(reportesDir, archivo), 'utf8');
          const reporte = JSON.parse(contenido);
          
          // Incluir solo metadatos básicos para la lista
          reportes.push({
            id: reporte.id,
            timestamp: reporte.timestamp,
            tipoPliego: reporte.tipoPliego,
            puntuacionCalidad: reporte.puntuacionCalidad,
            fileName: reporte.metadatos?.fileName || 'Texto directo',
            erroresCriticos: reporte.erroresCriticos?.length || 0,
            advertencias: reporte.advertencias?.length || 0
          });
        } catch (parseError) {
          console.warn(`[PLIEGO-VALIDATION] ⚠️ Error leyendo reporte ${archivo}:`, parseError.message);
        }
      }
      
      res.json({
        success: true,
        reportes,
        total: reportes.length,
        offset: parseInt(offset),
        limit: parseInt(limit)
      });
      
    } catch (dirError) {
      // Si no existe el directorio, retornar lista vacía
      res.json({
        success: true,
        reportes: [],
        total: 0,
        offset: 0,
        limit: parseInt(limit),
        mensaje: 'No se han generado reportes aún'
      });
    }
    
  } catch (error) {
    console.error('[PLIEGO-VALIDATION] ❌ Error listando reportes:', error);
    
    res.status(500).json({
      success: false,
      error: 'Error interno del servidor al listar reportes'
    });
  }
});

/**
 * GET /api/pliego-validation/reportes/:id
 * Obtiene un reporte específico por ID
 */
router.get('/reportes/:id', async (req, res) => {
  try {
    const { id } = req.params;
    
    if (!id || !/^[a-f0-9-]{36}$/i.test(id)) {
      return res.status(400).json({
        success: false,
        error: 'ID de reporte inválido'
      });
    }
    
    const archivoReporte = `./validation_results/reporte_validacion_${id}.json`;
    
    try {
      const contenido = await fs.readFile(archivoReporte, 'utf8');
      const reporte = JSON.parse(contenido);
      
      res.json({
        success: true,
        reporte
      });
      
    } catch (fileError) {
      res.status(404).json({
        success: false,
        error: 'Reporte no encontrado'
      });
    }
    
  } catch (error) {
    console.error('[PLIEGO-VALIDATION] ❌ Error obteniendo reporte:', error);
    
    res.status(500).json({
      success: false,
      error: 'Error interno del servidor al obtener reporte'
    });
  }
});

/**
 * GET /api/pliego-validation/campos-variables
 * Obtiene el archivo de campos variables detectados
 */
router.get('/campos-variables', async (req, res) => {
  try {
    const archivoCampos = './validation_results/campos_variables_detectados.txt';
    
    try {
      const contenido = await fs.readFile(archivoCampos, 'utf8');
      
      res.json({
        success: true,
        contenido,
        mensaje: 'Archivo de campos variables obtenido exitosamente'
      });
      
    } catch (fileError) {
      res.json({
        success: true,
        contenido: '',
        mensaje: 'No se han detectado campos variables aún'
      });
    }
    
  } catch (error) {
    console.error('[PLIEGO-VALIDATION] ❌ Error obteniendo campos variables:', error);
    
    res.status(500).json({
      success: false,
      error: 'Error interno del servidor al obtener campos variables'
    });
  }
});

/**
 * GET /api/pliego-validation/tipos-pliego
 * Obtiene información sobre los tipos de pliego soportados
 */
router.get('/tipos-pliego', (req, res) => {
  try {
    const tiposInfo = Object.entries(pliegoValidationService.tiposPliego).map(([tipo, config]) => ({
      tipo,
      codigo: config.codigo,
      nombre: config.nombre,
      apartadosObligatorios: config.apartadosObligatorios,
      patrones: config.patrones
    }));
    
    const categoriasInfo = Object.entries(pliegoValidationService.categoriasDocumento).map(([categoria, config]) => ({
      categoria,
      descripcion: config.descripcion,
      sufijo: config.sufijo,
      contextoRAG: config.contextoRAG
    }));
    
    res.json({
      success: true,
      tipos: tiposInfo,
      categorias: categoriasInfo,
      totalTipos: tiposInfo.length,
      variantes: ['sin_lotes', 'con_lotes'],
      totalCombinaciones: tiposInfo.length * 2 * categoriasInfo.length
    });
    
  } catch (error) {
    console.error('[PLIEGO-VALIDATION] ❌ Error obteniendo tipos:', error);
    
    res.status(500).json({
      success: false,
      error: 'Error interno del servidor al obtener tipos de pliego'
    });
  }
});

/**
 * GET /api/pliego-validation/nomenclatura
 * Obtiene información sobre la convención de nomenclatura
 */
router.get('/nomenclatura', (req, res) => {
  try {
    const ejemplos = [];
    
    // Generar ejemplos para cada combinación
    Object.keys(pliegoValidationService.tiposPliego).forEach(tipo => {
      Object.keys(pliegoValidationService.categoriasDocumento).forEach(categoria => {
        ['con_lotes', 'sin_lotes'].forEach(modalidad => {
          const tieneLotes = modalidad === 'con_lotes';
          const nombreEjemplo = pliegoValidationService.generarNombreEstandar(tipo, tieneLotes, categoria);
          
          ejemplos.push({
            tipo,
            modalidad,
            categoria,
            nombreEjemplo,
            contextoRAG: pliegoValidationService.obtenerContextoRAG(categoria)
          });
        });
      });
    });
    
    res.json({
      success: true,
      convencion: {
        formato: 'pliego_{tipo}_{modalidad}_{categoria}',
        descripcion: 'Convención estándar para nombrar documentos de pliegos',
        componentes: {
          tipo: 'obra_civil_obert | obra_civil_simplificat | obra_edificacio_obert | obra_edificacio_simplificat',
          modalidad: 'con_lotes | sin_lotes',
          categoria: 'PLANTILLA | PLANTILLA_TAGS | GENERADO | VALIDACION'
        }
      },
      ejemplos: ejemplos.slice(0, 16), // Mostrar algunos ejemplos
      totalCombinaciones: ejemplos.length,
      contextosRAG: Object.values(pliegoValidationService.categoriasDocumento).map(c => c.contextoRAG)
    });
    
  } catch (error) {
    console.error('[PLIEGO-VALIDATION] ❌ Error obteniendo nomenclatura:', error);
    
    res.status(500).json({
      success: false,
      error: 'Error interno del servidor al obtener información de nomenclatura'
    });
  }
});

/**
 * POST /api/pliego-validation/validar-nombre
 * Valida si un nombre de archivo sigue la convención
 */
router.post('/validar-nombre', (req, res) => {
  try {
    const { nombreArchivo } = req.body;
    
    if (!nombreArchivo || typeof nombreArchivo !== 'string') {
      return res.status(400).json({
        success: false,
        error: 'Se requiere el campo "nombreArchivo" con un nombre válido'
      });
    }
    
    const analisis = pliegoValidationService.validarNombreConvencion(nombreArchivo);
    const categoriaDetectada = pliegoValidationService.detectarCategoriaPorNombre(nombreArchivo);
    const contextoSugerido = pliegoValidationService.obtenerContextoRAG(categoriaDetectada);
    
    res.json({
      success: true,
      nombreArchivo,
      analisis,
      categoriaDetectada,
      contextoRAGSugerido: contextoSugerido,
      recomendaciones: analisis.esValido 
        ? ['✅ El nombre sigue la convención correctamente']
        : [
            `📝 Renombrar a: ${analisis.nombreSugerido || 'formato correcto'}`,
            `📂 Subir al contexto: ${contextoSugerido}`
          ]
    });
    
  } catch (error) {
    console.error('[PLIEGO-VALIDATION] ❌ Error validando nombre:', error);
    
    res.status(500).json({
      success: false,
      error: 'Error interno del servidor al validar nombre'
    });
  }
});

/**
 * GET /api/pliego-validation/health
 * Health check del servicio de validación
 */
router.get('/health', (req, res) => {
  res.json({
    success: true,
    service: 'Pliego Validation Service',
    status: 'healthy',
    timestamp: new Date().toISOString(),
    version: '2.0.0',
    features: {
      tiposPliego: Object.keys(pliegoValidationService.tiposPliego).length,
      categoriasDocumento: Object.keys(pliegoValidationService.categoriasDocumento).length,
      validacionEstructural: true,
      deteccionLotes: true,
      extraccionCamposVariables: true,
      reportesDetallados: true,
      nomenclaturaEstandar: true,
      validacionNombres: true,
      contextosRAGAutomaticos: true
    },
    nomenclatura: {
      formato: 'pliego_{tipo}_{modalidad}_{categoria}',
      totalCombinaciones: Object.keys(pliegoValidationService.tiposPliego).length * 2 * Object.keys(pliegoValidationService.categoriasDocumento).length,
      contextosRAG: Object.values(pliegoValidationService.categoriasDocumento).map(c => c.contextoRAG)
    },
    endpoints: {
      validar: '/api/pliego-validation/validate',
      validarTexto: '/api/pliego-validation/validate-text',
      reportes: '/api/pliego-validation/reportes',
      nomenclatura: '/api/pliego-validation/nomenclatura',
      validarNombre: '/api/pliego-validation/validar-nombre',
      tiposPliego: '/api/pliego-validation/tipos-pliego'
    },
    parametrosValidacion: {
      obligatorios: ['documento o texto'],
      opcionales: ['contextId', 'uploadedBy', 'tags', 'metadatos'],
      formatoTags: 'array o string separado por comas'
    }
  });
});

export default router;
