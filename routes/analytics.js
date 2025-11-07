import express from 'express';
import { getValidationAnalytics, classifyPliego, compareWithTemplate } from '../services/pliegoAnalyticsService.js';
import multer from 'multer';
import path from 'path';
import fs from 'fs/promises';

const router = express.Router();
const upload = multer({ dest: 'uploads/' });

/**
 * @route GET /api/analytics/validation
 * @desc Obtiene métricas de validación de pliegos
 * @query {string} timeRange - Rango de tiempo (ej: 7d, 30d, 90d)
 * @query {string} contextId - ID del contexto para filtrar
 */
router.get('/validation', async (req, res) => {
  try {
    const { timeRange = '7d', contextId } = req.query;

    console.log(`[ANALYTICS] Generando métricas de validación - timeRange: ${timeRange}, contextId: ${contextId}`);

    const analytics = await getValidationAnalytics(timeRange, contextId);

    res.json({
      success: true,
      data: analytics
    });

  } catch (error) {
    console.error('[ANALYTICS] Error obteniendo métricas:', error);
    res.status(500).json({
      success: false,
      error: 'Error obteniendo métricas de validación',
      details: error.message
    });
  }
});

/**
 * @route POST /api/analytics/classify
 * @desc Clasifica automáticamente un pliego (tipo y modalidad)
 * @body {file} pdf - Archivo PDF del pliego
 */
router.post('/classify', upload.single('pdf'), async (req, res) => {
  try {
    if (!req.file) {
      return res.status(400).json({
        success: false,
        error: 'No se proporcionó archivo PDF'
      });
    }

    console.log(`[ANALYTICS] Clasificando pliego: ${req.file.originalname}`);

    const classification = await classifyPliego(req.file.path);

    // Limpiar archivo temporal
    try {
      await fs.unlink(req.file.path);
    } catch (cleanupError) {
      console.warn('[ANALYTICS] Error limpiando archivo temporal:', cleanupError.message);
    }

    res.json({
      success: true,
      classification
    });

  } catch (error) {
    console.error('[ANALYTICS] Error clasificando pliego:', error);

    // Limpiar archivo temporal en caso de error
    if (req.file) {
      try {
        await fs.unlink(req.file.path);
      } catch (cleanupError) {
        console.warn('[ANALYTICS] Error limpiando archivo temporal en error:', cleanupError.message);
      }
    }

    res.status(500).json({
      success: false,
      error: 'Error clasificando pliego',
      details: error.message
    });
  }
});

/**
 * @route POST /api/analytics/compare-template
 * @desc Compara un pliego con su plantilla correspondiente
 * @body {file} pdf - Archivo PDF del pliego
 * @body {string} pliegoType - Tipo de pliego (obra_civil, edificacio)
 * @body {string} pliegoModality - Modalidad (obert, simplificat)
 * @body {string} contextId - Contexto donde buscar plantillas (default: PLANTILLAS_BASE)
 */
router.post('/compare-template', upload.single('pdf'), async (req, res) => {
  try {
    if (!req.file) {
      return res.status(400).json({
        success: false,
        error: 'No se proporcionó archivo PDF'
      });
    }

    const { pliegoType, pliegoModality, contextId = 'PLANTILLAS_BASE' } = req.body;

    if (!pliegoType || !pliegoModality) {
      return res.status(400).json({
        success: false,
        error: 'Se requieren pliegoType y pliegoModality'
      });
    }

    console.log(`[ANALYTICS] Comparando con plantilla: ${pliegoType}_${pliegoModality} (contexto: ${contextId})`);

    const comparison = await compareWithTemplate(
      req.file.path,
      pliegoType,
      pliegoModality,
      contextId
    );

    // Limpiar archivo temporal
    try {
      await fs.unlink(req.file.path);
    } catch (cleanupError) {
      console.warn('[ANALYTICS] Error limpiando archivo temporal:', cleanupError.message);
    }

    res.json({
      success: true,
      comparison
    });

  } catch (error) {
    console.error('[ANALYTICS] Error comparando con plantilla:', error);

    // Limpiar archivo temporal en caso de error
    if (req.file) {
      try {
        await fs.unlink(req.file.path);
      } catch (cleanupError) {
        console.warn('[ANALYTICS] Error limpiando archivo temporal en error:', cleanupError.message);
      }
    }

    res.status(500).json({
      success: false,
      error: 'Error comparando con plantilla',
      details: error.message
    });
  }
});

/**
 * @route GET /api/analytics/dashboard-summary
 * @desc Obtiene resumen completo para dashboard
 */
router.get('/dashboard-summary', async (req, res) => {
  try {
    console.log('[ANALYTICS] Generando resumen de dashboard');

    // Obtener métricas de las últimas 4 semanas
    const [week1, week4, month] = await Promise.all([
      getValidationAnalytics('7d'),
      getValidationAnalytics('30d'),
      getValidationAnalytics('90d')
    ]);

    // Calcular tendencias
    const trends = {
      validations: {
        current: week1.summary.totalValidations,
        previous: month.summary.totalValidations - week1.summary.totalValidations,
        trend: calculateTrend(week1.summary.totalValidations, month.summary.totalValidations - week1.summary.totalValidations)
      },
      errors: {
        current: week1.summary.totalErrors,
        previous: month.summary.totalErrors - week1.summary.totalErrors,
        trend: calculateTrend(week1.summary.totalErrors, month.summary.totalErrors - week1.summary.totalErrors)
      },
      avgErrors: {
        current: week1.summary.avgErrorsPerValidation,
        previous: (month.summary.totalErrors - week1.summary.totalErrors) /
                 Math.max(1, month.summary.totalValidations - week1.summary.totalValidations),
        trend: calculateTrend(
          week1.summary.avgErrorsPerValidation,
          (month.summary.totalErrors - week1.summary.totalErrors) /
          Math.max(1, month.summary.totalValidations - week1.summary.totalValidations)
        )
      }
    };

    // Top errores de la semana
    const topErrors = week1.errorsByType.slice(0, 5);

    // Distribución por tipos
    const typesDistribution = week4.typesDistribution.slice(0, 5);

    res.json({
      success: true,
      dashboard: {
        summary: week1.summary,
        trends,
        topErrors,
        typesDistribution,
        weeklyChart: week1.trends,
        generatedAt: new Date().toISOString()
      }
    });

  } catch (error) {
    console.error('[ANALYTICS] Error generando dashboard:', error);
    res.status(500).json({
      success: false,
      error: 'Error generando resumen de dashboard',
      details: error.message
    });
  }
});

function calculateTrend(current, previous) {
  if (previous === 0) return current > 0 ? 100 : 0;
  const change = ((current - previous) / previous) * 100;
  return Math.round(change * 100) / 100; // 2 decimales
}

export default router;
