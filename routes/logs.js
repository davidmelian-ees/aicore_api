import express from 'express';
import loggerService from '../services/loggerService.js';

const router = express.Router();

/**
 * GET /api/logs
 * Ver los logs en formato JSON
 */
router.get('/', async (req, res) => {
  try {
    const logs = await loggerService.getLogs();
    res.json({
      success: true,
      logs: logs
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      error: 'Error obteniendo logs',
      message: error.message
    });
  }
});

/**
 * GET /api/logs/download
 * Descargar los logs en formato Markdown
 */
router.get('/download', async (req, res) => {
  try {
    const markdown = await loggerService.getLogsAsMarkdown();
    
    res.setHeader('Content-Type', 'text/markdown');
    res.setHeader('Content-Disposition', `attachment; filename="aicore_api_logs_${Date.now()}.md"`);
    
    res.send(markdown);
  } catch (error) {
    res.status(500).json({
      success: false,
      error: 'Error descargando logs',
      message: error.message
    });
  }
});

/**
 * GET /api/logs/stats
 * Obtener estadísticas de los logs
 */
router.get('/stats', async (req, res) => {
  try {
    const stats = await loggerService.getLogStats();
    res.json({
      success: true,
      stats: stats
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      error: 'Error obteniendo estadísticas de logs',
      message: error.message
    });
  }
});

/**
 * DELETE /api/logs
 * Limpiar los logs
 */
router.delete('/', async (req, res) => {
  try {
    const cleared = await loggerService.clearLogs();
    if (cleared) {
      res.json({
        success: true,
        message: 'Logs limpiados correctamente'
      });
    } else {
      res.status(500).json({
        success: false,
        error: 'No se pudieron limpiar los logs'
      });
    }
  } catch (error) {
    res.status(500).json({
      success: false,
      error: 'Error limpiando logs',
      message: error.message
    });
  }
});

export default router;
