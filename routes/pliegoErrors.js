import express from 'express';
import path from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs/promises';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const router = express.Router();

// Ruta al archivo de errores
const ERRORS_DB_PATH = path.join(__dirname, '..', 'data', 'pliego_errors.json');

/**
 * Funciones auxiliares para manejo de fechas
 */
function parseDate(dateStr) {
  // Soporta YYYY-MM-DD o DD/MM/YYYY
  if (dateStr.includes('/')) {
    const [day, month, year] = dateStr.split('/');
    return new Date(year, month - 1, day);
  } else {
    return new Date(dateStr);
  }
}

function formatDate(date) {
  const day = String(date.getDate()).padStart(2, '0');
  const month = String(date.getMonth() + 1).padStart(2, '0');
  const year = date.getFullYear();
  return `${day}/${month}/${year}`;
}

function formatDateISO(date) {
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, '0');
  const day = String(date.getDate()).padStart(2, '0');
  return `${year}-${month}-${day}`;
}

function isDateInRange(dateStr, dateIni, dateFin) {
  const date = new Date(dateStr);
  const ini = parseDate(dateIni);
  const fin = parseDate(dateFin);
  
  // Ajustar fin de día
  fin.setHours(23, 59, 59, 999);
  
  return date >= ini && date <= fin;
}

/**
 * Función auxiliar para leer la base de datos de errores
 * Convierte la estructura de objeto a array para compatibilidad
 */
async function readErrorsDB() {
  try {
    const data = await fs.readFile(ERRORS_DB_PATH, 'utf-8');
    const db = JSON.parse(data);
    
    // Convertir estructura de objeto a array
    // Estructura original: { "PLIEGO_001": {...}, "PLIEGO_002": {...} }
    // Estructura convertida: { pliegos: [{...}, {...}] }
    if (db && typeof db === 'object' && !Array.isArray(db.pliegos)) {
      const pliegosArray = Object.values(db);
      return { pliegos: pliegosArray };
    }
    
    return db;
  } catch (error) {
    if (error.code === 'ENOENT') {
      // Archivo no existe, devolver estructura vacía
      return { pliegos: [] };
    }
    throw error;
  }
}

/**
 * GET /api/pdf-correction/errors/by-date
 * Obtiene errores filtrados por rango de fechas
 * Query params: dateIni, dateFin, errorType (opcional)
 */
router.get('/by-date', async (req, res) => {
  try {
    const { dateIni, dateFin, errorType } = req.query;

    if (!dateIni || !dateFin) {
      return res.status(400).json({
        success: false,
        error: 'Se requieren los parámetros: dateIni y dateFin (formato: YYYY-MM-DD o DD/MM/YYYY)'
      });
    }

    console.log(`[ERRORS-API] 📅 Obteniendo errores entre ${dateIni} y ${dateFin}`);

    // Leer base de datos
    const db = await readErrorsDB();

    // Filtrar por rango de fechas
    const filteredPliegos = db.pliegos.filter(pliego => {
      return isDateInRange(pliego.metadata.analyzedAt, dateIni, dateFin);
    });

    // Filtrar por tipo de error si se especifica
    let finalPliegos = filteredPliegos;
    if (errorType) {
      console.log(`[ERRORS-API] 🔍 Filtrando por tipo: ${errorType}`);
      finalPliegos = filteredPliegos.map(pliego => ({
        ...pliego,
        criticalErrors: pliego.criticalErrors.filter(e => e.type === errorType),
        warnings: pliego.warnings.filter(e => e.type === errorType)
      })).filter(pliego => pliego.criticalErrors.length > 0 || pliego.warnings.length > 0);
    }

    // Calcular estadísticas
    const totalCriticalErrors = finalPliegos.reduce((sum, p) => sum + p.criticalErrors.length, 0);
    const totalWarnings = finalPliegos.reduce((sum, p) => sum + p.warnings.length, 0);

    // Contar tipos de error
    const errorTypes = {};
    finalPliegos.forEach(pliego => {
      [...pliego.criticalErrors, ...pliego.warnings].forEach(error => {
        errorTypes[error.type] = (errorTypes[error.type] || 0) + 1;
      });
    });

    res.json({
      success: true,
      dateRange: `${formatDate(parseDate(dateIni))} - ${formatDate(parseDate(dateFin))}`,
      totalPliegos: finalPliegos.length,
      totalCriticalErrors,
      totalWarnings,
      errorTypes,
      pliegos: finalPliegos
    });

  } catch (error) {
    console.error('[ERRORS-API] ❌ Error obteniendo errores por fecha:', error);
    res.status(500).json({
      success: false,
      error: 'Error obteniendo errores por fecha',
      message: error.message
    });
  }
});

/**
 * GET /api/pdf-correction/errors/by-date/:date
 * Obtiene errores de una fecha específica
 * Param: date (formato: YYYY-MM-DD o DD-MM-YYYY)
 */
router.get('/by-date/:date', async (req, res) => {
  try {
    const { date } = req.params;
    const { errorType } = req.query;

    console.log(`[ERRORS-API] 📅 Obteniendo errores del día ${date}`);

    const targetDate = parseDate(date);
    const nextDay = new Date(targetDate);
    nextDay.setDate(nextDay.getDate() + 1);

    const dateIni = formatDateISO(targetDate);
    const dateFin = formatDateISO(nextDay);

    // Leer base de datos
    const db = await readErrorsDB();

    // Filtrar por fecha
    const filteredPliegos = db.pliegos.filter(pliego => {
      return isDateInRange(pliego.metadata.analyzedAt, dateIni, dateFin);
    });

    // Filtrar por tipo de error si se especifica
    let finalPliegos = filteredPliegos;
    if (errorType) {
      console.log(`[ERRORS-API] 🔍 Filtrando por tipo: ${errorType}`);
      finalPliegos = filteredPliegos.map(pliego => ({
        ...pliego,
        criticalErrors: pliego.criticalErrors.filter(e => e.type === errorType),
        warnings: pliego.warnings.filter(e => e.type === errorType)
      })).filter(pliego => pliego.criticalErrors.length > 0 || pliego.warnings.length > 0);
    }

    // Calcular estadísticas
    const totalCriticalErrors = finalPliegos.reduce((sum, p) => sum + p.criticalErrors.length, 0);
    const totalWarnings = finalPliegos.reduce((sum, p) => sum + p.warnings.length, 0);

    // Contar tipos de error
    const errorTypes = {};
    finalPliegos.forEach(pliego => {
      [...pliego.criticalErrors, ...pliego.warnings].forEach(error => {
        errorTypes[error.type] = (errorTypes[error.type] || 0) + 1;
      });
    });

    res.json({
      success: true,
      date: formatDate(targetDate),
      totalPliegos: finalPliegos.length,
      totalCriticalErrors,
      totalWarnings,
      errorTypes,
      pliegos: finalPliegos
    });

  } catch (error) {
    console.error('[ERRORS-API] ❌ Error obteniendo errores por fecha:', error);
    res.status(500).json({
      success: false,
      error: 'Error obteniendo errores por fecha',
      message: error.message
    });
  }
});

/**
 * GET /api/pdf-correction/errors/grouped-by-date
 * Obtiene errores agrupados por fecha
 * Query params: dateIni, dateFin
 */
router.get('/grouped-by-date', async (req, res) => {
  try {
    const { dateIni, dateFin } = req.query;

    if (!dateIni || !dateFin) {
      return res.status(400).json({
        success: false,
        error: 'Se requieren los parámetros: dateIni y dateFin'
      });
    }

    console.log(`[ERRORS-API] 📅 Agrupando errores por fecha entre ${dateIni} y ${dateFin}`);

    // Leer base de datos
    const db = await readErrorsDB();

    // Filtrar por rango de fechas
    const filteredPliegos = db.pliegos.filter(pliego => {
      return isDateInRange(pliego.metadata.analyzedAt, dateIni, dateFin);
    });

    // Agrupar por fecha
    const pliegosByDate = {};
    filteredPliegos.forEach(pliego => {
      const date = formatDate(new Date(pliego.metadata.analyzedAt));
      if (!pliegosByDate[date]) {
        pliegosByDate[date] = [];
      }
      pliegosByDate[date].push(pliego);
    });

    res.json({
      success: true,
      dateRange: `${formatDate(parseDate(dateIni))} - ${formatDate(parseDate(dateFin))}`,
      totalPliegos: filteredPliegos.length,
      pliegosByDate
    });

  } catch (error) {
    console.error('[ERRORS-API] ❌ Error agrupando errores:', error);
    res.status(500).json({
      success: false,
      error: 'Error agrupando errores por fecha',
      message: error.message
    });
  }
});

/**
 * GET /api/pdf-correction/errors/by-type/:errorType
 * Obtiene errores filtrados por tipo
 * Param: errorType (ej: TAG_SIN_REEMPLAZAR)
 * Query params: dateIni, dateFin (opcional)
 */
router.get('/by-type/:errorType', async (req, res) => {
  try {
    const { errorType } = req.params;
    const { dateIni, dateFin } = req.query;

    console.log(`[ERRORS-API] 🔍 Obteniendo errores de tipo: ${errorType}`);

    // Leer base de datos
    const db = await readErrorsDB();

    // Filtrar por rango de fechas si se especifica
    let filteredPliegos = db.pliegos;
    if (dateIni && dateFin) {
      filteredPliegos = db.pliegos.filter(pliego => {
        return isDateInRange(pliego.metadata.analyzedAt, dateIni, dateFin);
      });
    }

    // Filtrar por tipo de error
    const finalPliegos = filteredPliegos.map(pliego => ({
      ...pliego,
      criticalErrors: pliego.criticalErrors.filter(e => e.type === errorType),
      warnings: pliego.warnings.filter(e => e.type === errorType)
    })).filter(pliego => pliego.criticalErrors.length > 0 || pliego.warnings.length > 0);

    // Calcular estadísticas
    const totalCriticalErrors = finalPliegos.reduce((sum, p) => sum + p.criticalErrors.length, 0);
    const totalWarnings = finalPliegos.reduce((sum, p) => sum + p.warnings.length, 0);

    res.json({
      success: true,
      errorType,
      dateRange: dateIni && dateFin ? `${formatDate(parseDate(dateIni))} - ${formatDate(parseDate(dateFin))}` : 'Todos los tiempos',
      totalPliegos: finalPliegos.length,
      totalCriticalErrors,
      totalWarnings,
      pliegos: finalPliegos
    });

  } catch (error) {
    console.error('[ERRORS-API] ❌ Error obteniendo errores por tipo:', error);
    res.status(500).json({
      success: false,
      error: 'Error obteniendo errores por tipo',
      message: error.message
    });
  }
});

/**
 * GET /api/pdf-correction/errors/raw
 * Obtiene el JSON completo de errores (para debugging)
 */
router.get('/raw', async (req, res) => {
  try {
    console.log('[ERRORS-API] 📄 Obteniendo JSON completo de errores...');
    
    const db = await readErrorsDB();
    
    res.json({
      success: true,
      database: db,
      totalPliegos: db.pliegos?.length || 0
    });

  } catch (error) {
    console.error('[ERRORS-API] ❌ Error obteniendo JSON:', error);
    res.status(500).json({
      success: false,
      error: 'Error obteniendo JSON de errores',
      message: error.message
    });
  }
});

/**
 * GET /api/pdf-correction/errors/download
 * Descarga el archivo JSON completo de errores
 */
router.get('/download', async (req, res) => {
  try {
    console.log('[ERRORS-API] 💾 Descargando JSON de errores...');
    
    const db = await readErrorsDB();
    
    // Configurar headers para descarga
    res.setHeader('Content-Type', 'application/json');
    res.setHeader('Content-Disposition', `attachment; filename="pliego_errors_${Date.now()}.json"`);
    
    res.send(JSON.stringify(db, null, 2));

  } catch (error) {
    console.error('[ERRORS-API] ❌ Error descargando JSON:', error);
    res.status(500).json({
      success: false,
      error: 'Error descargando JSON de errores',
      message: error.message
    });
  }
});

/**
 * GET /api/pdf-correction/errors/statistics
 * Obtiene estadísticas resumidas de errores
 * Query params: dateIni, dateFin (opcional)
 */
router.get('/statistics', async (req, res) => {
  try {
    const { dateIni, dateFin } = req.query;

    console.log('[ERRORS-API] 📊 Generando estadísticas...');

    // Leer base de datos
    const db = await readErrorsDB();

    // Validar estructura
    if (!db.pliegos || !Array.isArray(db.pliegos)) {
      return res.json({
        success: true,
        statistics: {
          totalPliegos: 0,
          totalCriticalErrors: 0,
          totalWarnings: 0,
          avgCriticalPerPliego: "0.00",
          avgWarningsPerPliego: "0.00",
          mostCommonError: null,
          errorTypes: {}
        },
        note: 'No hay errores registrados aún. Analiza un pliego para generar datos.'
      });
    }

    // Filtrar por rango de fechas si se especifica
    let filteredPliegos = db.pliegos;
    if (dateIni && dateFin) {
      filteredPliegos = db.pliegos.filter(pliego => {
        return isDateInRange(pliego.metadata.analyzedAt, dateIni, dateFin);
      });
    }

    // Calcular estadísticas
    const totalPliegos = filteredPliegos.length;
    const totalCriticalErrors = filteredPliegos.reduce((sum, p) => sum + (p.criticalErrors?.length || 0), 0);
    const totalWarnings = filteredPliegos.reduce((sum, p) => sum + (p.warnings?.length || 0), 0);

    // Contar tipos de error
    const errorTypes = {};
    filteredPliegos.forEach(pliego => {
      [...pliego.criticalErrors, ...pliego.warnings].forEach(error => {
        errorTypes[error.type] = (errorTypes[error.type] || 0) + 1;
      });
    });

    // Encontrar error más común
    let mostCommonError = null;
    let maxCount = 0;
    Object.entries(errorTypes).forEach(([type, count]) => {
      if (count > maxCount) {
        maxCount = count;
        mostCommonError = { type, count };
      }
    });

    const stats = {
      totalPliegos,
      totalCriticalErrors,
      totalWarnings,
      avgCriticalPerPliego: totalPliegos > 0 ? (totalCriticalErrors / totalPliegos).toFixed(2) : '0.00',
      avgWarningsPerPliego: totalPliegos > 0 ? (totalWarnings / totalPliegos).toFixed(2) : '0.00',
      mostCommonError,
      errorTypes
    };

    if (dateIni && dateFin) {
      stats.dateRange = `${formatDate(parseDate(dateIni))} - ${formatDate(parseDate(dateFin))}`;
    }

    res.json({
      success: true,
      statistics: stats
    });

  } catch (error) {
    console.error('[ERRORS-API] ❌ Error generando estadísticas:', error);
    res.status(500).json({
      success: false,
      error: 'Error generando estadísticas',
      message: error.message
    });
  }
});

export default router;
