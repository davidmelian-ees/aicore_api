import fs from 'fs/promises';
import path from 'path';

/**
 * Servicio para almacenar y analizar errores detectados en pliegos
 * Permite tracking de errores recurrentes y generación de reportes agregados
 */

const ERRORS_DB_PATH = path.join(process.cwd(), 'data', 'pliego_errors.json');

/**
 * Estructura de datos:
 * {
 *   "pliegoId": {
 *     "metadata": {
 *       "pliegoId": "PLIEGO_001",
 *       "fileName": "pliego_ejemplo.pdf",
 *       "analyzedAt": "2025-11-13T09:20:00Z",
 *       "contextId": "PLIEGOS_TERMINADOS"
 *     },
 *     "criticalErrors": [
 *       {
 *         "type": "TAG_SIN_REEMPLAZAR",
 *         "description": "Tag SAP sin reemplazar: {B}HOLA{/B}",
 *         "location": "Apartado 18.- DOCUMENTACIÓ",
 *         "context": "QUADRE D'APARTATS",
 *         "detectedAt": "2025-11-13T09:20:00Z"
 *       }
 *     ],
 *     "warnings": [
 *       {
 *         "type": "FORMATO_FECHA_ERRONEO",
 *         "description": "El formato de fecha es erróneo",
 *         "location": "Apartado 5.- PLAZO",
 *         "context": "Fecha de inicio",
 *         "detectedAt": "2025-11-13T09:20:00Z"
 *       }
 *     ]
 *   }
 * }
 */

/**
 * Inicializa el archivo de base de datos si no existe
 */
async function initializeDB() {
  try {
    const dataDir = path.dirname(ERRORS_DB_PATH);
    await fs.mkdir(dataDir, { recursive: true });
    
    try {
      await fs.access(ERRORS_DB_PATH);
    } catch {
      // El archivo no existe, crearlo
      await fs.writeFile(ERRORS_DB_PATH, JSON.stringify({}, null, 2));
      console.log('[PLIEGO-ERRORS] Base de datos inicializada');
    }
  } catch (error) {
    console.error('[PLIEGO-ERRORS] Error inicializando DB:', error);
    throw error;
  }
}

/**
 * Lee la base de datos de errores
 */
async function readDB() {
  try {
    await initializeDB();
    const data = await fs.readFile(ERRORS_DB_PATH, 'utf8');
    return JSON.parse(data);
  } catch (error) {
    console.error('[PLIEGO-ERRORS] Error leyendo DB:', error);
    return {};
  }
}

/**
 * Escribe en la base de datos de errores
 */
async function writeDB(data) {
  try {
    await fs.writeFile(ERRORS_DB_PATH, JSON.stringify(data, null, 2));
  } catch (error) {
    console.error('[PLIEGO-ERRORS] Error escribiendo DB:', error);
    throw error;
  }
}

/**
 * Clasifica un error según su descripción
 */
function classifyError(description) {
  const descLower = description.toLowerCase();
  
  // Tags SAP sin reemplazar
  if (descLower.includes('tag sap') || descLower.includes('{b}') || descLower.includes('{/b}')) {
    return 'TAG_SIN_REEMPLAZAR';
  }
  
  // Comentarios de desarrollador
  if (descLower.includes('comentario de desarrollador') || descLower.includes('oriol:') || descLower.includes('david:')) {
    return 'COMENTARIO_DESARROLLADOR';
  }
  
  // Variables SAP sin reemplazar
  if (descLower.includes('variable sap') || descLower.includes('zvrm_') || descLower.includes('zrm_')) {
    return 'VARIABLE_SAP_SIN_REEMPLAZAR';
  }
  
  // Errores numéricos
  if (descLower.includes('incoherencia numérica') || descLower.includes('presupuesto') || descLower.includes('suma')) {
    return 'ERROR_NUMERICO';
  }
  
  // Tablas incompletas
  if (descLower.includes('tabla') && (descLower.includes('incompleta') || descLower.includes('aplica/no aplica'))) {
    return 'TABLA_INCOMPLETA';
  }
  
  // Formato de fecha
  if (descLower.includes('fecha') && descLower.includes('formato')) {
    return 'FORMATO_FECHA_ERRONEO';
  }
  
  // Secciones faltantes
  if (descLower.includes('sección') && (descLower.includes('falta') || descLower.includes('ausente'))) {
    return 'SECCION_FALTANTE';
  }
  
  // Error genérico
  return 'ERROR_GENERICO';
}

/**
 * Parsea errores del texto de la IA
 */
function parseErrorsFromAIResponse(aiResponse) {
  const criticalErrors = [];
  const warnings = [];
  
  const lines = aiResponse.split('\n');
  let currentSection = null;
  let currentError = null;
  
  for (const line of lines) {
    const trimmed = line.trim();
    
    // Detectar sección
    if (trimmed.includes('🔴') || trimmed.includes('ERRORES CRÍTICOS')) {
      currentSection = 'critical';
      continue;
    } else if (trimmed.includes('🟡') || trimmed.includes('ADVERTENCIAS')) {
      currentSection = 'warnings';
      continue;
    } else if (trimmed.includes('✅') || trimmed.includes('SUGERENCIAS') || trimmed.includes('📋')) {
      currentSection = null;
      continue;
    }
    
    // Detectar inicio de error (línea con "- ")
    if (trimmed.startsWith('- ') && !trimmed.includes('Ubicación:') && !trimmed.includes('Contexto:')) {
      // Guardar error anterior si existe
      if (currentError && currentSection) {
        if (currentSection === 'critical') {
          criticalErrors.push(currentError);
        } else if (currentSection === 'warnings') {
          warnings.push(currentError);
        }
      }
      
      // Crear nuevo error
      const description = trimmed.substring(2).trim();
      currentError = {
        type: classifyError(description),
        description: description,
        location: '',
        context: '',
        detectedAt: new Date().toISOString()
      };
    }
    // Detectar ubicación
    else if (trimmed.includes('- Ubicación:') || trimmed.includes('- Ubicacion:')) {
      if (currentError) {
        currentError.location = trimmed.split(':').slice(1).join(':').trim();
      }
    }
    // Detectar contexto
    else if (trimmed.includes('- Contexto:')) {
      if (currentError) {
        currentError.context = trimmed.split(':').slice(1).join(':').trim();
      }
    }
  }
  
  // Añadir último error
  if (currentError && currentSection) {
    if (currentSection === 'critical') {
      criticalErrors.push(currentError);
    } else if (currentSection === 'warnings') {
      warnings.push(currentError);
    }
  }
  
  return { criticalErrors, warnings };
}

/**
 * Almacena errores de un pliego
 */
export async function storeErrorsForPliego(pliegoId, aiResponse, metadata = {}) {
  try {
    console.log(`[PLIEGO-ERRORS] Almacenando errores para pliego: ${pliegoId}`);
    
    // Parsear errores de la respuesta de la IA
    const { criticalErrors, warnings } = parseErrorsFromAIResponse(aiResponse);
    
    // Leer DB actual
    const db = await readDB();
    
    // Crear o actualizar entrada del pliego
    db[pliegoId] = {
      metadata: {
        pliegoId,
        fileName: metadata.fileName || 'unknown.pdf',
        analyzedAt: new Date().toISOString(),
        contextId: metadata.contextId || null
      },
      criticalErrors,
      warnings,
      summary: {
        totalCriticalErrors: criticalErrors.length,
        totalWarnings: warnings.length,
        errorTypes: {}
      }
    };
    
    // Contar tipos de errores
    const allErrors = [...criticalErrors, ...warnings];
    allErrors.forEach(error => {
      db[pliegoId].summary.errorTypes[error.type] = 
        (db[pliegoId].summary.errorTypes[error.type] || 0) + 1;
    });
    
    // Guardar DB
    await writeDB(db);
    
    console.log(`[PLIEGO-ERRORS] ✅ Almacenados ${criticalErrors.length} errores críticos y ${warnings.length} advertencias`);
    
    return {
      success: true,
      pliegoId,
      criticalErrors: criticalErrors.length,
      warnings: warnings.length,
      errorTypes: db[pliegoId].summary.errorTypes
    };
    
  } catch (error) {
    console.error('[PLIEGO-ERRORS] Error almacenando errores:', error);
    throw error;
  }
}

/**
 * Obtiene errores de un pliego específico
 */
export async function getErrorsForPliego(pliegoId) {
  try {
    const db = await readDB();
    return db[pliegoId] || null;
  } catch (error) {
    console.error('[PLIEGO-ERRORS] Error obteniendo errores:', error);
    return null;
  }
}

/**
 * Genera reporte agregado de los últimos N pliegos
 */
export async function generateAggregatedReport(lastN = 5) {
  try {
    console.log(`[PLIEGO-ERRORS] Generando reporte agregado de últimos ${lastN} pliegos...`);
    
    const db = await readDB();
    const pliegoIds = Object.keys(db);
    
    // Ordenar por fecha de análisis (más recientes primero)
    const sortedPliegoIds = pliegoIds.sort((a, b) => {
      const dateA = new Date(db[a].metadata.analyzedAt);
      const dateB = new Date(db[b].metadata.analyzedAt);
      return dateB - dateA;
    });
    
    // Tomar últimos N
    const recentPliegoIds = sortedPliegoIds.slice(0, lastN);
    
    if (recentPliegoIds.length === 0) {
      return {
        success: true,
        message: 'No hay pliegos analizados',
        totalPliegos: 0,
        errorsByType: {},
        warningsByType: {},
        details: []
      };
    }
    
    // Agregar errores por tipo
    const errorsByType = {};
    const warningsByType = {};
    const details = [];
    
    for (const pliegoId of recentPliegoIds) {
      const pliegoData = db[pliegoId];
      
      // Contar errores críticos por tipo
      pliegoData.criticalErrors.forEach(error => {
        if (!errorsByType[error.type]) {
          errorsByType[error.type] = {
            count: 0,
            examples: []
          };
        }
        errorsByType[error.type].count++;
        
        // Guardar hasta 3 ejemplos
        if (errorsByType[error.type].examples.length < 3) {
          errorsByType[error.type].examples.push({
            pliegoId,
            description: error.description,
            location: error.location
          });
        }
      });
      
      // Contar warnings por tipo
      pliegoData.warnings.forEach(warning => {
        if (!warningsByType[warning.type]) {
          warningsByType[warning.type] = {
            count: 0,
            examples: []
          };
        }
        warningsByType[warning.type].count++;
        
        // Guardar hasta 3 ejemplos
        if (warningsByType[warning.type].examples.length < 3) {
          warningsByType[warning.type].examples.push({
            pliegoId,
            description: warning.description,
            location: warning.location
          });
        }
      });
      
      // Añadir a detalles
      details.push({
        pliegoId,
        fileName: pliegoData.metadata.fileName,
        analyzedAt: pliegoData.metadata.analyzedAt,
        criticalErrors: pliegoData.criticalErrors.length,
        warnings: pliegoData.warnings.length
      });
    }
    
    return {
      success: true,
      totalPliegos: recentPliegoIds.length,
      period: `Últimos ${lastN} pliegos`,
      errorsByType,
      warningsByType,
      details,
      generatedAt: new Date().toISOString()
    };
    
  } catch (error) {
    console.error('[PLIEGO-ERRORS] Error generando reporte:', error);
    throw error;
  }
}

/**
 * Formatea el reporte agregado en texto legible
 */
export function formatAggregatedReport(report) {
  if (!report.success) {
    return 'Error generando reporte';
  }
  
  if (report.totalPliegos === 0) {
    return 'No hay pliegos analizados';
  }
  
  let text = `📊 REPORTE AGREGADO DE ERRORES\n`;
  text += `================================================================================\n\n`;
  text += `Período: ${report.period}\n`;
  text += `Total de pliegos analizados: ${report.totalPliegos}\n`;
  text += `Generado: ${new Date(report.generatedAt).toLocaleString('es-ES')}\n\n`;
  
  // Errores críticos
  text += `🔴 ERRORES CRÍTICOS DETECTADOS:\n`;
  text += `================================================================================\n\n`;
  
  const errorTypes = Object.entries(report.errorsByType);
  if (errorTypes.length === 0) {
    text += `✅ No se detectaron errores críticos\n\n`;
  } else {
    errorTypes
      .sort((a, b) => b[1].count - a[1].count)
      .forEach(([type, data]) => {
        text += `📌 ${type.replace(/_/g, ' ')}: ${data.count} ocurrencia(s)\n`;
        text += `   Ejemplos:\n`;
        data.examples.forEach(ex => {
          text += `   - [${ex.pliegoId}] ${ex.description}\n`;
          if (ex.location) text += `     Ubicación: ${ex.location}\n`;
        });
        text += `\n`;
      });
  }
  
  // Advertencias
  text += `🟡 ADVERTENCIAS DETECTADAS:\n`;
  text += `================================================================================\n\n`;
  
  const warningTypes = Object.entries(report.warningsByType);
  if (warningTypes.length === 0) {
    text += `✅ No se detectaron advertencias\n\n`;
  } else {
    warningTypes
      .sort((a, b) => b[1].count - a[1].count)
      .forEach(([type, data]) => {
        text += `📌 ${type.replace(/_/g, ' ')}: ${data.count} ocurrencia(s)\n`;
        text += `   Ejemplos:\n`;
        data.examples.forEach(ex => {
          text += `   - [${ex.pliegoId}] ${ex.description}\n`;
          if (ex.location) text += `     Ubicación: ${ex.location}\n`;
        });
        text += `\n`;
      });
  }
  
  // Detalles por pliego
  text += `📋 DETALLE POR PLIEGO:\n`;
  text += `================================================================================\n\n`;
  
  report.details.forEach(detail => {
    text += `- ${detail.pliegoId} (${detail.fileName})\n`;
    text += `  Errores críticos: ${detail.criticalErrors} | Advertencias: ${detail.warnings}\n`;
    text += `  Analizado: ${new Date(detail.analyzedAt).toLocaleString('es-ES')}\n\n`;
  });
  
  return text;
}

/**
 * Elimina errores de un pliego
 */
export async function deleteErrorsForPliego(pliegoId) {
  try {
    const db = await readDB();
    delete db[pliegoId];
    await writeDB(db);
    
    console.log(`[PLIEGO-ERRORS] Eliminados errores del pliego: ${pliegoId}`);
    return { success: true };
  } catch (error) {
    console.error('[PLIEGO-ERRORS] Error eliminando errores:', error);
    throw error;
  }
}

/**
 * Limpia toda la base de datos
 */
export async function clearAllErrors() {
  try {
    await writeDB({});
    console.log('[PLIEGO-ERRORS] Base de datos limpiada');
    return { success: true };
  } catch (error) {
    console.error('[PLIEGO-ERRORS] Error limpiando DB:', error);
    throw error;
  }
}
