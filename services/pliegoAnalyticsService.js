import { searchContext } from './ragService.js';
import { getAiCoreClient } from '../auth/aiCoreClient.js';

/**
 * Servicio de métricas y analytics para validación de pliegos
 */

const VALIDATION_METRICS_STORE = new Map(); // Almacén temporal de métricas

/**
 * Registra una validación completada para métricas
 */
export function recordValidationMetrics(validationData) {
  const {
    pdfPath,
    errorsFound,
    contextId,
    processingTime,
    timestamp = new Date().toISOString(),
    pliegoType = 'unknown',
    pliegoModality = 'unknown'
  } = validationData;

  const validationRecord = {
    id: generateId(),
    pdfPath,
    errorsFound,
    errorCount: errorsFound.length,
    contextId,
    processingTime,
    timestamp,
    pliegoType,
    pliegoModality,
    errorsByType: categorizeErrors(errorsFound)
  };

  VALIDATION_METRICS_STORE.set(validationRecord.id, validationRecord);
  console.log(`[METRICS] Validación registrada: ${validationRecord.id} (${validationRecord.errorCount} errores)`);

  return validationRecord.id;
}

/**
 * Obtiene métricas de validación agregadas
 */
export async function getValidationAnalytics(timeRange = '7d', contextId = null) {
  try {
    const now = new Date();
    const cutoffDate = getCutoffDate(timeRange, now);

    // Filtrar validaciones por fecha y contexto
    const relevantValidations = Array.from(VALIDATION_METRICS_STORE.values())
      .filter(v => {
        const validationDate = new Date(v.timestamp);
        const contextMatch = !contextId || v.contextId === contextId;
        return validationDate >= cutoffDate && contextMatch;
      });

    if (relevantValidations.length === 0) {
      return {
        summary: {
          totalValidations: 0,
          totalErrors: 0,
          avgErrorsPerValidation: 0,
          avgProcessingTime: 0
        },
        errorsByType: [],
        typesDistribution: [],
        modalitiesDistribution: [],
        trends: []
      };
    }

    // Calcular métricas
    const totalValidations = relevantValidations.length;
    const totalErrors = relevantValidations.reduce((sum, v) => sum + v.errorCount, 0);
    const avgErrorsPerValidation = totalErrors / totalValidations;
    const avgProcessingTime = relevantValidations.reduce((sum, v) => sum + v.processingTime, 0) / totalValidations;

    // Errores por tipo
    const errorsByType = aggregateErrorsByType(relevantValidations);

    // Distribución por tipos de pliego
    const typesDistribution = aggregateByField(relevantValidations, 'pliegoType');

    // Distribución por modalidades
    const modalitiesDistribution = aggregateByField(relevantValidations, 'pliegoModality');

    // Tendencias (últimos 7 días)
    const trends = calculateTrends(relevantValidations, 7);

    return {
      summary: {
        totalValidations,
        totalErrors,
        avgErrorsPerValidation: Math.round(avgErrorsPerValidation * 100) / 100,
        avgProcessingTime: Math.round(avgProcessingTime * 100) / 100
      },
      errorsByType: errorsByType.slice(0, 10), // Top 10 tipos de error
      typesDistribution,
      modalitiesDistribution,
      trends,
      timeRange,
      generatedAt: new Date().toISOString()
    };

  } catch (error) {
    console.error('[METRICS] Error generando analytics:', error);
    throw new Error(`Error generando analytics: ${error.message}`);
  }
}

/**
 * Clasifica automáticamente el tipo y modalidad de un pliego
 */
export async function classifyPliego(pdfPath) {
  try {
    console.log(`[CLASSIFICATION] Clasificando pliego: ${pdfPath}`);

    // Extraer texto del PDF
    const { processDocument } = await import('./documentProcessor.js');
    const documentData = await processDocument(pdfPath, 'application/pdf');
    const text = documentData.chunks.map(chunk => chunk.content).join(' ').toLowerCase();

    // Patrones de clasificación
    const classification = {
      tipo: detectPliegoType(text),
      modalidad: detectPliegoModality(text),
      confidence: 0,
      reasoning: []
    };

    // Calcular confianza basada en matches
    classification.confidence = calculateClassificationConfidence(text, classification);

    // Buscar en contexto RAG para validar clasificación
    const ragValidation = await validateClassificationWithRAG(text, classification);

    return {
      ...classification,
      ragValidation,
      suggestedFilename: generateSuggestedFilename(classification),
      suggestedContext: getSuggestedContext(classification),
      detectedAt: new Date().toISOString()
    };

  } catch (error) {
    console.error('[CLASSIFICATION] Error clasificando pliego:', error);
    return {
      tipo: 'unknown',
      modalidad: 'unknown',
      confidence: 0,
      error: error.message,
      suggestedFilename: 'CT0000000_plec_unknown_unknown_unknown.pdf',
      suggestedContext: 'DOCUMENTOS_VALIDACION'
    };
  }
}

/**
 * Compara un pliego con su plantilla correspondiente
 */
export async function compareWithTemplate(pdfPath, pliegoType, pliegoModality, contextId = 'PLANTILLAS_BASE') {
  try {
    console.log(`[TEMPLATE-COMPARISON] Comparando ${pdfPath} con plantilla ${pliegoType}_${pliegoModality}`);

    // Extraer estructura del pliego actual
    const currentStructure = await extractPliegoStructure(pdfPath);

    // Buscar plantilla correspondiente en RAG
    const templateQuery = `plantilla ${pliegoType} ${pliegoModality}`;
    const templateResults = await searchContext(templateQuery, {
      contextId,
      topK: 5
    });

    if (templateResults.length === 0) {
      return {
        comparison: 'NO_TEMPLATE_FOUND',
        message: `No se encontró plantilla para ${pliegoType}_${pliegoModality}`,
        suggestions: ['Subir plantilla correspondiente', 'Verificar nomenclatura']
      };
    }

    // Extraer estructura de la plantilla
    const templateStructure = extractTemplateStructure(templateResults);

    // Comparar estructuras
    const comparison = compareStructures(currentStructure, templateStructure);

    return {
      comparison,
      templateFound: true,
      templateSource: templateResults[0].metadata?.fileName,
      structuralScore: calculateStructuralScore(comparison),
      recommendations: generateComparisonRecommendations(comparison)
    };

  } catch (error) {
    console.error('[TEMPLATE-COMPARISON] Error comparando con plantilla:', error);
    return {
      comparison: 'ERROR',
      error: error.message,
      recommendations: ['Verificar que existe plantilla', 'Comprobar permisos de acceso']
    };
  }
}

// FUNCIONES AUXILIARES

function generateId() {
  return Date.now().toString(36) + Math.random().toString(36).substr(2);
}

function getCutoffDate(timeRange, now) {
  const days = parseInt(timeRange.replace('d', ''));
  return new Date(now.getTime() - (days * 24 * 60 * 60 * 1000));
}

function categorizeErrors(errorsFound) {
  const categories = {};

  errorsFound.forEach(error => {
    // Clasificar errores por tipo basado en el texto
    let category = 'UNKNOWN';

    if (error.toLowerCase().includes('tag') || error.toLowerCase().includes('sap')) {
      category = 'TAGS_SAP_SIN_REEMPLAZAR';
    } else if (error.toLowerCase().includes('clau') || error.toLowerCase().includes('cláusula')) {
      category = 'CLAU_VACIA';
    } else if (error.toLowerCase().includes('fecha') || error.toLowerCase().includes('date')) {
      category = 'FECHAS_INCORRECTAS';
    } else if (error.toLowerCase().includes('importe') || error.toLowerCase().includes('presupuesto')) {
      category = 'IMPORTES_INCORRECTOS';
    } else if (error.toLowerCase().includes('campo') || error.toLowerCase().includes('variable')) {
      category = 'CAMPOS_VACIOS';
    }

    categories[category] = (categories[category] || 0) + 1;
  });

  return categories;
}

function aggregateErrorsByType(validations) {
  const errorCounts = {};

  validations.forEach(validation => {
    Object.entries(validation.errorsByType).forEach(([type, count]) => {
      errorCounts[type] = (errorCounts[type] || 0) + count;
    });
  });

  return Object.entries(errorCounts)
    .map(([type, count]) => ({ type, count, percentage: Math.round((count / Object.values(errorCounts).reduce((a, b) => a + b, 0)) * 100) }))
    .sort((a, b) => b.count - a.count);
}

function aggregateByField(validations, field) {
  const distribution = {};

  validations.forEach(validation => {
    const value = validation[field] || 'unknown';
    distribution[value] = (distribution[value] || 0) + 1;
  });

  return Object.entries(distribution)
    .map(([value, count]) => ({ value, count, percentage: Math.round((count / validations.length) * 100) }))
    .sort((a, b) => b.count - a.count);
}

function calculateTrends(validations, days) {
  const trends = [];
  const now = new Date();

  for (let i = days - 1; i >= 0; i--) {
    const date = new Date(now.getTime() - (i * 24 * 60 * 60 * 1000));
    const dateStr = date.toISOString().split('T')[0];

    const dayValidations = validations.filter(v =>
      v.timestamp.startsWith(dateStr)
    );

    trends.push({
      date: dateStr,
      validations: dayValidations.length,
      errors: dayValidations.reduce((sum, v) => sum + v.errorCount, 0),
      avgErrors: dayValidations.length > 0 ?
        Math.round((dayValidations.reduce((sum, v) => sum + v.errorCount, 0) / dayValidations.length) * 100) / 100 : 0
    });
  }

  return trends;
}

function detectPliegoType(text) {
  if (text.includes('obra civil') || text.includes('obra_civil')) return 'obra_civil';
  if (text.includes('edificaci') || text.includes('edificacio') || text.includes('edificació')) return 'edificacio';
  return 'unknown';
}

function detectPliegoModality(text) {
  if (text.includes('obert') || text.includes('open')) return 'obert';
  if (text.includes('simplificat') || text.includes('simplified')) return 'simplificat';
  return 'unknown';
}

function calculateClassificationConfidence(text, classification) {
  let confidence = 0;

  // Confianza por tipo
  if (classification.tipo !== 'unknown') confidence += 40;
  if (classification.modality !== 'unknown') confidence += 30;

  // Confianza por matches específicos
  const specificTerms = ['contracte', 'execució', 'obres', 'licitació'];
  const matches = specificTerms.filter(term => text.includes(term)).length;
  confidence += matches * 7.5;

  return Math.min(confidence, 100);
}

async function validateClassificationWithRAG(text, classification) {
  try {
    const query = `${classification.tipo} ${classification.modality} pliego plantilla`;
    const results = await searchContext(query, { topK: 3 });

    return {
      validated: results.length > 0,
      similarDocuments: results.length,
      confidence: results.length > 0 ? 85 : 30
    };
  } catch (error) {
    return { validated: false, error: error.message };
  }
}

function generateSuggestedFilename(classification) {
  const timestamp = Date.now();
  const tipo = classification.tipo || 'unknown';
  const modalidad = classification.modality || 'unknown';

  return `CT${timestamp.toString().slice(-7)}_plec_${tipo}_${modalidad}_validacion.pdf`;
}

function getSuggestedContext(classification) {
  return 'DOCUMENTOS_VALIDACION'; // Siempre sugerir validación para nuevos pliegos
}

async function extractPliegoStructure(pdfPath) {
  // Implementar extracción de estructura (secciones, campos, etc.)
  return {
    sections: [],
    fields: [],
    structure: {}
  };
}

function extractTemplateStructure(templateResults) {
  // Extraer estructura de resultados de plantilla
  return {
    sections: [],
    requiredFields: [],
    optionalFields: []
  };
}

function compareStructures(current, template) {
  // Implementar comparación estructural
  return {
    missingSections: [],
    extraSections: [],
    fieldDifferences: []
  };
}

function calculateStructuralScore(comparison) {
  // Calcular puntuación de similitud estructural
  return 75; // Placeholder
}

function generateComparisonRecommendations(comparison) {
  // Generar recomendaciones basadas en comparación
  return [
    'Verificar secciones obligatorias',
    'Completar campos faltantes',
    'Alinear estructura con plantilla'
  ];
}
