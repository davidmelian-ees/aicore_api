import { PDFDocument, rgb, StandardFonts } from 'pdf-lib';
import * as pdfjsLib from 'pdfjs-dist';
import fs from 'fs/promises';
import { processDocument } from './documentProcessor.js';
import { getAiCoreClient } from '../auth/aiCoreClient.js';
import { searchContext } from './ragService.js';
import { recordValidationMetrics, classifyPliego } from './pliegoAnalyticsService.js';
import path from 'path';

/**
 * Servicio para corrección de PDFs con dos enfoques:
 * 1. Generar PDF con lista de correcciones
 * 2. Aplicar correcciones directamente por replace
 */

// Función para limpiar y normalizar texto con caracteres especiales
function cleanTextEncoding(text) {
  if (!text) return text;
  
  return text
    // Reemplazar caracteres mal codificados comunes del euro
    .replace(/â‚¬/g, 'EUR')         // Euro mal codificado
    .replace(/Â€/g, 'EUR')          // Euro mal codificado variante
    .replace(/€/g, 'EUR')           // Euro normal a EUR
    .replace(/\?\)/g, ' EUR')       // ?) a EUR (común en PDFs mal codificados)
    .replace(/\?{1,2}\s*\)/g, ' EUR') // ? o ?? seguido de ) a EUR
    // Otros caracteres mal codificados
    .replace(/â€™/g, "'")           // Apóstrofe
    .replace(/â€˜/g, "'")           // Apóstrofe apertura
    .replace(/â€œ/g, '"')           // Comilla doble apertura
    .replace(/â€\u009d/g, '"')      // Comilla doble cierre
    .replace(/â€"/g, '-')           // Guión
    .replace(/â€"/g, '—')           // Guión largo
    // Apóstrofes catalanes (IMPORTANTE: antes de limpiar ?)
    // Usar regex global para capturar TODOS los casos (mayúsculas, minúsculas, mixtas)
    .replace(/([DdLlSsNnMmTt])\?/g, "$1'")  // Cualquier letra + ? -> letra + '
    // Caracteres catalanes
    .replace(/Ã /g, 'à')            // à catalana
    .replace(/Ã¨/g, 'è')            // è catalana
    .replace(/Ã©/g, 'é')            // é catalana
    .replace(/Ã­/g, 'í')            // í catalana
    .replace(/Ã²/g, 'ò')            // ò catalana
    .replace(/Ã³/g, 'ó')            // ó catalana
    .replace(/Ãº/g, 'ú')            // ú catalana
    .replace(/Ã§/g, 'ç')            // ç catalana
    .replace(/Ã±/g, 'ñ')            // ñ
    .replace(/Â·/g, '·')            // punt volat
    .replace(/â€¢/g, '•')           // bullet
    .replace(/\uFFFD/g, '')         // Carácter de reemplazo Unicode
    .replace(/\?{2,}/g, '\n')       // múltiples ?? a salto de línea (DESPUÉS de apóstrofes)
    .replace(/\[DOCUMENTO\]/g, '')  // eliminar marcador [DOCUMENTO]
    // Normalizar saltos de línea
    .replace(/\r\n/g, '\n')
    .replace(/\r/g, '\n')
    // Normalizar formato de ubicación (quitar emojis y añadir indentación)
    .replace(/📍 Ubicación:/g, '\n    - Ubicación:')
    .replace(/📄 Contexto:/g, '\n    - Contexto:')
    // Limpiar líneas vacías múltiples
    .replace(/\n{3,}/g, '\n\n')
    // Limpiar espacios al final de cada línea pero PRESERVAR indentación al inicio
    .split('\n')
    .map(line => {
      // Si la línea tiene "- Ubicación:" o "- Contexto:", preservar los 4 espacios
      if (line.includes('- Ubicación:') || line.includes('- Contexto:')) {
        return line.trimEnd();
      }
      // Para otras líneas, limpiar espacios múltiples pero no al inicio si es indentación
      return line.replace(/\s+/g, ' ').trimEnd();
    })
    .join('\n');
}

// Función para cargar prompts de validación
async function loadValidationPrompts() {
  try {
    const promptsDir = path.join(process.cwd(), 'prompts_dev');
    
    const [
      nomenclatura,
      erroresComunes,
      validationSystem,
      analisisPliegos,
      ejemplosErrores
    ] = await Promise.all([
      fs.readFile(path.join(promptsDir, 'NOMENCLATURA_PLIEGOS.txt'), 'utf8'),
      fs.readFile(path.join(promptsDir, 'ERRORES_COMUNES_PLIEGOS.txt'), 'utf8'),
      fs.readFile(path.join(promptsDir, 'PLIEGOS_VALIDATION_SYSTEM.txt'), 'utf8'),
      fs.readFile(path.join(promptsDir, 'ANALISIS_PLIEGOS_GENERADOS.txt'), 'utf8'),
      fs.readFile(path.join(promptsDir, 'PLIEGOS_ERRORES_EJEMPLOS.txt'), 'utf8')
    ]);
    
    return {
      nomenclatura,
      erroresComunes,
      validationSystem,
      analisisPliegos,
      ejemplosErrores
    };
  } catch (error) {
    console.error('[PDF-CORRECTION] Error cargando prompts:', error);
    return null;
  }
}

// Función para construir el prompt de validación específico
async function buildValidationPrompt(textForAnalysis, ragContext = null, learnedPatterns = null, visualErrors = null) {
  const prompts = await loadValidationPrompts();
  
  if (!prompts) {
    // Fallback si no se pueden cargar los prompts
    return `Analiza el siguiente texto de pliego y genera un informe de validación con errores encontrados:

TEXTO A ANALIZAR:
${textForAnalysis}

Genera un informe detallado de errores estructurales, ortográficos y de formato encontrados.`;
  }

  // Construir prompt completo usando los archivos de prompts_dev
  return `SISTEMA DE VALIDACIÓN DE PLIEGOS SAP
================================================================================

CONTEXTO DE VALIDACIÓN:
${prompts.validationSystem}

ERRORES COMUNES A DETECTAR:
${prompts.erroresComunes}

EJEMPLOS DE ERRORES REALES (ENTRENAMIENTO):
${prompts.ejemplosErrores}

NOMENCLATURA ESPERADA:
${prompts.nomenclatura}

${ragContext ? `CONTEXTO RAG ADICIONAL (EJEMPLOS Y PLANTILLAS):
${ragContext}

` : ''}${learnedPatterns ? `
================================================================================
PATRONES APRENDIDOS DEL CONTEXTO (VALIDACIÓN BASADA EN EJEMPLOS REALES):
================================================================================

⚠️ INSTRUCCIÓN CRÍTICA: Los siguientes patrones fueron extraídos de ${learnedPatterns.documentsAnalyzed} pliegos reales del contexto.
DEBES validar que el pliego actual siga estos patrones comunes.
Si el pliego NO tiene algún elemento que aparece en el 70%+ de los documentos, REPORTA como ADVERTENCIA.

${learnedPatterns.patternsText || 'Analizando patrones...'}

VALIDACIÓN DE ANOMALÍAS:
- Si una sección aparece en 8+ documentos pero NO en este: 🟡 ADVERTENCIA
- Si un punto de numeración aparece en 8+ documentos pero NO en este: 🟡 ADVERTENCIA  
- Si una tabla común aparece en 8+ documentos pero NO en este: 🟡 ADVERTENCIA
- Si el orden de secciones es diferente al patrón común: 🟡 ADVERTENCIA

` : ''}${visualErrors ? `
================================================================================
ERRORES VISUALES DETECTADOS (ANÁLISIS AUTOMÁTICO DEL PDF):
================================================================================

⚠️ IMPORTANTE: Los siguientes errores fueron detectados automáticamente mediante análisis visual del PDF.
DEBES incluirlos en tu informe final como ADVERTENCIAS.

${visualErrors}

` : ''}================================================================================
INSTRUCCIONES DE VALIDACIÓN:
================================================================================

⚠️ INSTRUCCIONES CRÍTICAS OBLIGATORIAS:

1. ANALIZA el siguiente texto de pliego MINUCIOSAMENTE

2. IDENTIFICA errores según los patrones definidos arriba

⚠️ OBLIGATORIO - UBICACIÓN DE ERRORES:
   Para CADA error detectado, DEBES incluir:
   - 📍 Ubicación: El apartado/sección exacto donde se encuentra (ej: "18.- DOCUMENTACIÓ A PRESENTAR")
   - 📄 Contexto: La tabla, cuadro o párrafo específico donde aparece el error
   
   NUNCA reportes un error sin indicar su ubicación exacta en el documento.

3. REALIZA CÁLCULOS MATEMÁTICOS EXPLÍCITOS:
   - Si encuentras "PRESSUPOST DE LICITACIÓ" o "PRESUPUESTO DE LICITACIÓN"
   - EXTRAE el importe total declarado
   - BUSCA la tabla de lotes inmediatamente después
   - EXTRAE todos los importes de cada lote
   - SUMA manualmente: Lot1 + Lot2 + Lot3 + ... = TOTAL
   - COMPARA: ¿TOTAL calculado == TOTAL declarado?
   - Si NO coinciden: REPORTA como ERROR CRÍTICO con cálculos explícitos

4. VALIDA TABLAS APLICA/NO APLICA COLUMNA POR COLUMNA:
   - Si encuentras tabla con columnas "APLICA" y "NO APLICA"
   - CUENTA el número de columnas en el encabezado (debe ser 2)
   - Para CADA fila: CUENTA cuántos valores tiene
   - Si una fila tiene solo 1 valor cuando la tabla tiene 2 columnas: ERROR CRÍTICO
   - IDENTIFICA exactamente qué filas tienen campos vacíos
   - REPORTA con número de fila y nombre del criterio

5. DETECTA COMENTARIOS DE DESARROLLADORES Y TAGS SAP:
   - BUSCA texto con nombre + dos puntos: "Oriol:", "David:", "Maria:"
   - BUSCA instrucciones técnicas: "S'haurà de treure", "no treure", "Escollir"
   - BUSCA variables SAP sin reemplazar que empiecen con Z: ZRM_, ZVRM_, ZVRM_QDC_
   - BUSCA referencias a tablas SAP: "si hi ha valors a la taula ZRM_"
   - BUSCA condiciones técnicas: "Si ZVRM_QDC_CLO_LIC-ZZ_NUM_LOT = 000"
   - Si encuentras CUALQUIERA de estos: ERROR CRÍTICO

6. GENERA un informe detallado con:
   - Errores críticos (bloquean generación)
   - Advertencias (permiten continuar)
   - Sugerencias de corrección específicas
   - Campos variables detectados
   - CÁLCULOS EXPLÍCITOS para errores numéricos
   - UBICACIÓN EXACTA de cada error (sección, apartado, tabla)

7. FORMATO DE RESPUESTA EXACTO (COPIA ESTE FORMATO PRECISAMENTE):
================================================================================

🔴 ERRORES CRÍTICOS:
- [Descripción del error]
    - Ubicación: [Sección/Apartado exacto donde se encuentra]
    - Contexto: [Tabla, cuadro o párrafo específico]

🟡 ADVERTENCIAS:
- [Descripción de la advertencia]
    - Ubicación: [Sección/Apartado exacto donde se encuentra]
    - Contexto: [Tabla, cuadro o párrafo específico]

✅ SUGERENCIAS:
- [Correcciones específicas recomendadas]
- [Cada sugerencia en una línea separada]

📋 CAMPOS VARIABLES DETECTADOS:
- [Lista de variables SAP encontradas]
- [Cada variable en una línea separada]

================================================================================

IMPORTANTE:
- Usa EXACTAMENTE los emojis y títulos mostrados arriba
- NO uses símbolos de euro (€), usa "EUR" en su lugar
- Cada sección debe empezar con el emoji correspondiente
- Usa guiones (-) para listas
- No uses números ni letras para listas
- Si no hay elementos en una sección, omítela completamente
- Mantén el formato limpio sin símbolos extra (#, *, etc.)
- Todos los importes deben expresarse como "29.040.000,00 EUR" (sin símbolo €)

⚠️ EJEMPLO 1 - FORMATO CON UBICACIÓN (TAG SIN REEMPLAZAR):

Si encuentras en el texto:
"18.- DOCUMENTACIÓ A PRESENTAR PER LES EMPRESES LICITADORES
 QUADRE D'APARTATS/SUBAPARTATS D'APLICACIÓ
 {B}CRITERIS{/B}    APLICA    NO APLICA"

DEBES REPORTAR:
🔴 ERRORES CRÍTICOS:
- Tag SAP sin reemplazar: {B}CRITERIS{/B}
    - Ubicación: Apartado 18.- DOCUMENTACIÓ A PRESENTAR PER LES EMPRESES LICITADORES
    - Contexto: QUADRE D'APARTATS/SUBAPARTATS D'APLICACIÓ

⚠️ EJEMPLO 2 - VALIDACIÓN NUMÉRICA CON UBICACIÓN:

Si encuentras en el texto:
"2.- DADES ECONÒMIQUES
 PRESSUPOST DE LICITACIÓ: 243.936,00 euros (IVA inclòs)
 Lot 1: 241.840,28 euros
 Lot 2: 1.942,72 euros"

DEBES hacer:
1. Extraer: 243.936,00 (presupuesto declarado)
2. Extraer lotes: 241.840,28 y 1.942,72
3. SUMAR: 241.840,28 + 1.942,72 = 243.783,00
4. COMPARAR: 243.936,00 ≠ 243.783,00
5. DIFERENCIA: 153,00 euros
6. REPORTAR:
🔴 ERRORES CRÍTICOS:
- Incoherencia numérica: Presupuesto declarado (243.936,00 EUR) no coincide con suma de lotes (243.783,00 EUR). Diferencia: 153,00 EUR
    - Ubicación: Apartado 2.- DADES ECONÒMIQUES
    - Contexto: PRESSUPOST DE LICITACIÓ - Tabla de lotes

⚠️ EJEMPLO 3 - VALIDACIÓN TABLAS APLICA/NO APLICA CON UBICACIÓN:

Si encuentras en el texto:
"15.- CRITERIS D'ADJUDICACIÓ
 QUADRE RESUM DE CRITERIS
 1.03 Compromís sobre subcontractació    APLICA    APLICA
 1.04 Compromís sobre emissions CO2eq    APLICA
 1.05 Declaracions Ambientals            APLICA    APLICA
 1.06 Utilització de fusta certificada   APLICA"

DEBES hacer:
1. Identificar tabla con 2 columnas: APLICA | NO APLICA
2. Contar valores por fila:
   - Fila 1.03: 2 valores ✅
   - Fila 1.04: 1 valor ❌ (falta columna NO APLICA)
   - Fila 1.05: 2 valores ✅
   - Fila 1.06: 1 valor ❌ (falta columna NO APLICA)
3. REPORTAR:
🔴 ERRORES CRÍTICOS:
- Tabla APLICA/NO APLICA incompleta. Filas 1.04 y 1.06 tienen solo 1 valor cuando deberían tener 2
    - Ubicación: Apartado 15.- CRITERIS D'ADJUDICACIÓ
    - Contexto: QUADRE RESUM DE CRITERIS - Filas 1.04 (emissions CO2eq) y 1.06 (fusta certificada)

NO asumas que las tablas están completas. SIEMPRE cuenta los valores por fila.

⚠️ EJEMPLO 4 - DETECCIÓN DE COMENTARIOS DE DESARROLLADORES CON UBICACIÓN:

Si encuentras en el texto:
"12.- CRITERIS DE SOSTENIBILITAT
 Oriol: En cas que apliqui el CO2 (si hi ha valors a la taula ZRM_DM_MAT_CO2 o 
 ZVRM_QDC_MAT_LIC -> Escollir quina de les 2) S'haurà de treure el text en groc."

DEBES hacer:
1. Detectar nombre + dos puntos: "Oriol:"
2. Detectar instrucciones técnicas: "S'haurà de treure", "Escollir quina de les 2"
3. Detectar tags SAP: ZRM_DM_MAT_CO2, ZVRM_QDC_MAT_LIC
4. Detectar referencias a tablas SAP: "si hi ha valors a la taula"
5. REPORTAR:
🔴 ERRORES CRÍTICOS:
- Comentario de desarrollador detectado: "Oriol: En cas que apliqui el CO2..."
    - Ubicación: Apartado 12.- CRITERIS DE SOSTENIBILITAT
    - Contexto: Instrucciones técnicas que deben eliminarse. Tags SAP: ZRM_DM_MAT_CO2, ZVRM_QDC_MAT_LIC

⚠️ EJEMPLO 4 - DETECCIÓN DE CONDICIONES TÉCNICAS SAP:

Si encuentras:
"Oriol: Si ZVRM_QDC_CLO_LIC-ZZ_NUM_LOT = 000 no treure la taula següent"

DEBES hacer:
1. Detectar nombre + dos puntos: "Oriol:"
2. Detectar condición técnica: "Si ZVRM_QDC_CLO_LIC-ZZ_NUM_LOT = 000"
3. Detectar tag SAP: ZVRM_QDC_CLO_LIC-ZZ_NUM_LOT
4. Detectar instrucción: "no treure la taula"
5. REPORTAR: "🔴 ERROR CRÍTICO: Comentario de desarrollador con condición técnica SAP
   - Línea: 'Oriol: Si ZVRM_QDC_CLO_LIC-ZZ_NUM_LOT = 000...'
   - Tag SAP sin reemplazar: ZVRM_QDC_CLO_LIC-ZZ_NUM_LOT
   - Este texto debe eliminarse completamente del pliego final"

BUSCA ACTIVAMENTE estos patrones en TODO el documento.

================================================================================
TEXTO DEL PLIEGO A VALIDAR:
================================================================================

${textForAnalysis}

================================================================================
GENERA EL INFORME SIGUIENDO EL FORMATO EXACTO:
RECUERDA: 
- VERIFICA TODAS LAS SUMAS Y CÁLCULOS NUMÉRICOS
- CUENTA LOS VALORES EN CADA FILA DE TABLAS APLICA/NO APLICA
- BUSCA COMENTARIOS DE DESARROLLADORES (Oriol:, David:, etc.)
- BUSCA TAGS SAP SIN REEMPLAZAR (ZRM_, ZVRM_, etc.)
================================================================================`;
}

/**
 * Analiza patrones comunes en documentos del contexto RAG
 * @param {string} contextId - ID del contexto a analizar
 * @returns {Promise<Object>} - Patrones detectados
 */
async function analyzeContextPatterns(contextId) {
  try {
    console.log(`[PDF-CORRECTION] 🔍 Analizando patrones en contexto: ${contextId}`);
    
    // Buscar todos los documentos del contexto para análisis de patrones
    const ragResults = await searchContext(
      `estructura secciones puntos numeración índice tabla contenidos`,
      {
        contextId: contextId,
        topK: 30 // Más documentos para mejor análisis de patrones
      }
    );

    if (!ragResults || ragResults.length === 0) {
      console.log('[PDF-CORRECTION] ⚠️ No hay documentos en el contexto para análisis');
      return null;
    }

    // Construir contexto para análisis de patrones
    const documentsContext = ragResults
      .map((result, index) => {
        const metadata = result.metadata || {};
        return `
DOCUMENTO ${index + 1}: ${metadata.filename || 'Sin nombre'}
${result.content}
---`;
      })
      .join('\n');

    console.log(`[PDF-CORRECTION] 📊 Analizando ${ragResults.length} documentos para detectar patrones...`);

    // Prompt para que la IA extraiga patrones comunes
    const patternAnalysisPrompt = `Analiza los siguientes ${ragResults.length} documentos de pliegos y extrae PATRONES COMUNES:

${documentsContext}

INSTRUCCIONES:
1. Identifica secciones que aparecen en TODOS o MAYORÍA de documentos
2. Detecta puntos de numeración que se repiten (ej: punto 18, punto 25, etc.)
3. Encuentra tablas o estructuras comunes
4. Identifica campos variables que siempre están presentes
5. Detecta orden típico de secciones

FORMATO DE RESPUESTA:

SECCIONES COMUNES (aparecen en X de ${ragResults.length} documentos):
- [Nombre de sección]: [Frecuencia]

PUNTOS DE NUMERACIÓN COMUNES:
- Punto [número]: [Descripción] - Aparece en [X] documentos

TABLAS COMUNES:
- [Tipo de tabla]: [Frecuencia]

CAMPOS VARIABLES COMUNES:
- [Nombre del campo]: [Frecuencia]

ORDEN TÍPICO DE SECCIONES:
1. [Sección 1]
2. [Sección 2]
...

Genera SOLO los patrones que aparecen en al menos el 70% de los documentos.`;

    return {
      documentsAnalyzed: ragResults.length,
      analysisPrompt: patternAnalysisPrompt
    };

  } catch (error) {
    console.error('[PDF-CORRECTION] ❌ Error analizando patrones:', error);
    return null;
  }
}

/**
 * Genera un PDF con lista de correcciones basado en contexto RAG
 * @param {string} prompt - Prompt personalizado para la validación
 * @param {string} contextId - ID del contexto RAG a usar
 * @returns {Promise<Object>} - PDF con correcciones basadas en contexto
 */
export async function generatePDFWithCorrectionsFromContext(prompt, contextId) {
  const startTime = Date.now();
  try {
    console.log(`[PDF-CORRECTION] Generando PDF desde contexto RAG: ${contextId}`);

    // 1. Cargar prompts de validación
    const prompts = await loadValidationPrompts();

    // 2. NUEVO: Analizar patrones del contexto
    const patternsAnalysis = await analyzeContextPatterns(contextId);
    
    // 2.1 Si hay patrones, extraerlos con la IA
    let learnedPatterns = null;
    if (patternsAnalysis) {
      console.log(`[PDF-CORRECTION] 🤖 Extrayendo patrones con IA...`);
      try {
        const client = getAiCoreClient('gpt-4o');
        const patternResponse = await client.run({
          messages: [{ role: 'user', content: patternsAnalysis.analysisPrompt }]
        });
        
        const patternsText = patternResponse.getContent();
        learnedPatterns = {
          documentsAnalyzed: patternsAnalysis.documentsAnalyzed,
          patternsText: patternsText
        };
        
        console.log(`[PDF-CORRECTION] ✅ Patrones extraídos de ${patternsAnalysis.documentsAnalyzed} documentos`);
      } catch (error) {
        console.error('[PDF-CORRECTION] ⚠️ Error extrayendo patrones:', error.message);
      }
    }

    // 3. Buscar documentos relevantes en el contexto RAG
    console.log(`[PDF-CORRECTION] Buscando documentos en contexto: ${contextId}`);

    const ragResults = await searchContext(
      `errores pliegos validación tags SAP campos variables ${prompt.substring(0, 200)}`,
      {
        contextId: contextId,
        topK: 15 // Más documentos para análisis de contexto
      }
    );

    if (!ragResults || ragResults.length === 0) {
      throw new Error(`No se encontraron documentos relevantes en el contexto ${contextId}`);
    }

    console.log(`[PDF-CORRECTION] Encontrados ${ragResults.length} documentos relevantes`);

    // 3. Preparar el texto del contexto para análisis
    const contextText = ragResults
      .map(result => `DOCUMENTO: ${result.metadata?.fileName || 'Sin nombre'}
CONTENIDO: ${result.content}
RELEVANCIA: ${result.similarity}
TIPO: ${result.metadata?.type || 'Desconocido'}
---`)
      .join('\n\n');

    // Limitar el texto si es muy largo
    let textForAnalysis = contextText;
    if (contextText.length > 30000) {
      console.warn(`[PDF-CORRECTION] Texto de contexto muy largo: ${contextText.length} caracteres, truncando...`);
      textForAnalysis = contextText.substring(0, 30000) + '\n\n[TEXTO DE CONTEXTO TRUNCADO...]';
    }

    // 4. Construir prompt con patrones aprendidos
    const contextAnalysisPrompt = await buildValidationPrompt(
      `${prompt}\n\n${textForAnalysis}`,
      textForAnalysis,
      learnedPatterns
    );

    console.log(`[PDF-CORRECTION] Generando análisis con SAP AI Core (${contextAnalysisPrompt.length} caracteres)...`);

    let correctionsReport;
    try {
      const client = getAiCoreClient('gpt-4o');
      const response = await client.run({
        messages: [{ role: 'user', content: contextAnalysisPrompt }]
      });

      correctionsReport = response.getContent();

      if (!correctionsReport || correctionsReport.trim().length === 0) {
        throw new Error('SAP AI Core devolvió una respuesta vacía');
      }

      console.log(`[PDF-CORRECTION] Análisis generado: ${correctionsReport.length} caracteres`);

    } catch (aiError) {
      console.error(`[PDF-CORRECTION] Error detallado en SAP AI Core:`, {
        message: aiError.message,
        status: aiError.status || aiError.code,
        response: aiError.response?.data || 'No response data',
        promptLength: contextAnalysisPrompt.length
      });

      // Fallback: generar reporte básico sin IA
      console.log(`[PDF-CORRECTION] Usando fallback sin IA...`);
      correctionsReport = `ANÁLISIS DE CONTEXTO - REPORTE BÁSICO

No se pudo generar análisis automático con IA.

Documentos analizados: ${ragResults.length}
Contexto utilizado: ${contextId}

Se encontraron ${ragResults.length} documentos relevantes que pueden servir como referencia
para validación manual de pliegos SAP.

Para análisis completo, verificar conexión con SAP AI Core.`;
    }

    // 5. Crear PDF SOLO con el informe de análisis de contexto
    const newPdf = await PDFDocument.create();

    // 6. Añadir páginas del informe
    await addContextAnalysisReportPages(newPdf, correctionsReport, contextId, ragResults.length);

    // 7. Generar PDF final
    const finalPdfBytes = await newPdf.save();
    const pdfBuffer = Buffer.from(finalPdfBytes);

    console.log(`[PDF-CORRECTION] PDF de análisis de contexto generado: ${pdfBuffer.length} bytes`);

    return {
      success: true,
      pdfBuffer,
      contextId,
      documentsAnalyzed: ragResults.length,
      analysisReport: correctionsReport,
      metadata: {
        processedAt: new Date().toISOString(),
        contextId: contextId,
        documentsCount: ragResults.length,
        analysisLength: correctionsReport.length,
        processingTime: Date.now() - startTime
      }
    };

  } catch (error) {
    console.error('[PDF-CORRECTION] Error generando PDF desde contexto:', error);
    throw new Error(`Error generando análisis de contexto: ${error.message}`);
  }
}
export async function generatePDFWithCorrectionsList(originalPdfPath, customPrompt = null, contextId = null, visualErrors = null) {
  const startTime = Date.now();
  try {
    console.log(`[PDF-CORRECTION] Generando PDF con lista de correcciones...`);
    
    // 1. Extraer texto del PDF original
    const documentData = await processDocument(originalPdfPath, 'application/pdf');
    let originalText = documentData.chunks.map(chunk => chunk.content).join('\n\n');
    
    // 1.5. Limpiar caracteres mal codificados del PDF antes de analizar
    originalText = cleanTextEncoding(originalText);
    
    // 2. Limitar texto para SAP AI Core (máximo 50,000 caracteres)
    let textForAnalysis = originalText;
    if (originalText.length > 50000) {
      console.warn(`[PDF-CORRECTION] Texto muy largo: ${originalText.length} caracteres, truncando...`);
      textForAnalysis = originalText.substring(0, 50000) + '\n\n[TEXTO TRUNCADO...]';
    }

    // 3. Cargar prompts de validación
    const prompts = await loadValidationPrompts();
    
    // 4. Obtener contexto RAG y analizar patrones si se especifica
    let ragContext = '';
    let learnedPatterns = null;
    
    if (contextId) {
      try {
        console.log(`[PDF-CORRECTION] Cargando contexto RAG: ${contextId}`);
        
        // 4.1 Analizar patrones comunes en el contexto
        console.log(`[PDF-CORRECTION] Analizando patrones del contexto...`);
        const patternsAnalysis = await analyzeContextPatterns(contextId);
        
        if (patternsAnalysis) {
          learnedPatterns = patternsAnalysis;
          console.log(`[PDF-CORRECTION] Patrones aprendidos: ${patternsAnalysis.documentsAnalyzed} documentos analizados`);
        }
        
        // 4.2 Buscar documentos relevantes en el contexto RAG
        const ragResults = await searchContext(
          `estructura secciones apartados numeración tablas formato ${textForAnalysis.substring(0, 500)}`,
          {
            contextId: contextId,
            topK: 15 // Más documentos para mejor comparación
          }
        );
        
        if (ragResults && ragResults.length > 0) {
          ragContext = ragResults
            .map(result => `DOCUMENTO: ${result.metadata?.fileName || 'Sin nombre'}
CONTENIDO: ${result.content}
RELEVANCIA: ${result.similarity}
---`)
            .join('\n');
          
          console.log(`[PDF-CORRECTION] Contexto RAG cargado: ${ragResults.length} documentos relevantes`);
        } else {
          console.log(`[PDF-CORRECTION] No se encontraron documentos relevantes en contexto ${contextId}`);
        }
      } catch (error) {
        console.warn(`[PDF-CORRECTION] Error cargando contexto RAG: ${error.message}`);
      }
    }
    
    // 5. Generar prompt de validación específico para pliegos (incluyendo patrones aprendidos y errores visuales)
    const correctionPrompt = customPrompt || await buildValidationPrompt(textForAnalysis, ragContext, learnedPatterns, visualErrors);

    console.log(`[PDF-CORRECTION] Generando correcciones con SAP AI Core (${correctionPrompt.length} caracteres)...`);
    
    let correctionsList;
    try {
      const client = getAiCoreClient('gpt-4o', { 
        temperature: 0.2,  // Temperatura muy baja para validación consistente y precisa
        maxTokens: 4000 
      });
      const response = await client.run({
        messages: [{ role: 'user', content: correctionPrompt }],
        temperature: 0.2,  // Temperatura baja = respuestas más deterministas y precisas
        max_tokens: 4000
      });
      
      correctionsList = response.getContent();
      
      if (!correctionsList || correctionsList.trim().length === 0) {
        throw new Error('SAP AI Core devolvió una respuesta vacía');
      }
      
      // Limpiar caracteres mal codificados
      correctionsList = cleanTextEncoding(correctionsList);
      
      console.log(`[PDF-CORRECTION] Correcciones generadas: ${correctionsList.length} caracteres`);
      
    } catch (aiError) {
      console.error(`[PDF-CORRECTION] Error detallado en SAP AI Core:`, {
        message: aiError.message,
        status: aiError.status || aiError.code,
        response: aiError.response?.data || 'No response data',
        promptLength: correctionPrompt.length
      });
      
      // Fallback: generar lista básica sin IA
      console.log(`[PDF-CORRECTION] Usando fallback sin IA...`);
      correctionsList = "No se pudieron generar correcciones automáticas.\nRevise el documento manualmente para errores ortográficos.";
    }
    
    // 3. Crear PDF SOLO con el informe de validación (sin PDF original)
    const newPdf = await PDFDocument.create();
    
    // 4. Añadir páginas del informe de validación
    await addValidationReportPages(newPdf, correctionsList);
    
    // 6. Generar PDF final
    const finalPdfBytes = await newPdf.save();
    const pdfBuffer = Buffer.from(finalPdfBytes);
    
    console.log(`[PDF-CORRECTION] PDF con correcciones generado: ${pdfBuffer.length} bytes`);
    
    // 7. Clasificar automáticamente el pliego para métricas
    let pliegoClassification = null;
    try {
      pliegoClassification = await classifyPliego(originalPdfPath);
      console.log(`[PDF-CORRECTION] Pliego clasificado: ${pliegoClassification.tipo}_${pliegoClassification.modalidad} (${pliegoClassification.confidence}% confianza)`);
    } catch (error) {
      console.warn(`[PDF-CORRECTION] Error clasificando pliego: ${error.message}`);
    }
    
    // 8. Registrar métricas de validación
    try {
      const metricsId = recordValidationMetrics({
        pdfPath: originalPdfPath,
        errorsFound: correctionsList ? [correctionsList] : [],
        contextId: contextId,
        processingTime: Date.now() - startTime,
        pliegoType: pliegoClassification?.tipo,
        pliegoModality: pliegoClassification?.modalidad
      });
      console.log(`[PDF-CORRECTION] Métricas registradas: ${metricsId}`);
    } catch (error) {
      console.warn(`[PDF-CORRECTION] Error registrando métricas: ${error.message}`);
    }
    
    return {
      success: true,
      pdfBuffer,
      correctionsList,
      totalPageCount: newPdf.getPageCount(),
      classification: pliegoClassification,
      metrics: {
        processedAt: new Date().toISOString(),
        originalTextLength: originalText.length,
        correctionsLength: correctionsList.length,
        processingTime: Date.now() - startTime
      },
      metadata: {
        processedAt: new Date().toISOString(),
        originalTextLength: originalText.length,
        correctionsLength: correctionsList.length
      }
    };
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error generando PDF con correcciones:', error);
    throw new Error(`Error generando PDF con correcciones: ${error.message}`);
  }
}

/**
 * Añade páginas del informe de análisis de archivo + contexto al PDF
 * @param {PDFDocument} pdf - Documento PDF
 * @param {string} analysisReport - Informe de análisis
 * @param {string} contextId - ID del contexto
 * @param {number} documentsCount - Número de documentos de referencia
 * @param {string} fileName - Nombre del archivo analizado
 */
async function addFileAndContextAnalysisReportPages(pdf, analysisReport, contextId, documentsCount, fileName) {
  const font = await pdf.embedFont(StandardFonts.Helvetica);
  const boldFont = await pdf.embedFont(StandardFonts.HelveticaBold);

  const pageHeight = 792; // Tamaño carta
  const pageWidth = 612;
  const margin = 50;
  const lineHeight = 16;
  const maxWidth = pageWidth - 2 * margin;
  const maxLinesPerPage = Math.floor((pageHeight - 2 * margin - 80) / lineHeight);

  let currentPage = pdf.addPage([pageWidth, pageHeight]);
  let yPosition = pageHeight - margin;
  let lineCount = 0;

  // Título principal
  currentPage.drawText('ANÁLISIS DE PLIEGO CON CONTEXTO', {
    x: margin,
    y: yPosition,
    size: 18,
    font: boldFont,
    color: rgb(0, 0, 0)
  });

  yPosition -= 30;

  // Información del archivo y contexto
  currentPage.drawText(`Archivo analizado: ${fileName}`, {
    x: margin,
    y: yPosition,
    size: 12,
    font: font,
    color: rgb(0.4, 0.4, 0.4)
  });

  yPosition -= 20;

  currentPage.drawText(`Contexto de referencia: ${contextId}`, {
    x: margin,
    y: yPosition,
    size: 12,
    font: font,
    color: rgb(0.4, 0.4, 0.4)
  });

  yPosition -= 20;

  currentPage.drawText(`Documentos de referencia: ${documentsCount}`, {
    x: margin,
    y: yPosition,
    size: 12,
    font: font,
    color: rgb(0.4, 0.4, 0.4)
  });

  yPosition -= 30;

  // Fecha y hora
  const now = new Date();
  const dateStr = now.toLocaleDateString('es-ES') + ' ' + now.toLocaleTimeString('es-ES');
  currentPage.drawText(`Generado: ${dateStr}`, {
    x: margin,
    y: yPosition,
    size: 10,
    font: font,
    color: rgb(0.5, 0.5, 0.5)
  });

  yPosition -= 40;
  lineCount += 7;

  // Procesar el contenido línea por línea
  const lines = analysisReport.split('\n');

  for (const line of lines) {
    // Limpiar línea de respuesta de IA antes de procesar
    const cleanedLine = cleanAIResponseLine(line.trim());

    // Verificar si necesitamos nueva página
    if (lineCount >= maxLinesPerPage) {
      currentPage = pdf.addPage([pageWidth, pageHeight]);
      yPosition = pageHeight - margin;
      lineCount = 0;

      // Título en nueva página
      currentPage.drawText('ANÁLISIS DE PLIEGO (continuación)', {
        x: margin,
        y: yPosition,
        size: 16,
        font: boldFont,
        color: rgb(0, 0, 0)
      });

      yPosition -= 40;
      lineCount += 2;
    }

    // Procesar línea
    const processedLine = processLineFormatting(cleanedLine);

    if (processedLine.text.length === 0) {
      // Línea vacía - añadir espacio
      yPosition -= lineHeight * 0.5;
      lineCount += 0.5;
      continue;
    }

    // Dividir líneas largas
    const wrappedLines = wrapText(processedLine.text, maxWidth, processedLine.isBold ? boldFont : font, processedLine.fontSize);

    for (const wrappedLine of wrappedLines) {
      if (lineCount >= maxLinesPerPage) {
        currentPage = pdf.addPage([pageWidth, pageHeight]);
        yPosition = pageHeight - margin;
        lineCount = 0;
      }

      // Limpiar caracteres especiales para WinAnsi
      const cleanLine = cleanTextForPDF(wrappedLine);

      currentPage.drawText(cleanLine, {
        x: margin + processedLine.indent,
        y: yPosition,
        size: processedLine.fontSize,
        font: processedLine.isBold ? boldFont : font,
        color: processedLine.color
      });

      yPosition -= lineHeight;
      lineCount++;
    }
  }
}
async function addContextAnalysisReportPages(pdf, analysisReport, contextId, documentsCount) {
  const font = await pdf.embedFont(StandardFonts.Helvetica);
  const boldFont = await pdf.embedFont(StandardFonts.HelveticaBold);

  const pageHeight = 792; // Tamaño carta
  const pageWidth = 612;
  const margin = 50;
  const lineHeight = 16;
  const maxWidth = pageWidth - 2 * margin;
  const maxLinesPerPage = Math.floor((pageHeight - 2 * margin - 80) / lineHeight);

  let currentPage = pdf.addPage([pageWidth, pageHeight]);
  let yPosition = pageHeight - margin;
  let lineCount = 0;

  // Título principal
  currentPage.drawText('ANÁLISIS DE CONTEXTO PARA VALIDACIÓN', {
    x: margin,
    y: yPosition,
    size: 18,
    font: boldFont,
    color: rgb(0, 0, 0)
  });

  yPosition -= 30;

  // Información del contexto
  currentPage.drawText(`Contexto: ${contextId}`, {
    x: margin,
    y: yPosition,
    size: 12,
    font: font,
    color: rgb(0.4, 0.4, 0.4)
  });

  yPosition -= 20;

  currentPage.drawText(`Documentos analizados: ${documentsCount}`, {
    x: margin,
    y: yPosition,
    size: 12,
    font: font,
    color: rgb(0.4, 0.4, 0.4)
  });

  yPosition -= 30;

  // Fecha y hora
  const now = new Date();
  const dateStr = now.toLocaleDateString('es-ES') + ' ' + now.toLocaleTimeString('es-ES');
  currentPage.drawText(`Generado: ${dateStr}`, {
    x: margin,
    y: yPosition,
    size: 10,
    font: font,
    color: rgb(0.5, 0.5, 0.5)
  });

  yPosition -= 40;
  lineCount += 6;

  // Procesar el contenido línea por línea
  const lines = analysisReport.split('\n');

  for (const line of lines) {
    // Limpiar línea de respuesta de IA antes de procesar
    const cleanedLine = cleanAIResponseLine(line.trim());

    // Verificar si necesitamos nueva página
    if (lineCount >= maxLinesPerPage) {
      currentPage = pdf.addPage([pageWidth, pageHeight]);
      yPosition = pageHeight - margin;
      lineCount = 0;

      // Título en nueva página
      currentPage.drawText('ANÁLISIS DE CONTEXTO (continuación)', {
        x: margin,
        y: yPosition,
        size: 16,
        font: boldFont,
        color: rgb(0, 0, 0)
      });

      yPosition -= 40;
      lineCount += 2;
    }

    // Procesar línea
    const processedLine = processLineFormatting(cleanedLine);

    if (processedLine.text.length === 0) {
      // Línea vacía - añadir espacio
      yPosition -= lineHeight * 0.5;
      lineCount += 0.5;
      continue;
    }

    // Dividir líneas largas
    const wrappedLines = wrapText(processedLine.text, maxWidth, processedLine.isBold ? boldFont : font, processedLine.fontSize);

    for (const wrappedLine of wrappedLines) {
      if (lineCount >= maxLinesPerPage) {
        currentPage = pdf.addPage([pageWidth, pageHeight]);
        yPosition = pageHeight - margin;
        lineCount = 0;
      }

      // Limpiar caracteres especiales para WinAnsi
      const cleanLine = cleanTextForPDF(wrappedLine);

      currentPage.drawText(cleanLine, {
        x: margin + processedLine.indent,
        y: yPosition,
        size: processedLine.fontSize,
        font: processedLine.isBold ? boldFont : font,
        color: processedLine.color
      });

      yPosition -= lineHeight;
      lineCount++;
    }
  }
}
async function addValidationReportPages(pdf, validationReport) {
  const font = await pdf.embedFont(StandardFonts.Helvetica);
  const boldFont = await pdf.embedFont(StandardFonts.HelveticaBold);
  
  const pageHeight = 792; // Tamaño carta
  const pageWidth = 612;
  const margin = 50;
  const lineHeight = 16;
  const maxWidth = pageWidth - 2 * margin;
  const maxLinesPerPage = Math.floor((pageHeight - 2 * margin - 80) / lineHeight);
  
  let currentPage = pdf.addPage([pageWidth, pageHeight]);
  let yPosition = pageHeight - margin;
  let lineCount = 0;
  
  // Título principal
  currentPage.drawText('INFORME DE VALIDACIÓN DE PLIEGO', {
    x: margin,
    y: yPosition,
    size: 18,
    font: boldFont,
    color: rgb(0, 0, 0)
  });
  
  yPosition -= 30;
  
  // Fecha y hora
  const now = new Date();
  const dateStr = now.toLocaleDateString('es-ES') + ' ' + now.toLocaleTimeString('es-ES');
  currentPage.drawText(`Generado: ${dateStr}`, {
    x: margin,
    y: yPosition,
    size: 10,
    font: font,
    color: rgb(0.5, 0.5, 0.5)
  });
  
  yPosition -= 40;
  lineCount += 4;
  
  // Procesar el contenido línea por línea
  const lines = validationReport.split('\n');
  
  for (const line of lines) {
    // Limpiar línea de respuesta de IA antes de procesar
    const cleanedLine = cleanAIResponseLine(line.trim());
    
    // Verificar si necesitamos nueva página
    if (lineCount >= maxLinesPerPage) {
      currentPage = pdf.addPage([pageWidth, pageHeight]);
      yPosition = pageHeight - margin;
      lineCount = 0;
      
      // Título en nueva página
      currentPage.drawText('INFORME DE VALIDACIÓN (continuación)', {
        x: margin,
        y: yPosition,
        size: 16,
        font: boldFont,
        color: rgb(0, 0, 0)
      });
      
      yPosition -= 40;
      lineCount += 2;
    }
    
    // Procesar línea
    const processedLine = processLineFormatting(cleanedLine);
    
    if (processedLine.text.length === 0) {
      // Línea vacía - añadir espacio
      yPosition -= lineHeight * 0.5;
      lineCount += 0.5;
      continue;
    }
    
    // Dividir líneas largas
    const wrappedLines = wrapText(processedLine.text, maxWidth, processedLine.isBold ? boldFont : font, processedLine.fontSize);
    
    for (const wrappedLine of wrappedLines) {
      if (lineCount >= maxLinesPerPage) {
        currentPage = pdf.addPage([pageWidth, pageHeight]);
        yPosition = pageHeight - margin;
        lineCount = 0;
      }
      
      // Limpiar caracteres especiales para WinAnsi (incluyendo emojis)
      const cleanLine = cleanTextForPDF(wrappedLine);
      
      currentPage.drawText(cleanLine, {
        x: margin + processedLine.indent,
        y: yPosition,
        size: processedLine.fontSize,
        font: processedLine.isBold ? boldFont : font,
        color: processedLine.color
      });
      
      yPosition -= lineHeight;
      lineCount++;
    }
  }
}

/**
 * Procesa una línea para determinar formato (negrita, color, indentación)
 */
function processLineFormatting(line) {
  let text = line;
  let isBold = false;
  let fontSize = 12;
  let color = rgb(0, 0, 0);
  let indent = 0;

  // Limpiar texto primero - remover símbolos "#" inesperados
  text = text.replace(/^#+\s*/g, '').trim();

  // Detectar y procesar texto en negritas **TEXTO**
  if (text.includes('**')) {
    text = text.replace(/\*\*(.*?)\*\*/g, '$1');
    isBold = true;
  }

  // Convertir texto a minúsculas para comparación pero mantener original para display
  const textLower = text.toLowerCase();

  // Detectar títulos y secciones con múltiples patrones
  if (text.startsWith('🔴') || text.startsWith('[ERROR CRITICO]') ||
      text.startsWith('ERRORES CRÍTICOS') || textLower.includes('errores críticos') ||
      text.startsWith('# ERRORES CRÍTICOS') || text.startsWith('### ERRORES CRÍTICOS')) {
    isBold = true;
    fontSize = 14;
    color = rgb(0.8, 0, 0); // Rojo
    // Limpiar marcadores adicionales
    text = text.replace(/^🔴\s*|^\[ERROR CRITICO\]\s*|^ERRORES CRÍTICOS\s*|^#+\s*ERRORES CRÍTICOS\s*/i, '').trim();
    if (!text) text = 'ERRORES CRÍTICOS:';
  } else if (text.startsWith('🟡') || text.startsWith('[ADVERTENCIA]') ||
             text.startsWith('ADVERTENCIAS') || textLower.includes('advertencias') ||
             text.startsWith('# ADVERTENCIAS') || text.startsWith('### ADVERTENCIAS')) {
    isBold = true;
    fontSize = 14;
    color = rgb(0.8, 0.6, 0); // Naranja
    // Limpiar marcadores adicionales
    text = text.replace(/^🟡\s*|^\[ADVERTENCIA\]\s*|^ADVERTENCIAS\s*|^#+\s*ADVERTENCIAS\s*/i, '').trim();
    if (!text) text = 'ADVERTENCIAS:';
  } else if (text.startsWith('✅') || text.startsWith('[SUGERENCIA]') ||
             text.startsWith('SUGERENCIAS') || textLower.includes('sugerencias') ||
             text.startsWith('# SUGERENCIAS') || text.startsWith('### SUGERENCIAS')) {
    isBold = true;
    fontSize = 14;
    color = rgb(0, 0.6, 0); // Verde
    // Limpiar marcadores adicionales
    text = text.replace(/^✅\s*|^\[SUGERENCIA\]\s*|^SUGERENCIAS\s*|^#+\s*SUGERENCIAS\s*/i, '').trim();
    if (!text) text = 'SUGERENCIAS:';
  } else if (text.startsWith('📋') || text.startsWith('[CAMPOS VARIABLES]') ||
             text.startsWith('CAMPOS VARIABLES') || textLower.includes('campos variables') ||
             text.startsWith('# CAMPOS VARIABLES') || text.startsWith('### CAMPOS VARIABLES')) {
    isBold = true;
    fontSize = 14;
    color = rgb(0, 0, 0.8); // Azul
    // Limpiar marcadores adicionales
    text = text.replace(/^📋\s*|^\[CAMPOS VARIABLES\]\s*|^CAMPOS VARIABLES\s*|^#+\s*CAMPOS VARIABLES\s*/i, '').trim();
    if (!text) text = 'CAMPOS VARIABLES DETECTADOS:';
  } else if (text.startsWith('===') || text.includes('================') ||
             text.startsWith('---') || text.includes('----------')) {
    // Separadores - hacer más pequeños
    fontSize = 10;
    color = rgb(0.6, 0.6, 0.6);
  } else if (textLower.startsWith('error') || textLower.includes('crítico') ||
             textLower.includes('problema grave')) {
    // Detectar líneas que mencionan errores críticos
    isBold = true;
    fontSize = 13;
    color = rgb(0.7, 0, 0); // Rojo más claro
  } else if (textLower.startsWith('advertencia') || textLower.includes('cuidado') ||
             textLower.includes('revisar')) {
    // Detectar líneas que mencionan advertencias
    fontSize = 13;
    color = rgb(0.7, 0.5, 0); // Naranja más claro
  } else if (textLower.startsWith('sugerencia') || textLower.includes('recomend') ||
             textLower.includes('considera')) {
    // Detectar líneas que mencionan sugerencias
    fontSize = 13;
    color = rgb(0, 0.5, 0); // Verde más claro
  }

  // Detectar "Ubicación:" y "Contexto:" con formato especial
  if (text.includes('- Ubicación:') || text.includes('- Ubicacion:')) {
    indent = 40;
    fontSize = 11;
    color = rgb(0, 0, 0); // Negro normal
    isBold = false;
  } else if (text.includes('- Contexto:')) {
    indent = 40;
    fontSize = 11;
    color = rgb(0.4, 0.4, 0.4); // Gris oscuro
    isBold = false;
  }
  // Detectar elementos de lista (errores principales) - NEGRITA
  else if (text.startsWith('- ') && !text.includes('Ubicación:') && !text.includes('Contexto:')) {
    indent = 20;
    isBold = true; // Errores en negrita
    fontSize = 12;
  } else if (text.startsWith('• ') || text.startsWith('· ') ||
      /^\d+\.\s/.test(text) || /^[a-zA-Z]\.\s/.test(text)) {
    indent = 20;
  } else if (text.startsWith('  - ') || text.startsWith('  • ') || text.startsWith('  · ') ||
             /^  \d+\.\s/.test(text) || /^  [a-zA-Z]\.\s/.test(text)) {
    indent = 40;
  } else if (text.startsWith('    - ') || text.startsWith('    • ') || text.startsWith('    · ')) {
    indent = 60;
  }

  return { text, isBold, fontSize, color, indent };
}

/**
 * Limpia texto para compatibilidad con WinAnsi encoding
 */
function cleanTextForPDF(text) {
  return text
    // Limpiar símbolos "#" al inicio de líneas
    .replace(/^#+\s*/gm, '')
    // Reemplazar emojis comunes con texto
    .replace(/🔴/g, '[ERROR CRITICO]')
    .replace(/🟡/g, '[ADVERTENCIA]')
    .replace(/✅/g, '[SUGERENCIA]')
    .replace(/📋/g, '[CAMPOS VARIABLES]')
    .replace(/⚠️/g, '[ATENCION]')
    .replace(/❌/g, '[X]')
    .replace(/✔️/g, '[OK]')
    .replace(/💡/g, '[IDEA]')
    .replace(/🔍/g, '[BUSCAR]')
    .replace(/📊/g, '[ESTADISTICAS]')
    .replace(/🏗️/g, '[CONSTRUCCION]')
    .replace(/📄/g, '[DOCUMENTO]')
    // Limpiar otros caracteres especiales Unicode que no están en WinAnsi
    .replace(/[^\x20-\x7E\u00A0-\u00FF]/g, '?')
    // Limpiar secuencias de puntos o guiones largos que podrían ser separadores
    .replace(/[-]{3,}/g, '---')
    .replace(/[=]{3,}/g, '===')
    // Normalizar espacios múltiples
    .replace(/\s+/g, ' ')
    .trim();
}

/**
 * Limpia líneas de respuesta de IA para eliminar formatos inesperados
 */
function cleanAIResponseLine(line) {
  return line
    // Remover marcadores markdown no deseados
    .replace(/^#{1,6}\s*/g, '')
    .replace(/^\*+\s*/g, '')
    .replace(/^•+\s*/g, '')
    // Limpiar texto entre paréntesis o corchetes al inicio si son marcadores
    .replace(/^\([^)]+\)\s*/g, '')
    .replace(/^\[[^\]]+\]\s*/g, '')
    // Normalizar espacios
    .replace(/\s+/g, ' ')
    .trim();
}

/**
 * Divide texto largo en múltiples líneas
 */
function wrapText(text, maxWidth, font, fontSize) {
  // Limpiar texto antes de procesar
  const cleanText = cleanTextForPDF(text);
  const words = cleanText.split(' ');
  const lines = [];
  let currentLine = '';
  
  for (const word of words) {
    const testLine = currentLine ? `${currentLine} ${word}` : word;
    
    try {
      const testWidth = font.widthOfTextAtSize(testLine, fontSize);
      
      if (testWidth <= maxWidth) {
        currentLine = testLine;
      } else {
        if (currentLine) {
          lines.push(currentLine);
          currentLine = word;
        } else {
          // Palabra muy larga - dividir
          lines.push(word);
        }
      }
    } catch (error) {
      // Si hay error calculando ancho, usar la línea actual
      console.warn('[PDF-CORRECTION] Error calculando ancho de texto:', error.message);
      if (currentLine) {
        lines.push(currentLine);
        currentLine = word;
      } else {
        lines.push(word);
      }
    }
  }
  
  if (currentLine) {
    lines.push(currentLine);
  }
  
  return lines.length > 0 ? lines : [''];
}

/**
 * Aplica correcciones directamente al PDF preservando el formato original
 * @param {string} originalPdfPath - Ruta del PDF original
 * @param {Array} corrections - Array de correcciones [{from: 'error', to: 'correcto'}]
 * @returns {Promise<Object>} - PDF corregido con formato preservado
 */
export async function applyCorrectionsDirectly(originalPdfPath, corrections) {
  try {
    console.log(`[PDF-CORRECTION] Aplicando ${corrections.length} correcciones preservando formato...`);
    
    // 1. Cargar PDF original
    const originalPdfBytes = await fs.readFile(originalPdfPath);
    const originalPdf = await PDFDocument.load(originalPdfBytes);
    
    // 2. Extraer texto con posiciones usando pdfjs-dist
    const textWithPositions = await extractTextWithPositions(originalPdfBytes);
    
    // 3. Crear nuevo PDF copiando páginas originales
    const correctedPdf = await PDFDocument.create();
    const pageIndices = Array.from({ length: originalPdf.getPageCount() }, (_, i) => i);
    const copiedPages = await correctedPdf.copyPages(originalPdf, pageIndices);
    
    let appliedCorrections = 0;
    const appliedChanges = [];
    
    // 4. Procesar cada página
    for (let pageIndex = 0; pageIndex < copiedPages.length; pageIndex++) {
      const page = copiedPages[pageIndex];
      const pageTextItems = textWithPositions.pages[pageIndex]?.textItems || [];
      
      // Aplicar correcciones a los elementos de texto de esta página
      for (let itemIndex = 0; itemIndex < pageTextItems.length; itemIndex++) {
        const textItem = pageTextItems[itemIndex];
        let originalText = textItem.str;
        let correctedText = originalText;
        let hasChanges = false;
        
        // Aplicar cada corrección
        for (const correction of corrections) {
          const { from, to } = correction;
          if (correctedText.includes(from)) {
            correctedText = correctedText.replace(new RegExp(escapeRegExp(from), 'g'), to);
            hasChanges = true;
            
            // Registrar cambio si no se ha registrado ya
            if (!appliedChanges.find(c => c.from === from && c.to === to && c.applied)) {
              appliedChanges.push({ from, to, applied: true, pageIndex });
              appliedCorrections++;
              console.log(`[PDF-CORRECTION] Aplicada en página ${pageIndex + 1}: "${from}" -> "${to}"`);
            }
          }
        }
        
        // Detectar si hay elementos de texto muy cercanos (texto denso)
        const nextItem = pageTextItems[itemIndex + 1];
        let isDenseText = false;
        if (nextItem) {
          const currentX = textItem.transform[4];
          const currentWidth = textItem.width || 0;
          const nextX = nextItem.transform[4];
          const gap = nextX - (currentX + currentWidth);
          
          // Si el gap es muy pequeño, es texto denso
          isDenseText = gap < (Math.abs(textItem.transform[0]) || 12) * 0.2;
        }
        
        // Si hay cambios, reemplazar el texto manteniendo posición y formato
        if (hasChanges && correctedText !== originalText) {
          try {
            // Obtener fuente y tamaño originales (aproximados)
            const fontSize = Math.abs(textItem.transform[0]) || 12;
            const font = await correctedPdf.embedFont(StandardFonts.Helvetica);
            
            // Calcular área de limpieza más precisa considerando espacios
            const originalWidth = font.widthOfTextAtSize(originalText, fontSize);
            const correctedWidth = font.widthOfTextAtSize(correctedText, fontSize);
            
            // Usar el ancho mayor + margen adicional para espacios
            const maxWidth = Math.max(originalWidth, correctedWidth);
            
            // Ajustar márgenes según densidad del texto
            let extraMargin, textHeight, verticalAdjust;
            
            if (isDenseText) {
              // Para texto denso: márgenes más conservadores
              extraMargin = fontSize * 0.3;
              textHeight = fontSize * 1.4;
              verticalAdjust = fontSize * 0.2;
              console.log(`[PDF-CORRECTION] Texto denso detectado, usando márgenes conservadores`);
            } else {
              // Para texto normal: márgenes más amplios
              extraMargin = fontSize * 0.6;
              textHeight = fontSize * 1.7;
              verticalAdjust = fontSize * 0.4;
            }
            
            const cleanWidth = maxWidth + extraMargin;
            
            // Detectar color de fondo (por defecto blanco)
            let backgroundColor = rgb(1, 1, 1); // Blanco por defecto
            
            // Calcular posición de limpieza más precisa
            const cleanX = textItem.transform[4] - (extraMargin / 2);
            const cleanY = textItem.transform[5] - verticalAdjust;
            
            // Limpiar área original con rectángulo optimizado
            page.drawRectangle({
              x: cleanX,
              y: cleanY,
              width: cleanWidth,
              height: textHeight,
              color: backgroundColor,
              borderWidth: 0 // Sin borde
            });
            
            // Intentar detectar color original del texto
            let textColor = rgb(0, 0, 0); // Negro por defecto
            
            // Si hay información de color en el textItem, usarla
            if (textItem.color) {
              try {
                if (Array.isArray(textItem.color) && textItem.color.length >= 3) {
                  textColor = rgb(textItem.color[0], textItem.color[1], textItem.color[2]);
                  console.log(`[PDF-CORRECTION] Color detectado: RGB(${textItem.color[0]}, ${textItem.color[1]}, ${textItem.color[2]})`);
                }
              } catch (colorError) {
                console.warn(`[PDF-CORRECTION] Error detectando color: ${colorError.message}`);
              }
            }
            
            // Verificar si el texto corregido cabe en el espacio disponible
            let finalCorrectedText = correctedText;
            if (isDenseText && correctedWidth > originalWidth * 1.2) {
              // Si el texto corregido es mucho más largo, intentar abreviarlo o usar el original
              console.warn(`[PDF-CORRECTION] Texto corregido muy largo para espacio denso: "${correctedText}"`);
              // Para texto denso, mantener longitud similar
              if (correctedText.length > originalText.length * 1.3) {
                console.log(`[PDF-CORRECTION] Manteniendo texto original por limitaciones de espacio`);
                finalCorrectedText = originalText; // Fallback al original
              }
            }
            
            console.log(`[PDF-CORRECTION] Reemplazando "${originalText}" → "${finalCorrectedText}" en posición (${textItem.transform[4]}, ${textItem.transform[5]}) con fontSize ${fontSize} ${isDenseText ? '(DENSO)' : ''}`);
            
            // Dibujar texto corregido en la misma posición con color original
            page.drawText(finalCorrectedText, {
              x: textItem.transform[4],
              y: textItem.transform[5],
              size: fontSize,
              font: font,
              color: textColor
            });
            
          } catch (textError) {
            console.warn(`[PDF-CORRECTION] Error aplicando corrección visual: ${textError.message}`);
          }
        }
      }
      
      correctedPdf.addPage(page);
    }
    
    // Registrar correcciones no encontradas
    for (const correction of corrections) {
      if (!appliedChanges.find(c => c.from === correction.from && c.applied)) {
        appliedChanges.push({ from: correction.from, to: correction.to, applied: false });
        console.log(`[PDF-CORRECTION] No encontrada: "${correction.from}"`);
      }
    }
    
    // 5. Generar PDF final
    const correctedPdfBytes = await correctedPdf.save();
    const pdfBuffer = Buffer.from(correctedPdfBytes);
    
    console.log(`[PDF-CORRECTION] Correcciones aplicadas con formato preservado: ${appliedCorrections}/${corrections.length}`);
    
    return {
      success: true,
      pdfBuffer,
      appliedCorrections,
      totalCorrections: corrections.length,
      appliedChanges,
      formatPreserved: true,
      metadata: {
        processedAt: new Date().toISOString(),
        pagesProcessed: copiedPages.length,
        method: 'format-preserving'
      }
    };
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error aplicando correcciones con formato:', error);
    
    // Fallback: usar método simple si falla el preservado
    console.log('[PDF-CORRECTION] Usando método fallback sin preservar formato...');
    return await applyCorrectionsSimple(originalPdfPath, corrections);
  }
}

/**
 * Extrae texto con posiciones exactas usando pdfjs-dist
 * @param {Buffer} pdfBytes - Bytes del PDF
 * @returns {Promise<Object>} - Texto con posiciones por página
 */
async function extractTextWithPositions(pdfBytes) {
  try {
    const uint8Array = new Uint8Array(pdfBytes);
    const loadingTask = pdfjsLib.getDocument({
      data: uint8Array,
      verbosity: 0
    });
    
    const pdfDocument = await loadingTask.promise;
    const pages = [];
    
    for (let pageNum = 1; pageNum <= pdfDocument.numPages; pageNum++) {
      const page = await pdfDocument.getPage(pageNum);
      const textContent = await page.getTextContent();
      
      const textItems = textContent.items.map(item => ({
        str: item.str,
        transform: item.transform,
        width: item.width,
        height: item.height,
        fontName: item.fontName,
        // Intentar capturar información de color si está disponible
        color: item.color || null,
        // Información adicional de estilo
        hasEOL: item.hasEOL || false
      }));
      
      pages.push({ pageIndex: pageNum - 1, textItems });
    }
    
    return { pages };
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error extrayendo texto con posiciones:', error);
    throw new Error(`Error extrayendo posiciones de texto: ${error.message}`);
  }
}

/**
 * Método fallback simple sin preservar formato
 * @param {string} originalPdfPath - Ruta del PDF original
 * @param {Array} corrections - Array de correcciones
 * @returns {Promise<Object>} - PDF corregido simple
 */
async function applyCorrectionsSimple(originalPdfPath, corrections) {
  try {
    console.log(`[PDF-CORRECTION] Aplicando correcciones con método simple...`);
    
    // 1. Extraer texto del PDF original
    const documentData = await processDocument(originalPdfPath, 'application/pdf');
    let correctedText = documentData.chunks.map(chunk => chunk.content).join('\n\n');
    
    // 2. Aplicar cada corrección usando replace
    let appliedCorrections = 0;
    const appliedChanges = [];
    
    for (const correction of corrections) {
      const { from, to } = correction;
      if (correctedText.includes(from)) {
        correctedText = correctedText.replace(new RegExp(escapeRegExp(from), 'g'), to);
        appliedCorrections++;
        appliedChanges.push({ from, to, applied: true });
        console.log(`[PDF-CORRECTION] Aplicada: "${from}" -> "${to}"`);
      } else {
        appliedChanges.push({ from, to, applied: false });
        console.log(`[PDF-CORRECTION] No encontrada: "${from}"`);
      }
    }
    
    // 3. Crear nuevo PDF con el texto corregido
    const correctedPdfBuffer = await createPDFFromText(correctedText, originalPdfPath);
    
    console.log(`[PDF-CORRECTION] Correcciones aplicadas (método simple): ${appliedCorrections}/${corrections.length}`);
    
    return {
      success: true,
      pdfBuffer: correctedPdfBuffer,
      appliedCorrections,
      totalCorrections: corrections.length,
      appliedChanges,
      formatPreserved: false,
      metadata: {
        processedAt: new Date().toISOString(),
        originalTextLength: documentData.fullText.length,
        correctedTextLength: correctedText.length,
        method: 'simple-text-replacement'
      }
    };
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error en método simple:', error);
    throw new Error(`Error aplicando correcciones simples: ${error.message}`);
  }
}

/**
 * Crea un PDF simple a partir de texto corregido
 * @param {string} text - Texto corregido
 * @param {string} originalPdfPath - Ruta del PDF original (para metadatos)
 * @returns {Promise<Buffer>} - Buffer del PDF generado
 */
async function createPDFFromText(text, originalPdfPath) {
  const pdf = await PDFDocument.create();
  const font = await pdf.embedFont(StandardFonts.Helvetica);
  
  const pageWidth = 612; // Carta
  const pageHeight = 792;
  const margin = 50;
  const fontSize = 12;
  const lineHeight = 16;
  const maxCharsPerLine = Math.floor((pageWidth - 2 * margin) / (fontSize * 0.6));
  const maxLinesPerPage = Math.floor((pageHeight - 2 * margin) / lineHeight);
  
  const lines = text.split('\n');
  const wrappedLines = [];
  
  // Dividir líneas largas
  for (const line of lines) {
    if (line.length <= maxCharsPerLine) {
      wrappedLines.push(line);
    } else {
      const words = line.split(' ');
      let currentLine = '';
      
      for (const word of words) {
        if ((currentLine + ' ' + word).length <= maxCharsPerLine) {
          currentLine += (currentLine ? ' ' : '') + word;
        } else {
          if (currentLine) wrappedLines.push(currentLine);
          currentLine = word;
        }
      }
      if (currentLine) wrappedLines.push(currentLine);
    }
  }
  
  // Crear páginas
  let currentPage = pdf.addPage([pageWidth, pageHeight]);
  let yPosition = pageHeight - margin;
  let lineCount = 0;
  
  for (const line of wrappedLines) {
    if (lineCount >= maxLinesPerPage) {
      currentPage = pdf.addPage([pageWidth, pageHeight]);
      yPosition = pageHeight - margin;
      lineCount = 0;
    }
    
    // Limpiar caracteres especiales
    const cleanLine = line.replace(/[^\x20-\x7E]/g, '?');
    
    currentPage.drawText(cleanLine, {
      x: margin,
      y: yPosition,
      size: fontSize,
      font: font,
      color: rgb(0, 0, 0)
    });
    
    yPosition -= lineHeight;
    lineCount++;
  }
  
  const pdfBytes = await pdf.save();
  return Buffer.from(pdfBytes);
}

/**
 * Parsea una lista de correcciones en formato texto
 * @param {string} correctionsList - Lista de correcciones en formato "• error -> correcto"
 * @returns {Array} - Array de objetos {from, to}
 */
export function parseCorrections(correctionsList) {
  const corrections = [];
  const lines = correctionsList.split('\n');
  
  for (const line of lines) {
    const trimmed = line.trim();
    if (trimmed.includes('->')) {
      // Remover bullet points y limpiar
      const cleaned = trimmed.replace(/^[•\-\*]\s*/, '');
      const [from, to] = cleaned.split('->').map(s => s.trim());
      
      if (from && to && from !== to) {
        corrections.push({ from, to });
      }
    }
  }
  
  return corrections;
}

/**
 * Escapa caracteres especiales para RegExp
 * @param {string} string - String a escapar
 * @returns {string} - String escapado
 */
function escapeRegExp(string) {
  return string.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

/**
 * Prueba la conectividad con SAP AI Core
 * @returns {Promise<Object>} - Resultado de la prueba
 */
export async function testAiCoreConnection() {
  try {
    console.log(`[PDF-CORRECTION] Probando conexión con SAP AI Core...`);
    
    const testPrompt = "Responde solo con 'OK' si recibes este mensaje.";
    
    const client = getAiCoreClient('gpt-4o');
    const response = await client.run({
      messages: [{ role: 'user', content: testPrompt }]
    });
    
    const result = response.getContent();
    
    return {
      success: true,
      connected: true,
      response: result,
      timestamp: new Date().toISOString()
    };
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error en conexión SAP AI Core:', error);
    return {
      success: false,
      connected: false,
      error: error.message,
      status: error.status || error.code,
      timestamp: new Date().toISOString()
    };
  }
}

/**
 * Genera correcciones automáticas para un PDF
 * @param {string} pdfPath - Ruta del PDF
 * @returns {Promise<Object>} - Lista de correcciones generadas
 */
export async function generateCorrections(pdfPath) {
  try {
    console.log(`[PDF-CORRECTION] Generando correcciones para: ${pdfPath}`);
    
    const documentData = await processDocument(pdfPath, 'application/pdf');
    const originalText = documentData.chunks.map(chunk => chunk.content).join('\n\n');
    
    // Limitar texto para SAP AI Core
    let textForAnalysis = originalText;
    if (originalText.length > 50000) {
      console.warn(`[PDF-CORRECTION] Texto muy largo: ${originalText.length} caracteres, truncando...`);
      textForAnalysis = originalText.substring(0, 50000) + '\n\n[TEXTO TRUNCADO...]';
    }
    
    const prompt = `Analiza el siguiente texto y genera una lista de correcciones ortográficas y gramaticales.

Para cada corrección, usa EXACTAMENTE este formato:
• palabra_incorrecta -> palabra_correcta

TEXTO A ANALIZAR:
${textForAnalysis}

IMPORTANTE: 
- Solo devuelve la lista de correcciones en el formato especificado
- Una corrección por línea
- No incluyas explicaciones adicionales
- Si no hay errores, devuelve "No se encontraron errores ortográficos"`;

    console.log(`[PDF-CORRECTION] Enviando prompt de ${prompt.length} caracteres a SAP AI Core...`);

    let correctionsList;
    let corrections = [];
    
    try {
      const client = getAiCoreClient('gpt-4o');
      const response = await client.run({
        messages: [{ role: 'user', content: prompt }]
      });
      
      correctionsList = response.getContent();
      
      if (!correctionsList || correctionsList.trim().length === 0) {
        throw new Error('SAP AI Core devolvió una respuesta vacía');
      }
      
      corrections = parseCorrections(correctionsList);
      console.log(`[PDF-CORRECTION] ${corrections.length} correcciones generadas exitosamente`);
      
    } catch (aiError) {
      console.error(`[PDF-CORRECTION] Error detallado en SAP AI Core:`, {
        message: aiError.message,
        status: aiError.status || aiError.code,
        response: aiError.response?.data || 'No response data',
        promptLength: prompt.length
      });
      
      // Fallback: respuesta básica
      correctionsList = "No se pudieron generar correcciones automáticas debido a un error en el servicio de IA.\nRevise el documento manualmente para errores ortográficos.";
      corrections = [];
    }
    
    return {
      success: true,
      correctionsList,
      corrections,
      totalCorrections: corrections.length,
      originalTextLength: originalText.length,
      textAnalyzedLength: textForAnalysis.length,
      wasTruncated: originalText.length > 50000
    };
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error generando correcciones:', error);
    throw new Error(`Error generando correcciones: ${error.message}`);
  }
}
