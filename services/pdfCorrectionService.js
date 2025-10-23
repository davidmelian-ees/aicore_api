import { PDFDocument, rgb, StandardFonts } from 'pdf-lib';
import * as pdfjsLib from 'pdfjs-dist';
import fs from 'fs/promises';
import { processDocument } from './documentProcessor.js';
import { getAiCoreClient } from '../auth/aiCoreClient.js';
import path from 'path';

/**
 * Servicio para corrección de PDFs con dos enfoques:
 * 1. Generar PDF con lista de correcciones
 * 2. Aplicar correcciones directamente por replace
 */

// Función para cargar prompts de validación
async function loadValidationPrompts() {
  try {
    const promptsDir = path.join(process.cwd(), 'prompts_dev');
    
    const [
      nomenclatura,
      erroresComunes,
      validationSystem,
      analisisPliegos
    ] = await Promise.all([
      fs.readFile(path.join(promptsDir, 'NOMENCLATURA_PLIEGOS.txt'), 'utf8'),
      fs.readFile(path.join(promptsDir, 'ERRORES_COMUNES_PLIEGOS.txt'), 'utf8'),
      fs.readFile(path.join(promptsDir, 'PLIEGOS_VALIDATION_SYSTEM.txt'), 'utf8'),
      fs.readFile(path.join(promptsDir, 'ANALISIS_PLIEGOS_GENERADOS.txt'), 'utf8')
    ]);
    
    return {
      nomenclatura,
      erroresComunes,
      validationSystem,
      analisisPliegos
    };
  } catch (error) {
    console.error('[PDF-CORRECTION] Error cargando prompts:', error);
    return null;
  }
}

// Función para construir el prompt de validación específico
async function buildValidationPrompt(textForAnalysis, prompts) {
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
${prompts.validationSystem.substring(0, 2000)}

ERRORES COMUNES A DETECTAR:
${prompts.erroresComunes.substring(0, 3000)}

NOMENCLATURA ESPERADA:
${prompts.nomenclatura.substring(0, 1500)}

================================================================================
INSTRUCCIONES DE VALIDACIÓN:
================================================================================

1. ANALIZA el siguiente texto de pliego
2. IDENTIFICA errores según los patrones definidos arriba
3. GENERA un informe detallado con:
   - Errores críticos (bloquean generación)
   - Advertencias (permiten continuar)
   - Sugerencias de corrección específicas
   - Campos variables detectados

4. FORMATO DE RESPUESTA:
   🔴 ERRORES CRÍTICOS:
   - [Lista de errores que impiden continuar]
   
   🟡 ADVERTENCIAS:
   - [Lista de problemas menores]
   
   ✅ SUGERENCIAS:
   - [Correcciones específicas recomendadas]
   
   📋 CAMPOS VARIABLES DETECTADOS:
   - [Lista de variables SAP encontradas]

================================================================================
TEXTO DEL PLIEGO A VALIDAR:
================================================================================

${textForAnalysis}

================================================================================
GENERA EL INFORME DE VALIDACIÓN:
================================================================================`;
}

/**
 * Genera un PDF con el contenido original + lista de correcciones al final
 * @param {string} originalPdfPath - Ruta del PDF original
 * @param {string} customPrompt - Prompt para generar correcciones
 * @returns {Promise<Object>} - PDF con correcciones listadas
 */
export async function generatePDFWithCorrectionsList(originalPdfPath, customPrompt = null) {
  try {
    console.log(`[PDF-CORRECTION] Generando PDF con lista de correcciones...`);
    
    // 1. Extraer texto del PDF original
    const documentData = await processDocument(originalPdfPath, 'application/pdf');
    const originalText = documentData.chunks.map(chunk => chunk.content).join('\n\n');
    
    // 2. Limitar texto para SAP AI Core (máximo 50,000 caracteres)
    let textForAnalysis = originalText;
    if (originalText.length > 50000) {
      console.warn(`[PDF-CORRECTION] Texto muy largo: ${originalText.length} caracteres, truncando...`);
      textForAnalysis = originalText.substring(0, 50000) + '\n\n[TEXTO TRUNCADO...]';
    }

    // 3. Cargar prompts de validación
    const prompts = await loadValidationPrompts();
    
    // 4. Generar prompt de validación específico para pliegos
    const correctionPrompt = customPrompt || await buildValidationPrompt(textForAnalysis, prompts);

    console.log(`[PDF-CORRECTION] Generando correcciones con SAP AI Core (${correctionPrompt.length} caracteres)...`);
    
    let correctionsList;
    try {
      const client = getAiCoreClient('gpt-4o');
      const response = await client.run({
        messages: [{ role: 'user', content: correctionPrompt }]
      });
      
      correctionsList = response.getContent();
      
      if (!correctionsList || correctionsList.trim().length === 0) {
        throw new Error('SAP AI Core devolvió una respuesta vacía');
      }
      
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
    
    // 3. Cargar PDF original
    const originalPdfBytes = await fs.readFile(originalPdfPath);
    const originalPdf = await PDFDocument.load(originalPdfBytes);
    
    // 4. Crear nuevo PDF copiando el original
    const newPdf = await PDFDocument.create();
    
    // Copiar todas las páginas originales
    const pageIndices = Array.from({ length: originalPdf.getPageCount() }, (_, i) => i);
    const copiedPages = await newPdf.copyPages(originalPdf, pageIndices);
    copiedPages.forEach(page => newPdf.addPage(page));
    
    // 5. Añadir página(s) de correcciones
    await addCorrectionsPages(newPdf, correctionsList);
    
    // 6. Generar PDF final
    const finalPdfBytes = await newPdf.save();
    const pdfBuffer = Buffer.from(finalPdfBytes);
    
    console.log(`[PDF-CORRECTION] PDF con correcciones generado: ${pdfBuffer.length} bytes`);
    
    return {
      success: true,
      pdfBuffer,
      correctionsList,
      originalPageCount: originalPdf.getPageCount(),
      totalPageCount: newPdf.getPageCount(),
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
 * Añade páginas de correcciones al PDF
 * @param {PDFDocument} pdf - Documento PDF
 * @param {string} correctionsList - Lista de correcciones
 */
async function addCorrectionsPages(pdf, correctionsList) {
  const font = await pdf.embedFont(StandardFonts.Helvetica);
  const boldFont = await pdf.embedFont(StandardFonts.HelveticaBold);
  
  const corrections = correctionsList.split('\n').filter(line => line.trim());
  const pageHeight = 792; // Tamaño carta
  const pageWidth = 612;
  const margin = 50;
  const lineHeight = 20;
  const maxLinesPerPage = Math.floor((pageHeight - 2 * margin - 100) / lineHeight);
  
  let currentPage = pdf.addPage([pageWidth, pageHeight]);
  let yPosition = pageHeight - margin;
  let lineCount = 0;
  
  // Título
  currentPage.drawText('CORRECCIONES ORTOGRÁFICAS', {
    x: margin,
    y: yPosition,
    size: 18,
    font: boldFont,
    color: rgb(0, 0, 0)
  });
  
  yPosition -= 40;
  lineCount += 2;
  
  // Añadir cada corrección
  for (const correction of corrections) {
    if (lineCount >= maxLinesPerPage) {
      // Nueva página
      currentPage = pdf.addPage([pageWidth, pageHeight]);
      yPosition = pageHeight - margin;
      lineCount = 0;
      
      // Título en nueva página
      currentPage.drawText('CORRECCIONES ORTOGRÁFICAS (continuación)', {
        x: margin,
        y: yPosition,
        size: 16,
        font: boldFont,
        color: rgb(0, 0, 0)
      });
      
      yPosition -= 40;
      lineCount += 2;
    }
    
    // Limpiar caracteres especiales para WinAnsi
    const cleanCorrection = correction.replace(/[^\x20-\x7E]/g, '?');
    
    currentPage.drawText(cleanCorrection, {
      x: margin,
      y: yPosition,
      size: 12,
      font: font,
      color: rgb(0, 0, 0)
    });
    
    yPosition -= lineHeight;
    lineCount++;
  }
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
