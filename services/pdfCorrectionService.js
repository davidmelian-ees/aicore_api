import { PDFDocument, rgb, StandardFonts } from 'pdf-lib';
import * as pdfjsLib from 'pdfjs-dist';
import fs from 'fs/promises';
import { processDocument } from './documentProcessor.js';
import { getAiCoreClient } from '../auth/aiCoreClient.js';

/**
 * Servicio para corrección de PDFs con dos enfoques:
 * 1. Generar PDF con lista de correcciones
 * 2. Aplicar correcciones directamente por replace
 */

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
    
    // 2. Generar correcciones usando SAP AI Core
    const correctionPrompt = customPrompt || `Analiza el siguiente texto y genera una lista de correcciones ortográficas y gramaticales.

Para cada corrección, usa EXACTAMENTE este formato:
• palabra_incorrecta -> palabra_correcta

TEXTO A ANALIZAR:
${originalText}

IMPORTANTE: 
- Solo devuelve la lista de correcciones en el formato especificado
- Una corrección por línea
- No incluyas explicaciones adicionales
- Si no hay errores, devuelve "No se encontraron errores ortográficos"`;

    console.log(`[PDF-CORRECTION] Generando correcciones con SAP AI Core...`);
    
    const client = getAiCoreClient('gpt-4o');
    const response = await client.run({
      messages: [{ role: 'user', content: correctionPrompt }]
    });
    
    const correctionsList = response.getContent();
    
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
      for (const textItem of pageTextItems) {
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
        
        // Si hay cambios, reemplazar el texto manteniendo posición y formato
        if (hasChanges && correctedText !== originalText) {
          try {
            // Obtener fuente y tamaño originales (aproximados)
            const fontSize = Math.abs(textItem.transform[0]) || 12;
            const font = await correctedPdf.embedFont(StandardFonts.Helvetica);
            
            // Limpiar área original (dibujar rectángulo blanco)
            const textWidth = font.widthOfTextAtSize(originalText, fontSize);
            const textHeight = fontSize * 1.2;
            
            page.drawRectangle({
              x: textItem.transform[4] - 1,
              y: textItem.transform[5] - 2,
              width: textWidth + 2,
              height: textHeight,
              color: rgb(1, 1, 1) // Blanco
            });
            
            // Dibujar texto corregido en la misma posición
            page.drawText(correctedText, {
              x: textItem.transform[4],
              y: textItem.transform[5],
              size: fontSize,
              font: font,
              color: rgb(0, 0, 0) // Negro por defecto
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
        fontName: item.fontName
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
 * Genera correcciones automáticas para un PDF
 * @param {string} pdfPath - Ruta del PDF
 * @returns {Promise<Object>} - Lista de correcciones generadas
 */
export async function generateCorrections(pdfPath) {
  try {
    console.log(`[PDF-CORRECTION] Generando correcciones para: ${pdfPath}`);
    
    const documentData = await processDocument(pdfPath, 'application/pdf');
    const originalText = documentData.chunks.map(chunk => chunk.content).join('\n\n');
    
    const prompt = `Analiza el siguiente texto y genera una lista de correcciones ortográficas y gramaticales.

Para cada corrección, usa EXACTAMENTE este formato:
• palabra_incorrecta -> palabra_correcta

TEXTO A ANALIZAR:
${originalText.substring(0, 50000)} ${originalText.length > 50000 ? '\n\n[TEXTO TRUNCADO...]' : ''}

IMPORTANTE: 
- Solo devuelve la lista de correcciones en el formato especificado
- Una corrección por línea
- No incluyas explicaciones adicionales
- Si no hay errores, devuelve "No se encontraron errores ortográficos"`;

    const client = getAiCoreClient('gpt-4o');
    const response = await client.run({
      messages: [{ role: 'user', content: prompt }]
    });
    
    const correctionsList = response.getContent();
    const corrections = parseCorrections(correctionsList);
    
    return {
      success: true,
      correctionsList,
      corrections,
      totalCorrections: corrections.length,
      originalTextLength: originalText.length
    };
    
  } catch (error) {
    console.error('[PDF-CORRECTION] Error generando correcciones:', error);
    throw new Error(`Error generando correcciones: ${error.message}`);
  }
}
