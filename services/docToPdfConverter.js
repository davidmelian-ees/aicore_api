import mammoth from 'mammoth';
import WordExtractor from 'word-extractor';
import { PDFDocument, rgb, StandardFonts } from 'pdf-lib';
import fs from 'fs/promises';

/**
 * Limpia el texto de caracteres que no puede manejar pdf-lib con WinAnsi
 * @param {string} text - Texto a limpiar
 * @returns {string} - Texto limpio
 */
function cleanTextForPdf(text) {
  if (!text) return '';
  
  return text
    // Reemplazar tabulaciones por espacios
    .replace(/\t/g, '    ')
    // Reemplazar caracteres de control (excepto \n y \r)
    .replace(/[\x00-\x08\x0B-\x0C\x0E-\x1F\x7F]/g, '')
    // Reemplazar caracteres Unicode problemáticos con equivalentes ASCII
    .replace(/['']/g, "'")  // Comillas simples curvas
    .replace(/[""]/g, '"')  // Comillas dobles curvas
    .replace(/[–—]/g, '-')  // Guiones largos
    .replace(/…/g, '...')   // Puntos suspensivos
    .replace(/[•]/g, '*')   // Viñetas
    // Mantener solo caracteres imprimibles ASCII y saltos de línea
    .replace(/[^\x20-\x7E\n\r]/g, '');
}

/**
 * Convierte un archivo DOC/DOCX a PDF
 * @param {string} docxPath - Ruta al archivo DOCX
 * @returns {Promise<Buffer>} - Buffer del PDF generado
 */
export async function convertDocxToPdf(docxPath) {
  try {
    console.log(`[DOC-TO-PDF] Convirtiendo DOC/DOCX a PDF: ${docxPath}`);
    
    // 1. Leer archivo y detectar tipo
    const buffer = await fs.readFile(docxPath);
    const fileType = detectFileTypeFromBuffer(buffer);
    let text;
    
    if (fileType === 'doc') {
      // 1a. Extraer texto del .doc usando word-extractor
      console.log(`[DOC-TO-PDF] Detectado archivo .doc (formato antiguo)`);
      const extractor = new WordExtractor();
      const extracted = await extractor.extract(buffer);
      text = extracted.getBody();
    } else if (fileType === 'docx') {
      // 1b. Extraer texto del .docx usando mammoth
      console.log(`[DOC-TO-PDF] Detectado archivo .docx (formato moderno)`);
      const result = await mammoth.extractRawText({ buffer });
      text = result.value;
    } else {
      throw new Error(`Tipo de archivo no soportado para conversión: ${fileType}`);
    }
    
    if (!text || text.trim().length === 0) {
      throw new Error('El documento está vacío o no se pudo extraer texto');
    }
    
    console.log(`[DOC-TO-PDF] Texto extraído: ${text.length} caracteres`);
    
    // 2. Limpiar texto de caracteres problemáticos
    text = cleanTextForPdf(text);
    console.log(`[DOC-TO-PDF] Texto limpiado: ${text.length} caracteres`);
    
    // 3. Crear un nuevo PDF con el texto extraído
    const pdfDoc = await PDFDocument.create();
    const font = await pdfDoc.embedFont(StandardFonts.Helvetica);
    const boldFont = await pdfDoc.embedFont(StandardFonts.HelveticaBold);
    
    // Configuración de página
    const pageWidth = 595.28; // A4 width in points
    const pageHeight = 841.89; // A4 height in points
    const margin = 50;
    const maxWidth = pageWidth - (2 * margin);
    const fontSize = 11;
    const lineHeight = fontSize * 1.5;
    
    // Dividir texto en líneas y páginas
    const lines = wrapText(text, font, fontSize, maxWidth);
    
    let currentPage = pdfDoc.addPage([pageWidth, pageHeight]);
    let yPosition = pageHeight - margin;
    
    for (const line of lines) {
      // Si no hay espacio en la página actual, crear una nueva
      if (yPosition < margin + lineHeight) {
        currentPage = pdfDoc.addPage([pageWidth, pageHeight]);
        yPosition = pageHeight - margin;
      }
      
      // Dibujar la línea
      currentPage.drawText(line, {
        x: margin,
        y: yPosition,
        size: fontSize,
        font: font,
        color: rgb(0, 0, 0),
      });
      
      yPosition -= lineHeight;
    }
    
    // 4. Guardar PDF
    const pdfBytes = await pdfDoc.save();
    const pdfBuffer = Buffer.from(pdfBytes);
    
    console.log(`[DOC-TO-PDF] PDF generado: ${pdfBuffer.length} bytes, ${pdfDoc.getPageCount()} páginas`);
    
    return pdfBuffer;
    
  } catch (error) {
    console.error('[DOC-TO-PDF] Error convirtiendo DOCX a PDF:', error);
    throw new Error(`Error convirtiendo documento a PDF: ${error.message}`);
  }
}

/**
 * Divide el texto en líneas que caben en el ancho especificado
 * @param {string} text - Texto a dividir
 * @param {Object} font - Fuente PDF
 * @param {number} fontSize - Tamaño de fuente
 * @param {number} maxWidth - Ancho máximo en puntos
 * @returns {string[]} - Array de líneas
 */
function wrapText(text, font, fontSize, maxWidth) {
  const lines = [];
  const paragraphs = text.split('\n');
  
  for (const paragraph of paragraphs) {
    if (paragraph.trim().length === 0) {
      lines.push(''); // Línea vacía para párrafos
      continue;
    }
    
    const words = paragraph.split(' ');
    let currentLine = '';
    
    for (const word of words) {
      const testLine = currentLine ? `${currentLine} ${word}` : word;
      const width = font.widthOfTextAtSize(testLine, fontSize);
      
      if (width > maxWidth && currentLine.length > 0) {
        lines.push(currentLine);
        currentLine = word;
      } else {
        currentLine = testLine;
      }
    }
    
    if (currentLine.length > 0) {
      lines.push(currentLine);
    }
  }
  
  return lines;
}

/**
 * Detecta si un archivo es DOC/DOCX basándose en su mimetype o extensión
 * @param {string} mimetype - MIME type del archivo
 * @param {string} filename - Nombre del archivo
 * @returns {boolean} - true si es DOC/DOCX
 */
export function isDocxFile(mimetype, filename) {
  const docxMimetypes = [
    'application/vnd.openxmlformats-officedocument.wordprocessingml.document', // .docx
    'application/msword', // .doc
    'application/vnd.ms-word',
    'application/octet-stream' // Fallback para algunos navegadores
  ];
  
  const hasDocxMimetype = docxMimetypes.includes(mimetype);
  const hasDocxExtension = filename && (
    filename.toLowerCase().endsWith('.docx') || 
    filename.toLowerCase().endsWith('.doc')
  );
  
  return hasDocxMimetype || hasDocxExtension;
}

/**
 * Detecta el tipo de archivo desde un buffer analizando los magic bytes
 * @param {Buffer} buffer - Buffer del archivo
 * @returns {string} - 'pdf', 'docx', 'doc' o 'unknown'
 */
export function detectFileTypeFromBuffer(buffer) {
  if (!buffer || buffer.length < 8) {
    return 'unknown';
  }
  
  // PDF: Comienza con %PDF
  if (buffer[0] === 0x25 && buffer[1] === 0x50 && buffer[2] === 0x44 && buffer[3] === 0x46) {
    return 'pdf';
  }
  
  // DOCX: Es un archivo ZIP (PK..) que contiene word/
  if (buffer[0] === 0x50 && buffer[1] === 0x4B && buffer[2] === 0x03 && buffer[3] === 0x04) {
    // Es un ZIP, probablemente DOCX
    const bufferStr = buffer.toString('utf8', 0, Math.min(buffer.length, 1000));
    if (bufferStr.includes('word/') || bufferStr.includes('docProps/')) {
      return 'docx';
    }
  }
  
  // DOC: Comienza con D0CF11E0A1B11AE1 (OLE/COM Document)
  if (buffer[0] === 0xD0 && buffer[1] === 0xCF && buffer[2] === 0x11 && buffer[3] === 0xE0 &&
      buffer[4] === 0xA1 && buffer[5] === 0xB1 && buffer[6] === 0x1A && buffer[7] === 0xE1) {
    return 'doc';
  }
  
  return 'unknown';
}

/**
 * Convierte un buffer DOC/DOCX a PDF
 * @param {Buffer} docBuffer - Buffer del archivo DOC/DOCX
 * @returns {Promise<Buffer>} - Buffer del PDF generado
 */
export async function convertDocxBufferToPdf(docBuffer) {
  try {
    console.log(`[DOC-TO-PDF] Convirtiendo buffer DOC/DOCX a PDF (${docBuffer.length} bytes)`);
    
    // Detectar si es .doc o .docx
    const fileType = detectFileTypeFromBuffer(docBuffer);
    let text;
    
    if (fileType === 'doc') {
      // 1a. Extraer texto del .doc usando word-extractor
      console.log(`[DOC-TO-PDF] Detectado archivo .doc (formato antiguo)`);
      const extractor = new WordExtractor();
      const extracted = await extractor.extract(docBuffer);
      text = extracted.getBody();
    } else if (fileType === 'docx') {
      // 1b. Extraer texto del .docx usando mammoth
      console.log(`[DOC-TO-PDF] Detectado archivo .docx (formato moderno)`);
      const result = await mammoth.extractRawText({ buffer: docBuffer });
      text = result.value;
    } else {
      throw new Error(`Tipo de archivo no soportado para conversión: ${fileType}`);
    }
    
    if (!text || text.trim().length === 0) {
      throw new Error('El documento está vacío o no se pudo extraer texto');
    }
    
    console.log(`[DOC-TO-PDF] Texto extraído: ${text.length} caracteres`);
    
    // 2. Limpiar texto de caracteres problemáticos
    text = cleanTextForPdf(text);
    console.log(`[DOC-TO-PDF] Texto limpiado: ${text.length} caracteres`);
    
    // 3. Crear un nuevo PDF con el texto extraído
    const pdfDoc = await PDFDocument.create();
    const font = await pdfDoc.embedFont(StandardFonts.Helvetica);
    
    // Configuración de página
    const pageWidth = 595.28; // A4 width in points
    const pageHeight = 841.89; // A4 height in points
    const margin = 50;
    const maxWidth = pageWidth - (2 * margin);
    const fontSize = 11;
    const lineHeight = fontSize * 1.5;
    
    // Dividir texto en líneas y páginas
    const lines = wrapText(text, font, fontSize, maxWidth);
    
    let currentPage = pdfDoc.addPage([pageWidth, pageHeight]);
    let yPosition = pageHeight - margin;
    
    for (const line of lines) {
      // Si no hay espacio en la página actual, crear una nueva
      if (yPosition < margin + lineHeight) {
        currentPage = pdfDoc.addPage([pageWidth, pageHeight]);
        yPosition = pageHeight - margin;
      }
      
      // Dibujar la línea
      currentPage.drawText(line, {
        x: margin,
        y: yPosition,
        size: fontSize,
        font: font,
        color: rgb(0, 0, 0),
      });
      
      yPosition -= lineHeight;
    }
    
    // 4. Guardar PDF
    const pdfBytes = await pdfDoc.save();
    const pdfBuffer = Buffer.from(pdfBytes);
    
    console.log(`[DOC-TO-PDF] PDF generado: ${pdfBuffer.length} bytes, ${pdfDoc.getPageCount()} páginas`);
    
    return pdfBuffer;
    
  } catch (error) {
    console.error('[DOC-TO-PDF] Error convirtiendo buffer DOCX a PDF:', error);
    throw new Error(`Error convirtiendo documento a PDF: ${error.message}`);
  }
}

export default {
  convertDocxToPdf,
  convertDocxBufferToPdf,
  isDocxFile,
  detectFileTypeFromBuffer
};
