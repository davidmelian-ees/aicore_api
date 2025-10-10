import fs from 'fs/promises';
import path from 'path';
import mammoth from 'mammoth';
import * as pdfjsLib from 'pdfjs-dist';

/**
 * Procesa un archivo y extrae su contenido de texto
 * @param {string} filePath - Ruta al archivo
 * @param {string} mimeType - Tipo MIME del archivo
 * @returns {Promise<string>} - Contenido de texto extraído
 */
export async function extractTextFromFile(filePath, mimeType) {
  const extension = path.extname(filePath).toLowerCase();
  
  try {
    switch (extension) {
      case '.txt':
      case '.md':
      case '.json':
      case '.csv':
        return await extractTextFromPlainText(filePath);
      
      case '.pdf':
        return await extractTextFromPDF(filePath);
      
      case '.docx':
        return await extractTextFromDocx(filePath);
      
      default:
        throw new Error(`Tipo de archivo no soportado: ${extension}`);
    }
  } catch (error) {
    console.error(`Error procesando archivo ${filePath}:`, error);
    throw error;
  }
}

/**
 * Extrae texto de archivos de texto plano
 */
async function extractTextFromPlainText(filePath) {
  const content = await fs.readFile(filePath, 'utf-8');
  return content;
}

/**
 * Extrae texto de archivos PDF usando pdfjs-dist (Mozilla PDF.js)
 */
async function extractTextFromPDF(filePath) {
  try {
    console.log(`[PDF] Procesando archivo PDF: ${filePath}`);
    
    // Leer el archivo PDF
    const buffer = await fs.readFile(filePath);
    const uint8Array = new Uint8Array(buffer);
    
    // Cargar el documento PDF
    const loadingTask = pdfjsLib.getDocument({
      data: uint8Array,
      verbosity: 0 // Reducir logs de pdfjs
    });
    
    const pdfDocument = await loadingTask.promise;
    const numPages = pdfDocument.numPages;
    
    console.log(`[PDF] Documento cargado: ${numPages} páginas`);
    
    let fullText = '';
    
    // Extraer texto de cada página
    for (let pageNum = 1; pageNum <= numPages; pageNum++) {
      try {
        const page = await pdfDocument.getPage(pageNum);
        const textContent = await page.getTextContent();
        
        // Combinar todos los elementos de texto de la página
        const pageText = textContent.items
          .map(item => item.str)
          .join(' ');
        
        if (pageText.trim()) {
          fullText += pageText + '\n\n';
        }
        
        console.log(`[PDF] Página ${pageNum}/${numPages}: ${pageText.length} caracteres`);
      } catch (pageError) {
        console.warn(`[PDF] Error procesando página ${pageNum}:`, pageError.message);
        // Continuar con las siguientes páginas
      }
    }
    
    // Limpiar el texto
    fullText = fullText
      .replace(/\s+/g, ' ') // Normalizar espacios
      .replace(/\n\s*\n/g, '\n\n') // Normalizar saltos de línea
      .trim();
    
    if (!fullText || fullText.length === 0) {
      throw new Error('El PDF no contiene texto extraíble o está vacío');
    }
    
    console.log(`[PDF] Texto extraído exitosamente: ${fullText.length} caracteres`);
    return fullText;
    
  } catch (error) {
    console.error(`[PDF] Error extrayendo texto de ${filePath}:`, error);
    throw new Error(`Error procesando PDF: ${error.message}`);
  }
}

/**
 * Extrae texto de archivos DOCX
 */
async function extractTextFromDocx(filePath) {
  const buffer = await fs.readFile(filePath);
  const result = await mammoth.extractRawText({ buffer });
  return result.value;
}

/**
 * Divide el texto en chunks para embeddings
 * @param {string} text - Texto a dividir
 * @param {number} chunkSize - Tamaño de cada chunk
 * @param {number} overlap - Solapamiento entre chunks
 * @returns {Array<string>} - Array de chunks de texto
 */
export function splitTextIntoChunks(text, chunkSize = 1000, overlap = 200) {
  const chunks = [];
  let start = 0;
  
  while (start < text.length) {
    const end = Math.min(start + chunkSize, text.length);
    const chunk = text.slice(start, end);
    
    // Solo agregar chunks con contenido significativo
    if (chunk.trim().length > 0) {
      chunks.push(chunk.trim());
    }
    
    start += chunkSize - overlap;
  }
  
  return chunks;
}

/**
 * Procesa un archivo completo: extrae texto y lo divide en chunks
 * @param {string} filePath - Ruta al archivo
 * @param {string} mimeType - Tipo MIME del archivo
 * @param {Object} options - Opciones de procesamiento
 * @returns {Promise<Object>} - Objeto con texto completo y chunks
 */
export async function processDocument(filePath, mimeType, options = {}) {
  const { chunkSize = 1000, overlap = 200 } = options;
  
  try {
    console.log(`[DOC] Procesando documento: ${filePath} (${mimeType})`);
    
    // Extraer texto
    const text = await extractTextFromFile(filePath, mimeType);
    
    if (!text || text.trim().length === 0) {
      throw new Error('No se pudo extraer contenido del documento');
    }
    
    // Dividir en chunks
    const textChunks = splitTextIntoChunks(text, chunkSize, overlap);
    
    // Convertir chunks a formato esperado por RAG
    const chunks = textChunks.map((chunk, index) => ({
      content: chunk,
      index,
      metadata: {
        chunkIndex: index,
        totalChunks: textChunks.length
      }
    }));
    
    const fileStats = await fs.stat(filePath);
    
    console.log(`[DOC] Documento procesado: ${chunks.length} chunks, ${text.length} caracteres`);
    
    return {
      fullText: text,
      chunks,
      metadata: {
        fileName: path.basename(filePath),
        fileSize: fileStats.size,
        chunkCount: chunks.length,
        processedAt: new Date().toISOString(),
        contentLength: text.length
      }
    };
  } catch (error) {
    console.error(`[DOC] Error procesando documento ${filePath}:`, error);
    throw error;
  }
}
