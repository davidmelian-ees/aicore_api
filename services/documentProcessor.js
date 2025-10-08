import fs from 'fs/promises';
import path from 'path';
import mammoth from 'mammoth';

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
        throw new Error('Procesamiento de PDF temporalmente deshabilitado. Use TXT, DOCX, MD, JSON o CSV.');
      
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
 * Extrae texto de archivos PDF (deshabilitado temporalmente)
 */
async function extractTextFromPDF(filePath) {
  throw new Error('Procesamiento de PDF temporalmente deshabilitado');
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
  
  // Extraer texto
  const text = await extractTextFromFile(filePath, mimeType);
  
  // Dividir en chunks
  const chunks = splitTextIntoChunks(text, chunkSize, overlap);
  
  return {
    fullText: text,
    chunks,
    metadata: {
      fileName: path.basename(filePath),
      fileSize: (await fs.stat(filePath)).size,
      chunkCount: chunks.length,
      processedAt: new Date().toISOString()
    }
  };
}
