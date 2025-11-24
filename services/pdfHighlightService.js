import { PDFDocument, rgb } from 'pdf-lib';
import * as pdfjsLib from 'pdfjs-dist';
import fs from 'fs/promises';

/**
 * Servicio para subrayar errores en PDFs
 * Dibuja rectángulos amarillos sobre el texto con errores
 */

/**
 * Extrae posiciones exactas del texto en el PDF
 * @param {string} pdfPath - Ruta al PDF
 * @returns {Promise<Array>} - Array de textos con sus posiciones
 */
async function extractTextPositions(pdfPath) {
  try {
    const pdfBuffer = await fs.readFile(pdfPath);
    const loadingTask = pdfjsLib.getDocument({ data: new Uint8Array(pdfBuffer) });
    const pdfDoc = await loadingTask.promise;
    
    const textPositions = [];
    
    for (let pageNum = 1; pageNum <= pdfDoc.numPages; pageNum++) {
      const page = await pdfDoc.getPage(pageNum);
      const textContent = await page.getTextContent();
      const viewport = page.getViewport({ scale: 1.0 });
      
      textContent.items.forEach(item => {
        if (item.str && item.str.trim()) {
          // Obtener posición del texto con mayor precisión
          const transform = item.transform;
          const x = transform[4];
          const y = transform[5];
          
          // Calcular dimensiones reales del texto
          const fontSize = Math.sqrt(transform[0] * transform[0] + transform[1] * transform[1]);
          const width = item.width;
          const height = item.height || fontSize;
          
          textPositions.push({
            text: item.str,
            page: pageNum,
            x: x,
            y: y, // Mantener coordenadas originales de pdfjs
            width: width,
            height: height,
            fontSize: fontSize
          });
        }
      });
    }
    
    console.log(`[PDF-HIGHLIGHT] Extraídas ${textPositions.length} posiciones de texto`);
    return textPositions;
    
  } catch (error) {
    console.error('[PDF-HIGHLIGHT] Error extrayendo posiciones:', error);
    throw error;
  }
}

/**
 * Busca la posición de un texto específico en el PDF
 * @param {Array} textPositions - Posiciones extraídas
 * @param {string} searchText - Texto a buscar
 * @returns {Array} - Posiciones encontradas
 */
function findTextPosition(textPositions, searchText) {
  const found = [];
  const searchLower = searchText.toLowerCase().trim();
  
  // Buscar coincidencias exactas
  textPositions.forEach(pos => {
    if (pos.text.toLowerCase().includes(searchLower)) {
      found.push(pos);
    }
  });
  
  // Si no hay coincidencias exactas, buscar por palabras clave
  if (found.length === 0) {
    const keywords = searchLower.split(' ').filter(w => w.length > 3);
    textPositions.forEach(pos => {
      const posLower = pos.text.toLowerCase();
      if (keywords.some(keyword => posLower.includes(keyword))) {
        found.push(pos);
      }
    });
  }
  
  return found;
}

/**
 * Subraya errores en el PDF con rectángulos amarillos
 * @param {string} pdfPath - Ruta al PDF original
 * @param {Array} errors - Lista de errores a subrayar
 * @returns {Promise<Buffer>} - PDF con subrayados
 */
export async function highlightErrorsInPDF(pdfPath, errors) {
  try {
    console.log(`[PDF-HIGHLIGHT] Subrayando ${errors.length} errores en PDF...`);
    
    // 1. Extraer posiciones del texto
    const textPositions = await extractTextPositions(pdfPath);
    
    // 2. Cargar PDF con pdf-lib (convertir Buffer a Uint8Array)
    const pdfBuffer = await fs.readFile(pdfPath);
    const pdfDoc = await PDFDocument.load(new Uint8Array(pdfBuffer));
    const pages = pdfDoc.getPages();
    
    let highlightCount = 0;
    
    // 3. Para cada error, encontrar su posición y subrayar
    for (const error of errors) {
      const searchTexts = error.searchTexts || [error.text];
      let foundAny = false;
      
      // Buscar cada texto del error
      for (const searchText of searchTexts) {
        if (!searchText || searchText.length < 3) continue;
        
        // Buscar posiciones del texto del error
        const positions = findTextPosition(textPositions, searchText);
        
        if (positions.length > 0) {
          console.log(`[PDF-HIGHLIGHT] ✓ Encontradas ${positions.length} posiciones para: "${searchText}"`);
          foundAny = true;
          
          // Subrayar cada ocurrencia
          for (const pos of positions) {
            const page = pages[pos.page - 1];
            const pageHeight = page.getHeight();
            
            // Convertir coordenadas de pdfjs (origen arriba-izquierda) a pdf-lib (origen abajo-izquierda)
            const highlightX = pos.x;
            const highlightY = pageHeight - pos.y - pos.height;
            const highlightWidth = pos.width;
            const highlightHeight = pos.height;
            
            // Dibujar rectángulo rojo translúcido (más visible pero sin tapar el texto)
            page.drawRectangle({
              x: highlightX,
              y: highlightY,
              width: highlightWidth,
              height: highlightHeight,
              color: rgb(1, 0, 0), // Rojo brillante
              opacity: 0.25, // Translúcido para ver el texto detrás
              borderColor: rgb(0.8, 0, 0), // Borde rojo oscuro
              borderWidth: 1.5, // Borde más grueso
              borderOpacity: 0.8 // Borde más opaco para mejor visibilidad
            });
            
            highlightCount++;
          }
        }
      }
      
      if (!foundAny) {
        console.warn(`[PDF-HIGHLIGHT] ✗ No se encontró ninguna posición para error: "${error.description.substring(0, 60)}..."`);
        console.warn(`[PDF-HIGHLIGHT]   Textos buscados: ${searchTexts.join(', ')}`);
      }
    }
    
    console.log(`[PDF-HIGHLIGHT] ${highlightCount} subrayados aplicados`);
    
    // 4. Guardar PDF modificado
    const modifiedPdfBuffer = await pdfDoc.save();
    return Buffer.from(modifiedPdfBuffer);
    
  } catch (error) {
    console.error('[PDF-HIGHLIGHT] Error subrayando errores:', error);
    throw new Error(`Error subrayando errores en PDF: ${error.message}`);
  }
}

/**
 * Parsea la lista de errores desde el texto de la IA
 * @param {string} errorText - Texto con errores de la IA
 * @returns {Array} - Lista de errores parseados
 */
export function parseErrorsFromAIResponse(errorText) {
  const errors = [];
  const lines = errorText.split('\n');
  
  let currentError = null;
  
  for (const line of lines) {
    const trimmed = line.trim();
    
    // Detectar inicio de error (línea que empieza con "- ")
    if (trimmed.startsWith('- ') && !trimmed.includes('Ubicación:') && !trimmed.includes('Contexto:')) {
      if (currentError) {
        errors.push(currentError);
      }
      
      currentError = {
        description: trimmed.substring(2).trim(),
        location: '',
        context: '',
        text: '' // Texto a buscar en el PDF
      };
    }
    // Detectar ubicación
    else if (trimmed.includes('- Ubicación:') || trimmed.includes('- Ubicacion:')) {
      if (currentError) {
        currentError.location = trimmed.split(':')[1]?.trim() || '';
      }
    }
    // Detectar contexto
    else if (trimmed.includes('- Contexto:')) {
      if (currentError) {
        currentError.context = trimmed.split(':')[1]?.trim() || '';
      }
    }
  }
  
  // Añadir último error
  if (currentError) {
    errors.push(currentError);
  }
  
  // Extraer texto a buscar de cada error
  errors.forEach(error => {
    const desc = error.description;
    error.searchTexts = []; // Array de textos a buscar
    
    // 1. Detectar comentarios de desarrollador (Oriol:, David:, etc.)
    const devCommentMatch = desc.match(/[""]([^""]+)[""]|"([^"]+)"/);
    if (devCommentMatch) {
      const comment = devCommentMatch[1] || devCommentMatch[2];
      error.searchTexts.push(comment.trim());
      console.log(`[PDF-HIGHLIGHT] Comentario detectado: "${comment.trim()}"`);
    }
    
    // 2. Detectar tags SAP sin reemplazar ({B}TEXTO{/B}, ZVRM_, etc.)
    const sapTagMatch = desc.match(/\{[A-Z]\}[^{]+\{\/[A-Z]\}/g);
    if (sapTagMatch) {
      sapTagMatch.forEach(tag => {
        error.searchTexts.push(tag);
        console.log(`[PDF-HIGHLIGHT] Tag SAP detectado: "${tag}"`);
      });
    }
    
    // 3. Detectar variables SAP (ZVRM_, ZRM_, etc.)
    const sapVarMatch = desc.match(/Z[A-Z_]+[A-Z0-9_]*/g);
    if (sapVarMatch) {
      sapVarMatch.forEach(varName => {
        if (varName.length > 5) { // Solo variables largas
          error.searchTexts.push(varName);
          console.log(`[PDF-HIGHLIGHT] Variable SAP detectada: "${varName}"`);
        }
      });
    }
    
    // 4. Detectar importes específicos (29.040.000,00, etc.)
    const amountMatch = desc.match(/\d{1,3}(?:\.\d{3})*,\d{2}/g);
    if (amountMatch) {
      amountMatch.forEach(amount => {
        error.searchTexts.push(amount);
        console.log(`[PDF-HIGHLIGHT] Importe detectado: "${amount}"`);
      });
    }
    
    // 5. Detectar texto entre comillas
    const quotedMatch = desc.match(/"([^"]+)"/g);
    if (quotedMatch) {
      quotedMatch.forEach(quoted => {
        const text = quoted.replace(/"/g, '');
        if (text.length > 3) {
          error.searchTexts.push(text);
          console.log(`[PDF-HIGHLIGHT] Texto citado detectado: "${text}"`);
        }
      });
    }
    
    // 6. Si no se encontró nada específico, usar palabras clave del contexto
    if (error.searchTexts.length === 0 && error.context) {
      // Extraer palabras clave del contexto (nombres de tablas, secciones, etc.)
      const contextWords = error.context.split(/[\s,.-]+/).filter(w => w.length > 4);
      error.searchTexts.push(...contextWords.slice(0, 3));
      console.log(`[PDF-HIGHLIGHT] Usando contexto: ${contextWords.slice(0, 3).join(', ')}`);
    }
    
    // 7. Fallback: usar primeras palabras significativas de la descripción
    if (error.searchTexts.length === 0) {
      const words = desc.split(' ').filter(w => w.length > 4 && !['detectado', 'encontrado', 'error'].includes(w.toLowerCase()));
      error.searchTexts.push(words.slice(0, 3).join(' '));
    }
    
    // Mantener compatibilidad con código anterior
    error.text = error.searchTexts[0] || '';
  });
  
  console.log(`[PDF-HIGHLIGHT] Parseados ${errors.length} errores de la respuesta de la IA`);
  return errors;
}
