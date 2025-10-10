import { vectorStore } from './vectorStore.js';
import { chromaVectorStore } from './chromaVectorStore.js';
import { chromaPythonClient } from './chromaPythonClient.js';
import { generateEmbedding, generateEmbeddings } from './embeddingService.js';
import { processDocument } from './documentProcessor.js';
import { getAiCoreClient } from '../auth/aiCoreClient.js';
import { PDFDocument, rgb, StandardFonts } from 'pdf-lib';
import fs from 'fs/promises';
import { v4 as uuidv4 } from 'uuid';

// Almacenamiento en memoria para contextos
const contexts = new Map();
// Inicializar contexto por defecto
if (!contexts.has('default')) {
  contexts.set('default', {
    id: 'default',
    name: 'Contexto Principal',
    description: 'Contexto por defecto del sistema',
    createdAt: new Date().toISOString(),
    documentCount: 0
  });
}

// Configuración para el tipo de almacenamiento vectorial
// En Cloud Foundry usar memoria por defecto, localmente python
const VECTOR_STORE_TYPE = process.env.VECTOR_STORE_TYPE || (process.env.NODE_ENV === 'production' ? 'memory' : 'python');

/**
 * Servicio principal de RAG (Retrieval-Augmented Generation)
 * Integrado completamente con SAP AI Core
 */

/**
 * Obtiene el vector store apropiado según configuración
 * @returns {Object} - Vector store a usar
 */
async function getVectorStore() {
  switch (VECTOR_STORE_TYPE) {
    case 'python':
      if (!chromaPythonClient.isInitialized) {
        console.log('[RAG] Inicializando ChromaDB Python Service...');
        try {
          await chromaPythonClient.initialize();
          return chromaPythonClient;
        } catch (error) {
          console.warn('[RAG] ⚠️  ChromaDB Python Service no disponible, usando memoria');
          console.warn('[RAG] 💡 Inicia el servicio: cd chroma_service && start_service.bat');
          return vectorStore;
        }
      }
      return chromaPythonClient;
      
    case 'chroma':
      if (!chromaVectorStore.isInitialized) {
        console.log('[RAG] Inicializando ChromaDB...');
        try {
          await chromaVectorStore.initialize();
          return chromaVectorStore;
        } catch (error) {
          console.warn('[RAG] ⚠️  ChromaDB no disponible, usando memoria');
          console.warn('[RAG] 💡 Para usar ChromaDB: docker run -p 8000:8000 chromadb/chroma');
          return vectorStore;
        }
      }
      return chromaVectorStore;
      
    case 'memory':
    default:
      return vectorStore;
  }
}

/**
 * Indexa un documento completo en el vector store
 * @param {string} filePath - Ruta al archivo
 * @param {string} mimeType - Tipo MIME del archivo
 * @param {Object} metadata - Metadatos adicionales del documento
 * @returns {Promise<Object>} - Información del documento indexado
 */
/**
 * Crea un nuevo contexto
 * @param {string} name - Nombre del contexto
 * @param {string} description - Descripción del contexto
 * @returns {Promise<Object>} - Información del contexto creado
 */
export async function createContext(name, description = '') {
  const contextId = uuidv4();
  const context = {
    id: contextId,
    name,
    description,
    createdAt: new Date().toISOString(),
    documentCount: 0
  };
  
  contexts.set(contextId, context);
  console.log(`[RAG] Contexto creado: ${name} (${contextId})`);
  
  return context;
}

/**
 * Lista todos los contextos
 * @returns {Promise<Array<Object>>} - Lista de contextos
 */
export async function listContexts() {
  return Array.from(contexts.values());
}

/**
 * Obtiene información de un contexto específico
 * @param {string} contextId - ID del contexto
 * @returns {Promise<Object|null>} - Información del contexto o null si no existe
 */
export async function getContextInfo(contextId) {
  return contexts.get(contextId) || null;
}

/**
 * Elimina un contexto y todos sus documentos
 * @param {string} contextId - ID del contexto
 * @returns {Promise<Object>} - Resultado de la eliminación
 */
export async function deleteContext(contextId) {
  if (contextId === 'default') {
    throw new Error('No se puede eliminar el contexto por defecto');
  }
  
  const context = contexts.get(contextId);
  if (!context) {
    return { deleted: false, contextId };
  }
  
  try {
    // Eliminar todos los documentos del contexto
    const store = await getVectorStore();
    const documents = await store.getDocumentsByContext(contextId);
    
    for (const doc of documents) {
      await store.deleteDocument(doc.documentId);
    }
    
    // Eliminar el contexto
    contexts.delete(contextId);
    
    console.log(`[RAG] Contexto eliminado: ${context.name} (${contextId})`);
    
    return {
      deleted: true,
      contextId,
      documentsDeleted: documents.length
    };
    
  } catch (error) {
    console.error('[RAG] Error eliminando contexto:', error);
    throw new Error(`Error eliminando contexto: ${error.message}`);
  }
}

export async function indexDocument(filePath, mimeType, metadata = {}) {
  try {
    console.log(`[RAG] Iniciando indexación de documento: ${filePath}`);
    
    // Generar ID único para el documento
    const documentId = uuidv4();
    
    // Procesar el documento (extraer texto y dividir en chunks)
    const { fullText, chunks, metadata: fileMetadata } = await processDocument(
      filePath,
      mimeType,
      { 
        chunkSize: 1000, 
        overlap: 200 
      }
    );
    
    console.log(`[RAG] Documento procesado: ${fileMetadata.fileName}`);
    console.log(`[RAG] Total de chunks: ${chunks.length}`);
    
    // Generar embeddings para todos los chunks usando SAP AI Core
    console.log(`[RAG] Generando embeddings con SAP AI Core...`);
    const embeddings = await generateEmbeddings(chunks);
    
    // Indexar cada chunk en el vector store
    const indexedChunks = [];
    for (let i = 0; i < chunks.length; i++) {
      const chunkId = `${documentId}_chunk_${i}`;
      const chunk = chunks[i];
      const embedding = embeddings[i];
      
      const chunkDocument = {
        id: chunkId,
        content: chunk,
        metadata: {
          documentId,
          fileName: fileMetadata.fileName,
          originalName: metadata.originalName || fileMetadata.fileName,
          chunkIndex: i,
          totalChunks: chunks.length,
          fileSize: fileMetadata.fileSize,
          uploadedAt: metadata.uploadedAt || new Date().toISOString(),
          contextId: metadata.contextId || 'default',
          ...metadata
        }
      };
      
      const store = await getVectorStore();
      await store.addDocument(chunkDocument, embedding);
      indexedChunks.push(chunkDocument);
    }
    
    // Actualizar contador de documentos en el contexto
    const contextId = metadata.contextId || 'default';
    const context = contexts.get(contextId);
    if (context) {
      context.documentCount += 1;
      contexts.set(contextId, context);
    }
    
    console.log(`[RAG] Documento indexado exitosamente: ${documentId} en contexto ${contextId}`);
    
    return {
      documentId,
      fileName: fileMetadata.fileName,
      originalName: metadata.originalName || fileMetadata.fileName,
      totalChunks: chunks.length,
      fileSize: fileMetadata.fileSize,
      indexedAt: new Date().toISOString(),
      chunks: indexedChunks.map(chunk => ({
        id: chunk.id,
        preview: chunk.content.substring(0, 100) + '...'
      }))
    };
    
  } catch (error) {
    console.error('[RAG] Error indexando documento:', error);
    throw new Error(`Error indexando documento: ${error.message}`);
  }
}

/**
 * Busca contexto relevante para una consulta
 * @param {string} query - Consulta del usuario
 * @param {Object} options - Opciones de búsqueda
 * @returns {Promise<Array<Object>>} - Chunks relevantes con scores
 */
export async function searchContext(query, options = {}) {
  const { 
    topK = 5, 
    minSimilarity = 0.1,
    documentId = null,
    contextId = 'default'
  } = options;
  
  try {
    console.log(`[RAG] Buscando contexto para: "${query.substring(0, 50)}..."`);
    
    // Generar embedding de la consulta usando SAP AI Core
    const queryEmbedding = await generateEmbedding(query);
    
    // Buscar en el vector store
    const store = await getVectorStore();
    let results;
    
    // Manejar diferentes tipos de stores
    if (store === chromaPythonClient) {
      // Para el cliente Python, usar el embedding ya generado
      results = await store.searchWithEmbedding(queryEmbedding, topK * 2, {});
    } else if (store === chromaVectorStore) {
      // Para ChromaDB directo, usar la query string
      results = await store.search(query, topK * 2, minSimilarity);
    } else {
      // Para vector store en memoria, usar el embedding
      results = store.search(queryEmbedding, topK * 2, minSimilarity);
    }
    
    // Filtrar por contexto
    if (contextId && contextId !== 'all') {
      results = results.filter(result => 
        result.metadata?.contextId === contextId
      );
    }
    
    // Filtrar por documento específico si se especifica
    if (documentId) {
      results = results.filter(result => 
        result.metadata?.documentId === documentId
      );
    }
    
    // Limitar a topK resultados finales
    results = results.slice(0, topK);
    
    console.log(`[RAG] Búsqueda completada: ${results.length} chunks encontrados`);
    
    return results.map(result => ({
      id: result.id,
      content: result.content,
      similarity: result.similarity,
      metadata: {
        fileName: result.metadata?.fileName,
        documentId: result.metadata?.documentId,
        chunkIndex: result.metadata?.chunkIndex,
        totalChunks: result.metadata?.totalChunks
      }
    }));
    
  } catch (error) {
    console.error('[RAG] Error buscando contexto:', error);
    throw new Error(`Error buscando contexto: ${error.message}`);
  }
}

/**
 * Genera una respuesta usando RAG con SAP AI Core
 * @param {string} query - Consulta del usuario
 * @param {Object} options - Opciones de configuración
 * @returns {Promise<Object>} - Respuesta con contexto y metadatos
 */
export async function generateRAGResponse(query, options = {}) {
  const { 
    topK = 5, 
    includeContext = true,
    documentId = null,
    contextId = 'default',
    model = "gpt-4o",
    systemPrompt = null
  } = options;
  
  try {
    console.log(`[RAG] Generando respuesta RAG para: "${query.substring(0, 50)}..."`);
    
    // Buscar contexto relevante
    const contextResults = await searchContext(query, { topK, documentId, contextId });
    
    if (contextResults.length === 0) {
      console.log('[RAG] No se encontró contexto relevante');
      return {
        answer: "No encontré información relevante en los documentos indexados para responder tu pregunta.",
        context: [],
        metadata: {
          chunksUsed: 0,
          sources: [],
          hasContext: false
        }
      };
    }
    
    // Construir el contexto para el prompt
    const contextText = contextResults
      .map((result, index) => {
        const source = result.metadata?.fileName || 'Documento desconocido';
        return `[Fuente ${index + 1}: ${source}]\n${result.content}`;
      })
      .join('\n\n---\n\n');
    
    // Construir el prompt del sistema
    const defaultSystemPrompt = `Eres un asistente experto que responde preguntas basándose únicamente en el contexto proporcionado.

INSTRUCCIONES:
- Responde SOLO basándote en la información del contexto
- Si la información no está en el contexto, indícalo claramente
- Cita las fuentes cuando sea relevante
- Sé preciso y conciso
- Si hay información contradictoria, menciónalo

CONTEXTO:
${contextText}`;

    const finalSystemPrompt = systemPrompt || defaultSystemPrompt;
    
    // Preparar mensajes para SAP AI Core
    const messages = [
      { role: 'system', content: finalSystemPrompt },
      { role: 'user', content: query }
    ];
    
    console.log(`[RAG] Generando respuesta con ${contextResults.length} chunks de contexto`);
    
    // Generar respuesta usando SAP AI Core
    const client = getAiCoreClient(model);
    const response = await client.run({ messages });
    const answer = response.getContent();
    
    // Extraer fuentes únicas
    const sources = [...new Set(contextResults
      .map(r => r.metadata?.fileName)
      .filter(Boolean)
    )];
    
    console.log(`[RAG] Respuesta generada exitosamente`);
    
    return {
      answer,
      context: includeContext ? contextResults : undefined,
      metadata: {
        chunksUsed: contextResults.length,
        sources,
        hasContext: true,
        model,
        queryLength: query.length,
        responseLength: answer.length
      }
    };
    
  } catch (error) {
    console.error('[RAG] Error generando respuesta RAG:', error);
    throw new Error(`Error generando respuesta RAG: ${error.message}`);
  }
}

/**
 * Lista todos los documentos indexados
 * @returns {Array<Object>} - Lista de documentos únicos con estadísticas
 */
export async function listDocuments(contextId = null) {
  try {
    const store = await getVectorStore();
    let documents = await store.getUniqueDocuments();
    
    // Filtrar por contexto si se especifica
    if (contextId && contextId !== 'all') {
      documents = documents.filter(doc => 
        doc.contextId === contextId || 
        (contextId === 'default' && !doc.contextId)
      );
    }
    
    return documents.map(doc => ({
      documentId: doc.documentId,
      fileName: doc.fileName,
      totalChunks: doc.totalChunks,
      addedAt: doc.addedAt,
      contextId: doc.contextId || 'default',
      chunks: doc.chunks?.length || 0
    }));
    
  } catch (error) {
    console.error('[RAG] Error listando documentos:', error);
    throw new Error(`Error listando documentos: ${error.message}`);
  }
}

/**
 * Obtiene información detallada de un documento
 * @param {string} documentId - ID del documento
 * @returns {Object|null} - Información del documento o null si no existe
 */
export async function getDocumentInfo(documentId) {
  try {
    const store = await getVectorStore();
    const chunks = await store.getDocumentChunks(documentId);
    
    if (chunks.length === 0) {
      return null;
    }
    
    const firstChunk = chunks[0];
    
    return {
      documentId,
      fileName: firstChunk.metadata?.fileName,
      originalName: firstChunk.metadata?.originalName,
      totalChunks: chunks.length,
      fileSize: firstChunk.metadata?.fileSize,
      uploadedAt: firstChunk.metadata?.uploadedAt,
      chunks: chunks.map(chunk => ({
        id: chunk.id,
        chunkIndex: chunk.metadata?.chunkIndex,
        preview: chunk.content.substring(0, 200) + '...',
        length: chunk.content.length
      }))
    };
    
  } catch (error) {
    console.error('[RAG] Error obteniendo info del documento:', error);
    throw new Error(`Error obteniendo información del documento: ${error.message}`);
  }
}

/**
 * Elimina un documento del índice
 * @param {string} documentId - ID del documento a eliminar
 * @returns {Object} - Resultado de la eliminación
 */
export async function deleteDocument(documentId) {
  try {
    const store = await getVectorStore();
    const result = await store.deleteDocument(documentId);
    const deletedChunks = result.chunksDeleted || result;
    
    console.log(`[RAG] Documento eliminado: ${documentId} (${deletedChunks} chunks)`);
    
    return {
      documentId,
      deleted: deletedChunks > 0,
      chunksDeleted: deletedChunks
    };
    
  } catch (error) {
    console.error('[RAG] Error eliminando documento:', error);
    throw new Error(`Error eliminando documento: ${error.message}`);
  }
}

/**
 * Obtiene estadísticas completas del RAG
 * @returns {Object} - Estadísticas detalladas
 */
export async function getRAGStats() {
  try {
    const store = await getVectorStore();
    const vectorStats = await store.getStats();
    const integrity = store.checkIntegrity ? await store.checkIntegrity() : { isValid: true };
    
    return {
      ...vectorStats,
      integrity,
      lastUpdated: new Date().toISOString()
    };
    
  } catch (error) {
    console.error('[RAG] Error obteniendo estadísticas:', error);
    throw new Error(`Error obteniendo estadísticas: ${error.message}`);
  }
}

/**
 * Procesa un pliego PDF con un prompt personalizado sin almacenarlo permanentemente
 * @param {string} filePath - Ruta del archivo PDF
 * @param {string} customPrompt - Prompt personalizado para el análisis
 * @param {Object} metadata - Metadatos del archivo
 * @returns {Promise<Object>} - Respuesta del análisis del pliego
 */
export async function processPliegoWithPrompt(filePath, customPrompt, metadata = {}) {
  try {
    console.log(`[RAG] Procesando pliego con prompt personalizado: ${filePath}`);
    
    // Procesar el documento PDF para extraer texto
    const documentData = await processDocument(filePath, 'application/pdf');
    
    if (!documentData.chunks || documentData.chunks.length === 0) {
      throw new Error('No se pudo extraer contenido del PDF');
    }
    
    // Combinar todo el contenido del documento
    const fullContent = documentData.chunks.map(chunk => chunk.content).join('\n\n');
    
    console.log(`[RAG] Contenido extraído: ${fullContent.length} caracteres`);
    
    // Crear el prompt completo combinando el prompt personalizado con el contenido
    const fullPrompt = `${customPrompt}

CONTENIDO DEL PLIEGO:
${fullContent}

Por favor, analiza el contenido del pliego basándote en las instrucciones proporcionadas.`;

    // Validar longitud del prompt
    let finalPrompt = fullPrompt;
    if (fullPrompt.length > 100000) {
      console.warn(`[RAG] Prompt muy largo: ${fullPrompt.length} caracteres, truncando...`);
      const maxContentLength = 80000 - customPrompt.length;
      const truncatedContent = fullContent.substring(0, maxContentLength) + '\n\n[CONTENIDO TRUNCADO...]';
      finalPrompt = `${customPrompt}

CONTENIDO DEL PLIEGO:
${truncatedContent}

Por favor, analiza el contenido del pliego basándote en las instrucciones proporcionadas.`;
    }

    console.log(`[RAG] Enviando prompt de ${finalPrompt.length} caracteres a SAP AI Core`);

    // Generar respuesta usando SAP AI Core
    let analysis;
    try {
      const client = getAiCoreClient('gpt-4o');
      const response = await client.run({
        messages: [
          {
            role: 'user',
            content: finalPrompt
          }
        ]
      });

      analysis = response.getContent();
      
      if (!analysis || analysis.trim().length === 0) {
        throw new Error('SAP AI Core devolvió una respuesta vacía');
      }

    } catch (aiError) {
      console.error(`[RAG] Error en SAP AI Core:`, aiError);
      throw new Error(`Error en SAP AI Core: ${aiError.message}`);
    }
    
    console.log(`[RAG] Análisis completado: ${analysis.length} caracteres`);
    
    // Si el prompt incluye "correcciones" o "ortografía", generar también el texto corregido
    let correctedText = null;
    let correctedPdfBuffer = null;
    
    if (customPrompt.toLowerCase().includes('correc') || customPrompt.toLowerCase().includes('ortograf')) {
      console.log(`[RAG] Generando correcciones ortográficas...`);
      
      try {
        // Limitar el contenido para correcciones si es muy largo
        let contentForCorrection = fullContent;
        if (fullContent.length > 80000) {
          console.warn(`[RAG] Contenido muy largo para correcciones: ${fullContent.length} caracteres, truncando...`);
          contentForCorrection = fullContent.substring(0, 80000) + '\n\n[CONTENIDO TRUNCADO...]';
        }

        const correctionPrompt = `Corrige únicamente los errores ortográficos y gramaticales del siguiente texto, manteniendo EXACTAMENTE el mismo formato, estructura, saltos de línea y estilo. No cambies el contenido, solo corrige errores:

${contentForCorrection}

IMPORTANTE: Devuelve SOLO el texto corregido, sin comentarios adicionales.`;

        console.log(`[RAG] Enviando ${correctionPrompt.length} caracteres para corrección`);

        const correctionClient = getAiCoreClient('gpt-4o');
        const correctionResponse = await correctionClient.run({
          messages: [
            {
              role: 'user',
              content: correctionPrompt
            }
          ]
        });

        correctedText = correctionResponse.getContent();
        
        if (!correctedText || correctedText.trim().length === 0) {
          throw new Error('SAP AI Core devolvió correcciones vacías');
        }
        
        console.log(`[RAG] Texto corregido generado: ${correctedText.length} caracteres`);
        
        // Generar PDF corregido
        try {
          correctedPdfBuffer = await createCorrectedPDF(filePath, correctedText);
          console.log(`[RAG] PDF corregido generado: ${correctedPdfBuffer.length} bytes`);
        } catch (pdfError) {
          console.warn(`[RAG] Error generando PDF corregido:`, pdfError.message);
        }
        
      } catch (correctionError) {
        console.error(`[RAG] Error detallado generando correcciones:`, {
          message: correctionError.message,
          status: correctionError.status,
          code: correctionError.code,
          response: correctionError.response?.data || 'No response data',
          stack: correctionError.stack?.split('\n').slice(0, 3).join('\n')
        });
        console.warn(`[RAG] Error generando correcciones: ${correctionError.message}`);
      }
    }
    
    return {
      success: true,
      analysis,
      correctedText,
      correctedPdfBuffer,
      metadata: {
        fileName: metadata.originalName || 'pliego.pdf',
        fileSize: documentData.metadata?.fileSize || 0,
        processedAt: new Date().toISOString(),
        customPrompt,
        contentLength: fullContent.length,
        chunksProcessed: documentData.chunks.length,
        model: 'gpt-4o',
        hasCorrectedText: !!correctedText,
        hasCorrectedPdf: !!correctedPdfBuffer
      }
    };
    
  } catch (error) {
    console.error('[RAG] Error procesando pliego:', error);
    throw new Error(`Error procesando pliego: ${error.message}`);
  }
}

/**
 * Crea un PDF corregido manteniendo el formato original
 * @param {string} originalPdfPath - Ruta del PDF original
 * @param {string} correctedText - Texto corregido
 * @returns {Promise<Buffer>} - Buffer del PDF corregido
 */
export async function createCorrectedPDF(originalPdfPath, correctedText) {
  try {
    console.log(`[PDF] Creando PDF corregido...`);
    
    // Leer el PDF original
    const originalPdfBytes = await fs.readFile(originalPdfPath);
    const originalPdf = await PDFDocument.load(originalPdfBytes);
    
    // Crear nuevo PDF
    const correctedPdf = await PDFDocument.create();
    
    // Obtener fuente estándar
    const font = await correctedPdf.embedFont(StandardFonts.Helvetica);
    const fontSize = 12;
    const margin = 50;
    
    // Obtener dimensiones de la primera página original
    const originalPages = originalPdf.getPages();
    const firstPage = originalPages[0];
    const { width, height } = firstPage.getSize();
    
    // Dividir el texto corregido en líneas que quepan en la página
    const maxWidth = width - (margin * 2);
    const lineHeight = fontSize * 1.2;
    const maxLinesPerPage = Math.floor((height - (margin * 2)) / lineHeight);
    
    // Función para dividir texto en líneas
    const wrapText = (text, maxWidth, font, fontSize) => {
      const words = text.split(' ');
      const lines = [];
      let currentLine = '';
      
      for (const word of words) {
        const testLine = currentLine + (currentLine ? ' ' : '') + word;
        const testWidth = font.widthOfTextAtSize(testLine, fontSize);
        
        if (testWidth <= maxWidth) {
          currentLine = testLine;
        } else {
          if (currentLine) {
            lines.push(currentLine);
            currentLine = word;
          } else {
            // Palabra muy larga, dividirla
            lines.push(word);
          }
        }
      }
      
      if (currentLine) {
        lines.push(currentLine);
      }
      
      return lines;
    };
    
    // Procesar el texto corregido
    const paragraphs = correctedText.split('\n\n');
    let allLines = [];
    
    for (const paragraph of paragraphs) {
      if (paragraph.trim()) {
        const wrappedLines = wrapText(paragraph.trim(), maxWidth, font, fontSize);
        allLines.push(...wrappedLines);
        allLines.push(''); // Línea vacía entre párrafos
      }
    }
    
    // Crear páginas con el texto corregido
    let currentPageLines = [];
    
    for (let i = 0; i < allLines.length; i++) {
      currentPageLines.push(allLines[i]);
      
      // Si la página está llena o es la última línea
      if (currentPageLines.length >= maxLinesPerPage || i === allLines.length - 1) {
        const page = correctedPdf.addPage([width, height]);
        
        // Añadir texto a la página
        let yPosition = height - margin;
        
        for (const line of currentPageLines) {
          if (yPosition > margin) {
            page.drawText(line, {
              x: margin,
              y: yPosition,
              size: fontSize,
              font: font,
              color: rgb(0, 0, 0)
            });
            yPosition -= lineHeight;
          }
        }
        
        currentPageLines = [];
      }
    }
    
    // Generar el PDF corregido
    const correctedPdfBytes = await correctedPdf.save();
    
    // Asegurar que devolvemos un Buffer
    const pdfBuffer = Buffer.from(correctedPdfBytes);
    
    console.log(`[PDF] PDF corregido creado exitosamente: ${pdfBuffer.length} bytes`);
    return pdfBuffer;
    
  } catch (error) {
    console.error('[PDF] Error creando PDF corregido:', error);
    throw new Error(`Error creando PDF corregido: ${error.message}`);
  }
}

/**
 * Limpia todo el índice RAG
 * @returns {Object} - Resultado de la limpieza
 */
export async function clearRAGIndex() {
  try {
    const store = await getVectorStore();
    const statsBefore = await store.getStats();
    await store.clear();
    
    console.log('[RAG] Índice RAG limpiado completamente');
    
    return {
      cleared: true,
      documentsRemoved: statsBefore.totalDocuments,
      chunksRemoved: statsBefore.totalChunks,
      clearedAt: new Date().toISOString()
    };
    
  } catch (error) {
    console.error('[RAG] Error limpiando índice:', error);
    throw new Error(`Error limpiando índice RAG: ${error.message}`);
  }
}
