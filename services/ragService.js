import { vectorStore } from './vectorStore.js';
import { chromaVectorStore } from './chromaVectorStore.js';
import { chromaPythonClient } from './chromaPythonClient.js';
import { generateEmbedding, generateEmbeddings } from './embeddingService.js';
import { processDocument } from './documentProcessor.js';
import { getAiCoreClient } from '../auth/aiCoreClient.js';
import { v4 as uuidv4 } from 'uuid';

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
          ...metadata
        }
      };
      
      const store = await getVectorStore();
      await store.addDocument(chunkDocument, embedding);
      indexedChunks.push(chunkDocument);
    }
    
    console.log(`[RAG] Documento indexado exitosamente: ${documentId}`);
    
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
    documentId = null 
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
    model = "gpt-4o",
    systemPrompt = null
  } = options;
  
  try {
    console.log(`[RAG] Generando respuesta RAG para: "${query.substring(0, 50)}..."`);
    
    // Buscar contexto relevante
    const contextResults = await searchContext(query, { topK, documentId });
    
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
export async function listDocuments() {
  try {
    const store = await getVectorStore();
    const documents = await store.getUniqueDocuments();
    
    return documents.map(doc => ({
      documentId: doc.documentId,
      fileName: doc.fileName,
      totalChunks: doc.totalChunks,
      addedAt: doc.addedAt,
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
