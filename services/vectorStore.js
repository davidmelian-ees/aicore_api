import { calculateCosineSimilarity, validateEmbedding } from './embeddingService.js';

/**
 * Almacenamiento vectorial optimizado para documentos RAG
 * Integrado con SAP AI Core embeddings
 */
class VectorStore {
  constructor() {
    this.documents = new Map(); // Map para acceso rápido por ID
    this.embeddings = new Map(); // Map de embeddings por documento ID
    this.metadata = new Map(); // Metadatos adicionales
    this.index = []; // Índice para búsquedas rápidas
  }

  /**
   * Agrega un documento con su embedding al almacenamiento
   * @param {Object} document - Documento a agregar
   * @param {string} document.id - ID único del chunk
   * @param {string} document.content - Contenido del chunk
   * @param {Object} document.metadata - Metadatos del documento
   * @param {Array<number>} embedding - Vector de embedding
   */
  addDocument(document, embedding) {
    try {
      // Validar embedding
      if (!validateEmbedding(embedding)) {
        throw new Error('Embedding inválido');
      }

      // Validar documento
      if (!document.id || !document.content) {
        throw new Error('Documento debe tener id y content');
      }

      // Almacenar documento
      this.documents.set(document.id, {
        id: document.id,
        content: document.content,
        metadata: document.metadata || {},
        addedAt: new Date().toISOString()
      });

      // Almacenar embedding
      this.embeddings.set(document.id, embedding);

      // Actualizar índice
      this.index.push({
        id: document.id,
        documentId: document.metadata?.documentId,
        fileName: document.metadata?.fileName
      });

      console.log(`[VECTOR STORE] Documento agregado: ${document.id}`);

    } catch (error) {
      console.error('[VECTOR STORE] Error agregando documento:', error);
      throw error;
    }
  }

  /**
   * Busca documentos similares usando embeddings
   * @param {Array<number>} queryEmbedding - Embedding de la consulta
   * @param {number} topK - Número de resultados a retornar
   * @param {number} minSimilarity - Similitud mínima (0-1)
   * @returns {Array<Object>} - Documentos más similares con scores
   */
  search(queryEmbedding, topK = 5, minSimilarity = 0.1) {
    try {
      if (!validateEmbedding(queryEmbedding)) {
        throw new Error('Query embedding inválido');
      }

      const results = [];

      // Calcular similitudes para todos los documentos
      for (const [docId, embedding] of this.embeddings) {
        const similarity = calculateCosineSimilarity(queryEmbedding, embedding);
        
        if (similarity >= minSimilarity) {
          const document = this.documents.get(docId);
          results.push({
            id: docId,
            similarity,
            content: document.content,
            metadata: document.metadata
          });
        }
      }

      // Ordenar por similitud descendente
      results.sort((a, b) => b.similarity - a.similarity);

      // Retornar top K resultados
      const topResults = results.slice(0, topK);
      
      console.log(`[VECTOR STORE] Búsqueda completada: ${topResults.length}/${results.length} resultados`);
      
      return topResults;

    } catch (error) {
      console.error('[VECTOR STORE] Error en búsqueda:', error);
      throw error;
    }
  }

  /**
   * Busca documentos por documento padre
   * @param {string} documentId - ID del documento padre
   * @returns {Array<Object>} - Chunks del documento
   */
  getDocumentChunks(documentId) {
    const chunks = [];
    
    for (const [chunkId, document] of this.documents) {
      if (document.metadata?.documentId === documentId) {
        chunks.push({
          id: chunkId,
          content: document.content,
          metadata: document.metadata
        });
      }
    }

    // Ordenar por índice de chunk
    chunks.sort((a, b) => (a.metadata?.chunkIndex || 0) - (b.metadata?.chunkIndex || 0));
    
    return chunks;
  }

  /**
   * Obtiene todos los documentos únicos
   * @returns {Array<Object>} - Lista de documentos únicos
   */
  getUniqueDocuments() {
    const documentsMap = new Map();
    
    for (const [chunkId, document] of this.documents) {
      const docId = document.metadata?.documentId;
      if (docId && !documentsMap.has(docId)) {
        documentsMap.set(docId, {
          documentId: docId,
          fileName: document.metadata?.fileName,
          addedAt: document.addedAt,
          chunks: []
        });
      }
    }

    // Contar chunks por documento
    for (const [chunkId, document] of this.documents) {
      const docId = document.metadata?.documentId;
      if (docId && documentsMap.has(docId)) {
        documentsMap.get(docId).chunks.push(chunkId);
      }
    }

    return Array.from(documentsMap.values()).map(doc => ({
      ...doc,
      totalChunks: doc.chunks.length
    }));
  }

  /**
   * Elimina un documento y todos sus chunks
   * @param {string} documentId - ID del documento a eliminar
   * @returns {number} - Número de chunks eliminados
   */
  deleteDocument(documentId) {
    let deletedCount = 0;
    const chunksToDelete = [];

    // Encontrar todos los chunks del documento
    for (const [chunkId, document] of this.documents) {
      if (document.metadata?.documentId === documentId) {
        chunksToDelete.push(chunkId);
      }
    }

    // Eliminar chunks
    for (const chunkId of chunksToDelete) {
      this.documents.delete(chunkId);
      this.embeddings.delete(chunkId);
      deletedCount++;
    }

    // Actualizar índice
    this.index = this.index.filter(item => item.documentId !== documentId);

    console.log(`[VECTOR STORE] Documento eliminado: ${documentId} (${deletedCount} chunks)`);
    
    return deletedCount;
  }

  /**
   * Elimina un chunk específico
   * @param {string} chunkId - ID del chunk a eliminar
   * @returns {boolean} - True si se eliminó exitosamente
   */
  deleteChunk(chunkId) {
    const deleted = this.documents.delete(chunkId) && this.embeddings.delete(chunkId);
    
    if (deleted) {
      this.index = this.index.filter(item => item.id !== chunkId);
      console.log(`[VECTOR STORE] Chunk eliminado: ${chunkId}`);
    }
    
    return deleted;
  }

  /**
   * Limpia todo el almacenamiento
   */
  clear() {
    this.documents.clear();
    this.embeddings.clear();
    this.metadata.clear();
    this.index = [];
    console.log('[VECTOR STORE] Almacenamiento limpiado');
  }

  /**
   * Obtiene estadísticas del almacenamiento
   * @returns {Object} - Estadísticas detalladas
   */
  getStats() {
    const uniqueDocuments = new Set();
    const fileNames = new Set();
    
    for (const document of this.documents.values()) {
      if (document.metadata?.documentId) {
        uniqueDocuments.add(document.metadata.documentId);
      }
      if (document.metadata?.fileName) {
        fileNames.add(document.metadata.fileName);
      }
    }

    const embeddingDimensions = this.embeddings.size > 0 
      ? Array.from(this.embeddings.values())[0].length 
      : 0;

    return {
      totalChunks: this.documents.size,
      totalDocuments: uniqueDocuments.size,
      totalFiles: fileNames.size,
      embeddingDimension: embeddingDimensions,
      indexSize: this.index.length,
      memoryUsage: {
        documents: this.documents.size,
        embeddings: this.embeddings.size,
        metadata: this.metadata.size
      }
    };
  }

  /**
   * Exporta el almacenamiento a JSON (sin embeddings por tamaño)
   * @returns {Object} - Datos exportables
   */
  export() {
    return {
      documents: Array.from(this.documents.entries()),
      index: this.index,
      stats: this.getStats(),
      exportedAt: new Date().toISOString()
    };
  }

  /**
   * Verifica la integridad del almacenamiento
   * @returns {Object} - Reporte de integridad
   */
  checkIntegrity() {
    const issues = [];
    
    // Verificar que cada documento tenga su embedding
    for (const docId of this.documents.keys()) {
      if (!this.embeddings.has(docId)) {
        issues.push(`Documento ${docId} no tiene embedding`);
      }
    }

    // Verificar que cada embedding tenga su documento
    for (const docId of this.embeddings.keys()) {
      if (!this.documents.has(docId)) {
        issues.push(`Embedding ${docId} no tiene documento`);
      }
    }

    return {
      isValid: issues.length === 0,
      issues,
      checkedAt: new Date().toISOString()
    };
  }
}

// Instancia singleton del vector store
export const vectorStore = new VectorStore();

// Exportar la clase para crear instancias adicionales si es necesario
export { VectorStore };
