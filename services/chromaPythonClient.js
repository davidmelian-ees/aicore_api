import fetch from 'node-fetch';

/**
 * Cliente para conectar con el servicio ChromaDB Python
 * Proporciona interfaz compatible con el vector store original
 */
class ChromaPythonClient {
  constructor() {
    this.baseUrl = 'http://localhost:8001';
    this.isInitialized = false;
  }

  /**
   * Inicializa la conexión con el servicio Python
   */
  async initialize() {
    try {
      console.log('[CHROMA-PY] Conectando con servicio ChromaDB Python...');
      
      // Verificar que el servicio esté corriendo
      const response = await fetch(`${this.baseUrl}/health`);
      
      if (!response.ok) {
        throw new Error(`Servicio no disponible: ${response.status}`);
      }
      
      const health = await response.json();
      console.log('[CHROMA-PY] ✅ Conectado con ChromaDB Python');
      console.log(`[CHROMA-PY] ℹ️  Colección: ${health.collection_info.name}`);
      console.log(`[CHROMA-PY] ℹ️  Documentos: ${health.collection_info.document_count}`);
      
      this.isInitialized = true;
      return true;
      
    } catch (error) {
      console.error('[CHROMA-PY] ❌ Error conectando con servicio Python:', error.message);
      console.warn('[CHROMA-PY] 💡 Inicia el servicio con: cd chroma_service && start_service.bat');
      throw error;
    }
  }

  /**
   * Verifica que el cliente esté inicializado
   */
  _ensureInitialized() {
    if (!this.isInitialized) {
      throw new Error('ChromaPythonClient no está inicializado. Llama a initialize() primero.');
    }
  }

  /**
   * Agrega un documento al servicio ChromaDB Python
   * @param {Object} document - Documento a agregar
   * @param {Array<number>} embedding - Vector de embedding
   */
  async addDocument(document, embedding) {
    this._ensureInitialized();

    try {
      const response = await fetch(`${this.baseUrl}/documents`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          id: document.id,
          content: document.content,
          embedding: embedding,
          metadata: document.metadata || {}
        })
      });

      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.detail || `HTTP ${response.status}`);
      }

      const result = await response.json();
      console.log(`[CHROMA-PY] ✅ Documento agregado: ${document.id}`);
      
      return {
        id: document.id,
        success: result.success,
        embeddingDimension: result.embedding_dimension
      };

    } catch (error) {
      console.error(`[CHROMA-PY] ❌ Error agregando documento ${document.id}:`, error.message);
      throw error;
    }
  }

  /**
   * Busca documentos similares usando el servicio Python
   * @param {string} query - Consulta de texto (no se usa, se espera que ya se haya generado el embedding)
   * @param {number} topK - Número de resultados
   * @param {Object} filters - Filtros adicionales
   */
  async search(query, topK = 5, filters = {}) {
    this._ensureInitialized();

    try {
      // Nota: Este método espera que el embedding ya esté generado
      // Se llamará desde ragService que ya tiene el embedding
      console.log(`[CHROMA-PY] ⚠️  Método search requiere embedding pre-generado`);
      throw new Error('Use searchWithEmbedding() en su lugar');

    } catch (error) {
      console.error('[CHROMA-PY] ❌ Error en búsqueda:', error.message);
      throw error;
    }
  }

  /**
   * Busca documentos usando embedding pre-generado
   * @param {Array<number>} queryEmbedding - Embedding de la consulta
   * @param {number} topK - Número de resultados
   * @param {Object} filters - Filtros adicionales
   */
  async searchWithEmbedding(queryEmbedding, topK = 5, filters = {}) {
    this._ensureInitialized();

    try {
      const response = await fetch(`${this.baseUrl}/search`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          query_embedding: queryEmbedding,
          top_k: topK,
          filters: filters
        })
      });

      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.detail || `HTTP ${response.status}`);
      }

      const results = await response.json();
      
      console.log(`[CHROMA-PY] ✅ Búsqueda completada: ${results.length} resultados`);
      
      return results.map(result => ({
        id: result.id,
        similarity: result.similarity,
        content: result.content,
        metadata: result.metadata
      }));

    } catch (error) {
      console.error('[CHROMA-PY] ❌ Error en búsqueda:', error.message);
      throw error;
    }
  }

  /**
   * Obtiene chunks de un documento específico
   * @param {string} documentId - ID del documento padre
   */
  async getDocumentChunks(documentId) {
    this._ensureInitialized();

    try {
      const response = await fetch(`${this.baseUrl}/documents`);
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`);
      }

      const data = await response.json();
      
      // Filtrar por documentId en los metadatos
      const chunks = data.documents.filter(doc => 
        doc.metadata && doc.metadata.documentId === documentId
      );

      // Ordenar por índice de chunk
      chunks.sort((a, b) => 
        (a.metadata?.chunkIndex || 0) - (b.metadata?.chunkIndex || 0)
      );

      return chunks.map(chunk => ({
        id: chunk.id,
        content: chunk.content || '',
        metadata: chunk.metadata || {}
      }));

    } catch (error) {
      console.error(`[CHROMA-PY] ❌ Error obteniendo chunks del documento ${documentId}:`, error.message);
      throw error;
    }
  }

  /**
   * Obtiene todos los documentos únicos
   */
  async getUniqueDocuments() {
    this._ensureInitialized();

    try {
      const response = await fetch(`${this.baseUrl}/documents`);
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`);
      }

      const data = await response.json();
      const documentsMap = new Map();

      // Agrupar por documentId
      for (const doc of data.documents) {
        const docId = doc.metadata?.documentId;
        if (docId && !documentsMap.has(docId)) {
          documentsMap.set(docId, {
            documentId: docId,
            fileName: doc.metadata?.fileName,
            addedAt: doc.metadata?.added_at,
            chunks: []
          });
        }
        
        if (docId) {
          documentsMap.get(docId).chunks.push(doc.id);
        }
      }

      return Array.from(documentsMap.values()).map(doc => ({
        ...doc,
        totalChunks: doc.chunks.length
      }));

    } catch (error) {
      console.error('[CHROMA-PY] ❌ Error obteniendo documentos únicos:', error.message);
      throw error;
    }
  }

  /**
   * Elimina un documento y todos sus chunks
   * @param {string} documentId - ID del documento a eliminar
   */
  async deleteDocument(documentId) {
    this._ensureInitialized();

    try {
      // Primero obtener todos los chunks del documento
      const chunks = await this.getDocumentChunks(documentId);
      
      if (chunks.length === 0) {
        return {
          deleted: false,
          chunksDeleted: 0,
          message: 'Documento no encontrado'
        };
      }

      // Eliminar cada chunk
      let deletedCount = 0;
      for (const chunk of chunks) {
        try {
          const response = await fetch(`${this.baseUrl}/documents/${chunk.id}`, {
            method: 'DELETE'
          });
          
          if (response.ok) {
            deletedCount++;
          }
        } catch (error) {
          console.warn(`[CHROMA-PY] ⚠️  Error eliminando chunk ${chunk.id}:`, error.message);
        }
      }

      console.log(`[CHROMA-PY] ✅ Documento eliminado: ${documentId} (${deletedCount} chunks)`);

      return {
        deleted: deletedCount > 0,
        chunksDeleted: deletedCount,
        documentId: documentId
      };

    } catch (error) {
      console.error(`[CHROMA-PY] ❌ Error eliminando documento ${documentId}:`, error.message);
      throw error;
    }
  }

  /**
   * Limpia toda la colección
   */
  async clear() {
    this._ensureInitialized();

    try {
      const response = await fetch(`${this.baseUrl}/clear?confirm=DELETE_ALL`, {
        method: 'DELETE'
      });

      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.detail || `HTTP ${response.status}`);
      }

      const result = await response.json();
      console.log('[CHROMA-PY] ✅ Colección limpiada completamente');

      return result;

    } catch (error) {
      console.error('[CHROMA-PY] ❌ Error limpiando colección:', error.message);
      throw error;
    }
  }

  /**
   * Obtiene estadísticas de la colección
   */
  async getStats() {
    this._ensureInitialized();

    try {
      const response = await fetch(`${this.baseUrl}/stats`);
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`);
      }

      const stats = await response.json();

      return {
        totalChunks: stats.total_documents,
        totalDocuments: (await this.getUniqueDocuments()).length,
        embeddingDimension: stats.embedding_dimension,
        collectionName: stats.collection_name,
        isInitialized: this.isInitialized,
        integrity: {
          isValid: true,
          checkedAt: new Date().toISOString()
        }
      };

    } catch (error) {
      console.error('[CHROMA-PY] ❌ Error obteniendo estadísticas:', error.message);
      throw error;
    }
  }

  /**
   * Verifica la salud de la conexión
   */
  async healthCheck() {
    try {
      if (!this.isInitialized) {
        return {
          status: 'unhealthy',
          message: 'ChromaPythonClient no inicializado'
        };
      }

      const response = await fetch(`${this.baseUrl}/health`);
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`);
      }

      const health = await response.json();

      return {
        status: health.status,
        message: health.message,
        documentsCount: health.collection_info.document_count,
        collectionName: health.collection_info.name
      };

    } catch (error) {
      return {
        status: 'unhealthy',
        message: error.message,
        suggestion: 'Verifica que el servicio Python esté corriendo en http://localhost:8001'
      };
    }
  }
}

// Instancia singleton
export const chromaPythonClient = new ChromaPythonClient();

// Exportar la clase
export { ChromaPythonClient };
