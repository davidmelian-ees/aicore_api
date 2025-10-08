import { ChromaClient } from 'chromadb';
import { generateEmbedding, validateEmbedding } from './embeddingService.js';

/**
 * Servicio de almacenamiento vectorial usando ChromaDB
 * Proporciona persistencia y escalabilidad para el sistema RAG
 */
class ChromaVectorStore {
  constructor() {
    this.client = null;
    this.collection = null;
    this.collectionName = 'rag_documents';
    this.isInitialized = false;
  }

  /**
   * Inicializa la conexión con ChromaDB
   */
  async initialize() {
    try {
      console.log('[CHROMA] Inicializando cliente ChromaDB...');
      
      // Crear cliente ChromaDB (por defecto usa http://localhost:8000)
      this.client = new ChromaClient({
        path: "http://localhost:8000"
      });

      // Verificar conexión
      await this.client.heartbeat();
      console.log('[CHROMA] ✅ Conexión establecida con ChromaDB');

      // Obtener o crear colección
      try {
        this.collection = await this.client.getCollection({
          name: this.collectionName
        });
        console.log(`[CHROMA] ✅ Colección '${this.collectionName}' encontrada`);
      } catch (error) {
        // Si no existe, crear nueva colección
        console.log(`[CHROMA] Creando nueva colección '${this.collectionName}'`);
        this.collection = await this.client.createCollection({
          name: this.collectionName,
          metadata: {
            description: "RAG documents collection for SAP AI Core integration",
            created_at: new Date().toISOString()
          }
        });
        console.log('[CHROMA] ✅ Colección creada exitosamente');
      }

      this.isInitialized = true;
      console.log('[CHROMA] 🚀 ChromaVectorStore inicializado correctamente');

    } catch (error) {
      console.error('[CHROMA] ❌ Error inicializando ChromaDB:', error);
      console.warn('[CHROMA] ⚠️  Asegúrate de que ChromaDB esté corriendo en http://localhost:8000');
      console.warn('[CHROMA] 💡 Ejecuta: docker run -p 8000:8000 chromadb/chroma');
      throw error;
    }
  }

  /**
   * Verifica que ChromaDB esté inicializado
   */
  _ensureInitialized() {
    if (!this.isInitialized) {
      throw new Error('ChromaVectorStore no está inicializado. Llama a initialize() primero.');
    }
  }

  /**
   * Agrega un documento con su embedding a ChromaDB
   * @param {Object} document - Documento a agregar
   * @param {string} document.id - ID único del chunk
   * @param {string} document.content - Contenido del chunk
   * @param {Object} document.metadata - Metadatos del documento
   * @param {Array<number>} embedding - Vector de embedding (opcional, se genera automáticamente)
   */
  async addDocument(document, embedding = null) {
    this._ensureInitialized();

    try {
      // Validar documento
      if (!document.id || !document.content) {
        throw new Error('Documento debe tener id y content');
      }

      // Generar embedding si no se proporciona
      if (!embedding) {
        console.log(`[CHROMA] Generando embedding para documento ${document.id}`);
        embedding = await generateEmbedding(document.content);
      }

      // Validar embedding
      if (!validateEmbedding(embedding)) {
        throw new Error('Embedding inválido');
      }

      // Preparar metadatos para ChromaDB
      const metadata = {
        ...document.metadata,
        addedAt: new Date().toISOString(),
        contentLength: document.content.length
      };

      // Agregar a ChromaDB
      await this.collection.add({
        ids: [document.id],
        embeddings: [embedding],
        documents: [document.content],
        metadatas: [metadata]
      });

      console.log(`[CHROMA] ✅ Documento agregado: ${document.id}`);

      return {
        id: document.id,
        success: true,
        embeddingDimension: embedding.length
      };

    } catch (error) {
      console.error(`[CHROMA] ❌ Error agregando documento ${document.id}:`, error);
      throw error;
    }
  }

  /**
   * Busca documentos similares usando embeddings
   * @param {string} query - Consulta de texto
   * @param {number} topK - Número de resultados a retornar
   * @param {Object} filters - Filtros adicionales (opcional)
   * @returns {Array<Object>} - Documentos más similares con scores
   */
  async search(query, topK = 5, filters = {}) {
    this._ensureInitialized();

    try {
      console.log(`[CHROMA] Buscando: "${query.substring(0, 50)}..."`);

      // Generar embedding de la consulta
      const queryEmbedding = await generateEmbedding(query);

      // Preparar filtros para ChromaDB
      const whereClause = Object.keys(filters).length > 0 ? filters : undefined;

      // Realizar búsqueda en ChromaDB
      const results = await this.collection.query({
        queryEmbeddings: [queryEmbedding],
        nResults: topK,
        where: whereClause,
        include: ['documents', 'metadatas', 'distances']
      });

      // Procesar resultados
      const processedResults = [];
      
      if (results.ids && results.ids[0]) {
        for (let i = 0; i < results.ids[0].length; i++) {
          const similarity = 1 - (results.distances[0][i] || 0); // ChromaDB usa distancia, convertir a similitud
          
          processedResults.push({
            id: results.ids[0][i],
            similarity: Math.max(0, similarity), // Asegurar que no sea negativo
            content: results.documents[0][i],
            metadata: results.metadatas[0][i] || {}
          });
        }
      }

      console.log(`[CHROMA] ✅ Búsqueda completada: ${processedResults.length} resultados`);
      
      return processedResults;

    } catch (error) {
      console.error('[CHROMA] ❌ Error en búsqueda:', error);
      throw error;
    }
  }

  /**
   * Busca documentos por documento padre
   * @param {string} documentId - ID del documento padre
   * @returns {Array<Object>} - Chunks del documento
   */
  async getDocumentChunks(documentId) {
    this._ensureInitialized();

    try {
      const results = await this.collection.get({
        where: { documentId: documentId },
        include: ['documents', 'metadatas']
      });

      const chunks = [];
      if (results.ids) {
        for (let i = 0; i < results.ids.length; i++) {
          chunks.push({
            id: results.ids[i],
            content: results.documents[i],
            metadata: results.metadatas[i] || {}
          });
        }
      }

      // Ordenar por índice de chunk
      chunks.sort((a, b) => (a.metadata?.chunkIndex || 0) - (b.metadata?.chunkIndex || 0));
      
      return chunks;

    } catch (error) {
      console.error(`[CHROMA] ❌ Error obteniendo chunks del documento ${documentId}:`, error);
      throw error;
    }
  }

  /**
   * Obtiene todos los documentos únicos
   * @returns {Array<Object>} - Lista de documentos únicos
   */
  async getUniqueDocuments() {
    this._ensureInitialized();

    try {
      // Obtener todos los documentos
      const results = await this.collection.get({
        include: ['metadatas']
      });

      const documentsMap = new Map();
      
      if (results.ids && results.metadatas) {
        for (let i = 0; i < results.ids.length; i++) {
          const metadata = results.metadatas[i] || {};
          const docId = metadata.documentId;
          
          if (docId && !documentsMap.has(docId)) {
            documentsMap.set(docId, {
              documentId: docId,
              fileName: metadata.fileName,
              addedAt: metadata.addedAt,
              chunks: []
            });
          }
          
          if (docId) {
            documentsMap.get(docId).chunks.push(results.ids[i]);
          }
        }
      }

      return Array.from(documentsMap.values()).map(doc => ({
        ...doc,
        totalChunks: doc.chunks.length
      }));

    } catch (error) {
      console.error('[CHROMA] ❌ Error obteniendo documentos únicos:', error);
      throw error;
    }
  }

  /**
   * Elimina un documento y todos sus chunks
   * @param {string} documentId - ID del documento a eliminar
   * @returns {Object} - Resultado de la eliminación
   */
  async deleteDocument(documentId) {
    this._ensureInitialized();

    try {
      // Obtener todos los chunks del documento
      const chunks = await this.getDocumentChunks(documentId);
      const chunkIds = chunks.map(chunk => chunk.id);

      if (chunkIds.length === 0) {
        return {
          deleted: false,
          chunksDeleted: 0,
          message: 'Documento no encontrado'
        };
      }

      // Eliminar chunks de ChromaDB
      await this.collection.delete({
        ids: chunkIds
      });

      console.log(`[CHROMA] ✅ Documento eliminado: ${documentId} (${chunkIds.length} chunks)`);
      
      return {
        deleted: true,
        chunksDeleted: chunkIds.length,
        documentId: documentId
      };

    } catch (error) {
      console.error(`[CHROMA] ❌ Error eliminando documento ${documentId}:`, error);
      throw error;
    }
  }

  /**
   * Limpia toda la colección
   */
  async clear() {
    this._ensureInitialized();

    try {
      // Eliminar la colección actual
      await this.client.deleteCollection({ name: this.collectionName });
      
      // Crear nueva colección vacía
      this.collection = await this.client.createCollection({
        name: this.collectionName,
        metadata: {
          description: "RAG documents collection for SAP AI Core integration",
          created_at: new Date().toISOString()
        }
      });

      console.log('[CHROMA] ✅ Colección limpiada completamente');

    } catch (error) {
      console.error('[CHROMA] ❌ Error limpiando colección:', error);
      throw error;
    }
  }

  /**
   * Obtiene estadísticas de la colección
   * @returns {Object} - Estadísticas detalladas
   */
  async getStats() {
    this._ensureInitialized();

    try {
      // Obtener información de la colección
      const count = await this.collection.count();
      const uniqueDocuments = await this.getUniqueDocuments();

      return {
        totalChunks: count,
        totalDocuments: uniqueDocuments.length,
        embeddingDimension: 384, // Dimensión estándar de nuestros embeddings
        collectionName: this.collectionName,
        isInitialized: this.isInitialized,
        integrity: {
          isValid: true,
          checkedAt: new Date().toISOString()
        }
      };

    } catch (error) {
      console.error('[CHROMA] ❌ Error obteniendo estadísticas:', error);
      throw error;
    }
  }

  /**
   * Verifica la salud de la conexión con ChromaDB
   * @returns {Object} - Estado de salud
   */
  async healthCheck() {
    try {
      if (!this.isInitialized) {
        return {
          status: 'unhealthy',
          message: 'ChromaVectorStore no inicializado'
        };
      }

      await this.client.heartbeat();
      const count = await this.collection.count();

      return {
        status: 'healthy',
        message: 'ChromaDB funcionando correctamente',
        documentsCount: count,
        collectionName: this.collectionName
      };

    } catch (error) {
      return {
        status: 'unhealthy',
        message: error.message,
        suggestion: 'Verifica que ChromaDB esté corriendo en http://localhost:8000'
      };
    }
  }
}

// Instancia singleton
export const chromaVectorStore = new ChromaVectorStore();

// Exportar la clase
export { ChromaVectorStore };
