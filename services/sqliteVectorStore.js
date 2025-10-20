import Database from 'better-sqlite3';
import { generateEmbedding, validateEmbedding, calculateCosineSimilarity } from './embeddingService.js';
import fs from 'fs/promises';
import path from 'path';

/**
 * Vector Store usando SQLite para persistencia completa
 * Compatible con Cloud Foundry y desarrollo local
 */
class SQLiteVectorStore {
  constructor() {
    this.db = null;
    this.isInitialized = false;
    this.dbPath = './data/rag_vectors.db';
  }

  /**
   * Inicializa la base de datos SQLite
   */
  async initialize() {
    try {
      console.log('[SQLITE-VECTOR] Inicializando SQLite Vector Store...');
      
      // Crear directorio de datos si no existe
      await fs.mkdir('./data', { recursive: true });
      
      // Crear conexión a SQLite
      this.db = new Database(this.dbPath);
      
      // Crear tablas si no existen
      this.createTables();
      
      // Configurar SQLite para mejor rendimiento
      this.db.pragma('journal_mode = WAL');
      this.db.pragma('synchronous = NORMAL');
      this.db.pragma('cache_size = 1000');
      
      this.isInitialized = true;
      console.log('[SQLITE-VECTOR] ✅ SQLite Vector Store inicializado');
      console.log(`[SQLITE-VECTOR] 📁 Base de datos: ${this.dbPath}`);
      
      // Mostrar estadísticas
      const stats = this.getStats();
      console.log(`[SQLITE-VECTOR] 📊 Documentos: ${stats.totalDocuments}, Chunks: ${stats.totalChunks}`);
      
    } catch (error) {
      console.error('[SQLITE-VECTOR] ❌ Error inicializando SQLite:', error);
      throw error;
    }
  }

  /**
   * Crea las tablas necesarias
   */
  createTables() {
    // Tabla para documentos
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS documents (
        id TEXT PRIMARY KEY,
        content TEXT NOT NULL,
        embedding TEXT NOT NULL,
        metadata TEXT NOT NULL,
        context_id TEXT NOT NULL,
        document_id TEXT NOT NULL,
        chunk_index INTEGER NOT NULL,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP
      )
    `);

    // Índices para mejor rendimiento
    this.db.exec(`
      CREATE INDEX IF NOT EXISTS idx_context_id ON documents(context_id);
      CREATE INDEX IF NOT EXISTS idx_document_id ON documents(document_id);
      CREATE INDEX IF NOT EXISTS idx_created_at ON documents(created_at);
    `);

    console.log('[SQLITE-VECTOR] ✅ Tablas creadas/verificadas');
  }

  /**
   * Verifica que esté inicializado
   */
  _ensureInitialized() {
    if (!this.isInitialized) {
      throw new Error('SQLiteVectorStore no está inicializado. Llama a initialize() primero.');
    }
  }

  /**
   * Agrega un documento con su embedding
   * @param {Object} document - Documento a agregar
   * @param {Array<number>} embedding - Vector de embedding
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
        console.log(`[SQLITE-VECTOR] Generando embedding para documento ${document.id}`);
        embedding = await generateEmbedding(document.content);
      }

      // Validar embedding
      if (!validateEmbedding(embedding)) {
        throw new Error('Embedding inválido');
      }

      // Preparar datos para SQLite
      const embeddingJson = JSON.stringify(embedding);
      const metadataJson = JSON.stringify({
        ...document.metadata,
        addedAt: new Date().toISOString(),
        contentLength: document.content.length
      });

      // Insertar en base de datos
      const stmt = this.db.prepare(`
        INSERT OR REPLACE INTO documents 
        (id, content, embedding, metadata, context_id, document_id, chunk_index)
        VALUES (?, ?, ?, ?, ?, ?, ?)
      `);

      stmt.run(
        document.id,
        document.content,
        embeddingJson,
        metadataJson,
        document.metadata?.contextId || 'default',
        document.metadata?.documentId || document.id,
        document.metadata?.chunkIndex || 0
      );

      console.log(`[SQLITE-VECTOR] ✅ Documento agregado: ${document.id}`);

      return {
        id: document.id,
        success: true,
        embeddingDimension: embedding.length
      };

    } catch (error) {
      console.error(`[SQLITE-VECTOR] ❌ Error agregando documento ${document.id}:`, error);
      throw error;
    }
  }

  /**
   * Busca documentos similares usando embeddings
   * @param {Array<number>} queryEmbedding - Embedding de la consulta
   * @param {number} topK - Número de resultados
   * @param {Object} filters - Filtros adicionales
   */
  async search(queryEmbedding, topK = 5, filters = {}) {
    this._ensureInitialized();

    try {
      if (!validateEmbedding(queryEmbedding)) {
        throw new Error('Query embedding inválido');
      }

      // Construir consulta SQL con filtros
      let sql = 'SELECT * FROM documents';
      const params = [];

      if (filters.contextId) {
        sql += ' WHERE context_id = ?';
        params.push(filters.contextId);
      }

      // Obtener todos los documentos (SQLite no tiene funciones de similitud coseno nativas)
      const stmt = this.db.prepare(sql);
      const rows = stmt.all(...params);

      // Calcular similitudes en memoria
      const results = [];
      for (const row of rows) {
        try {
          const embedding = JSON.parse(row.embedding);
          const similarity = calculateCosineSimilarity(queryEmbedding, embedding);
          
          if (similarity >= (filters.minSimilarity || 0.1)) {
            results.push({
              id: row.id,
              similarity,
              content: row.content,
              metadata: JSON.parse(row.metadata)
            });
          }
        } catch (error) {
          console.warn(`[SQLITE-VECTOR] ⚠️ Error procesando embedding para ${row.id}`);
        }
      }

      // Ordenar por similitud y tomar top K
      results.sort((a, b) => b.similarity - a.similarity);
      const topResults = results.slice(0, topK);

      console.log(`[SQLITE-VECTOR] 🔍 Búsqueda completada: ${topResults.length}/${results.length} resultados`);
      
      return topResults;

    } catch (error) {
      console.error('[SQLITE-VECTOR] ❌ Error en búsqueda:', error);
      throw error;
    }
  }

  /**
   * Obtiene documentos únicos
   */
  getUniqueDocuments() {
    this._ensureInitialized();

    const stmt = this.db.prepare(`
      SELECT 
        document_id,
        context_id,
        MIN(metadata) as metadata,
        COUNT(*) as total_chunks,
        MIN(created_at) as created_at
      FROM documents 
      GROUP BY document_id, context_id
      ORDER BY created_at DESC
    `);

    const rows = stmt.all();
    
    return rows.map(row => {
      const metadata = JSON.parse(row.metadata);
      return {
        documentId: row.document_id,
        contextId: row.context_id,
        fileName: metadata.fileName || row.document_id,
        totalChunks: row.total_chunks,
        addedAt: row.created_at,
        chunks: [] // Se puede llenar si es necesario
      };
    });
  }

  /**
   * Obtiene documentos por contexto
   */
  getDocumentsByContext(contextId) {
    const allDocs = this.getUniqueDocuments();
    return allDocs.filter(doc => 
      doc.contextId === contextId || 
      (contextId === 'default' && !doc.contextId)
    );
  }

  /**
   * Elimina un documento y todos sus chunks
   */
  deleteDocument(documentId) {
    this._ensureInitialized();

    const stmt = this.db.prepare('DELETE FROM documents WHERE document_id = ?');
    const result = stmt.run(documentId);

    console.log(`[SQLITE-VECTOR] 🗑️ Documento eliminado: ${documentId} (${result.changes} chunks)`);
    
    return result.changes;
  }

  /**
   * Elimina un chunk específico
   */
  deleteChunk(chunkId) {
    this._ensureInitialized();

    const stmt = this.db.prepare('DELETE FROM documents WHERE id = ?');
    const result = stmt.run(chunkId);

    if (result.changes > 0) {
      console.log(`[SQLITE-VECTOR] 🗑️ Chunk eliminado: ${chunkId}`);
    }
    
    return result.changes > 0;
  }

  /**
   * Limpia todo el almacenamiento
   */
  clear() {
    this._ensureInitialized();

    this.db.exec('DELETE FROM documents');
    console.log('[SQLITE-VECTOR] 🧹 Almacenamiento limpiado');
  }

  /**
   * Obtiene estadísticas del almacenamiento
   */
  getStats() {
    if (!this.isInitialized) {
      return {
        totalChunks: 0,
        totalDocuments: 0,
        totalContexts: 0,
        embeddingDimension: 0
      };
    }

    const totalChunks = this.db.prepare('SELECT COUNT(*) as count FROM documents').get().count;
    const totalDocuments = this.db.prepare('SELECT COUNT(DISTINCT document_id) as count FROM documents').get().count;
    const totalContexts = this.db.prepare('SELECT COUNT(DISTINCT context_id) as count FROM documents').get().count;
    
    // Obtener dimensión de embedding de un documento aleatorio
    let embeddingDimension = 0;
    const sampleRow = this.db.prepare('SELECT embedding FROM documents LIMIT 1').get();
    if (sampleRow) {
      try {
        const embedding = JSON.parse(sampleRow.embedding);
        embeddingDimension = embedding.length;
      } catch (error) {
        // Ignorar error
      }
    }

    return {
      totalChunks,
      totalDocuments,
      totalContexts,
      embeddingDimension,
      dbPath: this.dbPath,
      dbSize: this.getDatabaseSize()
    };
  }

  /**
   * Obtiene el tamaño de la base de datos
   */
  getDatabaseSize() {
    try {
      const stats = require('fs').statSync(this.dbPath);
      return stats.size;
    } catch (error) {
      return 0;
    }
  }

  /**
   * Cierra la conexión a la base de datos
   */
  close() {
    if (this.db) {
      this.db.close();
      this.isInitialized = false;
      console.log('[SQLITE-VECTOR] 🔒 Conexión cerrada');
    }
  }

  /**
   * Verifica la integridad de la base de datos
   */
  checkIntegrity() {
    this._ensureInitialized();

    try {
      const result = this.db.prepare('PRAGMA integrity_check').get();
      const isValid = result.integrity_check === 'ok';
      
      return {
        isValid,
        message: result.integrity_check,
        checkedAt: new Date().toISOString()
      };
    } catch (error) {
      return {
        isValid: false,
        message: error.message,
        checkedAt: new Date().toISOString()
      };
    }
  }
}

// Instancia singleton
export const sqliteVectorStore = new SQLiteVectorStore();
