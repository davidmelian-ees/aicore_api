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
   * Cierra la conexión a la base de datos
   */
  close() {
    if (this.db) {
      try {
        console.log('[SQLITE-VECTOR] 🔒 Cerrando conexión a base de datos...');
        this.db.close();
        this.db = null;
        this.isInitialized = false;
        console.log('[SQLITE-VECTOR] ✅ Conexión cerrada');
      } catch (error) {
        console.error('[SQLITE-VECTOR] ⚠️ Error cerrando conexión:', error.message);
      }
    }
  }

  /**
   * Reinicializa la base de datos (útil después de restaurar un backup)
   */
  async reinitialize() {
    console.log('[SQLITE-VECTOR] 🔄 Reinicializando base de datos...');
    this.close();
    await this.initialize();
    console.log('[SQLITE-VECTOR] ✅ Base de datos reinicializada');
  }

  /**
   * Obtiene todos los contextos únicos de los documentos
   * @returns {Array} - Lista de contextos con estadísticas
   */
  getAllContexts() {
    this._ensureInitialized();
    
    try {
      const query = `
        SELECT 
          context_id,
          COUNT(DISTINCT document_id) as document_count,
          COUNT(*) as chunk_count,
          MIN(created_at) as created_at
        FROM documents
        GROUP BY context_id
        ORDER BY created_at DESC
      `;
      
      const contexts = this.db.prepare(query).all();
      
      return contexts.map(ctx => ({
        id: ctx.context_id,
        name: ctx.context_id === 'default' ? 'Contexto Principal' : ctx.context_id,
        description: `Contexto con ${ctx.document_count} documentos`,
        createdAt: ctx.created_at,
        documentCount: ctx.document_count,
        chunkCount: ctx.chunk_count
      }));
      
    } catch (error) {
      console.error('[SQLITE-VECTOR] Error obteniendo contextos:', error);
      return [];
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
      
      // Validar que el JSON se puede parsear correctamente (test de ida y vuelta)
      try {
        const testParse = JSON.parse(embeddingJson);
        if (!validateEmbedding(testParse)) {
          throw new Error('Embedding no se puede serializar/deserializar correctamente');
        }
      } catch (parseError) {
        console.error(`[SQLITE-VECTOR] ❌ Error validando serialización de embedding para ${document.id}:`, parseError);
        throw new Error(`Embedding no se puede serializar correctamente: ${parseError.message}`);
      }

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

      // Verificar que se guardó correctamente
      const verifyStmt = this.db.prepare('SELECT embedding FROM documents WHERE id = ?');
      const verifyRow = verifyStmt.get(document.id);
      
      if (!verifyRow || !verifyRow.embedding) {
        throw new Error('Documento no se guardó correctamente en la base de datos');
      }
      
      // Verificar que se puede leer correctamente
      try {
        const verifyEmbedding = JSON.parse(verifyRow.embedding);
        if (!validateEmbedding(verifyEmbedding)) {
          throw new Error('Embedding guardado no es válido al leerlo');
        }
      } catch (verifyError) {
        console.error(`[SQLITE-VECTOR] ❌ Error verificando embedding guardado para ${document.id}:`, verifyError);
        throw new Error(`Embedding guardado no se puede leer: ${verifyError.message}`);
      }

      console.log(`[SQLITE-VECTOR] ✅ Documento agregado y verificado: ${document.id}`);

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
          
          // Validar que el embedding parseado sea válido
          if (!validateEmbedding(embedding)) {
            console.warn(`[SQLITE-VECTOR] ⚠️ Embedding inválido para ${row.id}: no es un array válido de números`);
            continue;
          }
          
          // Validar dimensiones coincidentes
          if (embedding.length !== queryEmbedding.length) {
            console.warn(`[SQLITE-VECTOR] ⚠️ Dimensión incompatible para ${row.id}: query=${queryEmbedding.length}, stored=${embedding.length}`);
            continue;
          }
          
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
          console.warn(`[SQLITE-VECTOR] ⚠️ Error procesando embedding para ${row.id}:`, error.message);
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
   * Obtiene todos los chunks de un documento específico
   * @param {string} documentId - ID del documento
   * @returns {Array<Object>} - Array de chunks del documento
   */
  getDocumentChunks(documentId) {
    this._ensureInitialized();

    const stmt = this.db.prepare(`
      SELECT id, content, metadata, chunk_index
      FROM documents 
      WHERE document_id = ?
      ORDER BY chunk_index ASC
    `);

    const rows = stmt.all(documentId);
    
    return rows.map(row => ({
      id: row.id,
      content: row.content,
      metadata: JSON.parse(row.metadata)
    }));
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

  /**
   * Diagnostica embeddings corruptos en la base de datos
   * @param {string} contextId - ID del contexto a diagnosticar (opcional)
   * @returns {Object} - Reporte de diagnóstico
   */
  diagnoseEmbeddings(contextId = null) {
    this._ensureInitialized();

    try {
      console.log('[SQLITE-VECTOR] 🔍 Iniciando diagnóstico de embeddings...');
      
      let sql = 'SELECT id, embedding, LENGTH(embedding) as embedding_length FROM documents';
      const params = [];
      
      if (contextId) {
        sql += ' WHERE context_id = ?';
        params.push(contextId);
      }
      
      const stmt = this.db.prepare(sql);
      const rows = stmt.all(...params);
      
      const report = {
        totalDocuments: rows.length,
        validEmbeddings: 0,
        invalidEmbeddings: 0,
        corruptedIds: [],
        embeddingSizes: {
          min: Infinity,
          max: 0,
          avg: 0
        },
        dimensionGroups: {} // Agrupar por dimensión
      };
      
      let totalSize = 0;
      
      for (const row of rows) {
        try {
          // Intentar parsear el embedding
          const embedding = JSON.parse(row.embedding);
          
          // Validar el embedding
          if (validateEmbedding(embedding)) {
            report.validEmbeddings++;
            const size = embedding.length;
            totalSize += size;
            
            if (size < report.embeddingSizes.min) report.embeddingSizes.min = size;
            if (size > report.embeddingSizes.max) report.embeddingSizes.max = size;
            
            // Agrupar por dimensión
            if (!report.dimensionGroups[size]) {
              report.dimensionGroups[size] = {
                count: 0,
                ids: []
              };
            }
            report.dimensionGroups[size].count++;
            if (report.dimensionGroups[size].ids.length < 5) {
              report.dimensionGroups[size].ids.push(row.id);
            }
          } else {
            report.invalidEmbeddings++;
            report.corruptedIds.push({
              id: row.id,
              reason: 'Embedding no es un array válido de números',
              embeddingLength: row.embedding_length
            });
          }
        } catch (error) {
          report.invalidEmbeddings++;
          report.corruptedIds.push({
            id: row.id,
            reason: error.message,
            embeddingLength: row.embedding_length
          });
        }
      }
      
      if (report.validEmbeddings > 0) {
        report.embeddingSizes.avg = Math.round(totalSize / report.validEmbeddings);
      }
      
      console.log('[SQLITE-VECTOR] 📊 Diagnóstico completado:');
      console.log(`  - Total documentos: ${report.totalDocuments}`);
      console.log(`  - Embeddings válidos: ${report.validEmbeddings}`);
      console.log(`  - Embeddings inválidos: ${report.invalidEmbeddings}`);
      console.log(`  - Dimensión promedio: ${report.embeddingSizes.avg}`);
      console.log(`  - Rango dimensiones: ${report.embeddingSizes.min} - ${report.embeddingSizes.max}`);
      
      // Mostrar grupos por dimensión
      console.log('  - Grupos por dimensión:');
      Object.entries(report.dimensionGroups).forEach(([dim, data]) => {
        const modelType = dim === '384' ? '(fallback local)' : dim === '1536' ? '(SAP AI Core)' : '';
        console.log(`    * ${dim}D: ${data.count} chunks ${modelType}`);
      });
      
      if (report.corruptedIds.length > 0) {
        console.log(`  - IDs corruptos: ${report.corruptedIds.slice(0, 5).map(c => c.id).join(', ')}${report.corruptedIds.length > 5 ? '...' : ''}`);
      }
      
      return report;
      
    } catch (error) {
      console.error('[SQLITE-VECTOR] ❌ Error en diagnóstico:', error);
      throw error;
    }
  }

  /**
   * Repara embeddings corruptos regenerándolos
   * @param {Array<string>} documentIds - IDs de documentos a reparar (opcional, si no se proporciona repara todos)
   * @returns {Object} - Resultado de la reparación
   */
  async repairEmbeddings(documentIds = null) {
    this._ensureInitialized();

    try {
      console.log('[SQLITE-VECTOR] 🔧 Iniciando reparación de embeddings...');
      
      // Si no se proporcionan IDs, diagnosticar primero para encontrar los corruptos
      if (!documentIds) {
        const diagnosis = this.diagnoseEmbeddings();
        documentIds = diagnosis.corruptedIds.map(c => c.id);
      }
      
      if (documentIds.length === 0) {
        console.log('[SQLITE-VECTOR] ✅ No hay embeddings que reparar');
        return { repaired: 0, failed: 0 };
      }
      
      console.log(`[SQLITE-VECTOR] 🔧 Reparando ${documentIds.length} embeddings...`);
      
      const result = { repaired: 0, failed: 0, errors: [] };
      
      for (const docId of documentIds) {
        try {
          // Obtener el documento
          const stmt = this.db.prepare('SELECT content FROM documents WHERE id = ?');
          const row = stmt.get(docId);
          
          if (!row) {
            result.failed++;
            result.errors.push({ id: docId, error: 'Documento no encontrado' });
            continue;
          }
          
          // Regenerar embedding
          console.log(`[SQLITE-VECTOR] 🔄 Regenerando embedding para ${docId}...`);
          const newEmbedding = await generateEmbedding(row.content);
          
          // Validar el nuevo embedding
          if (!validateEmbedding(newEmbedding)) {
            result.failed++;
            result.errors.push({ id: docId, error: 'Nuevo embedding inválido' });
            continue;
          }
          
          // Actualizar en la base de datos
          const updateStmt = this.db.prepare('UPDATE documents SET embedding = ? WHERE id = ?');
          updateStmt.run(JSON.stringify(newEmbedding), docId);
          
          result.repaired++;
          console.log(`[SQLITE-VECTOR] ✅ Embedding reparado: ${docId}`);
          
        } catch (error) {
          result.failed++;
          result.errors.push({ id: docId, error: error.message });
          console.error(`[SQLITE-VECTOR] ❌ Error reparando ${docId}:`, error.message);
        }
      }
      
      console.log(`[SQLITE-VECTOR] 🏁 Reparación completada: ${result.repaired} reparados, ${result.failed} fallidos`);
      
      return result;
      
    } catch (error) {
      console.error('[SQLITE-VECTOR] ❌ Error en reparación:', error);
      throw error;
    }
  }

  /**
   * Normaliza todos los embeddings a la misma dimensión (SAP AI Core)
   * Regenera embeddings que tengan dimensión diferente a la esperada
   * @param {number} targetDimension - Dimensión objetivo (por defecto 1536 para SAP AI Core)
   * @returns {Object} - Resultado de la normalización
   */
  async normalizeEmbeddings(targetDimension = 1536) {
    this._ensureInitialized();

    try {
      console.log(`[SQLITE-VECTOR] 🔄 Normalizando embeddings a dimensión ${targetDimension}...`);
      
      // Diagnosticar para encontrar embeddings con dimensión incorrecta
      const diagnosis = this.diagnoseEmbeddings();
      
      // Encontrar todos los embeddings que no tienen la dimensión objetivo
      const toNormalize = [];
      for (const [dim, data] of Object.entries(diagnosis.dimensionGroups)) {
        if (parseInt(dim) !== targetDimension) {
          console.log(`[SQLITE-VECTOR] 📊 Encontrados ${data.count} embeddings con dimensión ${dim} (necesitan normalización)`);
          
          // Obtener todos los IDs con esta dimensión
          const stmt = this.db.prepare('SELECT id FROM documents');
          const rows = stmt.all();
          
          for (const row of rows) {
            try {
              const embStmt = this.db.prepare('SELECT embedding FROM documents WHERE id = ?');
              const embRow = embStmt.get(row.id);
              const embedding = JSON.parse(embRow.embedding);
              
              if (embedding.length === parseInt(dim)) {
                toNormalize.push(row.id);
              }
            } catch (error) {
              console.warn(`[SQLITE-VECTOR] ⚠️ Error verificando ${row.id}:`, error.message);
            }
          }
        }
      }
      
      if (toNormalize.length === 0) {
        console.log(`[SQLITE-VECTOR] ✅ Todos los embeddings ya tienen dimensión ${targetDimension}`);
        return { 
          normalized: 0, 
          failed: 0, 
          message: `Todos los embeddings ya tienen la dimensión correcta (${targetDimension}D)` 
        };
      }
      
      console.log(`[SQLITE-VECTOR] 🔧 Normalizando ${toNormalize.length} embeddings...`);
      
      const result = { normalized: 0, failed: 0, errors: [] };
      
      for (const docId of toNormalize) {
        try {
          // Obtener el documento
          const stmt = this.db.prepare('SELECT content, embedding FROM documents WHERE id = ?');
          const row = stmt.get(docId);
          
          if (!row) {
            result.failed++;
            result.errors.push({ id: docId, error: 'Documento no encontrado' });
            continue;
          }
          
          const oldEmbedding = JSON.parse(row.embedding);
          console.log(`[SQLITE-VECTOR] 🔄 Normalizando ${docId} (${oldEmbedding.length}D → ${targetDimension}D)...`);
          
          // Regenerar embedding con SAP AI Core
          const newEmbedding = await generateEmbedding(row.content);
          
          // Validar el nuevo embedding
          if (!validateEmbedding(newEmbedding)) {
            result.failed++;
            result.errors.push({ id: docId, error: 'Nuevo embedding inválido' });
            continue;
          }
          
          if (newEmbedding.length !== targetDimension) {
            result.failed++;
            result.errors.push({ 
              id: docId, 
              error: `Embedding generado tiene dimensión ${newEmbedding.length}, esperada ${targetDimension}` 
            });
            continue;
          }
          
          // Actualizar en la base de datos
          const updateStmt = this.db.prepare('UPDATE documents SET embedding = ? WHERE id = ?');
          updateStmt.run(JSON.stringify(newEmbedding), docId);
          
          result.normalized++;
          
          if (result.normalized % 10 === 0) {
            console.log(`[SQLITE-VECTOR] 📊 Progreso: ${result.normalized}/${toNormalize.length} normalizados`);
          }
          
        } catch (error) {
          result.failed++;
          result.errors.push({ id: docId, error: error.message });
          console.error(`[SQLITE-VECTOR] ❌ Error normalizando ${docId}:`, error.message);
        }
      }
      
      console.log(`[SQLITE-VECTOR] 🏁 Normalización completada: ${result.normalized} normalizados, ${result.failed} fallidos`);
      
      return result;
      
    } catch (error) {
      console.error('[SQLITE-VECTOR] ❌ Error en normalización:', error);
      throw error;
    }
  }
}

// Instancia singleton
export const sqliteVectorStore = new SQLiteVectorStore();
