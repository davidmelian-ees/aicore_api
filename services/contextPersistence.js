import fs from 'fs/promises';
import path from 'path';

/**
 * Servicio de persistencia para contextos RAG
 * Almacena contextos en archivo JSON para persistencia entre reinicios
 */
class ContextPersistence {
  constructor() {
    this.dataDir = './data';
    this.contextFile = path.join(this.dataDir, 'contexts.json');
    this.initialized = false;
  }

  /**
   * Inicializa el servicio de persistencia
   */
  async initialize() {
    try {
      // Crear directorio de datos si no existe
      await fs.mkdir(this.dataDir, { recursive: true });
      
      // Verificar si existe el archivo de contextos
      try {
        await fs.access(this.contextFile);
        console.log('[CONTEXT-PERSIST] ✅ Archivo de contextos encontrado');
      } catch (error) {
        // Si no existe, crear archivo inicial con contexto por defecto
        const defaultContexts = {
          'default': {
            id: 'default',
            name: 'Contexto Principal',
            description: 'Contexto por defecto del sistema',
            createdAt: new Date().toISOString(),
            documentCount: 0
          }
        };
        
        // Guardar directamente sin llamar a saveContexts para evitar bucle infinito
        const data = JSON.stringify(defaultContexts, null, 2);
        await fs.writeFile(this.contextFile, data, 'utf8');
        console.log('[CONTEXT-PERSIST] ✅ Archivo de contextos inicializado');
      }
      
      this.initialized = true;
      console.log('[CONTEXT-PERSIST] 🚀 Servicio de persistencia inicializado');
      
    } catch (error) {
      console.error('[CONTEXT-PERSIST] ❌ Error inicializando persistencia:', error);
      throw error;
    }
  }

  /**
   * Carga todos los contextos desde el archivo
   * @returns {Promise<Map>} - Map de contextos
   */
  async loadContexts() {
    try {
      if (!this.initialized) {
        await this.initialize();
      }

      const data = await fs.readFile(this.contextFile, 'utf8');
      const contextsObj = JSON.parse(data);
      
      // Convertir objeto a Map
      const contextsMap = new Map();
      for (const [id, context] of Object.entries(contextsObj)) {
        contextsMap.set(id, context);
      }
      
      console.log(`[CONTEXT-PERSIST] ✅ Cargados ${contextsMap.size} contextos`);
      return contextsMap;
      
    } catch (error) {
      console.error('[CONTEXT-PERSIST] ❌ Error cargando contextos:', error);
      // Retornar Map vacío con contexto por defecto
      const defaultMap = new Map();
      defaultMap.set('default', {
        id: 'default',
        name: 'Contexto Principal',
        description: 'Contexto por defecto del sistema',
        createdAt: new Date().toISOString(),
        documentCount: 0
      });
      return defaultMap;
    }
  }

  /**
   * Guarda todos los contextos en el archivo
   * @param {Map|Object} contexts - Contextos a guardar
   */
  async saveContexts(contexts) {
    try {
      if (!this.initialized) {
        await this.initialize();
      }

      // Convertir Map a objeto si es necesario
      let contextsObj;
      if (contexts instanceof Map) {
        contextsObj = Object.fromEntries(contexts);
      } else {
        contextsObj = contexts;
      }
      
      // Guardar con formato legible
      const data = JSON.stringify(contextsObj, null, 2);
      await fs.writeFile(this.contextFile, data, 'utf8');
      
      console.log(`[CONTEXT-PERSIST] ✅ Guardados ${Object.keys(contextsObj).length} contextos`);
      
    } catch (error) {
      console.error('[CONTEXT-PERSIST] ❌ Error guardando contextos:', error);
      throw error;
    }
  }

  /**
   * Guarda un contexto específico
   * @param {string} contextId - ID del contexto
   * @param {Object} context - Datos del contexto
   */
  async saveContext(contextId, context) {
    try {
      const contexts = await this.loadContexts();
      contexts.set(contextId, context);
      await this.saveContexts(contexts);
      
    } catch (error) {
      console.error(`[CONTEXT-PERSIST] ❌ Error guardando contexto ${contextId}:`, error);
      throw error;
    }
  }

  /**
   * Elimina un contexto específico
   * @param {string} contextId - ID del contexto a eliminar
   */
  async deleteContext(contextId) {
    try {
      const contexts = await this.loadContexts();
      const deleted = contexts.delete(contextId);
      
      if (deleted) {
        await this.saveContexts(contexts);
        console.log(`[CONTEXT-PERSIST] ✅ Contexto eliminado: ${contextId}`);
      }
      
      return deleted;
      
    } catch (error) {
      console.error(`[CONTEXT-PERSIST] ❌ Error eliminando contexto ${contextId}:`, error);
      throw error;
    }
  }

  /**
   * Actualiza el contador de documentos de un contexto
   * @param {string} contextId - ID del contexto
   * @param {number} documentCount - Nuevo contador de documentos
   */
  async updateDocumentCount(contextId, documentCount) {
    try {
      const contexts = await this.loadContexts();
      const context = contexts.get(contextId);
      
      if (context) {
        context.documentCount = documentCount;
        context.updatedAt = new Date().toISOString();
        await this.saveContext(contextId, context);
      }
      
    } catch (error) {
      console.error(`[CONTEXT-PERSIST] ❌ Error actualizando contador de documentos:`, error);
    }
  }

  /**
   * Obtiene estadísticas de persistencia
   * @returns {Promise<Object>} - Estadísticas
   */
  async getStats() {
    try {
      const contexts = await this.loadContexts();
      const stats = await fs.stat(this.contextFile);
      
      return {
        totalContexts: contexts.size,
        fileSize: stats.size,
        lastModified: stats.mtime.toISOString(),
        filePath: this.contextFile
      };
      
    } catch (error) {
      console.error('[CONTEXT-PERSIST] ❌ Error obteniendo estadísticas:', error);
      return {
        totalContexts: 0,
        fileSize: 0,
        lastModified: null,
        filePath: this.contextFile,
        error: error.message
      };
    }
  }
}

// Instancia singleton
export const contextPersistence = new ContextPersistence();
