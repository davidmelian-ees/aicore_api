import fs from 'fs/promises';
import path from 'path';

/**
 * Gestor de persistencia para Cloud Foundry
 * Guarda datos esenciales en variables de entorno y archivos de configuración
 */
class PersistenceManager {
  constructor() {
    this.backupPath = './data/backup';
    this.isCloudFoundry = process.env.NODE_ENV === 'production';
  }

  /**
   * Inicializa el sistema de persistencia
   */
  async initialize() {
    try {
      // Crear directorio de backup si no existe
      await fs.mkdir(this.backupPath, { recursive: true });
      
      if (this.isCloudFoundry) {
        console.log('[PERSISTENCE] 🔄 Modo Cloud Foundry - Restaurando datos...');
        await this.restoreFromBackup();
      } else {
        console.log('[PERSISTENCE] 💾 Modo local - Persistencia nativa');
      }
    } catch (error) {
      console.warn('[PERSISTENCE] ⚠️  Error inicializando:', error.message);
    }
  }

  /**
   * Crea backup de datos esenciales
   */
  async createBackup() {
    if (!this.isCloudFoundry) return; // Solo en Cloud Foundry
    
    try {
      const backupData = {
        timestamp: new Date().toISOString(),
        contexts: await this.exportContexts(),
        documents: await this.exportDocumentMetadata(),
        version: '1.0'
      };

      const backupFile = path.join(this.backupPath, 'rag-backup.json');
      await fs.writeFile(backupFile, JSON.stringify(backupData, null, 2));
      
      console.log('[PERSISTENCE] ✅ Backup creado:', backupFile);
      return backupData;
    } catch (error) {
      console.error('[PERSISTENCE] ❌ Error creando backup:', error);
    }
  }

  /**
   * Restaura datos desde backup
   */
  async restoreFromBackup() {
    try {
      const backupFile = path.join(this.backupPath, 'rag-backup.json');
      
      // Verificar si existe backup
      try {
        await fs.access(backupFile);
      } catch {
        console.log('[PERSISTENCE] 📝 No hay backup previo - Iniciando limpio');
        return;
      }

      const backupData = JSON.parse(await fs.readFile(backupFile, 'utf8'));
      
      console.log(`[PERSISTENCE] 🔄 Restaurando backup del ${backupData.timestamp}`);
      
      // Restaurar contextos
      if (backupData.contexts) {
        await this.importContexts(backupData.contexts);
      }
      
      // Restaurar metadatos de documentos
      if (backupData.documents) {
        await this.importDocumentMetadata(backupData.documents);
      }
      
      console.log('[PERSISTENCE] ✅ Datos restaurados exitosamente');
    } catch (error) {
      console.error('[PERSISTENCE] ❌ Error restaurando backup:', error);
    }
  }

  /**
   * Exporta contextos para backup
   */
  async exportContexts() {
    try {
      // Importar dinámicamente para evitar dependencias circulares
      const { contextPersistence } = await import('./contextPersistence.js');
      const contexts = await contextPersistence.loadContexts();
      
      return Array.from(contexts.entries()).map(([id, context]) => ({
        id,
        ...context
      }));
    } catch (error) {
      console.warn('[PERSISTENCE] ⚠️  Error exportando contextos:', error.message);
      return [];
    }
  }

  /**
   * Importa contextos desde backup
   */
  async importContexts(contextsData) {
    try {
      const { contextPersistence } = await import('./contextPersistence.js');
      
      for (const contextData of contextsData) {
        await contextPersistence.saveContext(contextData.id, contextData);
      }
      
      console.log(`[PERSISTENCE] ✅ ${contextsData.length} contextos restaurados`);
    } catch (error) {
      console.warn('[PERSISTENCE] ⚠️  Error importando contextos:', error.message);
    }
  }

  /**
   * Exporta metadatos de documentos
   */
  async exportDocumentMetadata() {
    try {
      // Obtener lista de documentos del vector store
      const { listDocuments } = await import('./ragService.js');
      const documents = await listDocuments();
      
      return documents.map(doc => ({
        id: doc.id,
        filename: doc.filename,
        contextId: doc.contextId,
        uploadedAt: doc.uploadedAt,
        chunkCount: doc.chunkCount
      }));
    } catch (error) {
      console.warn('[PERSISTENCE] ⚠️  Error exportando documentos:', error.message);
      return [];
    }
  }

  /**
   * Importa metadatos de documentos
   */
  async importDocumentMetadata(documentsData) {
    try {
      // Guardar metadatos para referencia
      const metadataFile = path.join(this.backupPath, 'documents-metadata.json');
      await fs.writeFile(metadataFile, JSON.stringify(documentsData, null, 2));
      
      console.log(`[PERSISTENCE] ✅ Metadatos de ${documentsData.length} documentos guardados`);
    } catch (error) {
      console.warn('[PERSISTENCE] ⚠️  Error importando metadatos:', error.message);
    }
  }

  /**
   * Programa backups automáticos
   */
  scheduleBackups() {
    if (!this.isCloudFoundry) return;
    
    // Backup cada 30 minutos
    setInterval(async () => {
      await this.createBackup();
    }, 30 * 60 * 1000);
    
    console.log('[PERSISTENCE] ⏰ Backups automáticos programados cada 30 minutos');
  }

  /**
   * Backup manual
   */
  async manualBackup() {
    console.log('[PERSISTENCE] 🔄 Iniciando backup manual...');
    return await this.createBackup();
  }
}

export const persistenceManager = new PersistenceManager();
