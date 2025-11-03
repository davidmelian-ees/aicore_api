import fs from 'fs/promises';
import fsSync from 'fs';
import path from 'path';
import zlib from 'zlib';
import { promisify } from 'util';

const gzip = promisify(zlib.gzip);

/**
 * Servicio de backups para la base de datos SQLite
 */
class BackupService {
  constructor() {
    this.dbPath = './data/rag_vectors.db';
    this.backupDir = './data/backup';
    this.maxBackups = 10; // Mantener máximo 10 backups
  }

  /**
   * Inicializa el directorio de backups
   */
  async initialize() {
    try {
      await fs.mkdir(this.backupDir, { recursive: true });
      console.log('[BACKUP] ✅ Directorio de backups inicializado');
    } catch (error) {
      console.error('[BACKUP] ❌ Error inicializando directorio:', error);
    }
  }

  /**
   * Crea un backup de la base de datos
   * @param {boolean} compress - Si se debe comprimir el backup
   * @returns {Object} - Información del backup creado
   */
  async createBackup(compress = true) {
    try {
      await this.initialize();

      // Verificar que la BD existe
      try {
        await fs.access(this.dbPath);
      } catch {
        throw new Error('Base de datos no encontrada');
      }

      const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
      const stats = await fs.stat(this.dbPath);
      
      let backupPath;
      let backupSize;

      if (compress) {
        // Crear backup comprimido
        backupPath = path.join(this.backupDir, `backup_${timestamp}.db.gz`);
        
        const dbBuffer = await fs.readFile(this.dbPath);
        const compressed = await gzip(dbBuffer, { level: 9 });
        await fs.writeFile(backupPath, compressed);
        
        const compressedStats = await fs.stat(backupPath);
        backupSize = compressedStats.size;
        
        const compressionRatio = ((1 - backupSize / stats.size) * 100).toFixed(2);
        console.log(`[BACKUP] 📦 Compresión: ${compressionRatio}% (${stats.size} → ${backupSize} bytes)`);
        
      } else {
        // Crear backup sin comprimir
        backupPath = path.join(this.backupDir, `backup_${timestamp}.db`);
        await fs.copyFile(this.dbPath, backupPath);
        backupSize = stats.size;
      }

      console.log(`[BACKUP] ✅ Backup creado: ${path.basename(backupPath)} (${(backupSize / 1024 / 1024).toFixed(2)} MB)`);

      // Limpiar backups antiguos
      await this.cleanOldBackups();

      return {
        success: true,
        filename: path.basename(backupPath),
        path: backupPath,
        size: backupSize,
        originalSize: stats.size,
        compressed: compress,
        timestamp: new Date().toISOString()
      };

    } catch (error) {
      console.error('[BACKUP] ❌ Error creando backup:', error);
      throw error;
    }
  }

  /**
   * Lista todos los backups disponibles
   * @returns {Array} - Lista de backups
   */
  async listBackups() {
    try {
      await this.initialize();

      const files = await fs.readdir(this.backupDir);
      const backups = [];

      for (const file of files) {
        if (file.startsWith('backup_') && (file.endsWith('.db') || file.endsWith('.db.gz'))) {
          const filePath = path.join(this.backupDir, file);
          const stats = await fs.stat(filePath);
          
          backups.push({
            filename: file,
            path: filePath,
            size: stats.size,
            sizeFormatted: this.formatSize(stats.size),
            compressed: file.endsWith('.gz'),
            created: stats.birthtime,
            modified: stats.mtime
          });
        }
      }

      // Ordenar por fecha de creación (más reciente primero)
      backups.sort((a, b) => b.created - a.created);

      return backups;

    } catch (error) {
      console.error('[BACKUP] ❌ Error listando backups:', error);
      return [];
    }
  }

  /**
   * Elimina backups antiguos manteniendo solo los más recientes
   */
  async cleanOldBackups() {
    try {
      const backups = await this.listBackups();
      
      if (backups.length > this.maxBackups) {
        const toDelete = backups.slice(this.maxBackups);
        
        for (const backup of toDelete) {
          await fs.unlink(backup.path);
          console.log(`[BACKUP] 🗑️ Backup antiguo eliminado: ${backup.filename}`);
        }
        
        console.log(`[BACKUP] 🧹 ${toDelete.length} backups antiguos eliminados`);
      }

    } catch (error) {
      console.error('[BACKUP] ❌ Error limpiando backups antiguos:', error);
    }
  }

  /**
   * Elimina un backup específico
   * @param {string} filename - Nombre del archivo de backup
   */
  async deleteBackup(filename) {
    try {
      const backupPath = path.join(this.backupDir, filename);
      
      // Verificar que el archivo existe y está en el directorio de backups
      const stats = await fs.stat(backupPath);
      if (!stats.isFile()) {
        throw new Error('No es un archivo válido');
      }

      await fs.unlink(backupPath);
      console.log(`[BACKUP] 🗑️ Backup eliminado: ${filename}`);

      return {
        success: true,
        filename,
        message: 'Backup eliminado correctamente'
      };

    } catch (error) {
      console.error('[BACKUP] ❌ Error eliminando backup:', error);
      throw error;
    }
  }

  /**
   * Restaura un backup
   * @param {string} filename - Nombre del archivo de backup
   */
  async restoreBackup(filename) {
    try {
      const backupPath = path.join(this.backupDir, filename);
      
      // Verificar que el backup existe
      await fs.access(backupPath);

      // Crear backup de la BD actual antes de restaurar
      console.log('[BACKUP] 📦 Creando backup de seguridad antes de restaurar...');
      await this.createBackup(true);

      const isCompressed = filename.endsWith('.gz');
      
      if (isCompressed) {
        // Descomprimir y restaurar
        const compressed = await fs.readFile(backupPath);
        const gunzip = promisify(zlib.gunzip);
        const decompressed = await gunzip(compressed);
        await fs.writeFile(this.dbPath, decompressed);
      } else {
        // Copiar directamente
        await fs.copyFile(backupPath, this.dbPath);
      }

      console.log(`[BACKUP] ✅ Base de datos restaurada desde: ${filename}`);

      return {
        success: true,
        filename,
        message: 'Base de datos restaurada correctamente',
        timestamp: new Date().toISOString()
      };

    } catch (error) {
      console.error('[BACKUP] ❌ Error restaurando backup:', error);
      throw error;
    }
  }

  /**
   * Obtiene información sobre el espacio usado por backups
   */
  async getBackupStats() {
    try {
      const backups = await this.listBackups();
      
      const totalSize = backups.reduce((sum, backup) => sum + backup.size, 0);
      const compressedCount = backups.filter(b => b.compressed).length;
      
      return {
        totalBackups: backups.length,
        compressedBackups: compressedCount,
        uncompressedBackups: backups.length - compressedCount,
        totalSize,
        totalSizeFormatted: this.formatSize(totalSize),
        maxBackups: this.maxBackups,
        oldestBackup: backups.length > 0 ? backups[backups.length - 1].created : null,
        newestBackup: backups.length > 0 ? backups[0].created : null
      };

    } catch (error) {
      console.error('[BACKUP] ❌ Error obteniendo estadísticas:', error);
      return null;
    }
  }

  /**
   * Formatea el tamaño en bytes a formato legible
   * @param {number} bytes - Tamaño en bytes
   * @returns {string} - Tamaño formateado
   */
  formatSize(bytes) {
    if (bytes === 0) return '0 Bytes';
    const k = 1024;
    const sizes = ['Bytes', 'KB', 'MB', 'GB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
  }

  /**
   * Programa backups automáticos
   * @param {number} intervalHours - Intervalo en horas entre backups
   */
  scheduleAutoBackup(intervalHours = 24) {
    const intervalMs = intervalHours * 60 * 60 * 1000;
    
    console.log(`[BACKUP] ⏰ Backups automáticos programados cada ${intervalHours} horas`);
    
    // Crear backup inicial
    this.createBackup(true).catch(error => {
      console.error('[BACKUP] ❌ Error en backup automático inicial:', error);
    });

    // Programar backups periódicos
    setInterval(() => {
      console.log('[BACKUP] ⏰ Ejecutando backup automático programado...');
      this.createBackup(true).catch(error => {
        console.error('[BACKUP] ❌ Error en backup automático:', error);
      });
    }, intervalMs);
  }
}

// Exportar instancia singleton
export const backupService = new BackupService();
