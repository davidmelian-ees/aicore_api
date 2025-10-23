#!/usr/bin/env node

/**
 * Script para hacer backup automático de la base de datos RAG
 * Uso: node scripts/backup-database.js [--download-url URL]
 */

import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

class DatabaseBackupManager {
  constructor() {
    this.backupDir = path.join(__dirname, '../backups');
    this.baseUrl = process.argv.includes('--download-url') 
      ? process.argv[process.argv.indexOf('--download-url') + 1]
      : 'https://ai_core_api.cfapps.eu10-005.hana.ondemand.com';
  }

  /**
   * Crea directorio de backups si no existe
   */
  async ensureBackupDir() {
    try {
      await fs.mkdir(this.backupDir, { recursive: true });
      console.log(`📁 Directorio de backups: ${this.backupDir}`);
    } catch (error) {
      console.error('❌ Error creando directorio de backups:', error);
      throw error;
    }
  }

  /**
   * Descarga la base de datos desde Cloud Foundry
   */
  async downloadDatabase() {
    try {
      console.log('🔄 Descargando base de datos desde Cloud Foundry...');
      
      const response = await fetch(`${this.baseUrl}/api/rag/download-db`);
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      // Obtener nombre del archivo desde headers
      const contentDisposition = response.headers.get('content-disposition');
      let filename = `rag_vectors_backup_${new Date().toISOString().replace(/[:.]/g, '-')}.db`;
      
      if (contentDisposition) {
        const match = contentDisposition.match(/filename="(.+)"/);
        if (match) filename = match[1];
      }

      const backupPath = path.join(this.backupDir, filename);
      const buffer = await response.arrayBuffer();
      
      await fs.writeFile(backupPath, Buffer.from(buffer));
      
      const stats = await fs.stat(backupPath);
      console.log(`✅ Base de datos descargada: ${filename}`);
      console.log(`📊 Tamaño: ${(stats.size / (1024 * 1024)).toFixed(2)} MB`);
      console.log(`📍 Ubicación: ${backupPath}`);
      
      return { filename, path: backupPath, size: stats.size };
    } catch (error) {
      console.error('❌ Error descargando base de datos:', error);
      throw error;
    }
  }

  /**
   * Obtiene información de la base de datos remota
   */
  async getDatabaseInfo() {
    try {
      console.log('📊 Obteniendo información de base de datos...');
      
      const response = await fetch(`${this.baseUrl}/api/rag/db-info`);
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      const info = await response.json();
      
      if (info.success) {
        console.log('📈 Información de la base de datos:');
        console.log(`   Tamaño: ${info.database.size_mb} MB`);
        console.log(`   Documentos: ${info.content.total_documents}`);
        console.log(`   Chunks: ${info.content.total_chunks}`);
        console.log(`   Modificada: ${new Date(info.database.modified).toLocaleString()}`);
        
        return info;
      } else {
        throw new Error(info.error || 'Error obteniendo información');
      }
    } catch (error) {
      console.error('❌ Error obteniendo información:', error);
      throw error;
    }
  }

  /**
   * Lista backups locales
   */
  async listLocalBackups() {
    try {
      const files = await fs.readdir(this.backupDir);
      const backups = [];

      for (const file of files) {
        if (file.endsWith('.db')) {
          const filePath = path.join(this.backupDir, file);
          const stats = await fs.stat(filePath);
          backups.push({
            filename: file,
            path: filePath,
            size: stats.size,
            created: stats.birthtime,
            modified: stats.mtime
          });
        }
      }

      backups.sort((a, b) => b.created - a.created);

      console.log(`📋 Backups locales (${backups.length}):`);
      backups.forEach((backup, index) => {
        console.log(`   ${index + 1}. ${backup.filename}`);
        console.log(`      Tamaño: ${(backup.size / (1024 * 1024)).toFixed(2)} MB`);
        console.log(`      Creado: ${backup.created.toLocaleString()}`);
        console.log('');
      });

      return backups;
    } catch (error) {
      console.log('📝 No hay backups locales o directorio no existe');
      return [];
    }
  }

  /**
   * Limpia backups antiguos (mantiene solo los últimos N)
   */
  async cleanOldBackups(keepCount = 5) {
    try {
      const backups = await this.listLocalBackups();
      
      if (backups.length <= keepCount) {
        console.log(`✅ Solo ${backups.length} backups, no hay nada que limpiar`);
        return;
      }

      const toDelete = backups.slice(keepCount);
      
      for (const backup of toDelete) {
        await fs.unlink(backup.path);
        console.log(`🗑️  Eliminado: ${backup.filename}`);
      }

      console.log(`✅ Limpieza completada: ${toDelete.length} backups eliminados`);
    } catch (error) {
      console.error('❌ Error limpiando backups:', error);
    }
  }

  /**
   * Ejecuta backup completo
   */
  async runBackup(options = {}) {
    try {
      await this.ensureBackupDir();
      
      // Obtener información
      if (!options.skipInfo) {
        await this.getDatabaseInfo();
        console.log('');
      }

      // Descargar backup
      const backup = await this.downloadDatabase();
      console.log('');

      // Limpiar backups antiguos
      if (!options.skipCleanup) {
        await this.cleanOldBackups(options.keepCount || 5);
      }

      return backup;
    } catch (error) {
      console.error('❌ Error en backup:', error);
      process.exit(1);
    }
  }
}

// Ejecutar si es llamado directamente
if (import.meta.url === `file://${process.argv[1]}`) {
  const manager = new DatabaseBackupManager();
  
  const command = process.argv[2];
  
  switch (command) {
    case 'download':
      manager.runBackup();
      break;
      
    case 'info':
      manager.getDatabaseInfo();
      break;
      
    case 'list':
      manager.ensureBackupDir().then(() => manager.listLocalBackups());
      break;
      
    case 'clean':
      const keepCount = parseInt(process.argv[3]) || 5;
      manager.ensureBackupDir().then(() => manager.cleanOldBackups(keepCount));
      break;
      
    default:
      console.log('🔧 Uso del script de backup:');
      console.log('');
      console.log('  node scripts/backup-database.js download    # Descargar backup');
      console.log('  node scripts/backup-database.js info        # Ver información de BD');
      console.log('  node scripts/backup-database.js list        # Listar backups locales');
      console.log('  node scripts/backup-database.js clean [N]   # Limpiar backups (mantener N)');
      console.log('');
      console.log('Opciones:');
      console.log('  --download-url URL    # URL personalizada del servidor');
      console.log('');
      console.log('Ejemplos:');
      console.log('  node scripts/backup-database.js download');
      console.log('  node scripts/backup-database.js clean 3');
      console.log('  node scripts/backup-database.js download --download-url http://localhost:4000');
      break;
  }
}

export { DatabaseBackupManager };
