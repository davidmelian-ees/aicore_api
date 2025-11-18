import fs from 'fs/promises';
import path from 'path';

class LoggerService {
  constructor() {
    this.logFile = path.join(process.cwd(), 'logs', 'app.log');
    this.maxLogSize = 5 * 1024 * 1024; // 5MB
    this.initializeLogFile();
  }

  async initializeLogFile() {
    try {
      const logDir = path.dirname(this.logFile);
      await fs.mkdir(logDir, { recursive: true });
      
      // Verificar si el archivo existe y su tamaño
      try {
        const stats = await fs.stat(this.logFile);
        if (stats.size > this.maxLogSize) {
          // Rotar el log si es muy grande
          await this.rotateLog();
        }
      } catch (error) {
        // El archivo no existe, se creará automáticamente
      }
    } catch (error) {
      console.error('[LOGGER] ❌ Error inicializando archivo de log:', error.message);
    }
  }

  async rotateLog() {
    try {
      const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
      const backupFile = this.logFile.replace('.log', `_${timestamp}.log`);
      await fs.rename(this.logFile, backupFile);
      console.log(`[LOGGER] 🔄 Log rotado a: ${backupFile}`);
    } catch (error) {
      console.error('[LOGGER] ❌ Error rotando log:', error.message);
    }
  }

  formatLogEntry(level, module, message, data = null) {
    const timestamp = new Date().toISOString();
    let entry = `[${timestamp}] [${level}] [${module}] ${message}`;
    
    if (data) {
      entry += `\n${JSON.stringify(data, null, 2)}`;
    }
    
    return entry;
  }

  async writeLog(level, module, message, data = null) {
    try {
      const entry = this.formatLogEntry(level, module, message, data);
      
      // Escribir en consola (para Cloud Foundry logs)
      console.log(entry);
      
      // Escribir en archivo
      await fs.appendFile(this.logFile, entry + '\n\n');
    } catch (error) {
      console.error('[LOGGER] ❌ Error escribiendo log:', error.message);
    }
  }

  info(module, message, data = null) {
    return this.writeLog('INFO', module, message, data);
  }

  error(module, message, data = null) {
    return this.writeLog('ERROR', module, message, data);
  }

  warn(module, message, data = null) {
    return this.writeLog('WARN', module, message, data);
  }

  debug(module, message, data = null) {
    return this.writeLog('DEBUG', module, message, data);
  }

  success(module, message, data = null) {
    return this.writeLog('SUCCESS', module, message, data);
  }

  async getLogs() {
    try {
      const content = await fs.readFile(this.logFile, 'utf-8');
      return content;
    } catch (error) {
      if (error.code === 'ENOENT') {
        return '# No hay logs disponibles\n\nEl archivo de log aún no se ha creado.';
      }
      throw error;
    }
  }

  async getLogsAsMarkdown() {
    try {
      const content = await this.getLogs();
      
      // Convertir a formato Markdown
      let markdown = `# 📋 Logs de AI Core API\n\n`;
      markdown += `**Generado:** ${new Date().toISOString()}\n\n`;
      markdown += `---\n\n`;
      markdown += '```log\n';
      markdown += content;
      markdown += '\n```\n';
      
      return markdown;
    } catch (error) {
      return `# ❌ Error obteniendo logs\n\n${error.message}`;
    }
  }

  async clearLogs() {
    try {
      await fs.writeFile(this.logFile, '');
      console.log('[LOGGER] 🗑️ Logs limpiados');
      return true;
    } catch (error) {
      console.error('[LOGGER] ❌ Error limpiando logs:', error.message);
      return false;
    }
  }

  async getLogStats() {
    try {
      const stats = await fs.stat(this.logFile);
      const content = await this.getLogs();
      const lines = content.split('\n').filter(line => line.trim());
      
      return {
        size: stats.size,
        sizeFormatted: `${(stats.size / 1024).toFixed(2)} KB`,
        lines: lines.length,
        lastModified: stats.mtime,
        path: this.logFile
      };
    } catch (error) {
      return {
        size: 0,
        sizeFormatted: '0 KB',
        lines: 0,
        lastModified: null,
        path: this.logFile,
        error: error.message
      };
    }
  }
}

// Exportar instancia única
const loggerService = new LoggerService();
export default loggerService;
