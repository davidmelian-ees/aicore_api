import Database from 'better-sqlite3';
import fs from 'fs/promises';
import path from 'path';

/**
 * Servicio para gestionar validaciones de pliegos con retención de 3 días
 * Almacena documento original + informe de validación como ARCHIVOS en carpetas
 * Estructura: data/validaciones/[validacion_id]/
 *   - documento_original.pdf
 *   - informe_validacion.json
 *   - informe.pdf
 *   - metadata.json
 */
class ValidacionService {
  constructor() {
    this.db = null;
    this.isInitialized = false;
    this.dbPath = './data/validaciones.db';
    this.validacionesDir = './data/validaciones'; // Carpeta base
  }

  /**
   * Inicializa la base de datos de validaciones
   */
  async initialize() {
    try {
      console.log('[VALIDACION-SERVICE] Inicializando servicio de validaciones...');
      
      // Crear directorio de datos si no existe
      await fs.mkdir('./data', { recursive: true });
      
      // Crear directorio de validaciones
      await fs.mkdir(this.validacionesDir, { recursive: true });
      
      // Crear conexión a SQLite (solo para índice)
      this.db = new Database(this.dbPath);
      
      // Crear tablas
      this.createTables();
      
      // Configurar SQLite para mejor rendimiento
      this.db.pragma('journal_mode = WAL');
      this.db.pragma('synchronous = NORMAL');
      this.db.pragma('cache_size = 1000');
      
      this.isInitialized = true;
      console.log('[VALIDACION-SERVICE] ✅ Servicio inicializado');
      console.log(`[VALIDACION-SERVICE] 📁 Base de datos: ${this.dbPath}`);
      console.log(`[VALIDACION-SERVICE] 📂 Carpeta validaciones: ${this.validacionesDir}`);
      
      // Mostrar estadísticas
      const stats = this.getStats();
      console.log(`[VALIDACION-SERVICE] 📊 Validaciones activas: ${stats.validacionesActivas}`);
      
    } catch (error) {
      console.error('[VALIDACION-SERVICE] ❌ Error inicializando:', error);
      throw error;
    }
  }

  /**
   * Crea las tablas necesarias
   */
  createTables() {
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS validaciones (
        id TEXT PRIMARY KEY,
        usuario_id TEXT NOT NULL,
        
        -- Rutas a archivos
        carpeta_path TEXT NOT NULL,
        documento_nombre TEXT NOT NULL,
        documento_tipo TEXT NOT NULL,
        documento_size INTEGER NOT NULL,
        
        -- Metadatos
        context_id TEXT,
        fecha_creacion DATETIME DEFAULT CURRENT_TIMESTAMP,
        fecha_expiracion DATETIME NOT NULL,
        
        -- Índices
        UNIQUE(id)
      )
    `);

    // Índices para búsquedas rápidas
    this.db.exec(`
      CREATE INDEX IF NOT EXISTS idx_validaciones_usuario ON validaciones(usuario_id);
      CREATE INDEX IF NOT EXISTS idx_validaciones_expiracion ON validaciones(fecha_expiracion);
      CREATE INDEX IF NOT EXISTS idx_validaciones_fecha_creacion ON validaciones(fecha_creacion DESC);
    `);

    console.log('[VALIDACION-SERVICE] ✅ Tablas creadas/verificadas');
  }

  /**
   * Limpia validaciones expiradas (más de 3 días)
   * Se ejecuta automáticamente al inicio de cada validación
   * Borra tanto los registros de BD como las carpetas con archivos
   */
  async limpiarValidacionesExpiradas() {
    try {
      if (!this.isInitialized) {
        console.warn('[VALIDACION-SERVICE] ⚠️ Servicio no inicializado');
        return 0;
      }

      // Obtener validaciones expiradas antes de borrarlas
      const expiradas = this.db.prepare(`
        SELECT id, carpeta_path, documento_nombre
        FROM validaciones 
        WHERE fecha_expiracion < datetime('now')
      `).all();
      
      if (expiradas.length === 0) {
        return 0;
      }
      
      console.log(`[VALIDACION-SERVICE] 🧹 Limpiando ${expiradas.length} validaciones expiradas...`);
      
      // Borrar carpetas de archivos
      let carpetasBorradas = 0;
      for (const validacion of expiradas) {
        try {
          const carpetaPath = path.join(process.cwd(), validacion.carpeta_path);
          await fs.rm(carpetaPath, { recursive: true, force: true });
          carpetasBorradas++;
          console.log(`[VALIDACION-SERVICE]   ✓ Borrada carpeta: ${validacion.id}`);
        } catch (err) {
          console.warn(`[VALIDACION-SERVICE]   ⚠️ Error borrando carpeta ${validacion.id}:`, err.message);
        }
      }
      
      // Borrar registros de BD
      const stmt = this.db.prepare(`
        DELETE FROM validaciones 
        WHERE fecha_expiracion < datetime('now')
      `);
      
      const resultado = stmt.run();
      
      console.log(`[VALIDACION-SERVICE] ✅ Eliminadas ${resultado.changes} validaciones (${carpetasBorradas} carpetas borradas)`);
      
      return resultado.changes;
      
    } catch (error) {
      console.error('[VALIDACION-SERVICE] ❌ Error limpiando validaciones:', error);
      // No lanzar error, solo logear (la limpieza no debe bloquear la validación)
      return 0;
    }
  }

  /**
   * Guarda documento + validación enlazados como ARCHIVOS en carpeta
   * Estructura creada:
   *   data/validaciones/[id]/
   *     - documento_original.pdf (o .docx)
   *     - informe_validacion.json
   *     - informe.pdf
   *     - metadata.json
   * 
   * @param {string} userId - ID del usuario
   * @param {Object} documento - Documento con buffer, originalname, mimetype, size
   * @param {Object} informeValidacion - Informe de validación generado
   * @param {Buffer} informePdf - PDF del informe (opcional)
   * @param {string} contextId - ID del contexto (opcional)
   * @returns {Object} Validación guardada con id, fecha_creacion, fecha_expiracion
   */
  async guardarValidacion(userId, documento, informeValidacion, informePdf = null, contextId = null) {
    try {
      if (!this.isInitialized) {
        throw new Error('Servicio no inicializado');
      }

      // Generar ID único
      const id = this.generateId();
      
      // Crear carpeta para esta validación (ruta absoluta)
      const carpetaPath = path.join(this.validacionesDir, id);
      const carpetaAbsoluta = path.resolve(carpetaPath);
      
      console.log(`[VALIDACION-SERVICE] 📂 Creando carpeta: ${carpetaAbsoluta}`);
      await fs.mkdir(carpetaAbsoluta, { recursive: true });
      
      // Obtener extensión del documento original
      const ext = path.extname(documento.originalname) || '.pdf';
      
      // Guardar documento original
      const documentoPath = path.join(carpetaAbsoluta, `documento_original${ext}`);
      console.log(`[VALIDACION-SERVICE] 💾 Guardando documento: ${documentoPath}`);
      await fs.writeFile(documentoPath, documento.buffer);
      
      // Guardar informe de validación (JSON)
      const informePath = path.join(carpetaAbsoluta, 'informe_validacion.json');
      console.log(`[VALIDACION-SERVICE] 📋 Guardando informe: ${informePath}`);
      await fs.writeFile(informePath, JSON.stringify(informeValidacion, null, 2));
      
      // Guardar PDF del informe (si existe)
      if (informePdf) {
        const informePdfPath = path.join(carpetaAbsoluta, 'informe.pdf');
        console.log(`[VALIDACION-SERVICE] 📄 Guardando PDF: ${informePdfPath}`);
        await fs.writeFile(informePdfPath, informePdf);
      }
      
      // Guardar metadata
      const metadata = {
        id,
        usuario_id: userId,
        documento_nombre: documento.originalname,
        documento_tipo: documento.mimetype,
        documento_size: documento.size,
        context_id: contextId,
        fecha_creacion: new Date().toISOString(),
        fecha_expiracion: new Date(Date.now() + 3 * 24 * 60 * 60 * 1000).toISOString()
      };
      const metadataPath = path.join(carpetaAbsoluta, 'metadata.json');
      console.log(`[VALIDACION-SERVICE] 📝 Guardando metadata: ${metadataPath}`);
      await fs.writeFile(metadataPath, JSON.stringify(metadata, null, 2));
      
      // Guardar referencia en BD (solo rutas)
      const stmt = this.db.prepare(`
        INSERT INTO validaciones 
        (id, usuario_id, carpeta_path, documento_nombre, documento_tipo, documento_size, 
         context_id, fecha_expiracion)
        VALUES (?, ?, ?, ?, ?, ?, ?, datetime('now', '+3 days'))
      `);
      
      stmt.run(
        id,
        userId,
        carpetaPath,
        documento.originalname,
        documento.mimetype,
        documento.size,
        contextId
      );
      
      // Obtener datos de la validación guardada
      const validacion = this.db.prepare(`
        SELECT id, fecha_creacion, fecha_expiracion 
        FROM validaciones 
        WHERE id = ?
      `).get(id);
      
      console.log(`[VALIDACION-SERVICE] ✅ Validación guardada: ${id}`);
      console.log(`[VALIDACION-SERVICE] 📂 Carpeta ABSOLUTA: ${carpetaAbsoluta}`);
      console.log(`[VALIDACION-SERVICE] 📄 Documento: ${documento.originalname} (${documento.size} bytes)`);
      console.log(`[VALIDACION-SERVICE] ⏰ Expira: ${validacion.fecha_expiracion}`);
      
      return validacion;
      
    } catch (error) {
      console.error('[VALIDACION-SERVICE] ❌ Error guardando validación:', error);
      throw error;
    }
  }

  /**
   * Obtiene validaciones del usuario (no expiradas)
   * @param {string} userId - ID del usuario
   * @returns {Array} Lista de validaciones
   */
  obtenerValidaciones(userId) {
    try {
      if (!this.isInitialized) {
        throw new Error('Servicio no inicializado');
      }

      const stmt = this.db.prepare(`
        SELECT 
          id,
          documento_nombre,
          documento_tipo,
          documento_size,
          context_id,
          fecha_creacion,
          fecha_expiracion,
          CAST((julianday(fecha_expiracion) - julianday('now')) AS INTEGER) as dias_restantes
        FROM validaciones 
        WHERE usuario_id = ? AND fecha_expiracion > datetime('now')
        ORDER BY fecha_creacion DESC
      `);
      
      const validaciones = stmt.all(userId);
      
      console.log(`[VALIDACION-SERVICE] 📋 Encontradas ${validaciones.length} validaciones para usuario ${userId}`);
      
      return validaciones;
      
    } catch (error) {
      console.error('[VALIDACION-SERVICE] ❌ Error obteniendo validaciones:', error);
      throw error;
    }
  }

  /**
   * Obtiene una validación específica leyendo archivos de la carpeta
   * @param {string} validacionId - ID de la validación
   * @param {string} userId - ID del usuario
   * @returns {Object|null} Validación completa o null si no existe
   */
  async obtenerValidacion(validacionId, userId) {
    try {
      if (!this.isInitialized) {
        throw new Error('Servicio no inicializado');
      }

      const stmt = this.db.prepare(`
        SELECT 
          id,
          carpeta_path,
          documento_nombre,
          documento_tipo,
          documento_size,
          context_id,
          fecha_creacion,
          fecha_expiracion
        FROM validaciones 
        WHERE id = ? AND usuario_id = ? AND fecha_expiracion > datetime('now')
      `);
      
      const validacion = stmt.get(validacionId, userId);
      
      if (!validacion) {
        console.log(`[VALIDACION-SERVICE] ⚠️ Validación no encontrada o expirada: ${validacionId}`);
        return null;
      }
      
      // Leer archivos de la carpeta
      const carpetaPath = path.join(process.cwd(), validacion.carpeta_path);
      
      // Leer documento original
      const ext = path.extname(validacion.documento_nombre) || '.pdf';
      const documentoPath = path.join(carpetaPath, `documento_original${ext}`);
      validacion.documento_original = await fs.readFile(documentoPath);
      
      // Leer informe de validación
      const informePath = path.join(carpetaPath, 'informe_validacion.json');
      const informeContent = await fs.readFile(informePath, 'utf8');
      validacion.informe_validacion = JSON.parse(informeContent);
      
      // Leer PDF del informe (si existe)
      const informePdfPath = path.join(carpetaPath, 'informe.pdf');
      try {
        validacion.informe_pdf = await fs.readFile(informePdfPath);
      } catch (err) {
        validacion.informe_pdf = null;
      }
      
      return validacion;
      
    } catch (error) {
      console.error('[VALIDACION-SERVICE] ❌ Error obteniendo validación:', error);
      throw error;
    }
  }

  /**
   * Elimina una validación específica (BD + carpeta de archivos)
   * @param {string} validacionId - ID de la validación
   * @param {string} userId - ID del usuario
   * @returns {boolean} True si se eliminó, false si no existía
   */
  async eliminarValidacion(validacionId, userId) {
    try {
      if (!this.isInitialized) {
        throw new Error('Servicio no inicializado');
      }

      // Obtener carpeta antes de borrar
      const validacion = this.db.prepare(`
        SELECT carpeta_path FROM validaciones 
        WHERE id = ? AND usuario_id = ?
      `).get(validacionId, userId);
      
      if (!validacion) {
        return false;
      }
      
      // Borrar carpeta de archivos
      const carpetaPath = path.join(process.cwd(), validacion.carpeta_path);
      try {
        await fs.rm(carpetaPath, { recursive: true, force: true });
        console.log(`[VALIDACION-SERVICE] 📂 Carpeta eliminada: ${carpetaPath}`);
      } catch (err) {
        console.warn(`[VALIDACION-SERVICE] ⚠️ Error borrando carpeta:`, err.message);
      }
      
      // Borrar registro de BD
      const stmt = this.db.prepare(`
        DELETE FROM validaciones 
        WHERE id = ? AND usuario_id = ?
      `);
      
      const resultado = stmt.run(validacionId, userId);
      
      if (resultado.changes > 0) {
        console.log(`[VALIDACION-SERVICE] 🗑️ Validación eliminada: ${validacionId}`);
        return true;
      }
      
      return false;
      
    } catch (error) {
      console.error('[VALIDACION-SERVICE] ❌ Error eliminando validación:', error);
      throw error;
    }
  }

  /**
   * Obtiene estadísticas del servicio
   * @returns {Object} Estadísticas
   */
  getStats() {
    try {
      if (!this.isInitialized) {
        return { validacionesActivas: 0, validacionesExpiradas: 0, totalSize: 0 };
      }

      const activas = this.db.prepare(`
        SELECT COUNT(*) as count, SUM(documento_size) as total_size
        FROM validaciones 
        WHERE fecha_expiracion > datetime('now')
      `).get();
      
      const expiradas = this.db.prepare(`
        SELECT COUNT(*) as count
        FROM validaciones 
        WHERE fecha_expiracion <= datetime('now')
      `).get();
      
      return {
        validacionesActivas: activas.count || 0,
        validacionesExpiradas: expiradas.count || 0,
        totalSize: activas.total_size || 0
      };
      
    } catch (error) {
      console.error('[VALIDACION-SERVICE] ❌ Error obteniendo estadísticas:', error);
      return { validacionesActivas: 0, validacionesExpiradas: 0, totalSize: 0 };
    }
  }

  /**
   * Genera un ID único para la validación
   * @returns {string} ID único
   */
  generateId() {
    return `val_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  }

  /**
   * Cierra la conexión a la base de datos
   */
  close() {
    if (this.db) {
      this.db.close();
      this.isInitialized = false;
      console.log('[VALIDACION-SERVICE] 🔒 Conexión cerrada');
    }
  }
}

// Exportar instancia singleton
const validacionService = new ValidacionService();

export default validacionService;
