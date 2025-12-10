import Database from 'better-sqlite3';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

/**
 * Script para ver validaciones guardadas desde la terminal
 * Uso: node scripts/ver-validaciones.js [usuario]
 */

const dbPath = path.join(__dirname, '..', 'data', 'validaciones.db');

try {
  // Conectar a la base de datos
  const db = new Database(dbPath, { readonly: true });
  
  // Obtener usuario de argumentos o usar 'anonymous'
  const usuario = process.argv[2] || 'anonymous';
  
  console.log('================================================================================');
  console.log('📋 VALIDACIONES GUARDADAS');
  console.log('================================================================================');
  console.log(`Usuario: ${usuario}`);
  console.log('');
  
  // Obtener validaciones del usuario
  const validaciones = db.prepare(`
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
  `).all(usuario);
  
  if (validaciones.length === 0) {
    console.log('❌ No hay validaciones guardadas para este usuario');
    console.log('');
    console.log('💡 Las validaciones se guardan automáticamente al validar un pliego');
    console.log('   Retención: 3 días');
  } else {
    console.log(`✅ Encontradas ${validaciones.length} validación(es):`);
    console.log('');
    
    validaciones.forEach((v, index) => {
      console.log(`${index + 1}. 📄 ${v.documento_nombre}`);
      console.log(`   ID: ${v.id}`);
      console.log(`   Tamaño: ${(v.documento_size / 1024).toFixed(2)} KB`);
      console.log(`   Creado: ${v.fecha_creacion}`);
      console.log(`   Expira: ${v.fecha_expiracion} (${v.dias_restantes} días restantes)`);
      if (v.context_id) {
        console.log(`   Contexto: ${v.context_id}`);
      }
      console.log('');
    });
  }
  
  // Estadísticas generales
  const stats = db.prepare(`
    SELECT 
      COUNT(*) as total_activas,
      SUM(documento_size) as total_size,
      COUNT(DISTINCT usuario_id) as total_usuarios
    FROM validaciones 
    WHERE fecha_expiracion > datetime('now')
  `).get();
  
  const expiradas = db.prepare(`
    SELECT COUNT(*) as total
    FROM validaciones 
    WHERE fecha_expiracion <= datetime('now')
  `).get();
  
  console.log('================================================================================');
  console.log('📊 ESTADÍSTICAS GENERALES');
  console.log('================================================================================');
  console.log(`Validaciones activas: ${stats.total_activas}`);
  console.log(`Validaciones expiradas: ${expiradas.total}`);
  console.log(`Usuarios con validaciones: ${stats.total_usuarios}`);
  console.log(`Tamaño total: ${((stats.total_size || 0) / 1024 / 1024).toFixed(2)} MB`);
  console.log('');
  
  db.close();
  
} catch (error) {
  if (error.code === 'SQLITE_CANTOPEN') {
    console.log('❌ Base de datos no encontrada');
    console.log('💡 Aún no se ha validado ningún pliego');
    console.log(`   Ruta esperada: ${dbPath}`);
  } else {
    console.error('❌ Error:', error.message);
  }
}
