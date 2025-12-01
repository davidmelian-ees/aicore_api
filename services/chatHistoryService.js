import Database from 'better-sqlite3';
import path from 'path';
import fs from 'fs';
import { v4 as uuidv4 } from 'uuid';

const DB_PATH = './data/chat_history.db';

// Asegurar que el directorio existe
const dbDir = path.dirname(DB_PATH);
if (!fs.existsSync(dbDir)) {
  fs.mkdirSync(dbDir, { recursive: true });
}

// Inicializar base de datos
const db = new Database(DB_PATH);

// Crear tablas si no existen
db.exec(`
  CREATE TABLE IF NOT EXISTS chat_sessions (
    id TEXT PRIMARY KEY,
    user_id TEXT NOT NULL,
    context_id TEXT NOT NULL,
    title TEXT,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    message_count INTEGER DEFAULT 0
  );

  CREATE TABLE IF NOT EXISTS chat_messages (
    id TEXT PRIMARY KEY,
    session_id TEXT NOT NULL,
    role TEXT NOT NULL,
    content TEXT NOT NULL,
    metadata TEXT,
    created_at TEXT NOT NULL,
    FOREIGN KEY (session_id) REFERENCES chat_sessions(id) ON DELETE CASCADE
  );

  CREATE INDEX IF NOT EXISTS idx_sessions_user ON chat_sessions(user_id);
  CREATE INDEX IF NOT EXISTS idx_sessions_context ON chat_sessions(context_id);
  CREATE INDEX IF NOT EXISTS idx_messages_session ON chat_messages(session_id);
`);

console.log('✅ Base de datos de historial de chats inicializada');

/**
 * Crear una nueva sesión de chat
 */
export function createSession(userId, contextId, title = null) {
  const sessionId = uuidv4();
  const now = new Date().toISOString();
  
  const stmt = db.prepare(`
    INSERT INTO chat_sessions (id, user_id, context_id, title, created_at, updated_at, message_count)
    VALUES (?, ?, ?, ?, ?, ?, 0)
  `);
  
  stmt.run(sessionId, userId, contextId, title, now, now);
  
  return {
    id: sessionId,
    userId,
    contextId,
    title,
    createdAt: now,
    updatedAt: now,
    messageCount: 0
  };
}

/**
 * Obtener una sesión por ID
 */
export function getSession(sessionId) {
  const stmt = db.prepare(`
    SELECT id, user_id as userId, context_id as contextId, title, 
           created_at as createdAt, updated_at as updatedAt, message_count as messageCount
    FROM chat_sessions
    WHERE id = ?
  `);
  
  return stmt.get(sessionId);
}

/**
 * Listar sesiones de un usuario (solo sesiones con mensajes)
 */
export function listSessions(userId, contextId = null, limit = 50) {
  let query = `
    SELECT id, user_id as userId, context_id as contextId, title, 
           created_at as createdAt, updated_at as updatedAt, message_count as messageCount
    FROM chat_sessions
    WHERE user_id = ? AND message_count > 0
  `;
  
  const params = [userId];
  
  if (contextId) {
    query += ` AND context_id = ?`;
    params.push(contextId);
  }
  
  query += ` ORDER BY updated_at DESC LIMIT ?`;
  params.push(limit);
  
  const stmt = db.prepare(query);
  return stmt.all(...params);
}

/**
 * Agregar un mensaje a una sesión
 */
export function addMessage(sessionId, role, content, metadata = null) {
  const messageId = uuidv4();
  const now = new Date().toISOString();
  
  // Insertar mensaje
  const insertStmt = db.prepare(`
    INSERT INTO chat_messages (id, session_id, role, content, metadata, created_at)
    VALUES (?, ?, ?, ?, ?, ?)
  `);
  
  insertStmt.run(
    messageId,
    sessionId,
    role,
    content,
    metadata ? JSON.stringify(metadata) : null,
    now
  );
  
  // Actualizar contador y timestamp de la sesión
  const updateStmt = db.prepare(`
    UPDATE chat_sessions
    SET message_count = message_count + 1,
        updated_at = ?
    WHERE id = ?
  `);
  
  updateStmt.run(now, sessionId);
  
  // Generar título automático si es el primer mensaje del usuario
  const session = getSession(sessionId);
  if (!session.title && role === 'user') {
    const title = content.substring(0, 50) + (content.length > 50 ? '...' : '');
    updateSessionTitle(sessionId, title);
  }
  
  return {
    id: messageId,
    sessionId,
    role,
    content,
    metadata: metadata ? metadata : null,
    createdAt: now
  };
}

/**
 * Obtener mensajes de una sesión
 */
export function getMessages(sessionId, limit = 100) {
  const stmt = db.prepare(`
    SELECT id, session_id as sessionId, role, content, metadata, created_at as createdAt
    FROM chat_messages
    WHERE session_id = ?
    ORDER BY created_at ASC
    LIMIT ?
  `);
  
  const messages = stmt.all(sessionId, limit);
  
  // Parsear metadata JSON
  return messages.map(msg => ({
    ...msg,
    metadata: msg.metadata ? JSON.parse(msg.metadata) : null
  }));
}

/**
 * Actualizar título de una sesión
 */
export function updateSessionTitle(sessionId, title) {
  const stmt = db.prepare(`
    UPDATE chat_sessions
    SET title = ?, updated_at = ?
    WHERE id = ?
  `);
  
  const now = new Date().toISOString();
  stmt.run(title, now, sessionId);
}

/**
 * Eliminar una sesión (y sus mensajes en cascada)
 */
export function deleteSession(sessionId) {
  const stmt = db.prepare(`DELETE FROM chat_sessions WHERE id = ?`);
  const result = stmt.run(sessionId);
  return result.changes > 0;
}

/**
 * Obtener estadísticas de historial
 */
export function getHistoryStats(userId = null) {
  let query = `
    SELECT 
      COUNT(DISTINCT id) as totalSessions,
      SUM(message_count) as totalMessages,
      COUNT(DISTINCT context_id) as totalContexts
    FROM chat_sessions
  `;
  
  const params = [];
  
  if (userId) {
    query += ` WHERE user_id = ?`;
    params.push(userId);
  }
  
  const stmt = db.prepare(query);
  return stmt.get(...params);
}

/**
 * Limpiar sesiones antiguas (opcional, para mantenimiento)
 */
export function cleanOldSessions(daysOld = 30) {
  const cutoffDate = new Date();
  cutoffDate.setDate(cutoffDate.getDate() - daysOld);
  const cutoffISO = cutoffDate.toISOString();
  
  const stmt = db.prepare(`
    DELETE FROM chat_sessions
    WHERE updated_at < ?
  `);
  
  const result = stmt.run(cutoffISO);
  return result.changes;
}

export default {
  createSession,
  getSession,
  listSessions,
  addMessage,
  getMessages,
  updateSessionTitle,
  deleteSession,
  getHistoryStats,
  cleanOldSessions
};
