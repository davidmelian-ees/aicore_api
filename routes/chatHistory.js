import express from 'express';
import {
  createSession,
  getSession,
  listSessions,
  addMessage,
  getMessages,
  updateSessionTitle,
  deleteSession,
  getHistoryStats
} from '../services/chatHistoryService.js';

const router = express.Router();

/**
 * POST /api/chat-history/sessions
 * Crear una nueva sesión de chat
 */
router.post('/sessions', async (req, res) => {
  try {
    const { userId, contextId, title } = req.body;
    
    if (!userId || !contextId) {
      return res.status(400).json({
        success: false,
        error: 'userId y contextId son requeridos'
      });
    }
    
    const session = createSession(userId, contextId, title);
    
    res.json({
      success: true,
      session
    });
    
  } catch (error) {
    console.error('[CHAT-HISTORY] Error creando sesión:', error);
    res.status(500).json({
      success: false,
      error: error.message
    });
  }
});

/**
 * GET /api/chat-history/sessions
 * Listar sesiones de un usuario
 */
router.get('/sessions', async (req, res) => {
  try {
    const { userId, contextId, limit } = req.query;
    
    if (!userId) {
      return res.status(400).json({
        success: false,
        error: 'userId es requerido'
      });
    }
    
    const sessions = listSessions(
      userId,
      contextId || null,
      limit ? parseInt(limit) : 50
    );
    
    res.json({
      success: true,
      sessions,
      count: sessions.length
    });
    
  } catch (error) {
    console.error('[CHAT-HISTORY] Error listando sesiones:', error);
    res.status(500).json({
      success: false,
      error: error.message
    });
  }
});

/**
 * GET /api/chat-history/sessions/:sessionId
 * Obtener una sesión específica
 */
router.get('/sessions/:sessionId', async (req, res) => {
  try {
    const { sessionId } = req.params;
    
    const session = getSession(sessionId);
    
    if (!session) {
      return res.status(404).json({
        success: false,
        error: 'Sesión no encontrada'
      });
    }
    
    res.json({
      success: true,
      session
    });
    
  } catch (error) {
    console.error('[CHAT-HISTORY] Error obteniendo sesión:', error);
    res.status(500).json({
      success: false,
      error: error.message
    });
  }
});

/**
 * PUT /api/chat-history/sessions/:sessionId
 * Actualizar título de una sesión
 */
router.put('/sessions/:sessionId', async (req, res) => {
  try {
    const { sessionId } = req.params;
    const { title } = req.body;
    
    if (!title) {
      return res.status(400).json({
        success: false,
        error: 'title es requerido'
      });
    }
    
    updateSessionTitle(sessionId, title);
    const session = getSession(sessionId);
    
    res.json({
      success: true,
      session
    });
    
  } catch (error) {
    console.error('[CHAT-HISTORY] Error actualizando sesión:', error);
    res.status(500).json({
      success: false,
      error: error.message
    });
  }
});

/**
 * DELETE /api/chat-history/sessions/:sessionId
 * Eliminar una sesión
 */
router.delete('/sessions/:sessionId', async (req, res) => {
  try {
    const { sessionId } = req.params;
    
    const deleted = deleteSession(sessionId);
    
    if (!deleted) {
      return res.status(404).json({
        success: false,
        error: 'Sesión no encontrada'
      });
    }
    
    res.json({
      success: true,
      message: 'Sesión eliminada exitosamente'
    });
    
  } catch (error) {
    console.error('[CHAT-HISTORY] Error eliminando sesión:', error);
    res.status(500).json({
      success: false,
      error: error.message
    });
  }
});

/**
 * POST /api/chat-history/sessions/:sessionId/messages
 * Agregar un mensaje a una sesión
 */
router.post('/sessions/:sessionId/messages', async (req, res) => {
  try {
    const { sessionId } = req.params;
    const { role, content, metadata } = req.body;
    
    if (!role || !content) {
      return res.status(400).json({
        success: false,
        error: 'role y content son requeridos'
      });
    }
    
    if (!['user', 'assistant', 'system'].includes(role)) {
      return res.status(400).json({
        success: false,
        error: 'role debe ser user, assistant o system'
      });
    }
    
    const message = addMessage(sessionId, role, content, metadata);
    
    res.json({
      success: true,
      message
    });
    
  } catch (error) {
    console.error('[CHAT-HISTORY] Error agregando mensaje:', error);
    res.status(500).json({
      success: false,
      error: error.message
    });
  }
});

/**
 * GET /api/chat-history/sessions/:sessionId/messages
 * Obtener mensajes de una sesión (máximo 20 por defecto)
 */
router.get('/sessions/:sessionId/messages', async (req, res) => {
  try {
    const { sessionId } = req.params;
    const { limit } = req.query;
    
    const messages = getMessages(sessionId, limit ? parseInt(limit) : 20);
    
    res.json({
      success: true,
      messages,
      count: messages.length
    });
    
  } catch (error) {
    console.error('[CHAT-HISTORY] Error obteniendo mensajes:', error);
    res.status(500).json({
      success: false,
      error: error.message
    });
  }
});

/**
 * GET /api/chat-history/stats
 * Obtener estadísticas de historial
 */
router.get('/stats', async (req, res) => {
  try {
    const { userId } = req.query;
    
    const stats = getHistoryStats(userId || null);
    
    res.json({
      success: true,
      stats
    });
    
  } catch (error) {
    console.error('[CHAT-HISTORY] Error obteniendo estadísticas:', error);
    res.status(500).json({
      success: false,
      error: error.message
    });
  }
});

export default router;
