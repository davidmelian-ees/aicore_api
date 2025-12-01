import express from 'express';
import jwt from 'jsonwebtoken';

const router = express.Router();

// Credenciales simples para desarrollo
const DEV_CREDENTIALS = {
  username: 'admin',
  password: 'admin123'
};

/**
 * @route POST /oauth/token
 * @desc Endpoint híbrido: Basic Auth simple (desarrollo) o OAuth2 SAP (producción)
 * @header {string} Authorization - Basic Auth con usuario:contraseña (desarrollo) O Bearer token (producción)
 * @body {string} grant_type - Tipo de grant
 */
router.post('/token', async (req, res) => {
  try {
    console.log('[AUTH] Endpoint /oauth/token - Este endpoint ya no se usa para login');
    console.log('[AUTH] El frontend llama directamente a SAP OAuth2');

    // Este endpoint podría usarse para refresh tokens o validación
    // Pero por ahora, devolver error indicando que no se usa
    return res.status(410).json({
      error: 'gone',
      error_description: 'Este endpoint ya no se usa. El frontend llama directamente a SAP OAuth2'
    });

  } catch (error) {
    console.error('[AUTH] Error en endpoint token:', error);
    res.status(500).json({
      error: 'server_error',
      error_description: 'Internal server error'
    });
  }
});

/**
 * @route POST /oauth/logout
 * @desc Logout
 */
router.post('/logout', (req, res) => {
  res.json({
    message: 'Logged out successfully'
  });
});

/**
 * @route GET /oauth/dev-info
 * @desc Información de desarrollo (solo en desarrollo)
 */
router.get('/dev-info', (req, res) => {
  const isProduction = process.env.NODE_ENV === 'production';

  if (isProduction) {
    return res.status(404).json({ error: 'Not found' });
  }

  res.json({
    mode: 'development',
    credentials: {
      username: DEV_CREDENTIALS.username,
      password: DEV_CREDENTIALS.password
    },
    endpoints: {
      token: '/oauth/token',
      logout: '/oauth/logout'
    },
    note: 'En desarrollo usa Basic Auth con estas credenciales'
  });
});

function generateJti() {
  return Math.random().toString(36).substring(2, 15) + Math.random().toString(36).substring(2, 15);
}

export default router;
