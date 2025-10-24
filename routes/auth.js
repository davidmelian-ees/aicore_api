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
    const isProduction = process.env.NODE_ENV === 'production';

    if (isProduction) {
      // En producción: usar OAuth2 de SAP con xssec
      try {
        const xsenv = await import('@sap/xsenv');
        const { XsuaaService } = await import('@sap/xssec');

        xsenv.default.loadEnv();
        const services = xsenv.default.getServices({ xsuaa: { name: "aicore-app-auth" } });
        const credentials = services.xsuaa;

        // Aquí iría la lógica de OAuth2 de SAP
        // Por ahora, devolver error para indicar que debe usar OAuth2 completo
        return res.status(501).json({
          error: 'not_implemented',
          error_description: 'En producción use OAuth2 completo de SAP'
        });

      } catch (sapError) {
        console.error('[AUTH] Error SAP OAuth2:', sapError);
        return res.status(500).json({
          error: 'sap_auth_error',
          error_description: 'Error en autenticación SAP'
        });
      }
    } else {
      // En desarrollo: login simple con Basic Auth
      console.log('[AUTH] Modo desarrollo - usando login simple');

      // Extraer credenciales de Basic Auth
      const authHeader = req.headers.authorization;

      let username, password;

      if (authHeader && authHeader.startsWith('Basic ')) {
        // Decodificar Basic Auth
        const base64Credentials = authHeader.split(' ')[1];
        const credentials = Buffer.from(base64Credentials, 'base64').toString('ascii');
        [username, password] = credentials.split(':');
      } else {
        // Usar credenciales del body (fallback)
        username = req.body.username;
        password = req.body.password;
      }

      // Validar credenciales
      if (!username || !password) {
        return res.status(400).json({
          error: 'invalid_request',
          error_description: 'Username and password required'
        });
      }

      if (username !== DEV_CREDENTIALS.username || password !== DEV_CREDENTIALS.password) {
        return res.status(401).json({
          error: 'invalid_grant',
          error_description: 'Invalid username or password'
        });
      }

      // Generar token JWT simple para desarrollo
      const token = jwt.sign(
        {
          sub: username,
          name: username,
          preferred_username: username,
          email: `${username}@local.com`,
          jti: generateJti()
        },
        'dev-secret-key', // En desarrollo usar clave simple
        { expiresIn: '12h' }
      );

      console.log(`[AUTH] Login desarrollo exitoso para usuario: ${username}`);

      // Respuesta compatible con SAP OAuth2
      res.json({
        access_token: token,
        token_type: 'bearer',
        expires_in: 43199,
        scope: 'uaa.resource',
        jti: generateJti()
      });
    }

  } catch (error) {
    console.error('[AUTH] Error en login:', error);
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
