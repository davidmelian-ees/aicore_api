import loggerService from '../services/loggerService.js';

/**
 * Middleware para logging automático de todas las requests
 * Registra información detallada de cada endpoint llamado
 */
export function requestLogger(req, res, next) {
  const startTime = Date.now();
  
  // Información básica de la request
  const requestInfo = {
    method: req.method,
    endpoint: req.originalUrl || req.url,
    ip: req.ip || req.connection.remoteAddress,
    userAgent: req.get('user-agent'),
    username: req.body?.username || req.query?.username || 'anonymous'
  };

  // Log de inicio de request
  loggerService.info('API-REQUEST', `${req.method} ${requestInfo.endpoint}`, {
    username: requestInfo.username,
    ip: requestInfo.ip,
    userAgent: requestInfo.userAgent
  });

  // Capturar el método original res.json para loggear la respuesta
  const originalJson = res.json.bind(res);
  const originalSend = res.send.bind(res);

  // Override res.json
  res.json = function(body) {
    const duration = Date.now() - startTime;
    
    // Determinar nivel de log según status code
    if (res.statusCode >= 500) {
      loggerService.error('API-RESPONSE', `${req.method} ${requestInfo.endpoint} - ${res.statusCode}`, {
        username: requestInfo.username,
        statusCode: res.statusCode,
        duration: `${duration}ms`,
        error: body?.error || body?.message
      });
    } else if (res.statusCode >= 400) {
      loggerService.warn('API-RESPONSE', `${req.method} ${requestInfo.endpoint} - ${res.statusCode}`, {
        username: requestInfo.username,
        statusCode: res.statusCode,
        duration: `${duration}ms`,
        error: body?.error || body?.message
      });
    } else {
      loggerService.success('API-RESPONSE', `${req.method} ${requestInfo.endpoint} - ${res.statusCode}`, {
        username: requestInfo.username,
        statusCode: res.statusCode,
        duration: `${duration}ms`
      });
    }
    
    return originalJson(body);
  };

  // Override res.send (para PDFs y otros tipos)
  res.send = function(body) {
    const duration = Date.now() - startTime;
    
    if (res.statusCode >= 500) {
      loggerService.error('API-RESPONSE', `${req.method} ${requestInfo.endpoint} - ${res.statusCode}`, {
        username: requestInfo.username,
        statusCode: res.statusCode,
        duration: `${duration}ms`,
        contentType: res.get('content-type')
      });
    } else if (res.statusCode >= 400) {
      loggerService.warn('API-RESPONSE', `${req.method} ${requestInfo.endpoint} - ${res.statusCode}`, {
        username: requestInfo.username,
        statusCode: res.statusCode,
        duration: `${duration}ms`,
        contentType: res.get('content-type')
      });
    } else {
      loggerService.success('API-RESPONSE', `${req.method} ${requestInfo.endpoint} - ${res.statusCode}`, {
        username: requestInfo.username,
        statusCode: res.statusCode,
        duration: `${duration}ms`,
        contentType: res.get('content-type'),
        size: body?.length || 0
      });
    }
    
    return originalSend(body);
  };

  next();
}

/**
 * Middleware para extraer username de diferentes fuentes
 * Prioridad: body > query > header
 */
export function extractUsername(req, res, next) {
  // Intentar obtener username de diferentes fuentes
  const username = 
    req.body?.username || 
    req.query?.username || 
    req.headers['x-username'] ||
    req.headers['username'] ||
    'anonymous';
  
  // Añadir username al objeto req para uso posterior
  req.username = username;
  
  next();
}
