import "dotenv/config";
import express from "express";
import { initAuth } from "./auth.js";
import chatRoutes from "./routes/chat.js";
import ragRoutes from "./routes/rag.js";
import pdfCorrectionRoutes from "./routes/pdfCorrection.js";
import analyticsRoutes from "./routes/analytics.js";
import authRoutes from "./routes/auth.js";
import pliegoErrorsRoutes from "./routes/pliegoErrors.js";
import logsRoutes from "./routes/logs.js";
import chatHistoryRoutes from "./routes/chatHistory.js";
import aiCoreHealthRoutes from "./routes/aiCoreHealth.js";
import { requestLogger, extractUsername } from "./middleware/requestLogger.js";
import { initializeSampleData } from "./scripts/init-sample-data.js";
import { persistenceManager } from "./services/persistenceManager.js";

const app = express();

// Configuración para Cloud Foundry
const isProduction = process.env.NODE_ENV === 'production';
const port = process.env.PORT || 4000;

console.log(`🌍 Entorno: ${isProduction ? 'Producción (Cloud Foundry)' : 'Desarrollo'}`);
console.log(`📊 Vector Store: ${process.env.VECTOR_STORE_TYPE || 'auto'}`);

// ⚠️ CORS DEBE IR ANTES QUE TODO ⚠️
// Configurar CORS para desarrollo y producción
app.use((req, res, next) => {
  const origin = req.headers.origin;
  
  // Debug logging en desarrollo
  if (!isProduction) {
    console.log(`[CORS] ${req.method} ${req.path} - Origin: ${origin || 'none'}`);
    console.log(`[CORS] Headers recibidos:`, Object.keys(req.headers));
  }
  
  if (isProduction) {
    // En producción, permitir localhost para desarrollo local + dominio de producción
    const allowedOrigins = [
      'https://rag-dashboard-ui.cfapps.eu10-005.hana.ondemand.com',
      'http://localhost:3000',
      'http://127.0.0.1:3000',
      'http://localhost:3001',
      'http://127.0.0.1:3001'
    ];
    
    if (origin && allowedOrigins.includes(origin)) {
      res.header('Access-Control-Allow-Origin', origin);
      console.log(`[CORS] ✅ Producción - permitiendo origen: ${origin}`);
    } else if (!origin) {
      // Para requests sin origin (como Postman)
      res.header('Access-Control-Allow-Origin', '*');
      console.log(`[CORS] ✅ Sin origin - permitiendo *`);
    } else {
      console.log(`[CORS] ⚠️ Origen no permitido en producción: ${origin}`);
      // En producción, rechazar orígenes no permitidos
      res.status(403).json({ error: 'CORS not allowed' });
      return;
    }
  } else {
    // En desarrollo, permitir localhost en cualquier puerto
    const allowedOrigins = [
      'http://localhost:3000',
      'http://127.0.0.1:3000',
      'http://localhost:3001',
      'http://127.0.0.1:3001'
    ];
    
    if (origin && allowedOrigins.includes(origin)) {
      res.header('Access-Control-Allow-Origin', origin);
      console.log(`[CORS] ✅ Desarrollo - permitiendo origen específico: ${origin}`);
    } else if (!origin) {
      // Para requests sin origin (como Postman)
      res.header('Access-Control-Allow-Origin', '*');
      console.log(`[CORS] ✅ Sin origin - permitiendo *`);
    } else {
      console.log(`[CORS] ⚠️ Origen no permitido: ${origin}`);
      res.header('Access-Control-Allow-Origin', origin); // Permitir de todas formas en desarrollo
    }
  }
  
  res.header('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS, PATCH');
  res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept, Authorization, Cache-Control, Pragma');
  res.header('Access-Control-Allow-Credentials', 'true');
  res.header('Access-Control-Max-Age', '86400'); // Cache preflight por 24 horas
  
  if (req.method === 'OPTIONS') {
    console.log(`[CORS] ✅ Preflight request para ${req.path} - respondiendo 200`);
    res.status(200).end();
    return;
  }
  
  next();
});

// Middleware de validación de tokens SAP
const validateSAPToken = (req, res, next) => {
  const authHeader = req.headers.authorization;

  if (!authHeader || !authHeader.startsWith('Bearer ')) {
    return res.status(401).json({
      error: 'unauthorized',
      error_description: 'Token Bearer requerido'
    });
  }

  const token = authHeader.substring(7); // Remover 'Bearer '

  try {
    // Decodificar JWT para validar estructura básica
    const decoded = JSON.parse(Buffer.from(token.split('.')[1], 'base64').toString());

    // Verificar que sea un token de SAP (básico)
    if (!decoded.iss || !decoded.exp) {
      throw new Error('Token inválido');
    }

    // Verificar expiración
    const now = Math.floor(Date.now() / 1000);
    if (decoded.exp < now) {
      return res.status(401).json({
        error: 'token_expired',
        error_description: 'Token expirado'
      });
    }

    // Agregar información del usuario al request
    req.user = {
      client_id: decoded.client_id || decoded.sub,
      scope: decoded.scope || [],
      iss: decoded.iss
    };

    console.log(`[AUTH] ✅ Token válido para client: ${req.user.client_id}`);
    next();

  } catch (error) {
    console.error('[AUTH] ❌ Error validando token:', error.message);
    return res.status(401).json({
      error: 'invalid_token',
      error_description: 'Token inválido'
    });
  }
};

console.log(`🌐 CORS configurado para ${isProduction ? 'producción' : 'desarrollo'}`);

// Middleware de parsing JSON (después de CORS)
// Aumentar límites para permitir bases de datos grandes
app.use(express.json({ limit: '500mb' }));
app.use(express.urlencoded({ limit: '500mb', extended: true }));

// Middleware de logging automático para todas las requests
app.use(extractUsername);
app.use(requestLogger);
console.log('📝 Request logger activado para todos los endpoints');

// Aplicar validación de tokens SAP a rutas protegidas
if (isProduction) {
  console.log('🔐 Aplicando validación de tokens SAP a rutas protegidas...');
  app.use('/api', validateSAPToken);
} else {
  console.log('🔓 Modo desarrollo - autenticación deshabilitada para APIs');
}

// Configurar rutas de la API
app.use('/api/chat', chatRoutes);
app.use('/api/rag', ragRoutes);
app.use('/api/chat-history', chatHistoryRoutes);
// IMPORTANTE: Registrar rutas específicas ANTES de las rutas con parámetros dinámicos
app.use('/api/pdf-correction/errors', pliegoErrorsRoutes);
app.use('/api/pdf-correction', pdfCorrectionRoutes);
app.use('/api/analytics', analyticsRoutes);
app.use('/api/logs', logsRoutes);
app.use('/api/ai-core', aiCoreHealthRoutes);

// Rutas de autenticación SIEMPRE disponibles (incluso en desarrollo)
app.use('/oauth', authRoutes);

// Servir archivos estáticos de documentación
app.use('/docs', express.static('docs'));
app.use('/public', express.static('public'));

// Ruta de salud para Cloud Foundry
app.get('/health', (req, res) => {
  res.json({
    status: 'healthy',
    timestamp: new Date().toISOString(),
    environment: process.env.NODE_ENV || 'development',
    vectorStore: process.env.VECTOR_STORE_TYPE || 'auto'
  });
});

// Ruta raíz informativa
app.get('/', (req, res) => {
  res.json({
    service: 'AI Core API with RAG',
    version: '1.0.0',
    environment: process.env.NODE_ENV || 'development',
    endpoints: {
      health: '/health',
      chat: '/api/chat',
      rag: '/api/rag',
      ragHealth: '/api/rag/health',
      ragDocs: '/api/rag/documents',
      ragPliego: '/api/rag/process-pliego',
      pdfCorrection: '/api/pdf-correction',
      pdfCorrectionHealth: '/api/pdf-correction/health',
      pliegoErrors: {
        byDate: '/api/pdf-correction/errors/by-date?dateIni=YYYY-MM-DD&dateFin=YYYY-MM-DD',
        bySpecificDate: '/api/pdf-correction/errors/by-date/:date',
        groupedByDate: '/api/pdf-correction/errors/grouped-by-date?dateIni=YYYY-MM-DD&dateFin=YYYY-MM-DD',
        byType: '/api/pdf-correction/errors/by-type/:errorType',
        statistics: '/api/pdf-correction/errors/statistics',
        raw: '/api/pdf-correction/errors/raw',
        download: '/api/pdf-correction/errors/download'
      },
      analytics: '/api/analytics',
      analyticsValidation: '/api/analytics/validation',
      analyticsClassify: '/api/analytics/classify',
      analyticsCompare: '/api/analytics/compare-template',
      analyticsDashboard: '/api/analytics/dashboard-summary',
      logs: {
        view: '/api/logs',
        download: '/api/logs/download',
        stats: '/api/logs/stats',
        clear: 'DELETE /api/logs'
      },
      authToken: '/oauth/token',
      authLogout: '/oauth/logout',
      documentation: '/docs/RAG_SYSTEM_DOCUMENTATION.html'
    }
  });
});

// Inicializar datos de ejemplo en producción
async function startServer() {
  try {
    // Inicializar sistema de persistencia
    await persistenceManager.initialize();
    
    if (isProduction) {
      console.log('📄 Inicializando datos de ejemplo para Cloud Foundry...');
      // Esperar un poco para que los servicios se inicialicen
      setTimeout(async () => {
        try {
          await initializeSampleData();
        } catch (error) {
          console.warn('⚠️  Error inicializando datos de ejemplo:', error.message);
        }
      }, 5000);
    }

    app.listen(port, '0.0.0.0', () => {
      console.log(`🚀 AI Core API running on port ${port}`);
      console.log(`🌐 Health check: http://localhost:${port}/health`);
      console.log(`📚 RAG API: http://localhost:${port}/api/rag`);
      
      if (!isProduction) {
        console.log(`💡 Para usar ChromaDB local: cd chroma_service && start_service.bat`);
      }
    });

  } catch (error) {
    console.error('❌ Error starting server:', error);
    process.exit(1);
  }
}

startServer();
