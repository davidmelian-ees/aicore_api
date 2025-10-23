import express from "express";
import { initAuth } from "./auth.js";
import chatRoutes from "./routes/chat.js";
import ragRoutes from "./routes/rag.js";
import pdfCorrectionRoutes from "./routes/pdfCorrection.js";
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
    // En producción, solo permitir el dominio específico del frontend
    if (origin === 'https://rag-dashboard-ui.cfapps.eu10-005.hana.ondemand.com') {
      res.header('Access-Control-Allow-Origin', origin);
      console.log(`[CORS] Permitido origen de producción: ${origin}`);
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

console.log(`🌐 CORS configurado para ${isProduction ? 'producción' : 'desarrollo'}`);

// Middleware de parsing JSON (después de CORS)
app.use(express.json());

// Inicializar autenticación (después de CORS y JSON)
if (isProduction) {
  console.log('🔐 Inicializando autenticación para producción...');
  initAuth(app);
} else {
  console.log('🔓 Modo desarrollo - autenticación deshabilitada');
}

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
