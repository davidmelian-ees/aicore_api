import express from "express";
import { initAuth } from "./auth.js";
import chatRoutes from "./routes/chat.js";
import ragRoutes from "./routes/rag.js";
import { initializeSampleData } from "./init-sample-data.js";

const app = express();

// Configuración para Cloud Foundry
const isProduction = process.env.NODE_ENV === 'production';
const port = process.env.PORT || 4000;

console.log(`🌍 Entorno: ${isProduction ? 'Producción (Cloud Foundry)' : 'Desarrollo'}`);
console.log(`📊 Vector Store: ${process.env.VECTOR_STORE_TYPE || 'auto'}`);

// Inicializar autenticación
if (isProduction) {
  console.log('🔐 Inicializando autenticación para producción...');
  initAuth(app);
} else {
  console.log('🔓 Modo desarrollo - autenticación deshabilitada');
}

app.use(express.json());

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
      ragDocs: '/api/rag/documents'
    },
    vectorStore: process.env.VECTOR_STORE_TYPE || 'auto'
  });
});

// Rutas principales
app.use("/api", chatRoutes);
app.use("/api/rag", ragRoutes);

// Inicializar datos de ejemplo en producción
async function startServer() {
  try {
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