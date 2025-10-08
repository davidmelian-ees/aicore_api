import fetch from 'node-fetch';

/**
 * Script de prueba para el servicio ChromaDB Python
 * Ejecutar con: node test_python_chroma.js
 */

const CHROMA_SERVICE_URL = 'http://localhost:8001';
const RAG_API_URL = 'http://localhost:4000/api/rag';

// Colores para la consola
const colors = {
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
  reset: '\x1b[0m'
};

function log(message, color = 'reset') {
  console.log(`${colors[color]}${message}${colors.reset}`);
}

function logSection(title) {
  console.log('\n' + '='.repeat(60));
  log(`🧪 ${title}`, 'cyan');
  console.log('='.repeat(60));
}

async function testChromaService() {
  logSection('Test 1: Verificar Servicio ChromaDB Python');
  
  try {
    const response = await fetch(`${CHROMA_SERVICE_URL}/health`);
    const data = await response.json();
    
    if (response.ok && data.status === 'healthy') {
      log('✅ Servicio ChromaDB Python funcionando', 'green');
      console.log(`   Colección: ${data.collection_info.name}`);
      console.log(`   Documentos: ${data.collection_info.document_count}`);
      return true;
    } else {
      log('❌ Servicio ChromaDB Python no saludable', 'red');
      return false;
    }
  } catch (error) {
    log(`❌ Error conectando con ChromaDB Python: ${error.message}`, 'red');
    log('💡 Inicia el servicio: cd chroma_service && start_service.bat', 'yellow');
    return false;
  }
}

async function testRAGService() {
  logSection('Test 2: Verificar Servicio RAG Node.js');
  
  try {
    const response = await fetch(`${RAG_API_URL}/health`);
    const data = await response.json();
    
    if (response.ok && data.status === 'healthy') {
      log('✅ Servicio RAG Node.js funcionando', 'green');
      console.log(`   Documentos: ${data.summary.totalDocuments}`);
      console.log(`   Chunks: ${data.summary.totalChunks}`);
      return true;
    } else {
      log('❌ Servicio RAG Node.js no saludable', 'red');
      return false;
    }
  } catch (error) {
    log(`❌ Error conectando con RAG: ${error.message}`, 'red');
    log('💡 Inicia el servidor: npm start', 'yellow');
    return false;
  }
}

async function testDirectChromaAPI() {
  logSection('Test 3: API Directa ChromaDB Python');
  
  try {
    // Test agregar documento
    log('📄 Agregando documento de prueba...', 'blue');
    
    const testDoc = {
      id: 'test_python_doc_1',
      content: 'Este es un documento de prueba para ChromaDB Python Service',
      embedding: Array.from({length: 384}, () => Math.random() - 0.5),
      metadata: {
        fileName: 'test_python.txt',
        documentId: 'test_python_doc',
        chunkIndex: 0,
        testDocument: true
      }
    };
    
    const addResponse = await fetch(`${CHROMA_SERVICE_URL}/documents`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(testDoc)
    });
    
    if (addResponse.ok) {
      log('✅ Documento agregado correctamente', 'green');
    } else {
      const error = await addResponse.json();
      log(`❌ Error agregando documento: ${error.detail}`, 'red');
      return false;
    }
    
    // Test búsqueda
    log('🔍 Probando búsqueda...', 'blue');
    
    const searchResponse = await fetch(`${CHROMA_SERVICE_URL}/search`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        query_embedding: testDoc.embedding,
        top_k: 3
      })
    });
    
    if (searchResponse.ok) {
      const results = await searchResponse.json();
      log(`✅ Búsqueda exitosa: ${results.length} resultados`, 'green');
      
      if (results.length > 0) {
        console.log(`   Mejor resultado: ${results[0].similarity.toFixed(3)} similitud`);
      }
    } else {
      log('❌ Error en búsqueda', 'red');
      return false;
    }
    
    // Test estadísticas
    log('📊 Obteniendo estadísticas...', 'blue');
    
    const statsResponse = await fetch(`${CHROMA_SERVICE_URL}/stats`);
    if (statsResponse.ok) {
      const stats = await statsResponse.json();
      log('✅ Estadísticas obtenidas', 'green');
      console.log(`   Total documentos: ${stats.total_documents}`);
      console.log(`   Dimensión embedding: ${stats.embedding_dimension}`);
    }
    
    return true;
    
  } catch (error) {
    log(`❌ Error en pruebas directas: ${error.message}`, 'red');
    return false;
  }
}

async function testRAGIntegration() {
  logSection('Test 4: Integración RAG con ChromaDB Python');
  
  try {
    // Test estadísticas RAG
    log('📊 Verificando estadísticas RAG...', 'blue');
    
    const statsResponse = await fetch(`${RAG_API_URL}/stats`);
    if (statsResponse.ok) {
      const data = await statsResponse.json();
      log('✅ Estadísticas RAG obtenidas', 'green');
      console.log(`   Documentos: ${data.stats.totalDocuments}`);
      console.log(`   Chunks: ${data.stats.totalChunks}`);
      console.log(`   Dimensión: ${data.stats.embeddingDimension}`);
    } else {
      log('❌ Error obteniendo estadísticas RAG', 'red');
      return false;
    }
    
    // Test búsqueda RAG
    log('🔍 Probando búsqueda RAG...', 'blue');
    
    const searchResponse = await fetch(`${RAG_API_URL}/search`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        query: 'documento de prueba ChromaDB',
        topK: 3
      })
    });
    
    if (searchResponse.ok) {
      const data = await searchResponse.json();
      log(`✅ Búsqueda RAG exitosa: ${data.count} resultados`, 'green');
      
      if (data.results && data.results.length > 0) {
        console.log(`   Mejor resultado: ${data.results[0].similarity.toFixed(3)} similitud`);
      }
    } else {
      const error = await searchResponse.json();
      log(`❌ Error en búsqueda RAG: ${error.error}`, 'red');
    }
    
    return true;
    
  } catch (error) {
    log(`❌ Error en integración RAG: ${error.message}`, 'red');
    return false;
  }
}

async function testCleanup() {
  logSection('Test 5: Limpieza (Opcional)');
  
  try {
    log('🧹 ¿Limpiar datos de prueba? (Comentado por seguridad)', 'yellow');
    
    // Descomenta para limpiar datos de prueba
    /*
    const cleanResponse = await fetch(`${CHROMA_SERVICE_URL}/clear?confirm=DELETE_ALL`, {
      method: 'DELETE'
    });
    
    if (cleanResponse.ok) {
      log('✅ Datos de prueba limpiados', 'green');
    }
    */
    
    return true;
    
  } catch (error) {
    log(`❌ Error en limpieza: ${error.message}`, 'red');
    return false;
  }
}

async function runAllTests() {
  log('🚀 Iniciando tests del sistema ChromaDB Python + RAG', 'cyan');
  log(`🔗 ChromaDB Service: ${CHROMA_SERVICE_URL}`, 'blue');
  log(`🔗 RAG API: ${RAG_API_URL}`, 'blue');
  
  const tests = [
    { name: 'ChromaDB Service', fn: testChromaService },
    { name: 'RAG Service', fn: testRAGService },
    { name: 'Direct ChromaDB API', fn: testDirectChromaAPI },
    { name: 'RAG Integration', fn: testRAGIntegration },
    { name: 'Cleanup', fn: testCleanup }
  ];
  
  let passed = 0;
  let total = tests.length;
  
  for (const test of tests) {
    const result = await test.fn();
    if (result) passed++;
    
    // Pausa entre tests
    await new Promise(resolve => setTimeout(resolve, 1000));
  }
  
  // Resumen final
  logSection('Resumen de Tests');
  log(`📊 Tests ejecutados: ${total}`, 'blue');
  log(`✅ Exitosos: ${passed}`, 'green');
  log(`❌ Fallidos: ${total - passed}`, 'red');
  
  const successRate = ((passed / total) * 100).toFixed(1);
  log(`📈 Tasa de éxito: ${successRate}%`, successRate > 80 ? 'green' : 'yellow');
  
  if (passed === total) {
    log('\n🎉 ¡Todos los tests pasaron! Sistema completamente funcional', 'green');
    log('\n💡 Próximos pasos:', 'cyan');
    log('   1. Subir documentos reales con ingest_simple.js', 'yellow');
    log('   2. Probar chat RAG con documentos', 'yellow');
    log('   3. Verificar persistencia reiniciando servicios', 'yellow');
  } else {
    log('\n⚠️  Algunos tests fallaron', 'yellow');
    
    if (passed === 0) {
      log('\n💡 Verificar que ambos servicios estén corriendo:', 'blue');
      log('   1. ChromaDB Python: cd chroma_service && start_service.bat', 'yellow');
      log('   2. RAG Node.js: npm start', 'yellow');
    }
  }
}

// Ejecutar tests
runAllTests().catch(error => {
  log(`❌ Error ejecutando tests: ${error.message}`, 'red');
  process.exit(1);
});
