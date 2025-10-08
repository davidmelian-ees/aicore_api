import fetch from 'node-fetch';

/**
 * Test simple para verificar que el sistema RAG funciona
 */

const BASE_URL = 'http://localhost:4000';
const API_BASE = `${BASE_URL}/api/rag`;

// Colores para la consola
const colors = {
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  reset: '\x1b[0m'
};

function log(message, color = 'reset') {
  console.log(`${colors[color]}${message}${colors.reset}`);
}

async function testServerHealth() {
  console.log('\n🧪 Test 1: Verificar servidor...');
  
  try {
    const response = await fetch(`${API_BASE}/health`);
    const data = await response.json();
    
    if (response.ok) {
      log('✅ Servidor RAG funcionando correctamente', 'green');
      console.log(`   Status: ${data.status}`);
      return true;
    } else {
      log('❌ Servidor no responde correctamente', 'red');
      return false;
    }
  } catch (error) {
    log(`❌ Error conectando al servidor: ${error.message}`, 'red');
    log('   Asegúrate de que el servidor esté corriendo en puerto 4000', 'yellow');
    return false;
  }
}

async function testGetStats() {
  console.log('\n🧪 Test 2: Obtener estadísticas...');
  
  try {
    const response = await fetch(`${API_BASE}/stats`);
    const data = await response.json();
    
    if (response.ok) {
      log('✅ Estadísticas obtenidas correctamente', 'green');
      console.log(`   Documentos: ${data.stats.totalDocuments}`);
      console.log(`   Chunks: ${data.stats.totalChunks}`);
      return data.stats;
    } else {
      log('❌ Error obteniendo estadísticas', 'red');
      return null;
    }
  } catch (error) {
    log(`❌ Error: ${error.message}`, 'red');
    return null;
  }
}

async function testListDocuments() {
  console.log('\n🧪 Test 3: Listar documentos...');
  
  try {
    const response = await fetch(`${API_BASE}/documents`);
    const data = await response.json();
    
    if (response.ok) {
      log('✅ Documentos listados correctamente', 'green');
      console.log(`   Total: ${data.count} documentos`);
      
      if (data.documents.length > 0) {
        console.log('   Documentos encontrados:');
        data.documents.forEach((doc, index) => {
          console.log(`   ${index + 1}. ${doc.fileName} (${doc.totalChunks} chunks)`);
        });
      }
      
      return data.documents;
    } else {
      log('❌ Error listando documentos', 'red');
      return null;
    }
  } catch (error) {
    log(`❌ Error: ${error.message}`, 'red');
    return null;
  }
}

async function testSearch() {
  console.log('\n🧪 Test 4: Buscar contexto...');
  
  try {
    const response = await fetch(`${API_BASE}/search`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        query: 'vacaciones políticas empresa',
        topK: 3
      })
    });
    
    const data = await response.json();
    
    if (response.ok) {
      log('✅ Búsqueda completada', 'green');
      console.log(`   Resultados encontrados: ${data.count}`);
      
      if (data.results.length > 0) {
        data.results.forEach((result, index) => {
          console.log(`   ${index + 1}. Similitud: ${result.similarity.toFixed(3)} - ${result.metadata.fileName}`);
        });
      } else {
        log('   No se encontraron resultados (necesitas subir documentos primero)', 'yellow');
      }
      
      return data.results;
    } else {
      log('❌ Error en búsqueda', 'red');
      return null;
    }
  } catch (error) {
    log(`❌ Error: ${error.message}`, 'red');
    return null;
  }
}

async function testChat() {
  console.log('\n🧪 Test 5: Chat RAG...');
  
  try {
    const response = await fetch(`${API_BASE}/chat`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        message: '¿Cuáles son las políticas de la empresa?',
        topK: 3,
        includeContext: true
      })
    });
    
    const data = await response.json();
    
    if (response.ok) {
      log('✅ Chat RAG funcionando', 'green');
      console.log(`   Chunks usados: ${data.metadata?.chunksUsed || 0}`);
      console.log(`   Fuentes: ${data.metadata?.sources?.join(', ') || 'Ninguna'}`);
      
      if (data.answer) {
        console.log('\n📝 Respuesta:');
        console.log(`   ${data.answer.substring(0, 200)}...`);
      }
      
      return data;
    } else {
      log('❌ Error en chat RAG', 'red');
      console.log(`   Error: ${data.error}`);
      return null;
    }
  } catch (error) {
    log(`❌ Error: ${error.message}`, 'red');
    return null;
  }
}

async function runTests() {
  console.log('🚀 Iniciando tests simples del sistema RAG');
  console.log(`🌐 URL: ${BASE_URL}`);
  
  let passed = 0;
  let total = 5;
  
  // Test 1: Server Health
  if (await testServerHealth()) passed++;
  
  // Test 2: Stats
  if (await testGetStats()) passed++;
  
  // Test 3: List Documents
  if (await testListDocuments()) passed++;
  
  // Test 4: Search
  if (await testSearch()) passed++;
  
  // Test 5: Chat
  if (await testChat()) passed++;
  
  // Resumen
  console.log('\n' + '='.repeat(50));
  console.log('📊 RESUMEN DE TESTS');
  console.log('='.repeat(50));
  log(`Tests ejecutados: ${total}`, 'blue');
  log(`Exitosos: ${passed}`, 'green');
  log(`Fallidos: ${total - passed}`, 'red');
  
  const successRate = ((passed / total) * 100).toFixed(1);
  log(`Tasa de éxito: ${successRate}%`, successRate > 80 ? 'green' : 'yellow');
  
  if (passed === total) {
    log('\n🎉 ¡Todos los tests pasaron!', 'green');
  } else {
    log('\n⚠️  Algunos tests fallaron', 'yellow');
    
    if (passed === 0) {
      log('\n💡 Sugerencias:', 'blue');
      log('   1. Verifica que el servidor esté corriendo: npm start', 'yellow');
      log('   2. Verifica que el puerto 4000 esté disponible', 'yellow');
      log('   3. Revisa los logs del servidor para errores', 'yellow');
    } else if (passed < 3) {
      log('\n💡 Sugerencias:', 'blue');
      log('   1. Sube algunos documentos primero para probar búsqueda y chat', 'yellow');
      log('   2. Usa: curl -X POST http://localhost:4000/api/rag/upload -F "document=@sample_documents/empresa_politicas.txt"', 'yellow');
    }
  }
}

// Ejecutar tests
runTests().catch(error => {
  log(`❌ Error ejecutando tests: ${error.message}`, 'red');
  process.exit(1);
});
