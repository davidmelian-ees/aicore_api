import fs from 'fs/promises';
import path from 'path';
import fetch from 'node-fetch';

/**
 * Script de prueba para todos los endpoints del sistema RAG
 * Ejecutar con: node test_rag.js
 */

const BASE_URL = 'http://localhost:4000';
const API_BASE = `${BASE_URL}/api/rag`;

// Colores para output en consola
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m'
};

function log(message, color = 'reset') {
  console.log(`${colors[color]}${message}${colors.reset}`);
}

function logSection(title) {
  console.log('\n' + '='.repeat(60));
  log(`🧪 ${title}`, 'cyan');
  console.log('='.repeat(60));
}

function logTest(testName) {
  log(`\n📋 Test: ${testName}`, 'yellow');
}

function logSuccess(message) {
  log(`✅ ${message}`, 'green');
}

function logError(message) {
  log(`❌ ${message}`, 'red');
}

function logInfo(message) {
  log(`ℹ️  ${message}`, 'blue');
}

/**
 * Función helper para hacer requests HTTP
 */
async function makeRequest(url, options = {}) {
  try {
    const response = await fetch(url, {
      headers: {
        'Content-Type': 'application/json',
        ...options.headers
      },
      ...options
    });

    const data = await response.json();
    
    return {
      ok: response.ok,
      status: response.status,
      data
    };
  } catch (error) {
    return {
      ok: false,
      status: 0,
      error: error.message
    };
  }
}

/**
 * Función helper para subir archivos
 */
async function uploadFile(filePath, additionalFields = {}) {
  try {
    // Importar FormData dinámicamente
    const { FormData } = await import('formdata-node');
    const { fileFromPath } = await import('formdata-node/file-from-path');
    
    // Crear FormData
    const formData = new FormData();
    
    // Agregar archivo
    const file = await fileFromPath(filePath);
    formData.append('document', file);
    
    // Agregar campos adicionales
    Object.entries(additionalFields).forEach(([key, value]) => {
      formData.append(key, value);
    });

    const response = await fetch(`${API_BASE}/upload`, {
      method: 'POST',
      body: formData
    });

    const data = await response.json();
    
    return {
      ok: response.ok,
      status: response.status,
      data
    };
  } catch (error) {
    return {
      ok: false,
      status: 0,
      error: error.message
    };
  }
}

/**
 * Obtener content type basado en extensión
 */
function getContentType(fileName) {
  const ext = path.extname(fileName).toLowerCase();
  const types = {
    '.txt': 'text/plain',
    '.md': 'text/markdown',
    '.json': 'application/json',
    '.pdf': 'application/pdf',
    '.docx': 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  };
  return types[ext] || 'text/plain';
}

/**
 * Esperar un tiempo determinado
 */
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Test 1: Verificar que el servidor esté corriendo
 */
async function testServerHealth() {
  logTest('Verificar estado del servidor');
  
  const result = await makeRequest(`${API_BASE}/health`);
  
  if (result.ok) {
    logSuccess('Servidor RAG está corriendo correctamente');
    logInfo(`Status: ${result.data.status}`);
    return true;
  } else {
    logError(`Servidor no disponible: ${result.error || result.data?.error}`);
    return false;
  }
}

/**
 * Test 2: Obtener estadísticas iniciales
 */
async function testInitialStats() {
  logTest('Obtener estadísticas iniciales');
  
  const result = await makeRequest(`${API_BASE}/stats`);
  
  if (result.ok) {
    logSuccess('Estadísticas obtenidas correctamente');
    logInfo(`Documentos: ${result.data.stats.totalDocuments}`);
    logInfo(`Chunks: ${result.data.stats.totalChunks}`);
    return result.data.stats;
  } else {
    logError(`Error obteniendo estadísticas: ${result.data?.error}`);
    return null;
  }
}

/**
 * Test 3: Listar documentos (debería estar vacío inicialmente)
 */
async function testListEmptyDocuments() {
  logTest('Listar documentos (vacío)');
  
  const result = await makeRequest(`${API_BASE}/documents`);
  
  if (result.ok) {
    logSuccess(`Documentos listados: ${result.data.count} documentos`);
    if (result.data.count === 0) {
      logInfo('Lista vacía como se esperaba');
    }
    return result.data.documents;
  } else {
    logError(`Error listando documentos: ${result.data?.error}`);
    return null;
  }
}

/**
 * Test 4: Subir documento de políticas de empresa
 */
async function testUploadPoliciesDocument() {
  logTest('Subir documento de políticas de empresa');
  
  const filePath = './sample_documents/empresa_politicas.txt';
  
  try {
    // Verificar que el archivo existe
    await fs.access(filePath);
    
    const result = await uploadFile(filePath, {
      uploadedBy: 'test_user',
      tags: 'políticas,empresa,rrhh'
    });
    
    if (result.ok) {
      logSuccess('Documento de políticas subido correctamente');
      logInfo(`Document ID: ${result.data.document.documentId}`);
      logInfo(`Chunks: ${result.data.document.totalChunks}`);
      logInfo(`Tamaño: ${result.data.document.fileSize} bytes`);
      return result.data.document;
    } else {
      logError(`Error subiendo documento: ${result.data?.error}`);
      return null;
    }
  } catch (error) {
    logError(`Archivo no encontrado: ${filePath}`);
    return null;
  }
}

/**
 * Test 5: Subir documento técnico
 */
async function testUploadTechnicalDocument() {
  logTest('Subir manual técnico');
  
  const filePath = './sample_documents/manual_tecnico.md';
  
  try {
    await fs.access(filePath);
    
    const result = await uploadFile(filePath, {
      uploadedBy: 'test_user',
      tags: 'técnico,manual,desarrollo'
    });
    
    if (result.ok) {
      logSuccess('Manual técnico subido correctamente');
      logInfo(`Document ID: ${result.data.document.documentId}`);
      logInfo(`Chunks: ${result.data.document.totalChunks}`);
      return result.data.document;
    } else {
      logError(`Error subiendo manual: ${result.data?.error}`);
      return null;
    }
  } catch (error) {
    logError(`Archivo no encontrado: ${filePath}`);
    return null;
  }
}

/**
 * Test 6: Subir documento de seguridad
 */
async function testUploadSecurityDocument() {
  logTest('Subir procedimientos de seguridad');
  
  const filePath = './sample_documents/procedimientos_seguridad.json';
  
  try {
    await fs.access(filePath);
    
    const result = await uploadFile(filePath, {
      uploadedBy: 'test_user',
      tags: 'seguridad,procedimientos,IT'
    });
    
    if (result.ok) {
      logSuccess('Procedimientos de seguridad subidos correctamente');
      logInfo(`Document ID: ${result.data.document.documentId}`);
      logInfo(`Chunks: ${result.data.document.totalChunks}`);
      return result.data.document;
    } else {
      logError(`Error subiendo procedimientos: ${result.data?.error}`);
      return null;
    }
  } catch (error) {
    logError(`Archivo no encontrado: ${filePath}`);
    return null;
  }
}

/**
 * Test 7: Listar documentos después de subir
 */
async function testListDocumentsAfterUpload() {
  logTest('Listar documentos después de subir');
  
  const result = await makeRequest(`${API_BASE}/documents`);
  
  if (result.ok) {
    logSuccess(`Documentos encontrados: ${result.data.count}`);
    result.data.documents.forEach((doc, index) => {
      logInfo(`${index + 1}. ${doc.fileName} (${doc.totalChunks} chunks)`);
    });
    return result.data.documents;
  } else {
    logError(`Error listando documentos: ${result.data?.error}`);
    return null;
  }
}

/**
 * Test 8: Obtener información de un documento específico
 */
async function testGetDocumentInfo(documentId) {
  logTest(`Obtener información del documento: ${documentId}`);
  
  const result = await makeRequest(`${API_BASE}/documents/${documentId}`);
  
  if (result.ok) {
    logSuccess('Información del documento obtenida');
    logInfo(`Archivo: ${result.data.document.fileName}`);
    logInfo(`Chunks: ${result.data.document.totalChunks}`);
    logInfo(`Tamaño: ${result.data.document.fileSize} bytes`);
    return result.data.document;
  } else {
    logError(`Error obteniendo info del documento: ${result.data?.error}`);
    return null;
  }
}

/**
 * Test 9: Buscar contexto sobre vacaciones
 */
async function testSearchVacations() {
  logTest('Buscar contexto sobre vacaciones');
  
  const result = await makeRequest(`${API_BASE}/search`, {
    method: 'POST',
    body: JSON.stringify({
      query: 'vacaciones días libres permisos',
      topK: 3
    })
  });
  
  if (result.ok) {
    logSuccess(`Contexto encontrado: ${result.data.count} resultados`);
    result.data.results.forEach((item, index) => {
      logInfo(`${index + 1}. Similitud: ${item.similarity.toFixed(3)} - ${item.metadata.fileName}`);
    });
    return result.data.results;
  } else {
    logError(`Error buscando contexto: ${result.data?.error}`);
    return null;
  }
}

/**
 * Test 10: Buscar contexto sobre instalación técnica
 */
async function testSearchInstallation() {
  logTest('Buscar contexto sobre instalación');
  
  const result = await makeRequest(`${API_BASE}/search`, {
    method: 'POST',
    body: JSON.stringify({
      query: 'instalación configuración setup sistema',
      topK: 3
    })
  });
  
  if (result.ok) {
    logSuccess(`Contexto encontrado: ${result.data.count} resultados`);
    result.data.results.forEach((item, index) => {
      logInfo(`${index + 1}. Similitud: ${item.similarity.toFixed(3)} - ${item.metadata.fileName}`);
    });
    return result.data.results;
  } else {
    logError(`Error buscando contexto: ${result.data?.error}`);
    return null;
  }
}

/**
 * Test 11: Chat RAG sobre políticas de vacaciones
 */
async function testChatVacations() {
  logTest('Chat RAG: Preguntar sobre vacaciones');
  
  const result = await makeRequest(`${API_BASE}/chat`, {
    method: 'POST',
    body: JSON.stringify({
      message: '¿Cuántos días de vacaciones tengo al año y qué otros permisos están disponibles?',
      topK: 5,
      includeContext: true
    })
  });
  
  if (result.ok) {
    logSuccess('Respuesta RAG generada correctamente');
    logInfo(`Chunks usados: ${result.data.metadata.chunksUsed}`);
    logInfo(`Fuentes: ${result.data.metadata.sources.join(', ')}`);
    log('\n📝 Respuesta:', 'magenta');
    console.log(result.data.answer);
    return result.data;
  } else {
    logError(`Error en chat RAG: ${result.data?.error}`);
    return null;
  }
}

/**
 * Test 12: Chat RAG sobre instalación del sistema
 */
async function testChatInstallation() {
  logTest('Chat RAG: Preguntar sobre instalación');
  
  const result = await makeRequest(`${API_BASE}/chat`, {
    method: 'POST',
    body: JSON.stringify({
      message: '¿Cómo instalo el sistema de gestión de inventario? ¿Qué prerrequisitos necesito?',
      topK: 5,
      includeContext: true
    })
  });
  
  if (result.ok) {
    logSuccess('Respuesta RAG generada correctamente');
    logInfo(`Chunks usados: ${result.data.metadata.chunksUsed}`);
    logInfo(`Fuentes: ${result.data.metadata.sources.join(', ')}`);
    log('\n📝 Respuesta:', 'magenta');
    console.log(result.data.answer);
    return result.data;
  } else {
    logError(`Error en chat RAG: ${result.data?.error}`);
    return null;
  }
}

/**
 * Test 13: Chat RAG sobre procedimientos de seguridad
 */
async function testChatSecurity() {
  logTest('Chat RAG: Preguntar sobre seguridad');
  
  const result = await makeRequest(`${API_BASE}/chat`, {
    method: 'POST',
    body: JSON.stringify({
      message: '¿Cuál es el procedimiento para reportar un incidente de seguridad crítico?',
      topK: 5,
      includeContext: true
    })
  });
  
  if (result.ok) {
    logSuccess('Respuesta RAG generada correctamente');
    logInfo(`Chunks usados: ${result.data.metadata.chunksUsed}`);
    logInfo(`Fuentes: ${result.data.metadata.sources.join(', ')}`);
    log('\n📝 Respuesta:', 'magenta');
    console.log(result.data.answer);
    return result.data;
  } else {
    logError(`Error en chat RAG: ${result.data?.error}`);
    return null;
  }
}

/**
 * Test 14: Obtener estadísticas finales
 */
async function testFinalStats() {
  logTest('Obtener estadísticas finales');
  
  const result = await makeRequest(`${API_BASE}/stats`);
  
  if (result.ok) {
    logSuccess('Estadísticas finales obtenidas');
    const stats = result.data.stats;
    logInfo(`Documentos totales: ${stats.totalDocuments}`);
    logInfo(`Chunks totales: ${stats.totalChunks}`);
    logInfo(`Dimensión embeddings: ${stats.embeddingDimension}`);
    logInfo(`Integridad: ${stats.integrity?.isValid ? 'OK' : 'ERROR'}`);
    return stats;
  } else {
    logError(`Error obteniendo estadísticas: ${result.data?.error}`);
    return null;
  }
}

/**
 * Test 15: Eliminar un documento
 */
async function testDeleteDocument(documentId) {
  logTest(`Eliminar documento: ${documentId}`);
  
  const result = await makeRequest(`${API_BASE}/documents/${documentId}`, {
    method: 'DELETE'
  });
  
  if (result.ok) {
    logSuccess('Documento eliminado correctamente');
    logInfo(`Chunks eliminados: ${result.data.result.chunksDeleted}`);
    return result.data.result;
  } else {
    logError(`Error eliminando documento: ${result.data?.error}`);
    return null;
  }
}

/**
 * Test 16: Intentar subir archivo no soportado
 */
async function testUploadUnsupportedFile() {
  logTest('Intentar subir archivo no soportado');
  
  // Crear un archivo temporal no soportado
  const tempFile = './temp_test.xyz';
  await fs.writeFile(tempFile, 'Contenido de prueba');
  
  try {
    const result = await uploadFile(tempFile);
    
    if (!result.ok) {
      logSuccess('Error esperado para archivo no soportado');
      logInfo(`Error: ${result.data?.error}`);
    } else {
      logError('Se esperaba un error para archivo no soportado');
    }
  } finally {
    // Limpiar archivo temporal
    try {
      await fs.unlink(tempFile);
    } catch (e) {
      // Ignorar error de limpieza
    }
  }
}

/**
 * Función principal que ejecuta todos los tests
 */
async function runAllTests() {
  log('🚀 Iniciando tests del sistema RAG', 'bright');
  log(`🌐 URL Base: ${BASE_URL}`, 'blue');
  
  const results = {
    passed: 0,
    failed: 0,
    total: 0
  };
  
  const tests = [
    { name: 'Server Health', fn: testServerHealth },
    { name: 'Initial Stats', fn: testInitialStats },
    { name: 'List Empty Documents', fn: testListEmptyDocuments },
    { name: 'Upload Policies Document', fn: testUploadPoliciesDocument },
    { name: 'Upload Technical Document', fn: testUploadTechnicalDocument },
    { name: 'Upload Security Document', fn: testUploadSecurityDocument }
  ];
  
  // Variables para almacenar datos entre tests
  let uploadedDocuments = [];
  
  // Ejecutar tests básicos
  for (const test of tests) {
    logSection(test.name);
    results.total++;
    
    try {
      const result = await test.fn();
      if (result !== null && result !== false) {
        results.passed++;
        if (test.name.includes('Upload') && result?.documentId) {
          uploadedDocuments.push(result);
        }
      } else {
        results.failed++;
      }
    } catch (error) {
      logError(`Test falló con excepción: ${error.message}`);
      results.failed++;
    }
    
    // Pausa entre tests
    await sleep(1000);
  }
  
  // Tests que dependen de documentos subidos
  if (uploadedDocuments.length > 0) {
    const additionalTests = [
      { name: 'List Documents After Upload', fn: testListDocumentsAfterUpload },
      { name: 'Get Document Info', fn: () => testGetDocumentInfo(uploadedDocuments[0].documentId) },
      { name: 'Search Vacations', fn: testSearchVacations },
      { name: 'Search Installation', fn: testSearchInstallation },
      { name: 'Chat Vacations', fn: testChatVacations },
      { name: 'Chat Installation', fn: testChatInstallation },
      { name: 'Chat Security', fn: testChatSecurity },
      { name: 'Final Stats', fn: testFinalStats },
      { name: 'Upload Unsupported File', fn: testUploadUnsupportedFile }
    ];
    
    for (const test of additionalTests) {
      logSection(test.name);
      results.total++;
      
      try {
        const result = await test.fn();
        if (result !== null && result !== false) {
          results.passed++;
        } else {
          results.failed++;
        }
      } catch (error) {
        logError(`Test falló con excepción: ${error.message}`);
        results.failed++;
      }
      
      await sleep(1500); // Pausa más larga para tests de IA
    }
    
    // Test opcional de eliminación
    if (uploadedDocuments.length > 1) {
      logSection('Delete Document Test');
      results.total++;
      
      try {
        const result = await testDeleteDocument(uploadedDocuments[1].documentId);
        if (result !== null) {
          results.passed++;
        } else {
          results.failed++;
        }
      } catch (error) {
        logError(`Test falló con excepción: ${error.message}`);
        results.failed++;
      }
    }
  }
  
  // Resumen final
  logSection('Resumen de Tests');
  log(`📊 Tests ejecutados: ${results.total}`, 'bright');
  log(`✅ Exitosos: ${results.passed}`, 'green');
  log(`❌ Fallidos: ${results.failed}`, 'red');
  
  const successRate = ((results.passed / results.total) * 100).toFixed(1);
  log(`📈 Tasa de éxito: ${successRate}%`, successRate > 80 ? 'green' : 'yellow');
  
  if (results.failed === 0) {
    log('\n🎉 ¡Todos los tests pasaron exitosamente!', 'green');
  } else {
    log('\n⚠️  Algunos tests fallaron. Revisar logs arriba.', 'yellow');
  }
}

// Ejecutar tests si el script se ejecuta directamente
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllTests().catch(error => {
    logError(`Error ejecutando tests: ${error.message}`);
    process.exit(1);
  });
}

export { runAllTests };
