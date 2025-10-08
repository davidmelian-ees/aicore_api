import fs from 'fs/promises';
import path from 'path';
import fetch from 'node-fetch';
import { fileFromPath } from 'formdata-node/file-from-path';
import { FormData } from 'formdata-node';

/**
 * Script para ingestar automáticamente todos los documentos de sample_documents
 * Ejecutar con: node ingest_documents.js
 */

const BASE_URL = 'http://localhost:4000';
const API_BASE = `${BASE_URL}/api/rag`;
const DOCUMENTS_FOLDER = './sample_documents';

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
  log(`📁 ${title}`, 'cyan');
  console.log('='.repeat(60));
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

function logWarning(message) {
  log(`⚠️  ${message}`, 'yellow');
}

/**
 * Verifica que el servidor esté corriendo
 */
async function checkServerHealth() {
  try {
    const response = await fetch(`${API_BASE}/health`);
    const data = await response.json();
    
    if (response.ok && data.status === 'healthy') {
      logSuccess('Servidor RAG está corriendo y saludable');
      return true;
    } else {
      logError('Servidor RAG no está saludable');
      return false;
    }
  } catch (error) {
    logError(`No se puede conectar al servidor: ${error.message}`);
    logWarning('Asegúrate de que el servidor esté corriendo con: npm start');
    return false;
  }
}

/**
 * Obtiene la lista de archivos en la carpeta sample_documents
 */
async function getDocumentFiles() {
  try {
    const files = await fs.readdir(DOCUMENTS_FOLDER);
    
    // Filtrar solo archivos soportados
    const supportedExtensions = ['.txt', '.md', '.json', '.csv', '.docx'];
    const documentFiles = files.filter(file => {
      const ext = path.extname(file).toLowerCase();
      return supportedExtensions.includes(ext);
    });
    
    return documentFiles.map(file => ({
      name: file,
      path: path.join(DOCUMENTS_FOLDER, file),
      extension: path.extname(file).toLowerCase()
    }));
    
  } catch (error) {
    logError(`Error leyendo carpeta ${DOCUMENTS_FOLDER}: ${error.message}`);
    return [];
  }
}

/**
 * Sube un documento al sistema RAG
 */
async function uploadDocument(documentInfo) {
  try {
    logInfo(`Subiendo: ${documentInfo.name}`);
    
    // Crear FormData
    const formData = new FormData();
    
    // Agregar archivo
    const file = await fileFromPath(documentInfo.path);
    formData.append('document', file);
    
    // Agregar metadatos
    formData.append('uploadedBy', 'ingest_script');
    formData.append('tags', getTagsForDocument(documentInfo));
    
    // Hacer request
    const response = await fetch(`${API_BASE}/upload`, {
      method: 'POST',
      body: formData
    });
    
    const data = await response.json();
    
    if (response.ok) {
      logSuccess(`${documentInfo.name} subido correctamente`);
      logInfo(`  Document ID: ${data.document.documentId}`);
      logInfo(`  Chunks: ${data.document.totalChunks}`);
      logInfo(`  Tamaño: ${data.document.fileSize} bytes`);
      
      return {
        success: true,
        document: data.document,
        fileName: documentInfo.name
      };
    } else {
      logError(`Error subiendo ${documentInfo.name}: ${data.error}`);
      return {
        success: false,
        fileName: documentInfo.name,
        error: data.error
      };
    }
    
  } catch (error) {
    logError(`Error procesando ${documentInfo.name}: ${error.message}`);
    return {
      success: false,
      fileName: documentInfo.name,
      error: error.message
    };
  }
}

/**
 * Genera tags apropiados basados en el nombre del archivo
 */
function getTagsForDocument(documentInfo) {
  const fileName = documentInfo.name.toLowerCase();
  const tags = [];
  
  // Tags basados en el nombre del archivo
  if (fileName.includes('politicas') || fileName.includes('empresa')) {
    tags.push('políticas', 'empresa', 'rrhh');
  }
  
  if (fileName.includes('manual') || fileName.includes('tecnico')) {
    tags.push('manual', 'técnico', 'desarrollo');
  }
  
  if (fileName.includes('seguridad') || fileName.includes('procedimientos')) {
    tags.push('seguridad', 'procedimientos', 'IT');
  }
  
  // Tags basados en la extensión
  switch (documentInfo.extension) {
    case '.txt':
      tags.push('texto');
      break;
    case '.md':
      tags.push('markdown', 'documentación');
      break;
    case '.json':
      tags.push('json', 'estructurado');
      break;
    case '.csv':
      tags.push('csv', 'datos');
      break;
    case '.docx':
      tags.push('word', 'documento');
      break;
  }
  
  return tags.join(',');
}

/**
 * Obtiene estadísticas del sistema después de la ingesta
 */
async function getSystemStats() {
  try {
    const response = await fetch(`${API_BASE}/stats`);
    const data = await response.json();
    
    if (response.ok) {
      return data.stats;
    } else {
      logWarning('No se pudieron obtener estadísticas del sistema');
      return null;
    }
  } catch (error) {
    logWarning(`Error obteniendo estadísticas: ${error.message}`);
    return null;
  }
}

/**
 * Lista los documentos después de la ingesta
 */
async function listUploadedDocuments() {
  try {
    const response = await fetch(`${API_BASE}/documents`);
    const data = await response.json();
    
    if (response.ok) {
      return data.documents;
    } else {
      logWarning('No se pudieron listar los documentos');
      return [];
    }
  } catch (error) {
    logWarning(`Error listando documentos: ${error.message}`);
    return [];
  }
}

/**
 * Función principal de ingesta
 */
async function ingestAllDocuments() {
  logSection('Iniciando Ingesta de Documentos');
  log(`📂 Carpeta: ${DOCUMENTS_FOLDER}`, 'blue');
  log(`🌐 Servidor: ${BASE_URL}`, 'blue');
  
  // 1. Verificar servidor
  logSection('Verificando Servidor');
  const serverOk = await checkServerHealth();
  if (!serverOk) {
    log('\n❌ No se puede continuar sin servidor activo', 'red');
    process.exit(1);
  }
  
  // 2. Obtener archivos
  logSection('Escaneando Documentos');
  const documentFiles = await getDocumentFiles();
  
  if (documentFiles.length === 0) {
    logWarning('No se encontraron documentos para ingestar');
    logInfo('Asegúrate de que la carpeta sample_documents contenga archivos .txt, .md, .json, .csv o .docx');
    return;
  }
  
  logInfo(`Documentos encontrados: ${documentFiles.length}`);
  documentFiles.forEach((doc, index) => {
    console.log(`  ${index + 1}. ${doc.name} (${doc.extension})`);
  });
  
  // 3. Subir documentos
  logSection('Subiendo Documentos');
  const results = [];
  
  for (let i = 0; i < documentFiles.length; i++) {
    const doc = documentFiles[i];
    console.log(`\n📄 Procesando ${i + 1}/${documentFiles.length}: ${doc.name}`);
    
    const result = await uploadDocument(doc);
    results.push(result);
    
    // Pausa entre uploads para no sobrecargar el servidor
    if (i < documentFiles.length - 1) {
      await new Promise(resolve => setTimeout(resolve, 1000));
    }
  }
  
  // 4. Resumen de resultados
  logSection('Resumen de Ingesta');
  
  const successful = results.filter(r => r.success);
  const failed = results.filter(r => !r.success);
  
  logInfo(`Total procesados: ${results.length}`);
  logSuccess(`Exitosos: ${successful.length}`);
  
  if (failed.length > 0) {
    logError(`Fallidos: ${failed.length}`);
    failed.forEach(fail => {
      console.log(`  ❌ ${fail.fileName}: ${fail.error}`);
    });
  }
  
  // 5. Estadísticas del sistema
  if (successful.length > 0) {
    logSection('Estadísticas del Sistema');
    const stats = await getSystemStats();
    
    if (stats) {
      logInfo(`Documentos totales: ${stats.totalDocuments}`);
      logInfo(`Chunks totales: ${stats.totalChunks}`);
      logInfo(`Dimensión embeddings: ${stats.embeddingDimension}`);
      
      if (stats.integrity?.isValid) {
        logSuccess('Integridad del sistema: OK');
      } else {
        logWarning('Integridad del sistema: Revisar');
      }
    }
    
    // 6. Listar documentos subidos
    logSection('Documentos en el Sistema');
    const documents = await listUploadedDocuments();
    
    if (documents.length > 0) {
      documents.forEach((doc, index) => {
        console.log(`  ${index + 1}. ${doc.fileName}`);
        console.log(`     ID: ${doc.documentId}`);
        console.log(`     Chunks: ${doc.totalChunks}`);
        console.log(`     Subido: ${new Date(doc.indexedAt).toLocaleString()}`);
      });
    }
  }
  
  // 7. Mensaje final
  logSection('Ingesta Completada');
  
  if (successful.length === results.length) {
    logSuccess('🎉 ¡Todos los documentos fueron ingestados exitosamente!');
    log('\n💡 Próximos pasos:', 'cyan');
    log('   1. Probar búsqueda: node test_simple.js', 'yellow');
    log('   2. Hacer preguntas sobre los documentos', 'yellow');
    log('   3. Usar los endpoints de chat RAG', 'yellow');
  } else if (successful.length > 0) {
    logWarning('⚠️  Ingesta parcialmente exitosa');
    log('\n💡 Algunos documentos fallaron, pero puedes usar los que se subieron correctamente', 'yellow');
  } else {
    logError('❌ No se pudo ingestar ningún documento');
    log('\n💡 Revisa los errores arriba y verifica:', 'yellow');
    log('   1. Que el servidor esté corriendo', 'yellow');
    log('   2. Que los archivos sean del formato correcto', 'yellow');
    log('   3. Que no haya problemas de permisos', 'yellow');
  }
}

// Ejecutar ingesta si el script se ejecuta directamente
if (import.meta.url === `file://${process.argv[1]}`) {
  ingestAllDocuments().catch(error => {
    logError(`Error ejecutando ingesta: ${error.message}`);
    process.exit(1);
  });
}

export { ingestAllDocuments };
