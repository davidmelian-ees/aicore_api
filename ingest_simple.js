import fs from 'fs/promises';
import path from 'path';

/**
 * Script simple para ingestar documentos usando curl
 * Ejecutar con: node ingest_simple.js
 */

const BASE_URL = 'http://localhost:4000';
const API_BASE = `${BASE_URL}/api/rag`;
const DOCUMENTS_FOLDER = './sample_documents';

// Colores para output en consola
const colors = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m'
};

function log(message, color = 'reset') {
  console.log(`${colors[color]}${message}${colors.reset}`);
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
    log(`❌ Error leyendo carpeta ${DOCUMENTS_FOLDER}: ${error.message}`, 'red');
    return [];
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
 * Genera comandos curl para subir documentos
 */
async function generateCurlCommands() {
  log('📁 Generando comandos para ingestar documentos', 'cyan');
  log(`📂 Carpeta: ${DOCUMENTS_FOLDER}`, 'blue');
  log(`🌐 Servidor: ${BASE_URL}`, 'blue');
  
  // Obtener archivos
  const documentFiles = await getDocumentFiles();
  
  if (documentFiles.length === 0) {
    log('⚠️  No se encontraron documentos para ingestar', 'yellow');
    log('ℹ️  Asegúrate de que la carpeta sample_documents contenga archivos .txt, .md, .json, .csv o .docx', 'blue');
    return;
  }
  
  log(`\nℹ️  Documentos encontrados: ${documentFiles.length}`, 'blue');
  documentFiles.forEach((doc, index) => {
    console.log(`  ${index + 1}. ${doc.name} (${doc.extension})`);
  });
  
  log('\n📋 Comandos curl para ejecutar:', 'cyan');
  log('=' .repeat(80), 'cyan');
  
  // Generar comandos curl
  documentFiles.forEach((doc, index) => {
    const tags = getTagsForDocument(doc);
    const curlCommand = `curl -X POST ${API_BASE}/upload \\
  -F "document=@${doc.path}" \\
  -F "uploadedBy=ingest_script" \\
  -F "tags=${tags}"`;
    
    console.log(`\n# ${index + 1}. Subir ${doc.name}`);
    console.log(curlCommand);
  });
  
  log('\n' + '=' .repeat(80), 'cyan');
  log('💡 Instrucciones:', 'yellow');
  log('1. Asegúrate de que el servidor esté corriendo: npm start', 'yellow');
  log('2. Copia y pega cada comando curl en la terminal', 'yellow');
  log('3. O ejecuta todos los comandos de una vez', 'yellow');
  
  // Generar script batch para Windows
  log('\n🪟 Para Windows, también puedes usar este archivo batch:', 'cyan');
  
  let batchContent = '@echo off\n';
  batchContent += 'echo Subiendo documentos al sistema RAG...\n';
  batchContent += 'echo.\n\n';
  
  documentFiles.forEach((doc, index) => {
    const tags = getTagsForDocument(doc);
    batchContent += `echo Subiendo ${doc.name}...\n`;
    batchContent += `curl -X POST ${API_BASE}/upload -F "document=@${doc.path}" -F "uploadedBy=ingest_script" -F "tags=${tags}"\n`;
    batchContent += 'echo.\n\n';
  });
  
  batchContent += 'echo Ingesta completada!\n';
  batchContent += 'pause\n';
  
  // Escribir archivo batch
  await fs.writeFile('./upload_documents.bat', batchContent);
  log('✅ Archivo upload_documents.bat creado', 'green');
  log('   Ejecuta: ./upload_documents.bat', 'yellow');
  
  // Generar script para verificar resultados
  log('\n📊 Para verificar los resultados:', 'cyan');
  console.log(`curl ${API_BASE}/documents`);
  console.log(`curl ${API_BASE}/stats`);
  
  // Generar comandos de prueba
  log('\n🧪 Comandos de prueba después de subir:', 'cyan');
  console.log(`# Buscar contexto sobre vacaciones`);
  console.log(`curl -X POST ${API_BASE}/search -H "Content-Type: application/json" -d '{"query":"vacaciones políticas empresa","topK":3}'`);
  
  console.log(`\n# Chat RAG sobre políticas`);
  console.log(`curl -X POST ${API_BASE}/chat -H "Content-Type: application/json" -d '{"message":"¿Cuáles son las políticas de vacaciones?","topK":3}'`);
}

/**
 * Función principal
 */
async function main() {
  try {
    await generateCurlCommands();
  } catch (error) {
    log(`❌ Error: ${error.message}`, 'red');
    process.exit(1);
  }
}

// Ejecutar si el script se ejecuta directamente
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

main()

export { generateCurlCommands };
