#!/usr/bin/env node

/**
 * Script para ingestar documentos desde una carpeta
 * Uso: node scripts/ingest-documents.js [carpeta]
 */

import { indexDocument } from '../services/ragService.js';
import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Tipos de archivos soportados
const SUPPORTED_EXTENSIONS = ['.txt', '.md', '.json', '.csv', '.docx'];

async function ingestDocuments(folderPath = '../sample_documents') {
  try {
    const docsPath = path.resolve(__dirname, folderPath);
    
    console.log(`📁 Ingiriendo documentos desde: ${docsPath}`);
    
    // Verificar que la carpeta existe
    try {
      await fs.access(docsPath);
    } catch {
      console.error(`❌ Carpeta no encontrada: ${docsPath}`);
      process.exit(1);
    }
    
    // Leer archivos de la carpeta
    const files = await fs.readdir(docsPath);
    const documentFiles = files.filter(file => {
      const ext = path.extname(file).toLowerCase();
      return SUPPORTED_EXTENSIONS.includes(ext);
    });
    
    if (documentFiles.length === 0) {
      console.log('⚠️  No se encontraron documentos soportados');
      console.log(`Extensiones soportadas: ${SUPPORTED_EXTENSIONS.join(', ')}`);
      return;
    }
    
    console.log(`📄 Encontrados ${documentFiles.length} documentos:`);
    documentFiles.forEach(file => console.log(`   - ${file}`));
    console.log('');
    
    let processed = 0;
    let errors = 0;
    
    // Procesar cada archivo
    for (const file of documentFiles) {
      const filePath = path.join(docsPath, file);
      
      try {
        console.log(`🔄 Procesando: ${file}`);
        
        // Determinar tipo MIME
        const ext = path.extname(file).toLowerCase();
        const mimeTypes = {
          '.txt': 'text/plain',
          '.md': 'text/markdown',
          '.json': 'application/json',
          '.csv': 'text/csv',
          '.docx': 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
        };
        
        const mimeType = mimeTypes[ext] || 'text/plain';
        
        // Metadatos del documento
        const metadata = {
          originalName: file,
          source: 'ingest_script',
          uploadedAt: new Date().toISOString(),
          fileSize: (await fs.stat(filePath)).size,
          extension: ext
        };
        
        // Indexar documento
        const result = await indexDocument(filePath, mimeType, metadata);
        
        console.log(`✅ ${file}: ${result.totalChunks} chunks indexados`);
        processed++;
        
      } catch (error) {
        console.error(`❌ Error procesando ${file}:`, error.message);
        errors++;
      }
    }
    
    // Resumen final
    console.log('\n' + '='.repeat(50));
    console.log('📊 Resumen de Ingesta:');
    console.log(`   ✅ Procesados: ${processed}`);
    console.log(`   ❌ Errores: ${errors}`);
    console.log(`   📁 Total archivos: ${documentFiles.length}`);
    
    if (processed > 0) {
      console.log('\n🎉 Ingesta completada exitosamente!');
      console.log('💡 Puedes probar el sistema con:');
      console.log('   - node tests/test_python_chroma.js');
      console.log('   - Postman collection en /postman/');
    }
    
  } catch (error) {
    console.error('❌ Error en ingesta:', error);
    process.exit(1);
  }
}

// Ejecutar si es llamado directamente
if (import.meta.url === `file://${process.argv[1]}`) {
  const folderPath = process.argv[2] || '../sample_documents';
  ingestDocuments(folderPath);
}

export { ingestDocuments };
