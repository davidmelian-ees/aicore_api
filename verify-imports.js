#!/usr/bin/env node

/**
 * Script para verificar que todas las importaciones funcionan
 */

console.log('🔍 Verificando importaciones...');

try {
  console.log('✅ Verificando server.js...');
  await import('./server.js');
  console.log('✅ server.js - OK');
} catch (error) {
  console.error('❌ Error en server.js:', error.message);
}

try {
  console.log('✅ Verificando scripts/init-sample-data.js...');
  await import('./scripts/init-sample-data.js');
  console.log('✅ init-sample-data.js - OK');
} catch (error) {
  console.error('❌ Error en init-sample-data.js:', error.message);
}

try {
  console.log('✅ Verificando scripts/ingest-documents.js...');
  await import('./scripts/ingest-documents.js');
  console.log('✅ ingest-documents.js - OK');
} catch (error) {
  console.error('❌ Error en ingest-documents.js:', error.message);
}

console.log('🎉 Verificación completada');
