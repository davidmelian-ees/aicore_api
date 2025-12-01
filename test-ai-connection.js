/**
 * Test rápido de conexión SAP AI Core
 */

import { getAiCoreClient } from './auth/aiCoreClient.js';

console.log('🧪 Probando conexión con SAP AI Core...\n');

try {
  console.log('1️⃣ Inicializando cliente...');
  const client = getAiCoreClient('gpt-4o', { temperature: 0.2, maxTokens: 100 });
  console.log('✅ Cliente inicializado\n');
  
  console.log('2️⃣ Enviando prompt de prueba...');
  const response = await client.run({
    messages: [{ role: 'user', content: 'Di solo "OK"' }],
    temperature: 0.2,
    max_tokens: 100
  });
  
  const content = response.getContent();
  console.log('✅ Respuesta recibida:', content);
  console.log('\n✅ SAP AI Core funciona correctamente!\n');
  
} catch (error) {
  console.error('\n❌ ERROR en SAP AI Core:');
  console.error('Message:', error.message);
  console.error('Status:', error.status || error.code || 'unknown');
  
  if (error.message.includes('Failed to fetch the list of deployments')) {
    console.error('\n🔧 SOLUCIÓN:');
    console.error('1. Ve a BTP Cockpit → Instances and Subscriptions');
    console.error('2. Busca "default_aicore"');
    console.error('3. Service Keys → Create new key');
    console.error('4. Copia las credenciales a default-env.json');
    console.error('5. Reinicia el servidor\n');
  }
  
  process.exit(1);
}
