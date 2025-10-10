// Script para probar conectividad y CORS
import fetch from 'node-fetch';

const API_BASE_URL = 'http://localhost:4000';

async function testCORS() {
  console.log('🧪 Probando conectividad y CORS...\n');
  
  const tests = [
    {
      name: 'Health Check',
      url: `${API_BASE_URL}/health`,
      method: 'GET'
    },
    {
      name: 'RAG Health',
      url: `${API_BASE_URL}/api/rag/health`,
      method: 'GET'
    },
    {
      name: 'OPTIONS Preflight',
      url: `${API_BASE_URL}/api/rag/health`,
      method: 'OPTIONS',
      headers: {
        'Origin': 'http://localhost:3000',
        'Access-Control-Request-Method': 'GET',
        'Access-Control-Request-Headers': 'Content-Type, Authorization'
      }
    }
  ];

  for (const test of tests) {
    try {
      console.log(`📡 ${test.name}...`);
      
      const response = await fetch(test.url, {
        method: test.method,
        headers: {
          'Origin': 'http://localhost:3000',
          'Content-Type': 'application/json',
          ...test.headers
        }
      });

      console.log(`   Status: ${response.status}`);
      console.log(`   CORS Headers:`);
      console.log(`     Access-Control-Allow-Origin: ${response.headers.get('access-control-allow-origin')}`);
      console.log(`     Access-Control-Allow-Methods: ${response.headers.get('access-control-allow-methods')}`);
      console.log(`     Access-Control-Allow-Headers: ${response.headers.get('access-control-allow-headers')}`);
      
      if (test.method !== 'OPTIONS') {
        const data = await response.text();
        console.log(`   Response: ${data.substring(0, 100)}${data.length > 100 ? '...' : ''}`);
      }
      
      console.log(`   ✅ ${test.name} OK\n`);
      
    } catch (error) {
      console.log(`   ❌ ${test.name} FAILED: ${error.message}\n`);
    }
  }
}

// Ejecutar test
testCORS().catch(console.error);
