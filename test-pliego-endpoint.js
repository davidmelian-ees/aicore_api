// Script para probar el endpoint de procesamiento de pliegos
import fetch from 'node-fetch';
import FormData from 'form-data';
import fs from 'fs';
import path from 'path';

const API_BASE_URL = 'http://localhost:4000';

async function testPliegoEndpoint() {
  console.log('🧪 Probando endpoint /api/rag/process-pliego...\n');

  // Crear un archivo PDF de prueba (simulado con texto)
  const testContent = `
PLIEGO DE CONDICIONES TÉCNICAS

1. OBJETO DEL CONTRATO
El presente pliego tiene por objeto establecer las condiciones técnicas para la contratación de servicios de desarrollo de software.

2. REQUISITOS TÉCNICOS
- Lenguaje de programación: JavaScript/Node.js
- Base de datos: PostgreSQL o MongoDB
- Framework frontend: React o Vue.js
- Documentación técnica completa

3. PRESUPUESTO
El presupuesto máximo es de 50.000€ IVA incluido.

4. PLAZO DE EJECUCIÓN
El plazo máximo de ejecución será de 6 meses desde la firma del contrato.

5. CRITERIOS DE EVALUACIÓN
- Propuesta técnica: 60%
- Propuesta económica: 30%
- Experiencia del equipo: 10%
`;

  // Crear archivo temporal para la prueba
  const tempFilePath = path.join(process.cwd(), 'temp-pliego-test.txt');
  fs.writeFileSync(tempFilePath, testContent);
  
  console.log('📄 Archivo de prueba creado con contenido de ejemplo');
  console.log(`📊 Tamaño: ${testContent.length} caracteres\n`);

  const tests = [
    {
      name: 'Análisis General del Pliego',
      prompt: 'Analiza este pliego de manera general, identificando los puntos clave, requisitos principales y aspectos más importantes.'
    },
    {
      name: 'Extracción de Requisitos Técnicos',
      prompt: 'Extrae y lista todos los requisitos técnicos específicos mencionados en este pliego, organizándolos por categorías.'
    },
    {
      name: 'Información de Presupuesto y Plazos',
      prompt: 'Identifica toda la información relacionada con presupuesto, costos, precios y plazos mencionados en el pliego.'
    }
  ];

  for (const test of tests) {
    try {
      console.log(`📋 ${test.name}...`);
      console.log(`💭 Prompt: "${test.prompt.substring(0, 80)}..."`);
      
      const formData = new FormData();
      formData.append('pliego', fs.createReadStream(tempFilePath), {
        filename: 'pliego-test.txt',
        contentType: 'text/plain'
      });
      formData.append('prompt', test.prompt);

      const startTime = Date.now();
      
      const response = await fetch(`${API_BASE_URL}/api/rag/process-pliego`, {
        method: 'POST',
        body: formData,
        headers: formData.getHeaders()
      });

      const endTime = Date.now();
      const processingTime = endTime - startTime;

      if (response.ok) {
        const result = await response.json();
        
        console.log(`   ✅ Status: ${response.status}`);
        console.log(`   ⏱️  Tiempo: ${processingTime}ms`);
        console.log(`   📄 Archivo: ${result.metadata?.fileName}`);
        console.log(`   📊 Contenido: ${result.metadata?.contentLength} caracteres`);
        console.log(`   🤖 Modelo: ${result.metadata?.model}`);
        console.log(`   📝 Análisis (primeros 200 chars):`);
        console.log(`      ${result.analysis.substring(0, 200)}...`);
        console.log(`   ✅ ${test.name} OK\n`);
        
      } else {
        const error = await response.text();
        console.log(`   ❌ Status: ${response.status}`);
        console.log(`   ❌ Error: ${error}`);
        console.log(`   ❌ ${test.name} FAILED\n`);
      }
      
    } catch (error) {
      console.log(`   ❌ ${test.name} FAILED: ${error.message}\n`);
    }

    // Pausa entre tests
    await new Promise(resolve => setTimeout(resolve, 1000));
  }

  // Limpiar archivo temporal
  try {
    fs.unlinkSync(tempFilePath);
    console.log('🗑️  Archivo temporal eliminado');
  } catch (error) {
    console.warn('⚠️  No se pudo eliminar archivo temporal:', error.message);
  }
}

// Test de conectividad básica
async function testConnectivity() {
  console.log('🔗 Probando conectividad básica...\n');
  
  try {
    const response = await fetch(`${API_BASE_URL}/health`);
    const data = await response.json();
    
    if (response.ok) {
      console.log('✅ Backend conectado');
      console.log(`   Status: ${data.status}`);
      console.log(`   Environment: ${data.environment}`);
      console.log(`   Vector Store: ${data.vectorStore}\n`);
      return true;
    } else {
      console.log('❌ Backend no responde correctamente\n');
      return false;
    }
  } catch (error) {
    console.log(`❌ No se puede conectar al backend: ${error.message}\n`);
    return false;
  }
}

// Ejecutar tests
async function runTests() {
  console.log('🚀 Iniciando tests del endpoint de procesamiento de pliegos');
  console.log('=' .repeat(60));
  console.log();

  // Test de conectividad
  const isConnected = await testConnectivity();
  
  if (!isConnected) {
    console.log('💡 Asegúrate de que el backend esté corriendo:');
    console.log('   cd aicore_api && npm start');
    process.exit(1);
  }

  // Test del endpoint
  await testPliegoEndpoint();
  
  console.log('🎉 Tests completados!');
  console.log('\n💡 Para usar el frontend:');
  console.log('   1. Inicia el frontend: cd aicore_api_web/aicore_web && npm start');
  console.log('   2. Abre http://localhost:3000');
  console.log('   3. Ve a la pestaña "Procesador de Pliegos"');
}

// Ejecutar
runTests().catch(console.error);
