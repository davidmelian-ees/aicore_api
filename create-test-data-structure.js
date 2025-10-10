// Script para crear la estructura de datos de prueba
import fs from 'fs';
import path from 'path';

async function createTestDataStructure() {
  console.log('🔧 Creando estructura de datos de prueba...\n');

  try {
    // Crear directorios
    const testDir = './test';
    const dataDir = './test/data';
    
    if (!fs.existsSync(testDir)) {
      fs.mkdirSync(testDir);
      console.log('✅ Directorio test/ creado');
    }
    
    if (!fs.existsSync(dataDir)) {
      fs.mkdirSync(dataDir);
      console.log('✅ Directorio test/data/ creado');
    }

    // Rutas de archivos
    const sourceFile = './node_modules/pdf-parse/test/data/05-versions-space.pdf';
    const targetFile = './test/data/05-versions-space.pdf';

    // Verificar si el archivo fuente existe
    if (fs.existsSync(sourceFile)) {
      // Copiar archivo PDF de prueba
      fs.copyFileSync(sourceFile, targetFile);
      console.log('✅ Archivo 05-versions-space.pdf copiado');
      
      // Verificar el archivo copiado
      const stats = fs.statSync(targetFile);
      console.log(`📊 Tamaño del archivo: ${(stats.size / 1024).toFixed(2)} KB`);
      
    } else {
      console.log('⚠️  Archivo fuente no encontrado, creando archivo alternativo...');
      
      // Crear un archivo de texto que simule un PDF para pruebas
      const alternativeContent = `%PDF-1.4
1 0 obj
<<
/Type /Catalog
/Pages 2 0 R
>>
endobj

2 0 obj
<<
/Type /Pages
/Kids [3 0 R]
/Count 1
>>
endobj

3 0 obj
<<
/Type /Page
/Parent 2 0 R
/MediaBox [0 0 612 792]
/Contents 4 0 R
>>
endobj

4 0 obj
<<
/Length 44
>>
stream
BT
/F1 12 Tf
72 720 Td
(Test PDF Content) Tj
ET
endstream
endobj

xref
0 5
0000000000 65535 f 
0000000009 00000 n 
0000000058 00000 n 
0000000115 00000 n 
0000000206 00000 n 
trailer
<<
/Size 5
/Root 1 0 R
>>
startxref
295
%%EOF`;

      fs.writeFileSync(targetFile, alternativeContent);
      console.log('✅ Archivo PDF alternativo creado');
    }

    // Crear otros archivos de prueba útiles
    const testFiles = [
      {
        name: 'pliego-ejemplo.txt',
        content: `PLIEGO DE CONDICIONES TÉCNICAS

1. OBJETO DEL CONTRATO
Desarrollo de aplicación web con tecnología moderna.

2. REQUISITOS TÉCNICOS
- Frontend: React.js
- Backend: Node.js
- Base de datos: PostgreSQL

3. PRESUPUESTO
Máximo: 50.000€

4. PLAZO
4 meses de desarrollo.`
      },
      {
        name: 'documento-prueba.md',
        content: `# Documento de Prueba

## Introducción
Este es un documento de prueba para el sistema RAG.

## Contenido
- Punto 1
- Punto 2
- Punto 3

## Conclusión
Documento creado para testing.`
      }
    ];

    for (const file of testFiles) {
      const filePath = path.join(dataDir, file.name);
      fs.writeFileSync(filePath, file.content, 'utf8');
      console.log(`✅ Archivo ${file.name} creado`);
    }

    console.log('\n🎉 Estructura de datos de prueba creada exitosamente!');
    console.log('\n📁 Archivos disponibles:');
    
    const files = fs.readdirSync(dataDir);
    files.forEach(file => {
      const filePath = path.join(dataDir, file);
      const stats = fs.statSync(filePath);
      console.log(`   📄 ${file} (${(stats.size / 1024).toFixed(2)} KB)`);
    });

    console.log('\n💡 Ahora puedes usar estos archivos para pruebas del sistema RAG.');

  } catch (error) {
    console.error('❌ Error creando estructura:', error.message);
    process.exit(1);
  }
}

// Ejecutar
createTestDataStructure();
