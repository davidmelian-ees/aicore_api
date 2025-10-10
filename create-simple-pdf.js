// Script para crear un PDF simple usando texto plano con formato PDF básico
import fs from 'fs';

// Crear un PDF muy básico con contenido de pliego
const createSimplePDF = () => {
  // Contenido del pliego
  const pliegoText = `PLIEGO DE CONDICIONES TÉCNICAS
DESARROLLO DE APLICACIÓN WEB

1. OBJETO DEL CONTRATO
El presente pliego tiene por objeto establecer las condiciones técnicas para la contratación de servicios de desarrollo de una aplicación web para gestión de documentos.

CARACTERÍSTICAS PRINCIPALES:
- Sistema de autenticación de usuarios
- Gestión de documentos PDF y otros formatos
- Búsqueda avanzada con tecnología RAG
- Interfaz responsive y moderna
- API REST completa

2. REQUISITOS TÉCNICOS

2.1 TECNOLOGÍAS OBLIGATORIAS:
- Frontend: React.js v18+ o Vue.js v3+
- Backend: Node.js v18+ con Express.js
- Base de datos: PostgreSQL v14+ o MongoDB v6+
- Autenticación: JWT o OAuth2
- Hosting: Compatible con Cloud (AWS, Azure, GCP)

2.2 ESPECIFICACIONES TÉCNICAS:
- Responsive design (mobile-first)
- Soporte para navegadores modernos
- Tiempo de carga menor a 3 segundos
- Disponibilidad 99.9%
- Cifrado HTTPS obligatorio

3. PRESUPUESTO Y CONDICIONES ECONÓMICAS

3.1 PRESUPUESTO MÁXIMO:
El presupuesto máximo para este proyecto es de 45.000€ (IVA incluido).

3.2 FORMA DE PAGO:
- 30% al inicio del proyecto
- 40% en la entrega de la versión beta
- 30% en la entrega final y puesta en producción

4. PLAZO DE EJECUCIÓN
El plazo máximo de ejecución será de 4 meses desde la firma del contrato.

5. CRITERIOS DE EVALUACIÓN
- Propuesta técnica: 50%
- Propuesta económica: 30%
- Experiencia del equipo: 20%`;

  // Crear estructura PDF básica
  const pdfContent = `%PDF-1.4
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
/Resources <<
/Font <<
/F1 5 0 R
>>
>>
>>
endobj

4 0 obj
<<
/Length ${pliegoText.length + 100}
>>
stream
BT
/F1 12 Tf
50 750 Td
15 TL
${pliegoText.split('\n').map(line => `(${line.replace(/[()\\]/g, '\\$&')}) Tj T*`).join('\n')}
ET
endstream
endobj

5 0 obj
<<
/Type /Font
/Subtype /Type1
/BaseFont /Helvetica
>>
endobj

xref
0 6
0000000000 65535 f 
0000000009 00000 n 
0000000058 00000 n 
0000000115 00000 n 
0000000273 00000 n 
0000000${(400 + pliegoText.length).toString().padStart(3, '0')} 00000 n 
trailer
<<
/Size 6
/Root 1 0 R
>>
startxref
${500 + pliegoText.length}
%%EOF`;

  return pdfContent;
};

// Crear archivos de prueba
console.log('📄 Creando archivos de prueba para el procesador de pliegos...\n');

// 1. Archivo de texto simple
const textContent = `PLIEGO DE CONDICIONES TÉCNICAS

1. OBJETO DEL CONTRATO
Desarrollo de aplicación web con tecnología moderna.

2. REQUISITOS TÉCNICOS
- Frontend: React.js
- Backend: Node.js
- Base de datos: PostgreSQL

3. PRESUPUESTO
Máximo: 50.000€

4. PLAZO
4 meses de desarrollo.

5. CRITERIOS DE EVALUACIÓN
- Propuesta técnica: 60%
- Propuesta económica: 30%
- Experiencia: 10%`;

fs.writeFileSync('pliego-simple.txt', textContent, 'utf8');
console.log('✅ pliego-simple.txt creado');

// 2. Archivo PDF básico
const pdfContent = createSimplePDF();
fs.writeFileSync('pliego-simple.pdf', pdfContent, 'binary');
console.log('✅ pliego-simple.pdf creado');

// 3. Archivo Markdown
const markdownContent = `# Pliego de Condiciones Técnicas

## 1. Objeto del Contrato
Desarrollo de aplicación web con tecnología moderna.

## 2. Requisitos Técnicos
- **Frontend**: React.js
- **Backend**: Node.js  
- **Base de datos**: PostgreSQL

## 3. Presupuesto
**Máximo**: 50.000€

## 4. Plazo
4 meses de desarrollo.

## 5. Criterios de Evaluación
- Propuesta técnica: 60%
- Propuesta económica: 30%
- Experiencia: 10%`;

fs.writeFileSync('pliego-simple.md', markdownContent, 'utf8');
console.log('✅ pliego-simple.md creado');

console.log('\n🎉 Archivos de prueba creados exitosamente!');
console.log('\n📋 Archivos disponibles:');
console.log('   📄 pliego-simple.txt - Para pruebas básicas');
console.log('   📄 pliego-simple.pdf - PDF básico para testing');
console.log('   📄 pliego-simple.md - Formato Markdown');

console.log('\n💡 Para probar:');
console.log('   1. Instala pdfjs-dist: npm install pdfjs-dist@4.0.379');
console.log('   2. Reinicia el servidor: npm start');
console.log('   3. Usa cualquiera de estos archivos en Postman');
console.log('   4. O ejecuta: node test-pliego-endpoint.js');

console.log('\n🔧 pdfjs-dist es más confiable que pdf-parse:');
console.log('   - Desarrollado por Mozilla');
console.log('   - Mejor manejo de PDFs complejos');
console.log('   - Extracción página por página');
console.log('   - Más estable y mantenido');
