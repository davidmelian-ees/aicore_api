import { logoDetectionService } from '../services/logoDetectionService.js';
import path from 'path';

/**
 * Script de prueba para el servicio de detección de logos
 * 
 * Uso:
 * node tests/test_logo_detection.js <ruta-al-pdf>
 * 
 * Ejemplo:
 * node tests/test_logo_detection.js ./uploads/pliego_ejemplo.pdf
 */

async function testLogoDetection() {
  console.log('🔍 TEST DE DETECCIÓN DE LOGOS\n');
  console.log('='.repeat(60));

  // Obtener ruta del PDF desde argumentos
  const pdfPath = process.argv[2];

  if (!pdfPath) {
    console.error('❌ Error: Debes proporcionar la ruta a un archivo PDF');
    console.log('\nUso: node tests/test_logo_detection.js <ruta-al-pdf>');
    console.log('Ejemplo: node tests/test_logo_detection.js ./uploads/pliego.pdf');
    process.exit(1);
  }

  try {
    console.log(`\n📄 Analizando archivo: ${pdfPath}`);
    console.log('='.repeat(60));

    // 1. Detectar logos
    console.log('\n1️⃣ DETECCIÓN DE LOGOS...\n');
    const detection = await logoDetectionService.detectLogosInPDF(pdfPath);

    console.log('📊 Resultados de detección:');
    console.log(`   - Total de imágenes: ${detection.totalImages}`);
    console.log(`   - Páginas con imágenes: ${detection.pagesWithImages.length}`);
    console.log(`   - Imágenes en header: ${detection.headerImages.length}`);
    console.log(`   - Imágenes en footer: ${detection.footerImages.length}`);
    console.log(`   - Otras imágenes: ${detection.otherImages.length}`);

    // 2. Generar reporte de validación
    console.log('\n2️⃣ REPORTE DE VALIDACIÓN...\n');
    const filename = path.basename(pdfPath);
    const report = logoDetectionService.generateLogoValidationReport(detection, filename);

    console.log(`📋 Estado: ${report.status.toUpperCase()}`);
    console.log(`   - Logo obligatorio: ${report.hasRequiredLogo ? '✅ SÍ' : '❌ NO'}`);
    console.log(`   - Confianza: ${report.confidence}`);

    if (report.errors.length > 0) {
      console.log('\n❌ ERRORES:');
      report.errors.forEach((error, index) => {
        console.log(`   ${index + 1}. [${error.severity.toUpperCase()}] ${error.type}`);
        console.log(`      ${error.message}`);
        console.log(`      💡 ${error.recommendation}`);
      });
    }

    if (report.warnings.length > 0) {
      console.log('\n⚠️  ADVERTENCIAS:');
      report.warnings.forEach((warning, index) => {
        console.log(`   ${index + 1}. [${warning.severity.toUpperCase()}] ${warning.type}`);
        console.log(`      ${warning.message}`);
        console.log(`      💡 ${warning.recommendation}`);
      });
    }

    if (report.info.length > 0) {
      console.log('\nℹ️  INFORMACIÓN:');
      report.info.forEach((info, index) => {
        console.log(`   ${index + 1}. ${info.type}`);
        console.log(`      ${info.message}`);
        if (info.details) {
          console.log(`      📝 ${info.details}`);
        }
      });
    }

    // 3. Generar descripción para RAG
    console.log('\n3️⃣ DESCRIPCIÓN PARA CONTEXTO RAG...\n');
    const description = logoDetectionService.generateLogoDescription(detection, filename);
    console.log(description);

    // 4. Resumen final
    console.log('\n' + '='.repeat(60));
    console.log('📈 RESUMEN FINAL\n');

    if (report.status === 'success') {
      console.log('✅ DOCUMENTO VÁLIDO');
      console.log('   El pliego cumple con todos los requisitos de logos.');
      console.log('   Puede ser publicado sin problemas.');
    } else if (report.status === 'warning') {
      console.log('⚠️  DOCUMENTO CON ADVERTENCIAS');
      console.log('   El pliego tiene logos pero requiere revisión manual.');
      console.log('   Se recomienda verificar antes de publicar.');
    } else {
      console.log('❌ DOCUMENTO INVÁLIDO');
      console.log('   El pliego NO cumple con los requisitos obligatorios.');
      console.log('   NO debe ser publicado en su estado actual.');
    }

    console.log('\n' + '='.repeat(60));
    console.log('✅ Test completado exitosamente\n');

  } catch (error) {
    console.error('\n❌ ERROR durante el test:', error.message);
    console.error('\nDetalles:', error);
    process.exit(1);
  }
}

// Ejecutar test
testLogoDetection();
