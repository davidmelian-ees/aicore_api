import fs from 'fs/promises';
import * as pdfjsLib from 'pdfjs-dist/legacy/build/pdf.mjs';

/**
 * Servicio de detección de logos en PDFs
 * Detecta la presencia de imágenes/logos en documentos PDF
 */
class LogoDetectionService {
  constructor() {
    this.minimumLogoSize = 1000; // Tamaño mínimo en bytes para considerar como logo
    this.expectedLogos = {
      required: ['Infraestructuras de Cataluña'], // Logo obligatorio
      optional: ['Ayuntamiento', 'Empresa', 'Certificación']
    };
  }

  /**
   * Detecta logos en un archivo PDF
   * @param {string} pdfPath - Ruta al archivo PDF
   * @returns {Object} - Información sobre logos detectados
   */
  async detectLogosInPDF(pdfPath) {
    try {
      console.log('[LOGO-DETECT] 🔍 Analizando PDF:', pdfPath);

      const data = await fs.readFile(pdfPath);
      const loadingTask = pdfjsLib.getDocument({
        data,
        useSystemFonts: true,
        standardFontDataUrl: null
      });
      const pdf = await loadingTask.promise;

      const detection = {
        hasLogos: false,
        totalImages: 0,
        imagesPerPage: {},
        pagesWithImages: [],
        headerImages: [], // Imágenes en parte superior (posible logo obligatorio)
        footerImages: [], // Imágenes en pie de página
        otherImages: [],  // Otras imágenes
        analysis: {
          hasRequiredLogo: false, // Logo de Infraestructuras de Cataluña
          logoPosition: null,
          confidence: 'unknown'
        }
      };

      // Analizar cada página
      for (let pageNum = 1; pageNum <= pdf.numPages; pageNum++) {
        const page = await pdf.getPage(pageNum);
        const viewport = page.getViewport({ scale: 1.0 });
        const pageHeight = viewport.height;
        
        // Obtener operaciones de la página
        const operatorList = await page.getOperatorList();
        
        let pageImageCount = 0;
        
        // Buscar operaciones de imagen
        for (let i = 0; i < operatorList.fnArray.length; i++) {
          if (operatorList.fnArray[i] === pdfjsLib.OPS.paintImageXObject) {
            pageImageCount++;
            
            // Intentar obtener información de posición
            // Las transformaciones están en argsArray
            const transform = operatorList.argsArray[i - 1] || null;
            
            let position = 'unknown';
            let yPosition = null;
            
            // Analizar transformación si está disponible
            if (transform && Array.isArray(transform) && transform.length >= 6) {
              yPosition = transform[5]; // Posición Y en la matriz de transformación
              
              // Determinar si está en header, footer o centro
              const relativePosition = yPosition / pageHeight;
              
              if (relativePosition > 0.85) {
                position = 'header'; // Parte superior (85-100%)
                detection.headerImages.push({
                  page: pageNum,
                  yPosition,
                  relativePosition
                });
              } else if (relativePosition < 0.15) {
                position = 'footer'; // Parte inferior (0-15%)
                detection.footerImages.push({
                  page: pageNum,
                  yPosition,
                  relativePosition
                });
              } else {
                position = 'body';
                detection.otherImages.push({
                  page: pageNum,
                  yPosition,
                  relativePosition
                });
              }
            }
            
            console.log(`[LOGO-DETECT] 📷 Imagen detectada en página ${pageNum}, posición: ${position}`);
          }
        }
        
        if (pageImageCount > 0) {
          detection.imagesPerPage[pageNum] = pageImageCount;
          detection.pagesWithImages.push(pageNum);
          detection.totalImages += pageImageCount;
        }
      }

      detection.hasLogos = detection.totalImages > 0;

      // Analizar si tiene el logo obligatorio (Infraestructuras de Cataluña)
      // Típicamente está en la parte superior de las primeras páginas
      if (detection.headerImages.length > 0) {
        // Si hay imágenes en header de múltiples páginas, probablemente es el logo oficial
        const pagesWithHeader = [...new Set(detection.headerImages.map(img => img.page))];
        
        if (pagesWithHeader.length >= 1) {
          detection.analysis.hasRequiredLogo = true;
          detection.analysis.logoPosition = 'header';
          detection.analysis.confidence = pagesWithHeader.length > 3 ? 'high' : 'medium';
          
          console.log('[LOGO-DETECT] ✅ Logo en header detectado en', pagesWithHeader.length, 'páginas');
        } else {
          detection.analysis.hasRequiredLogo = false;
          detection.analysis.confidence = 'low';
          console.log('[LOGO-DETECT] ⚠️ No se detectó logo consistente en header');
        }
      } else {
        detection.analysis.hasRequiredLogo = false;
        detection.analysis.confidence = 'none';
        console.log('[LOGO-DETECT] ❌ No se detectaron imágenes en header');
      }

      // Resumen
      console.log('[LOGO-DETECT] 📊 Resumen:');
      console.log(`  - Total imágenes: ${detection.totalImages}`);
      console.log(`  - Páginas con imágenes: ${detection.pagesWithImages.length}`);
      console.log(`  - Imágenes en header: ${detection.headerImages.length}`);
      console.log(`  - Imágenes en footer: ${detection.footerImages.length}`);
      console.log(`  - Logo obligatorio: ${detection.analysis.hasRequiredLogo ? '✅ SÍ' : '❌ NO'}`);

      return detection;

    } catch (error) {
      console.error('[LOGO-DETECT] ❌ Error detectando logos:', error);
      return {
        hasLogos: false,
        totalImages: 0,
        error: error.message,
        analysis: {
          hasRequiredLogo: false,
          confidence: 'error'
        }
      };
    }
  }

  /**
   * Genera un reporte de validación de logos
   * @param {Object} detection - Resultado de detectLogosInPDF
   * @param {string} filename - Nombre del archivo
   * @returns {Object} - Reporte de validación
   */
  generateLogoValidationReport(detection, filename) {
    const report = {
      filename,
      timestamp: new Date().toISOString(),
      hasRequiredLogo: detection.analysis.hasRequiredLogo,
      confidence: detection.analysis.confidence,
      status: 'unknown',
      errors: [],
      warnings: [],
      info: []
    };

    // Validación del logo obligatorio
    if (!detection.analysis.hasRequiredLogo) {
      report.status = 'error';
      report.errors.push({
        type: 'LOGO_OBLIGATORIO_AUSENTE',
        severity: 'critical',
        message: 'No se detectó el logo obligatorio de "Infraestructuras de Cataluña" en la parte superior del documento',
        recommendation: 'Añadir el logo institucional en el header de todas las páginas'
      });
    } else if (detection.analysis.confidence === 'low' || detection.analysis.confidence === 'medium') {
      report.status = 'warning';
      report.warnings.push({
        type: 'LOGO_INCONSISTENTE',
        severity: 'medium',
        message: 'Se detectó logo en header pero no está presente en todas las páginas de forma consistente',
        recommendation: 'Verificar que el logo aparezca en todas las páginas requeridas'
      });
    } else {
      report.status = 'success';
      report.info.push({
        type: 'LOGO_CORRECTO',
        message: `Logo obligatorio detectado correctamente en ${detection.headerImages.length} ubicaciones`,
        details: `Confianza: ${detection.analysis.confidence}`
      });
    }

    // Información adicional sobre otros logos
    if (detection.footerImages.length > 0) {
      report.info.push({
        type: 'LOGOS_ADICIONALES',
        message: `Se detectaron ${detection.footerImages.length} imágenes adicionales en pie de página`,
        details: 'Pueden ser logos de empresas, ayuntamientos u otras instituciones'
      });
    }

    // Advertencia si no hay ninguna imagen
    if (detection.totalImages === 0) {
      report.errors.push({
        type: 'SIN_IMAGENES',
        severity: 'critical',
        message: 'El documento no contiene ninguna imagen o logo',
        recommendation: 'Añadir el logo institucional obligatorio'
      });
    }

    return report;
  }

  /**
   * Genera descripción textual para entrenar la IA
   * @param {Object} detection - Resultado de detectLogosInPDF
   * @param {string} filename - Nombre del archivo
   * @returns {string} - Descripción para el contexto RAG
   */
  generateLogoDescription(detection, filename) {
    const parts = [];

    parts.push(`ANÁLISIS DE LOGOS - ${filename}`);
    parts.push('');

    if (detection.analysis.hasRequiredLogo) {
      parts.push('✅ LOGO OBLIGATORIO: SÍ');
      parts.push(`   - Logo de "Infraestructuras de Cataluña" detectado en header`);
      parts.push(`   - Presente en ${detection.headerImages.length} ubicaciones`);
      parts.push(`   - Nivel de confianza: ${detection.analysis.confidence}`);
      parts.push(`   - Este documento CUMPLE con el requisito de logo institucional`);
    } else {
      parts.push('❌ LOGO OBLIGATORIO: NO');
      parts.push(`   - NO se detectó el logo de "Infraestructuras de Cataluña"`);
      parts.push(`   - Este documento NO CUMPLE con el requisito de logo institucional`);
      parts.push(`   - ACCIÓN REQUERIDA: Añadir logo en la parte superior`);
    }

    parts.push('');
    parts.push(`📊 ESTADÍSTICAS:`);
    parts.push(`   - Total de imágenes: ${detection.totalImages}`);
    parts.push(`   - Páginas con imágenes: ${detection.pagesWithImages.length}`);
    parts.push(`   - Imágenes en header (superior): ${detection.headerImages.length}`);
    parts.push(`   - Imágenes en footer (inferior): ${detection.footerImages.length}`);
    parts.push(`   - Otras imágenes: ${detection.otherImages.length}`);

    if (detection.footerImages.length > 0) {
      parts.push('');
      parts.push('📌 LOGOS ADICIONALES:');
      parts.push(`   - Se detectaron ${detection.footerImages.length} imágenes en pie de página`);
      parts.push(`   - Pueden ser logos de: Ayuntamiento, Empresa contratista, Certificaciones`);
    }

    parts.push('');
    parts.push('---');
    parts.push('Este análisis se utilizará como referencia para validar futuros documentos.');

    return parts.join('\n');
  }
}

// Exportar instancia singleton
export const logoDetectionService = new LogoDetectionService();
