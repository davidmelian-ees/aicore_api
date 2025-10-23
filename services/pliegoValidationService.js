import { generateEmbedding } from './embeddingService.js';
import { getAiCoreClient } from '../auth/aiCoreClient.js';
import { sqliteVectorStore } from './sqliteVectorStore.js';
import fs from 'fs/promises';
import path from 'path';
import { v4 as uuidv4 } from 'uuid';

/**
 * Servicio de Validación Estructural de Pliegos
 * Integrado con SAP - Validación de Plantillas y Estructura
 */
class PliegoValidationService {
  constructor() {
    this.tiposPliego = {
      'OBRA_CIVIL_OBERT': {
        nombre: 'Obra Civil Obert (Abierto)',
        codigo: 'obra_civil_obert',
        patrones: ['OBRA CIVIL OBERT', 'OBRA CIVIL ABIERTO', 'PROCEDIMIENTO ABIERTO OBRA CIVIL'],
        apartadosObligatorios: [
          '1. OBJETO DE LAS OBRAS',
          '2. PRESUPUESTO BASE DE LICITACIÓN',
          '3. CRITERIOS DE ADJUDICACIÓN',
          '4. PLAZO DE EJECUCIÓN',
          '5. CONDICIONES TÉCNICAS'
        ]
      },
      'OBRA_CIVIL_SIMPLIFICAT': {
        nombre: 'Obra Civil Simplificat (Simplificado)',
        codigo: 'obra_civil_simplificat',
        patrones: ['OBRA CIVIL SIMPLIFICAT', 'OBRA CIVIL SIMPLIFICADO', 'PROCEDIMIENTO SIMPLIFICADO OBRA CIVIL'],
        apartadosObligatorios: [
          '1. OBJETO DE LAS OBRAS',
          '2. PRESUPUESTO ESTIMADO',
          '3. PLAZO DE EJECUCIÓN',
          '4. CRITERIOS BÁSICOS'
        ]
      },
      'OBRA_EDIFICACIO_OBERT': {
        nombre: 'Obra Edificació Obert (Edificación Abierto)',
        codigo: 'obra_edificacio_obert',
        patrones: ['OBRA EDIFICACIÓ OBERT', 'OBRA EDIFICACIÓN ABIERTO', 'PROCEDIMIENTO ABIERTO EDIFICACIÓN'],
        apartadosObligatorios: [
          '1. OBJETO DE LA EDIFICACIÓN',
          '2. PRESUPUESTO BASE DE LICITACIÓN',
          '3. CRITERIOS DE ADJUDICACIÓN',
          '4. PLAZO DE EJECUCIÓN',
          '5. ESPECIFICACIONES TÉCNICAS',
          '6. NORMATIVA EDIFICACIÓN'
        ]
      },
      'OBRA_EDIFICACIO_SIMPLIFICAT': {
        nombre: 'Obra Edificació Simplificat (Edificación Simplificado)',
        codigo: 'obra_edificacio_simplificat',
        patrones: ['OBRA EDIFICACIÓ SIMPLIFICAT', 'OBRA EDIFICACIÓN SIMPLIFICADO', 'PROCEDIMIENTO SIMPLIFICADO EDIFICACIÓN'],
        apartadosObligatorios: [
          '1. OBJETO DE LA EDIFICACIÓN',
          '2. PRESUPUESTO ESTIMADO',
          '3. PLAZO DE EJECUCIÓN',
          '4. CRITERIOS BÁSICOS',
          '5. NORMATIVA BÁSICA'
        ]
      }
    };

    this.categoriasDocumento = {
      'PLANTILLA': {
        descripcion: 'Documento base limpio sin datos específicos',
        sufijo: 'PLANTILLA',
        contextoRAG: 'PLANTILLAS_BASE'
      },
      'PLANTILLA_TAGS': {
        descripcion: 'Plantilla con marcadores SAP {{VARIABLE}}',
        sufijo: 'PLANTILLA_TAGS',
        contextoRAG: 'PLANTILLAS_TAGS'
      },
      'GENERADO': {
        descripcion: 'Pliego final con datos SAP insertados (referencia correcta)',
        sufijo: 'GENERADO',
        contextoRAG: 'PLIEGOS_REFERENCIA'
      },
      'VALIDACION': {
        descripcion: 'Documento a validar (puede tener errores)',
        sufijo: 'VALIDACION',
        contextoRAG: 'DOCUMENTOS_VALIDACION'
      }
    };

    this.patronesTagsSAP = [
      /\{\{[A-Z_]+\}\}/g,           // {{VARIABLE}}
      /\[[\w\s_]+\]/g,              // [VARIABLE]
      /{[\w\s_]+}/g,                // {VARIABLE}
      /<<[\w\s_]+>>/g,              // <<VARIABLE>>
      /@[\w_]+@/g                   // @VARIABLE@
    ];

    this.camposVariablesDetectados = new Map();
    
    // Patrones para errores comunes
    this.erroresComunes = {
      tagsSinRellenar: [
        /\{\{[^}]*\}\}/g,
        /\[[^\]]*\]/g,
        /<<[^>]*>>/g,
        /@[^@]*@/g,
        /{[^}]*}/g
      ],
      camposSiNoduplicados: /(\[X\]|\☑|X)\s*(SÍ|SI)[\s\S]*?(\[X\]|\☑|X)\s*(NO)/gi,
      fechasIncorrectas: [
        /\d{4}-\d{2}-\d{2}/g,
        /\d{1,2}-\d{1,2}-\d{4}/g,
        /[A-Za-z]+ \d{1,2}, \d{4}/g
      ],
      importesIncorrectos: [
        /€\s*\d+/g,
        /\d+,\d{3}\.\d{2}/g,
        /\d+€/g
      ],
      lotes: [
        /LOTE?\s*(\d+)/gi,
        /LOT\s*(\d+)/gi
      ],
      palabrasCastellanas: /\b(presupuesto|contrato|licitación|adjudicación|obras|proyecto|enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre)\b/gi
    };
  }

  /**
   * Genera nombre estándar para un documento según la convención
   */
  generarNombreEstandar(tipoPliego, tieneLotes, categoria) {
    const tipo = this.tiposPliego[tipoPliego]?.codigo || 'desconocido';
    const modalidad = tieneLotes ? 'con_lotes' : 'sin_lotes';
    const sufijo = this.categoriasDocumento[categoria]?.sufijo || categoria;
    
    return `pliego_${tipo}_${modalidad}_${sufijo}`;
  }

  /**
   * Detecta la categoría del documento por su nombre de archivo
   */
  detectarCategoriaPorNombre(nombreArchivo) {
    const nombreLower = nombreArchivo.toLowerCase();
    
    if (nombreLower.includes('plantilla_tags') || nombreLower.includes('tags')) {
      return 'PLANTILLA_TAGS';
    }
    if (nombreLower.includes('plantilla')) {
      return 'PLANTILLA';
    }
    if (nombreLower.includes('generado') || nombreLower.includes('referencia')) {
      return 'GENERADO';
    }
    if (nombreLower.includes('validacion') || nombreLower.includes('validar')) {
      return 'VALIDACION';
    }
    
    return 'VALIDACION'; // Por defecto, asumimos que es para validar
  }

  /**
   * Sugiere el contexto RAG apropiado según la categoría
   */
  obtenerContextoRAG(categoria) {
    return this.categoriasDocumento[categoria]?.contextoRAG || 'DOCUMENTOS_VALIDACION';
  }

  /**
   * Valida si un nombre de archivo sigue la convención
   */
  validarNombreConvencion(nombreArchivo) {
    // Patrón para plantillas (sin modalidad)
    const patronPlantillas = /^pliego_(obra_civil_obert|obra_civil_simplificat|obra_edificacio_obert|obra_edificacio_simplificat)_(PLANTILLA|PLANTILLA_TAGS)/i;
    
    // Patrón para generados/validación (con modalidad)
    const patronGenerados = /^pliego_(obra_civil_obert|obra_civil_simplificat|obra_edificacio_obert|obra_edificacio_simplificat)_(con_lotes|sin_lotes)_(GENERADO|VALIDACION)/i;
    
    let match = nombreArchivo.match(patronPlantillas);
    if (match) {
      return {
        esValido: true,
        tipo: match[1].toUpperCase(),
        modalidad: null, // Las plantillas no tienen modalidad
        categoria: match[2].toUpperCase(),
        nombreSugerido: nombreArchivo
      };
    }
    
    match = nombreArchivo.match(patronGenerados);
    if (match) {
      return {
        esValido: true,
        tipo: match[1].toUpperCase(),
        modalidad: match[2],
        categoria: match[3].toUpperCase(),
        nombreSugerido: nombreArchivo
      };
    }
    
    return {
      esValido: false,
      tipo: null,
      modalidad: null,
      categoria: null,
      nombreSugerido: null
    };
  }

  /**
   * PRE-VALIDACIÓN: Detecta errores comunes antes del análisis completo
   */
  async validarErroresComunes(texto) {
    try {
      console.log('[PLIEGO-VALIDATION] 🔍 Ejecutando pre-validación de errores comunes...');
      
      const erroresCriticos = [];
      const advertencias = [];
      
      // 1. TAGS SAP SIN RELLENAR (CRÍTICO)
      const tagsSinRellenar = [];
      for (const patron of this.erroresComunes.tagsSinRellenar) {
        const matches = texto.match(patron) || [];
        tagsSinRellenar.push(...matches);
      }
      
      if (tagsSinRellenar.length > 0) {
        erroresCriticos.push({
          tipo: 'TAGS_SIN_RELLENAR',
          criticidad: 'CRITICO',
          descripcion: `Se encontraron ${tagsSinRellenar.length} tags SAP sin rellenar`,
          ejemplos: tagsSinRellenar.slice(0, 5), // Mostrar máximo 5 ejemplos
          total: tagsSinRellenar.length
        });
      }
      
      // 2. CAMPOS SÍ/NO DUPLICADOS (CRÍTICO)
      const camposDuplicados = texto.match(this.erroresComunes.camposSiNoduplicados) || [];
      if (camposDuplicados.length > 0) {
        erroresCriticos.push({
          tipo: 'CAMPOS_SI_NO_DUPLICADOS',
          criticidad: 'CRITICO',
          descripcion: `Se encontraron ${camposDuplicados.length} campos con SÍ y NO marcados simultáneamente`,
          ejemplos: camposDuplicados.slice(0, 3),
          total: camposDuplicados.length
        });
      }
      
      // 3. CANTIDAD DE LOTES (CRÍTICO)
      const lotesEncontrados = [];
      for (const patron of this.erroresComunes.lotes) {
        const matches = texto.match(patron) || [];
        lotesEncontrados.push(...matches);
      }
      
      const numerosLotes = lotesEncontrados.map(l => {
        const match = l.match(/(\d+)/);
        return match ? parseInt(match[1]) : 0;
      }).filter(n => n > 0);
      
      const lotesUnicos = [...new Set(numerosLotes)].sort((a, b) => a - b);
      const maxLote = Math.max(...numerosLotes, 0);
      
      // Verificar secuencia de lotes
      const secuenciaCompleta = Array.from({length: maxLote}, (_, i) => i + 1);
      const lotesFaltantes = secuenciaCompleta.filter(n => !lotesUnicos.includes(n));
      
      if (lotesFaltantes.length > 0 && maxLote > 0) {
        erroresCriticos.push({
          tipo: 'LOTES_INCONSISTENTES',
          criticidad: 'CRITICO',
          descripcion: `Secuencia de lotes incompleta. Faltan lotes: ${lotesFaltantes.join(', ')}`,
          lotesEncontrados: lotesUnicos,
          lotesFaltantes,
          maxLote
        });
      }
      
      // 4. FECHAS INCORRECTAS (ADVERTENCIA)
      const fechasIncorrectas = [];
      for (const patron of this.erroresComunes.fechasIncorrectas) {
        const matches = texto.match(patron) || [];
        fechasIncorrectas.push(...matches);
      }
      
      if (fechasIncorrectas.length > 0) {
        advertencias.push({
          tipo: 'FECHAS_FORMATO_INCORRECTO',
          criticidad: 'ADVERTENCIA',
          descripcion: `Se encontraron ${fechasIncorrectas.length} fechas con formato incorrecto`,
          ejemplos: fechasIncorrectas.slice(0, 5),
          formatoCorrecto: 'DD/MM/AAAA'
        });
      }
      
      // 5. IMPORTES INCORRECTOS (ADVERTENCIA)
      const importesIncorrectos = [];
      for (const patron of this.erroresComunes.importesIncorrectos) {
        const matches = texto.match(patron) || [];
        importesIncorrectos.push(...matches);
      }
      
      if (importesIncorrectos.length > 0) {
        advertencias.push({
          tipo: 'IMPORTES_FORMATO_INCORRECTO',
          criticidad: 'ADVERTENCIA',
          descripcion: `Se encontraron ${importesIncorrectos.length} importes con formato incorrecto`,
          ejemplos: importesIncorrectos.slice(0, 5),
          formatoCorrecto: '123.456,78 €'
        });
      }
      
      // 6. PALABRAS EN CASTELLANO (ADVERTENCIA)
      const palabrasCastellanas = texto.match(this.erroresComunes.palabrasCastellanas) || [];
      if (palabrasCastellanas.length > 0) {
        const palabrasUnicas = [...new Set(palabrasCastellanas.map(p => p.toLowerCase()))];
        advertencias.push({
          tipo: 'IDIOMA_INCORRECTO',
          criticidad: 'ADVERTENCIA',
          descripcion: `Se encontraron ${palabrasUnicas.length} palabras en castellano`,
          ejemplos: palabrasUnicas.slice(0, 10),
          idiomaRequerido: 'Catalán'
        });
      }
      
      const resultado = {
        tieneErroresCriticos: erroresCriticos.length > 0,
        tieneAdvertencias: advertencias.length > 0,
        erroresCriticos,
        advertencias,
        resumen: {
          totalErroresCriticos: erroresCriticos.length,
          totalAdvertencias: advertencias.length,
          bloqueaValidacion: erroresCriticos.length > 0
        }
      };
      
      console.log(`[PLIEGO-VALIDATION] 📊 Pre-validación completada: ${erroresCriticos.length} críticos, ${advertencias.length} advertencias`);
      
      return resultado;
      
    } catch (error) {
      console.error('[PLIEGO-VALIDATION] ❌ Error en pre-validación:', error);
      throw error;
    }
  }

  /**
   * Identifica automáticamente el tipo de documento
   */
  async identificarTipoDocumento(texto) {
    try {
      // 1. Detectar presencia de tags SAP
      const tieneTagsSAP = this.patronesTagsSAP.some(patron => patron.test(texto));
      
      // 2. Analizar especificidad del contenido
      const tieneImportesEspecificos = /\d+\.?\d*\s*€|\d+\.?\d*\s*euros?/i.test(texto);
      const tieneFechasEspecificas = /\d{1,2}\/\d{1,2}\/\d{4}|\d{1,2}-\d{1,2}-\d{4}/g.test(texto);
      const tieneCodigosExpediente = /EXP[\d\-\/]+|EXPEDIENTE[\s\d\-\/]+/i.test(texto);
      
      // 3. Buscar patrones de plantilla genérica
      const esGenerico = this.esContenidoGenerico(texto);
      
      // 4. Clasificar tipo de documento
      if (tieneTagsSAP) {
        return {
          tipo: 'PLANTILLA_CON_TAGS',
          confianza: 0.95,
          evidencia: 'Presencia de tags SAP detectados'
        };
      }
      
      if (tieneImportesEspecificos && tieneFechasEspecificas && tieneCodigosExpediente) {
        return {
          tipo: 'PLIEGO_GENERADO',
          confianza: 0.90,
          evidencia: 'Datos específicos: importes, fechas y códigos'
        };
      }
      
      if (esGenerico) {
        return {
          tipo: 'PLANTILLA_BASE',
          confianza: 0.85,
          evidencia: 'Contenido genérico sin datos específicos'
        };
      }
      
      return {
        tipo: 'DOCUMENTO_REFERENCIA',
        confianza: 0.70,
        evidencia: 'Documento con datos específicos históricos'
      };
      
    } catch (error) {
      console.error('[PLIEGO-VALIDATION] Error identificando tipo:', error);
      throw error;
    }
  }

  /**
   * Clasifica el tipo específico de pliego (A, B, C, D)
   */
  async clasificarTipoPliego(texto) {
    try {
      const resultados = [];
      
      for (const [tipo, config] of Object.entries(this.tiposPliego)) {
        let puntuacion = 0;
        const evidencias = [];
        
        // Buscar patrones característicos
        for (const patron of config.patrones) {
          const regex = new RegExp(patron, 'gi');
          const coincidencias = (texto.match(regex) || []).length;
          if (coincidencias > 0) {
            puntuacion += coincidencias * 10;
            evidencias.push(`Patrón "${patron}" encontrado ${coincidencias} veces`);
          }
        }
        
        // Verificar apartados característicos
        for (const apartado of config.apartadosObligatorios) {
          const regex = new RegExp(apartado.replace(/\d+\./, '\\d+\\.'), 'gi');
          if (regex.test(texto)) {
            puntuacion += 5;
            evidencias.push(`Apartado "${apartado}" encontrado`);
          }
        }
        
        if (puntuacion > 0) {
          resultados.push({
            tipo,
            nombre: config.nombre,
            puntuacion,
            evidencias
          });
        }
      }
      
      // Ordenar por puntuación y retornar el mejor
      resultados.sort((a, b) => b.puntuacion - a.puntuacion);
      
      if (resultados.length === 0) {
        return {
          tipo: 'TIPO_DESCONOCIDO',
          confianza: 0,
          evidencia: 'No se encontraron patrones reconocibles'
        };
      }
      
      const mejor = resultados[0];
      return {
        tipo: mejor.tipo,
        nombre: mejor.nombre,
        confianza: Math.min(mejor.puntuacion / 50, 1), // Normalizar a 0-1
        evidencias: mejor.evidencias
      };
      
    } catch (error) {
      console.error('[PLIEGO-VALIDATION] Error clasificando tipo:', error);
      throw error;
    }
  }

  /**
   * Detecta si el pliego incluye lotes
   */
  detectarLotes(texto) {
    const patronesLotes = [
      /lote\s*\d+/gi,
      /lotes?\s*:/gi,
      /división\s*en\s*lotes/gi,
      /\d+\.\s*lotes/gi,
      /apartado\s*\d+\.\s*lotes/gi
    ];
    
    const coincidencias = patronesLotes.reduce((total, patron) => {
      return total + (texto.match(patron) || []).length;
    }, 0);
    
    return {
      tieneLotes: coincidencias > 0,
      numeroLotes: this.extraerNumeroLotes(texto),
      confianza: Math.min(coincidencias / 3, 1),
      evidencias: coincidencias
    };
  }

  /**
   * Extrae y cataloga campos variables del documento
   */
  async extraerCamposVariables(texto, tipoDocumento) {
    try {
      const camposEncontrados = [];
      
      if (tipoDocumento === 'PLANTILLA_CON_TAGS') {
        // Extraer tags SAP directamente
        for (const patron of this.patronesTagsSAP) {
          const matches = texto.match(patron) || [];
          for (const match of matches) {
            camposEncontrados.push({
              campo: match,
              tipo: 'TAG_SAP',
              ubicacion: this.encontrarUbicacion(texto, match),
              valor: null
            });
          }
        }
      } else if (tipoDocumento === 'PLIEGO_GENERADO') {
        // Identificar campos variables por patrones de contenido
        camposEncontrados.push(...this.identificarCamposPorPatrones(texto));
      }
      
      // Guardar campos detectados para análisis futuro
      await this.guardarCamposDetectados(camposEncontrados);
      
      return camposEncontrados;
      
    } catch (error) {
      console.error('[PLIEGO-VALIDATION] Error extrayendo campos:', error);
      throw error;
    }
  }

  /**
   * Valida la estructura del pliego contra su plantilla
   */
  async validarEstructura(texto, tipoPliego) {
    try {
      if (!this.tiposPliego[tipoPliego]) {
        throw new Error(`Tipo de pliego desconocido: ${tipoPliego}`);
      }
      
      const config = this.tiposPliego[tipoPliego];
      const estructura = this.extraerEstructuraDocumento(texto);
      
      // Verificar apartados obligatorios
      const apartadosFaltantes = [];
      const apartadosPresentes = [];
      
      for (const apartado of config.apartadosObligatorios) {
        const regex = new RegExp(apartado.replace(/\d+\./, '\\d+\\.'), 'gi');
        if (regex.test(texto)) {
          apartadosPresentes.push(apartado);
        } else {
          apartadosFaltantes.push(apartado);
        }
      }
      
      // Verificar orden de apartados
      const ordenCorrecto = this.verificarOrdenApartados(estructura, config.apartadosObligatorios);
      
      // Calcular puntuación de completitud
      const completitud = (apartadosPresentes.length / config.apartadosObligatorios.length) * 100;
      
      return {
        cumplePlantilla: apartadosFaltantes.length === 0,
        apartadosFaltantes,
        apartadosPresentes,
        apartadosAdicionales: this.identificarApartadosAdicionales(estructura, config.apartadosObligatorios),
        ordenCorrecto,
        completitud: Math.round(completitud),
        estructura
      };
      
    } catch (error) {
      console.error('[PLIEGO-VALIDATION] Error validando estructura:', error);
      throw error;
    }
  }

  /**
   * Genera un reporte completo de validación
   */
  async generarReporteValidacion(texto, metadatos = {}) {
    try {
      console.log('[PLIEGO-VALIDATION] Iniciando validación completa...');
      
      // 0. Analizar contexto y nombre del archivo si están disponibles
      const contextId = metadatos.contextId;
      let analisisNombre = null;
      let categoriaDetectada = 'VALIDACION';
      
      if (metadatos.fileName) {
        analisisNombre = this.validarNombreConvencion(metadatos.fileName);
        if (!analisisNombre.esValido) {
          categoriaDetectada = this.detectarCategoriaPorNombre(metadatos.fileName);
        } else {
          categoriaDetectada = analisisNombre.categoria;
        }
      }
      
      // Determinar contexto de validación
      let contextoValidacion = 'GENERAL';
      if (contextId) {
        contextoValidacion = contextId;
        console.log(`[PLIEGO-VALIDATION] 📂 Validando contra contexto: ${contextId}`);
      }
      
      // 0.5. PRE-VALIDACIÓN: Detectar errores comunes
      const erroresComunes = await this.validarErroresComunes(texto);
      
      // Si hay errores críticos, devolver reporte inmediato
      if (erroresComunes.tieneErroresCriticos) {
        console.log(`[PLIEGO-VALIDATION] 🚨 ERRORES CRÍTICOS detectados - Deteniendo validación`);
        
        const reporteErroresCriticos = {
          id: uuidv4(),
          timestamp: new Date().toISOString(),
          metadatos,
          contextoValidacion: {
            contextId: contextId || null,
            contextoUsado: contextoValidacion,
            contextoSugerido: this.obtenerContextoRAG(categoriaDetectada)
          },
          validacionCompleta: false,
          erroresComunes,
          bloqueadoPorErroresCriticos: true,
          mensaje: 'Validación detenida por errores críticos. Corrija estos errores antes de continuar.',
          puntuacionCalidad: 0
        };
        
        await this.guardarReporte(reporteErroresCriticos);
        return reporteErroresCriticos;
      }
      
      // 1. Identificar tipo de documento
      const tipoDocumento = await this.identificarTipoDocumento(texto);
      
      // 2. Clasificar tipo de pliego
      const tipoPliego = await this.clasificarTipoPliego(texto);
      
      // 3. Detectar lotes
      const infoLotes = this.detectarLotes(texto);
      
      // 4. Extraer campos variables
      const camposVariables = await this.extraerCamposVariables(texto, tipoDocumento.tipo);
      
      // 5. Validar estructura (solo si es un tipo conocido)
      let validacionEstructural = null;
      if (tipoPliego.tipo !== 'TIPO_DESCONOCIDO') {
        validacionEstructural = await this.validarEstructura(texto, tipoPliego.tipo);
      }
      
      // 6. Generar nombre sugerido según convención
      const nombreSugerido = tipoPliego.tipo !== 'TIPO_DESCONOCIDO' 
        ? this.generarNombreEstandar(tipoPliego.tipo, infoLotes.tieneLotes, categoriaDetectada)
        : null;
      
      // 7. Calcular puntuación global
      const puntuacionCalidad = this.calcularPuntuacionGlobal({
        tipoDocumento,
        tipoPliego,
        infoLotes,
        validacionEstructural
      });
      
      // 8. Generar recomendaciones
      const recomendaciones = this.generarRecomendaciones({
        tipoDocumento,
        tipoPliego,
        validacionEstructural,
        camposVariables,
        analisisNombre
      });
      
      const reporte = {
        id: uuidv4(),
        timestamp: new Date().toISOString(),
        metadatos,
        contextoValidacion: {
          contextId: contextId || null,
          contextoUsado: contextoValidacion,
          contextoSugerido: this.obtenerContextoRAG(categoriaDetectada)
        },
        nomenclatura: {
          analisisNombre,
          categoriaDetectada,
          nombreSugerido,
          contextoRAGSugerido: this.obtenerContextoRAG(categoriaDetectada),
          cumpleConvencion: analisisNombre?.esValido || false
        },
        validacionCompleta: true,
        erroresComunes, // Incluir resultado de pre-validación
        tipoDocumento,
        tipoPliego: `${tipoPliego.tipo}_${infoLotes.tieneLotes ? 'CON' : 'SIN'}_LOTES`,
        validacionEstructural,
        validacionLotes: {
          requiereLotes: infoLotes.tieneLotes,
          lotesPresentes: infoLotes.tieneLotes,
          numeroLotes: infoLotes.numeroLotes,
          confianza: infoLotes.confianza
        },
        camposVariables,
        puntuacionCalidad,
        recomendaciones,
        erroresCriticos: this.identificarErroresCriticos(validacionEstructural, camposVariables),
        advertencias: this.generarAdvertencias(tipoDocumento, validacionEstructural, analisisNombre)
      };
      
      // Guardar reporte
      await this.guardarReporte(reporte);
      
      console.log(`[PLIEGO-VALIDATION] ✅ Validación completada - Puntuación: ${puntuacionCalidad}`);
      
      return reporte;
      
    } catch (error) {
      console.error('[PLIEGO-VALIDATION] ❌ Error en validación:', error);
      throw error;
    }
  }

  // Métodos auxiliares
  esContenidoGenerico(texto) {
    const patronesGenericos = [
      /\[insertar\s+\w+\]/gi,
      /\(a\s+completar\)/gi,
      /xxx+/gi,
      /pendiente/gi,
      /\[fecha\]/gi,
      /\[importe\]/gi
    ];
    
    return patronesGenericos.some(patron => patron.test(texto));
  }

  extraerNumeroLotes(texto) {
    const matches = texto.match(/lote\s*(\d+)/gi) || [];
    const numeros = matches.map(m => parseInt(m.match(/\d+/)[0]));
    return numeros.length > 0 ? Math.max(...numeros) : 0;
  }

  encontrarUbicacion(texto, campo) {
    const index = texto.indexOf(campo);
    if (index === -1) return 'No encontrado';
    
    const antes = texto.substring(0, index);
    const lineas = antes.split('\n').length;
    return `Línea aproximada: ${lineas}`;
  }

  identificarCamposPorPatrones(texto) {
    const campos = [];
    
    // Patrones para identificar campos variables comunes
    const patrones = {
      'FECHA': /\d{1,2}\/\d{1,2}\/\d{4}/g,
      'IMPORTE': /\d+\.?\d*\s*€/g,
      'EXPEDIENTE': /EXP[\d\-\/]+/gi,
      'CPV': /CPV[\s:]*\d+/gi,
      'ORGANISMO': /[A-Z][a-z]+\s+[A-Z][a-z]+\s+de\s+[A-Z][a-z]+/g
    };
    
    for (const [tipo, patron] of Object.entries(patrones)) {
      const matches = texto.match(patron) || [];
      for (const match of matches) {
        campos.push({
          campo: match,
          tipo,
          ubicacion: this.encontrarUbicacion(texto, match),
          valor: match
        });
      }
    }
    
    return campos;
  }

  extraerEstructuraDocumento(texto) {
    // Extraer títulos y apartados principales
    const lineas = texto.split('\n');
    const estructura = [];
    
    for (const linea of lineas) {
      // Buscar patrones de títulos (números seguidos de punto y texto)
      const match = linea.match(/^\s*(\d+\.?\d*\.?)\s*(.+)/);
      if (match && match[2].trim().length > 3) {
        estructura.push({
          numero: match[1],
          titulo: match[2].trim(),
          linea: linea.trim()
        });
      }
    }
    
    return estructura;
  }

  verificarOrdenApartados(estructura, apartadosObligatorios) {
    // Simplificado: verificar que los apartados aparezcan en orden creciente
    let ultimoNumero = 0;
    
    for (const item of estructura) {
      const numero = parseFloat(item.numero.replace(/\.$/, ''));
      if (numero < ultimoNumero) {
        return false;
      }
      ultimoNumero = numero;
    }
    
    return true;
  }

  identificarApartadosAdicionales(estructura, apartadosObligatorios) {
    const obligatoriosNormalizados = apartadosObligatorios.map(a => 
      a.replace(/^\d+\.\s*/, '').toLowerCase()
    );
    
    return estructura
      .filter(item => {
        const tituloNormalizado = item.titulo.toLowerCase();
        return !obligatoriosNormalizados.some(obl => 
          tituloNormalizado.includes(obl.toLowerCase())
        );
      })
      .map(item => item.titulo);
  }

  calcularPuntuacionGlobal(datos) {
    let puntuacion = 0;
    
    // Puntuación por identificación correcta (20 puntos)
    if (datos.tipoPliego.confianza > 0.8) puntuacion += 20;
    else if (datos.tipoPliego.confianza > 0.6) puntuacion += 15;
    else if (datos.tipoPliego.confianza > 0.4) puntuacion += 10;
    
    // Puntuación por estructura (60 puntos)
    if (datos.validacionEstructural) {
      puntuacion += (datos.validacionEstructural.completitud * 0.6);
    }
    
    // Puntuación por lotes (20 puntos)
    if (datos.infoLotes.confianza > 0.8) puntuacion += 20;
    else if (datos.infoLotes.confianza > 0.5) puntuacion += 15;
    
    return Math.round(Math.min(puntuacion, 100));
  }

  generarRecomendaciones(datos) {
    const recomendaciones = [];
    
    // Recomendaciones de nomenclatura
    if (datos.analisisNombre && !datos.analisisNombre.esValido) {
      recomendaciones.push(`📝 Renombrar archivo siguiendo la convención: ${datos.analisisNombre.nombreSugerido || 'pliego_{tipo}_{modalidad}_{categoria}'}`);
      recomendaciones.push(`📂 Subir al contexto RAG sugerido: ${this.obtenerContextoRAG(datos.categoriaDetectada || 'VALIDACION')}`);
    }
    
    // Recomendaciones de clasificación
    if (datos.tipoPliego.confianza < 0.7) {
      recomendaciones.push('🔍 Revisar la clasificación del tipo de pliego - confianza baja');
    }
    
    // Recomendaciones estructurales
    if (datos.validacionEstructural && datos.validacionEstructural.apartadosFaltantes.length > 0) {
      recomendaciones.push(`📋 Añadir apartados faltantes: ${datos.validacionEstructural.apartadosFaltantes.join(', ')}`);
    }
    
    // Recomendaciones de campos variables
    if (datos.camposVariables.length === 0 && datos.tipoDocumento.tipo === 'PLIEGO_GENERADO') {
      recomendaciones.push('✅ Verificar que todos los campos variables han sido correctamente reemplazados');
    }
    
    // Recomendaciones específicas por categoría
    const categoria = datos.categoriaDetectada || 'VALIDACION';
    switch (categoria) {
      case 'PLANTILLA':
        recomendaciones.push('📚 Este documento servirá como plantilla base - verificar que no contenga datos específicos');
        break;
      case 'PLANTILLA_TAGS':
        recomendaciones.push('🏷️ Verificar que todos los tags SAP estén correctamente formateados');
        break;
      case 'GENERADO':
        recomendaciones.push('✨ Este documento puede usarse como referencia para futuras validaciones');
        break;
      case 'VALIDACION':
        recomendaciones.push('🔎 Documento listo para validación - revisar errores críticos y advertencias');
        break;
    }
    
    return recomendaciones;
  }

  identificarErroresCriticos(validacionEstructural, camposVariables) {
    const errores = [];
    
    if (validacionEstructural && validacionEstructural.completitud < 50) {
      errores.push('Estructura incompleta - faltan más del 50% de apartados obligatorios');
    }
    
    // Detectar tags SAP no reemplazados
    const tagsNoReemplazados = camposVariables.filter(c => 
      c.tipo === 'TAG_SAP' && c.valor === null
    );
    
    if (tagsNoReemplazados.length > 0) {
      errores.push(`Tags SAP no reemplazados: ${tagsNoReemplazados.length} encontrados`);
    }
    
    return errores;
  }

  generarAdvertencias(tipoDocumento, validacionEstructural) {
    const advertencias = [];
    
    if (tipoDocumento.confianza < 0.8) {
      advertencias.push('Confianza baja en la identificación del tipo de documento');
    }
    
    if (validacionEstructural && !validacionEstructural.ordenCorrecto) {
      advertencias.push('El orden de los apartados no sigue la secuencia esperada');
    }
    
    return advertencias;
  }

  async guardarCamposDetectados(campos) {
    try {
      // Crear directorio si no existe
      await fs.mkdir('./validation_results', { recursive: true });
      
      const archivo = './validation_results/campos_variables_detectados.txt';
      const timestamp = new Date().toISOString();
      
      let contenido = `\n=== CAMPOS DETECTADOS - ${timestamp} ===\n`;
      
      for (const campo of campos) {
        contenido += `CAMPO: ${campo.campo} | TIPO: ${campo.tipo} | UBICACIÓN: ${campo.ubicacion}\n`;
        
        // Actualizar mapa de frecuencias
        const key = campo.campo;
        this.camposVariablesDetectados.set(key, 
          (this.camposVariablesDetectados.get(key) || 0) + 1
        );
      }
      
      await fs.appendFile(archivo, contenido);
      
    } catch (error) {
      console.error('[PLIEGO-VALIDATION] Error guardando campos:', error);
    }
  }

  async guardarReporte(reporte) {
    try {
      await fs.mkdir('./validation_results', { recursive: true });
      
      const archivo = `./validation_results/reporte_validacion_${reporte.id}.json`;
      await fs.writeFile(archivo, JSON.stringify(reporte, null, 2));
      
      console.log(`[PLIEGO-VALIDATION] 📄 Reporte guardado: ${archivo}`);
      
    } catch (error) {
      console.error('[PLIEGO-VALIDATION] Error guardando reporte:', error);
    }
  }
}

// Instancia singleton
export const pliegoValidationService = new PliegoValidationService();
