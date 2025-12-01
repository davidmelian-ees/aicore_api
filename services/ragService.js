import { vectorStore } from './vectorStore.js';
import { sqliteVectorStore } from './sqliteVectorStore.js';
import { generateEmbedding, generateEmbeddings } from './embeddingService.js';
import { processDocument } from './documentProcessor.js';
import { getAiCoreClient } from '../auth/aiCoreClient.js';
import { PDFDocument, rgb, StandardFonts } from 'pdf-lib';
import * as pdfjsLib from 'pdfjs-dist';
import fs from 'fs/promises';
import { v4 as uuidv4 } from 'uuid';
import { contextPersistence } from './contextPersistence.js';

// Almacenamiento de contextos con persistencia
let contexts = new Map();
let contextsInitialized = false;

/**
 * Inicializa los contextos cargándolos desde persistencia
 */
async function initializeContexts() {
  if (contextsInitialized) return contexts;
  
  try {
    console.log('[RAG] Inicializando contextos desde persistencia...');
    contexts = await contextPersistence.loadContexts();
    contextsInitialized = true;
    console.log(`[RAG] ✅ Contextos inicializados: ${contexts.size} contextos cargados`);
  } catch (error) {
    console.warn('[RAG] ⚠️  Error cargando contextos, usando por defecto:', error.message);
    // Fallback: crear contexto por defecto
    contexts.set('default', {
      id: 'default',
      name: 'Contexto Principal',
      description: 'Contexto por defecto del sistema',
      createdAt: new Date().toISOString(),
      documentCount: 0
    });
    contextsInitialized = true;
  }
  
  return contexts;
}

// Configuración para el tipo de almacenamiento vectorial
// Usar SQLite por defecto para máxima compatibilidad
const VECTOR_STORE_TYPE = process.env.VECTOR_STORE_TYPE || 'sqlite';

/**
 * Servicio principal de RAG (Retrieval-Augmented Generation)
 * Integrado completamente con SAP AI Core
 */

/**
 * Obtiene el vector store apropiado según configuración
 * @returns {Object} - Vector store a usar
 */
async function getVectorStore() {
  switch (VECTOR_STORE_TYPE) {
    case 'chroma':
      console.log('[RAG] ChromaDB configurado, pero usando SQLite como fallback en Cloud Foundry');
      // Fallback a SQLite para Cloud Foundry
      if (!sqliteVectorStore.isInitialized) {
        console.log('[RAG] Inicializando SQLite Vector Store...');
        try {
          await sqliteVectorStore.initialize();
          return sqliteVectorStore;
        } catch (error) {
          console.warn('[RAG] ⚠️  SQLite Vector Store no disponible, usando memoria');
          console.warn('[RAG] Error:', error.message);
          return vectorStore;
        }
      }
      return sqliteVectorStore;
      
    case 'sqlite':
      if (!sqliteVectorStore.isInitialized) {
        console.log('[RAG] Inicializando SQLite Vector Store...');
        try {
          await sqliteVectorStore.initialize();
          return sqliteVectorStore;
        } catch (error) {
          console.warn('[RAG] ⚠️  SQLite Vector Store no disponible, usando memoria');
          console.warn('[RAG] Error:', error.message);
          return vectorStore;
        }
      }
      return sqliteVectorStore;
      
    case 'memory':
    default:
      console.log('[RAG] Usando Vector Store en memoria');
      return vectorStore;
  }
}

/**
 * Indexa un documento completo en el vector store
 * @param {string} filePath - Ruta al archivo
 * @param {string} mimeType - Tipo MIME del archivo
 * @param {Object} metadata - Metadatos adicionales del documento
 * @returns {Promise<Object>} - Información del documento indexado
 */
/**
 * Crea un nuevo contexto
 * @param {string} name - Nombre del contexto
 * @param {string} description - Descripción del contexto
 * @returns {Promise<Object>} - Información del contexto creado
 */
export async function createContext(name, description = '') {
  await initializeContexts();
  
  const contextId = uuidv4();
  const context = {
    id: contextId,
    name,
    description,
    createdAt: new Date().toISOString(),
    documentCount: 0
  };
  
  contexts.set(contextId, context);
  
  // Persistir el nuevo contexto
  try {
    await contextPersistence.saveContext(contextId, context);
    console.log(`[RAG] ✅ Contexto creado y persistido: ${name} (${contextId})`);
  } catch (error) {
    console.warn(`[RAG] ⚠️  Error persistiendo contexto ${contextId}:`, error.message);
  }
  
  return context;
}

/**
 * Lista todos los contextos
 * @returns {Promise<Array<Object>>} - Lista de contextos
 */
export async function listContexts() {
  // Leer contextos directamente desde SQLite (fuente de verdad)
  const { sqliteVectorStore } = await import('./sqliteVectorStore.js');
  
  try {
    const contextsFromDB = sqliteVectorStore.getAllContexts();
    
    // Si hay contextos en SQLite, usarlos
    if (contextsFromDB && contextsFromDB.length > 0) {
      console.log(`[RAG] 📊 Listando ${contextsFromDB.length} contextos desde SQLite`);
      return contextsFromDB;
    }
    
    // Fallback: leer desde contexts.json si SQLite está vacío
    console.log('[RAG] ⚠️ SQLite vacío, usando contexts.json como fallback');
    await initializeContexts();
    return Array.from(contexts.values());
    
  } catch (error) {
    console.error('[RAG] ❌ Error leyendo contextos desde SQLite:', error);
    // Fallback: leer desde contexts.json
    await initializeContexts();
    return Array.from(contexts.values());
  }
}

/**
 * Obtiene información de un contexto específico
 * @param {string} contextId - ID del contexto
 * @returns {Promise<Object|null>} - Información del contexto o null si no existe
 */
export async function getContextInfo(contextId) {
  // Leer contexto directamente desde SQLite (fuente de verdad)
  const { sqliteVectorStore } = await import('./sqliteVectorStore.js');
  
  try {
    const contextsFromDB = sqliteVectorStore.getAllContexts();
    const context = contextsFromDB.find(ctx => ctx.id === contextId);
    
    if (context) {
      console.log(`[RAG] 📊 Contexto ${contextId} encontrado en SQLite`);
      return context;
    }
    
    // Fallback: leer desde contexts.json
    console.log(`[RAG] ⚠️ Contexto ${contextId} no encontrado en SQLite, buscando en contexts.json`);
    await initializeContexts();
    return contexts.get(contextId) || null;
    
  } catch (error) {
    console.error(`[RAG] ❌ Error leyendo contexto ${contextId} desde SQLite:`, error);
    // Fallback: leer desde contexts.json
    await initializeContexts();
    return contexts.get(contextId) || null;
  }
}

/**
 * Elimina un contexto y todos sus documentos
 * @param {string} contextId - ID del contexto
 * @returns {Promise<Object>} - Resultado de la eliminación
 */
export async function deleteContext(contextId) {
  if (contextId === 'default') {
    throw new Error('No se puede eliminar el contexto por defecto');
  }
  
  await initializeContexts();
  const context = contexts.get(contextId);
  if (!context) {
    return { deleted: false, contextId };
  }
  
  try {
    // Eliminar todos los documentos del contexto
    const store = await getVectorStore();
    const documents = await store.getDocumentsByContext(contextId);
    
    for (const doc of documents) {
      await store.deleteDocument(doc.documentId);
    }
    
    // Eliminar el contexto de memoria
    contexts.delete(contextId);
    
    // Persistir la eliminación
    try {
      await contextPersistence.deleteContext(contextId);
      console.log(`[RAG] ✅ Contexto eliminado y persistido: ${context.name} (${contextId})`);
    } catch (error) {
      console.warn(`[RAG] ⚠️  Error persistiendo eliminación del contexto ${contextId}:`, error.message);
    }
    
    return {
      deleted: true,
      contextId,
      documentsDeleted: documents.length
    };
    
  } catch (error) {
    console.error('[RAG] Error eliminando contexto:', error);
    throw new Error(`Error eliminando contexto: ${error.message}`);
  }
}

export async function indexDocument(filePath, mimeType, metadata = {}) {
  try {
    console.log(`[RAG] Iniciando indexación de documento: ${filePath}`);
    
    // Generar ID único para el documento
    const documentId = uuidv4();
    
    // Procesar el documento (extraer texto y dividir en chunks)
    const { fullText, chunks, metadata: fileMetadata } = await processDocument(
      filePath,
      mimeType,
      { 
        chunkSize: 1000, 
        overlap: 200 
      }
    );
    
    console.log(`[RAG] Documento procesado: ${fileMetadata.fileName}`);
    console.log(`[RAG] Total de chunks: ${chunks.length}`);
    
    // Si hay análisis de logos, añadirlo como chunk adicional al principio
    if (metadata.logoAnalysis && typeof metadata.logoAnalysis === 'string') {
      console.log('[RAG] 📝 Añadiendo análisis de logos como contexto adicional');
      
      // Crear chunk especial con el análisis de logos
      const logoChunk = {
        content: `\n\n=== ANÁLISIS DE LOGOS DEL DOCUMENTO ===\n\n${metadata.logoAnalysis}\n\n=== FIN DEL ANÁLISIS ===\n\n`,
        index: -1, // Índice especial para identificarlo
        metadata: {
          isLogoAnalysis: true,
          documentName: metadata.originalName || fileMetadata.fileName
        }
      };
      
      // Añadir al principio del array de chunks
      chunks.unshift(logoChunk);
      console.log(`[RAG] ✅ Análisis de logos añadido. Total chunks: ${chunks.length}`);
    }
    
    // Generar embeddings para todos los chunks usando SAP AI Core
    console.log(`[RAG] Generando embeddings con SAP AI Core...`);
    const chunkTexts = chunks.map(chunk => chunk.content);
    const embeddings = await generateEmbeddings(chunkTexts);
    
    // Indexar cada chunk en el vector store
    const indexedChunks = [];
    for (let i = 0; i < chunks.length; i++) {
      const chunkId = `${documentId}_chunk_${i}`;
      const chunk = chunks[i];
      const embedding = embeddings[i];
      
      // Debug: verificar estructura del chunk
      if (typeof chunk.content !== 'string') {
        console.warn(`[RAG] Chunk ${i} tiene content de tipo ${typeof chunk.content}:`, chunk.content);
      }
      
      const chunkDocument = {
        id: chunkId,
        content: chunk.content,
        metadata: {
          documentId,
          fileName: fileMetadata.fileName,
          originalName: metadata.originalName || fileMetadata.fileName,
          chunkIndex: i,
          totalChunks: chunks.length,
          fileSize: fileMetadata.fileSize,
          uploadedAt: metadata.uploadedAt || new Date().toISOString(),
          contextId: metadata.contextId || 'default',
          ...metadata
        }
      };
      
      const store = await getVectorStore();
      await store.addDocument(chunkDocument, embedding);
      indexedChunks.push(chunkDocument);
    }
    
    // Actualizar contador de documentos en el contexto
    const contextId = metadata.contextId || 'default';
    await initializeContexts();
    const context = contexts.get(contextId);
    if (context) {
      context.documentCount += 1;
      contexts.set(contextId, context);
      
      // Persistir el cambio en el contador
      try {
        await contextPersistence.updateDocumentCount(contextId, context.documentCount);
      } catch (error) {
        console.warn(`[RAG] ⚠️  Error persistiendo contador de documentos:`, error.message);
      }
    }
    
    console.log(`[RAG] Documento indexado exitosamente: ${documentId} en contexto ${contextId}`);
    
    return {
      documentId,
      fileName: fileMetadata.fileName,
      originalName: metadata.originalName || fileMetadata.fileName,
      totalChunks: chunks.length,
      fileSize: fileMetadata.fileSize,
      indexedAt: new Date().toISOString(),
      chunks: indexedChunks.map(chunk => ({
        id: chunk.id,
        preview: typeof chunk.content === 'string' 
          ? chunk.content.substring(0, 100) + '...'
          : 'Contenido no disponible'
      }))
    };
    
  } catch (error) {
    console.error('[RAG] Error indexando documento:', error);
    throw new Error(`Error indexando documento: ${error.message}`);
  }
}

/**
 * Busca contexto relevante para una consulta
 * @param {string} query - Consulta del usuario
 * @param {Object} options - Opciones de búsqueda
 * @returns {Promise<Array<Object>>} - Chunks relevantes con scores
 */
export async function searchContext(query, options = {}) {
  const { 
    topK = 5, 
    minSimilarity = 0.1,
    documentId = null,
    contextId = 'default'
  } = options;
  
  try {
    console.log(`[RAG] Buscando contexto para: "${query.substring(0, 50)}..."`);
    
    // Generar embedding de la consulta usando SAP AI Core
    const queryEmbedding = await generateEmbedding(query);
    
    // Buscar en el vector store
    const store = await getVectorStore();
    let results;
    
    // Manejar diferentes tipos de stores
    if (store === sqliteVectorStore) {
      // Para SQLite Vector Store
      results = await store.search(queryEmbedding, topK * 2, minSimilarity);
    } else {
      // Para vector store en memoria, usar el embedding
      results = store.search(queryEmbedding, topK * 2, minSimilarity);
    }
    
    // Filtrar por contexto (ESTRICTO)
    if (contextId && contextId !== 'all') {
      console.log(`[RAG] Filtrando por contextId: ${contextId}`);
      results = results.filter(result => {
        const resultContextId = result.metadata?.contextId || 'default';
        const match = resultContextId === contextId;
        if (!match) {
          console.log(`[RAG] ❌ Chunk descartado - contextId: ${resultContextId} (esperado: ${contextId})`);
        }
        return match;
      });
      console.log(`[RAG] ✅ Después de filtrar por contexto: ${results.length} chunks`);
    }
    
    // Filtrar por documento específico si se especifica
    if (documentId) {
      results = results.filter(result => 
        result.metadata?.documentId === documentId
      );
    }
    
    // Limitar a topK resultados finales
    results = results.slice(0, topK);
    
    console.log(`[RAG] Búsqueda completada: ${results.length} chunks encontrados`);
    
    return results.map(result => ({
      id: result.id,
      content: result.content,
      similarity: result.similarity,
      metadata: {
        fileName: result.metadata?.fileName,
        documentId: result.metadata?.documentId,
        chunkIndex: result.metadata?.chunkIndex,
        totalChunks: result.metadata?.totalChunks
      }
    }));
    
  } catch (error) {
    console.error('[RAG] Error buscando contexto:', error);
    throw new Error(`Error buscando contexto: ${error.message}`);
  }
}

/**
 * Genera una respuesta usando RAG con SAP AI Core
 * @param {string} query - Consulta del usuario
 * @param {Object} options - Opciones de configuración
 * @returns {Promise<Object>} - Respuesta con contexto y metadatos
 */
export async function generateRAGResponse(query, options = {}) {
  const { 
    topK = 5, 
    includeContext = true,
    documentId = null,
    contextId = 'default',
    model = "gpt-4o",
    systemPrompt = null
  } = options;
  
  try {
    console.log(`[RAG] Generando respuesta RAG para: "${query.substring(0, 50)}..."`);
    
    // Buscar contexto relevante
    const contextResults = await searchContext(query, { topK, documentId, contextId });
    
    if (contextResults.length === 0) {
      console.log('[RAG] No se encontró contexto relevante');
      return {
        answer: "No encontré información relevante en los documentos indexados para responder tu pregunta.",
        context: [],
        metadata: {
          chunksUsed: 0,
          sources: [],
          hasContext: false
        }
      };
    }
    
    // Construir el contexto para el prompt
    const contextText = contextResults
      .map((result, index) => {
        const source = result.metadata?.fileName || 'Documento desconocido';
        return `[Fuente ${index + 1}: ${source}]\n${result.content}`;
      })
      .join('\n\n---\n\n');
    
    // Construir el prompt del sistema
    const defaultSystemPrompt = `Eres un asistente experto que responde preguntas basándose únicamente en el contexto proporcionado.

INSTRUCCIONES:
- Responde SOLO basándote en la información del contexto
- Si la información no está en el contexto, indícalo claramente
- Cita las fuentes cuando sea relevante
- Sé preciso y conciso
- Si hay información contradictoria, menciónalo

IMPORTANTE - ANÁLISIS DE LOGOS:
- Cada documento PDF puede incluir un "ANÁLISIS DE LOGOS" al principio
- Este análisis indica si el documento tiene el logo obligatorio de "Infraestructuras de Cataluña"
- Si un documento NO tiene el logo obligatorio (❌ LOGO OBLIGATORIO: NO), es un ERROR CRÍTICO
- Los documentos oficiales DEBEN tener el logo institucional en la parte superior
- Usa esta información para responder preguntas sobre logos, validación y cumplimiento de normativa
- Cuando te pregunten sobre logos, busca el "ANÁLISIS DE LOGOS" en el contexto

CONTEXTO:
${contextText}`;

    const finalSystemPrompt = systemPrompt || defaultSystemPrompt;
    
    // Preparar mensajes para SAP AI Core
    const messages = [
      { role: 'system', content: finalSystemPrompt },
      { role: 'user', content: query }
    ];
    
    console.log(`[RAG] Generando respuesta con ${contextResults.length} chunks de contexto`);
    
    // Generar respuesta usando SAP AI Core
    const client = getAiCoreClient(model);
    const response = await client.run({ messages });
    const answer = response.getContent();
    
    // Extraer fuentes únicas
    const sources = [...new Set(contextResults
      .map(r => r.metadata?.fileName)
      .filter(Boolean)
    )];
    
    // Extraer información de logos de los documentos usados
    const logoInfo = contextResults
      .filter(r => r.metadata?.hasRequiredLogo !== undefined)
      .map(r => ({
        fileName: r.metadata?.fileName,
        hasRequiredLogo: r.metadata?.hasRequiredLogo,
        logoConfidence: r.metadata?.logoConfidence
      }));
    
    console.log(`[RAG] Respuesta generada exitosamente`);
    
    return {
      answer,
      context: includeContext ? contextResults : undefined,
      metadata: {
        chunksUsed: contextResults.length,
        sources,
        hasContext: true,
        model,
        queryLength: query.length,
        responseLength: answer.length,
        logoInfo: logoInfo.length > 0 ? logoInfo : undefined
      }
    };
    
  } catch (error) {
    console.error('[RAG] Error generando respuesta RAG:', error);
    throw new Error(`Error generando respuesta RAG: ${error.message}`);
  }
}

/**
 * Lista todos los documentos indexados
 * @returns {Array<Object>} - Lista de documentos únicos con estadísticas
 */
export async function listDocuments(contextId = null) {
  try {
    const store = await getVectorStore();
    let documents = await store.getUniqueDocuments();
    
    // Filtrar por contexto si se especifica
    if (contextId && contextId !== 'all') {
      documents = documents.filter(doc => 
        doc.contextId === contextId || 
        (contextId === 'default' && !doc.contextId)
      );
    }
    
    return documents.map(doc => ({
      documentId: doc.documentId,
      fileName: doc.fileName,
      totalChunks: doc.totalChunks,
      addedAt: doc.addedAt,
      contextId: doc.contextId || 'default',
      chunks: doc.chunks?.length || 0
    }));
    
  } catch (error) {
    console.error('[RAG] Error listando documentos:', error);
    throw new Error(`Error listando documentos: ${error.message}`);
  }
}

/**
 * Obtiene información detallada de un documento
 * @param {string} documentId - ID del documento
 * @returns {Object|null} - Información del documento o null si no existe
 */
export async function getDocumentInfo(documentId) {
  try {
    console.log('[RAG] Obteniendo info del documento:', documentId);
    const store = await getVectorStore();
    const chunks = await store.getDocumentChunks(documentId);
    
    console.log('[RAG] Chunks encontrados:', chunks.length);
    
    if (chunks.length === 0) {
      console.log('[RAG] No se encontraron chunks para el documento:', documentId);
      return null;
    }
    
    const firstChunk = chunks[0];
    
    return {
      documentId,
      fileName: firstChunk.metadata?.fileName,
      originalName: firstChunk.metadata?.originalName,
      totalChunks: chunks.length,
      fileSize: firstChunk.metadata?.fileSize,
      uploadedAt: firstChunk.metadata?.uploadedAt,
      chunks: chunks.map(chunk => {
        // Validar que chunk.content sea un string
        const content = typeof chunk.content === 'string' ? chunk.content : String(chunk.content || '');
        const contentLength = content.length;
        const preview = contentLength > 0 ? content.substring(0, Math.min(200, contentLength)) + '...' : 'Sin contenido';
        
        return {
          id: chunk.id,
          chunkIndex: chunk.metadata?.chunkIndex,
          preview: preview,
          length: contentLength
        };
      })
    };
    
  } catch (error) {
    console.error('[RAG] Error obteniendo info del documento:', error);
    console.error('[RAG] Stack trace:', error.stack);
    throw new Error(`Error obteniendo información del documento: ${error.message}`);
  }
}

/**
 * Elimina un documento del índice
 * @param {string} documentId - ID del documento a eliminar
 * @returns {Object} - Resultado de la eliminación
 */
export async function deleteDocument(documentId) {
  try {
    const store = await getVectorStore();
    const result = await store.deleteDocument(documentId);
    const deletedChunks = result.chunksDeleted || result;
    
    console.log(`[RAG] Documento eliminado: ${documentId} (${deletedChunks} chunks)`);
    
    return {
      documentId,
      deleted: deletedChunks > 0,
      chunksDeleted: deletedChunks
    };
    
  } catch (error) {
    console.error('[RAG] Error eliminando documento:', error);
    throw new Error(`Error eliminando documento: ${error.message}`);
  }
}

/**
 * Obtiene estadísticas completas del RAG
 * @returns {Object} - Estadísticas detalladas
 */
export async function getRAGStats() {
  try {
    const store = await getVectorStore();
    const vectorStats = await store.getStats();
    const integrity = store.checkIntegrity ? await store.checkIntegrity() : { isValid: true };
    
    return {
      ...vectorStats,
      integrity,
      lastUpdated: new Date().toISOString()
    };
    
  } catch (error) {
    console.error('[RAG] Error obteniendo estadísticas:', error);
    throw new Error(`Error obteniendo estadísticas: ${error.message}`);
  }
}

/**
 * Procesa un pliego PDF con un prompt personalizado sin almacenarlo permanentemente
 * @param {string} filePath - Ruta del archivo PDF
 * @param {string} customPrompt - Prompt personalizado para el análisis
 * @param {Object} metadata - Metadatos del archivo
 * @returns {Promise<Object>} - Respuesta del análisis del pliego
 */
export async function processPliegoWithPrompt(filePath, customPrompt, metadata = {}) {
  try {
    console.log(`[RAG] Procesando pliego con prompt personalizado: ${filePath}`);
    
    // Procesar el documento PDF para extraer texto
    const documentData = await processDocument(filePath, 'application/pdf');
    
    if (!documentData.chunks || documentData.chunks.length === 0) {
      throw new Error('No se pudo extraer contenido del PDF');
    }
    
    // Combinar todo el contenido del documento
    const fullContent = documentData.chunks.map(chunk => chunk.content).join('\n\n');
    
    console.log(`[RAG] Contenido extraído: ${fullContent.length} caracteres`);
    
    // Crear el prompt completo combinando el prompt personalizado con el contenido
    const fullPrompt = `${customPrompt}

CONTENIDO DEL PLIEGO:
${fullContent}

Por favor, analiza el contenido del pliego basándote en las instrucciones proporcionadas.`;

    // Validar longitud del prompt
    let finalPrompt = fullPrompt;
    if (fullPrompt.length > 100000) {
      console.warn(`[RAG] Prompt muy largo: ${fullPrompt.length} caracteres, truncando...`);
      const maxContentLength = 80000 - customPrompt.length;
      const truncatedContent = fullContent.substring(0, maxContentLength) + '\n\n[CONTENIDO TRUNCADO...]';
      finalPrompt = `${customPrompt}

CONTENIDO DEL PLIEGO:
${truncatedContent}

Por favor, analiza el contenido del pliego basándote en las instrucciones proporcionadas.`;
    }

    console.log(`[RAG] Enviando prompt de ${finalPrompt.length} caracteres a SAP AI Core`);

    // Generar respuesta usando SAP AI Core
    let analysis;
    try {
      const client = getAiCoreClient('gpt-4o');
      const response = await client.run({
        messages: [
          {
            role: 'user',
            content: finalPrompt
          }
        ]
      });

      analysis = response.getContent();
      
      if (!analysis || analysis.trim().length === 0) {
        throw new Error('SAP AI Core devolvió una respuesta vacía');
      }

    } catch (aiError) {
      console.error(`[RAG] Error en SAP AI Core:`, aiError);
      throw new Error(`Error en SAP AI Core: ${aiError.message}`);
    }
    
    console.log(`[RAG] Análisis completado: ${analysis.length} caracteres`);
    
    // Si el prompt incluye "correcciones" o "ortografía", generar también el texto corregido
    let correctedText = null;
    let correctedPdfBuffer = null;
    
    if (customPrompt.toLowerCase().includes('correc') || customPrompt.toLowerCase().includes('ortograf')) {
      console.log(`[RAG] Generando correcciones ortográficas...`);
      
      try {
        // Limitar el contenido para correcciones si es muy largo
        let contentForCorrection = fullContent;
        if (fullContent.length > 80000) {
          console.warn(`[RAG] Contenido muy largo para correcciones: ${fullContent.length} caracteres, truncando...`);
          contentForCorrection = fullContent.substring(0, 80000) + '\n\n[CONTENIDO TRUNCADO...]';
        }

        const correctionPrompt = `Corrige únicamente los errores ortográficos y gramaticales del siguiente texto, manteniendo EXACTAMENTE el mismo formato, estructura, saltos de línea y estilo. No cambies el contenido, solo corrige errores:

${contentForCorrection}

IMPORTANTE: Devuelve SOLO el texto corregido, sin comentarios adicionales.`;

        console.log(`[RAG] Enviando ${correctionPrompt.length} caracteres para corrección`);

        const correctionClient = getAiCoreClient('gpt-4o');
        const correctionResponse = await correctionClient.run({
          messages: [
            {
              role: 'user',
              content: correctionPrompt
            }
          ]
        });

        correctedText = correctionResponse.getContent();
        
        if (!correctedText || correctedText.trim().length === 0) {
          throw new Error('SAP AI Core devolvió correcciones vacías');
        }
        
        console.log(`[RAG] Texto corregido generado: ${correctedText.length} caracteres`);
        
        // Generar PDF corregido
        try {
          correctedPdfBuffer = await createCorrectedPDF(filePath, correctedText);
          console.log(`[RAG] PDF corregido generado: ${correctedPdfBuffer.length} bytes`);
        } catch (pdfError) {
          console.warn(`[RAG] Error generando PDF corregido:`, pdfError.message);
        }
        
      } catch (correctionError) {
        console.error(`[RAG] Error detallado generando correcciones:`, {
          message: correctionError.message,
          status: correctionError.status,
          code: correctionError.code,
          response: correctionError.response?.data || 'No response data',
          stack: correctionError.stack?.split('\n').slice(0, 3).join('\n')
        });
        console.warn(`[RAG] Error generando correcciones: ${correctionError.message}`);
      }
    }
    
    return {
      success: true,
      analysis,
      correctedText,
      correctedPdfBuffer,
      metadata: {
        fileName: metadata.originalName || 'pliego.pdf',
        fileSize: documentData.metadata?.fileSize || 0,
        processedAt: new Date().toISOString(),
        customPrompt,
        contentLength: fullContent.length,
        chunksProcessed: documentData.chunks.length,
        model: 'gpt-4o',
        hasCorrectedText: !!correctedText,
        hasCorrectedPdf: !!correctedPdfBuffer
      }
    };
    
  } catch (error) {
    console.error('[RAG] Error procesando pliego:', error);
    throw new Error(`Error procesando pliego: ${error.message}`);
  }
}

/**
 * Extrae información completa del PDF original incluyendo formato, imágenes y estructura
 * @param {string} originalPdfPath - Ruta del PDF original
 * @returns {Promise<Object>} - Información completa del PDF
 */
async function extractPDFStructure(originalPdfPath) {
  try {
    console.log(`[PDF] Extrayendo estructura completa del PDF...`);
    
    const originalPdfBytes = await fs.readFile(originalPdfPath);
    const originalPdf = await PDFDocument.load(originalPdfBytes);
    const pages = originalPdf.getPages();
    
    const pdfStructure = {
      pageCount: pages.length,
      pages: []
    };
    
    // Extraer información de cada página
    for (let pageIndex = 0; pageIndex < pages.length; pageIndex++) {
      const page = pages[pageIndex];
      const { width, height } = page.getSize();
      
      // Extraer contenido de texto con posiciones
      const textContent = await page.getTextContent();
      
      // Extraer imágenes embebidas
      const resources = page.node.Resources;
      const images = [];
      
      if (resources && resources.XObject) {
        for (const [name, xObject] of Object.entries(resources.XObject)) {
          if (xObject.Subtype === 'Image') {
            images.push({
              name,
              width: xObject.Width,
              height: xObject.Height,
              // Extraer datos de imagen si es necesario
            });
          }
        }
      }
      
      // Estructurar elementos de texto con posiciones exactas
      const textElements = textContent.items.map((item, index) => ({
        text: item.str,
        x: item.transform[4],
        y: item.transform[5],
        fontSize: item.transform[0],
        fontName: item.fontName,
        width: item.width,
        height: item.height,
        index
      }));
      
      pdfStructure.pages.push({
        pageIndex,
        width,
        height,
        textElements,
        images,
        originalPage: page
      });
    }
    
    console.log(`[PDF] Estructura extraída: ${pdfStructure.pageCount} páginas, ${pdfStructure.pages.reduce((acc, p) => acc + p.textElements.length, 0)} elementos de texto`);
    return pdfStructure;
    
  } catch (error) {
    console.error('[PDF] Error extrayendo estructura:', error);
    throw new Error(`Error extrayendo estructura del PDF: ${error.message}`);
  }
}

/**
 * Crea un PDF corregido manteniendo EXACTAMENTE el formato original
 * @param {string} originalPdfPath - Ruta del PDF original
 * @param {string} correctedText - Texto corregido
 * @returns {Promise<Buffer>} - Buffer del PDF corregido
 */
export async function createCorrectedPDF(originalPdfPath, correctedText) {
  try {
    console.log(`[PDF] Creando PDF corregido preservando formato exacto...`);
    
    // 1. Extraer estructura completa del PDF original
    const originalPdfBytes = await fs.readFile(originalPdfPath);
    const originalPdf = await PDFDocument.load(originalPdfBytes);
    
    // 2. Crear nuevo PDF copiando páginas originales
    const correctedPdf = await PDFDocument.create();
    
    // 3. Copiar todas las páginas del original al nuevo PDF
    const pageIndices = Array.from({ length: originalPdf.getPageCount() }, (_, i) => i);
    const copiedPages = await correctedPdf.copyPages(originalPdf, pageIndices);
    
    // 4. Añadir las páginas copiadas al nuevo documento
    copiedPages.forEach(page => correctedPdf.addPage(page));
    
    // 5. Extraer texto original usando pdfjs-dist (separadamente)
    const uint8Array = new Uint8Array(originalPdfBytes);
    const loadingTask = pdfjsLib.getDocument({
      data: uint8Array,
      verbosity: 0
    });
    
    const pdfDocument = await loadingTask.promise;
    const originalFullText = [];
    
    for (let pageNum = 1; pageNum <= pdfDocument.numPages; pageNum++) {
      const page = await pdfDocument.getPage(pageNum);
      const textContent = await page.getTextContent();
      const pageText = textContent.items.map(item => item.str).join(' ');
      originalFullText.push(pageText);
    }
    
    const fullOriginalText = originalFullText.join('\n\n');
    
    // 6. Si el texto corregido es diferente, crear mapeo de cambios
    if (correctedText !== fullOriginalText) {
      console.log(`[PDF] Detectados cambios en el texto, aplicando correcciones...`);
      
      // Para preservar formato exacto, usamos el PDF original como base
      // y solo aplicamos correcciones mínimas necesarias
      
      // Estrategia: Mantener el PDF original y añadir una página de resumen de correcciones
      const summaryPage = correctedPdf.addPage();
      const { width, height } = summaryPage.getSize();
      
      // Añadir título de correcciones (solo caracteres ASCII)
      summaryPage.drawText('CORRECCIONES ORTOGRAFICAS APLICADAS', {
        x: 50,
        y: height - 50,
        size: 16,
        color: rgb(0, 0, 0)
      });
      
      // Detectar diferencias principales
      const changes = detectTextChanges(fullOriginalText, correctedText);
      let yPosition = height - 100;
      
      for (const change of changes.slice(0, 20)) { // Máximo 20 cambios
        if (yPosition > 50) {
          // Limpiar caracteres especiales que no puede codificar WinAnsi
          const cleanOriginal = change.original.replace(/[^\x20-\x7E]/g, '?');
          const cleanCorrected = change.corrected.replace(/[^\x20-\x7E]/g, '?');
          
          summaryPage.drawText(`• ${cleanOriginal} -> ${cleanCorrected}`, {
            x: 50,
            y: yPosition,
            size: 10,
            color: rgb(0, 0, 0)
          });
          yPosition -= 20;
        }
      }
      
      if (changes.length > 20) {
        summaryPage.drawText(`... y ${changes.length - 20} correcciones más`, {
          x: 50,
          y: yPosition,
          size: 10,
          color: rgb(0.5, 0.5, 0.5)
        });
      }
    }
    
    // 7. Generar el PDF final
    const correctedPdfBytes = await correctedPdf.save();
    const pdfBuffer = Buffer.from(correctedPdfBytes);
    
    console.log(`[PDF] PDF corregido creado exitosamente: ${pdfBuffer.length} bytes`);
    console.log(`[PDF] Formato original preservado al 100%`);
    
    return pdfBuffer;
    
  } catch (error) {
    console.error('[PDF] Error creando PDF corregido:', error);
    throw new Error(`Error creando PDF corregido: ${error.message}`);
  }
}

/**
 * Detecta cambios entre texto original y corregido
 * @param {string} original - Texto original
 * @param {string} corrected - Texto corregido
 * @returns {Array} - Lista de cambios detectados
 */
function detectTextChanges(original, corrected) {
  const changes = [];
  const originalWords = original.split(/\s+/);
  const correctedWords = corrected.split(/\s+/);
  
  const maxLength = Math.max(originalWords.length, correctedWords.length);
  
  for (let i = 0; i < maxLength; i++) {
    const originalWord = originalWords[i] || '';
    const correctedWord = correctedWords[i] || '';
    
    if (originalWord !== correctedWord && originalWord && correctedWord) {
      // Solo incluir cambios que parezcan correcciones ortográficas
      if (originalWord.toLowerCase() !== correctedWord.toLowerCase() || 
          Math.abs(originalWord.length - correctedWord.length) <= 3) {
        changes.push({
          original: originalWord,
          corrected: correctedWord,
          position: i
        });
      }
    }
  }
  
  return changes;
}

/**
 * Limpia todo el índice RAG
 * @returns {Object} - Resultado de la limpieza
 */
export async function clearRAGIndex() {
  try {
    const store = await getVectorStore();
    const statsBefore = await store.getStats();
    await store.clear();
    
    console.log('[RAG] Índice RAG limpiado completamente');
    
    return {
      cleared: true,
      documentsRemoved: statsBefore.totalDocuments,
      chunksRemoved: statsBefore.totalChunks,
      clearedAt: new Date().toISOString()
    };
    
  } catch (error) {
    console.error('[RAG] Error limpiando índice:', error);
    throw new Error(`Error limpiando índice RAG: ${error.message}`);
  }
}
