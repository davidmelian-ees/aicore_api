import { AzureOpenAiEmbeddingClient } from "@sap-ai-sdk/foundation-models";
import xsenv from "@sap/xsenv";

// Cargar variables de entorno
xsenv.loadEnv();

let embeddingClient;

/**
 * Inicializa el cliente de embeddings de SAP AI Core
 * @param {string} model - Modelo de embeddings a usar
 * @returns {AzureOpenAiEmbeddingClient} - Cliente de embeddings
 */
function getEmbeddingClient(model = "text-embedding-3-small") {
  if (!embeddingClient) {
    try {
      embeddingClient = new AzureOpenAiEmbeddingClient(model);
      console.log(`[EMBEDDINGS] Cliente inicializado con modelo: ${model}`);
    } catch (error) {
      console.error("[EMBEDDINGS] Error inicializando cliente:", error);
      throw error;
    }
  }
  return embeddingClient;
}

/**
 * Genera embeddings para un texto usando SAP AI Core o fallback local
 * @param {string} text - Texto para generar embedding
 * @param {string} model - Modelo de embeddings (opcional)
 * @returns {Promise<Array<number>>} - Vector de embedding
 */
export async function generateEmbedding(text, model = "text-embedding-3-small") {
  // Validar que el input sea un string
  if (typeof text !== 'string') {
    throw new Error(`generateEmbedding espera un string, recibió: ${typeof text}. Valor: ${JSON.stringify(text)}`);
  }
  
  try {
    const client = getEmbeddingClient(model);
    
    // Normalizar el texto
    const normalizedText = text.trim().replace(/\s+/g, ' ');
    
    if (!normalizedText) {
      throw new Error("El texto no puede estar vacío");
    }
    
    console.log(`[EMBEDDINGS] Generando embedding para texto de ${normalizedText.length} caracteres`);
    
    // Intentar generar embedding usando SAP AI Core
    const response = await client.run({
      input: normalizedText
    });
    
    // Extraer el vector de embedding
    const embedding = response.getEmbedding();
    
    console.log(`[EMBEDDINGS] Embedding generado con SAP AI Core: dimensión ${embedding.length}`);
    
    return embedding;
    
  } catch (error) {
    console.error("[EMBEDDINGS] Error con SAP AI Core, usando fallback local:", error.message);
    
    // Fallback: generar embedding local
    return generateLocalEmbedding(text);
  }
}

/**
 * Genera embeddings para múltiples textos
 * @param {Array<string>} texts - Array de textos
 * @param {string} model - Modelo de embeddings (opcional)
 * @returns {Promise<Array<Array<number>>>} - Array de embeddings
 */
export async function generateEmbeddings(texts, model = "text-embedding-3-small") {
  try {
    console.log(`[EMBEDDINGS] Generando embeddings para ${texts.length} textos`);
    
    const embeddings = [];
    
    // Procesar en lotes para evitar límites de rate
    const batchSize = 10;
    for (let i = 0; i < texts.length; i += batchSize) {
      const batch = texts.slice(i, i + batchSize);
      
      console.log(`[EMBEDDINGS] Procesando lote ${Math.floor(i/batchSize) + 1}/${Math.ceil(texts.length/batchSize)}`);
      
      const batchPromises = batch.map(text => generateEmbedding(text, model));
      const batchEmbeddings = await Promise.all(batchPromises);
      
      embeddings.push(...batchEmbeddings);
      
      // Pequeña pausa entre lotes para evitar rate limiting (solo si no es fallback local)
      if (i + batchSize < texts.length) {
        await new Promise(resolve => setTimeout(resolve, 100));
      }
    }
    
    console.log(`[EMBEDDINGS] Completado: ${embeddings.length} embeddings generados`);
    
    return embeddings;
    
  } catch (error) {
    console.error("[EMBEDDINGS] Error generando embeddings múltiples:", error);
    throw error;
  }
}

/**
 * Calcula la similitud de coseno entre dos vectores de embedding
 * @param {Array<number>} embeddingA - Primer embedding
 * @param {Array<number>} embeddingB - Segundo embedding
 * @returns {number} - Similitud de coseno (0-1)
 */
export function calculateCosineSimilarity(embeddingA, embeddingB) {
  if (embeddingA.length !== embeddingB.length) {
    throw new Error('Los embeddings deben tener la misma dimensión');
  }

  let dotProduct = 0;
  let normA = 0;
  let normB = 0;

  for (let i = 0; i < embeddingA.length; i++) {
    dotProduct += embeddingA[i] * embeddingB[i];
    normA += embeddingA[i] * embeddingA[i];
    normB += embeddingB[i] * embeddingB[i];
  }

  normA = Math.sqrt(normA);
  normB = Math.sqrt(normB);

  if (normA === 0 || normB === 0) {
    return 0;
  }

  return dotProduct / (normA * normB);
}

/**
 * Busca los embeddings más similares a una consulta
 * @param {Array<number>} queryEmbedding - Embedding de la consulta
 * @param {Array<{embedding: Array<number>, metadata: Object}>} documents - Documentos con embeddings
 * @param {number} topK - Número de resultados a retornar
 * @returns {Array<{similarity: number, metadata: Object}>} - Documentos más similares
 */
export function findSimilarEmbeddings(queryEmbedding, documents, topK = 5) {
  try {
    const similarities = documents.map((doc, index) => ({
      index,
      similarity: calculateCosineSimilarity(queryEmbedding, doc.embedding),
      metadata: doc.metadata
    }));

    // Ordenar por similitud descendente
    similarities.sort((a, b) => b.similarity - a.similarity);

    // Retornar top K resultados
    return similarities.slice(0, topK);
    
  } catch (error) {
    console.error("[EMBEDDINGS] Error buscando similares:", error);
    throw error;
  }
}

/**
 * Valida que un embedding tenga el formato correcto
 * @param {Array<number>} embedding - Embedding a validar
 * @returns {boolean} - True si es válido
 */
export function validateEmbedding(embedding) {
  return Array.isArray(embedding) && 
         embedding.length > 0 && 
         embedding.every(val => typeof val === 'number' && !isNaN(val));
}

/**
 * Genera un embedding local simple para fallback
 * @param {string} text - Texto para generar embedding
 * @returns {Array<number>} - Vector de embedding local
 */
function generateLocalEmbedding(text) {
  // Validar que el input sea un string
  if (typeof text !== 'string') {
    throw new Error(`generateLocalEmbedding espera un string, recibió: ${typeof text}. Valor: ${JSON.stringify(text)}`);
  }
  
  console.log(`[EMBEDDINGS] Generando embedding local para texto de ${text.length} caracteres`);
  
  // Normalizar texto
  const normalized = text.toLowerCase().trim();
  
  // Dimensión del embedding (compatible con modelos típicos)
  const dimension = 384;
  const embedding = new Array(dimension).fill(0);
  
  // Generar embedding basado en características del texto
  
  // 1. Longitud del texto
  embedding[0] = Math.min(normalized.length / 1000, 1);
  
  // 2. Frecuencia de palabras
  const words = normalized.split(/\s+/);
  embedding[1] = Math.min(words.length / 100, 1);
  
  // 3. Hash de caracteres distribuido en el vector
  for (let i = 0; i < normalized.length; i++) {
    const charCode = normalized.charCodeAt(i);
    const index = (charCode * (i + 1)) % dimension;
    embedding[index] += 1 / normalized.length;
  }
  
  // 4. Distribución de n-gramas (bigramas)
  for (let i = 0; i < words.length; i++) {
    const word = words[i];
    for (let j = 0; j < word.length - 1; j++) {
      const bigram = word.substring(j, j + 2);
      const hash = hashString(bigram);
      const index = hash % dimension;
      embedding[index] += 0.5 / words.length;
    }
  }
  
  // 5. Palabras clave específicas (para mejorar la búsqueda semántica)
  const keywords = {
    'vacaciones': [10, 50, 100],
    'políticas': [20, 60, 120],
    'empresa': [30, 70, 140],
    'seguridad': [40, 80, 160],
    'instalación': [50, 90, 180],
    'sistema': [60, 100, 200],
    'manual': [70, 110, 220],
    'procedimientos': [80, 120, 240]
  };
  
  Object.entries(keywords).forEach(([keyword, indices]) => {
    if (normalized.includes(keyword)) {
      indices.forEach(index => {
        if (index < dimension) {
          embedding[index] += 0.3;
        }
      });
    }
  });
  
  // Normalizar el vector
  const norm = Math.sqrt(embedding.reduce((sum, val) => sum + val * val, 0));
  if (norm > 0) {
    for (let i = 0; i < dimension; i++) {
      embedding[i] /= norm;
    }
  }
  
  console.log(`[EMBEDDINGS] Embedding local generado: dimensión ${embedding.length}`);
  
  return embedding;
}

/**
 * Función hash simple para strings
 * @param {string} str - String a hashear
 * @returns {number} - Hash numérico
 */
function hashString(str) {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    const char = str.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash; // Convert to 32bit integer
  }
  return Math.abs(hash);
}
