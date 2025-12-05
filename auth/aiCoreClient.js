import xsenv from "@sap/xsenv";
import { AzureOpenAiChatClient } from "@sap-ai-sdk/foundation-models";

// Cargar variables de entorno (VCAP_SERVICES)
xsenv.loadEnv();

let client;

/**
 * Inicializa y devuelve un cliente AI Core
 * @param {string} model - Modelo a usar (default: gpt-4o)
 * @param {object} options - Opciones adicionales
 * @param {number} options.temperature - Temperatura (0.0 = determinista, 1.0 = creativo). Default: 0.3 para validación
 * @param {number} options.maxTokens - Máximo de tokens en la respuesta
 * @returns {AzureOpenAiChatClient} Cliente configurado
 */
export function getAiCoreClient(model = "gpt-4o", options = {}) {
  const {
    temperature = 0.2, // Temperatura baja para validación consistente
    maxTokens = 30000
  } = options;
  
  if (!client) {
    client = new AzureOpenAiChatClient(model);
    console.log(`[AI CORE] Cliente inicializado con modelo: ${model}, temperature: ${temperature}`);
  }
  
  // Configurar parámetros por defecto para las llamadas
  client.defaultConfig = {
    temperature,
    max_tokens: maxTokens
  };
  
  return client;
}