import xsenv from "@sap/xsenv";
import { AzureOpenAiChatClient } from "@sap-ai-sdk/foundation-models";

// Cargar variables de entorno (VCAP_SERVICES)
xsenv.loadEnv();

let client;

// Inicializa y devuelve un cliente AI Core
export function getAiCoreClient(model = "gpt-4o") {
  if (!client) {
    client = new AzureOpenAiChatClient(model);
    console.log(`[AI CORE] Cliente inicializado con modelo: ${model}`);
  }
  return client;
}