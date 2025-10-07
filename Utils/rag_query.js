import { AzureOpenAiChatClient, AzureOpenAiEmbeddingClient } from "@sap-ai-sdk/foundation-models";
import { ChromaClient } from 'chromadb';
import * as xsenv from '@sap/xsenv'; // Para cargar el .env file [6]

// Carga las variables de entorno para la conexión del SDK [7]
xsenv.loadEnv();

async function getRagAnswer(userQuery) {
    // 1. Inicializar clientes de SAP AI Core
    const embeddingClient = new AzureOpenAiEmbeddingClient("text-embedding-ada-002");
    const chatClient = new AzureOpenAiChatClient("gpt-4o"); // O el modelo de chat que prefieras [7, 8]
    console.log("[AI CORE] Clientes de Chat y Embedding listos.");

    // 2. Conectar a ChromaDB y obtener la colección
    const chroma = new ChromaClient();
    const collection = await chroma.getCollection({ name: "documentos_empresa" });
    console.log("[ChromaDB] Conectado a la colección.");

    // 3. Generar embedding para la pregunta del usuario
    const queryEmbeddingResponse = await embeddingClient.embedQuery(userQuery);
    const queryEmbedding = queryEmbeddingResponse.data.embedding;

    // 4. Buscar documentos relevantes en ChromaDB
    const relevantDocs = await collection.query({
        queryEmbeddings: [queryEmbedding],
        nResults: 3 // Número de resultados relevantes a recuperar
    });
    
    // Extraer el texto de los documentos encontrados
    const context = relevantDocs.documents.join("\n---\n");
    console.log("Contexto recuperado de ChromaDB:", context);
    
    // 5. Construir el prompt y llamar al LLM de Chat
    const prompt = [
        {
            role: "system",
            content: `Eres un asistente útil. Responde la pregunta del usuario basándote únicamente en el siguiente contexto:\n\n${context}`
        },
        {
            role: "user",
            content: userQuery
        }
    ];

    const response = await chatClient.chat(prompt);
    
    return response.choices.message.content;
}

// Ejemplo de uso
const pregunta = "¿Quién se une al evento en Madrid, España?";
getRagAnswer(pregunta)
    .then(respuesta => console.log("Respuesta del RAG:", respuesta))
    .catch(console.error);