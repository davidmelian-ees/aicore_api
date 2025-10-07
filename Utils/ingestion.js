import { AzureOpenAiEmbeddingClient } from "@sap-ai-sdk/foundation-models";

import { TextLoader } from "langchain/document_loaders/fs/text";
import { RecursiveCharacterTextSplitter } from "langchain/text_splitter";
import { v4 as uuidv4 } from 'uuid'; // Para generar IDs únicos
import { createCollection, listCollections, addDocument } from "./chromadb.js";
import e from "express";

async function ingestDocuments() {
    // 1. Conexión a SAP AI Core para el modelo de embeddings
    const embeddingClient = new AzureOpenAiEmbeddingClient("text-embedding-ada-002");
    console.log("[AI CORE] Cliente de embeddings inicializado.");
    

    // 3. Cargar y dividir el documento
    const loader = new TextLoader("/home/user/projects/aicore_api/Utils/data.txt");
    const docs = await loader.load();
    const splitter = new RecursiveCharacterTextSplitter({ chunkSize: 500, chunkOverlap: 50 });
    const chunks = await splitter.splitDocuments(docs);
    console.log(`Documento dividido en ${chunks.length} fragmentos.`);

    // 4. Generar embeddings y almacenar en ChromaDB
    for (const chunk of chunks) {
        const content = chunk.pageContent;
        
        // Llamada al SDK de SAP para obtener el embedding
        const response = await embeddingClient.embed({
            input: "Hola"
        })
        const embedding = response.data.embedding;

        // Almacenar en la colección de Chroma
        const result = await addDocument(
            "7e15392b-b107-4d0c-95d0-b971040f914e", 
            documents = content, 
            ids = [uuidv4()], 
            embeddings = embedding
        );
        console.log(result)
    }
    console.log("¡Documentos y embeddings almacenados exitosamente en ChromaDB!");
}

ingestDocuments().catch(console.error);