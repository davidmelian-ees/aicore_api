#!/usr/bin/env python3
"""
Servidor ChromaDB simplificado para el sistema RAG
Versión básica sin dependencias complejas
"""

import os
import json
import logging
from datetime import datetime
from typing import List, Dict, Any, Optional

try:
    import chromadb
    from chromadb.config import Settings
    print("✅ ChromaDB importado correctamente")
except ImportError as e:
    print(f"❌ Error importando ChromaDB: {e}")
    print("💡 Instala con: pip install chromadb")
    exit(1)

try:
    from fastapi import FastAPI, HTTPException, status
    from fastapi.middleware.cors import CORSMiddleware
    import uvicorn
    print("✅ FastAPI importado correctamente")
except ImportError as e:
    print(f"❌ Error importando FastAPI: {e}")
    print("💡 Instala con: pip install fastapi uvicorn")
    exit(1)

# Configurar logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# Inicializar FastAPI
app = FastAPI(
    title="ChromaDB RAG Service - Simple",
    description="Servicio simplificado de ChromaDB para sistema RAG",
    version="1.0.0-simple"
)

# Configurar CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Variables globales
chroma_client = None
collection = None
COLLECTION_NAME = "rag_documents"
DATA_DIR = "./chroma_data"

def initialize_chromadb():
    """Inicializa ChromaDB de forma simple"""
    global chroma_client, collection
    
    try:
        logger.info("🚀 Inicializando ChromaDB...")
        
        # Crear directorio de datos
        os.makedirs(DATA_DIR, exist_ok=True)
        
        # Configurar ChromaDB
        chroma_client = chromadb.PersistentClient(path=DATA_DIR)
        
        # Obtener o crear colección
        try:
            collection = chroma_client.get_collection(name=COLLECTION_NAME)
            logger.info(f"✅ Colección '{COLLECTION_NAME}' encontrada")
        except Exception:
            collection = chroma_client.create_collection(
                name=COLLECTION_NAME,
                metadata={"description": "RAG documents", "created_at": datetime.now().isoformat()}
            )
            logger.info(f"✅ Colección '{COLLECTION_NAME}' creada")
        
        logger.info("🎉 ChromaDB inicializado correctamente")
        return True
        
    except Exception as e:
        logger.error(f"❌ Error inicializando ChromaDB: {e}")
        return False

@app.on_event("startup")
async def startup_event():
    """Inicializar al arrancar"""
    success = initialize_chromadb()
    if not success:
        raise RuntimeError("No se pudo inicializar ChromaDB")

@app.get("/")
async def root():
    """Endpoint raíz"""
    return {
        "service": "ChromaDB RAG Service - Simple",
        "status": "running",
        "version": "1.0.0-simple",
        "collection": COLLECTION_NAME,
        "data_dir": DATA_DIR
    }

@app.get("/health")
async def health_check():
    """Verificación de salud"""
    try:
        if collection is None:
            raise HTTPException(status_code=503, detail="ChromaDB no inicializado")
        
        count = collection.count()
        
        return {
            "status": "healthy",
            "message": "ChromaDB funcionando correctamente",
            "timestamp": datetime.now().isoformat(),
            "collection_info": {
                "name": COLLECTION_NAME,
                "document_count": count,
                "data_directory": DATA_DIR
            }
        }
        
    except Exception as e:
        logger.error(f"❌ Error en health check: {e}")
        raise HTTPException(status_code=503, detail=f"Error: {str(e)}")

@app.post("/documents")
async def add_document(doc_data: dict):
    """Agregar documento"""
    try:
        if collection is None:
            raise HTTPException(status_code=503, detail="ChromaDB no inicializado")
        
        doc_id = doc_data.get("id")
        content = doc_data.get("content")
        embedding = doc_data.get("embedding")
        metadata = doc_data.get("metadata", {})
        
        if not doc_id or not content or not embedding:
            raise HTTPException(status_code=400, detail="Faltan campos requeridos: id, content, embedding")
        
        logger.info(f"📄 Agregando documento: {doc_id}")
        
        # Agregar metadatos adicionales
        metadata.update({
            "added_at": datetime.now().isoformat(),
            "content_length": len(content)
        })
        
        # Agregar a ChromaDB
        collection.add(
            ids=[doc_id],
            embeddings=[embedding],
            documents=[content],
            metadatas=[metadata]
        )
        
        logger.info(f"✅ Documento agregado: {doc_id}")
        
        return {
            "success": True,
            "message": f"Documento {doc_id} agregado exitosamente",
            "document_id": doc_id,
            "embedding_dimension": len(embedding)
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"❌ Error agregando documento: {e}")
        raise HTTPException(status_code=500, detail=f"Error: {str(e)}")

@app.post("/search")
async def search_documents(search_data: dict):
    """Buscar documentos similares"""
    try:
        if collection is None:
            raise HTTPException(status_code=503, detail="ChromaDB no inicializado")
        
        query_embedding = search_data.get("query_embedding")
        top_k = search_data.get("top_k", 5)
        filters = search_data.get("filters", {})
        
        if not query_embedding:
            raise HTTPException(status_code=400, detail="query_embedding requerido")
        
        logger.info(f"🔍 Buscando {top_k} documentos similares")
        
        # Realizar búsqueda
        where_clause = filters if filters else None
        
        results = collection.query(
            query_embeddings=[query_embedding],
            n_results=top_k,
            where=where_clause,
            include=["documents", "metadatas", "distances"]
        )
        
        # Procesar resultados
        search_results = []
        
        if results["ids"] and results["ids"][0]:
            for i in range(len(results["ids"][0])):
                distance = results["distances"][0][i] if results["distances"] else 0
                similarity = max(0, 1 - distance)
                
                search_results.append({
                    "id": results["ids"][0][i],
                    "similarity": similarity,
                    "content": results["documents"][0][i],
                    "metadata": results["metadatas"][0][i] or {}
                })
        
        logger.info(f"✅ Búsqueda completada: {len(search_results)} resultados")
        
        return search_results
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"❌ Error en búsqueda: {e}")
        raise HTTPException(status_code=500, detail=f"Error: {str(e)}")

@app.get("/stats")
async def get_stats():
    """Obtener estadísticas"""
    try:
        if collection is None:
            raise HTTPException(status_code=503, detail="ChromaDB no inicializado")
        
        count = collection.count()
        
        # Obtener dimensión de embedding de forma segura
        embedding_dim = 0
        if count > 0:
            try:
                sample = collection.get(limit=1, include=["embeddings"])
                if sample and "embeddings" in sample and sample["embeddings"] and len(sample["embeddings"]) > 0:
                    if sample["embeddings"][0] and len(sample["embeddings"][0]) > 0:
                        embedding_dim = len(sample["embeddings"][0])
            except Exception as e:
                logger.warning(f"⚠️  No se pudo obtener dimensión de embedding: {e}")
                embedding_dim = 384  # Dimensión por defecto
        
        return {
            "total_documents": count,
            "collection_name": COLLECTION_NAME,
            "embedding_dimension": embedding_dim,
            "created_at": datetime.now().isoformat()
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"❌ Error obteniendo estadísticas: {e}")
        raise HTTPException(status_code=500, detail=f"Error: {str(e)}")

@app.get("/documents")
async def list_documents(limit: int = 100):
    """Listar documentos"""
    try:
        if collection is None:
            raise HTTPException(status_code=503, detail="ChromaDB no inicializado")
        
        results = collection.get(limit=limit, include=["metadatas"])
        
        documents = []
        if results["ids"]:
            for i, doc_id in enumerate(results["ids"]):
                metadata = results["metadatas"][i] if results["metadatas"] else {}
                documents.append({
                    "id": doc_id,
                    "metadata": metadata
                })
        
        return {
            "documents": documents,
            "count": len(documents),
            "total_in_collection": collection.count()
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"❌ Error listando documentos: {e}")
        raise HTTPException(status_code=500, detail=f"Error: {str(e)}")

@app.delete("/documents/{doc_id}")
async def delete_document(doc_id: str):
    """Eliminar documento"""
    try:
        if collection is None:
            raise HTTPException(status_code=503, detail="ChromaDB no inicializado")
        
        # Verificar si existe
        existing = collection.get(ids=[doc_id])
        if not existing["ids"]:
            raise HTTPException(status_code=404, detail=f"Documento {doc_id} no encontrado")
        
        # Eliminar
        collection.delete(ids=[doc_id])
        
        logger.info(f"🗑️ Documento eliminado: {doc_id}")
        
        return {
            "success": True,
            "message": f"Documento {doc_id} eliminado exitosamente",
            "document_id": doc_id
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"❌ Error eliminando documento: {e}")
        raise HTTPException(status_code=500, detail=f"Error: {str(e)}")

@app.delete("/clear")
async def clear_collection(confirm: str = None):
    """Limpiar colección"""
    global collection
    
    try:
        if confirm != "DELETE_ALL":
            raise HTTPException(status_code=400, detail="Para confirmar, envía ?confirm=DELETE_ALL")
        
        if collection is None:
            raise HTTPException(status_code=503, detail="ChromaDB no inicializado")
        
        count_before = collection.count()
        
        # Eliminar y recrear colección
        chroma_client.delete_collection(name=COLLECTION_NAME)
        
        collection = chroma_client.create_collection(
            name=COLLECTION_NAME,
            metadata={"description": "RAG documents", "created_at": datetime.now().isoformat()}
        )
        
        logger.info(f"🧹 Colección limpiada: {count_before} documentos eliminados")
        
        return {
            "success": True,
            "message": "Colección limpiada completamente",
            "documents_removed": count_before,
            "cleared_at": datetime.now().isoformat()
        }
        
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"❌ Error limpiando colección: {e}")
        raise HTTPException(status_code=500, detail=f"Error: {str(e)}")

if __name__ == "__main__":
    print("🚀 Iniciando ChromaDB RAG Service - Simple...")
    print("📊 Endpoints disponibles:")
    print("   - Health: http://localhost:8001/health")
    print("   - Stats: http://localhost:8001/stats")
    print("   - Docs: http://localhost:8001/docs")
    print("")
    
    uvicorn.run(
        app,
        host="0.0.0.0",
        port=8001,
        log_level="info"
    )
