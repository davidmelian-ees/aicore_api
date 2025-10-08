# 🗄️ Configuración de ChromaDB para Sistema RAG

## 📋 Resumen

El sistema RAG ahora soporta **ChromaDB** como base de datos vectorial persistente, proporcionando:

✅ **Persistencia** - Los datos sobreviven a reinicios del servidor  
✅ **Escalabilidad** - Manejo eficiente de grandes volúmenes  
✅ **Performance** - Búsquedas vectoriales optimizadas  
✅ **Fallback automático** - Si ChromaDB no está disponible, usa memoria  

## 🚀 Instalación de ChromaDB

### Opción 1: Docker (Recomendado)

```bash
# Ejecutar ChromaDB en Docker
docker run -p 8000:8000 chromadb/chroma

# O en background
docker run -d -p 8000:8000 --name chroma-db chromadb/chroma
```

### Opción 2: Instalación Local

```bash
# Instalar ChromaDB
pip install chromadb

# Ejecutar servidor
chroma run --host 0.0.0.0 --port 8000
```

### Opción 3: Docker Compose

Crear `docker-compose.yml`:
```yaml
version: '3.8'
services:
  chromadb:
    image: chromadb/chroma
    ports:
      - "8000:8000"
    volumes:
      - ./chroma_data:/chroma/chroma
    environment:
      - CHROMA_SERVER_HOST=0.0.0.0
      - CHROMA_SERVER_HTTP_PORT=8000
```

Ejecutar:
```bash
docker-compose up -d
```

## ⚙️ Configuración del Sistema

### Variables de Entorno

```bash
# Para usar ChromaDB (por defecto está habilitado)
USE_CHROMA=true

# Para usar solo memoria
USE_CHROMA=false
```

### Configuración Automática

El sistema detecta automáticamente si ChromaDB está disponible:

```javascript
// Si ChromaDB está corriendo en localhost:8000
✅ Usa ChromaDB para persistencia

// Si ChromaDB no está disponible
⚠️  Fallback automático a vector store en memoria
```

## 🔧 Verificar Instalación

### 1. Verificar que ChromaDB esté corriendo:

```bash
curl http://localhost:8000/api/v1/heartbeat
```

**Respuesta esperada:**
```json
{"nanosecond heartbeat": 1696771200000000000}
```

### 2. Verificar integración con el sistema RAG:

```bash
curl http://localhost:4000/api/rag/health
```

**Respuesta esperada:**
```json
{
  "success": true,
  "status": "healthy",
  "summary": {
    "totalDocuments": 0,
    "totalChunks": 0,
    "embeddingDimension": 384,
    "integrityCheck": true
  }
}
```

## 📊 Diferencias entre Almacenamiento

| Característica | Memoria (VectorStore) | ChromaDB |
|----------------|----------------------|----------|
| **Persistencia** | ❌ Se pierde al reiniciar | ✅ Datos persistentes |
| **Escalabilidad** | ❌ Limitado por RAM | ✅ Escalable |
| **Performance** | ✅ Muy rápido | ✅ Optimizado |
| **Setup** | ✅ Sin configuración | ⚙️ Requiere instalación |
| **Desarrollo** | ✅ Perfecto | ✅ Perfecto |
| **Producción** | ❌ No recomendado | ✅ Recomendado |

## 🔄 Migración Automática

El sistema maneja automáticamente la migración:

```javascript
// Al iniciar el servidor
[RAG] Inicializando ChromaDB...
[CHROMA] ✅ Conexión establecida con ChromaDB
[CHROMA] ✅ Colección 'rag_documents' encontrada
[CHROMA] 🚀 ChromaVectorStore inicializado correctamente

// Si ChromaDB no está disponible
[RAG] ⚠️  ChromaDB no disponible, usando vector store en memoria
[RAG] 💡 Para usar ChromaDB: docker run -p 8000:8000 chromadb/chroma
```

## 📁 Estructura de Datos en ChromaDB

### Colección: `rag_documents`

```json
{
  "id": "doc_abc123_chunk_0",
  "embedding": [0.123, -0.456, 0.789, ...],
  "document": "Contenido del chunk...",
  "metadata": {
    "documentId": "doc_abc123",
    "fileName": "empresa_politicas.txt",
    "chunkIndex": 0,
    "totalChunks": 15,
    "addedAt": "2024-10-08T10:30:00Z",
    "contentLength": 1250
  }
}
```

## 🧪 Testing con ChromaDB

### 1. Iniciar ChromaDB:
```bash
docker run -d -p 8000:8000 --name chroma-db chromadb/chroma
```

### 2. Iniciar el servidor RAG:
```bash
npm start
```

### 3. Subir documentos:
```bash
node ingest_simple.js
# Seguir los comandos curl generados
```

### 4. Probar persistencia:
```bash
# Reiniciar el servidor
Ctrl+C
npm start

# Verificar que los documentos siguen ahí
curl http://localhost:4000/api/rag/documents
```

## 🛠️ Comandos Útiles

### Gestión de ChromaDB

```bash
# Ver logs de ChromaDB
docker logs chroma-db

# Reiniciar ChromaDB
docker restart chroma-db

# Parar ChromaDB
docker stop chroma-db

# Eliminar datos (cuidado!)
docker rm chroma-db
```

### Gestión de Datos

```bash
# Ver estadísticas
curl http://localhost:4000/api/rag/stats

# Limpiar todos los datos
curl -X DELETE http://localhost:4000/api/rag/clear \
  -H "Content-Type: application/json" \
  -d '{"confirm": "DELETE_ALL"}'

# Listar documentos
curl http://localhost:4000/api/rag/documents
```

## 🚨 Troubleshooting

### Error: "ChromaDB no disponible"

```
[CHROMA] ❌ Error inicializando ChromaDB: connect ECONNREFUSED 127.0.0.1:8000
```

**Soluciones:**
1. Verificar que ChromaDB esté corriendo: `docker ps`
2. Verificar puerto: `curl http://localhost:8000/api/v1/heartbeat`
3. Reiniciar ChromaDB: `docker restart chroma-db`

### Error: "Collection not found"

```
[CHROMA] Error: Collection 'rag_documents' not found
```

**Solución:**
- El sistema creará automáticamente la colección
- Si persiste, reiniciar el servidor RAG

### Performance lenta

**Optimizaciones:**
1. Usar SSD para volumen de Docker
2. Aumentar memoria asignada a Docker
3. Optimizar tamaño de chunks en `documentProcessor.js`

## 🎯 Próximos Pasos

1. **Iniciar ChromaDB**: `docker run -d -p 8000:8000 chromadb/chroma`
2. **Reiniciar servidor**: `npm start`
3. **Subir documentos**: Usar `ingest_simple.js`
4. **Probar persistencia**: Reiniciar y verificar datos

---

**¡ChromaDB está integrado y listo para usar!** 🎉

**Estado actual**: Fallback automático habilitado  
**Recomendación**: Usar ChromaDB para desarrollo y producción  
**Beneficio**: Datos persistentes + mejor performance
