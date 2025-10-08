# Sistema RAG con SAP AI Core

Sistema de Retrieval-Augmented Generation (RAG) completamente integrado con SAP AI Core para procesamiento de documentos y generación de respuestas contextuales.

## 🚀 Características

- **Integración completa con SAP AI Core** para embeddings y chat
- **Procesamiento multi-formato**: TXT, DOCX, MD, JSON, CSV (PDF temporalmente deshabilitado)
- **ChromaDB integrado** para persistencia vectorial
- **Fallback automático** a almacenamiento en memoria
- **API REST completa** para gestión de documentos
- **Búsqueda semántica avanzada** con similitud de coseno
- **Chat contextual** usando documentos indexados

## 📋 Prerequisitos

1. **SAP AI Core configurado** con servicios bindeados
2. **Dependencias instaladas**:
   ```bash
   npm install
   ```
3. **Archivo `default-env.json`** con credenciales de SAP BTP
4. **ChromaDB (opcional)** para persistencia:
   ```bash
   docker run -d -p 8000:8000 chromadb/chroma
   ```

## 🛠️ Instalación

1. **Instalar dependencias**:
   ```bash
   npm install
   ```

2. **Configurar variables de entorno** (ya tienes `default-env.json`)

3. **Iniciar el servidor**:
   ```bash
   npm start
   ```

## 📚 Uso del Sistema RAG

### 1. Subir Documentos

**Endpoint**: `POST /api/rag/upload`

```bash
curl -X POST http://localhost:4000/api/rag/upload \
  -F "document=@sample_documents/empresa_politicas.txt" \
  -F "uploadedBy=usuario_test"
```

**Respuesta**:
```json
{
  "success": true,
  "message": "Documento indexado exitosamente con SAP AI Core",
  "document": {
    "documentId": "uuid-generado",
    "fileName": "empresa_politicas.txt",
    "totalChunks": 15,
    "fileSize": 8432,
    "indexedAt": "2024-10-08T09:15:00Z"
  }
}
```

### 2. Chat con Contexto RAG

**Endpoint**: `POST /api/rag/chat`

```bash
curl -X POST http://localhost:4000/api/rag/chat \
  -H "Content-Type: application/json" \
  -d '{
    "message": "¿Cuáles son las políticas de vacaciones de la empresa?",
    "topK": 5,
    "includeContext": true
  }'
```

**Respuesta**:
```json
{
  "success": true,
  "query": "¿Cuáles son las políticas de vacaciones de la empresa?",
  "answer": "Según las políticas de la empresa, los empleados tienen derecho a 25 días hábiles de vacaciones anuales, además de 3 días personales al año...",
  "context": [
    {
      "content": "1.2 Vacaciones y Permisos\n- Vacaciones anuales: 25 días hábiles...",
      "similarity": 0.89,
      "metadata": {
        "fileName": "empresa_politicas.txt",
        "chunkIndex": 3
      }
    }
  ],
  "metadata": {
    "chunksUsed": 3,
    "sources": ["empresa_politicas.txt"],
    "hasContext": true
  }
}
```

### 3. Buscar Contexto

**Endpoint**: `POST /api/rag/search`

```bash
curl -X POST http://localhost:4000/api/rag/search \
  -H "Content-Type: application/json" \
  -d '{
    "query": "procedimientos de seguridad",
    "topK": 3
  }'
```

### 4. Listar Documentos

**Endpoint**: `GET /api/rag/documents`

```bash
curl http://localhost:4000/api/rag/documents
```

### 5. Obtener Estadísticas

**Endpoint**: `GET /api/rag/stats`

```bash
curl http://localhost:4000/api/rag/stats
```

## 🏗️ Arquitectura del Sistema

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Documentos    │    │   Procesador    │    │   SAP AI Core   │
│   (TXT,PDF,etc) │───►│   de Archivos   │───►│   Embeddings    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                │                       │
                                ▼                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   API REST      │◄───│  Vector Store   │◄───│   Embeddings    │
│   Endpoints     │    │   (Memoria)     │    │   Generados     │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │
         ▼                       ▼
┌─────────────────┐    ┌─────────────────┐
│   Chat RAG      │    │   Búsqueda      │
│   Response      │    │   Semántica     │
└─────────────────┘    └─────────────────┘
```

## 📁 Estructura de Archivos

```
aicore_api/
├── services/
│   ├── documentProcessor.js    # Procesamiento de archivos
│   ├── embeddingService.js     # Integración con SAP AI Core embeddings
│   ├── vectorStore.js          # Almacenamiento vectorial
│   └── ragService.js           # Servicio principal RAG
├── routes/
│   └── rag.js                  # Endpoints de la API RAG
├── sample_documents/           # Documentos de ejemplo
│   ├── empresa_politicas.txt
│   ├── manual_tecnico.md
│   └── procedimientos_seguridad.json
└── uploads/                    # Archivos subidos (temporal)
```

## 🔧 Servicios Principales

### DocumentProcessor
- Extrae texto de diferentes formatos de archivo
- Divide el contenido en chunks optimizados
- Maneja TXT, PDF, DOCX, MD, JSON, CSV

### EmbeddingService
- Integración nativa con SAP AI Core
- Genera embeddings usando `text-embedding-3-small`
- Procesamiento en lotes para eficiencia
- Cálculo de similitud de coseno

### VectorStore
- Almacenamiento vectorial optimizado en memoria
- Búsqueda rápida por similitud
- Gestión de metadatos de documentos
- Operaciones CRUD completas

### RAGService
- Orquesta todo el flujo RAG
- Indexación completa de documentos
- Generación de respuestas contextuales
- Integración con chat de SAP AI Core

## 📊 Endpoints Disponibles

| Método | Endpoint | Descripción |
|--------|----------|-------------|
| POST | `/api/rag/upload` | Subir y indexar documento |
| POST | `/api/rag/chat` | Chat con contexto RAG |
| POST | `/api/rag/search` | Buscar contexto relevante |
| GET | `/api/rag/documents` | Listar documentos indexados |
| GET | `/api/rag/documents/:id` | Info detallada de documento |
| DELETE | `/api/rag/documents/:id` | Eliminar documento |
| GET | `/api/rag/stats` | Estadísticas del sistema |
| GET | `/api/rag/health` | Estado de salud |
| DELETE | `/api/rag/clear` | Limpiar todo el índice |

## 🧪 Testing con Documentos de Ejemplo

El sistema incluye 3 documentos de ejemplo listos para probar:

1. **`empresa_politicas.txt`** - Políticas de RRHH y empresa
2. **`manual_tecnico.md`** - Manual técnico de sistema
3. **`procedimientos_seguridad.json`** - Procedimientos de seguridad IT

### Subir documentos de ejemplo:

```bash
# Subir políticas de empresa
curl -X POST http://localhost:4000/api/rag/upload \
  -F "document=@sample_documents/empresa_politicas.txt"

# Subir manual técnico
curl -X POST http://localhost:4000/api/rag/upload \
  -F "document=@sample_documents/manual_tecnico.md"

# Subir procedimientos de seguridad
curl -X POST http://localhost:4000/api/rag/upload \
  -F "document=@sample_documents/procedimientos_seguridad.json"
```

### Preguntas de ejemplo:

```bash
# Sobre políticas de empresa
curl -X POST http://localhost:4000/api/rag/chat \
  -H "Content-Type: application/json" \
  -d '{"message": "¿Cuántos días de vacaciones tengo al año?"}'

# Sobre el manual técnico
curl -X POST http://localhost:4000/api/rag/chat \
  -H "Content-Type: application/json" \
  -d '{"message": "¿Cómo instalar el sistema de inventario?"}'

# Sobre procedimientos de seguridad
curl -X POST http://localhost:4000/api/rag/chat \
  -H "Content-Type: application/json" \
  -d '{"message": "¿Cuál es el procedimiento para reportar un incidente de seguridad?"}'
```

## 🔍 Monitoreo y Logs

El sistema incluye logging detallado:

```
[RAG] Iniciando indexación de documento: ./uploads/documento.txt
[EMBEDDINGS] Generando embedding para texto de 1250 caracteres
[VECTOR STORE] Documento agregado: doc_abc123_chunk_0
[RAG] Documento indexado exitosamente: doc_abc123
[RAG] Buscando contexto para: "¿Cuáles son las políticas..."
[RAG] Búsqueda completada: 3 chunks encontrados
[RAG] Generando respuesta con 3 chunks de contexto
```

## 🚨 Troubleshooting

### Error de embeddings
```
Error: AzureOpenAiEmbeddingClient initialization failed
```
**Solución**: Verificar que el servicio AI Core esté bindeado y `default-env.json` sea correcto.

### Error de procesamiento de archivos
```
Error: Tipo de archivo no soportado
```
**Solución**: Verificar que el archivo sea TXT, PDF, DOCX, MD, JSON o CSV.

### Sin contexto encontrado
```
No encontré información relevante en los documentos
```
**Solución**: Verificar que hay documentos indexados y que la consulta sea relevante.

## 📈 Performance

- **Indexación**: ~2-5 segundos por documento (dependiendo del tamaño)
- **Búsqueda**: ~100-300ms por consulta
- **Chat RAG**: ~1-3 segundos (incluyendo llamada a SAP AI Core)
- **Capacidad**: Hasta 1000+ documentos en memoria

## 🔐 Seguridad

- Archivos temporales se eliminan después de indexar
- Validación de tipos de archivo
- Límites de tamaño de archivo (50MB)
- Sanitización de nombres de archivo

---

**¡El sistema RAG está listo para usar!** 🎉

Puedes empezar subiendo los documentos de ejemplo y haciendo preguntas sobre su contenido.
