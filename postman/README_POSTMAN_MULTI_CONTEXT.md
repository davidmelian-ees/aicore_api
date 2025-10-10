# 🚀 RAG API - Postman Collection (Multi-Context)

## 📋 Descripción

Colección completa de Postman para la API RAG con soporte para múltiples contextos, integrada con SAP AI Core. Esta versión incluye todas las funcionalidades de gestión de contextos, documentos y chat contextual.

## 🔧 Configuración Inicial

### Variables de Entorno

La colección incluye las siguientes variables que puedes configurar:

```json
{
  "base_url": "https://ai_core_api.cfapps.eu10-005.hana.ondemand.com",
  "local_url": "http://localhost:4000",
  "context_id": "default",
  "document_id": ""
}
```

### Cambiar entre Producción y Local

- **Producción**: Usa `{{base_url}}`
- **Local**: Cambia manualmente a `{{local_url}}`

## 📁 Estructura de la Colección

### 1. **Health & Status**
Endpoints para verificar el estado del sistema.

#### `GET /health`
- **Descripción**: Health check general del servidor
- **Respuesta**: Estado del servidor y configuración

#### `GET /api/rag/health`
- **Descripción**: Health check específico del sistema RAG
- **Respuesta**: Estado del RAG, estadísticas básicas

#### `GET /api/rag/stats`
- **Descripción**: Estadísticas detalladas del sistema RAG
- **Respuesta**: Métricas completas de documentos, chunks, contextos

---

### 2. **Contexts Management** 🆕
Gestión completa de contextos múltiples.

#### `POST /api/rag/contexts`
- **Descripción**: Crear un nuevo contexto
- **Body**:
```json
{
  "name": "Contexto de Prueba",
  "description": "Este es un contexto de ejemplo para testing"
}
```
- **Respuesta**: Información del contexto creado con ID único

#### `GET /api/rag/contexts`
- **Descripción**: Listar todos los contextos disponibles
- **Respuesta**: Array con todos los contextos y metadatos

#### `GET /api/rag/contexts/{contextId}`
- **Descripción**: Obtener información detallada de un contexto
- **Variables**: `{{context_id}}`
- **Respuesta**: Datos completos del contexto específico

#### `DELETE /api/rag/contexts/{contextId}`
- **Descripción**: Eliminar contexto y todos sus documentos
- **Variables**: `{{context_id}}`
- **Nota**: No permite eliminar el contexto 'default'

---

### 3. **Documents Management**
Gestión de documentos con soporte contextual.

#### `POST /api/rag/upload` (Actualizado)
- **Descripción**: Subir documento a un contexto específico
- **Tipo**: `multipart/form-data`
- **Campos**:
  - `document`: Archivo (TXT, DOC, DOCX, MD, JSON, CSV, PDF)
  - `contextId`: ID del contexto destino
  - `uploadedBy`: Usuario que sube el archivo
  - `tags`: Tags separados por comas

#### `GET /api/rag/documents`
- **Descripción**: Listar todos los documentos
- **Respuesta**: Todos los documentos del sistema

#### `GET /api/rag/documents?contextId={contextId}` 🆕
- **Descripción**: Listar documentos filtrados por contexto
- **Query Params**: `contextId={{context_id}}`
- **Respuesta**: Solo documentos del contexto especificado

#### `GET /api/rag/documents/{documentId}`
- **Descripción**: Obtener detalles de un documento específico
- **Variables**: `{{document_id}}`
- **Respuesta**: Información completa del documento y sus chunks

#### `DELETE /api/rag/documents/{documentId}`
- **Descripción**: Eliminar un documento específico
- **Variables**: `{{document_id}}`

---

### 4. **RAG Chat & Search**
Chat y búsqueda contextual con SAP AI Core.

#### `POST /api/rag/chat` (Actualizado)
- **Descripción**: Chat con contexto específico
- **Body**:
```json
{
  "message": "¿Qué información contienen los documentos de este contexto?",
  "contextId": "{{context_id}}",
  "topK": 5,
  "includeContext": true,
  "model": "gpt-4o"
}
```

#### `POST /api/rag/chat` (Default Context)
- **Descripción**: Chat con contexto por defecto
- **Body**:
```json
{
  "message": "Explícame el contenido de los documentos",
  "contextId": "default",
  "topK": 3,
  "includeContext": true,
  "model": "gpt-4o"
}
```

#### `POST /api/rag/search` (Actualizado)
- **Descripción**: Búsqueda en contexto específico
- **Body**:
```json
{
  "query": "información importante",
  "contextId": "{{context_id}}",
  "topK": 5,
  "minSimilarity": 0.1
}
```

#### `POST /api/rag/search` (Document Specific)
- **Descripción**: Búsqueda en documento específico dentro de contexto
- **Body**:
```json
{
  "query": "concepto específico",
  "contextId": "{{context_id}}",
  "documentId": "{{document_id}}",
  "topK": 3,
  "minSimilarity": 0.2
}
```

---

### 5. **System Management**
Administración del sistema.

#### `DELETE /api/rag/clear`
- **Descripción**: Limpiar completamente el índice RAG
- **Body**:
```json
{
  "confirm": "DELETE_ALL"
}
```
- **⚠️ Precaución**: Elimina todos los documentos y contextos

---

### 6. **Legacy Chat**
Compatibilidad con versiones anteriores.

#### `POST /api/chat`
- **Descripción**: Chat simple sin RAG (solo SAP AI Core)
- **Body**:
```json
{
  "messages": [
    {
      "role": "user",
      "content": "Hola, ¿cómo estás?"
    }
  ],
  "model": "gpt-4o",
  "maxTokens": 500,
  "temperature": 0.7
}
```

## 🔄 Flujo de Trabajo Recomendado

### 1. **Configuración Inicial**
```
1. GET /health - Verificar que el servidor esté funcionando
2. GET /api/rag/health - Verificar estado del RAG
3. GET /api/rag/stats - Ver estadísticas iniciales
```

### 2. **Gestión de Contextos**
```
1. GET /api/rag/contexts - Ver contextos existentes
2. POST /api/rag/contexts - Crear nuevo contexto
3. Copiar el ID del contexto creado a la variable {{context_id}}
```

### 3. **Subida de Documentos**
```
1. POST /api/rag/upload - Subir documento al contexto
2. GET /api/rag/documents?contextId={{context_id}} - Verificar subida
3. Copiar document_id para pruebas específicas
```

### 4. **Testing de RAG**
```
1. POST /api/rag/search - Probar búsqueda contextual
2. POST /api/rag/chat - Probar chat con contexto
3. Verificar que las respuestas usen solo documentos del contexto
```

## 🆕 Nuevas Funcionalidades vs Versión Anterior

### **Añadido**
- ✅ Gestión completa de contextos múltiples
- ✅ Filtrado de documentos por contexto
- ✅ Chat y búsqueda contextual
- ✅ Variables de Postman para facilitar testing
- ✅ Endpoints específicos para contextos

### **Mejorado**
- 🔄 Upload de documentos ahora acepta contextId
- 🔄 Chat y search filtran por contexto
- 🔄 Listado de documentos con filtro opcional
- 🔄 Mejor organización de la colección

### **Mantenido**
- ✅ Compatibilidad total con versión anterior
- ✅ Todos los endpoints legacy funcionan
- ✅ Misma estructura de respuestas

## 🔧 Variables Útiles para Testing

Configura estas variables en tu entorno de Postman:

```json
{
  "base_url": "https://ai_core_api.cfapps.eu10-005.hana.ondemand.com",
  "context_id": "tu-context-id-aqui",
  "document_id": "tu-document-id-aqui",
  "test_message": "¿Qué información importante contienen estos documentos?"
}
```

## 📝 Notas Importantes

1. **Contexto Default**: Siempre existe un contexto 'default' que no se puede eliminar
2. **Filtrado**: Si no especificas contextId, se usa 'default'
3. **Compatibilidad**: Todos los endpoints anteriores siguen funcionando
4. **Persistencia**: Los contextos se almacenan en memoria (se pierden al reiniciar)
5. **Formatos**: Soporta TXT, DOC, DOCX, MD, JSON, CSV, PDF

## 🚀 Importar en Postman

1. Abre Postman
2. Click en "Import"
3. Selecciona el archivo `RAG_API_Multi_Context.postman_collection.json`
4. Configura las variables de entorno
5. ¡Comienza a probar!

---

**Versión**: 2.0 - Multi Context Support  
**Fecha**: Octubre 2025  
**Compatibilidad**: SAP AI Core + ChromaDB + Node.js
