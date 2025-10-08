# 📚 Sistema RAG con SAP AI Core y ChromaDB - Documentación Completa

**Versión:** 1.0.0  
**Fecha:** Octubre 2024  
**Autor:** Equipo de Desarrollo  
**Proyecto:** AI Core API con Sistema RAG  

---

## 📋 Índice

1. [Resumen Ejecutivo](#resumen-ejecutivo)
2. [Arquitectura del Sistema](#arquitectura-del-sistema)
3. [Componentes Principales](#componentes-principales)
4. [Configuración y Despliegue](#configuración-y-despliegue)
5. [API Reference](#api-reference)
6. [Guías de Uso](#guías-de-uso)
7. [Testing y Validación](#testing-y-validación)
8. [Mantenimiento y Monitoreo](#mantenimiento-y-monitoreo)
9. [Troubleshooting](#troubleshooting)
10. [Anexos](#anexos)

---

## 🎯 Resumen Ejecutivo

### Descripción del Proyecto

El **Sistema RAG (Retrieval-Augmented Generation)** es una solución completa que integra **SAP AI Core** con **ChromaDB** para proporcionar capacidades avanzadas de búsqueda semántica y chat conversacional basado en documentos corporativos.

### Características Principales

- ✅ **Integración completa con SAP AI Core** para embeddings y generación de texto
- ✅ **ChromaDB embebido** para persistencia vectorial en Cloud Foundry
- ✅ **Procesamiento multi-formato**: TXT, DOCX, MD, JSON, CSV
- ✅ **API REST completa** con endpoints para gestión de documentos
- ✅ **Chat conversacional** usando contexto de documentos indexados
- ✅ **Fallback automático** a almacenamiento en memoria
- ✅ **Autenticación XSUAA** para entornos de producción
- ✅ **Despliegue en Cloud Foundry** con configuración automática

### Beneficios del Negocio

1. **Acceso Inteligente a Información**: Búsqueda semántica en documentos corporativos
2. **Automatización de Consultas**: Chat bot que responde basándose en documentación oficial
3. **Escalabilidad**: Arquitectura cloud-native en SAP BTP
4. **Seguridad**: Integración con XSUAA para control de acceso
5. **Mantenimiento Simplificado**: Configuración automática y fallbacks

---

## 🏗️ Arquitectura del Sistema

### Arquitectura General

```
┌─────────────────────────────────────────────────────────────┐
│                    SAP BTP Cloud Foundry                    │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────────┐    ┌─────────────────┐                │
│  │   Node.js API   │    │   SAP AI Core   │                │
│  │   (Express)     │◄──►│   (Embeddings   │                │
│  │                 │    │   & Chat LLM)   │                │
│  └─────────────────┘    └─────────────────┘                │
│           │                                                 │
│           ▼                                                 │
│  ┌─────────────────┐    ┌─────────────────┐                │
│  │   ChromaDB      │    │   XSUAA Auth    │                │
│  │   (Embebido)    │    │   Service       │                │
│  │   Persistente   │    │                 │                │
│  └─────────────────┘    └─────────────────┘                │
└─────────────────────────────────────────────────────────────┘
```

### Flujo de Datos

1. **Ingesta de Documentos**:
   ```
   Documento → Procesamiento → Chunking → Embeddings (SAP AI Core) → ChromaDB
   ```

2. **Búsqueda Semántica**:
   ```
   Query → Embedding (SAP AI Core) → Búsqueda Vectorial (ChromaDB) → Resultados Rankeados
   ```

3. **Chat RAG**:
   ```
   Pregunta → Búsqueda Contexto → Prompt + Contexto → LLM (SAP AI Core) → Respuesta
   ```

### Componentes de Desarrollo vs Producción

| Componente | Desarrollo Local | Cloud Foundry |
|------------|------------------|---------------|
| **Vector Store** | ChromaDB Python Service | ChromaDB Embebido |
| **Autenticación** | Deshabilitada | XSUAA |
| **Puerto** | 4000 + 8001 | Dinámico |
| **Persistencia** | Archivos externos | Archivos locales CF |
| **Datos** | Upload manual | Pre-cargados |

---

## 🔧 Componentes Principales

### 1. Servidor Principal (`server.js`)

**Responsabilidades:**
- Configuración de Express.js
- Inicialización de autenticación (XSUAA en producción)
- Carga automática de datos de ejemplo
- Configuración de rutas API
- Health checks para Cloud Foundry

**Configuración Automática:**
```javascript
const isProduction = process.env.NODE_ENV === 'production';
const VECTOR_STORE_TYPE = process.env.VECTOR_STORE_TYPE || 
  (isProduction ? 'chroma' : 'python');
```

### 2. Servicio RAG (`services/ragService.js`)

**Funcionalidades:**
- Indexación de documentos con chunking inteligente
- Búsqueda semántica con similitud de coseno
- Chat conversacional con contexto
- Gestión de metadatos de documentos
- Estadísticas del sistema

**Métodos Principales:**
- `indexDocument(filePath, mimeType, metadata)` - Indexar documento
- `search(query, options)` - Búsqueda semántica
- `chatWithContext(message, options)` - Chat RAG
- `listDocuments()` - Listar documentos indexados
- `getRAGStats()` - Estadísticas del sistema

### 3. Servicio de Embeddings (`services/embeddingService.js`)

**Integración SAP AI Core:**
- Generación de embeddings usando modelos de SAP
- Fallback a embeddings locales si SAP no está disponible
- Validación y normalización de vectores
- Cache de embeddings para optimización

### 4. ChromaDB Vector Store (`services/chromaVectorStore.js`)

**Características:**
- Cliente ChromaDB embebido para Cloud Foundry
- Persistencia automática en archivos locales
- Configuración diferenciada por entorno
- Operaciones CRUD completas en vectores

### 5. Procesador de Documentos (`services/documentProcessor.js`)

**Formatos Soportados:**
- **TXT**: Texto plano
- **MD**: Markdown
- **JSON**: Archivos JSON estructurados
- **CSV**: Archivos de datos tabulares
- **DOCX**: Documentos Microsoft Word

**Chunking Inteligente:**
- Tamaño configurable de chunks (por defecto 1000 caracteres)
- Solapamiento entre chunks (por defecto 200 caracteres)
- Preservación de contexto semántico

### 6. Sistema de Autenticación (`auth.js`)

**Configuración XSUAA:**
- Habilitado automáticamente en producción
- Integración con servicios SAP BTP
- Middleware de autenticación para rutas protegidas

---

## ⚙️ Configuración y Despliegue

### Configuración Local

#### Prerequisitos
- Node.js 20.x
- Python 3.8+ (para ChromaDB local)
- SAP BTP Account con AI Core

#### Instalación
```bash
# Clonar repositorio
git clone [repository-url]
cd aicore_api

# Instalar dependencias
npm install

# Configurar variables de entorno
cp default-env.json.example default-env.json
# Editar con credenciales SAP BTP

# Iniciar ChromaDB local
cd chroma_service
start_service.bat

# En otra terminal, iniciar servidor
npm start
```

### Despliegue en Cloud Foundry

#### Prerequisitos
- CF CLI instalado
- Acceso a organización SAP BTP
- Servicio `aicore-app-auth` configurado

#### Configuración (`manifest.yml`)
```yaml
---
applications:
  - name: ai_core_api
    memory: 512M
    buildpacks:
      - nodejs_buildpack
    command: node server.js
    services:
      - aicore-app-auth
    env:
      VECTOR_STORE_TYPE: chroma
      NODE_ENV: production
```

#### Proceso de Despliegue
```bash
# Login en Cloud Foundry
cf login -a https://api.cf.eu10.hana.ondemand.com

# Configurar target
cf target -o "ORGANIZACION" -s "ESPACIO"

# Desplegar aplicación
cf push

# Verificar estado
cf apps
cf logs ai_core_api --recent
```

### Variables de Entorno

| Variable | Desarrollo | Producción | Descripción |
|----------|------------|------------|-------------|
| `NODE_ENV` | development | production | Entorno de ejecución |
| `VECTOR_STORE_TYPE` | python | chroma | Tipo de almacén vectorial |
| `PORT` | 4000 | CF_PORT | Puerto del servidor |
| `RAG_CHUNK_SIZE` | 1000 | 1000 | Tamaño de chunks |
| `RAG_CHUNK_OVERLAP` | 200 | 200 | Solapamiento de chunks |

---

## 📡 API Reference

### Base URL
- **Desarrollo**: `http://localhost:4000`
- **Producción**: `https://tu-app.cfapps.sap.hana.ondemand.com`

### Endpoints Principales

#### Health Checks

**GET /health**
```json
{
  "status": "healthy",
  "timestamp": "2024-10-08T12:30:00Z",
  "environment": "production",
  "vectorStore": "chroma"
}
```

**GET /api/rag/health**
```json
{
  "status": "healthy",
  "message": "Sistema RAG funcionando correctamente",
  "summary": {
    "totalDocuments": 2,
    "totalChunks": 15,
    "embeddingDimension": 384,
    "vectorStoreType": "chroma"
  }
}
```

#### Gestión de Documentos

**GET /api/rag/documents**
- Lista todos los documentos indexados
- Respuesta: Array de documentos con metadatos

**POST /api/rag/upload**
- Sube y procesa un documento
- Content-Type: multipart/form-data
- Campos: `document` (file), `tags` (string), `description` (string)

**DELETE /api/rag/documents/{documentId}**
- Elimina un documento específico del índice

#### Búsqueda y Chat

**POST /api/rag/search**
```json
{
  "query": "políticas de vacaciones",
  "topK": 5,
  "minSimilarity": 0.3,
  "documentId": "opcional"
}
```

**POST /api/rag/chat**
```json
{
  "message": "¿Cuáles son las políticas de vacaciones?",
  "maxTokens": 500,
  "temperature": 0.7
}
```

#### Administración

**GET /api/rag/stats**
- Estadísticas detalladas del sistema RAG

**DELETE /api/rag/clear**
- Limpia todos los documentos (requiere confirmación)

---

## 📖 Guías de Uso

### 1. Subir y Procesar Documentos

#### Via API REST
```bash
curl -X POST http://localhost:4000/api/rag/upload \
  -F "document=@documento.pdf" \
  -F "tags=politicas,empresa" \
  -F "description=Políticas corporativas"
```

#### Via Script de Ingesta
```bash
# Colocar documentos en sample_documents/
node scripts/ingest-documents.js
```

### 2. Realizar Búsquedas Semánticas

#### Búsqueda General
```bash
curl -X POST http://localhost:4000/api/rag/search \
  -H "Content-Type: application/json" \
  -d '{
    "query": "trabajo remoto desde casa",
    "topK": 3,
    "minSimilarity": 0.4
  }'
```

#### Búsqueda en Documento Específico
```bash
curl -X POST http://localhost:4000/api/rag/search \
  -H "Content-Type: application/json" \
  -d '{
    "query": "beneficios empleados",
    "topK": 5,
    "documentId": "doc_123"
  }'
```

### 3. Chat Conversacional

#### Pregunta Simple
```bash
curl -X POST http://localhost:4000/api/rag/chat \
  -H "Content-Type: application/json" \
  -d '{
    "message": "¿Cuántos días de vacaciones tengo?"
  }'
```

#### Pregunta Compleja
```bash
curl -X POST http://localhost:4000/api/rag/chat \
  -H "Content-Type: application/json" \
  -d '{
    "message": "Compara las políticas de trabajo remoto con las de vacaciones",
    "maxTokens": 700,
    "temperature": 0.6
  }'
```

---

## 🧪 Testing y Validación

### Estructura de Tests

```
tests/
├── test_python_chroma.js    # Test integración ChromaDB
├── test_rag.js             # Test sistema RAG completo
├── test_service.js         # Test servicios individuales
├── test_simple.js          # Tests básicos
└── README.md               # Documentación de tests
```

### Ejecutar Tests

#### Test Completo del Sistema
```bash
node tests/test_python_chroma.js
```

#### Test Específico RAG
```bash
node tests/test_rag.js
```

### Colección Postman

**Ubicación**: `postman/RAG_API_Postman_Collection.json`

**Categorías incluidas:**
- 🏥 Health Checks
- 📄 Document Management
- 🔍 Search & Retrieval
- 💬 RAG Chat
- 🧹 Administration
- 🧪 Testing & Validation

**Configuración:**
```json
{
  "BASE_URL": "https://tu-app.cfapps.sap.hana.ondemand.com",
  "DOCUMENT_ID": "doc_ejemplo_123"
}
```

### Métricas de Performance

#### Tiempos de Respuesta Esperados
- **Health Check**: < 100ms
- **Búsqueda semántica**: 200-500ms
- **Chat RAG**: 1-3 segundos
- **Upload documento**: 2-10 segundos (según tamaño)

#### Uso de Recursos
- **Memoria base**: ~200MB
- **Memoria con documentos**: ~400-600MB
- **CPU**: Bajo en idle, picos durante procesamiento
- **Almacenamiento**: Variable según documentos

---

## 📊 Mantenimiento y Monitoreo

### Logs del Sistema

#### Ubicaciones de Logs
- **Cloud Foundry**: `cf logs ai_core_api`
- **Local**: Consola del servidor

#### Tipos de Logs
```
[RAG] - Operaciones del sistema RAG
[CHROMA] - Operaciones ChromaDB
[EMBED] - Generación de embeddings
[DOC] - Procesamiento de documentos
[API] - Requests HTTP
```

### Comandos de Monitoreo

#### Cloud Foundry
```bash
# Ver estado de la aplicación
cf app ai_core_api

# Ver logs en tiempo real
cf logs ai_core_api

# Ver métricas
cf events ai_core_api

# Escalar aplicación
cf scale ai_core_api -i 2 -m 1G
```

#### Health Checks Automáticos
```bash
# Script de monitoreo
#!/bin/bash
while true; do
  curl -f https://tu-app.cfapps.sap.hana.ondemand.com/health || echo "ALERT: App down"
  sleep 60
done
```

### Mantenimiento Preventivo

#### Tareas Regulares
1. **Revisar logs** semanalmente
2. **Monitorear uso de memoria** 
3. **Verificar performance** de búsquedas
4. **Actualizar documentos** según necesidad
5. **Backup de configuración** mensualmente

#### Limpieza de Datos
```bash
# Limpiar documentos obsoletos
curl -X DELETE https://tu-app.cfapps.sap.hana.ondemand.com/api/rag/documents/doc_old

# Estadísticas para análisis
curl https://tu-app.cfapps.sap.hana.ondemand.com/api/rag/stats
```

---

## 🚨 Troubleshooting

### Problemas Comunes

#### 1. Error: "ChromaDB no disponible"
**Síntomas**: Logs muestran fallback a memoria
**Solución**: 
- Verificar configuración `VECTOR_STORE_TYPE`
- En local: iniciar `chroma_service/start_service.bat`
- En CF: verificar que ChromaDB embebido se inicialice

#### 2. Error: "SAP AI Core no responde"
**Síntomas**: Timeouts en generación de embeddings
**Solución**:
- Verificar credenciales en `default-env.json`
- Comprobar servicios bindeados: `cf services`
- Verificar conectividad de red

#### 3. Error: "Memory limit exceeded"
**Síntomas**: Aplicación se reinicia frecuentemente
**Solución**:
```bash
# Aumentar memoria en manifest.yml
memory: 1G

# O via CLI
cf scale ai_core_api -m 1G
```

#### 4. Error: "No documents found"
**Síntomas**: Búsquedas no devuelven resultados
**Solución**:
- Verificar que documentos se cargaron: `GET /api/rag/documents`
- Revisar logs de inicialización
- Re-ejecutar ingesta: `node scripts/ingest-documents.js`

### Códigos de Error

| Código | Descripción | Solución |
|--------|-------------|----------|
| 500 | Error interno del servidor | Revisar logs detallados |
| 503 | Servicio no disponible | Verificar dependencias |
| 400 | Request malformado | Validar formato JSON |
| 404 | Documento no encontrado | Verificar ID del documento |
| 413 | Archivo demasiado grande | Reducir tamaño o aumentar límites |

### Herramientas de Diagnóstico

#### Script de Diagnóstico
```bash
# Crear script de diagnóstico
node tests/test_python_chroma.js > diagnostic.log 2>&1
```

#### Verificación de Servicios
```bash
# Verificar todos los endpoints
curl https://tu-app.cfapps.sap.hana.ondemand.com/health
curl https://tu-app.cfapps.sap.hana.ondemand.com/api/rag/health
curl https://tu-app.cfapps.sap.hana.ondemand.com/api/rag/stats
```

---

## 📎 Anexos

### Anexo A: Estructura de Archivos Completa

```
aicore_api/
├── 📁 auth/
│   ├── aiCoreClient.js
│   └── tokenManager.js
├── 📁 chroma_service/
│   ├── chroma_server.py
│   ├── start_service.bat
│   └── README.md
├── 📁 docs/
│   ├── README.md
│   ├── README_RAG.md
│   ├── DEPLOY_CF_FINAL.md
│   ├── CHROMADB_SETUP.md
│   └── INDEX.md
├── 📁 postman/
│   ├── RAG_API_Postman_Collection.json
│   └── README.md
├── 📁 routes/
│   ├── chat.js
│   └── rag.js
├── 📁 sample_documents/
│   ├── empresa_politicas.md
│   └── manual_procedimientos.md
├── 📁 scripts/
│   ├── init-sample-data.js
│   ├── ingest-documents.js
│   ├── start-cf.js
│   └── README.md
├── 📁 services/
│   ├── chromaPythonClient.js
│   ├── chromaVectorStore.js
│   ├── documentProcessor.js
│   ├── embeddingService.js
│   ├── ragService.js
│   └── vectorStore.js
├── 📁 tests/
│   ├── test.js
│   ├── test_python_chroma.js
│   ├── test_rag.js
│   ├── test_service.js
│   ├── test_simple.js
│   └── README.md
├── 📁 uploads/
├── .cfignore
├── .gitignore
├── auth.js
├── default-env.json
├── manifest.yml
├── package.json
├── server.js
└── xs-security.json
```

### Anexo B: Configuración de Servicios SAP BTP

#### Servicio AI Core
```json
{
  "serviceName": "aicore",
  "serviceInstanceName": "aicore-instance",
  "endpoints": {
    "AI_API_URL": "https://api.ai.prod.eu-central-1.aws.ml.hana.ondemand.com",
    "oauth": {
      "url": "https://oauth.prod.eu-central-1.aws.ml.hana.ondemand.com"
    }
  }
}
```

#### Servicio XSUAA
```json
{
  "xsappname": "aicore-app",
  "tenant-mode": "dedicated",
  "scopes": [
    {
      "name": "$XSAPPNAME.read",
      "description": "Read access"
    },
    {
      "name": "$XSAPPNAME.write", 
      "description": "Write access"
    }
  ]
}
```

### Anexo C: Ejemplos de Documentos

#### Documento de Políticas (Markdown)
```markdown
# Políticas de la Empresa

## Políticas de Vacaciones
- Los empleados tienen derecho a 22 días de vacaciones al año
- Las vacaciones deben solicitarse con 15 días de antelación

## Políticas de Trabajo Remoto
- El trabajo remoto está permitido hasta 3 días por semana
- Se requiere aprobación del supervisor
```

#### Metadatos de Documento
```json
{
  "documentId": "doc_123",
  "fileName": "politicas_empresa.md",
  "uploadedAt": "2024-10-08T12:30:00Z",
  "fileSize": 2048,
  "chunks": 5,
  "tags": ["politicas", "empresa", "rrhh"],
  "description": "Políticas corporativas actualizadas"
}
```

### Anexo D: Comandos de Referencia Rápida

#### Desarrollo Local
```bash
# Iniciar ChromaDB
cd chroma_service && start_service.bat

# Iniciar servidor
npm start

# Ejecutar tests
node tests/test_python_chroma.js

# Ingestar documentos
node scripts/ingest-documents.js
```

#### Cloud Foundry
```bash
# Login y configuración
cf login -a https://api.cf.eu10.hana.ondemand.com
cf target -o "ORG" -s "SPACE"

# Despliegue
cf push

# Monitoreo
cf logs ai_core_api
cf app ai_core_api
```

#### API Testing
```bash
# Health check
curl https://app-url/health

# Búsqueda
curl -X POST https://app-url/api/rag/search \
  -H "Content-Type: application/json" \
  -d '{"query": "vacaciones", "topK": 3}'

# Chat
curl -X POST https://app-url/api/rag/chat \
  -H "Content-Type: application/json" \
  -d '{"message": "¿Cuáles son las políticas?"}'
```

---

## 📞 Contacto y Soporte

**Equipo de Desarrollo**: [equipo@empresa.com]  
**Documentación**: [docs-url]  
**Repositorio**: [git-repo-url]  
**Issues**: [issues-url]  

---

**© 2024 - Sistema RAG con SAP AI Core y ChromaDB**  
**Versión de Documentación**: 1.0.0  
**Última Actualización**: Octubre 2024
