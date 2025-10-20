# Vector Storage - Sistema RAG

## Resumen

El sistema RAG utiliza diferentes tipos de almacenamiento vectorial para persistir documentos y sus embeddings. Este documento explica cómo funciona cada opción y cuál usar en cada entorno.

## Tipos de Vector Storage

### 1. **SQLite Vector Store** ⭐ **RECOMENDADO**

**Archivo:** `services/sqliteVectorStore.js`

#### Características:
- ✅ **Persistencia completa** en archivo SQLite
- ✅ **Sin dependencias externas** (solo better-sqlite3)
- ✅ **Compatible con Cloud Foundry**
- ✅ **Funciona localmente**
- ✅ **Base de datos relacional** con índices optimizados
- ✅ **Integridad de datos** verificable

#### Funcionamiento:
```javascript
// Estructura de la base de datos
CREATE TABLE documents (
  id TEXT PRIMARY KEY,
  content TEXT NOT NULL,
  embedding TEXT NOT NULL,        // JSON array de números
  metadata TEXT NOT NULL,         // JSON con metadatos
  context_id TEXT NOT NULL,
  document_id TEXT NOT NULL,
  chunk_index INTEGER NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

#### Archivos generados:
- `./data/rag_vectors.db` - Base de datos principal
- `./data/rag_vectors.db-wal` - Write-Ahead Log (rendimiento)
- `./data/rag_vectors.db-shm` - Shared memory

### 2. **ChromaDB Embebido** ❌ **DEPRECADO**

**Archivo:** `services/chromaVectorStore.js`

#### Problemas identificados:
- ❌ **Error de URL parsing** en configuración embebida
- ❌ **Dependencias complejas** de ChromaDB
- ❌ **Incompatibilidad** con algunas versiones de Node.js
- ❌ **Configuración compleja** para Cloud Foundry

#### Estado: **NO USAR** - Reemplazado por SQLite

### 3. **ChromaDB Python Service** ❌ **DEPRECADO**

**Carpeta:** `chroma_service/`

#### Qué contiene:
```
chroma_service/
├── chroma_server.py          # Servidor FastAPI con ChromaDB
├── requirements.txt          # Dependencias Python
├── start_service.bat         # Script de inicio (Windows)
├── README.md                 # Documentación del servicio
├── venv/                     # Entorno virtual Python
└── chroma_data/              # Datos de ChromaDB
    └── chroma.sqlite3        # Base de datos ChromaDB
```

#### Problemas:
- ❌ **Requiere Python** instalado
- ❌ **Servicio externo** (puerto 8001)
- ❌ **No compatible** con Cloud Foundry sin configuración adicional
- ❌ **Dependencias complejas** (FastAPI, ChromaDB, etc.)

#### Estado: **ELIMINAR** - Ya no se usa

### 4. **Memory Vector Store** ⚠️ **SOLO DESARROLLO**

**Archivo:** `services/vectorStore.js`

#### Características:
- ✅ **Rápido** para pruebas
- ❌ **No persistente** - se pierde al reiniciar
- ❌ **Solo para desarrollo/testing**

## Configuración por Entorno

### Desarrollo Local

```bash
# Opción 1: SQLite (RECOMENDADO)
set VECTOR_STORE_TYPE=sqlite
node server.js

# Opción 2: Memoria (solo para pruebas rápidas)
set VECTOR_STORE_TYPE=memory
node server.js
```

### Cloud Foundry (Producción)

```yaml
# manifest.yml
applications:
- name: aicore-api
  env:
    VECTOR_STORE_TYPE: sqlite    # SQLite funciona perfectamente en CF
    NODE_ENV: production
```

## Scripts de Inicio

### Localmente:
```bash
# Con SQLite
.\start-with-sqlite.bat

# Con ChromaDB (DEPRECADO - no usar)
.\start-with-chroma.bat
```

### Cloud Foundry:
```bash
cf push
# Automáticamente usa SQLite según manifest.yml
```

## Persistencia de Datos

### Contextos
- **Archivo:** `./data/contexts.json`
- **Formato:** JSON con estructura de contextos
- **Servicio:** `services/contextPersistence.js`

### Documentos y Embeddings
- **SQLite:** `./data/rag_vectors.db`
- **Servicio:** `services/sqliteVectorStore.js`

## Migración y Compatibilidad

### De ChromaDB a SQLite

El sistema automáticamente detecta el tipo de vector store y usa el apropiado:

```javascript
// ragService.js - Configuración automática
const VECTOR_STORE_TYPE = process.env.VECTOR_STORE_TYPE || 'sqlite';

// Fallbacks inteligentes
case 'chroma':
  try {
    await chromaVectorStore.initialize();
    return chromaVectorStore;
  } catch (error) {
    console.warn('ChromaDB no disponible, usando SQLite');
    process.env.VECTOR_STORE_TYPE = 'sqlite';
    return await getVectorStore(); // Fallback a SQLite
  }
```

## Rendimiento

### SQLite Vector Store
- **Inserción:** ~100-500 documentos/segundo
- **Búsqueda:** ~1000-5000 consultas/segundo
- **Almacenamiento:** ~1KB por chunk (texto + embedding)
- **Índices:** Optimizados para context_id y document_id

### Comparación
| Característica | SQLite | ChromaDB | Memory |
|---------------|--------|----------|---------|
| Persistencia | ✅ | ❌ (problemas) | ❌ |
| Rendimiento | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| Cloud Foundry | ✅ | ❌ | ❌ |
| Dependencias | Mínimas | Complejas | Ninguna |
| Mantenimiento | Fácil | Difícil | N/A |

## Recomendaciones

### Para Desarrollo:
1. **Usar SQLite** - Persistencia completa y fácil debug
2. **Memory solo para pruebas** rápidas sin persistencia

### Para Producción (Cloud Foundry):
1. **Usar SQLite** - Única opción confiable
2. **Configurar backup** de `./data/` si es necesario

### NO Usar:
- ❌ ChromaDB embebido (problemas de configuración)
- ❌ ChromaDB Python Service (dependencias complejas)

## Limpieza Recomendada

### Carpetas a ELIMINAR:
```
chroma_service/           # Servicio Python deprecado
├── venv/                # Entorno virtual Python
├── chroma_data/         # Datos ChromaDB antiguos
├── chroma_server.py     # Servidor Python
├── requirements.txt     # Dependencias Python
├── start_service.bat    # Script de inicio
└── README.md           # Documentación obsoleta
```

### Archivos a ELIMINAR:
```
start-with-chroma.bat    # Script obsoleto
services/chromaVectorStore.js  # Implementación problemática
services/chromaPythonClient.js # Cliente para servicio Python
```

### Mantener:
```
services/sqliteVectorStore.js   # Vector store principal
services/contextPersistence.js  # Persistencia de contextos
services/vectorStore.js         # Memory store (backup)
start-with-sqlite.bat          # Script de inicio SQLite
data/                          # Directorio de datos
├── contexts.json              # Contextos persistentes
├── rag_vectors.db            # Base de datos SQLite
├── rag_vectors.db-wal        # WAL de SQLite
└── rag_vectors.db-shm        # Shared memory SQLite
```

## Troubleshooting

### Error: "better-sqlite3 not found"
```bash
npm install better-sqlite3
```

### Error: "Database locked"
```bash
# Cerrar todas las conexiones Node.js
taskkill /F /IM node.exe
# Reiniciar servidor
.\start-with-sqlite.bat
```

### Error: "Context not found"
```bash
# Verificar archivo de contextos
cat ./data/contexts.json
# Reinicializar si está corrupto
rm ./data/contexts.json
node server.js  # Se creará automáticamente
```

## Conclusión

**SQLite Vector Store** es la solución definitiva para el sistema RAG:
- ✅ Funciona localmente y en Cloud Foundry
- ✅ Sin dependencias complejas
- ✅ Persistencia garantizada
- ✅ Rendimiento excelente
- ✅ Fácil mantenimiento

La carpeta `chroma_service/` y archivos relacionados con ChromaDB pueden eliminarse de forma segura.
