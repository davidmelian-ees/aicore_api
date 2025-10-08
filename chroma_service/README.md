# 🐍 ChromaDB Python Service para Sistema RAG

Servicio independiente de ChromaDB desarrollado en Python con FastAPI, proporcionando persistencia vectorial completa para el sistema RAG.

## 🚀 Características

- **FastAPI** - API REST moderna y rápida
- **ChromaDB** - Base de datos vectorial persistente
- **Instalación simplificada** - Sin problemas de compilación
- **Auto-documentación** - Swagger UI integrado
- **Logging completo** - Monitoreo detallado
- **Persistencia local** - Datos guardados en `./chroma_data`

## 📋 Estructura del Servicio

```
chroma_service/
├── chroma_server.py      # Servidor principal FastAPI (simplificado)
├── start_service.bat     # Script de inicio automático
├── README.md            # Esta documentación
└── chroma_data/         # Datos persistentes (se crea automáticamente)
```

## 🛠️ Instalación y Configuración

### 1. Inicio Rápido

```bash
# Navegar a la carpeta del servicio
cd chroma_service

# Ejecutar script de instalación e inicio
start_service.bat
```

El script automáticamente:
- ✅ Verifica Python
- ✅ Instala dependencias básicas
- ✅ Inicia el servicio

### 2. Instalación Manual

```bash
# Instalar dependencias básicas
pip install chromadb
pip install fastapi
pip install uvicorn

# Iniciar servicio
python chroma_server.py
```

## 🌐 Endpoints Disponibles

### Información del Servicio
- **GET /** - Información general y endpoints disponibles
- **GET /health** - Estado de salud del servicio
- **GET /stats** - Estadísticas de la colección
- **GET /docs** - Documentación interactiva Swagger UI

### Gestión de Documentos
- **POST /documents** - Agregar documento con embedding
- **GET /documents** - Listar todos los documentos
- **DELETE /documents/{doc_id}** - Eliminar documento específico

### Búsqueda Vectorial
- **POST /search** - Buscar documentos similares usando embedding

### Administración
- **DELETE /clear?confirm=DELETE_ALL** - Limpiar toda la colección

## 📊 Uso desde Node.js

El sistema RAG se conecta automáticamente usando `chromaPythonClient.js`:

```javascript
// Configuración en ragService.js
const VECTOR_STORE_TYPE = 'python'; // Usa el servicio Python

// Uso automático
await indexDocument('./documento.txt', 'text/plain', metadata);
const results = await searchContext('consulta de ejemplo');
```

## 🧪 Testing del Servicio

### 1. Verificar que esté corriendo:

```bash
curl http://localhost:8001/health
```

**Respuesta esperada:**
```json
{
  "status": "healthy",
  "message": "ChromaDB funcionando correctamente",
  "timestamp": "2024-10-08T10:30:00Z",
  "collection_info": {
    "name": "rag_documents",
    "document_count": 0,
    "data_directory": "./chroma_data"
  }
}
```

### 2. Ver documentación interactiva:

Abrir en navegador: http://localhost:8001/docs

### 3. Agregar un documento de prueba:

```bash
curl -X POST http://localhost:8001/documents \
  -H "Content-Type: application/json" \
  -d '{
    "id": "test_doc_1",
    "content": "Este es un documento de prueba para el sistema RAG",
    "embedding": [0.1, 0.2, 0.3, 0.4, 0.5],
    "metadata": {
      "fileName": "test.txt",
      "documentId": "test_doc",
      "chunkIndex": 0
    }
  }'
```

### 4. Buscar documentos:

```bash
curl -X POST http://localhost:8001/search \
  -H "Content-Type: application/json" \
  -d '{
    "query_embedding": [0.1, 0.2, 0.3, 0.4, 0.5],
    "top_k": 3
  }'
```

## 📁 Persistencia de Datos

Los datos se almacenan en `./chroma_data/`:
- ✅ **Sobrevive a reinicios** del servicio
- ✅ **Backup automático** por ChromaDB
- ✅ **Escalable** para grandes volúmenes

### Backup Manual

```bash
# Copiar directorio de datos
cp -r chroma_data chroma_data_backup_$(date +%Y%m%d)
```

### Restaurar Backup

```bash
# Parar servicio
# Restaurar datos
cp -r chroma_data_backup_20241008 chroma_data
# Reiniciar servicio
```

## 🔧 Configuración Avanzada

### Variables de Entorno

```bash
# Puerto del servicio (por defecto: 8001)
export CHROMA_PORT=8001

# Directorio de datos (por defecto: ./chroma_data)
export CHROMA_DATA_DIR=./chroma_data

# Nivel de logging (por defecto: INFO)
export LOG_LEVEL=DEBUG
```

### Modificar Puerto

Editar `chroma_server.py` línea final:
```python
uvicorn.run(
    app,
    host="0.0.0.0",
    port=8002,  # Cambiar puerto aquí
    log_level="info"
)
```

## 🚨 Troubleshooting

### Error: "Python no está instalado"

```
❌ Python no esta instalado
```

**Solución:**
1. Descargar Python desde: https://www.python.org/downloads/
2. Durante instalación, marcar "Add Python to PATH"
3. Reiniciar terminal

### Error: "ModuleNotFoundError: No module named 'chromadb'"

```
ModuleNotFoundError: No module named 'chromadb'
```

**Solución:**
```bash
# Activar entorno virtual
venv\Scripts\activate

# Reinstalar dependencias
pip install -r requirements.txt
```

### Error: "Address already in use"

```
ERROR: [Errno 10048] Only one usage of each socket address is normally permitted
```

**Solución:**
1. Cambiar puerto en `chroma_server.py`
2. O matar proceso existente:
   ```bash
   # Windows
   netstat -ano | findstr :8001
   taskkill /PID <PID_NUMBER> /F
   ```

### Error: "Permission denied"

```
PermissionError: [Errno 13] Permission denied: './chroma_data'
```

**Solución:**
```bash
# Crear directorio manualmente
mkdir chroma_data

# O ejecutar como administrador
```

## 📈 Performance y Escalabilidad

### Métricas Esperadas
- **Startup**: ~2-3 segundos
- **Agregar documento**: ~50-100ms
- **Búsqueda**: ~100-300ms
- **Memoria**: ~50-100MB base

### Optimizaciones
- Usar SSD para `chroma_data`
- Aumentar RAM disponible
- Optimizar tamaño de embeddings
- Usar índices apropiados en ChromaDB

## 🔄 Integración con Sistema RAG

### Configuración en Node.js

```javascript
// En ragService.js
const VECTOR_STORE_TYPE = 'python'; // Usar servicio Python

// El sistema automáticamente:
// 1. Detecta si el servicio Python está disponible
// 2. Si no, hace fallback a memoria
// 3. Logs informativos sobre el estado
```

### Flujo de Datos

```
Node.js RAG Service
       ↓ HTTP API
Python ChromaDB Service  
       ↓ Local Storage
    ./chroma_data/
```

## 🎯 Próximos Pasos

1. **Iniciar servicio**: `cd chroma_service && start_service.bat`
2. **Verificar funcionamiento**: `curl http://localhost:8001/health`
3. **Iniciar sistema RAG**: `cd .. && npm start`
4. **Probar integración**: Subir documentos y hacer consultas

---

**¡El servicio ChromaDB Python está listo para usar!** 🎉

**Puerto**: http://localhost:8001  
**Documentación**: http://localhost:8001/docs  
**Datos**: ./chroma_data/  
**Logs**: Consola del servicio
