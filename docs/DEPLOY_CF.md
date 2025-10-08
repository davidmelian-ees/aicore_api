# 🚀 Despliegue en Cloud Foundry

Guía completa para desplegar el sistema RAG + AI Core en Cloud Foundry.

## 📋 Prerequisitos

1. **CF CLI instalado** y configurado
2. **Servicio AI Core** disponible en tu espacio CF
3. **Servicio XSUAA** configurado (aicore-app-auth)

## 🛠️ Configuración del Sistema

### Arquitectura en Cloud Foundry

```
Cloud Foundry Application
├── Node.js Server (Puerto dinámico)
├── ChromaDB Embebido (Persistente)
├── SAP AI Core Integration
├── Datos de ejemplo pre-cargados
└── Autenticación XSUAA habilitada
```

### Diferencias vs Local

| Componente | Local | Cloud Foundry |
|------------|-------|---------------|
| **Vector Store** | ChromaDB Python Service | ChromaDB Embebido |
| **Autenticación** | Deshabilitada | XSUAA habilitada |
| **Datos** | Manual upload | Pre-cargados automáticamente |
| **Puerto** | 4000 + 8001 | Dinámico (CF_PORT) |
| **Persistencia** | Archivos externos | Archivos locales CF |

## 🚀 Comandos de Despliegue

### 1. Preparar el entorno

```bash
# Verificar que estás en la carpeta correcta
cd aicore_api

# Verificar archivos críticos
ls -la manifest.yml
ls -la package.json
ls -la server.js
```

### 2. Desplegar en Cloud Foundry

```bash
# Despliegue básico
cf push

# O con nombre específico
cf push ai-core-rag-api

# Verificar estado
cf apps
```

### 3. Verificar servicios

```bash
# Ver servicios bindeados
cf services

# Verificar que aicore-app-auth esté disponible
cf service aicore-app-auth
```

## 🔍 Verificación Post-Despliegue

### 1. Health Check

```bash
# Obtener URL de la app
cf app ai_core_api

# Verificar salud
curl https://your-app-url.cfapps.sap.hana.ondemand.com/health
```

**Respuesta esperada:**
```json
{
  "status": "healthy",
  "timestamp": "2024-10-08T11:30:00Z",
  "environment": "production",
  "vectorStore": "chroma"
}
```

### 2. Verificar RAG System

```bash
# Verificar sistema RAG
curl https://your-app-url.cfapps.sap.hana.ondemand.com/api/rag/health

# Ver documentos pre-cargados
curl https://your-app-url.cfapps.sap.hana.ondemand.com/api/rag/documents

# Probar búsqueda
curl -X POST https://your-app-url.cfapps.sap.hana.ondemand.com/api/rag/search \
  -H "Content-Type: application/json" \
  -d '{"query": "políticas de vacaciones", "topK": 3}'
```

### 3. Probar Chat RAG

```bash
curl -X POST https://your-app-url.cfapps.sap.hana.ondemand.com/api/rag/chat \
  -H "Content-Type: application/json" \
  -d '{"message": "¿Cuáles son las políticas de vacaciones?"}'
```

## 📊 Monitoreo y Logs

### Ver logs en tiempo real

```bash
# Logs de la aplicación
cf logs ai_core_api

# Logs recientes
cf logs ai_core_api --recent
```

### Logs esperados al iniciar

```
🌍 Entorno: Producción (Cloud Foundry)
📊 Vector Store: memory
🔐 Inicializando autenticación para producción...
🚀 AI Core API running on port 8080
📄 Inicializando datos de ejemplo para Cloud Foundry...
📝 Creando documento: empresa_politicas.md
🔍 Indexando: empresa_politicas.md
✅ Indexado: empresa_politicas.md (8 chunks)
✅ Inicialización de datos de ejemplo completada
```

## 🔧 Configuración Avanzada

### Variables de Entorno

Puedes modificar el comportamiento editando `manifest.yml`:

```yaml
env:
  VECTOR_STORE_TYPE: memory          # Tipo de almacenamiento
  NODE_ENV: production               # Entorno
  LOG_LEVEL: info                    # Nivel de logging
  RAG_CHUNK_SIZE: 1000              # Tamaño de chunks
  RAG_CHUNK_OVERLAP: 200            # Solapamiento de chunks
```

### Escalado

```bash
# Escalar instancias
cf scale ai_core_api -i 2

# Cambiar memoria
cf scale ai_core_api -m 1G

# Reiniciar aplicación
cf restart ai_core_api
```

## 🚨 Troubleshooting

### Error: "Application failed to start"

```bash
# Ver logs detallados
cf logs ai_core_api --recent

# Verificar servicios
cf services
cf service aicore-app-auth
```

### Error: "Service binding failed"

```bash
# Re-bindear servicio
cf unbind-service ai_core_api aicore-app-auth
cf bind-service ai_core_api aicore-app-auth
cf restart ai_core_api
```

### Error: "Memory limit exceeded"

```bash
# Aumentar memoria en manifest.yml
memory: 1G

# O via CLI
cf scale ai_core_api -m 1G
```

### Error: "No documents found"

Los documentos se cargan automáticamente. Si no aparecen:

```bash
# Verificar logs de inicialización
cf logs ai_core_api --recent | grep "Inicializando datos"

# Reiniciar para forzar recarga
cf restart ai_core_api
```

## 📈 Performance en Cloud Foundry

### Métricas Esperadas

- **Startup time**: ~30-60 segundos
- **Memory usage**: ~200-400MB
- **Response time**: ~200-500ms para búsquedas
- **Concurrent users**: ~50-100 (con 512MB RAM)

### Optimizaciones

1. **Aumentar memoria** si hay muchos documentos
2. **Usar múltiples instancias** para alta disponibilidad
3. **Configurar health checks** apropiados
4. **Monitorear logs** regularmente

## 🎯 Próximos Pasos Post-Despliegue

1. **Configurar dominio personalizado** si es necesario
2. **Agregar más documentos** via API REST
3. **Integrar con frontend** o aplicaciones cliente
4. **Configurar monitoreo** y alertas
5. **Implementar CI/CD** para actualizaciones automáticas

---

**¡Tu sistema RAG está listo en Cloud Foundry!** 🎉

**URLs importantes:**
- **Health**: `https://your-app.cfapps.sap.hana.ondemand.com/health`
- **RAG API**: `https://your-app.cfapps.sap.hana.ondemand.com/api/rag`
- **Documentos**: `https://your-app.cfapps.sap.hana.ondemand.com/api/rag/documents`
