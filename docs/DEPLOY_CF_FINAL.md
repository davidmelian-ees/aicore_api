# 🚀 Guía Completa: Despliegue RAG + ChromaDB en Cloud Foundry

**Guía paso a paso probada y funcional** para desplegar el sistema RAG con ChromaDB en SAP BTP Cloud Foundry.

## 📋 Prerequisitos Verificados

✅ **SAP BTP Account** con acceso a Cloud Foundry  
✅ **AI Core Service** configurado y bindeado  
✅ **XSUAA Service** (aicore-app-auth) configurado  
✅ **CF CLI** instalado y configurado  

## 🛠️ Configuración del Sistema

### Arquitectura Final

```
Cloud Foundry Application
├── Node.js Server (Puerto dinámico CF)
├── ChromaDB Embebido (Persistente)
├── SAP AI Core Integration
├── Datos de ejemplo pre-cargados
├── Autenticación XSUAA habilitada
└── Fallback automático a memoria si ChromaDB falla
```

### Configuración de Archivos

#### `manifest.yml`
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

#### `.cfignore`
```
# Dependencias locales
node_modules/
npm-debug.log*

# Archivos de desarrollo
.env*

# ChromaDB Python service (no necesario en CF)
chroma_service/start_service.bat
chroma_service/simple_server.py
chroma_service/venv/
chroma_data/

# Scripts de desarrollo local
test_*.js
ingest_*.js
setup_*.bat
```

## 🔧 Instalación y Configuración CF CLI

### 1. Instalar CF CLI

**Opción A: Chocolatey**
```cmd
choco install cloudfoundry-cli
```

**Opción B: Descarga directa**
```cmd
powershell -Command "Invoke-WebRequest -Uri 'https://packages.cloudfoundry.org/stable?release=windows64&version=v8&source=github' -OutFile 'cf-cli.zip'"
```

### 2. Verificar instalación

```cmd
# Si está en PATH
cf --version

# Si está en AppData (como en nuestro caso)
"C:\Users\[USERNAME]\AppData\Roaming\Cloud Foundry\cf8.exe" --version
```

### 3. Configurar PATH (opcional)

```cmd
setx PATH "%PATH%;C:\Users\[USERNAME]\AppData\Roaming\Cloud Foundry"
```

## 🔐 Configuración de Autenticación

### Método 1: Login directo

```cmd
# Usar ruta completa si no está en PATH
"C:\Users\[USERNAME]\AppData\Roaming\Cloud Foundry\cf8.exe" login -a https://api.cf.eu10.hana.ondemand.com

# Con SSO (recomendado para empresas)
"C:\Users\[USERNAME]\AppData\Roaming\Cloud Foundry\cf8.exe" login -a https://api.cf.eu10.hana.ondemand.com --sso
```

### Método 2: Exportar desde SAP Business Application Studio

**En BAS:**
```bash
# Ver configuración actual
cf target

# Obtener información completa
cf orgs
cf spaces

# Configurar target
cf target -o "TU_ORGANIZACION" -s "TU_ESPACIO"
```

**Localmente:**
- Copiar archivo `~/.cf/config.json` desde BAS
- Pegar en `C:\Users\[USERNAME]\.cf\config.json`

### 3. Configurar organización y espacio

```cmd
# Ver organizaciones disponibles
cf orgs

# Ver espacios disponibles
cf spaces

# Configurar target (usar comillas para nombres con espacios)
cf target -o "NOMBRE DE ORGANIZACION" -s "nombre-espacio"

# Verificar configuración
cf target
```

## 🚀 Proceso de Despliegue

### 1. Preparar el proyecto

```cmd
# Navegar al directorio del proyecto
cd c:\Users\[USERNAME]\CascadeProjects\aicore_api\aicore_api

# Verificar archivos críticos
dir manifest.yml
dir package.json
dir server.js
```

### 2. Verificar servicios

```cmd
# Listar servicios disponibles
cf services

# Verificar que aicore-app-auth existe
cf service aicore-app-auth
```

### 3. Desplegar aplicación

```cmd
# Despliegue básico
cf push

# O con nombre específico
cf push ai-core-rag-api

# Seguir logs durante despliegue
cf logs ai_core_api
```

## 📊 Verificación Post-Despliegue

### 1. Verificar estado de la aplicación

```cmd
# Ver aplicaciones
cf apps

# Ver detalles de la app
cf app ai_core_api

# Ver logs
cf logs ai_core_api --recent
```

### 2. Probar endpoints

```bash
# Health check general
curl https://tu-app.cfapps.sap.hana.ondemand.com/health

# Health check RAG
curl https://tu-app.cfapps.sap.hana.ondemand.com/api/rag/health

# Ver documentos pre-cargados
curl https://tu-app.cfapps.sap.hana.ondemand.com/api/rag/documents
```

**Respuesta esperada del health check:**
```json
{
  "status": "healthy",
  "timestamp": "2024-10-08T12:20:00Z",
  "environment": "production",
  "vectorStore": "chroma"
}
```

### 3. Probar funcionalidad RAG

```bash
# Buscar en documentos
curl -X POST https://tu-app.cfapps.sap.hana.ondemand.com/api/rag/search \
  -H "Content-Type: application/json" \
  -d '{"query": "políticas de vacaciones", "topK": 3}'

# Chat RAG
curl -X POST https://tu-app.cfapps.sap.hana.ondemand.com/api/rag/chat \
  -H "Content-Type: application/json" \
  -d '{"message": "¿Cuáles son las políticas de la empresa?"}'
```

## 🔧 Características del Sistema Desplegado

### Vector Store: ChromaDB Embebido

- ✅ **Persistencia**: Los datos sobreviven a reinicios
- ✅ **Performance**: Sin latencia de red (embebido)
- ✅ **Fallback**: Automático a memoria si ChromaDB falla
- ✅ **Escalabilidad**: Maneja grandes volúmenes

### Datos Pre-cargados

El sistema incluye automáticamente:

1. **`empresa_politicas.md`**
   - Políticas de vacaciones
   - Políticas de trabajo remoto
   - Políticas de seguridad
   - Beneficios para empleados

2. **`manual_procedimientos.md`**
   - Procedimiento de onboarding
   - Procedimiento de solicitud de vacaciones
   - Procedimiento de trabajo remoto
   - Procedimientos de emergencia

### Configuración Automática

- **Autenticación**: XSUAA habilitada automáticamente
- **Puerto**: Asignado dinámicamente por Cloud Foundry
- **Logging**: Configurado para producción
- **Inicialización**: Datos de ejemplo cargados al arrancar

## 🚨 Troubleshooting

### Error: "No org targeted"

```cmd
# Ver organizaciones disponibles
cf orgs

# Configurar target con comillas para nombres con espacios
cf target -o "NOMBRE COMPLETO DE ORGANIZACION" -s "espacio"
```

### Error: "Service not found"

```cmd
# Verificar servicios disponibles
cf services

# Verificar que el servicio existe
cf service aicore-app-auth

# Si no existe, contactar al administrador SAP BTP
```

### Error: "Application failed to start"

```cmd
# Ver logs detallados
cf logs ai_core_api --recent

# Verificar memoria asignada
cf app ai_core_api

# Aumentar memoria si es necesario
cf scale ai_core_api -m 1G
```

### Error: "ChromaDB initialization failed"

El sistema tiene fallback automático:
- Si ChromaDB falla → Usa vector store en memoria
- Los logs mostrarán: `[RAG] ⚠️ ChromaDB no disponible, usando memoria`
- La funcionalidad RAG sigue funcionando

## 📈 Monitoreo y Mantenimiento

### Comandos útiles

```cmd
# Ver estado de la aplicación
cf app ai_core_api

# Ver logs en tiempo real
cf logs ai_core_api

# Reiniciar aplicación
cf restart ai_core_api

# Escalar aplicación
cf scale ai_core_api -i 2 -m 1G

# Ver métricas
cf events ai_core_api
```

### URLs importantes

- **Aplicación**: `https://ai-core-api.cfapps.sap.hana.ondemand.com`
- **Health Check**: `/health`
- **RAG API**: `/api/rag`
- **Documentos**: `/api/rag/documents`
- **Búsqueda**: `/api/rag/search`
- **Chat**: `/api/rag/chat`

## 🎯 Próximos Pasos

1. **Agregar más documentos** via API REST
2. **Configurar dominio personalizado** si es necesario
3. **Implementar CI/CD** para actualizaciones automáticas
4. **Configurar monitoreo** y alertas
5. **Integrar con aplicaciones frontend**

## 📋 Checklist de Despliegue Exitoso

- [ ] CF CLI instalado y configurado
- [ ] Login en Cloud Foundry exitoso
- [ ] Organización y espacio configurados
- [ ] Servicios (aicore-app-auth) disponibles
- [ ] `cf push` ejecutado sin errores
- [ ] Aplicación en estado "running"
- [ ] Health check responde correctamente
- [ ] RAG API funcional
- [ ] Documentos pre-cargados visibles
- [ ] Búsqueda y chat funcionando

---

**¡Sistema RAG con ChromaDB desplegado exitosamente en Cloud Foundry!** 🎉

**Configuración final:**
- ✅ **ChromaDB embebido** con persistencia
- ✅ **Autenticación XSUAA** habilitada
- ✅ **Datos de ejemplo** pre-cargados
- ✅ **Fallback automático** a memoria
- ✅ **API REST completa** disponible

**Estado**: Producción ✅  
**Vector Store**: ChromaDB ✅  
**Persistencia**: Habilitada ✅
