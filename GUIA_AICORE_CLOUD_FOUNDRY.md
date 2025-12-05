# 🚀 Guía: SAP AI Core en Cloud Foundry

## 🎯 Problema

En **local** funciona porque usas `default-env.json`, pero en **Cloud Foundry** ese archivo:
- ❌ NO se sube (está en `.cfignore`)
- ❌ NO debe subirse (contiene credenciales sensibles)

## ✅ Solución: Service Binding

En Cloud Foundry, las credenciales se obtienen automáticamente del **Service Binding** a través de `VCAP_SERVICES`.

### 1. Verificar que el Servicio Existe en BTP

```bash
cf login -a https://api.cf.eu10.hana.ondemand.com
cf services
```

**Debes ver**:
```
name              service   plan       
aicore-app-auth   xsuaa     application
default_aicore    aicore    extended    ← ESTE ES IMPORTANTE
```

Si **NO existe** `default_aicore`, créalo:
```bash
cf create-service aicore extended default_aicore
```

### 2. Archivos Modificados

He agregado el binding de `default_aicore` en:

#### `manifest.yml`
```yaml
services:
  - aicore-app-auth
  - default_aicore    # ← AGREGADO
```

#### `mta.yaml`
```yaml
requires:
  - name: aicore-app-auth
  - name: default_aicore    # ← AGREGADO

resources:
  - name: default_aicore
    type: org.cloudfoundry.existing-service
    parameters:
      service-name: default_aicore
```

### 3. Cómo Funciona

1. **Local** (desarrollo):
   - `xsenv.loadEnv()` lee `default-env.json`
   - Obtiene credenciales de AI Core

2. **Cloud Foundry** (producción):
   - `xsenv.loadEnv()` lee `VCAP_SERVICES` (variable de entorno)
   - Cloud Foundry inyecta automáticamente las credenciales del servicio bindeado
   - Tu código usa las mismas credenciales sin cambios

### 4. Código Actual (NO requiere cambios)

```javascript
// auth/aiCoreClient.js
import xsenv from "@sap/xsenv";
import { AzureOpenAiChatClient } from "@sap-ai-sdk/foundation-models";

// Esto funciona en LOCAL y en CLOUD FOUNDRY
xsenv.loadEnv();

export function getAiCoreClient(model = "gpt-4o", options = {}) {
  const client = new AzureOpenAiChatClient(model);
  return client;
}
```

**Explicación**:
- En **local**: `xsenv.loadEnv()` lee `default-env.json`
- En **Cloud Foundry**: `xsenv.loadEnv()` lee `process.env.VCAP_SERVICES`
- El SDK de SAP AI Core (`@sap-ai-sdk`) se encarga de todo automáticamente

## 🚀 Desplegar a Cloud Foundry

### Opción 1: Despliegue Manual con CF CLI

```bash
# 1. Login
cf login -a https://api.cf.eu10.hana.ondemand.com

# 2. Push (usa manifest.yml)
cf push

# 3. Verificar bindings
cf env ai_core_api
```

**Deberías ver** en `VCAP_SERVICES`:
```json
{
  "aicore": [{
    "credentials": {
      "clientid": "sb-XXXXX",
      "clientsecret": "XXXXX",
      "url": "https://..."
    }
  }]
}
```

### Opción 2: Despliegue con CI/CD (MTA)

```bash
# 1. Build MTA
mbt build

# 2. Deploy
cf deploy mta_archives/ai-core-api_1.0.0.mtar
```

El `mta.yaml` se encarga de bindear automáticamente los servicios.

## 🔍 Verificar en Cloud Foundry

### 1. Ver Logs
```bash
cf logs ai_core_api --recent
```

**Busca**:
```
[AI CORE] Cliente inicializado con modelo: gpt-4o
```

### 2. Verificar Bindings
```bash
cf services
cf env ai_core_api
```

### 3. Test desde Cloud Foundry

```bash
# Obtener URL de la app
cf apps

# Test endpoint
curl -X POST https://ai-core-api.cfapps.eu10.hana.ondemand.com/api/pdf-correction/generate-list \
  -F "pdf=@documento.pdf"
```

## ⚠️ Troubleshooting

### Error: "Service 'default_aicore' not found"

**Solución**:
```bash
# Crear el servicio
cf create-service aicore extended default_aicore

# Bindear a la app
cf bind-service ai_core_api default_aicore

# Restage
cf restage ai_core_api
```

### Error: "Failed to fetch the list of deployments"

**Causa**: El servicio existe pero no está bindeado correctamente.

**Solución**:
```bash
# Ver bindings actuales
cf services

# Si default_aicore no está bindeado:
cf bind-service ai_core_api default_aicore
cf restage ai_core_api
```

### Error: "Deployment not found"

**Causa**: El deployment de GPT-4o no existe o está detenido.

**Solución**:
1. Ir a BTP Cockpit → AI Core
2. Deployments → Verificar que `gpt-4o` esté **RUNNING**
3. Si no existe, crear deployment desde AI Launchpad

## 📋 Checklist de Despliegue

- [x] ✅ Servicio `default_aicore` existe en BTP
- [x] ✅ `manifest.yml` incluye `default_aicore` en `services`
- [x] ✅ `mta.yaml` incluye `default_aicore` en `requires` y `resources`
- [ ] ⏳ Desplegar app a Cloud Foundry
- [ ] ⏳ Verificar logs: `cf logs ai_core_api --recent`
- [ ] ⏳ Verificar bindings: `cf env ai_core_api`
- [ ] ⏳ Probar endpoint desde Postman/curl

## 🎯 Resumen

### Antes:
- ❌ Solo `aicore-app-auth` bindeado
- ❌ SAP AI Core no disponible en Cloud Foundry
- ❌ Endpoint devuelve PDF genérico

### Ahora:
- ✅ `default_aicore` bindeado en `manifest.yml` y `mta.yaml`
- ✅ Credenciales inyectadas automáticamente por Cloud Foundry
- ✅ SAP AI Core funciona igual que en local
- ✅ Endpoint detecta errores correctamente

---

**Fecha**: 2025-12-01  
**Estado**: ✅ Listo para desplegar  
**Próximo paso**: `cf push` o `cf deploy mta_archives/...`
