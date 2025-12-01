# 🚀 Guía CI/CD para BTP Cloud Foundry (Producción)

## 📋 Problema Resuelto

BTP CI/CD requiere **MTA (Multi-Target Application)** en lugar de `manifest.yml` simple.

## ✅ Archivos Creados

1. **`mta.yaml`** - Descriptor MTA principal
2. **`mta-prod.mtaext`** - Extensión para producción
3. **`.mbtrc`** - Configuración de build

## 🔧 Configuración del Job en BTP CI/CD

### Paso 1: Crear el Job

1. Ve a **BTP Cockpit** → **DEV Subaccount**
2. Navega a **Services** → **Continuous Integration & Delivery**
3. Click en **Jobs** → **Create Job**

### Paso 2: Configurar el Job

```yaml
General:
  Job Name: ai-core-api-prod-deploy
  Repository: [Tu repositorio Git]
  Branch: main

Build:
  Build Tool: MTA Build Tool
  Build Descriptor: mta.yaml
  MTA Extension Descriptor: mta-prod.mtaext (opcional)

Deploy:
  Target: Cloud Foundry
  API Endpoint: https://api.cf.eu10.hana.ondemand.com
  Org: [Tu organización]
  Space: production
  Deploy Type: standard
```

### Paso 3: Credenciales

Necesitas configurar:

1. **Cloud Foundry Credentials**
   - Username: Tu email de BTP
   - Password: Tu contraseña de BTP

2. **Git Credentials** (si es repo privado)
   - Username: Tu usuario Git
   - Token: Personal Access Token

## 📦 Estructura de Archivos Requerida

```
aicore_api/
├── mta.yaml                 ← Descriptor MTA principal
├── mta-prod.mtaext         ← Extensión para producción
├── .mbtrc                  ← Configuración de build
├── package.json            ← Dependencias Node.js
├── server.js               ← Aplicación principal
├── manifest.yml            ← (Mantener para deploy manual)
└── [resto de archivos]
```

## 🔨 Build Local (Opcional)

Si quieres probar el build localmente:

### Instalar MBT (Cloud MTA Build Tool)

```bash
# Windows
npm install -g mbt

# Verificar instalación
mbt --version
```

### Construir MTA

```bash
# Build básico
mbt build

# Build con extensión de producción
mbt build -e mta-prod.mtaext

# Resultado: mta_archives/ai-core-api_1.0.0.mtar
```

### Desplegar manualmente

```bash
cf login -a https://api.cf.eu10.hana.ondemand.com

cf deploy mta_archives/ai-core-api_1.0.0.mtar
```

## 🎯 Configuración del mta.yaml

### Estructura Explicada

```yaml
_schema-version: '3.1'        # Versión del esquema MTA
ID: ai-core-api               # ID único de la aplicación
version: 1.0.0                # Versión de tu app

modules:                      # Módulos de la aplicación
  - name: ai_core_api         # Nombre del módulo
    type: nodejs              # Tipo de aplicación
    path: .                   # Ruta al código fuente
    parameters:
      memory: 2048M           # Memoria asignada
      buildpack: nodejs_buildpack
    properties:               # Variables de entorno
      VECTOR_STORE_TYPE: sqlite
      NODE_ENV: production
    requires:                 # Servicios requeridos
      - name: aicore-app-auth

resources:                    # Servicios externos
  - name: aicore-app-auth
    type: org.cloudfoundry.existing-service
```

## 🔄 Pipeline CI/CD Automático

El pipeline ejecutará:

1. **Fetch** - Descarga código del repositorio
2. **Install** - `npm install` (instala dependencias)
3. **Test** - `npm run test:ci` ✅ **TESTS UNITARIOS**
4. **Build** - Construye el MTA usando `mbt build`
5. **Deploy** - Despliega a Cloud Foundry

### ✅ Tests Integrados

Los tests se ejecutan automáticamente durante el build:

```yaml
build-parameters:
  commands:
    - npm install
    - npm run test:ci  # ← Tests ejecutados aquí
```

**Si los tests fallan:**
- ❌ El build se detiene
- ❌ No se despliega a producción
- 📧 Se notifica el error
- 📊 Se muestra el reporte de tests

**Tests implementados:**
- ✅ RAG Service (8 tests)
- ✅ Chat History (9 tests)
- ✅ Validaciones (12 tests)
- ✅ Health Check (4 tests)
- **Total: 33 tests**

Ver **TESTING_QUICKSTART.md** para más detalles.

## 📊 Diferencias: manifest.yml vs mta.yaml

| Característica | manifest.yml | mta.yaml |
|----------------|--------------|----------|
| Uso | Deploy manual | CI/CD automatizado |
| Complejidad | Simple | Más complejo |
| Multi-módulo | No | Sí |
| Build tool | No requiere | Requiere MBT |
| BTP CI/CD | ❌ No soportado | ✅ Soportado |

## 🚨 Errores Comunes

### Error: "Build descriptor not found"

**Solución:** Asegúrate que `mta.yaml` está en la raíz del repositorio.

### Error: "Service aicore-app-auth not found"

**Solución:** 
1. Verifica que el servicio existe en el space de producción
2. O crea el servicio antes del deploy:

```bash
cf create-service xsuaa application aicore-app-auth -c xs-security.json
```

### Error: "Module build failed"

**Solución:** Verifica que `package.json` tiene todas las dependencias.

## 🔐 Configuración de Servicios

Si el servicio `aicore-app-auth` no existe en producción:

### Crear xs-security.json

```json
{
  "xsappname": "aicore-app-auth",
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
  ],
  "role-templates": [
    {
      "name": "User",
      "scope-references": [
        "$XSAPPNAME.read",
        "$XSAPPNAME.write"
      ]
    }
  ]
}
```

### Crear el servicio

```bash
cf create-service xsuaa application aicore-app-auth -c xs-security.json
```

## 📝 Checklist Pre-Deploy

- [ ] `mta.yaml` creado y configurado
- [ ] `package.json` con todas las dependencias
- [ ] Servicio `aicore-app-auth` existe en producción
- [ ] Credenciales configuradas en BTP CI/CD
- [ ] Job creado en BTP CI/CD
- [ ] Repository conectado
- [ ] Branch configurado

## 🎉 Deploy Exitoso

Cuando el job termine exitosamente:

1. Ve a **Cloud Foundry** → **Spaces** → **production**
2. Verás tu aplicación `ai_core_api` corriendo
3. Click en la app para ver logs y estado
4. Accede a la URL de la aplicación

## 🔗 URLs Útiles

- **BTP Cockpit:** https://cockpit.eu10.hana.ondemand.com
- **Cloud Foundry API:** https://api.cf.eu10.hana.ondemand.com
- **CI/CD Service:** [Tu subaccount] → Services → CI/CD

## 💡 Consejos

1. **Mantén ambos archivos:** `manifest.yml` para deploy manual, `mta.yaml` para CI/CD
2. **Versiona el MTA:** Incrementa la versión en `mta.yaml` en cada release
3. **Usa extensiones:** Crea `.mtaext` diferentes para dev/qa/prod
4. **Logs:** Usa `cf logs ai_core_api --recent` para debug

## 🆘 Soporte

Si tienes problemas:

1. Revisa logs del job en BTP CI/CD
2. Verifica logs de la app: `cf logs ai_core_api --recent`
3. Comprueba estado: `cf app ai_core_api`
4. Revisa servicios: `cf services`

---

**¡Listo para desplegar a producción con CI/CD!** 🚀
