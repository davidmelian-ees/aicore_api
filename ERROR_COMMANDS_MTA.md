# ⚠️ Error: "commands property is not supported by the npm builder"

## 🔴 Error Completo

```
[2025-12-01 09:19:41] ERROR the "mta.yaml" file is not valid: 
line 20: the "commands" property is not supported by the "npm" builder
make: *** [Makefile_20251201091941.mta:26: pre_validate] Error 1
ERROR could not build the MTA project: exit status 2
```

## 🎯 Causa Raíz

El **builder `npm`** del MTA Build Tool **NO permite** usar la propiedad `commands` para personalizar el proceso de build.

### ❌ Configuración Incorrecta

```yaml
build-parameters:
  builder: npm
  commands:                    # ⚠️ ERROR: No soportado por npm builder
    - npm install --omit=dev
```

## ✅ Solución Aplicada

### Opción 1: Builder NPM Estándar (Recomendado)

```yaml
build-parameters:
  builder: npm                 # ✅ Usa comportamiento por defecto
  ignore:
    - node_modules/
    - tests/
    - coverage/
    - '*.test.js'
```

**Comportamiento automático del builder `npm`:**
1. Ejecuta `npm install` (todas las dependencias)
2. Ejecuta `npm run build` (si existe el script)

### Opción 2: Builder Custom (Avanzado)

Si necesitas comandos personalizados:

```yaml
build-parameters:
  builder: custom              # ✅ Permite commands personalizados
  commands:
    - npm install
    - npm run test:ci || true
    - npm prune --production
  build-result: .
```

## 📊 Comparación de Builders

| Builder | Auto npm install | Auto npm build | Commands personalizados | Complejidad |
|---------|-----------------|----------------|------------------------|-------------|
| `npm` | ✅ Sí | ✅ Sí | ❌ No | 🟢 Baja |
| `custom` | ❌ No | ❌ No | ✅ Sí | 🟡 Media |

## 🚀 Implementación

### Archivo: `mta.yaml`

```yaml
_schema-version: '3.1'
ID: ai-core-api
version: 1.0.0
description: "AI Core API with RAG capabilities"

parameters:
  enable-parallel-deployments: true

modules:
  - name: ai_core_api
    type: nodejs
    path: .
    parameters:
      memory: 2048M
      disk-quota: 1024M
      buildpack: nodejs_buildpack
    build-parameters:
      builder: npm              # ✅ Sin commands
      ignore:
        - node_modules/
        - .git/
        - .env
        - tests/
        - coverage/
        - '*.test.js'
        - '*.md'
        - backups/
        - logs/
        - uploads/
        - sample_documents/
        - python_tools/
        - prompts_dev/
        - prompts_proyect/
        - postman/
        - scripts/
        - venv/
        - '*.bat'
    properties:
      VECTOR_STORE_TYPE: sqlite
      NODE_ENV: production
    requires:
      - name: aicore-app-auth
    provides:
      - name: ai_core_api_api
        properties:
          url: ${default-url}

resources:
  - name: aicore-app-auth
    type: org.cloudfoundry.existing-service
    parameters:
      service-name: aicore-app-auth
```

## 📝 Notas Importantes

### ✅ Ventajas de usar builder `npm`
- Configuración simple y estándar
- Mantenimiento más fácil
- Menos propenso a errores
- Compatible con actualizaciones de MTA Build Tool

### ⚠️ Limitaciones
- No puedes ejecutar tests durante el build
- No puedes omitir devDependencies automáticamente
- No puedes ejecutar scripts personalizados pre/post build

### 💡 Recomendación
Para la mayoría de proyectos Node.js, el builder `npm` estándar es suficiente. Solo usa `custom` si tienes requisitos muy específicos.

## 🔗 Referencias

- [MTA Build Tool Documentation](https://sap.github.io/cloud-mta-build-tool/)
- [Supported Builders](https://sap.github.io/cloud-mta-build-tool/configuration/#configuring-the-build-parameters)
- [NPM Builder Behavior](https://sap.github.io/cloud-mta-build-tool/configuration/#npm-builder)

---

**Fecha**: 2025-12-01  
**Estado**: ✅ Resuelto  
**Solución**: Eliminada propiedad `commands` del builder `npm`
