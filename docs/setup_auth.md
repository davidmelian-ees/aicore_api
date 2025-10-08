# Configuración de Autenticación y Servicios en SAP BTP

Este documento describe el proceso completo para configurar los servicios necesarios y la autenticación en SAP Business Technology Platform (BTP) para la aplicación AI Core API.

## Tabla de Contenidos
1. [Prerequisitos](#prerequisitos)
2. [Servicios Requeridos](#servicios-requeridos)
3. [Binding de Servicios](#binding-de-servicios)
4. [Configuración de Variables de Entorno](#configuración-de-variables-de-entorno)
5. [Verificación](#verificación)

---

## Prerequisitos

Antes de comenzar, asegúrate de tener:
- Acceso a SAP BTP Cockpit
- Una aplicación desplegada en Cloud Foundry
- Permisos para crear y bindear servicios

---

## Servicios Requeridos

Para que la aplicación funcione correctamente, necesitas tener creados los siguientes servicios:

### 1. **SAP AI Core** (`default_aicore`)
- **Service**: SAP AI Core
- **Plan**: extended
- **Instance Name**: `default_aicore`
- **Runtime Environment**: Cloud Foundry
- **Scope**: dev-app1

**Características:**
- Proporciona acceso a modelos de IA y capacidades de machine learning
- Incluye credenciales para autenticación OAuth2
- URL de API: `https://api.ai.prod.eu-central-1.aws.ml.hana.ondemand.com`

### 2. **Authorization and Trust Management** (`aicore-app-auth`)
- **Service**: Authorization and Trust Management Service (XSUAA)
- **Plan**: application
- **Instance Name**: `aicore-app-auth`
- **Runtime Environment**: Cloud Foundry
- **Scope**: dev-app1

**Características:**
- Gestiona la autenticación y autorización de usuarios
- Proporciona tokens JWT para acceso seguro
- Integración con Passport.js para Node.js

---

## Binding de Servicios

Los servicios deben estar **bindeados** (vinculados) a tu aplicación para que puedan ser utilizados. Este es un paso crítico.

### Paso 1: Verificar las Instancias de Servicio

En el SAP BTP Cockpit, navega a tu subaccount y verifica que tienes las siguientes instancias creadas:

| Instance | Service | Plan | Credentials |
|----------|---------|------|-------------|
| `aicore-app-auth` | Authorization and Trust Management | application | 3 bindings |
| `default_aicore` | SAP AI Core | extended | 2 binding, 2 keys |

### Paso 2: Bindear `default_aicore` a tu Aplicación

1. Accede a la instancia `default_aicore` en el Cockpit
2. Ve a la pestaña **"Bound Applications"**
3. Haz clic en **"Create"** para crear un nuevo binding
4. Selecciona tu aplicación (ejemplo: `ai_core_api`)
5. Confirma la creación del binding
6. Verifica que el status sea **"Created"** (verde)

**Resultado esperado:**
- La aplicación `ai_core_api` debe aparecer en la lista de "Bound Applications (1)"
- Status: Created ✓

### Paso 3: Bindear `aicore-app-auth` a tu Aplicación

1. Accede a la instancia `aicore-app-auth` en el Cockpit
2. Ve a la pestaña **"Bound Applications"**
3. Haz clic en **"Create"** para crear un nuevo binding
4. Selecciona tu aplicación (ejemplo: `ai_core_api`)
5. Confirma la creación del binding
6. Verifica que el status sea **"Created"** (verde)

**Resultado esperado:**
- La aplicación `ai_core_api` debe aparecer en la lista de "Bound Applications"
- Status: Created ✓

### Paso 4: Verificar los Bindings

En la vista principal de instancias, deberías ver:

```
Instances (4)
┌─────────────────┬──────────────────────────┬─────────────┬─────────────────┬────────────┐
│ Instance        │ Service                  │ Plan        │ Runtime Env     │ Credentials│
├─────────────────┼──────────────────────────┼─────────────┼─────────────────┼────────────┤
│ aicore-app-auth │ Authorization and Trust  │ application │ Cloud Foundry   │ 3 bindings │
│ default_aicore  │ SAP AI Core              │ extended    │ Cloud Foundry   │ 2 binding  │
└─────────────────┴──────────────────────────┴─────────────┴─────────────────┴────────────┘
```

---

## Configuración de Variables de Entorno

Una vez que los servicios están bindeados, necesitas obtener las credenciales y configurarlas localmente.

### Paso 1: Acceder a las Variables de Entorno de la Aplicación

1. En el SAP BTP Cockpit, navega a tu aplicación `ai_core_api`
2. En el menú lateral izquierdo, ve a **Security** → **User-Provided Variables**
3. Haz clic en **"Environment Variables"**
4. Selecciona tu aplicación en el dropdown superior (ejemplo: `ai_core_api`)

### Paso 2: Copiar las Variables del Sistema

En la sección **"System-Provided"**, encontrarás un JSON con todas las credenciales de los servicios bindeados. Este JSON contiene:

```json
{
  "VCAP_SERVICES": {
    "aicore": [
      {
        "binding_guid": "...",
        "binding_name": null,
        "credentials": {
          "appname": "...",
          "clientid": "...",
          "clientsecret": "...",
          "serviceurls": {
            "AI_API_URL": "https://api.ai.prod.eu-central-1.aws.ml.hana.ondemand.com"
          },
          "url": "https://dev-environment-xxx.authentication.eu10.hana.ondemand.com"
        },
        "instance_name": "default_aicore",
        "label": "aicore",
        ...
      }
    ],
    "xsuaa": [
      {
        "binding_guid": "...",
        "credentials": {
          "clientid": "...",
          "clientsecret": "...",
          "url": "https://dev-environment-xxx.authentication.eu10.hana.ondemand.com",
          "xsappname": "aicore-app-auth!t593090",
          ...
        },
        "instance_name": "aicore-app-auth",
        "label": "xsuaa",
        ...
      }
    ]
  }
}
```

### Paso 3: Crear el Archivo `default-env.json`

1. En la raíz de tu proyecto, crea un archivo llamado **`default-env.json`**
2. Copia **TODO** el contenido de la sección "System-Provided"
3. Pega el contenido en el archivo `default-env.json`

**Ubicación del archivo:**
```
aicore_api/
├── default-env.json    ← Crear este archivo aquí
├── package.json
├── server.js
├── auth.js
└── ...
```

**⚠️ IMPORTANTE:**
- Este archivo contiene **credenciales sensibles** (clientsecret, tokens, etc.)
- **NUNCA** subas este archivo a Git
- Asegúrate de que `default-env.json` está en tu `.gitignore`

### Paso 4: Verificar el `.gitignore`

Asegúrate de que tu archivo `.gitignore` incluye:

```gitignore
# Environment variables
default-env.json
*.env
.env

# Credentials
*credentials*.json
```

---

## Verificación

### 1. Verificar que el archivo se carga correctamente

En tu código `auth.js`, la línea `xsenv.loadEnv()` carga automáticamente el archivo `default-env.json`:

```javascript
import xsenv from "@sap/xsenv";

xsenv.loadEnv(); // Carga default-env.json

// Obtener credenciales
const services = xsenv.getServices({ 
  xsuaa: { name: "aicore-app-auth" } 
});
```

### 2. Probar la conexión

Ejecuta el archivo de prueba:

```bash
node test.js
```

**Salida esperada:**
```
Credenciales cargadas: sb-aicore-app-auth!t593090 aicore-app-auth!t593090 [secret]
```

### 3. Verificar el cliente de AI Core

```javascript
import { AzureOpenAiChatClient } from "@sap-ai-sdk/foundation-models";

const client = new AzureOpenAiChatClient('gpt-4o', 'genai-dest');
const response = await client.run({
  messages: [{ role: 'user', content: 'Hello!' }]
});
console.log(response.getContent());
```

---

## Estructura de Credenciales

### Credenciales de AI Core (`aicore`)

```json
{
  "clientid": "sb-5db76073-2c42-4bd7-9ee0-ed3ac373b2b2!b593090|aicore!b540",
  "clientsecret": "d4377ac6-f44e-42db-8616-7d26619e3b11$...",
  "url": "https://dev-environment-auo8uoen.authentication.eu10.hana.ondemand.com",
  "serviceurls": {
    "AI_API_URL": "https://api.ai.prod.eu-central-1.aws.ml.hana.ondemand.com"
  }
}
```

### Credenciales de XSUAA (`xsuaa`)

```json
{
  "clientid": "sb-aicore-app-auth!t593090",
  "clientsecret": "cbfadb77-9961-4d16-8cfe-73f9a2608025$...",
  "url": "https://dev-environment-auo8uoen.authentication.eu10.hana.ondemand.com",
  "xsappname": "aicore-app-auth!t593090",
  "identityzone": "dev-environment-auo8uoen"
}
```

---

## Troubleshooting

### Error: "Service not found"
- Verifica que los servicios estén bindeados correctamente
- Comprueba que los nombres en el código coincidan con los nombres de instancia

### Error: "Invalid credentials"
- Asegúrate de haber copiado TODO el contenido de "System-Provided"
- Verifica que no haya caracteres extra o faltantes en el JSON

### Error: "Cannot read default-env.json"
- Verifica que el archivo esté en la raíz del proyecto
- Comprueba que el JSON sea válido (usa un validador JSON online)

### Los bindings no aparecen
- Espera unos minutos después de crear el binding
- Refresca la página del Cockpit
- Verifica que la aplicación esté en estado "Started"

---

## Resumen del Flujo

```
1. Crear servicios en SAP BTP
   ├── default_aicore (SAP AI Core)
   └── aicore-app-auth (XSUAA)

2. Bindear servicios a la aplicación
   ├── Bind default_aicore → ai_core_api
   └── Bind aicore-app-auth → ai_core_api

3. Obtener credenciales
   └── Copiar JSON de "System-Provided"

4. Crear default-env.json
   └── Pegar credenciales en el archivo

5. Verificar configuración
   ├── Ejecutar test.js
   └── Probar endpoints de la API
```

---

## Referencias

- [SAP AI Core Documentation](https://help.sap.com/docs/ai-core)
- [XSUAA Documentation](https://help.sap.com/docs/btp/sap-business-technology-platform/authorization-and-trust-management)
- [Cloud Foundry Service Bindings](https://docs.cloudfoundry.org/devguide/services/application-binding.html)
- [@sap/xsenv Package](https://www.npmjs.com/package/@sap/xsenv)

---

**Última actualización**: 8 de octubre de 2025
