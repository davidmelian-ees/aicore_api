# Node.js + XSUAA Authentication + express

Este proyecto es un ejemplo de **API Node.js protegida con XSUAA** en SAP BTP, usando `@sap/xssec` y `passport` para la autenticación JWT.

---

## Requisitos

- Node.js **v20.x** (recomendado)
- npm
- SAP BTP subaccount con **XSUAA** service provisionado
- Cloud Foundry CLI (`cf`) si quieres desplegar
- Postman o `curl` para probar la API

---
## Configuración
1. hacer "cf login -sso" y autenticar
2. Hacer bind de la app actual con la autenticación "cf bind-service nombre-mi-app my-auth"
    2.1. Importante haber configurado la autenticación previamente con el xs-security.json 
    2.2. Si no tienes la autenticación creada crearla en base a nuestro xs-security.json "cf create-service xsuaa application aicore-app-auth -c xs-security.json"
3. Cambiar nombre app en el package y en el manifest.yml

## Instalación
1. Clonar el repositorio:

```bash
git clone https://github.com/davidmelian-ees/template-api-oauth-cap-service-vanture

cd repo
npm i
touch default-env.json #con las credenciales proporcionadas
```
2. Cambiar en el package.json y en el manifest.yml el nombre de la app de autenticación 

---

## 📊 Sistema de Logging

El servicio incluye un sistema de logging que escribe tanto en consola (para Cloud Foundry) como en archivo local.

### **Endpoints de Logs:**

```bash
# Ver logs en formato JSON
GET /api/logs

# Descargar logs en formato Markdown
GET /api/logs/download

# Ver estadísticas de logs
GET /api/logs/stats

# Limpiar logs
DELETE /api/logs
```

### **Características:**
- ✅ Logs en archivo `logs/app.log`
- ✅ Rotación automática cuando supera 5MB
- ✅ Formato Markdown para descarga
- ✅ Timestamps en cada entrada
- ✅ Niveles: INFO, ERROR, WARN, DEBUG, SUCCESS

### **Uso en código:**

```javascript
import logger from './services/loggerService.js';

logger.info('MODULE-NAME', 'Mensaje informativo');
logger.error('MODULE-NAME', 'Error detectado', { error: error.message });
logger.warn('MODULE-NAME', 'Advertencia');
logger.debug('MODULE-NAME', 'Debug info', { data: someData });
logger.success('MODULE-NAME', 'Operación exitosa');
```

### **Descargar logs:**

```bash
# Descargar logs en formato Markdown
curl https://ai_core_api.cfapps.eu10-005.hana.ondemand.com/api/logs/download -o logs.md

# Ver estadísticas
curl https://ai_core_api.cfapps.eu10-005.hana.ondemand.com/api/logs/stats

# Limpiar logs
curl -X DELETE https://ai_core_api.cfapps.eu10-005.hana.ondemand.com/api/logs
```

---

