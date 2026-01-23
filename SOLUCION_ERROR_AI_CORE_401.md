# 🔴 Error: SAP AI Core 401 - Bad Credentials

## Error Detectado

```
Error: Could not fetch client credentials token for service of type aicore: 
HTTP response from https://dev-environment-auo8uoen.authentication.eu10.hana.ondemand.com/oauth/token 
was 401: {"error":"invalid_client","error_description":"Bad credentials"}
```

## 🎯 Causa Raíz

Las credenciales de SAP AI Core en `default-env.json` están **incorrectas o expiradas**.

## ✅ Soluciones

### Opción 1: Regenerar Credenciales desde BTP Cockpit

1. **Ir a BTP Cockpit**
   - https://cockpit.eu10.hana.ondemand.com

2. **Navegar al servicio AI Core**
   - Subaccount → Instances and Subscriptions
   - Buscar: `default_aicore`

3. **Crear nuevo Service Key**
   ```
   - Click en "default_aicore"
   - Service Keys → Create
   - Nombre: aicore-key-new
   - Click "Create"
   ```

4. **Copiar credenciales**
   - Click en el nuevo key
   - Click "View" o "Download"
   - Copiar el JSON completo

5. **Actualizar `default-env.json`**
   - Reemplazar la sección `"aicore": [...]` con las nuevas credenciales
   - Guardar el archivo

### Opción 2: Verificar Binding en Cloud Foundry

Si estás en Cloud Foundry:

```bash
# 1. Login
cf login -a https://api.cf.eu10.hana.ondemand.com

# 2. Ver servicios
cf services

# 3. Ver bindings de tu app
cf env ai_core_api

# 4. Si el servicio no está bindeado, bindearlo
cf bind-service ai_core_api default_aicore

# 5. Restage la app
cf restage ai_core_api
```

### Opción 3: Recrear Service Binding

```bash
# 1. Unbind servicio actual
cf unbind-service ai_core_api default_aicore

# 2. Bind nuevamente
cf bind-service ai_core_api default_aicore

# 3. Restage
cf restage ai_core_api

# 4. Obtener nuevas credenciales
cf env ai_core_api
```

## 🔍 Verificar Credenciales

Ejecuta el script de prueba:

```bash
node test-ai-core-quick.js
```

**Resultado esperado:**
```
✅ SAP AI Core funciona correctamente!
```

**Si falla:**
- Verifica que las credenciales en `default-env.json` sean correctas
- Verifica que el servicio `default_aicore` exista en BTP
- Verifica que tengas permisos en el subaccount

## 📝 Estructura de Credenciales Correcta

```json
{
  "VCAP_SERVICES": {
    "aicore": [
      {
        "credentials": {
          "serviceurls": {
            "AI_API_URL": "https://api.ai.prod.eu-central-1.aws.ml.hana.ondemand.com"
          },
          "clientid": "sb-XXXXX!b593090|aicore!b540",
          "clientsecret": "XXXXX$YYYYY=",
          "url": "https://dev-environment-XXXX.authentication.eu10.hana.ondemand.com"
        }
      }
    ]
  }
}
```

## 🚨 Errores Comunes

### Error: "invalid_client"
- **Causa**: `clientid` o `clientsecret` incorrectos
- **Solución**: Regenerar service key

### Error: "unauthorized"
- **Causa**: Token expirado o permisos insuficientes
- **Solución**: Verificar roles en BTP Cockpit

### Error: "service not found"
- **Causa**: Servicio AI Core no existe o no está bindeado
- **Solución**: Crear/bindear servicio

## 🔧 Cambio Realizado en el Código

**Antes** (fallback silencioso):
```javascript
} catch (aiError) {
  console.log('Usando fallback sin IA...');
  correctionsList = "No se pudieron generar correcciones...";
}
```

**Ahora** (error visible):
```javascript
} catch (aiError) {
  console.error('❌ ERROR CRÍTICO en SAP AI Core:', {
    message: aiError.message,
    status: aiError.status,
    stack: aiError.stack
  });
  throw new Error(`Error en SAP AI Core: ${aiError.message}`);
}
```

**Beneficio**: Ahora verás el error real en los logs en lugar de un PDF genérico.

## 📊 Próximos Pasos

1. ✅ **Regenerar credenciales** desde BTP Cockpit
2. ✅ **Actualizar** `default-env.json`
3. ✅ **Ejecutar** `node test-ai-core-quick.js`
4. ✅ **Probar endpoint** `/api/pdf-correction/generate-list`

---

**Fecha**: 2025-12-01  
**Estado**: 🔴 Requiere acción del usuario  
**Prioridad**: 🔴 Alta - Servicio no funcional
