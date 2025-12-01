# 🎯 Solución: Problema de Contexto y SAP AI Core

## 🔴 Problemas Identificados en los Logs

### 1. SAP AI Core Fallando
```
[PDF-CORRECTION] ❌ ERROR en SAP AI Core: {
  message: 'Failed to fetch the list of deployments.',
}
[PDF-CORRECTION] Usando fallback sin IA...
```

**Causa**: Las credenciales de SAP AI Core están incorrectas o el servicio no está disponible.

**Resultado**: Se usa el fallback que genera el mensaje genérico:
```
No se pudieron generar correcciones automáticas.
Revise el documento manualmente para errores ortográficos.
```

### 2. Contexto Incorrecto
```
[RAG] ❌ Chunk descartado - contextId: 09a6c72e-9c6f-4448-8ac7-d22d1c13e90b (esperado: default)
[RAG] ✅ Después de filtrar por contexto: 0 chunks
```

**Causa**: Todos tus documentos están en el contexto `09a6c72e-9c6f-4448-8ac7-d22d1c13e90b` pero el sistema busca en `default`.

**Resultado**: No encuentra documentos de referencia para comparar.

## ✅ Soluciones Implementadas

### 1. Auto-selección de Contexto

He modificado el endpoint para que **automáticamente use el primer contexto disponible** si no se especifica:

```javascript
// Si no se especifica contextId o viene vacío, usar el primer contexto disponible
let contextId = req.body.contextId;
if (!contextId || contextId.trim() === '' || contextId === 'default') {
  const { getFirstAvailableContext } = await import('../services/ragService.js');
  const firstContext = await getFirstAvailableContext();
  contextId = firstContext || '09a6c72e-9c6f-4448-8ac7-d22d1c13e90b';
  console.log(`[PDF-CORRECTION] ℹ️ No se especificó contextId, usando: ${contextId}`);
}
```

**Comportamiento**:
- Si envías `contextId`: usa ese contexto
- Si envías `contextId: ""` o `contextId: "default"`: usa el primer contexto disponible
- Si NO envías `contextId`: usa el primer contexto disponible

### 2. Nueva Función en RAG Service

```javascript
export async function getFirstAvailableContext() {
  // Obtiene todos los contextos y devuelve el que tenga más documentos
  const allContexts = await listContexts();
  const sortedContexts = allContexts.sort((a, b) => 
    (b.documentCount || 0) - (a.documentCount || 0)
  );
  return sortedContexts[0].id;
}
```

## 🚀 Cómo Usar

### Opción 1: Sin especificar contextId (RECOMENDADO)
```json
POST /api/pdf-correction/generate-list
{
  "pdf": [archivo],
  "pliegoId": "PLIEGO_123"
}
```
✅ Usará automáticamente: `09a6c72e-9c6f-4448-8ac7-d22d1c13e90b`

### Opción 2: Con contextId explícito
```json
POST /api/pdf-correction/generate-list
{
  "pdf": [archivo],
  "contextId": "09a6c72e-9c6f-4448-8ac7-d22d1c13e90b",
  "pliegoId": "PLIEGO_123"
}
```
✅ Usará el contexto especificado

### Opción 3: Con "default"
```json
POST /api/pdf-correction/generate-list
{
  "pdf": [archivo],
  "contextId": "default",
  "pliegoId": "PLIEGO_123"
}
```
✅ Usará automáticamente el primer contexto disponible

## 🔧 Solución para SAP AI Core

El error `Failed to fetch the list of deployments` indica que SAP AI Core no puede autenticarse.

### Verificar Credenciales

1. **Revisar `default-env.json`**:
   ```json
   {
     "VCAP_SERVICES": {
       "aicore": [{
         "credentials": {
           "clientid": "sb-XXXXX",
           "clientsecret": "XXXXX",
           "url": "https://..."
         }
       }]
     }
   }
   ```

2. **Regenerar Service Key** en BTP Cockpit:
   - Ir a: Instances and Subscriptions → `default_aicore`
   - Service Keys → Create new key
   - Copiar credenciales a `default-env.json`

3. **Verificar conexión**:
   ```bash
   node test-ai-core-quick.js
   ```

## 📊 Logs Esperados Después del Fix

### Con contexto automático:
```
[PDF-CORRECTION] ℹ️ No se especificó contextId, usando: 09a6c72e-9c6f-4448-8ac7-d22d1c13e90b
[RAG] 📌 Primer contexto disponible: 09a6c72e-9c6f-4448-8ac7-d22d1c13e90b (13 documentos)
[RAG] Búsqueda completada: 15 chunks encontrados
[PDF-CORRECTION] ✅ Respuesta recibida de SAP AI Core en 3245ms
[PDF-CORRECTION] - correctionsList length: 2500 caracteres
[PDF-CORRECTION] ✅ Almacenados: 5 errores críticos, 3 advertencias
```

## 🎯 Resumen

### Antes:
- ❌ Buscaba en contexto `default` (vacío)
- ❌ No encontraba documentos de referencia
- ❌ SAP AI Core fallaba silenciosamente
- ❌ Generaba PDF genérico sin errores

### Ahora:
- ✅ Usa automáticamente el contexto con más documentos
- ✅ Encuentra documentos de referencia
- ✅ Logs detallados de SAP AI Core
- ✅ Si SAP AI Core falla, el error es visible

## 📝 Próximos Pasos

1. **Reiniciar servidor**:
   ```bash
   npm start
   ```

2. **Probar endpoint SIN contextId**:
   - Enviar request desde Postman
   - Verificar en logs: `ℹ️ No se especificó contextId, usando: 09a6c72e-9c6f-4448-8ac7-d22d1c13e90b`

3. **Si SAP AI Core sigue fallando**:
   - Regenerar credenciales en BTP Cockpit
   - Actualizar `default-env.json`
   - Ejecutar `node test-ai-core-quick.js`

---

**Fecha**: 2025-12-01  
**Estado**: ✅ Implementado  
**Archivos modificados**:
- `routes/pdfCorrection.js` - Auto-selección de contexto
- `services/ragService.js` - Nueva función `getFirstAvailableContext()`
