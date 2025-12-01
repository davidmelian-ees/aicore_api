# 🔍 Guía de Logs para Debugging

## 📋 Logs Agregados

He agregado logs detallados en todo el flujo de corrección de PDF. Ahora cuando ejecutes el endpoint verás:

### 1. Logs de Entrada (Endpoint)
```
[PDF-CORRECTION] 📄 Procesando PDF: nombre.pdf (12345 bytes)
[PDF-CORRECTION] 🔍 Ejecutando análisis visual del documento...
[PDF-CORRECTION] ✅ Análisis visual completado
[PDF-CORRECTION] 🤖 Ejecutando análisis de IA...
```

### 2. Logs de SAP AI Core (Service)
```
[PDF-CORRECTION] ========================================
[PDF-CORRECTION] 🤖 LLAMANDO A SAP AI CORE
[PDF-CORRECTION] Prompt length: 15234 caracteres
[PDF-CORRECTION] Text length: 8456 caracteres
[PDF-CORRECTION] Context ID: 09a6c72e-9c6f-4448-8ac7-d22d1c13e90b
[PDF-CORRECTION] RAG Context: SÍ
[PDF-CORRECTION] Visual Errors: NO
[PDF-CORRECTION] ========================================
[PDF-CORRECTION] ⏳ Enviando request a SAP AI Core...
[PDF-CORRECTION] ✅ Respuesta recibida de SAP AI Core en 3245ms
[PDF-CORRECTION] 📝 Contenido de respuesta:
[PDF-CORRECTION] - Length: 1234 caracteres
[PDF-CORRECTION] - Primeros 500 caracteres:
[AQUÍ VERÁS LA RESPUESTA DE LA IA]
[PDF-CORRECTION] ========================================
[PDF-CORRECTION] ✅ Correcciones procesadas: 1234 caracteres
```

### 3. Logs de Resultado (Endpoint)
```
[PDF-CORRECTION] 📊 Resultado del análisis:
[PDF-CORRECTION] - Success: true
[PDF-CORRECTION] - correctionsList length: 1234
[PDF-CORRECTION] - correctionsList preview: [PRIMEROS 200 CARACTERES]
[PDF-CORRECTION] 💾 Almacenando errores para pliego: PLIEGO_1764584225342...
[PDF-CORRECTION] 📝 correctionsList a almacenar: [PRIMEROS 300 CARACTERES]
[PDF-CORRECTION] ✅ Almacenados: 2 errores críticos, 1 advertencias
```

## 🎯 Qué Buscar en los Logs

### Caso 1: SAP AI Core NO encuentra errores

Si ves esto:
```
[PDF-CORRECTION] 📝 Contenido de respuesta:
[PDF-CORRECTION] - Length: 150 caracteres
[PDF-CORRECTION] - Primeros 500 caracteres:
No se encontraron errores en el documento.
```

**Significa**: La IA analizó el documento y NO encontró errores.

**Posibles causas**:
- El documento realmente no tiene errores
- El prompt no está detectando el tipo de errores que buscas
- El contexto RAG no tiene suficiente información

### Caso 2: SAP AI Core falla

Si ves esto:
```
[PDF-CORRECTION] ❌ ERROR en SAP AI Core:
message: Failed to fetch...
```

**Significa**: Hay un problema de conexión o credenciales.

### Caso 3: Respuesta vacía

Si ves esto:
```
[PDF-CORRECTION] - Length: 0 caracteres
[PDF-CORRECTION] - Primeros 500 caracteres:
[VACÍO]
```

**Significa**: SAP AI Core devolvió una respuesta vacía (error).

## 🚀 Cómo Ejecutar y Ver Logs

### Opción 1: Desde Postman + Terminal

1. **Abre una terminal** en el proyecto:
   ```bash
   cd c:\Users\JesusDavidMelianHern\CascadeProjects\aicore_api\aicore_api
   npm start
   ```

2. **Envía request desde Postman** al endpoint:
   ```
   POST http://localhost:4000/api/pdf-correction/generate-list
   ```

3. **Mira los logs en la terminal** - Verás todos los logs detallados

### Opción 2: Logs en Archivo

Si quieres guardar los logs en un archivo:

```bash
npm start > logs-debug.txt 2>&1
```

Luego envía el request desde Postman y revisa `logs-debug.txt`.

## 📊 Ejemplo de Logs Completos

```
[PDF-CORRECTION] 📄 Procesando PDF: PCAP_CT1074723_GIS.pdf (1234567 bytes)
[PDF-CORRECTION] ⏭️ Análisis visual omitido (skipVisualAnalysis=true)
[PDF-CORRECTION] 🤖 Ejecutando análisis de IA...
[PDF-CORRECTION] ========================================
[PDF-CORRECTION] 🤖 LLAMANDO A SAP AI CORE
[PDF-CORRECTION] Prompt length: 25678 caracteres
[PDF-CORRECTION] Text length: 12345 caracteres
[PDF-CORRECTION] Context ID: 09a6c72e-9c6f-4448-8ac7-d22d1c13e90b
[PDF-CORRECTION] RAG Context: SÍ
[PDF-CORRECTION] Visual Errors: NO
[PDF-CORRECTION] ========================================
[PDF-CORRECTION] ⏳ Enviando request a SAP AI Core...
[PDF-CORRECTION] ✅ Respuesta recibida de SAP AI Core en 4532ms
[PDF-CORRECTION] 📝 Contenido de respuesta:
[PDF-CORRECTION] - Length: 89 caracteres
[PDF-CORRECTION] - Primeros 500 caracteres:
No se detectaron errores ortográficos, gramaticales ni de formato en el documento.
[PDF-CORRECTION] ========================================
[PDF-CORRECTION] ✅ Correcciones procesadas: 89 caracteres
[PDF-CORRECTION] 📊 Resultado del análisis:
[PDF-CORRECTION] - Success: true
[PDF-CORRECTION] - correctionsList length: 89
[PDF-CORRECTION] - correctionsList preview: No se detectaron errores ortográficos, gramaticales ni de formato en el documento.
[PDF-CORRECTION] 💾 Almacenando errores para pliego: PLIEGO_1764584225342...
[PDF-CORRECTION] 📝 correctionsList a almacenar: No se detectaron errores ortográficos, gramaticales ni de formato en el documento.
[PDF-CORRECTION] ✅ Almacenados: 0 errores críticos, 0 advertencias
```

## 🔍 Interpretación

En el ejemplo anterior:
- ✅ SAP AI Core funcionó correctamente (respuesta en 4.5 segundos)
- ✅ Devolvió una respuesta (89 caracteres)
- ⚠️ **NO encontró errores** en el documento
- ✅ Se almacenaron 0 errores en la BD

**Esto explica por qué el PDF dice "No se pudieron generar correcciones"** - La IA analizó el documento y no encontró nada.

## 💡 Próximos Pasos

1. **Ejecuta el endpoint** con tu PDF
2. **Copia TODOS los logs** que aparezcan en la terminal
3. **Envíamelos** para que pueda ver exactamente qué está pasando
4. Especialmente necesito ver:
   - Los "Primeros 500 caracteres" de la respuesta de la IA
   - El "correctionsList length"
   - Los errores almacenados (criticalErrors y warnings)

---

**Fecha**: 2025-12-01  
**Versión**: Debug v1.0
