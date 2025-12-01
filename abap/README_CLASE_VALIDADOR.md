# 📋 Clase ZCL_PLIEGO_VALIDATOR

## 🎯 Propósito

Clase ABAP para validar pliegos SAP mediante la API de AI Core. Permite enviar PDFs (desde archivo o base64) y recibir el PDF de validación con el informe de errores.

---

## 📦 Archivos Creados

| Archivo | Descripción |
|---------|-------------|
| `ZCL_PLIEGO_VALIDATOR.abap` | Clase principal con métodos de validación |
| `ZTEST_PLIEGO_VALIDATOR.abap` | Programa de prueba |
| `EJEMPLO_USO_CLASE.abap` | Ejemplos de uso |
| `README_CLASE_VALIDADOR.md` | Esta documentación |

---

## 🔧 Configuración Previa

### 1. Tabla de Configuración: `ZPA_RPAIA_UTILS`

Debe existir un registro con:
```abap
name_service = 'plecs_rev'
url = 'https://ai-core-api.cfapps.eu10-005.hana.ondemand.com'
endpoint = '/api/pdf-correction/generate-list'
```

### 2. Autenticación: `ZCL_RPAIA_AUTH`

Debe estar configurado el identificador `'AICOREAUTH'` para obtener el Bearer Token.

---

## 📚 Métodos Disponibles

### 0. `CHECK_CONNECTION` (Health Check)

Verifica la conexión con el servidor de AI Core.

**Parámetros de Salida:**
- `ev_http_status` (i) - Código HTTP (200 = OK)
- `ev_response` (string) - Respuesta JSON del servidor
- `ev_error_message` (string) - Mensaje descriptivo
- `ev_is_online` (abap_bool) - X = Online, '' = Offline

**Ejemplo:**
```abap
DATA: lv_is_online TYPE abap_bool,
      lv_http_status TYPE i,
      lv_error TYPE string.

CALL METHOD zcl_pliego_validator=>check_connection
  IMPORTING
    ev_is_online = lv_is_online
    ev_http_status = lv_http_status
    ev_error_message = lv_error.

IF lv_is_online = abap_true.
  MESSAGE 'Servidor disponible' TYPE 'S'.
ELSE.
  MESSAGE lv_error TYPE 'E'.
ENDIF.
```

**Cuándo usar:**
- Antes de validar pliegos (verificar disponibilidad)
- En jobs programados (health check periódico)
- En pantallas de configuración (test de conexión)
- Para diagnóstico de problemas

---

### 1. `VALIDATE_PLIEGO_FROM_FILE`

Valida un pliego desde un archivo en el frontend (PC del usuario).

**Parámetros de Entrada:**
- `iv_pdf_path` (string) - Ruta completa del archivo PDF
- `iv_pliego_id` (string, opcional) - ID del pliego
- `iv_context_id` (string, opcional) - ID del contexto RAG
- `iv_username` (string, opcional) - Usuario que valida

**Parámetros de Salida:**
- `ev_pdf_xstring` (xstring) - PDF validado en binario
- `ev_pdf_base64` (string) - PDF validado en base64
- `ev_http_status` (i) - Código HTTP (200 = OK)
- `ev_error_message` (string) - Mensaje de error si falla

**Ejemplo:**
```abap
DATA: lv_pdf_xstring TYPE xstring,
      lv_http_status TYPE i.

CALL METHOD zcl_pliego_validator=>validate_pliego_from_file
  EXPORTING
    iv_pdf_path = 'C:\temp\pliego.pdf'
    iv_pliego_id = 'PCAP_CT1074723'
  IMPORTING
    ev_pdf_xstring = lv_pdf_xstring
    ev_http_status = lv_http_status.
```

---

### 2. `VALIDATE_PLIEGO_FROM_BASE64`

Valida un pliego desde un string base64 (backend).

**Parámetros de Entrada:**
- `iv_pdf_base64` (string) - PDF codificado en base64
- `iv_filename` (string, opcional) - Nombre del archivo
- `iv_pliego_id` (string, opcional) - ID del pliego
- `iv_context_id` (string, opcional) - ID del contexto RAG
- `iv_username` (string, opcional) - Usuario que valida

**Parámetros de Salida:**
- `ev_pdf_xstring` (xstring) - PDF validado en binario
- `ev_pdf_base64` (string) - PDF validado en base64
- `ev_http_status` (i) - Código HTTP (200 = OK)
- `ev_error_message` (string) - Mensaje de error si falla

**Ejemplo:**
```abap
DATA: lv_pdf_base64_in TYPE string,
      lv_pdf_xstring_out TYPE xstring,
      lv_http_status TYPE i.

lv_pdf_base64_in = 'JVBERi0xLjQK...'. " Tu PDF en base64

CALL METHOD zcl_pliego_validator=>validate_pliego_from_base64
  EXPORTING
    iv_pdf_base64 = lv_pdf_base64_in
    iv_filename = 'pliego.pdf'
  IMPORTING
    ev_pdf_xstring = lv_pdf_xstring_out
    ev_http_status = lv_http_status.
```

---

## ✨ Mejores Prácticas

### 1. Verificar Conexión Antes de Validar

```abap
" ✅ BUENA PRÁCTICA
DATA: lv_is_online TYPE abap_bool.

" Primero verificar disponibilidad
CALL METHOD zcl_pliego_validator=>check_connection
  IMPORTING ev_is_online = lv_is_online.

IF lv_is_online = abap_true.
  " Servidor disponible - proceder
  CALL METHOD zcl_pliego_validator=>validate_pliego_from_base64
    " ... parámetros ...
ELSE.
  MESSAGE 'Servidor no disponible' TYPE 'E'.
ENDIF.
```

### 2. Manejo Robusto de Errores

```abap
" ✅ BUENA PRÁCTICA
DATA: lv_http_status TYPE i,
      lv_error TYPE string.

CALL METHOD zcl_pliego_validator=>validate_pliego_from_base64
  " ... parámetros ...
  IMPORTING
    ev_http_status = lv_http_status
    ev_error_message = lv_error.

CASE lv_http_status.
  WHEN 200.
    " Éxito
  WHEN 401.
    " Regenerar token
  WHEN 500.
    " Reintentar más tarde
  WHEN OTHERS.
    " Log del error
ENDCASE.
```

### 3. Usar XSTRING para Archivos

```abap
" ✅ BUENA PRÁCTICA - Guardar archivos
DATA: lv_pdf_xstring TYPE xstring.

CALL METHOD zcl_pliego_validator=>validate_pliego_from_base64
  IMPORTING ev_pdf_xstring = lv_pdf_xstring.

" Guardar con xstring (binario)
OPEN DATASET '/tmp/file.pdf' FOR OUTPUT IN BINARY MODE.
TRANSFER lv_pdf_xstring TO '/tmp/file.pdf'.

" ❌ MALA PRÁCTICA - NO usar base64 para guardar
" ev_pdf_base64 es solo para transmisión/APIs
```

---

## 🚀 Casos de Uso

### Caso 1: Validar desde Transacción SAP

```abap
REPORT z_validar_pliego.

PARAMETERS: p_file TYPE rlgrap-filename.

START-OF-SELECTION.
  DATA: lv_pdf_xstring TYPE xstring,
        lv_http_status TYPE i,
        lv_error TYPE string.

  CALL METHOD zcl_pliego_validator=>validate_pliego_from_file
    EXPORTING
      iv_pdf_path = p_file
      iv_pliego_id = 'PCAP_TEST'
      iv_username = sy-uname
    IMPORTING
      ev_pdf_xstring = lv_pdf_xstring
      ev_http_status = lv_http_status
      ev_error_message = lv_error.

  IF lv_http_status = 200.
    MESSAGE 'Validación OK' TYPE 'S'.
  ELSE.
    MESSAGE lv_error TYPE 'E'.
  ENDIF.
```

### Caso 2: Integrar en RFC/BAPI

```abap
FUNCTION z_validate_pliego.
  IMPORTING
    iv_pdf_base64 TYPE string
    iv_pliego_id TYPE string
  EXPORTING
    ev_pdf_validado TYPE xstring
    ev_status TYPE i
    ev_message TYPE string.

  CALL METHOD zcl_pliego_validator=>validate_pliego_from_base64
    EXPORTING
      iv_pdf_base64 = iv_pdf_base64
      iv_pliego_id = iv_pliego_id
      iv_username = sy-uname
    IMPORTING
      ev_pdf_xstring = ev_pdf_validado
      ev_http_status = ev_status
      ev_error_message = ev_message.

ENDFUNCTION.
```

### Caso 3: Validar y Guardar en Servidor

```abap
METHOD validar_y_guardar.
  DATA: lv_pdf_xstring TYPE xstring,
        lv_http_status TYPE i,
        lv_filepath TYPE string.

  " Validar
  CALL METHOD zcl_pliego_validator=>validate_pliego_from_base64
    EXPORTING
      iv_pdf_base64 = im_pdf_base64
    IMPORTING
      ev_pdf_xstring = lv_pdf_xstring
      ev_http_status = lv_http_status.

  IF lv_http_status = 200.
    " Guardar en application server
    lv_filepath = '/tmp/pliego_validado.pdf'.
    OPEN DATASET lv_filepath FOR OUTPUT IN BINARY MODE.
    TRANSFER lv_pdf_xstring TO lv_filepath.
    CLOSE DATASET lv_filepath.
  ENDIF.
ENDMETHOD.
```

---

## 📊 Códigos HTTP

| Código | Significado | Acción |
|--------|-------------|--------|
| 200 | OK | Validación exitosa |
| 400 | Bad Request | Verificar datos enviados |
| 401 | Unauthorized | Token inválido, regenerar |
| 404 | Not Found | Verificar configuración tabla |
| 500 | Server Error | Error del servidor, reintentar |
| 502 | Bad Gateway | Problema de red |
| 503 | Service Unavailable | Servicio no disponible |

---

## 🔍 Diferencias con el Programa ZVALIDATE_PLIEGO_PDF

| Característica | Programa | Clase |
|----------------|----------|-------|
| **Uso** | Interactivo (transacción) | Programático (código) |
| **Entrada** | Solo archivo | Archivo o base64 |
| **Salida** | Guarda en PC | Devuelve xstring/base64 |
| **Integración** | No reutilizable | Reutilizable en cualquier código |
| **UI** | Con pantalla de selección | Sin UI |
| **Logs** | Muestra en pantalla | Solo devuelve status |

---

## ⚠️ Solución al Problema del PDF Corrupto

### Problema Original
El programa `ZVALIDATE_PLIEGO_PDF` guardaba PDFs de 1KB corruptos porque:
1. Usaba `get_cdata()` en lugar de `get_data()`
2. `get_cdata()` devuelve texto, no binario
3. Los PDFs son archivos binarios

### Solución en la Clase
```abap
" ❌ INCORRECTO (programa antiguo)
lv_response = lo_http_client->response->get_cdata( ).

" ✅ CORRECTO (clase nueva)
lv_response_xstring = lo_http_client->response->get_data( ).
```

La clase usa `get_data()` que devuelve `xstring` (binario), preservando el PDF correctamente.

---

## 🧪 Probar la Clase

### 1. Crear la Clase
```
Transaction: SE24
Nombre: ZCL_PLIEGO_VALIDATOR
Copiar código de ZCL_PLIEGO_VALIDATOR.abap
Activar
```

### 2. Ejecutar Programa de Prueba
```
Transaction: SE38
Nombre: ZTEST_PLIEGO_VALIDATOR
Copiar código de ZTEST_PLIEGO_VALIDATOR.abap
Ejecutar (F8)
Seleccionar PDF
Ver resultado
```

### 3. Verificar PDF Descargado
- El PDF debe tener tamaño > 1KB
- Debe abrirse correctamente
- Debe contener el informe de validación

---

## 📝 Notas Importantes

1. **Formato de Respuesta**: La API devuelve el PDF en binario (application/pdf), no en base64.

2. **Tamaño del PDF**: Si el PDF original es muy grande (>5MB), puede haber timeout. Ajustar en BASIS si es necesario.

3. **Contexto RAG**: El parámetro `iv_context_id` es opcional. Si se proporciona, la IA usará documentos de ese contexto para mejorar la validación.

4. **Username**: Se registra en los logs del servidor para trazabilidad.

5. **Pliego ID**: Si no se proporciona, el servidor genera uno automático con timestamp.

---

## 🆘 Troubleshooting

### Error: "Configuración de servicio plecs_rev no encontrada"
**Solución**: Verificar que existe el registro en tabla `ZPA_RPAIA_UTILS` con `name_service = 'plecs_rev'`.

### Error: "Error obteniendo token de autenticación"
**Solución**: Verificar configuración de `ZCL_RPAIA_AUTH` con identificador `'AICOREAUTH'`.

### Error: "Connection closed"
**Solución**: El PDF es muy grande o hay timeout. Reducir tamaño del PDF o contactar BASIS.

### PDF corrupto (1KB)
**Solución**: Asegurarse de usar `ev_pdf_xstring` (no `ev_pdf_base64`) para guardar archivos.

### Error 401: Unauthorized
**Solución**: El token expiró. Regenerar token en la configuración de autenticación.

---

## 📞 Soporte

Para problemas o dudas:
1. Verificar configuración (tabla + autenticación)
2. Ejecutar programa de prueba `ZTEST_PLIEGO_VALIDATOR`
3. Revisar logs del servidor en `/api/logs`
4. Contactar al administrador BASIS si persiste

---

**¡Clase lista para usar!** 🎉
