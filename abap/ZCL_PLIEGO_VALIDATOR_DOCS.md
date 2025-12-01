# 📘 ZCL_RPAIA - Documentación Rápida

## 🎯 Propósito
Clase para validar pliegos SAP mediante la API de AI Core. Envía PDFs y recibe informes de validación.

---

## 🔧 Configuración Previa

```abap
" 1. Tabla ZPA_RPAIA_UTILS debe tener:
name_service = 'plecs_rev'
url = 'https://ai-core-api.cfapps.eu10-005.hana.ondemand.com'
endpoint = '/api/pdf-correction/generate-list'

" 2. Autenticación configurada:
" ZCL_RPAIA_AUTH con identificador 'AICOREAUTH'
```

---

## 📚 Métodos

### 1. `CHECK_CONNECTION` - Verificar Servidor

```abap
" Verifica si el servidor está disponible
CALL METHOD zcl_pliego_validator=>check_connection
  IMPORTING
    ev_is_online = lv_is_online.      " X = Online, '' = Offline

IF lv_is_online = abap_true.
  " Servidor disponible
ENDIF.
```

**Cuándo usar:** Antes de validar, en jobs de monitoreo, botones de test.

---

### 2. `VALIDATE_PLIEGO_FROM_FILE` - Validar desde Archivo

```abap
" Valida un PDF desde el PC del usuario
CALL METHOD zcl_pliego_validator=>validate_pliego_from_file
  EXPORTING
    iv_pdf_path = 'C:\temp\pliego.pdf'
    iv_pliego_id = 'PCAP_CT1074723'
  IMPORTING
    ev_pdf_xstring = lv_pdf_validado  " PDF con informe
    ev_http_status = lv_status.       " 200 = OK

IF lv_status = 200.
  " Guardar PDF validado
ENDIF.
```

**Cuándo usar:** Programas interactivos, transacciones con selección de archivo.

---

### 3. `VALIDATE_PLIEGO_FROM_BASE64` - Validar desde Base64

```abap
" Valida un PDF que ya tienes en base64
CALL METHOD zcl_pliego_validator=>validate_pliego_from_base64
  EXPORTING
    iv_pdf_base64 = lv_pdf_base64
    iv_pliego_id = 'PCAP_CT1074723'
  IMPORTING
    ev_pdf_xstring = lv_pdf_validado
    ev_http_status = lv_status.

IF lv_status = 200.
  " Usar PDF validado
ENDIF.
```

**Cuándo usar:** RFCs, BAPIs, procesos batch, integraciones.

---

## 📊 Parámetros Comunes

### Entrada (EXPORTING)
| Parámetro | Tipo | Obligatorio | Descripción |
|-----------|------|-------------|-------------|
| `iv_pdf_path` | string | Sí* | Ruta del archivo PDF |
| `iv_pdf_base64` | string | Sí** | PDF codificado en base64 |
| `iv_pliego_id` | string | No | ID del pliego (ej: PCAP_CT1074723) |
| `iv_context_id` | string | No | Contexto RAG para IA |
| `iv_username` | string | No | Usuario (default: sy-uname) |
| `iv_filename` | string | No | Nombre del archivo |

*Solo para `validate_pliego_from_file`  
**Solo para `validate_pliego_from_base64`

### Salida (IMPORTING)
| Parámetro | Tipo | Descripción |
|-----------|------|-------------|
| `ev_pdf_xstring` | xstring | PDF validado (binario) - **Usar para guardar** |
| `ev_pdf_base64` | string | PDF validado (base64) - Usar para APIs |
| `ev_http_status` | i | 200=OK, 401=Unauthorized, 404=Not Found, 500=Error |
| `ev_error_message` | string | Mensaje de error descriptivo |
| `ev_is_online` | abap_bool | X=Online, ''=Offline (solo check_connection) |

---

## 💡 Ejemplos Rápidos

### Ejemplo 1: Health Check
```abap
DATA: lv_online TYPE abap_bool.

zcl_pliego_validator=>check_connection(
  IMPORTING ev_is_online = lv_online ).

IF lv_online = abap_true.
  MESSAGE 'Servidor OK' TYPE 'S'.
ENDIF.
```

### Ejemplo 2: Validar y Guardar
```abap
DATA: lv_pdf TYPE xstring,
      lv_status TYPE i.

zcl_pliego_validator=>validate_pliego_from_file(
  EXPORTING iv_pdf_path = p_file
            iv_pliego_id = 'PCAP_TEST'
  IMPORTING ev_pdf_xstring = lv_pdf
            ev_http_status = lv_status ).

IF lv_status = 200.
  " Guardar PDF
  cl_gui_frontend_services=>gui_download(
    EXPORTING filename = 'validado.pdf'
              filetype = 'BIN'
    CHANGING data_tab = lt_binary ).
ENDIF.
```

### Ejemplo 3: Integración en Método
```abap
METHOD validar_pliego
  IMPORTING im_pdf_base64 TYPE string
  EXPORTING ex_pdf_validado TYPE xstring
            ex_status TYPE i
            ex_mensaje TYPE string.

  zcl_pliego_validator=>validate_pliego_from_base64(
    EXPORTING iv_pdf_base64 = im_pdf_base64
    IMPORTING ev_pdf_xstring = ex_pdf_validado
              ev_http_status = ex_status
              ev_error_message = ex_mensaje ).

ENDMETHOD.
```

---

## ⚠️ Códigos HTTP

| Código | Significado | Acción |
|--------|-------------|--------|
| 200 | OK | Validación exitosa |
| 401 | Unauthorized | Verificar token |
| 404 | Not Found | Verificar configuración tabla |
| 500 | Server Error | Reintentar más tarde |

---

## ✅ Buenas Prácticas

1. **Verificar conexión primero:**
```abap
zcl_pliego_validator=>check_connection( IMPORTING ev_is_online = lv_online ).
IF lv_online = abap_true.
  " Proceder con validación
ENDIF.
```

2. **Usar xstring para archivos:**
```abap
" ✅ CORRECTO
ev_pdf_xstring  " Para guardar archivos

" ❌ INCORRECTO
ev_pdf_base64   " Solo para APIs/transmisión
```

3. **Manejar errores:**
```abap
IF lv_http_status <> 200.
  MESSAGE lv_error_message TYPE 'E'.
  RETURN.
ENDIF.
```

---

## 🧪 Programas de Prueba

| Programa | Propósito |
|----------|-----------|
| `ZTEST_HEALTH_CHECK` | Verificar conexión |
| `ZTEST_PLIEGO_VALIDATOR` | Validar PDF completo |
| `ZTEST_HTTP_CONNECTION` | Diagnóstico de red |

---

## 🆘 Troubleshooting

| Error | Solución |
|-------|----------|
| "Configuración no encontrada" | Verificar tabla ZPA_RPAIA_UTILS |
| "Error obteniendo token" | Verificar ZCL_RPAIA_AUTH config |
| "Connection closed" | PDF muy grande o timeout |
| PDF corrupto (1KB) | Usar ev_pdf_xstring, no ev_pdf_base64 |

---

**Documentación completa:** `README_CLASE_VALIDADOR.md`  
**Ejemplos detallados:** `EJEMPLO_USO_CLASE.abap`
