# Ejemplo de Respuesta JSON para SAP ABAP

## Endpoint: POST /api/pdf-correction/generate-list

### Request desde SAP ABAP

```abap
" URL con parámetro format=json
DATA: lv_url TYPE string VALUE 
  'https://ai-core-api.cfapps.eu10-005.hana.ondemand.com/api/pdf-correction/generate-list?format=json'.

" Headers
lo_http_client->request->set_header_field(
  name  = 'Content-Type'
  value = 'application/json' ).

lo_http_client->request->set_header_field(
  name  = 'Accept'
  value = 'application/json' ).

" Body JSON
DATA: lv_json_body TYPE string.
CONCATENATE '{'
  '"pdfBase64":"' lv_pdf_base64 '",'
  '"fileName":"pliego_ejemplo.pdf",'
  '"pliegoId":"PLIEGO_001",'
  '"contextId":"CTX_NORMATIVA_2024",'
  '"username":"' sy-uname '"'
  '}' INTO lv_json_body.

lo_http_client->request->set_cdata( lv_json_body ).
```

---

## Respuesta JSON de la API

### Estructura Completa

```json
{
  "success": true,
  "pdf": "JVBERi0xLjQKJeLjz9MKMyAwIG9iago8PC9UeXBlIC9QYWdlCi9QYXJlbnQgMSAwIFIKL01lZGlhQm94IFswIDAgNTk1LjI4IDg0MS44OV0KL0NvbnRlbnRzIDQgMCBSCi9SZXNvdXJjZXMgPDwKL1Byb2NTZXQgWy9QREYgL1RleHQgL0ltYWdlQiAvSW1hZ2VDIC9JbWFnZUldCi9Gb250IDw8Ci9GMSA1IDAgUgo+Pgo+Pgo+PgplbmRvYmoKNCAwIG9iago8PC9MZW5ndGggMTIzND4+CnN0cmVhbQpCVAovRjEgMTIgVGYKNzIgNzIwIFRkCihFc3RlIGVzIHVuIGVqZW1wbG8gZGUgUERGIGNvbiBjb3JyZWNjaW9uZXMpIFRqCkVUCmVuZHN0cmVhbQplbmRvYmoK...",
  "fileSize": 245678,
  "fileName": "pliego_ejemplo.pdf",
  "pliegoId": "PLIEGO_001",
  "criticalErrors": 3,
  "warnings": 5,
  "correctionsList": "# ERRORES CRÍTICOS\n\n1. **Página 1** - Falta fecha de publicación\n   - Ubicación: Encabezado\n   - Corrección: Añadir fecha según formato DD/MM/YYYY\n\n2. **Página 3** - Referencia normativa incorrecta\n   - Ubicación: Sección 2.1\n   - Corrección: Cambiar \"RD 123/2020\" por \"RD 456/2021\"\n\n# ADVERTENCIAS\n\n1. **Página 2** - Formato de tabla no estándar\n2. **Página 5** - Numeración de apartados inconsistente",
  "metadata": {
    "fileName": "pliego_ejemplo.pdf",
    "fileSize": 123456,
    "pdfSize": 245678,
    "processingTime": 5420,
    "timestamp": "2025-12-01T09:00:00.000Z"
  },
  "errorStorage": {
    "pliegoId": "PLIEGO_001",
    "stored": true,
    "criticalErrors": 3,
    "warnings": 5
  }
}
```

---

## Campos Principales (Requeridos por SAP)

### 1. `pdf` (string)
- **Tipo**: String base64
- **Descripción**: PDF con correcciones en formato base64
- **Uso en ABAP**: Decodificar con `SSFC_BASE64_DECODE` y guardar con `GUI_DOWNLOAD`

```abap
DATA: lv_pdf_base64 TYPE string,
      lv_pdf_xstring TYPE xstring.

" Extraer del JSON
FIND REGEX '"pdf":"([^"]+)"' IN lv_json_response SUBMATCHES lv_pdf_base64.

" Decodificar base64
CALL FUNCTION 'SSFC_BASE64_DECODE'
  EXPORTING
    b64data = lv_pdf_base64
  IMPORTING
    bindata = lv_pdf_xstring.
```

### 2. `fileSize` (number)
- **Tipo**: Integer (bytes)
- **Descripción**: Tamaño del PDF generado en bytes
- **Uso en ABAP**: Parámetro `bin_filesize` de `GUI_DOWNLOAD`

```abap
DATA: lv_filesize TYPE i.

" Extraer del JSON
FIND REGEX '"fileSize":(\d+)' IN lv_json_response SUBMATCHES DATA(lv_size_str).
lv_filesize = lv_size_str.

" Usar en GUI_DOWNLOAD
CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
    filename     = 'correcciones_pliego.pdf'
    filetype     = 'BIN'
    bin_filesize = lv_filesize  " ← Campo crítico
  TABLES
    data_tab     = lt_binary_tab.
```

---

## Campos Adicionales

### 3. `fileName` (string)
- Nombre del archivo procesado
- Ejemplo: `"pliego_ejemplo.pdf"`

### 4. `pliegoId` (string)
- ID único del pliego validado
- Ejemplo: `"PLIEGO_001"`

### 5. `criticalErrors` (number)
- Número de errores críticos encontrados
- Ejemplo: `3`

### 6. `warnings` (number)
- Número de advertencias encontradas
- Ejemplo: `5`

### 7. `correctionsList` (string)
- Lista detallada de correcciones en formato Markdown
- Contiene descripción de cada error y su corrección sugerida

### 8. `metadata` (object)
- Información adicional del procesamiento
- Incluye tiempos, tamaños, timestamp

---

## Ejemplo Completo en ABAP

```abap
*&---------------------------------------------------------------------*
*& Parsear respuesta JSON y guardar PDF
*&---------------------------------------------------------------------*

DATA: lv_json_response TYPE string,
      lv_pdf_base64    TYPE string,
      lv_pdf_xstring   TYPE xstring,
      lv_filesize      TYPE i,
      lv_filename      TYPE string,
      lv_pliego_id     TYPE string,
      lv_errors        TYPE i,
      lv_warnings      TYPE i,
      lt_binary_tab    TYPE TABLE OF x255.

" 1. Obtener respuesta JSON de la API
lv_json_response = lo_http_client->response->get_cdata( ).

" 2. Parsear campos principales
FIND REGEX '"pdf":"([^"]+)"' IN lv_json_response 
  SUBMATCHES lv_pdf_base64.

FIND REGEX '"fileSize":(\d+)' IN lv_json_response 
  SUBMATCHES DATA(lv_size_str).
lv_filesize = lv_size_str.

FIND REGEX '"fileName":"([^"]+)"' IN lv_json_response 
  SUBMATCHES lv_filename.

FIND REGEX '"pliegoId":"([^"]+)"' IN lv_json_response 
  SUBMATCHES lv_pliego_id.

FIND REGEX '"criticalErrors":(\d+)' IN lv_json_response 
  SUBMATCHES DATA(lv_errors_str).
lv_errors = lv_errors_str.

FIND REGEX '"warnings":(\d+)' IN lv_json_response 
  SUBMATCHES DATA(lv_warnings_str).
lv_warnings = lv_warnings_str.

" 3. Decodificar PDF base64
CALL FUNCTION 'SSFC_BASE64_DECODE'
  EXPORTING
    b64data = lv_pdf_base64
  IMPORTING
    bindata = lv_pdf_xstring
  EXCEPTIONS
    OTHERS  = 1.

IF sy-subrc <> 0.
  WRITE: / '❌ Error decodificando PDF base64'.
  RETURN.
ENDIF.

" 4. Convertir xstring a tabla binaria
CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
  EXPORTING
    buffer        = lv_pdf_xstring
  IMPORTING
    output_length = DATA(lv_output_length)
  TABLES
    binary_tab    = lt_binary_tab.

" 5. Guardar PDF usando fileSize
DATA: lv_output_file TYPE string.
CONCATENATE 'C:\temp\correcciones_' lv_pliego_id '.pdf' 
  INTO lv_output_file.

CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
    filename              = lv_output_file
    filetype              = 'BIN'
    bin_filesize          = lv_filesize  " ← Usar fileSize de la API
  TABLES
    data_tab              = lt_binary_tab
  EXCEPTIONS
    file_write_error      = 1
    OTHERS                = 2.

IF sy-subrc = 0.
  WRITE: / '✅ PDF guardado correctamente:'.
  WRITE: / '   Archivo:', lv_output_file.
  WRITE: / '   Tamaño:', lv_filesize, 'bytes'.
  WRITE: / '   Errores críticos:', lv_errors.
  WRITE: / '   Advertencias:', lv_warnings.
ELSE.
  WRITE: / '❌ Error guardando PDF'.
ENDIF.
```

---

## Validación de Respuesta

### Verificar que la respuesta es JSON válido

```abap
" Verificar que comienza con {
IF lv_json_response(1) <> '{'.
  WRITE: / '❌ Respuesta no es JSON válido'.
  RETURN.
ENDIF.

" Verificar campo success
IF lv_json_response CS '"success":true'.
  WRITE: / '✅ Validación exitosa'.
ELSE.
  WRITE: / '❌ Validación falló'.
  " Extraer mensaje de error
  FIND REGEX '"error":"([^"]+)"' IN lv_json_response 
    SUBMATCHES DATA(lv_error_msg).
  WRITE: / '   Error:', lv_error_msg.
ENDIF.
```

---

## Notas Importantes

1. **Campo `pdf`**: Contiene el PDF completo en base64, no solo una referencia
2. **Campo `fileSize`**: Es el tamaño del PDF **generado** (con correcciones), no del original
3. **Decodificación**: Usar siempre `SSFC_BASE64_DECODE` para convertir a xstring
4. **Guardado**: El parámetro `bin_filesize` en `GUI_DOWNLOAD` es **obligatorio** para archivos binarios
5. **Validación**: Verificar siempre que `success = true` antes de procesar el PDF

---

## Diferencias con Respuesta PDF Directa

| Aspecto | JSON Response | PDF Response |
|---------|--------------|--------------|
| **Content-Type** | `application/json` | `application/pdf` |
| **Formato PDF** | Base64 string | Binary directo |
| **Campo fileSize** | ✅ En JSON | ❌ Solo en header `Content-Length` |
| **Metadata** | ✅ Incluida | ❌ Solo en headers custom |
| **Uso en ABAP** | Requiere parseo JSON | Descarga directa |
| **Ventaja** | Más información | Más simple |

---

## Ejemplo de Request Completo

```http
POST https://ai-core-api.cfapps.eu10-005.hana.ondemand.com/api/pdf-correction/generate-list?format=json
Content-Type: application/json
Accept: application/json

{
  "pdfBase64": "JVBERi0xLjQKJeLjz9MK...",
  "fileName": "pliego_ejemplo.pdf",
  "pliegoId": "PLIEGO_001",
  "contextId": "CTX_NORMATIVA_2024",
  "username": "JMELIAN"
}
```

**Respuesta:**
```json
{
  "success": true,
  "pdf": "JVBERi0xLjQKJeLjz9MK...",
  "fileSize": 245678,
  "fileName": "pliego_ejemplo.pdf",
  "pliegoId": "PLIEGO_001",
  "criticalErrors": 3,
  "warnings": 5,
  "correctionsList": "...",
  "metadata": {...}
}
```
