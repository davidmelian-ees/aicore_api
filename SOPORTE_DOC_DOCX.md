# Soporte para archivos DOC/DOCX en PDF Correction API

## 📋 Descripción

El endpoint `/api/pdf-correction/generate-list` ahora soporta archivos **DOC**, **DOCX** y **PDF**. Los archivos DOC/DOCX se convierten automáticamente a PDF antes de procesarse.

## 🚀 Uso

### Opción 1: Subir archivo PDF (como antes)

```bash
curl -X POST {{base_url}}/api/pdf-correction/generate-list \
  -F "pdf=@documento.pdf" \
  -F "pliegoId=PLIEGO_001" \
  -F "contextId=context123"
```

### Opción 2: Subir archivo DOC/DOCX (NUEVO)

```bash
curl -X POST {{base_url}}/api/pdf-correction/generate-list \
  -F "pdf=@documento.docx" \
  -F "pliegoId=PLIEGO_002" \
  -F "contextId=context123"
```

### Opción 3: Enviar PDF en base64 (como antes)

```bash
curl -X POST {{base_url}}/api/pdf-correction/generate-list \
  -H "Content-Type: application/json" \
  -d '{
    "pdfBase64": "JVBERi0xLjQKJeLjz9MKMSAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgMiAwIFI+PgplbmRvYmoKMiAwIG9iago8PC9UeXBlL1BhZ2VzL0NvdW50IDEvS2lkc1szIDAgUl0+PgplbmRvYmoKMyAwIG9iago8PC9UeXBlL1BhZ2UvTWVkaWFCb3hbMCAwIDYxMiA3OTJdL1BhcmVudCAyIDAgUi9SZXNvdXJjZXM8PC9Gb250PDwvRjEgNCAwIFI+Pj4+L0NvbnRlbnRzIDUgMCBSPj4KZW5kb2JqCjQgMCBvYmoKPDwvVHlwZS9Gb250L1N1YnR5cGUvVHlwZTEvQmFzZUZvbnQvSGVsdmV0aWNhPj4KZW5kb2JqCjUgMCBvYmoKPDwvTGVuZ3RoIDQ0Pj4Kc3RyZWFtCkJUCi9GMSA0OCBUZgoxMCA3MDAgVGQKKEhlbGxvIFdvcmxkKSBUagpFVAplbmRzdHJlYW0KZW5kb2JqCnhyZWYKMCA2CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDAxNSAwMDAwMCBuIAowMDAwMDAwMDY0IDAwMDAwIG4gCjAwMDAwMDAxMjEgMDAwMDAgbiAKMDAwMDAwMDIzMCAwMDAwMCBuIAowMDAwMDAwMzAwIDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA2L1Jvb3QgMSAwIFI+PgpzdGFydHhyZWYKMzkzCiUlRU9G",
    "fileName": "documento.pdf",
    "pliegoId": "PLIEGO_003",
    "contextId": "context123"
  }'
```

### Opción 4: Enviar DOC/DOCX en base64 (NUEVO)

```bash
curl -X POST {{base_url}}/api/pdf-correction/generate-list \
  -H "Content-Type: application/json" \
  -d '{
    "pdfBase64": "UEsDBBQABgAIAAAAIQDfpNJsWgEAACAFAAATAAgCW0NvbnRlbnRfVHlwZXNdLnhtbCCiBAIooAAC...",
    "fileName": "documento.docx",
    "pliegoId": "PLIEGO_004",
    "contextId": "context123"
  }'
```

> **Nota**: El sistema detecta automáticamente si el base64 es un PDF o un DOC/DOCX analizando los "magic bytes" del archivo. No es necesario especificar el tipo de archivo.

## 📝 Formatos soportados

| Formato | Extensión | MIME Type | Conversión |
|---------|-----------|-----------|------------|
| PDF | `.pdf` | `application/pdf` | No requiere |
| Word 2007+ | `.docx` | `application/vnd.openxmlformats-officedocument.wordprocessingml.document` | Automática |
| Word 97-2003 | `.doc` | `application/msword` | Automática |

## 🔄 Proceso de conversión

### Cuando se sube un archivo DOC/DOCX:

1. **Detección**: El sistema detecta automáticamente el tipo de archivo por MIME type y extensión
2. **Extracción**: Se extrae el texto del documento:
   - `.docx` → usa `mammoth` (formato moderno)
   - `.doc` → usa `word-extractor` (formato antiguo Word 97-2003)
3. **Conversión**: Se genera un PDF con el texto extraído usando `pdf-lib`
4. **Procesamiento**: El PDF generado se procesa normalmente como cualquier otro PDF
5. **Limpieza**: Los archivos temporales se eliminan automáticamente

### Cuando se envía base64:

1. **Decodificación**: Se decodifica el string base64 a Buffer
2. **Detección automática**: Se analizan los "magic bytes" para identificar el tipo de archivo:
   - PDF: Comienza con `%PDF` (bytes: `25 50 44 46`)
   - DOCX: Comienza con `PK..` (archivo ZIP, bytes: `50 4B 03 04`)
   - DOC: Comienza con `D0CF11E0A1B11AE1` (OLE/COM Document)
3. **Extracción de texto**: Si es DOC/DOCX:
   - `.docx` → usa `mammoth`
   - `.doc` → usa `word-extractor`
4. **Conversión condicional**: Si es DOC/DOCX, se convierte a PDF automáticamente
5. **Procesamiento**: El PDF se procesa normalmente
6. **Limpieza**: Los archivos temporales se eliminan automáticamente

## ⚙️ Parámetros del endpoint

| Parámetro | Tipo | Requerido | Descripción |
|-----------|------|-----------|-------------|
| `pdf` | File | Sí* | Archivo PDF, DOC o DOCX |
| `pdfBase64` | String | Sí* | PDF o DOC/DOCX codificado en base64 (detección automática) |
| `fileName` | String | No | Nombre del archivo (solo para base64, opcional) |
| `pliegoId` | String | No | ID del pliego (se genera automáticamente si no se proporciona) |
| `contextId` | String | No | ID del contexto RAG para validación |
| `customPrompt` | String | No | Prompt personalizado para la validación |
| `username` | String | No | Usuario que realiza la validación |

*Nota: Debes proporcionar `pdf` (archivo) O `pdfBase64`, pero no ambos.

**Importante sobre base64**: El sistema detecta automáticamente si el base64 contiene un PDF o un DOC/DOCX analizando los primeros bytes del archivo. No necesitas especificar el tipo de archivo.

## 📤 Respuesta

El endpoint devuelve un PDF con el informe de validación:

```
Content-Type: application/pdf
Content-Disposition: attachment; filename="correcciones-documento.pdf"
```

El PDF contiene:
- Lista de errores críticos encontrados
- Advertencias y sugerencias
- Análisis visual del documento
- Clasificación del pliego

## ⚠️ Limitaciones

- **Tamaño máximo**: 50MB por archivo
- **Formato**: Solo se extrae texto plano (sin formato, imágenes o tablas complejas)
- **Compatibilidad**: Funciona mejor con archivos `.docx` modernos

## 🔍 Ejemplo completo con Postman

1. **Método**: POST
2. **URL**: `{{base_url}}/api/pdf-correction/generate-list`
3. **Body**: form-data
   - Key: `pdf` (tipo: File)
   - Value: Seleccionar archivo `.pdf`, `.doc` o `.docx`
   - Key: `pliegoId` (tipo: Text)
   - Value: `PLIEGO_TEST_001`
   - Key: `contextId` (tipo: Text)
   - Value: `mi_contexto_rag`

## 🛠️ Implementación técnica

### Archivos modificados/creados:

1. **`services/docToPdfConverter.js`** (NUEVO)
   - Función `convertDocxToPdf()`: Convierte archivo DOC/DOCX a PDF
   - Función `convertDocxBufferToPdf()`: Convierte buffer DOC/DOCX a PDF
   - Función `isDocxFile()`: Detecta archivos DOC/DOCX por MIME type
   - Función `detectFileTypeFromBuffer()`: Detecta tipo de archivo por magic bytes

2. **`routes/pdfCorrection.js`** (MODIFICADO)
   - Multer ahora acepta PDF, DOC y DOCX
   - Lógica de conversión automática para archivos subidos
   - Detección automática de tipo para base64
   - Limpieza de archivos temporales mejorada

### Dependencias utilizadas:

- `mammoth`: Extracción de texto de archivos DOCX (formato moderno)
- `word-extractor`: Extracción de texto de archivos DOC (formato antiguo Word 97-2003)
- `pdf-lib`: Generación de PDFs

## ✅ Compatibilidad hacia atrás

**Todas las funcionalidades existentes siguen funcionando sin cambios:**

- ✅ Subir PDF como archivo
- ✅ Enviar PDF en base64
- ✅ **NUEVO**: Subir DOC/DOCX como archivo
- ✅ **NUEVO**: Enviar DOC/DOCX en base64 (detección automática)
- ✅ Todos los parámetros opcionales
- ✅ Análisis visual
- ✅ Contexto RAG
- ✅ Almacenamiento de errores

## 🐛 Solución de problemas

### Error: "Solo se permiten archivos PDF, DOC o DOCX"
- Verifica que el archivo tenga la extensión correcta
- Asegúrate de que el MIME type sea correcto

### Error: "El documento DOCX está vacío"
- El archivo puede estar corrupto
- Intenta abrir el archivo en Word para verificar que contiene texto

### Error: "Tipo de archivo no soportado. Se esperaba PDF, DOC o DOCX pero se detectó: unknown"
- El base64 puede estar mal codificado
- Verifica que el archivo original sea realmente un PDF o DOC/DOCX
- Asegúrate de que el base64 esté completo (sin truncar)

### Error de conversión
- Verifica que el archivo no esté protegido con contraseña
- Asegúrate de que el archivo no esté corrupto
- **Archivos .doc antiguos ahora son soportados** usando `word-extractor`

### Error: "Invalid PDF structure" al enviar DOC en base64
- **Solución**: Este error ya no debería ocurrir. El sistema ahora detecta automáticamente si el base64 es un DOC/DOCX y lo convierte a PDF antes de procesarlo.
- Si persiste, verifica que el base64 esté correctamente codificado
