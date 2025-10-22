# 📮 Guía de Postman para PDF Correction API

## 📥 Importar Colección

### 1. Importar Colección Principal
1. Abre Postman
2. Click en **Import**
3. Selecciona `PDF_Correction_API.postman_collection.json`
4. Click **Import**

### 2. Importar Entorno
1. Click en **Import**
2. Selecciona `PDF_Correction_Environment.postman_environment.json`
3. Click **Import**
4. Selecciona el entorno **"PDF Correction Environment"** en el dropdown

## 🚀 Endpoints Disponibles

### 1. **Health Check**
```
GET /api/pdf-correction/health
```
- **Propósito**: Verificar estado del servicio
- **Respuesta**: JSON con información del servicio

### 2. **Generar PDF con Lista de Correcciones**
```
POST /api/pdf-correction/generate-list
```
- **Body**: `multipart/form-data`
- **Campos**:
  - `pdf`: Archivo PDF (required)
  - `customPrompt`: Prompt personalizado (optional)
- **Respuesta**: PDF con correcciones listadas

### 3. **Solo Generar Correcciones**
```
POST /api/pdf-correction/generate-corrections
```
- **Body**: `multipart/form-data`
- **Campos**:
  - `pdf`: Archivo PDF (required)
- **Respuesta**: JSON con lista de correcciones

### 4. **Aplicar Correcciones Directamente**
```
POST /api/pdf-correction/apply-corrections
```
- **Body**: `multipart/form-data`
- **Campos**:
  - `pdf`: Archivo PDF (required)
  - `corrections`: Lista de correcciones (required)
- **Respuesta**: PDF corregido

### 5. **Test Workflows**
```
POST /api/pdf-correction/test-workflow
```
- **Body**: `multipart/form-data`
- **Campos**:
  - `pdf`: Archivo PDF (required)
  - `workflow`: "list" o "apply" (required)

## 📝 Formatos de Correcciones

### Formato Texto (Recomendado)
```
• Dokumento -> Documento
• parrafo -> párrafo
• ejemlo -> ejemplo
• recivir -> recibir
```

### Formato JSON Array
```json
[
  {"from": "Dokumento", "to": "Documento"},
  {"from": "parrafo", "to": "párrafo"},
  {"from": "ejemlo", "to": "ejemplo"},
  {"from": "recivir", "to": "recibir"}
]
```

## 🔧 Configuración

### Variables de Entorno
- `base_url`: `http://localhost:4000` (desarrollo)
- `base_url_prod`: URL de producción
- `timeout`: `30000` ms

### Cambiar Servidor
Para cambiar entre desarrollo y producción:
1. Ve a **Environments**
2. Edita **"PDF Correction Environment"**
3. Cambia `base_url` por `base_url_prod`

## 📋 Flujo de Trabajo Recomendado

### Flujo Completo
1. **Health Check** → Verificar servicio
2. **Solo Generar Correcciones** → Obtener lista
3. **Editar correcciones** → Revisar y modificar
4. **Aplicar Correcciones** → Generar PDF final

### Flujo Rápido
1. **Test Workflow (apply)** → Proceso automático completo

## 🧪 Tests Automáticos

Cada request incluye tests automáticos:
- ✅ Status code exitoso (200/201)
- ✅ Tiempo de respuesta < 30 segundos

### Ver Resultados de Tests
1. Ejecuta cualquier request
2. Ve a la pestaña **Test Results**
3. Revisa los tests pasados/fallidos

## 📁 Subir Archivos PDF

### En Postman
1. Selecciona el endpoint
2. Ve a **Body** → **form-data**
3. En el campo `pdf`:
   - Cambia tipo a **File**
   - Click **Select Files**
   - Selecciona tu PDF

### Archivos de Prueba Recomendados
- PDFs con texto seleccionable (no escaneados)
- Tamaño < 10MB para mejor rendimiento
- Con errores ortográficos para probar correcciones

## 🔍 Debug y Troubleshooting

### Ver Logs del Servidor
Los logs incluyen:
```
[PDF-CORRECTION] Aplicando 3 correcciones preservando formato...
[PDF-CORRECTION] Color detectado: RGB(0, 0, 1)
[PDF-CORRECTION] Reemplazando "Ejemlo" → "Ejemplo"
```

### Errores Comunes
- **400**: Archivo PDF no proporcionado
- **500**: Error procesando PDF o SAP AI Core
- **Timeout**: PDF muy grande o servidor ocupado

### Soluciones
- Verificar que el archivo sea PDF válido
- Reducir tamaño del PDF si es muy grande
- Verificar que el servidor esté ejecutándose

## 📊 Respuestas Esperadas

### Generar Correcciones (JSON)
```json
{
  "success": true,
  "correctionsList": "• error -> correcto\n• otro -> otro_correcto",
  "corrections": [
    {"from": "error", "to": "correcto"}
  ],
  "totalCorrections": 1,
  "originalTextLength": 1500
}
```

### Aplicar Correcciones (PDF)
- **Content-Type**: `application/pdf`
- **Content-Disposition**: `attachment; filename="corregido-documento.pdf"`
- **Body**: Datos binarios del PDF

## 🎯 Tips de Uso

### Para Desarrollo
- Usa **Health Check** antes de empezar
- Prueba con PDFs pequeños primero
- Revisa los logs del servidor para debug

### Para Producción
- Cambia la variable `base_url` 
- Aumenta timeout si es necesario
- Usa PDFs optimizados

### Para Tests
- Usa **Test Workflow** para pruebas rápidas
- Guarda respuestas exitosas como ejemplos
- Documenta casos de uso específicos

---

**💡 Tip**: Guarda tus PDFs de prueba en una carpeta específica para reutilizarlos fácilmente en Postman.
