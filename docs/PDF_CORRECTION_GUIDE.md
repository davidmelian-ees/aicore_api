# 📄 Guía de Corrección de PDFs

Sistema de corrección ortográfica automática para documentos PDF con dos enfoques diferentes.

## 🎯 Funcionalidades

### 1. **PDF con Lista de Correcciones**
- Mantiene el PDF original intacto
- Añade páginas con lista de correcciones al final
- Formato: `• palabra_incorrecta -> palabra_correcta`
- Ideal para revisión manual

### 2. **Aplicación Directa de Correcciones**
- ✨ **NUEVO**: Preserva formato original (posiciones, fuentes, colores)
- Aplica correcciones en posiciones exactas del texto
- Mantiene layout y estructura visual
- Fallback automático a método simple si es necesario
- Control total sobre los cambios

## 🚀 Uso Rápido

### Interfaz Web
Accede a la herramienta de prueba:
```
http://localhost:4000/public/pdf-correction-test.html
```

### API Endpoints

#### 1. Generar PDF con Lista de Correcciones
```bash
POST /api/pdf-correction/generate-list
Content-Type: multipart/form-data

# Campos:
# - pdf: archivo PDF (required)
# - customPrompt: prompt personalizado (optional)
```

#### 2. Aplicar Correcciones Directamente
```bash
POST /api/pdf-correction/apply-corrections
Content-Type: multipart/form-data

# Campos:
# - pdf: archivo PDF (required)
# - corrections: lista de correcciones (required)
```

#### 3. Solo Generar Correcciones
```bash
POST /api/pdf-correction/generate-corrections
Content-Type: multipart/form-data

# Campos:
# - pdf: archivo PDF (required)
```

## 📝 Formato de Correcciones

```
• Dokumento -> Documento
• parrafo -> párrafo
• ejemlo, -> ejemplo,
• recivir -> recibir
• tanbien -> también
```

**Reglas:**
- Una corrección por línea
- Formato: `• palabra_incorrecta -> palabra_correcta`
- Bullet point opcional (`•`, `-`, `*`)
- Espacios alrededor de `->` son opcionales

## 🔧 Ejemplos de Uso

### Ejemplo 1: Workflow Completo con Lista
```javascript
const formData = new FormData();
formData.append('pdf', pdfFile);

const response = await fetch('/api/pdf-correction/generate-list', {
    method: 'POST',
    body: formData
});

if (response.ok) {
    const blob = await response.blob();
    // Descargar PDF con correcciones listadas
    downloadFile(blob, 'correcciones-documento.pdf');
}
```

### Ejemplo 2: Aplicar Correcciones Específicas
```javascript
const corrections = `
• tanbien -> también
• recivir -> recibir
• Dokumento -> Documento
`;

const formData = new FormData();
formData.append('pdf', pdfFile);
formData.append('corrections', corrections);

const response = await fetch('/api/pdf-correction/apply-corrections', {
    method: 'POST',
    body: formData
});

if (response.ok) {
    const blob = await response.blob();
    // Descargar PDF corregido
    downloadFile(blob, 'documento-corregido.pdf');
}
```

### Ejemplo 3: Solo Generar Lista de Correcciones
```javascript
const formData = new FormData();
formData.append('pdf', pdfFile);

const response = await fetch('/api/pdf-correction/generate-corrections', {
    method: 'POST',
    body: formData
});

const result = await response.json();
console.log('Correcciones encontradas:', result.correctionsList);
console.log('Total:', result.totalCorrections);
```

## ⚙️ Configuración Técnica

### Servicios Utilizados
- **SAP AI Core**: Generación de correcciones ortográficas
- **pdf-lib**: Manipulación de PDFs
- **pdfjs-dist**: Extracción de texto
- **multer**: Manejo de archivos subidos

### Límites
- Tamaño máximo de archivo: 50MB
- Texto máximo para corrección: 80,000 caracteres
- Correcciones máximas mostradas en PDF: 20

### Estructura de Archivos
```
services/
├── pdfCorrectionService.js    # Lógica principal
└── documentProcessor.js       # Extracción de texto

routes/
└── pdfCorrection.js          # Endpoints API

public/
└── pdf-correction-test.html  # Interfaz de prueba
```

## 🎨 Interfaz Web

La interfaz web incluye:
- **Drag & Drop**: Arrastra PDFs directamente
- **Vista previa**: Muestra archivo seleccionado
- **Dos workflows**: Lista vs Aplicación directa
- **Ejemplos**: Formato de correcciones
- **Feedback visual**: Estados de carga y resultados

### Características
- ✅ Responsive design
- ✅ Drag & drop de archivos
- ✅ Validación de tipos de archivo
- ✅ Indicadores de progreso
- ✅ Descarga automática de resultados
- ✅ Manejo de errores

## 🔍 Casos de Uso

### 1. **Revisión de Documentos Oficiales**
```bash
# Generar lista para revisión manual
curl -X POST http://localhost:4000/api/pdf-correction/generate-list \
  -F "pdf=@documento-oficial.pdf" \
  -o correcciones-documento.pdf
```

### 2. **Corrección Automática de Borradores**
```bash
# Aplicar correcciones conocidas
curl -X POST http://localhost:4000/api/pdf-correction/apply-corrections \
  -F "pdf=@borrador.pdf" \
  -F "corrections=• tanbien -> también
• recivir -> recibir" \
  -o borrador-corregido.pdf
```

### 3. **Análisis de Calidad Ortográfica**
```bash
# Solo obtener estadísticas
curl -X POST http://localhost:4000/api/pdf-correction/generate-corrections \
  -F "pdf=@documento.pdf"
```

## 🚨 Consideraciones

### Ventajas del Enfoque por Lista
- ✅ Preserva formato original 100%
- ✅ Permite revisión manual
- ✅ No modifica contenido original
- ✅ Ideal para documentos oficiales

### Ventajas del Enfoque por Replace
- ✅ **Preserva formato original** (posiciones, fuentes, colores)
- ✅ Corrección directa en posiciones exactas
- ✅ Mantiene layout y estructura visual
- ✅ Control granular de cambios
- ✅ Fallback automático si es necesario
- ✅ Ideal para documentos con formato complejo

### Limitaciones
- ⚠️ Requiere texto extraíble del PDF (no escaneados)
- ⚠️ Dependiente de SAP AI Core para correcciones automáticas
- ⚠️ Fuentes complejas pueden aproximarse a Helvetica
- ⚠️ Colores se aproximan a negro por defecto
- ⚠️ Fallback a método simple si falla preservación

## 📊 Monitoreo

### Health Check
```bash
GET /api/pdf-correction/health
```

### Logs
Los logs incluyen:
- Archivos procesados
- Correcciones aplicadas
- Errores de procesamiento
- Tiempos de respuesta

## 🔧 Desarrollo

### Añadir Nuevas Funcionalidades
1. Modificar `pdfCorrectionService.js`
2. Añadir endpoints en `pdfCorrection.js`
3. Actualizar interfaz web si es necesario

### Testing
Usa la interfaz web en `/public/pdf-correction-test.html` para pruebas rápidas.

---

**💡 Tip**: Para mejores resultados, usa PDFs con texto seleccionable (no escaneados) y archivos menores a 10MB para procesamiento más rápido.
