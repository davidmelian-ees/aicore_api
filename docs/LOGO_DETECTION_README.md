# 🔍 Sistema de Detección de Logos en Pliegos

## Descripción

Sistema automático de detección y validación de logos en documentos PDF de pliegos administrativos. Detecta la presencia del logo obligatorio de "Infraestructuras de Cataluña" y otros logos opcionales.

## 🎯 Características

- ✅ **Detección automática** de imágenes/logos en PDFs
- ✅ **Validación obligatoria** del logo institucional
- ✅ **Clasificación por posición** (header, footer, body)
- ✅ **Niveles de confianza** (high, medium, low, none)
- ✅ **Reportes detallados** con errores, advertencias e información
- ✅ **Integración con RAG** para entrenar la IA
- ✅ **Descripción textual** para contexto RAG

## 📋 Requisitos

### Logo Obligatorio

**TODOS los pliegos oficiales DEBEN incluir:**

- Logo de "Infraestructuras de Cataluña"
- Ubicación: Parte superior (header)
- Presente en todas las páginas

### Logos Opcionales

Según el tipo de pliego:
- Logo del Ayuntamiento
- Logo de empresa contratista
- Logos de certificaciones

## 🚀 Uso

### 1. Validar Logo al Subir Documento

Al subir un PDF al sistema RAG, la detección de logos es **automática**:

```bash
curl -X POST http://localhost:4000/api/rag/upload \
  -F "document=@pliego.pdf" \
  -F "contextId=PLIEGOS_TERMINADOS"
```

**Respuesta incluye:**
```json
{
  "success": true,
  "document": { ... },
  "logoDetection": {
    "filename": "pliego.pdf",
    "hasRequiredLogo": true,
    "confidence": "high",
    "status": "success",
    "errors": [],
    "warnings": [],
    "info": [...]
  }
}
```

### 2. Validar Logo sin Subir al RAG

Para solo validar logos sin indexar el documento:

```bash
curl -X POST http://localhost:4000/api/rag/validate-logos \
  -F "pdf=@pliego.pdf"
```

**Respuesta:**
```json
{
  "success": true,
  "message": "Validación de logos completada",
  "report": {
    "filename": "pliego.pdf",
    "hasRequiredLogo": true,
    "confidence": "high",
    "status": "success",
    "errors": [],
    "warnings": [],
    "info": [...]
  },
  "description": "ANÁLISIS DE LOGOS - pliego.pdf\n\n✅ LOGO OBLIGATORIO: SÍ...",
  "details": {
    "totalImages": 15,
    "headerImages": 10,
    "footerImages": 5,
    "pagesWithImages": [1, 2, 3, 4, 5, ...]
  }
}
```

### 3. Test Manual con Script

```bash
node tests/test_logo_detection.js ./uploads/pliego.pdf
```

**Salida:**
```
🔍 TEST DE DETECCIÓN DE LOGOS
============================================================

📄 Analizando archivo: ./uploads/pliego.pdf
============================================================

1️⃣ DETECCIÓN DE LOGOS...

📊 Resultados de detección:
   - Total de imágenes: 15
   - Páginas con imágenes: 10
   - Imágenes en header: 10
   - Imágenes en footer: 5
   - Otras imágenes: 0

2️⃣ REPORTE DE VALIDACIÓN...

📋 Estado: SUCCESS
   - Logo obligatorio: ✅ SÍ
   - Confianza: high

ℹ️  INFORMACIÓN:
   1. LOGO_CORRECTO
      Logo obligatorio detectado correctamente en 10 ubicaciones
      📝 Confianza: high

✅ DOCUMENTO VÁLIDO
   El pliego cumple con todos los requisitos de logos.
```

## 📊 Estados de Validación

### ✅ SUCCESS (Éxito)

- Logo obligatorio detectado
- Presente en múltiples páginas
- Confianza alta
- **Acción**: Documento válido para publicar

### ⚠️ WARNING (Advertencia)

- Logo detectado pero inconsistente
- No en todas las páginas
- Confianza media/baja
- **Acción**: Revisar manualmente

### ❌ ERROR (Error)

- Logo obligatorio NO detectado
- Sin imágenes en header
- **Acción**: NO publicar, añadir logo

## 🔧 Integración con IA

### Entrenar la IA

1. **Subir la guía de referencia:**

```bash
curl -X POST http://localhost:4000/api/rag/upload \
  -F "document=@docs/GUIA_LOGOS_PLIEGOS.md" \
  -F "contextId=DOCUMENTACION_SISTEMA" \
  -F "tags=logos,normativa,referencia"
```

2. **Subir ejemplos con logos correctos:**

```bash
# Ejemplo correcto
curl -X POST http://localhost:4000/api/rag/upload \
  -F "document=@pliegos/CT1078146_plec_edificacio_obert_amb_lots.pdf" \
  -F "contextId=PLIEGOS_CON_LOGOS_CORRECTOS" \
  -F "tags=ejemplo_correcto,con_logos"
```

3. **Subir ejemplos sin logos (errores):**

```bash
# Ejemplo incorrecto
curl -X POST http://localhost:4000/api/rag/upload \
  -F "document=@pliegos/CT1078147_plec_sin_logos.pdf" \
  -F "contextId=PLIEGOS_SIN_LOGOS_ERROR" \
  -F "tags=ejemplo_error,sin_logos"
```

### Consultar a la IA

Una vez entrenada, la IA puede responder preguntas:

```bash
curl -X POST http://localhost:4000/api/rag/chat \
  -H "Content-Type: application/json" \
  -d '{
    "message": "¿Este pliego tiene el logo obligatorio de Infraestructuras de Cataluña?",
    "contextId": "PLIEGOS_TERMINADOS"
  }'
```

**La IA responderá basándose en:**
- Metadatos de detección automática
- Ejemplos de referencia en el RAG
- Guía de normativa de logos

## 📁 Estructura de Archivos

```
aicore_api/
├── services/
│   └── logoDetectionService.js      # Servicio principal
├── routes/
│   └── rag.js                        # Endpoints (modificado)
├── docs/
│   ├── GUIA_LOGOS_PLIEGOS.md        # Guía de referencia
│   └── LOGO_DETECTION_README.md     # Este archivo
└── tests/
    └── test_logo_detection.js        # Script de prueba
```

## 🎓 Cómo Funciona

### 1. Detección de Imágenes

```javascript
// El servicio analiza cada página del PDF
for (let pageNum = 1; pageNum <= pdf.numPages; pageNum++) {
  const page = await pdf.getPage(pageNum);
  
  // Busca operaciones de imagen (paintImageXObject)
  // Obtiene posición Y de cada imagen
  // Clasifica: header (85-100%), footer (0-15%), body (15-85%)
}
```

### 2. Validación del Logo Obligatorio

```javascript
// Si hay imágenes en header de múltiples páginas
if (headerImages.length >= 1) {
  hasRequiredLogo = true;
  confidence = headerImages.length > 3 ? 'high' : 'medium';
}
```

### 3. Generación de Reporte

```javascript
// Clasifica errores, advertencias e información
if (!hasRequiredLogo) {
  errors.push({
    type: 'LOGO_OBLIGATORIO_AUSENTE',
    severity: 'critical',
    message: 'No se detectó el logo obligatorio...'
  });
}
```

### 4. Descripción para RAG

```javascript
// Genera texto descriptivo para entrenar la IA
const description = `
ANÁLISIS DE LOGOS - ${filename}

✅ LOGO OBLIGATORIO: ${hasRequiredLogo ? 'SÍ' : 'NO'}
   - Logo de "Infraestructuras de Cataluña" detectado en header
   - Presente en ${headerImages.length} ubicaciones
   - Este documento ${hasRequiredLogo ? 'CUMPLE' : 'NO CUMPLE'} con el requisito
`;
```

## 🔍 Casos de Uso

### Caso 1: Validación Antes de Publicar

```javascript
// Validar pliego antes de publicación oficial
const validation = await fetch('/api/rag/validate-logos', {
  method: 'POST',
  body: formData
});

if (validation.report.status === 'error') {
  alert('❌ El pliego NO puede ser publicado. Falta el logo obligatorio.');
} else if (validation.report.status === 'warning') {
  alert('⚠️ El pliego requiere revisión manual.');
} else {
  alert('✅ El pliego es válido y puede ser publicado.');
}
```

### Caso 2: Análisis Masivo

```javascript
// Analizar múltiples pliegos
const pliegos = ['pliego1.pdf', 'pliego2.pdf', 'pliego3.pdf'];

for (const pliego of pliegos) {
  const result = await uploadAndValidate(pliego);
  
  if (!result.logoDetection.hasRequiredLogo) {
    console.log(`❌ ${pliego}: SIN LOGO OBLIGATORIO`);
    pliegosConError.push(pliego);
  }
}
```

### Caso 3: Consulta a la IA

```javascript
// Preguntar a la IA sobre logos
const response = await fetch('/api/rag/chat', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    message: '¿Qué pliegos no tienen el logo de Infraestructuras de Cataluña?',
    contextId: 'PLIEGOS_TERMINADOS'
  })
});

// La IA responderá basándose en los metadatos de detección
```

## 📈 Métricas y Estadísticas

El sistema registra:

- Total de pliegos analizados
- Pliegos con logo correcto
- Pliegos sin logo (errores)
- Pliegos con advertencias
- Nivel de confianza promedio

## 🛠️ Configuración

### Personalizar Tamaño Mínimo de Logo

```javascript
// En logoDetectionService.js
this.minimumLogoSize = 1000; // bytes
```

### Personalizar Umbrales de Posición

```javascript
// Cambiar rangos de header/footer
if (relativePosition > 0.85) {  // Header: 85-100%
  position = 'header';
} else if (relativePosition < 0.15) {  // Footer: 0-15%
  position = 'footer';
}
```

## 🐛 Troubleshooting

### Problema: "No se detectan logos pero están presentes"

**Causa**: Logo es texto o forma vectorial, no imagen

**Solución**: Regenerar PDF con logos como imágenes estándar

### Problema: "Se detectan demasiadas imágenes"

**Causa**: Documento contiene fotos, diagramas

**Solución**: El sistema filtra por posición (solo header cuenta para logo obligatorio)

### Problema: "Confianza baja aunque logo está presente"

**Causa**: Logo no está en todas las páginas

**Solución**: Añadir logo a todas las páginas del documento

## 📞 Soporte

Para más información, consulta:
- `docs/GUIA_LOGOS_PLIEGOS.md` - Guía completa de normativa
- `services/logoDetectionService.js` - Código fuente
- `tests/test_logo_detection.js` - Ejemplos de uso

---

**Versión**: 1.0  
**Fecha**: Noviembre 2025  
**Autor**: Sistema de Validación Automática de Pliegos
