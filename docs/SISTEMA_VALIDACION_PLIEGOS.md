# Sistema de Validación de Pliegos SAP

## 📋 Descripción General

El sistema de validación de pliegos es un endpoint especializado que genera informes de validación estructural para documentos de pliegos SAP. Utiliza inteligencia artificial para detectar errores, advertencias y sugerencias basándose en patrones específicos documentados.

## 🎯 Objetivo

**Generar un PDF con informe de validación** que contenga:
- 🔴 **Errores críticos** (bloquean generación)
- 🟡 **Advertencias** (permiten continuar)
- ✅ **Sugerencias** de corrección específicas
- 📋 **Campos variables** detectados

## 🔧 Arquitectura del Sistema

### Endpoint Principal
```
POST /api/pdf-correction/generate-list
```

### Flujo de Procesamiento

```mermaid
graph TD
    A[PDF Subido] --> B[Extraer Texto]
    B --> C[Cargar Prompts de prompts_dev/]
    C --> D[Construir Prompt de Validación]
    D --> E[Enviar a SAP AI Core]
    E --> F[Generar Informe]
    F --> G[Crear PDF con Formato]
    G --> H[Devolver PDF de Validación]
```

## 📁 Archivos de Configuración

El sistema carga automáticamente estos archivos de `prompts_dev/`:

| Archivo | Propósito |
|---------|-----------|
| `NOMENCLATURA_PLIEGOS.txt` | Patrones de nomenclatura esperada |
| `ERRORES_COMUNES_PLIEGOS.txt` | Lista de errores frecuentes |
| `PLIEGOS_VALIDATION_SYSTEM.txt` | Sistema de validación estructural |
| `ANALISIS_PLIEGOS_GENERADOS.txt` | Análisis de pliegos generados |
| `PLIEGOS_ERRORES_EJEMPLOS.txt` | **NUEVO:** Ejemplos específicos de errores y patrones SAP |

## 📋 Contextos RAG Disponibles

| Contexto | Contenido | Uso Recomendado |
|----------|-----------|-----------------|
| `PLANTILLAS_BASE` | Plantillas limpias sin tags | Referencia de estructura correcta |
| `PLANTILLAS_TAGS` | Plantillas con marcadores SAP | Identificación de tags a reemplazar |
| `PLIEGOS_TERMINADOS_ENTRENAMIENTO` | Pliegos perfectos validados | Base de conocimiento de ejemplos correctos |
| `DOCUMENTOS_VALIDACION` | Pliegos para validar + errores documentados | **Recomendado para validación completa** |

## 🚀 Uso del Endpoint

### Opción 1: Validación Básica
```bash
curl -X POST http://localhost:4000/api/pdf-correction/generate-list \
  -F "pdf=@mi_pliego.pdf"
```
**→ Usa solo los prompts de `prompts_dev/`**

### Opción 2: Validación con Contexto RAG (Recomendado)
```bash
curl -X POST http://localhost:4000/api/pdf-correction/generate-list \
  -F "pdf=@mi_pliego.pdf" \
  -F "contextId=DOCUMENTOS_VALIDACION"
```
**→ Usa prompts + plantillas y ejemplos del contexto RAG**

### Opción 3: Prompt Personalizado
```bash
curl -X POST http://localhost:4000/api/pdf-correction/generate-list \
  -F "pdf=@mi_pliego.pdf" \
  -F "customPrompt=Analiza este pliego y encuentra solo errores críticos"
```
**→ Usa el prompt que especifiques**

### Opción 4: Validación Completa
```bash
curl -X POST http://localhost:4000/api/pdf-correction/generate-list \
  -F "pdf=@mi_pliego.pdf" \
  -F "contextId=DOCUMENTOS_VALIDACION" \
  -F "customPrompt=Enfócate en tags SAP sin reemplazar"
```
**→ Combina contexto RAG + prompt personalizado**

### Respuesta
- **Content-Type**: `application/pdf`
- **Archivo**: PDF con informe de validación
- **Nombre**: `correcciones-{nombre_original}.pdf`

## 📄 Estructura del Prompt Generado

```
SISTEMA DE VALIDACIÓN DE PLIEGOS SAP
================================================================================

CONTEXTO DE VALIDACIÓN:
[Contenido completo de PLIEGOS_VALIDATION_SYSTEM.txt]

ERRORES COMUNES A DETECTAR:
[Contenido completo de ERRORES_COMUNES_PLIEGOS.txt]

EJEMPLOS DE ERRORES REALES (ENTRENAMIENTO):
[Contenido completo de PLIEGOS_ERRORES_EJEMPLOS.txt]

NOMENCLATURA ESPERADA:
[Contenido completo de NOMENCLATURA_PLIEGOS.txt]

CONTEXTO RAG ADICIONAL (EJEMPLOS Y PLANTILLAS):
[Si se especifica contextId, incluye documentos relevantes del contexto RAG:
- Plantillas de referencia
- Ejemplos de pliegos correctos
- Documentos con errores documentados
- Patrones específicos encontrados]

================================================================================
INSTRUCCIONES DE VALIDACIÓN:
================================================================================

1. ANALIZA el siguiente texto de pliego
2. IDENTIFICA errores según los patrones definidos arriba
3. GENERA un informe detallado con:
   - Errores críticos (bloquean generación)
   - Advertencias (permiten continuar)
   - Sugerencias de corrección específicas
   - Campos variables detectados

4. FORMATO DE RESPUESTA:
   🔴 ERRORES CRÍTICOS:
   - [Lista de errores que impiden continuar]
   
   🟡 ADVERTENCIAS:
   - [Lista de problemas menores]
   
   ✅ SUGERENCIAS:
   - [Correcciones específicas recomendadas]
   
   📋 CAMPOS VARIABLES DETECTADOS:
   - [Lista de variables SAP encontradas]

================================================================================
TEXTO DEL PLIEGO A VALIDAR:
================================================================================

[Texto extraído del PDF]

================================================================================
GENERA EL INFORME DE VALIDACIÓN:
================================================================================
```

## 🎨 Formato del PDF Generado

### Características Visuales
- **Título**: "INFORME DE VALIDACIÓN DE PLIEGO"
- **Fecha/Hora**: Timestamp de generación
- **Colores temáticos**:
  - 🔴 Errores críticos: Rojo
  - 🟡 Advertencias: Naranja
  - ✅ Sugerencias: Verde
  - 📋 Campos variables: Azul

### Formato de Texto
- **Negritas**: Procesa `**TEXTO**` automáticamente
- **Indentación**: Listas y sublistas
- **Saltos de línea**: Controlados y espaciado apropiado
- **Paginación**: Automática para documentos largos
- **Ancho controlado**: El texto no se sale de la página

### Ejemplo de Salida
```
INFORME DE VALIDACIÓN DE PLIEGO
Generado: 23/10/2025 16:19:32

🔴 ERRORES CRÍTICOS:
- Tags SAP {{PRESUPUESTO_BASE}} sin rellenar en línea 45
- Campos SÍ/NO duplicados en sección 3.2

🟡 ADVERTENCIAS:
- Formato de fecha incorrecto en "12/2025" (línea 23)
- Importes sin formato euro: "123456" debería ser "123.456,78 €"

✅ SUGERENCIAS:
- Corregir formato fechas a DD/MM/YYYY
- Usar formato 123.456,78 € para todos los importes
- Revisar ortografía: "contractacio" → "contractació"

📋 CAMPOS VARIABLES DETECTADOS:
- {{PRESUPUESTO_BASE}} (línea 45)
- {{FECHA_INICIO}} (línea 67)
- {{EMPRESA_ADJUDICATARIA}} (línea 89)
```

## 🔄 Flujo Técnico Detallado

### 1. Recepción del PDF
```javascript
// routes/pdfCorrection.js líneas 61-64
const result = await generatePDFWithCorrectionsList(
  req.file.path,
  req.body.customPrompt || null
);
```

### 2. Extracción de Texto
```javascript
// services/pdfCorrectionService.js
const documentData = await processDocument(originalPdfPath, 'application/pdf');
const originalText = documentData.chunks.map(chunk => chunk.content).join('\n\n');
```

### 3. Carga de Prompts
```javascript
async function loadValidationPrompts() {
  const promptsDir = path.join(process.cwd(), 'prompts_dev');
  
  const [nomenclatura, erroresComunes, validationSystem, analisisPliegos, ejemplosErrores] = 
    await Promise.all([
      fs.readFile(path.join(promptsDir, 'NOMENCLATURA_PLIEGOS.txt'), 'utf8'),
      fs.readFile(path.join(promptsDir, 'ERRORES_COMUNES_PLIEGOS.txt'), 'utf8'),
      fs.readFile(path.join(promptsDir, 'PLIEGOS_VALIDATION_SYSTEM.txt'), 'utf8'),
      fs.readFile(path.join(promptsDir, 'ANALISIS_PLIEGOS_GENERADOS.txt'), 'utf8'),
      fs.readFile(path.join(promptsDir, 'PLIEGOS_ERRORES_EJEMPLOS.txt'), 'utf8')
    ]);
    
  return { nomenclatura, erroresComunes, validationSystem, analisisPliegos, ejemplosErrores };
}
```

### 4. Carga de Contexto RAG (NUEVO)
```javascript
// 4. Obtener contexto RAG si se especifica
let ragContext = '';
if (contextId) {
  try {
    console.log(`[PDF-CORRECTION] Cargando contexto RAG: ${contextId}`);
    
    // Buscar documentos relevantes en el contexto RAG
    const ragResults = await searchDocuments({
      query: `errores pliegos validación tags SAP campos variables ${textForAnalysis.substring(0, 500)}`,
      contextId: contextId,
      limit: 10
    });
    
    if (ragResults.success && ragResults.results.length > 0) {
      ragContext = ragResults.results
        .map(result => `DOCUMENTO: ${result.metadata?.filename || 'Sin nombre'}
CONTENIDO: ${result.content}
RELEVANCIA: ${result.score}
---`)
        .join('\n');
      
      console.log(`[PDF-CORRECTION] Contexto RAG cargado: ${ragResults.results.length} documentos relevantes`);
    }
  } catch (error) {
    console.warn(`[PDF-CORRECTION] Error cargando contexto RAG: ${error.message}`);
  }
}
```

### 5. Construcción del Prompt Completo
```javascript
async function buildValidationPrompt(textForAnalysis, prompts, ragContext = '') {
  return `SISTEMA DE VALIDACIÓN DE PLIEGOS SAP
  
CONTEXTO DE VALIDACIÓN:
${prompts.validationSystem}

ERRORES COMUNES A DETECTAR:
${prompts.erroresComunes}

EJEMPLOS DE ERRORES REALES (ENTRENAMIENTO):
${prompts.ejemplosErrores}

NOMENCLATURA ESPERADA:
${prompts.nomenclatura}

${ragContext ? `CONTEXTO RAG ADICIONAL (EJEMPLOS Y PLANTILLAS):
${ragContext}

` : ''}// ... resto del prompt
`;
}
```

### 5. Procesamiento con IA
```javascript
const client = getAiCoreClient('gpt-4o');
const response = await client.run({
  messages: [{ role: 'user', content: correctionPrompt }]
});
```

### 6. Generación del PDF
```javascript
const newPdf = await PDFDocument.create();
await addValidationReportPages(newPdf, correctionsList);
const finalPdfBytes = await newPdf.save();
```

## 🏷️ Patrones SAP Específicos Detectados

### Tags SAP Sin Reemplazar
El sistema detecta automáticamente estos patrones problemáticos:

#### **Descripciones de Contratos:**
- ❌ `CONTRACTE D'EXECUCIÓ DE LES OBRES DE (Descripció llarga del Procediment d'Adjudicació)`
- ✅ `CONTRACTE D'EXECUCIÓ DE LES OBRES DEL PROJECTE CONSTRUCTIU...`

#### **Cláusulas de Expediente:**
- ❌ `CLAU: (Clau - Expedient Descripció Abreujada)`
- ✅ `CLAU: R0-21223` o `CLAU: ATL-23261`

#### **Tags ZERM de Lotes:**
- ❌ `[ZERM_QUADRE_LOTS-Z_DESCRIP_LOT]`
- ✅ `Vilafranca del Penedès – La Granada`
- ❌ `[ZERM_QUADRE_LOTS-ZZ_NUM_LOT]`
- ✅ `1, 2, 3, 4...`

#### **Información Presupuestaria:**
- ❌ `Partida pressupostària: (Partida pressupostària)`
- ✅ `Partida pressupostària: D/660.0002 – Programa 521`

#### **Garantías y Condiciones:**
- ❌ `GARANTIA PROVISIONAL: (Si no garantia provisional)`
- ✅ `GARANTIA PROVISIONAL: NO`
- ❌ `GARANTIA DEFINITIVA: (Percentatge aval definitiu)%`
- ✅ `GARANTIA DEFINITIVA: 5% del preu d'adjudicació`

### Patrones de Detección Automática
```regex
/\([^)]*Descripció[^)]*\)/g     # Descripciones entre paréntesis
/\(Clau - Expedient[^)]*\)/g    # Cláusulas de expediente
/\[ZERM_[A-Z_-]+\]/g            # Tags ZERM
/\(Capçalera:[^)]*\)/g          # Cabeceras con logos
/\(Si no garantia[^)]*\)/g      # Condiciones de garantía
/\(Percentatge aval[^)]*\)/g    # Porcentajes de aval
/CLAU:\s*$/gm                   # Cláusulas vacías
```

## ⚙️ Configuración

### Variables de Entorno Requeridas
- **SAP AI Core**: Configuración en `auth/aiCoreClient.js`
- **Modelo**: `gpt-4o` por defecto

### Límites del Sistema
- **Tamaño máximo PDF**: 50MB
- **Texto para análisis**: 50,000 caracteres (se trunca automáticamente)
- **Timeout**: 10 segundos por request
- **Prompts**: Sin limitación de tamaño (contenido completo)
- **Contexto RAG**: Hasta 10 documentos relevantes por consulta

## 🛠️ Mantenimiento

### Actualizar Prompts
1. Modifica los archivos en `prompts_dev/`
2. El sistema carga automáticamente los cambios
3. No requiere reinicio del servidor

### Logs de Depuración
```javascript
console.log(`[PDF-CORRECTION] Generando correcciones con SAP AI Core (${correctionPrompt.length} caracteres)...`);
```

### Manejo de Errores
- **Fallback automático** si SAP AI Core falla
- **Limpieza de archivos temporales**
- **Logs detallados** para depuración

## 📊 Métricas de Respuesta

```javascript
return {
  success: true,
  pdfBuffer,
  correctionsList,
  totalPageCount: newPdf.getPageCount(),
  metadata: {
    processedAt: new Date().toISOString(),
    originalTextLength: originalText.length,
    correctionsLength: correctionsList.length
  }
};
```

## 🔍 Casos de Uso

### Validación Previa a Generación
- **Detectar tags SAP sin reemplazar** antes de enviar a producción
- **Identificar campos variables vacíos** como `CLAU:` sin contenido
- **Validar estructura** según plantillas de referencia
- **Verificar nomenclatura** correcta de expedientes

### Control de Calidad con Contexto RAG
- **Comparar con plantillas** del contexto `PLANTILLAS_BASE`
- **Verificar contra ejemplos correctos** del contexto `PLIEGOS_TERMINADOS_ENTRENAMIENTO`
- **Detectar patrones de error** documentados en `DOCUMENTOS_VALIDACION`
- **Sugerir correcciones específicas** basadas en ejemplos reales

### Entrenamiento Continuo de IA
- **Documentar nuevos errores** encontrados en pliegos
- **Añadir ejemplos** al contexto `DOCUMENTOS_VALIDACION`
- **Refinar patrones** de detección automática
- **Mejorar precisión** de validación con cada caso

### Documentación de Errores Específicos
- **Generar informes detallados** con ubicación exacta de errores
- **Crear registro** de problemas por tipo de pliego
- **Facilitar corrección** con sugerencias específicas
- **Mantener historial** de errores comunes por proyecto

---

## 🚀 **FASE 1 IMPLEMENTADA - SISTEMA COMPLETO**

### ✅ **1. Dashboard de Métricas de Validación**

**Endpoint:** `GET /api/analytics/dashboard-summary`

**Características:**
- 📊 **Métricas en tiempo real** de validaciones realizadas
- 📈 **Gráficos de tendencias** semanales
- 🔴 **Top 5 errores** más comunes
- 🏗️ **Distribución por tipos** de pliego
- 🎯 **Tasa de mejora** de la IA

**Interfaz Visual:** Dashboard React completo en `/analytics`

### ✅ **2. Auto-Clasificación de Pliegos**

**Endpoint:** `POST /api/analytics/classify`

**Funcionalidades:**
- 🔍 **Detección automática** de tipo: `obra_civil`, `edificacio`
- 📋 **Clasificación de modalidad**: `obert`, `simplificat`
- 🎯 **Confianza de clasificación** (0-100%)
- 📝 **Nombres de archivo sugeridos**
- 🎪 **Contexto RAG recomendado**

### ✅ **3. Comparador con Plantillas**

**Endpoint:** `POST /api/analytics/compare-template`

**Características:**
- 🔄 **Comparación estructural** con plantillas
- 📏 **Puntuación de similitud** (0-100%)
- ⚠️ **Detección de secciones faltantes**
- ✅ **Recomendaciones de corrección**
- 📋 **Validación contra estándares**

---

## 🔐 **SISTEMA DE AUTENTICACIÓN SIMPLE**

### **Login Directo**
- **Endpoint:** `POST /oauth/token`
- **Autenticación:** Basic Auth (usuario:contraseña)
- **Respuesta:** Token Bearer JWT
- **Validez:** 12 horas

### **Credenciales de Prueba:**
```javascript
Usuario: admin
Contraseña: admin123
```

### **Ejemplo Postman:**
```
Method: POST
URL: http://localhost:4000/oauth/token
Authorization: Basic Auth
  Username: admin
  Password: admin123
Body: x-www-form-urlencoded
  grant_type: password
```

### **Respuesta Exitosa:**
```json
{
  "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "token_type": "bearer",
  "expires_in": 43199,
  "scope": "uaa.resource",
  "jti": "abc123..."
}
```

### **Uso del Token:**
```javascript
// En headers de todas las llamadas API
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

---

## 🌐 **FRONTEND CON AUTENTICACIÓN**

### **Flujo de Login:**
1. **Formulario simple** usuario/contraseña
2. **Validación automática** contra `/oauth/token`
3. **Almacenamiento seguro** del token JWT
4. **Acceso completo** a todas las funcionalidades

### **Persistencia de Sesión:**
- **Token almacenado** en localStorage
- **Auto-login** en recargas de página
- **Logout completo** con limpieza de datos

---

---

## 📊 **MÉTRICAS AUTOMÁTICAS**

El sistema registra automáticamente métricas en cada validación:

```javascript
recordValidationMetrics({
  pdfPath: 'pliego.pdf',
  errorsFound: ['TAG_SAP_SIN_REEMPLAZAR', 'CLAU_VACIA'],
  contextId: 'DOCUMENTOS_VALIDACION',
  processingTime: 1250, // ms
  pliegoType: 'obra_civil',
  pliegoModality: 'obert'
});
```

**Métricas Disponibles:**
- 📈 **Tendencias** de error por semana
- 🎯 **Tasa de detección** de problemas
- ⏱️ **Tiempo promedio** de procesamiento
- 📋 **Distribución** por tipo de pliego
- 🏆 **Mejora continua** de precisión IA
