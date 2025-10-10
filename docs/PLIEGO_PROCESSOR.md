# 📄 Procesador de Pliegos

## Descripción

El **Procesador de Pliegos** es una nueva funcionalidad que permite analizar documentos PDF (pliegos) usando prompts personalizados sin almacenarlos permanentemente en el sistema RAG.

## Características

- ✅ **Procesamiento temporal**: Los PDFs se procesan pero no se almacenan
- ✅ **Prompts personalizados**: Define exactamente qué análisis quieres
- ✅ **Prompts predefinidos**: Plantillas comunes para análisis rápido
- ✅ **Integración SAP AI Core**: Usa GPT-4o para análisis avanzado
- ✅ **Interfaz web intuitiva**: Componente React integrado en el dashboard

## Endpoint API

### `POST /api/rag/process-pliego`

Procesa un pliego PDF con un prompt personalizado.

**Parámetros:**
- `pliego` (file): Archivo PDF del pliego
- `prompt` (string): Prompt personalizado para el análisis

**Respuesta:**
```json
{
  "success": true,
  "message": "Pliego procesado exitosamente",
  "analysis": "Análisis detallado del pliego...",
  "metadata": {
    "fileName": "pliego.pdf",
    "fileSize": 1024000,
    "processedAt": "2024-01-01T12:00:00.000Z",
    "customPrompt": "Analiza los requisitos técnicos...",
    "contentLength": 5000,
    "chunksProcessed": 10,
    "model": "gpt-4o"
  }
}
```

## Uso desde Frontend

### 1. Acceder al Procesador

1. Abre el dashboard RAG: `http://localhost:3000`
2. Haz clic en **"📄 Procesador de Pliegos"** en la navegación superior

### 2. Procesar un Pliego

1. **Seleccionar archivo**: Elige un archivo PDF
2. **Elegir prompt**: Usa uno predefinido o escribe uno personalizado
3. **Procesar**: Haz clic en "🚀 Procesar Pliego"
4. **Ver resultados**: El análisis aparecerá automáticamente

### 3. Prompts Predefinidos

- **Análisis General**: Visión general del pliego
- **Requisitos Técnicos**: Extrae especificaciones técnicas
- **Presupuesto y Costos**: Información económica
- **Plazos y Cronograma**: Aspectos temporales
- **Criterios de Evaluación**: Factores de adjudicación

## Uso desde API

### cURL
```bash
curl -X POST http://localhost:4000/api/rag/process-pliego \
  -F "pliego=@mi-pliego.pdf" \
  -F "prompt=Extrae todos los requisitos técnicos del pliego"
```

### JavaScript
```javascript
const formData = new FormData();
formData.append('pliego', pdfFile);
formData.append('prompt', 'Analiza el presupuesto y plazos');

const response = await fetch('/api/rag/process-pliego', {
  method: 'POST',
  body: formData
});

const result = await response.json();
console.log(result.analysis);
```

### Python
```python
import requests

files = {'pliego': open('pliego.pdf', 'rb')}
data = {'prompt': 'Identifica los criterios de evaluación'}

response = requests.post(
    'http://localhost:4000/api/rag/process-pliego',
    files=files,
    data=data
)

result = response.json()
print(result['analysis'])
```

## Ejemplos de Prompts

### Análisis Técnico
```
Extrae y organiza todos los requisitos técnicos mencionados en este pliego:
- Tecnologías requeridas
- Especificaciones de hardware/software
- Estándares y normativas
- Certificaciones necesarias
```

### Análisis Económico
```
Identifica toda la información económica del pliego:
- Presupuesto máximo
- Forma de pago
- Garantías económicas
- Penalizaciones por incumplimiento
```

### Análisis de Plazos
```
Extrae todos los aspectos temporales:
- Plazo de ejecución
- Hitos importantes
- Fechas límite
- Cronograma de entregas
```

### Análisis de Evaluación
```
Identifica los criterios de evaluación y adjudicación:
- Factores de puntuación
- Pesos relativos
- Documentación requerida
- Proceso de selección
```

## Testing

### Script de Prueba
```bash
# Ejecutar tests automáticos
cd aicore_api
node test-pliego-endpoint.js

# O usar el script batch
test-pliego.bat
```

### Verificación Manual
1. Usar Postman o similar
2. Endpoint: `POST http://localhost:4000/api/rag/process-pliego`
3. Adjuntar PDF y prompt
4. Verificar respuesta

## Configuración

### Variables de Entorno
- `NODE_ENV`: Entorno de ejecución
- `VECTOR_STORE_TYPE`: Tipo de almacén vectorial (no afecta al procesador)

### Límites
- **Tamaño máximo**: 100MB por archivo PDF
- **Timeout**: 30 segundos por procesamiento
- **Modelo**: GPT-4o (configurable)

## Arquitectura

```
Frontend (React)
    ↓
PliegoProcessor.jsx
    ↓
axiosService.js
    ↓
/api/rag/process-pliego
    ↓
ragService.processPliegoWithPrompt()
    ↓
documentProcessor.js → SAP AI Core
```

## Diferencias con RAG Normal

| Aspecto | RAG Normal | Procesador Pliegos |
|---------|------------|-------------------|
| **Almacenamiento** | Permanente | Temporal |
| **Contexto** | Múltiples documentos | Un solo documento |
| **Prompt** | Fijo/conversacional | Personalizable |
| **Uso** | Chat continuo | Análisis puntual |

## Troubleshooting

### Error: "No se proporcionó ningún archivo PDF"
- Verificar que el archivo sea PDF válido
- Comprobar el nombre del campo: `pliego`

### Error: "El prompt personalizado es requerido"
- Asegurar que el prompt no esté vacío
- Verificar el campo `prompt` en la request

### Error CORS
- Ejecutar `diagnose-cors.bat` para verificar configuración
- Asegurar que el backend esté en puerto 4000

### Timeout o Error de Procesamiento
- Verificar conectividad con SAP AI Core
- Comprobar logs del servidor
- Reducir tamaño del PDF si es muy grande

## Roadmap

- [ ] Soporte para múltiples formatos (DOCX, TXT)
- [ ] Plantillas de prompts más específicas
- [ ] Exportación de análisis (PDF, Word)
- [ ] Comparación entre múltiples pliegos
- [ ] Integración con sistemas de gestión documental
