# 📮 Ejemplo de Postman - Procesador de Pliegos

## ⚠️ IMPORTANTE: Instalación Requerida

Antes de usar el endpoint, debes instalar el soporte para PDFs:

```bash
# Opción 1: Script automático
setup-pdf-support.bat

# Opción 2: Manual
npm install pdfjs-dist@4.0.379
npm start
```

## Configuración en Postman

### 1. Crear Nueva Request

1. **Método**: `POST`
2. **URL**: `http://localhost:4000/api/rag/process-pliego`
3. **Tipo**: Form-data

### 2. Configurar Headers (Automático)

Postman configurará automáticamente los headers necesarios para `multipart/form-data`:
```
Content-Type: multipart/form-data; boundary=----WebKitFormBoundary...
```

### 3. Configurar Body (Form-data)

| Key | Type | Value | Description |
|-----|------|-------|-------------|
| `pliego` | File | [Seleccionar archivo PDF] | Archivo PDF del pliego |
| `prompt` | Text | Ver ejemplos abajo | Prompt personalizado |

## Ejemplos de Prompts

### Ejemplo 1: Análisis General
```
Analiza este pliego de manera general, identificando:
- Objeto del contrato
- Principales requisitos
- Aspectos más importantes
- Puntos críticos a considerar

Proporciona un resumen estructurado y claro.
```

### Ejemplo 2: Requisitos Técnicos
```
Extrae y organiza todos los requisitos técnicos mencionados en este pliego:

1. TECNOLOGÍAS REQUERIDAS:
   - Lenguajes de programación
   - Frameworks y librerías
   - Bases de datos

2. ESPECIFICACIONES TÉCNICAS:
   - Hardware mínimo
   - Software necesario
   - Estándares y normativas

3. CERTIFICACIONES:
   - Certificaciones requeridas
   - Documentación técnica necesaria

Organiza la información de forma clara y detallada.
```

### Ejemplo 3: Presupuesto y Plazos
```
Identifica y extrae toda la información económica y temporal del pliego:

INFORMACIÓN ECONÓMICA:
- Presupuesto máximo
- Forma de pago
- Garantías económicas requeridas
- Penalizaciones por incumplimiento

INFORMACIÓN TEMPORAL:
- Plazo de ejecución
- Hitos y entregas
- Fechas límite importantes
- Cronograma detallado

Presenta la información de forma organizada y precisa.
```

### Ejemplo 4: Criterios de Evaluación
```
Analiza los criterios de evaluación y adjudicación del pliego:

1. FACTORES DE EVALUACIÓN:
   - Criterios técnicos (peso y descripción)
   - Criterios económicos (peso y descripción)
   - Otros criterios (experiencia, referencias, etc.)

2. PROCESO DE SELECCIÓN:
   - Documentación requerida
   - Fases del proceso
   - Métodos de puntuación

3. REQUISITOS DE PARTICIPACIÓN:
   - Requisitos mínimos
   - Documentación obligatoria
   - Exclusiones

Proporciona un análisis detallado y estructurado.
```

## Respuesta Esperada

```json
{
  "success": true,
  "message": "Pliego procesado exitosamente",
  "analysis": "ANÁLISIS DEL PLIEGO\n\n1. OBJETO DEL CONTRATO\nEl presente pliego tiene por objeto...\n\n2. REQUISITOS TÉCNICOS\n- Lenguaje de programación: JavaScript/Node.js\n- Base de datos: PostgreSQL o MongoDB\n...",
  "metadata": {
    "fileName": "pliego-ejemplo.pdf",
    "fileSize": 245760,
    "processedAt": "2024-10-10T08:33:45.123Z",
    "customPrompt": "Analiza este pliego de manera general...",
    "contentLength": 1250,
    "chunksProcessed": 3,
    "model": "gpt-4o"
  }
}
```

## Pasos Detallados en Postman

### Paso 1: Configurar Request
1. Crear nueva request
2. Seleccionar método `POST`
3. Introducir URL: `http://localhost:4000/api/rag/process-pliego`

### Paso 2: Configurar Body
1. Ir a la pestaña **Body**
2. Seleccionar **form-data**
3. Añadir campos:
   - Key: `pliego`, Type: `File`, Value: [Seleccionar PDF]
   - Key: `prompt`, Type: `Text`, Value: [Copiar uno de los ejemplos]

### Paso 3: Enviar Request
1. Hacer clic en **Send**
2. Verificar que el status sea `200 OK`
3. Revisar la respuesta JSON

## Archivo PDF de Prueba

Si no tienes un pliego real, puedes crear un archivo de texto con este contenido y guardarlo como PDF:

```
PLIEGO DE CONDICIONES TÉCNICAS
DESARROLLO DE APLICACIÓN WEB

1. OBJETO DEL CONTRATO
Desarrollo de una aplicación web para gestión de documentos con las siguientes características:
- Sistema de autenticación de usuarios
- Gestión de documentos PDF
- Búsqueda avanzada
- Interfaz responsive

2. REQUISITOS TÉCNICOS
- Frontend: React.js o Vue.js
- Backend: Node.js con Express
- Base de datos: PostgreSQL o MongoDB
- Autenticación: JWT o OAuth2
- Hosting: Cloud compatible (AWS, Azure, GCP)

3. PRESUPUESTO
Presupuesto máximo: 45.000€ (IVA incluido)
Forma de pago: 30% inicio, 40% entrega beta, 30% entrega final

4. PLAZO DE EJECUCIÓN
Plazo máximo: 4 meses desde la firma del contrato
Hitos:
- Mes 1: Análisis y diseño
- Mes 2: Desarrollo frontend
- Mes 3: Desarrollo backend
- Mes 4: Testing y despliegue

5. CRITERIOS DE EVALUACIÓN
- Propuesta técnica: 50%
- Propuesta económica: 30%
- Experiencia del equipo: 20%

6. DOCUMENTACIÓN REQUERIDA
- Memoria técnica detallada
- Propuesta económica
- Referencias de proyectos similares
- CV del equipo técnico
```

## Troubleshooting

### Error 400: "No se proporcionó ningún archivo PDF"
- Verificar que el campo se llame exactamente `pliego`
- Asegurar que el archivo sea tipo PDF
- Comprobar que el archivo no esté corrupto

### Error 400: "El prompt personalizado es requerido"
- Verificar que el campo se llame exactamente `prompt`
- Asegurar que el prompt no esté vacío
- El prompt debe ser texto plano

### Error 500: Error interno
- Verificar que el backend esté corriendo
- Comprobar logs del servidor
- Verificar conectividad con SAP AI Core

### Error de CORS
- Si usas Postman, no debería haber problemas de CORS
- Si persiste, verificar configuración del servidor

## Colección de Postman

Puedes importar esta configuración JSON en Postman:

```json
{
  "info": {
    "name": "RAG Pliego Processor",
    "description": "Colección para probar el procesador de pliegos"
  },
  "item": [
    {
      "name": "Process Pliego - Análisis General",
      "request": {
        "method": "POST",
        "header": [],
        "body": {
          "mode": "formdata",
          "formdata": [
            {
              "key": "pliego",
              "type": "file",
              "src": []
            },
            {
              "key": "prompt",
              "value": "Analiza este pliego de manera general, identificando los puntos clave, requisitos principales y aspectos más importantes.",
              "type": "text"
            }
          ]
        },
        "url": {
          "raw": "http://localhost:4000/api/rag/process-pliego",
          "protocol": "http",
          "host": ["localhost"],
          "port": "4000",
          "path": ["api", "rag", "process-pliego"]
        }
      }
    }
  ]
}
```

## Variables de Entorno

Para facilitar el testing, puedes configurar estas variables en Postman:

- `base_url`: `http://localhost:4000`
- `api_path`: `/api/rag/process-pliego`

Entonces la URL sería: `{{base_url}}{{api_path}}`
