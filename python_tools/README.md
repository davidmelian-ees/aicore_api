# 🚀 Herramientas Python para Subida de Contextos RAG

Este directorio contiene herramientas para subir automáticamente archivos de contexto al sistema RAG.

## 📁 Estructura del Proyecto

```
aicore_api/
├── venv_upload/              # Entorno virtual Python
├── FOR_CONTEXT/              # Carpeta con archivos a subir
├── python_tools/             # Herramientas Python
│   ├── requirements.txt      # Dependencias Python
│   ├── upload_context_files.py  # Script principal
│   ├── setup_env.bat         # Configurar entorno
│   ├── upload_to_cloud.bat   # Subir a Cloud Foundry
│   └── README.md             # Esta documentación
└── ...
```

## 🚀 Uso Rápido

### 1. Primera vez (configuración):
```bash
# Doble clic en:
setup_env.bat
```

### 2. Subir archivos:
```bash
# 1. Coloca tus archivos en ../FOR_CONTEXT/
# 2. Doble clic en:
upload_to_cloud.bat
```

## 📋 Pasos Detallados

### Paso 1: Configurar Entorno
1. Ejecutar `setup_env.bat`
2. Se activará el entorno virtual
3. Se instalarán las dependencias automáticamente

### Paso 2: Preparar Archivos
1. Colocar archivos en la carpeta `../FOR_CONTEXT/`
2. Tipos soportados: `.txt`, `.pdf`, `.docx`, `.md`, `.json`, `.csv`, `.xlsx`, `.xls`

### Paso 3: Subir Archivos
1. Ejecutar `upload_to_cloud.bat`
2. Seleccionar opción del menú
3. Los archivos se suben automáticamente a Cloud Foundry

## 🎯 Asignación Automática de Contextos

El script asigna automáticamente contextos basándose en el nombre del archivo:

| Patrón en Nombre | Contexto Asignado |
|------------------|-------------------|
| `errores_comunes` | `ERRORES_COMUNES` |
| `nomenclatura` | `NOMENCLATURA` |
| `training` | `TRAINING_STRATEGY` |
| `cronograma` | `CRONOGRAMA` |
| `analisis` | `ANALISIS_GENERADOS` |
| `contextos` | `CONTEXTOS_RAG` |
| `plantilla_tags` | `PLANTILLAS_TAGS` |
| `plantilla` | `PLANTILLAS_BASE` |
| `generado` | `PLIEGOS_GENERADOS_*` |
| Otros | `PLIEGOS_DESARROLLO` |

## 🔧 Uso Avanzado (Línea de Comandos)

```bash
# Activar entorno virtual
call ..\venv_upload\Scripts\activate.bat

# Subir a Cloud Foundry (por defecto)
python upload_context_files.py --folder ..\FOR_CONTEXT

# Solo mostrar archivos (no subir)
python upload_context_files.py --folder ..\FOR_CONTEXT --dry-run

# Contexto personalizado
python upload_context_files.py --folder ..\FOR_CONTEXT --context MI_CONTEXTO

# Servidor local (para pruebas)
python upload_context_files.py --url http://localhost:4000 --folder ..\FOR_CONTEXT

# Con pausa personalizada entre subidas
python upload_context_files.py --folder ..\FOR_CONTEXT --delay 2.0
```

## 📊 Ejemplo de Salida

```
🚀 Iniciando subida masiva desde: ..\FOR_CONTEXT
🎯 Contexto por defecto: PLIEGOS_DESARROLLO
👤 Subido por: script_auto
============================================================
✅ Conexión exitosa con https://ai_core_api.cfapps.eu10-005.hana.ondemand.com
📁 Encontrados 5 archivos soportados

[1/5] 📤 Subiendo: errores_comunes_pliegos.txt → Contexto: ERRORES_COMUNES
   ✅ Éxito: Documento indexado exitosamente

[2/5] 📤 Subiendo: nomenclatura_pliegos.txt → Contexto: NOMENCLATURA
   ✅ Éxito: Documento indexado exitosamente

============================================================
📊 RESUMEN DE SUBIDA
============================================================
📁 Total archivos encontrados: 5
✅ Subidos exitosamente: 5
❌ Errores: 0
⏱️  Tiempo total: 15.2 segundos
🎯 Tasa de éxito: 100.0%
```

## 🛠️ Solución de Problemas

### Error de Conexión
- Verificar que el servidor esté funcionando
- Comprobar conectividad a internet
- Revisar URL del servidor

### Error de Archivos
- Verificar que los archivos estén en `../FOR_CONTEXT/`
- Comprobar que sean tipos soportados
- Revisar permisos de lectura

### Error de Python
- Verificar que Python esté instalado
- Ejecutar `setup_env.bat` para configurar entorno
- Instalar dependencias manualmente: `pip install -r requirements.txt`

## 📝 Notas

- Los archivos se suben a Cloud Foundry por defecto
- La asignación de contextos es automática e inteligente
- Se incluyen estadísticas detalladas de cada subida
- Hay pausa de 1 segundo entre subidas para no sobrecargar el servidor
- El script es seguro y maneja errores automáticamente
