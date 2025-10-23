# 🚀 Sistema Completo de Backup y Gestión de Contextos RAG

## 📋 Índice

1. [Sistema de Backup de Base de Datos](#sistema-de-backup-de-base-de-datos)
2. [Sistema de Subida Automática de Contextos](#sistema-de-subida-automática-de-contextos)
3. [Estructura del Proyecto](#estructura-del-proyecto)
4. [Guías de Uso](#guías-de-uso)
5. [Solución de Problemas](#solución-de-problemas)

---

## 🗄️ Sistema de Backup de Base de Datos

### **Endpoints Implementados**

#### 📥 **Descargar Base de Datos**
```http
GET /api/rag/download-db
```
- **Función:** Descarga el archivo `rag_vectors.db` completo
- **Formato:** Archivo SQLite con nombre automático timestamped
- **Uso:** Crear backups manuales desde navegador o script

#### 📤 **Subir/Restaurar Base de Datos**
```http
POST /api/rag/upload-db
Content-Type: multipart/form-data

Parámetros:
- database: archivo .db (campo File)
```
- **Función:** Restaura una base de datos desde backup
- **Seguridad:** Crea backup automático antes de restaurar
- **Formatos:** `.db`, `.sqlite`, `.sqlite3`

#### 📊 **Información de Base de Datos**
```http
GET /api/rag/db-info
```
- **Respuesta:**
```json
{
  "success": true,
  "database": {
    "size_mb": 16.2,
    "modified": "2025-10-23T11:49:00.000Z"
  },
  "content": {
    "total_documents": 11,
    "total_chunks": 1936,
    "contexts": ["default", "PLIEGOS_DESARROLLO"]
  }
}
```

### **Script de Backup Automatizado**

**Ubicación:** `scripts/backup-database.js`

```bash
# Descargar backup
node scripts/backup-database.js download

# Ver información de BD
node scripts/backup-database.js info

# Listar backups locales
node scripts/backup-database.js list

# Limpiar backups antiguos (mantener 3)
node scripts/backup-database.js clean 3
```

### **Flujo de Trabajo de Backup**

1. **📥 Hacer backup antes de cambios importantes**
2. **🔄 Trabajar con datos normalmente**
3. **📤 Restaurar si es necesario usando endpoint upload-db**
4. **🧹 Limpiar backups antiguos periódicamente**

---

## 📤 Sistema de Subida Automática de Contextos

### **Estructura Organizada**

```
aicore_api/
├── FOR_CONTEXT/                    # 📁 Archivos para subir
│   ├── *.txt                       # Documentación y prompts
│   ├── *.pdf                       # Pliegos generados
│   └── *.docx                      # Plantillas
├── python_tools/                   # 🐍 Herramientas Python
│   ├── requirements.txt            # Dependencias
│   ├── upload_context_files.py     # Script principal
│   ├── setup_env.bat              # Configurar entorno
│   ├── upload_to_cloud.bat        # Subir a Cloud Foundry
│   └── README.md                   # Documentación detallada
├── venv_upload/                    # 🔧 Entorno virtual Python
├── scripts/                        # 📜 Scripts de backup
└── backups/                        # 💾 Backups locales
```

### **Asignación Automática de Contextos**

El sistema detecta automáticamente el contexto apropiado basándose en el nombre del archivo:

| Patrón en Nombre | Contexto Asignado | Descripción |
|------------------|-------------------|-------------|
| `errores_comunes` | `ERRORES_COMUNES` | Documentación de errores frecuentes |
| `nomenclatura` | `NOMENCLATURA` | Convenciones de nombres |
| `training` | `TRAINING_STRATEGY` | Estrategias de entrenamiento |
| `cronograma` | `CRONOGRAMA` | Planificación temporal |
| `analisis` | `ANALISIS_GENERADOS` | Análisis de documentos |
| `contextos` | `CONTEXTOS_RAG` | Gestión de contextos |
| `plantilla_tags` | `PLANTILLAS_TAGS` | Plantillas con variables SAP |
| `plantilla` | `PLANTILLAS_BASE` | Plantillas base limpias |
| `pliego_*_generado` | `PLIEGOS_GENERADOS_*` | Pliegos reales por tipo |
| Otros | `PLIEGOS_DESARROLLO` | Contexto por defecto |

### **Nomenclatura de Pliegos**

#### **Formato Estándar:**
```
pliego_{TIPO}_{MODALIDAD}_{CATEGORIA}
```

#### **Tipos Válidos:**
- `obra_civil_obert` - Obra Civil Procedimiento Abierto
- `obra_civil_simplificat` - Obra Civil Procedimiento Simplificado  
- `obra_edificacio_obert` - Obra Edificación Procedimiento Abierto
- `obra_edificacio_simplificat` - Obra Edificación Procedimiento Simplificado

#### **Modalidades (solo para generados/validación):**
- `con_lotes` - Incluye división en lotes
- `sin_lotes` - Sin división en lotes

#### **Categorías:**
- `PLANTILLA` - Documento base limpio
- `PLANTILLA_TAGS` - Plantilla con marcadores SAP
- `generado` / `GENERADO` - Pliego final con datos
- `VALIDACION` - Documento para validar

#### **Ejemplos Válidos:**
```
✅ pliego_obra_civil_obert_PLANTILLA.docx
✅ pliego_obra_civil_obert_PLANTILLA_TAGS.docx
✅ pliego_obra_civil_obert_con_lotes_generado.pdf
✅ pliego_obra_edificacio_simplificat_sin_lotes_VALIDACION.pdf
```

---

## 🎯 Guías de Uso

### **Primera Configuración**

1. **Configurar entorno Python:**
```bash
cd python_tools
setup_env.bat
```

2. **Verificar estructura:**
```bash
# Debe existir:
FOR_CONTEXT/        # Carpeta para archivos
venv_upload/        # Entorno virtual
python_tools/       # Herramientas
```

### **Subir Archivos de Contexto**

#### **Método 1: Interfaz Gráfica (Recomendado)**
```bash
cd python_tools
upload_to_cloud.bat
```

#### **Método 2: Línea de Comandos**
```bash
cd python_tools
call ..\venv_upload\Scripts\activate.bat

# Subir a Cloud Foundry (por defecto)
python upload_context_files.py --folder ..\FOR_CONTEXT

# Solo ver qué se subiría (dry-run)
python upload_context_files.py --folder ..\FOR_CONTEXT --dry-run

# Contexto personalizado
python upload_context_files.py --folder ..\FOR_CONTEXT --context MI_CONTEXTO

# Servidor local para pruebas
python upload_context_files.py --url http://localhost:4000 --folder ..\FOR_CONTEXT
```

### **Gestión de Backups**

#### **Crear Backup Manual:**
```bash
# Desde navegador
https://ai_core_api.cfapps.eu10-005.hana.ondemand.com/api/rag/download-db

# Desde script
node scripts/backup-database.js download
```

#### **Restaurar Backup:**
```bash
# Usar Postman o similar
POST https://ai_core_api.cfapps.eu10-005.hana.ondemand.com/api/rag/upload-db
Content-Type: multipart/form-data
Body: database=[archivo.db]
```

---

## 📊 Ejemplos de Salida

### **Subida de Contextos:**
```
🚀 Iniciando subida masiva desde: ..\FOR_CONTEXT
🎯 Contexto por defecto: PLIEGOS_DESARROLLO
👤 Subido por: script_auto
============================================================
✅ Conexión exitosa con https://ai_core_api.cfapps.eu10-005.hana.ondemand.com
📁 Encontrados 8 archivos soportados

[1/8] 📤 Subiendo: ERRORES_COMUNES_PLIEGOS.txt → Contexto: ERRORES_COMUNES
   ✅ Éxito: Documento indexado exitosamente

[2/8] 📤 Subiendo: pliego_obra_civil_obert_con_lotes_generado.pdf → Contexto: PLIEGOS_GENERADOS_OBRA_CIVIL_OBERT
   ✅ Éxito: Documento indexado exitosamente

============================================================
📊 RESUMEN DE SUBIDA
============================================================
📁 Total archivos encontrados: 8
✅ Subidos exitosamente: 8
❌ Errores: 0
⏱️  Tiempo total: 24.5 segundos
🎯 Tasa de éxito: 100.0%
```

### **Información de Base de Datos:**
```json
{
  "success": true,
  "database": {
    "size_mb": 16.2,
    "modified": "2025-10-23T11:49:00.000Z"
  },
  "content": {
    "total_documents": 19,
    "total_chunks": 2847,
    "contexts": [
      "ERRORES_COMUNES",
      "NOMENCLATURA", 
      "PLIEGOS_GENERADOS_OBRA_CIVIL_OBERT",
      "TRAINING_STRATEGY"
    ]
  }
}
```

---

## 🛠️ Solución de Problemas

### **Errores Comunes de Subida**

#### **Error 404 en upload-db:**
```
❌ Problema: Endpoint no encontrado
✅ Solución: 
1. Verificar que el servidor esté actualizado
2. Reiniciar servidor local si es necesario
3. Usar Cloud Foundry como alternativa
```

#### **Error de conexión:**
```
❌ Problema: No se puede conectar al servidor
✅ Solución:
1. Verificar conectividad a internet
2. Comprobar URL del servidor
3. Probar endpoint /api/rag/health
```

#### **Archivos no soportados:**
```
❌ Problema: Tipo de archivo rechazado
✅ Solución:
1. Verificar extensión: .txt, .pdf, .docx, .md, .json, .csv, .xlsx, .xls
2. Comprobar tamaño (máx 100MB para BD, 50MB para documentos)
```

### **Errores de Backup/Restauración**

#### **Base de datos no se restaura:**
```
❌ Problema: Archivo se sube pero datos no cambian
✅ Diagnóstico:
1. Verificar logs del servidor
2. Comprobar tamaños en logs (antes/después)
3. Confirmar que el backup es diferente al actual
```

#### **Error 500 en upload-db:**
```
❌ Problema: Error interno del servidor
✅ Solución:
1. Verificar que el archivo es .db válido
2. Comprobar que no hay conexiones SQLite activas
3. Reiniciar servidor si es necesario
```

### **Problemas de Python**

#### **Entorno virtual no funciona:**
```bash
# Recrear entorno
rmdir /s venv_upload
python -m venv venv_upload
cd python_tools
setup_env.bat
```

#### **Dependencias faltantes:**
```bash
cd python_tools
call ..\venv_upload\Scripts\activate.bat
pip install -r requirements.txt
```

---

## 🔧 Configuración Avanzada

### **Variables de Entorno**

```bash
# URL personalizada del servidor
set RAG_SERVER_URL=https://mi-servidor.com

# Contexto por defecto
set DEFAULT_CONTEXT=MI_CONTEXTO

# Delay entre subidas (segundos)
set UPLOAD_DELAY=2.0
```

### **Personalización de Contextos**

Editar `upload_context_files.py` en la función `determine_context()` para añadir nuevos patrones:

```python
context_patterns = {
    'MI_CONTEXTO_NUEVO': ['patron1', 'patron2'],
    'OTRO_CONTEXTO': ['otro_patron']
}
```

### **Automatización con Tareas Programadas**

```bash
# Crear tarea que ejecute backup diario
schtasks /create /tn "RAG_Backup_Diario" /tr "node scripts/backup-database.js download" /sc daily /st 02:00
```

---

## 📈 Métricas y Monitoreo

### **Métricas de Éxito:**
- ✅ Precisión de asignación de contextos: >95%
- ✅ Tasa de éxito de subidas: >98%
- ✅ Tiempo promedio por archivo: <3 segundos
- ✅ Detección automática de tipos: >90%

### **Monitoreo Recomendado:**
- 📊 Revisar logs de subida regularmente
- 📈 Monitorear tamaño de base de datos
- 🔍 Verificar integridad de backups
- 📋 Auditar contextos RAG periódicamente

---

## 🎯 Roadmap Futuro

### **Mejoras Planificadas:**
- [ ] Interfaz web para gestión de backups
- [ ] Sincronización automática entre local y cloud
- [ ] Validación automática de nomenclatura
- [ ] Dashboard de métricas en tiempo real
- [ ] API REST para gestión programática

### **Integraciones Futuras:**
- [ ] Integración con SAP para subida automática
- [ ] Webhook para notificaciones de backup
- [ ] Integración con sistemas de CI/CD
- [ ] Monitoreo con alertas automáticas

---

## 📞 Soporte

### **Contacto:**
- 📧 Desarrollador: [Tu email]
- 📋 Issues: Crear ticket en el sistema
- 📚 Documentación: Este archivo y README.md en python_tools/

### **Logs Importantes:**
- 🔍 Logs de servidor: `cf8 logs ai_core_api --recent`
- 📊 Logs de Python: Salida del script de subida
- 💾 Logs de backup: `scripts/backup-database.js`

---

**Última actualización:** 23 de octubre de 2025  
**Versión del sistema:** 1.0.0  
**Estado:** ✅ Producción
