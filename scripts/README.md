# 🛠️ Scripts

Carpeta con scripts de utilidades, ingesta y configuración.

## Archivos Incluidos

- **`init-sample-data.js`** - Inicializa datos de ejemplo (usado por server.js)
- **`ingest-documents.js`** - Script para ingestar documentos desde carpeta
- **`start-cf.js`** - Script de inicio para Cloud Foundry

## Uso

### Ingestar Documentos

```bash
# Ingestar desde sample_documents (por defecto)
node scripts/ingest-documents.js

# Ingestar desde carpeta específica
node scripts/ingest-documents.js /ruta/a/documentos
```

### Inicializar Datos de Ejemplo

```bash
# Ejecutar manualmente
node scripts/init-sample-data.js
```

### Inicio Cloud Foundry

```bash
# Script de inicio para CF
node scripts/start-cf.js
```

## Tipos de Archivos Soportados

- `.txt` - Archivos de texto plano
- `.md` - Archivos Markdown
- `.json` - Archivos JSON
- `.csv` - Archivos CSV
- `.docx` - Documentos Word
