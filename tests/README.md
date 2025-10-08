# 🧪 Tests

Carpeta con todos los scripts de testing del sistema RAG.

## Archivos Incluidos

- **`test.js`** - Tests generales del sistema
- **`test_simple.js`** - Tests básicos simplificados
- **`test_service.js`** - Tests de servicios específicos
- **`test_rag.js`** - Tests del sistema RAG completo
- **`test_python_chroma.js`** - Tests de integración ChromaDB Python

## Cómo Ejecutar

```bash
# Test completo del sistema
node tests/test_python_chroma.js

# Test específico
node tests/test_rag.js

# Desde la raíz del proyecto
npm test
```

## Prerequisitos

- Servicios RAG iniciados
- ChromaDB corriendo (para tests locales)
- Variables de entorno configuradas
