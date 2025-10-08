# 📮 Postman Collections

Colecciones de Postman para testear la API RAG.

## Archivos Incluidos

- **`RAG_API_Postman_Collection.json`** - Colección completa con todos los endpoints

## Cómo Usar

1. **Importar en Postman**:
   - File → Import
   - Seleccionar `RAG_API_Postman_Collection.json`

2. **Configurar Variables**:
   - `BASE_URL`: URL de tu aplicación
   - `DOCUMENT_ID`: ID de documento para tests específicos

3. **Ejecutar Tests**:
   - Individualmente o toda la colección
   - Runner para tests automáticos

## Categorías Incluidas

- 🏥 **Health Checks** - Verificar estado del sistema
- 📄 **Document Management** - Gestión de documentos
- 🔍 **Search & Retrieval** - Búsquedas semánticas
- 💬 **RAG Chat** - Chat conversacional
- 🧹 **Administration** - Funciones administrativas
- 🧪 **Testing & Validation** - Tests de validación

## Variables de Entorno

```
BASE_URL: https://tu-app.cfapps.sap.hana.ondemand.com
DOCUMENT_ID: doc_ejemplo_123
```
