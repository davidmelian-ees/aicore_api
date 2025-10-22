# Análisis de Archivos .BAT del Proyecto

## 📊 Estado Actual: 12 archivos .bat

### ✅ **MANTENER** (3 archivos esenciales)

1. **`start-with-sqlite.bat`** ⭐ **PRINCIPAL**
   - **Propósito:** Iniciar servidor con SQLite Vector Store
   - **Uso:** Desarrollo diario
   - **Estado:** ✅ Funcional y necesario

2. **`deploy-backend.bat`**
   - **Propósito:** Desplegar a Cloud Foundry
   - **Uso:** Producción
   - **Estado:** ✅ Funcional y necesario

3. **`cleanup-project.bat`**
   - **Propósito:** Limpieza general del proyecto
   - **Uso:** Mantenimiento
   - **Estado:** ✅ Funcional y útil

### ❌ **ELIMINAR** (9 archivos obsoletos/duplicados)

#### Instalación PDF (ya no necesarios):
4. **`install-pdf-support.bat`**
   - **Problema:** Dependencias ya instaladas en package.json
   - **Estado:** ❌ Obsoleto

5. **`install-pdfjs.bat`**
   - **Problema:** Duplicado de install-pdf-support.bat
   - **Estado:** ❌ Obsoleto

6. **`setup-pdf-support.bat`**
   - **Problema:** Setup ya completado
   - **Estado:** ❌ Obsoleto

#### Pruebas y Debug (temporales):
7. **`test-cors.bat`**
   - **Problema:** Prueba temporal, CORS ya funciona
   - **Estado:** ❌ Obsoleto

8. **`diagnose-cors.bat`**
   - **Problema:** Debug temporal
   - **Estado:** ❌ Obsoleto

9. **`check-server.bat`**
   - **Problema:** Debug temporal
   - **Estado:** ❌ Obsoleto

10. **`test-pliego.bat`**
    - **Problema:** Prueba específica temporal
    - **Estado:** ❌ Obsoleto

11. **`fix-pdf-test-data.bat`**
    - **Problema:** Fix temporal ya aplicado
    - **Estado:** ❌ Obsoleto

#### Duplicados:
12. **`start-backend-local.bat`**
    - **Problema:** Hace lo mismo que start-with-sqlite.bat
    - **Estado:** ❌ Duplicado

## 🎯 Resultado Final

### Después de la limpieza: **3 archivos .bat**

```
Proyecto/
├── start-with-sqlite.bat    ← USAR ESTE para desarrollo
├── deploy-backend.bat       ← Para desplegar a CF
└── cleanup-project.bat      ← Para limpieza general
```

## 📋 Comandos de Uso

### Desarrollo Local:
```cmd
start-with-sqlite.bat
```

### Despliegue a Producción:
```cmd
deploy-backend.bat
```

### Limpieza del Proyecto:
```cmd
cleanup-project.bat
```

## 🔧 Script de Limpieza

Ejecuta este comando para limpiar automáticamente:
```cmd
cleanup-bat-files.bat
```

## ✨ Beneficios de la Limpieza

### Antes (12 archivos):
- ❌ Confusión sobre cuál usar
- ❌ Archivos duplicados
- ❌ Scripts obsoletos
- ❌ Dependencias ya resueltas

### Después (3 archivos):
- ✅ Claro qué archivo usar para cada propósito
- ✅ Sin duplicados
- ✅ Solo scripts funcionales
- ✅ Proyecto organizado

## 🚀 Recomendación

**Ejecuta `cleanup-bat-files.bat` para limpiar automáticamente todos los archivos obsoletos y mantener solo los 3 esenciales.**
