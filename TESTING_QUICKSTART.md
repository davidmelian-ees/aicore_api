# 🚀 Testing - Inicio Rápido

## ⚡ Ejecutar Tests (3 formas)

### 1️⃣ Forma Más Fácil (Windows)
```bash
.\run-tests.bat
```
- Instala dependencias si es necesario
- Ejecuta todos los tests
- Muestra reporte de cobertura
- Pregunta si abrir reporte HTML

### 2️⃣ Comando NPM
```bash
npm test
```
- Ejecuta todos los tests
- Genera reporte de cobertura
- Muestra resultados en consola

### 3️⃣ Modo Desarrollo (Watch)
```bash
npm run test:watch
```
- Re-ejecuta tests al guardar archivos
- Ideal para desarrollo
- No genera reporte de cobertura

---

## 📊 Ver Resultados

### En Consola
Después de `npm test` verás:

```
PASS  tests/services/ragService.test.js
  ✓ debe dividir texto en chunks (5ms)
  ✓ debe validar extensiones (2ms)
  
PASS  tests/utils/validation.test.js
  ✓ debe validar email (3ms)
  
Test Suites: 4 passed, 4 total
Tests:       25 passed, 25 total
Coverage:    65.4% Lines | 58.2% Branches
```

### Reporte HTML
```bash
# Abrir reporte interactivo
start coverage\index.html
```

---

## 🎯 Tests Implementados

✅ **RAG Service** (8 tests)
- Chunking de texto
- Validación de archivos
- Metadatos

✅ **Chat History** (9 tests)
- Sesiones
- Mensajes
- Filtros

✅ **Validaciones** (12 tests)
- Archivos
- Strings
- IDs y fechas

✅ **Health Check** (4 tests)
- Estado del sistema
- Versión API

**Total: 33 tests** 🎉

---

## 🔄 Integración CI/CD

Los tests se ejecutan **automáticamente** en BTP CI/CD:

```
1. git push
2. CI/CD detecta cambios
3. npm install
4. npm run test:ci ← Tests aquí
5. Si pasan ✅ → Deploy
6. Si fallan ❌ → No deploy
```

---

## 📝 Añadir Nuevos Tests

### 1. Crear archivo de test
```bash
tests/services/miServicio.test.js
```

### 2. Escribir test básico
```javascript
import { describe, test, expect } from '@jest/globals';

describe('Mi Servicio', () => {
  test('debe hacer algo', () => {
    const resultado = miFuncion();
    expect(resultado).toBe('esperado');
  });
});
```

### 3. Ejecutar
```bash
npm test
```

---

## 🐛 Solución de Problemas

### Error: "Cannot find module"
```bash
npm install
```

### Tests no se ejecutan
```bash
# Limpiar caché
npx jest --clearCache

# Reinstalar
rm -rf node_modules
npm install
```

### Cobertura baja
```bash
# Ver qué falta cubrir
npm test
start coverage\index.html
```

---

## 📚 Documentación Completa

Ver **GUIA_TESTING.md** para:
- Mejores prácticas
- Debugging
- Configuración avanzada
- Métricas de calidad

---

## ✅ Checklist Rápido

Antes de hacer commit:

- [ ] `npm test` pasa ✅
- [ ] Cobertura > 50% ✅
- [ ] Sin console.logs ✅
- [ ] Tests documentados ✅

---

**¡Listo para testing!** 🧪
