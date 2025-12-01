# 🧪 Guía de Testing para AI Core API

## 📋 Índice

1. [Configuración](#configuración)
2. [Ejecutar Tests](#ejecutar-tests)
3. [Estructura de Tests](#estructura-de-tests)
4. [Integración con CI/CD](#integración-con-cicd)
5. [Cobertura de Código](#cobertura-de-código)
6. [Mejores Prácticas](#mejores-prácticas)

---

## 🔧 Configuración

### Instalación de Dependencias

```bash
npm install
```

Esto instalará:
- **Jest**: Framework de testing
- **Supertest**: Testing de APIs HTTP
- **@types/jest**: Tipos TypeScript para Jest

### Archivos de Configuración

- **`jest.config.js`**: Configuración principal de Jest
- **`tests/setup.js`**: Setup global para todos los tests
- **`package.json`**: Scripts de testing

---

## 🚀 Ejecutar Tests

### Comandos Disponibles

```bash
# Ejecutar todos los tests con cobertura
npm test

# Ejecutar tests en modo watch (desarrollo)
npm run test:watch

# Ejecutar tests para CI/CD
npm run test:ci
```

### Ejemplos de Uso

**Desarrollo local:**
```bash
npm run test:watch
```
Los tests se re-ejecutarán automáticamente cuando cambies archivos.

**Antes de commit:**
```bash
npm test
```
Verifica que todos los tests pasen y genera reporte de cobertura.

**En CI/CD:**
```bash
npm run test:ci
```
Optimizado para pipelines (sin watch, con cobertura).

---

## 📁 Estructura de Tests

```
tests/
├── setup.js                          # Configuración global
├── services/                         # Tests de servicios
│   ├── ragService.test.js           # Tests de RAG
│   └── chatHistoryService.test.js   # Tests de historial
├── routes/                          # Tests de rutas API
│   └── health.test.js               # Tests de health check
└── utils/                           # Tests de utilidades
    └── validation.test.js           # Tests de validación
```

### Convenciones de Nombres

- Archivos de test: `*.test.js`
- Ubicación: Carpeta `tests/` con misma estructura que `src/`
- Describe blocks: Nombre del módulo o funcionalidad
- Test cases: Descripción clara de lo que se prueba

---

## 🔄 Integración con CI/CD

### Configuración en mta.yaml

Los tests se ejecutan automáticamente durante el build:

```yaml
build-parameters:
  builder: npm
  commands:
    - npm install
    - npm run test:ci  # ← Tests ejecutados aquí
```

### Flujo en BTP CI/CD

1. **Fetch**: Descarga código del repositorio
2. **Install**: `npm install`
3. **Test**: `npm run test:ci` ✅
4. **Build**: Construye el MTA
5. **Deploy**: Despliega a Cloud Foundry

### ¿Qué pasa si los tests fallan?

- ❌ El build se detiene
- ❌ No se despliega a producción
- 📧 Se notifica el error
- 📊 Se muestra el reporte de tests

---

## 📊 Cobertura de Código

### Umbrales Configurados

```javascript
coverageThreshold: {
  global: {
    branches: 50,    // 50% de ramas cubiertas
    functions: 50,   // 50% de funciones cubiertas
    lines: 50,       // 50% de líneas cubiertas
    statements: 50   // 50% de statements cubiertos
  }
}
```

### Ver Reporte de Cobertura

Después de ejecutar `npm test`:

```bash
# Abrir reporte HTML
open coverage/index.html  # Mac/Linux
start coverage/index.html # Windows
```

### Archivos de Cobertura

```
coverage/
├── lcov-report/          # Reporte HTML interactivo
│   └── index.html       # Abrir este archivo
├── coverage-final.json  # Datos en JSON
└── lcov.info           # Formato LCOV
```

---

## ✅ Tests Implementados

### 1. RAG Service Tests

**Archivo**: `tests/services/ragService.test.js`

- ✅ Chunking de texto
- ✅ Validación de extensiones
- ✅ Validación de tamaño de archivo
- ✅ Generación de metadatos

### 2. Chat History Service Tests

**Archivo**: `tests/services/chatHistoryService.test.js`

- ✅ Validación de estructura de sesión
- ✅ Validación de mensajes
- ✅ Generación de títulos
- ✅ Filtros por usuario y contexto

### 3. Validation Utils Tests

**Archivo**: `tests/utils/validation.test.js`

- ✅ Validación de archivos
- ✅ Validación de strings
- ✅ Validación de UUIDs
- ✅ Validación de contextos
- ✅ Validación de fechas

### 4. Health Check Tests

**Archivo**: `tests/routes/health.test.js`

- ✅ Health check básico
- ✅ Información del sistema
- ✅ Estado de servicios
- ✅ Versión de API

---

## 📝 Escribir Nuevos Tests

### Template Básico

```javascript
import { describe, test, expect } from '@jest/globals';

describe('Nombre del Módulo', () => {
  test('debe hacer algo específico', () => {
    // Arrange (preparar)
    const input = 'valor de prueba';
    
    // Act (actuar)
    const result = funcionAProbar(input);
    
    // Assert (verificar)
    expect(result).toBe('resultado esperado');
  });
});
```

### Ejemplo Completo

```javascript
import { describe, test, expect, beforeEach } from '@jest/globals';

describe('Servicio de Usuarios', () => {
  let usuario;
  
  beforeEach(() => {
    usuario = {
      id: '123',
      nombre: 'Test User',
      email: 'test@example.com'
    };
  });

  test('debe crear usuario con datos válidos', () => {
    expect(usuario).toHaveProperty('id');
    expect(usuario).toHaveProperty('nombre');
    expect(usuario.email).toContain('@');
  });

  test('debe validar formato de email', () => {
    const emailValido = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    expect(emailValido.test(usuario.email)).toBe(true);
  });
});
```

---

## 🎯 Mejores Prácticas

### 1. Tests Independientes

```javascript
// ✅ BIEN: Cada test es independiente
test('debe sumar dos números', () => {
  const resultado = sumar(2, 3);
  expect(resultado).toBe(5);
});

test('debe restar dos números', () => {
  const resultado = restar(5, 3);
  expect(resultado).toBe(2);
});

// ❌ MAL: Tests dependientes
let resultado;
test('debe sumar', () => {
  resultado = sumar(2, 3);
});
test('debe usar resultado anterior', () => {
  expect(resultado).toBe(5); // Depende del test anterior
});
```

### 2. Nombres Descriptivos

```javascript
// ✅ BIEN: Nombre claro y descriptivo
test('debe retornar error 400 cuando el email es inválido', () => {
  // ...
});

// ❌ MAL: Nombre vago
test('test de email', () => {
  // ...
});
```

### 3. Arrange-Act-Assert

```javascript
test('debe calcular el total con descuento', () => {
  // Arrange: Preparar datos
  const precio = 100;
  const descuento = 0.2;
  
  // Act: Ejecutar función
  const total = calcularTotal(precio, descuento);
  
  // Assert: Verificar resultado
  expect(total).toBe(80);
});
```

### 4. Un Concepto por Test

```javascript
// ✅ BIEN: Un test, un concepto
test('debe validar email vacío', () => {
  expect(validarEmail('')).toBe(false);
});

test('debe validar email sin @', () => {
  expect(validarEmail('invalido')).toBe(false);
});

// ❌ MAL: Múltiples conceptos en un test
test('debe validar emails', () => {
  expect(validarEmail('')).toBe(false);
  expect(validarEmail('invalido')).toBe(false);
  expect(validarEmail('valido@test.com')).toBe(true);
});
```

---

## 🐛 Debugging Tests

### Ver Output Detallado

```bash
npm test -- --verbose
```

### Ejecutar un Solo Test

```bash
npm test -- tests/services/ragService.test.js
```

### Ejecutar Tests por Patrón

```bash
npm test -- --testNamePattern="validación"
```

### Ver Cobertura de un Archivo

```bash
npm test -- --collectCoverageFrom="services/ragService.js"
```

---

## 📈 Métricas de Calidad

### Objetivos de Cobertura

| Métrica | Mínimo | Objetivo | Excelente |
|---------|--------|----------|-----------|
| Lines | 50% | 70% | 90% |
| Functions | 50% | 70% | 90% |
| Branches | 50% | 65% | 85% |
| Statements | 50% | 70% | 90% |

### Aumentar Cobertura

1. Identificar código sin tests:
   ```bash
   npm test -- --coverage
   ```

2. Abrir reporte HTML:
   ```bash
   open coverage/index.html
   ```

3. Añadir tests para código no cubierto

4. Verificar mejora:
   ```bash
   npm test
   ```

---

## 🔗 Recursos

### Documentación

- [Jest Documentation](https://jestjs.io/docs/getting-started)
- [Supertest Documentation](https://github.com/visionmedia/supertest)
- [Testing Best Practices](https://testingjavascript.com/)

### Comandos Útiles

```bash
# Ver ayuda de Jest
npx jest --help

# Limpiar caché
npx jest --clearCache

# Actualizar snapshots
npm test -- -u

# Modo debug
node --inspect-brk node_modules/.bin/jest --runInBand
```

---

## 🎓 Próximos Pasos

1. **Añadir más tests**: Cubrir más servicios y rutas
2. **Tests de integración**: Probar flujos completos
3. **Tests E2E**: Probar desde el frontend
4. **Performance tests**: Medir tiempos de respuesta
5. **Security tests**: Validar seguridad

---

## ✅ Checklist Pre-Deploy

- [ ] Todos los tests pasan: `npm test`
- [ ] Cobertura > 50%
- [ ] No hay tests skipped (`test.skip`)
- [ ] No hay console.logs en tests
- [ ] Tests documentados
- [ ] CI/CD configurado

---

**¡Tests listos para CI/CD!** 🚀
