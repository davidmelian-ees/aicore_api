# 🧪 Configurar Tests en BTP Continuous Integration & Delivery

## 📋 Guía Paso a Paso

### 1️⃣ Acceder al Servicio CI/CD

1. Ve a **BTP Cockpit**
2. Selecciona tu **Subaccount** (DEV)
3. Navega a **Services** → **Instances and Subscriptions**
4. Click en **Continuous Integration & Delivery**
5. Click en **Go to Application**

---

### 2️⃣ Crear/Editar Job

#### Si es un Job Nuevo:

1. Click en **Jobs** (menú lateral)
2. Click en **+ (Create Job)**
3. Completa la configuración básica

#### Si es un Job Existente:

1. Click en **Jobs**
2. Selecciona tu job
3. Click en **Edit** (icono de lápiz)

---

### 3️⃣ Configuración del Job

#### **General Settings**

```yaml
Job Name: ai-core-api-prod-deploy
Description: Deploy AI Core API with automated tests
Repository: [URL de tu repositorio Git]
Branch: main (o master)
```

#### **Repository Credentials**

Si tu repositorio es privado:

```yaml
Credential Type: Basic Authentication
Username: [Tu usuario Git]
Password/Token: [Personal Access Token]
```

---

### 4️⃣ **Build Configuration** ⭐ IMPORTANTE

Aquí es donde configuramos los tests:

#### **Opción A: Con MTA Build Tool (Recomendado)**

```yaml
Build Tool: MTA Build Tool
Build Tool Version: Latest
MTA Build Descriptor: mta.yaml

Additional Commands (opcional):
  - npm install
  - npm run test:ci
```

Los tests ya están en `mta.yaml`:
```yaml
build-parameters:
  commands:
    - npm install
    - npm run test:ci  # ← Tests aquí
```

#### **Opción B: Con npm (Alternativa)**

```yaml
Build Tool: npm
Node Version: 20.x

Build Script: test:ci
```

Esto ejecutará `npm run test:ci` automáticamente.

---

### 5️⃣ **Stages Configuration**

BTP CI/CD tiene diferentes stages. Configura así:

#### **Build Stage**

```yaml
Stage: Build
Execute: Always

Commands:
  - npm install
  - npm run test:ci
```

#### **Additional Unit Tests Stage** (Opcional)

Si quieres una stage separada para tests:

```yaml
Stage: Additional Unit Tests
Execute: Always

Commands:
  - npm run test:ci
```

---

### 6️⃣ **Deploy Configuration**

```yaml
Deploy Tool: Cloud Foundry CLI
API Endpoint: https://api.cf.eu10.hana.ondemand.com
Org: [Tu organización]
Space: production

Deploy Type: Standard
```

#### **Cloud Foundry Credentials**

```yaml
Username: [Tu email de BTP]
Password: [Tu contraseña de BTP]
```

---

### 7️⃣ **Advanced Settings** (Opcional)

#### **Test Results**

Si quieres publicar resultados de tests:

```yaml
Publish Test Results: Yes
Test Results Pattern: coverage/junit.xml
```

Para esto, actualiza `jest.config.js`:

```javascript
reporters: [
  'default',
  ['jest-junit', {
    outputDirectory: 'coverage',
    outputName: 'junit.xml',
  }]
]
```

#### **Code Coverage**

```yaml
Publish Coverage: Yes
Coverage Pattern: coverage/lcov.info
```

---

## 🎯 Configuración Completa del Job

### **Vista Completa en YAML** (para referencia)

```yaml
# General
name: ai-core-api-prod-deploy
repository: https://github.com/tu-usuario/ai-core-api
branch: main

# Build
build:
  tool: MTA Build Tool
  descriptor: mta.yaml
  commands:
    - npm install
    - npm run test:ci

# Deploy
deploy:
  tool: Cloud Foundry CLI
  api: https://api.cf.eu10.hana.ondemand.com
  org: tu-organizacion
  space: production
  
# Tests
tests:
  unit:
    enabled: true
    command: npm run test:ci
    results: coverage/junit.xml
  coverage:
    enabled: true
    pattern: coverage/lcov.info
```

---

## 📊 Configuración en la Interfaz Web

### **Pantalla 1: General**

```
┌─────────────────────────────────────────────────────┐
│ Job Name                                            │
│ ┌─────────────────────────────────────────────────┐ │
│ │ ai-core-api-prod-deploy                         │ │
│ └─────────────────────────────────────────────────┘ │
│                                                     │
│ Repository URL                                      │
│ ┌─────────────────────────────────────────────────┐ │
│ │ https://github.com/tu-usuario/ai-core-api       │ │
│ └─────────────────────────────────────────────────┘ │
│                                                     │
│ Branch                                              │
│ ┌─────────────────────────────────────────────────┐ │
│ │ main                                            │ │
│ └─────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────┘
```

### **Pantalla 2: Build**

```
┌─────────────────────────────────────────────────────┐
│ Build Tool                                          │
│ ┌─────────────────────────────────────────────────┐ │
│ │ [v] MTA Build Tool                              │ │
│ └─────────────────────────────────────────────────┘ │
│                                                     │
│ MTA Build Descriptor                                │
│ ┌─────────────────────────────────────────────────┐ │
│ │ mta.yaml                                        │ │
│ └─────────────────────────────────────────────────┘ │
│                                                     │
│ ☑ Execute Unit Tests                               │
│                                                     │
│ Test Command                                        │
│ ┌─────────────────────────────────────────────────┐ │
│ │ npm run test:ci                                 │ │
│ └─────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────┘
```

### **Pantalla 3: Deploy**

```
┌─────────────────────────────────────────────────────┐
│ Deploy Tool                                         │
│ ┌─────────────────────────────────────────────────┐ │
│ │ [v] Cloud Foundry CLI                           │ │
│ └─────────────────────────────────────────────────┘ │
│                                                     │
│ API Endpoint                                        │
│ ┌─────────────────────────────────────────────────┐ │
│ │ https://api.cf.eu10.hana.ondemand.com           │ │
│ └─────────────────────────────────────────────────┘ │
│                                                     │
│ Organization                                        │
│ ┌─────────────────────────────────────────────────┐ │
│ │ tu-organizacion                                 │ │
│ └─────────────────────────────────────────────────┘ │
│                                                     │
│ Space                                               │
│ ┌─────────────────────────────────────────────────┐ │
│ │ production                                      │ │
│ └─────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────┘
```

---

## 🔄 Flujo del Pipeline con Tests

```
┌─────────────────────────────────────────────────────┐
│ 1. TRIGGER                                          │
│    • git push                                       │
│    • Manual trigger                                 │
│    • Scheduled                                      │
└─────────────────────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────┐
│ 2. FETCH                                            │
│    • Clone repository                               │
│    • Checkout branch                                │
└─────────────────────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────┐
│ 3. BUILD                                            │
│    • npm install                                    │
│    • Install dependencies                           │
└─────────────────────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────┐
│ 4. TEST ✅ ← AQUÍ SE EJECUTAN LOS TESTS            │
│    • npm run test:ci                                │
│    • Run 33 unit tests                              │
│    • Generate coverage report                       │
│                                                     │
│    Si PASAN ✅ → Continuar                          │
│    Si FALLAN ❌ → DETENER pipeline                  │
└─────────────────────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────┐
│ 5. BUILD MTA                                        │
│    • mbt build                                      │
│    • Create .mtar package                           │
└─────────────────────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────┐
│ 6. DEPLOY                                           │
│    • cf deploy                                      │
│    • Deploy to Cloud Foundry                        │
└─────────────────────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────┐
│ 7. NOTIFICATION                                     │
│    • Email notification                             │
│    • Status update                                  │
└─────────────────────────────────────────────────────┘
```

---

## 📧 Configurar Notificaciones

### **Email Notifications**

1. En el job, ve a **Notifications**
2. Activa **Email Notifications**
3. Configura:

```yaml
Notify on:
  ☑ Build Success
  ☑ Build Failure
  ☑ Test Failure ← Importante para tests

Recipients:
  - tu-email@empresa.com
  - equipo@empresa.com
```

---

## 📊 Ver Resultados de Tests

### **Durante la Ejecución**

1. Ve a **Jobs**
2. Click en tu job
3. Click en **Builds** (pestaña)
4. Selecciona el build en ejecución
5. Ve a **Logs**

Verás:

```
[INFO] Running tests...
[INFO] npm run test:ci

PASS  tests/services/ragService.test.js
  ✓ debe dividir texto en chunks (5ms)
  ✓ debe validar extensiones (2ms)
  
PASS  tests/utils/validation.test.js
  ✓ debe validar email (3ms)
  
Test Suites: 4 passed, 4 total
Tests:       33 passed, 33 total
Coverage:    65.4% Lines

[SUCCESS] All tests passed!
```

### **Después de la Ejecución**

1. Click en el build completado
2. Ve a **Test Results** (pestaña)
3. Verás:
   - Total de tests
   - Tests pasados/fallados
   - Tiempo de ejecución
   - Cobertura de código

---

## 🚨 Qué Hacer si los Tests Fallan

### **En el Pipeline**

Si los tests fallan en CI/CD:

1. **El pipeline se detiene** ❌
2. **No se despliega a producción** ❌
3. **Recibes notificación por email** 📧
4. **Logs disponibles en BTP CI/CD**

### **Pasos para Resolver**

1. **Ver logs del build:**
   ```
   Jobs → Tu Job → Builds → Build Fallido → Logs
   ```

2. **Identificar test fallido:**
   ```
   FAIL  tests/services/ragService.test.js
     ✕ debe validar extensiones (15ms)
     
   Expected: true
   Received: false
   ```

3. **Reproducir localmente:**
   ```bash
   npm test
   ```

4. **Corregir el código**

5. **Verificar localmente:**
   ```bash
   npm test
   ```

6. **Commit y push:**
   ```bash
   git add .
   git commit -m "Fix failing test"
   git push
   ```

7. **El pipeline se ejecutará automáticamente**

---

## 🎯 Configuración Recomendada

### **Para Producción**

```yaml
Job Configuration:
  Build Tool: MTA Build Tool
  Execute Tests: Yes (en mta.yaml)
  Test Command: npm run test:ci
  Coverage Threshold: 50%
  
  On Test Failure:
    Action: Stop Pipeline
    Notify: Yes
    
  On Test Success:
    Action: Continue to Deploy
    Notify: Optional
```

### **Para Desarrollo**

```yaml
Job Configuration:
  Build Tool: npm
  Execute Tests: Yes
  Test Command: npm run test:watch
  Coverage Threshold: 40%
  
  On Test Failure:
    Action: Continue (warning)
    Notify: No
```

---

## 📝 Checklist de Configuración

Antes de activar el job:

- [ ] Repository URL configurado
- [ ] Branch correcto (main/master)
- [ ] Credenciales de Git configuradas
- [ ] Build Tool: MTA Build Tool
- [ ] Build Descriptor: mta.yaml
- [ ] Tests en mta.yaml: `npm run test:ci`
- [ ] Cloud Foundry credentials configuradas
- [ ] Org y Space correctos
- [ ] Notificaciones configuradas
- [ ] Tests pasan localmente: `npm test`

---

## 🔍 Verificar Configuración

### **Test Manual del Job**

1. Ve a tu job en BTP CI/CD
2. Click en **Run** (botón de play)
3. Observa los logs en tiempo real
4. Verifica que:
   - ✅ Tests se ejecutan
   - ✅ Tests pasan
   - ✅ Build continúa
   - ✅ Deploy se ejecuta

---

## 💡 Tips y Mejores Prácticas

### **1. Tests Rápidos**

Los tests en CI/CD deben ser rápidos:

```javascript
// jest.config.js
testTimeout: 10000,  // 10 segundos max por test
```

### **2. Tests Determinísticos**

Evita tests que fallen aleatoriamente:

```javascript
// ❌ MAL: Depende de tiempo
test('debe completar en 1 segundo', async () => {
  await sleep(1000);
  expect(true).toBe(true);
});

// ✅ BIEN: Determinístico
test('debe validar formato', () => {
  expect(validar('test')).toBe(true);
});
```

### **3. Logs Claros**

```javascript
test('debe validar email', () => {
  const email = 'test@example.com';
  console.log(`Testing email: ${email}`);
  expect(validarEmail(email)).toBe(true);
});
```

### **4. Cleanup**

```javascript
afterEach(() => {
  // Limpiar después de cada test
  jest.clearAllMocks();
});
```

---

## 🆘 Troubleshooting

### **Error: "Tests not found"**

**Solución:**
```bash
# Verifica que existen los tests
ls tests/

# Verifica package.json
cat package.json | grep "test:ci"
```

### **Error: "npm run test:ci failed"**

**Solución:**
```bash
# Ejecuta localmente
npm run test:ci

# Revisa logs
# Corrige errores
# Vuelve a intentar
```

### **Error: "Coverage below threshold"**

**Solución:**
```javascript
// jest.config.js
coverageThreshold: {
  global: {
    lines: 40,  // Reduce temporalmente
  }
}
```

---

## 📚 Recursos

- **BTP CI/CD Docs**: https://help.sap.com/docs/CICD_OVERVIEW
- **Jest Docs**: https://jestjs.io/docs/getting-started
- **MTA Docs**: https://sap.github.io/cloud-mta-build-tool/

---

## ✅ Resumen

**Configuración Mínima:**

1. Job → Build Tool: **MTA Build Tool**
2. Build Descriptor: **mta.yaml**
3. Tests en mta.yaml: **npm run test:ci**
4. Deploy: **Cloud Foundry**

**Resultado:**

```
git push → Tests automáticos → Deploy solo si pasan ✅
```

---

**¡Tests configurados en BTP CI/CD!** 🎉
