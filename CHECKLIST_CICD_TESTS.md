# ✅ Checklist: Configurar Tests en BTP CI/CD

## 📋 Pasos para Configurar

### ☑️ 1. Preparación Local

- [ ] Tests creados en carpeta `tests/`
- [ ] `package.json` tiene script `test:ci`
- [ ] `jest.config.js` configurado
- [ ] Tests pasan localmente: `npm test`
- [ ] Cobertura > 50%

**Verificar:**
```bash
npm test
# Debe mostrar: Tests: 33 passed, 33 total
```

---

### ☑️ 2. Configuración en Repositorio

- [ ] `mta.yaml` incluye comando de tests
- [ ] `.gitignore` excluye `coverage/`
- [ ] Código commiteado y pusheado a Git

**Verificar mta.yaml:**
```yaml
build-parameters:
  commands:
    - npm install
    - npm run test:ci  # ← Debe estar aquí
```

---

### ☑️ 3. Acceso a BTP CI/CD

- [ ] Acceso a BTP Cockpit
- [ ] Servicio CI/CD suscrito
- [ ] Credenciales de Git disponibles
- [ ] Credenciales de Cloud Foundry disponibles

**URLs:**
- BTP Cockpit: https://cockpit.eu10.hana.ondemand.com
- CI/CD Service: [Tu subaccount] → Services → CI/CD

---

### ☑️ 4. Crear/Editar Job en BTP CI/CD

#### General Settings

- [ ] Job Name: `ai-core-api-prod-deploy`
- [ ] Repository URL configurado
- [ ] Branch: `main` (o `master`)
- [ ] Credenciales de Git añadidas

#### Build Configuration

- [ ] Build Tool: **MTA Build Tool**
- [ ] Build Descriptor: **mta.yaml**
- [ ] Version: Latest

#### Deploy Configuration

- [ ] Deploy Tool: **Cloud Foundry CLI**
- [ ] API Endpoint: `https://api.cf.eu10.hana.ondemand.com`
- [ ] Organization: [Tu org]
- [ ] Space: `production`
- [ ] Credenciales CF añadidas

#### Notifications

- [ ] Email notifications activadas
- [ ] Notify on Test Failure: **Yes**
- [ ] Notify on Build Failure: **Yes**
- [ ] Email configurado

---

### ☑️ 5. Guardar y Probar

- [ ] Click en **Save**
- [ ] Click en **Run** (trigger manual)
- [ ] Observar logs en tiempo real

**Verificar en logs:**
```
[INFO] Running: npm run test:ci
PASS  tests/services/ragService.test.js
Tests: 33 passed, 33 total
[SUCCESS] All tests passed!
```

---

## 🎯 Configuración Mínima (Copy-Paste)

### En BTP CI/CD Job:

```yaml
# GENERAL
Job Name: ai-core-api-prod-deploy
Repository: [Tu URL de Git]
Branch: main

# BUILD
Build Tool: MTA Build Tool
Build Descriptor: mta.yaml

# DEPLOY
Deploy Tool: Cloud Foundry CLI
API Endpoint: https://api.cf.eu10.hana.ondemand.com
Org: [Tu organización]
Space: production

# NOTIFICATIONS
Email: tu-email@empresa.com
Notify on Test Failure: Yes
```

---

## 🔍 Verificación Post-Configuración

### Test 1: Trigger Manual

- [ ] Ve al job en BTP CI/CD
- [ ] Click en **Run**
- [ ] Espera a que termine
- [ ] Verifica que tests se ejecutaron
- [ ] Verifica que deploy fue exitoso

### Test 2: Trigger Automático

- [ ] Haz un cambio pequeño en código
- [ ] `git commit -m "Test CI/CD"`
- [ ] `git push`
- [ ] Ve a BTP CI/CD
- [ ] Verifica que job se ejecutó automáticamente

### Test 3: Test Fallido

- [ ] Modifica un test para que falle
- [ ] `git commit` y `git push`
- [ ] Verifica que pipeline se detiene
- [ ] Verifica que recibes email de notificación
- [ ] Revierte el cambio

---

## 📊 Indicadores de Éxito

### ✅ Todo Funciona Si:

1. **Tests se ejecutan automáticamente** en cada push
2. **Pipeline se detiene** si tests fallan
3. **Deploy solo ocurre** si tests pasan
4. **Recibes notificaciones** de fallos
5. **Logs muestran** resultados de tests

### Ejemplo de Log Exitoso:

```
[INFO] Stage: Test
[INFO] Running: npm run test:ci
PASS  tests/services/ragService.test.js
PASS  tests/services/chatHistoryService.test.js
PASS  tests/utils/validation.test.js
PASS  tests/routes/health.test.js

Test Suites: 4 passed, 4 total
Tests:       33 passed, 33 total
Coverage:    65.4% Lines

[SUCCESS] All tests passed!
[INFO] Proceeding to Build stage...
[INFO] Running: mbt build
[SUCCESS] MTA built successfully
[INFO] Proceeding to Deploy stage...
[SUCCESS] Deployed to Cloud Foundry
```

---

## 🚨 Troubleshooting

### Problema: Tests no se ejecutan

**Verificar:**
- [ ] `mta.yaml` tiene comando `npm run test:ci`
- [ ] `package.json` tiene script `test:ci`
- [ ] Dependencias instaladas correctamente

**Solución:**
```bash
# Local
npm install
npm run test:ci

# Si funciona local, revisar mta.yaml
```

### Problema: Pipeline falla en tests

**Verificar:**
- [ ] Tests pasan localmente
- [ ] Versión de Node.js correcta
- [ ] Variables de entorno configuradas

**Solución:**
```bash
# Ejecutar exactamente como CI/CD
npm run test:ci

# Ver logs detallados
npm test -- --verbose
```

### Problema: No recibo notificaciones

**Verificar:**
- [ ] Email configurado en job
- [ ] "Notify on Test Failure" activado
- [ ] Email no está en spam

---

## 📝 Comandos Útiles

### Ver estado del job:
```bash
# En BTP CI/CD web interface
Jobs → [Tu job] → Builds
```

### Ejecutar tests como CI/CD:
```bash
npm run test:ci
```

### Ver cobertura:
```bash
npm test
start coverage/index.html
```

### Limpiar y reinstalar:
```bash
rm -rf node_modules
npm install
npm test
```

---

## 🎓 Próximos Pasos

Una vez configurado:

1. **Monitorear** primeros builds
2. **Ajustar** umbrales de cobertura si es necesario
3. **Añadir** más tests según sea necesario
4. **Documentar** proceso para el equipo
5. **Celebrar** 🎉 - ¡Tests automáticos funcionando!

---

## 📚 Recursos

- **Guía Completa**: `CONFIGURAR_TESTS_CICD_BTP.md`
- **Flujo Visual**: `FLUJO_TESTS_CICD.txt`
- **Testing Guide**: `GUIA_TESTING.md`
- **Quick Start**: `TESTING_QUICKSTART.md`

---

## ✅ Checklist Final

Antes de dar por terminado:

- [ ] Tests pasan localmente
- [ ] Job configurado en BTP CI/CD
- [ ] Trigger manual funciona
- [ ] Trigger automático funciona
- [ ] Tests se ejecutan en pipeline
- [ ] Pipeline se detiene si tests fallan
- [ ] Notificaciones funcionan
- [ ] Equipo informado del proceso

---

**¡Configuración completa!** 🚀

Tu código ahora se valida automáticamente antes de cada deploy a producción.
