@echo off
echo 🚀 Desplegando Backend API con CORS actualizado...
echo.

echo 📍 Verificando directorio actual...
if not exist "manifest.yml" (
    echo ❌ Error: No se encuentra manifest.yml
    echo 💡 Asegúrate de estar en el directorio aicore_api
    pause
    exit /b 1
)

echo 🔐 Verificando login en Cloud Foundry...
cf target
if %errorlevel% neq 0 (
    echo ❌ Error: No estás logueado en Cloud Foundry
    echo 💡 Ejecuta: cf login -a https://api.cf.eu10-005.hana.ondemand.com
    pause
    exit /b 1
)

echo 📦 Desplegando aplicación...
cf push
if %errorlevel% neq 0 (
    echo ❌ Error en el despliegue
    pause
    exit /b 1
)

echo ✅ Backend desplegado exitosamente!
echo 🌐 URL: https://ai_core_api.cfapps.eu10-005.hana.ondemand.com
echo 📊 Health: https://ai_core_api.cfapps.eu10-005.hana.ondemand.com/health
echo.

echo 📋 Verificando logs...
cf logs ai_core_api --recent

pause
