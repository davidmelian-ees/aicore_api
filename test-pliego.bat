@echo off
echo 🧪 Test del Endpoint de Procesamiento de Pliegos
echo ===============================================
echo.

echo 📡 Verificando que el backend esté corriendo...
curl -s http://localhost:4000/health > nul 2>&1
if %errorlevel% neq 0 (
    echo ❌ Backend no está corriendo en puerto 4000
    echo 💡 Ejecuta: npm start
    echo.
    pause
    exit /b 1
)

echo ✅ Backend detectado
echo.

echo 🚀 Ejecutando tests del endpoint...
node test-pliego-endpoint.js

echo.
echo 💡 Para usar el frontend:
echo    1. cd ..\aicore_api_web\aicore_web
echo    2. npm start
echo    3. Abre http://localhost:3000
echo    4. Ve a "Procesador de Pliegos"
echo.
pause
