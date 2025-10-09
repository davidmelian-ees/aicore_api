@echo off
echo 🧪 Diagnostico CORS - Backend y Frontend
echo =====================================
echo.

echo 📡 1. Verificando si el backend esta corriendo...
curl -s http://localhost:4000/health > nul 2>&1
if %errorlevel% equ 0 (
    echo ✅ Backend corriendo en puerto 4000
    echo.
    echo 📋 Health check response:
    curl -s http://localhost:4000/health
    echo.
    echo.
) else (
    echo ❌ Backend NO esta corriendo en puerto 4000
    echo 💡 Ejecuta: npm start o node server.js
    echo.
    goto :end
)

echo 📡 2. Probando CORS con curl...
echo.
echo 🔍 GET request con Origin header:
curl -v -H "Origin: http://localhost:3000" http://localhost:4000/health 2>&1 | findstr "Access-Control"
echo.

echo 🔍 OPTIONS preflight request:
curl -v -X OPTIONS -H "Origin: http://localhost:3000" -H "Access-Control-Request-Method: GET" http://localhost:4000/api/rag/health 2>&1 | findstr "Access-Control"
echo.

echo 📡 3. Probando con Node.js...
node test-cors.js

:end
echo.
echo 💡 Si ves errores CORS:
echo    1. Asegurate que el backend este corriendo: npm start
echo    2. Verifica que el frontend use http://localhost:3000
echo    3. Reinicia ambos servicios
echo.
pause
