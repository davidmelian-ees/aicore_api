@echo off
echo 🧪 Probando configuración CORS del backend...
echo.

echo 📍 Verificando que el servidor esté ejecutándose...
curl -s http://localhost:4000/health > nul
if %errorlevel% neq 0 (
    echo ❌ El servidor no está ejecutándose en localhost:4000
    echo 💡 Ejecuta primero: start-backend-local.bat
    pause
    exit /b 1
)

echo ✅ Servidor ejecutándose

echo.
echo 🔍 Probando endpoint de salud...
curl -s http://localhost:4000/health

echo.
echo.
echo 🔍 Probando CORS con preflight request...
curl -s -X OPTIONS ^
  -H "Origin: http://localhost:3000" ^
  -H "Access-Control-Request-Method: GET" ^
  -H "Access-Control-Request-Headers: Content-Type,Authorization" ^
  http://localhost:4000/api/rag/contexts

echo.
echo.
echo 🔍 Probando endpoint de contextos...
curl -s -H "Origin: http://localhost:3000" http://localhost:4000/api/rag/contexts

echo.
echo.
echo ✅ Pruebas completadas
pause
