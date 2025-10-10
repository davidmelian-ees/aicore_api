@echo off
echo 🔧 Configuración completa de soporte PDF
echo =======================================
echo.

echo 📦 1. Instalando dependencia pdfjs-dist...
npm install pdfjs-dist@4.0.379

if %errorlevel% neq 0 (
    echo ❌ Error instalando pdfjs-dist
    pause
    exit /b 1
)

echo ✅ pdfjs-dist instalado correctamente
echo.

echo 📄 2. Creando archivo de prueba...
node create-test-pdf.js

echo.
echo 🚀 3. Configuración completada!
echo.
echo 💡 Próximos pasos:
echo    1. Reinicia el servidor: npm start
echo    2. Prueba con Postman usando la guía en docs/POSTMAN_PLIEGO_EXAMPLE.md
echo    3. O ejecuta test automático: node test-pliego-endpoint.js
echo.
echo 📋 Archivos creados:
echo    - pliego-test.txt (archivo de prueba)
echo    - Dependencia pdfjs-dist instalada
echo.
pause
