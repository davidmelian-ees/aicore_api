@echo off
echo 📄 Instalando soporte PDF mejorado con pdfjs-dist
echo ===============================================
echo.

echo 🗑️  Desinstalando pdf-parse anterior (si existe)...
npm uninstall pdf-parse

echo 📦 Instalando pdfjs-dist (Mozilla PDF.js)...
npm install pdfjs-dist@4.0.379

if %errorlevel% neq 0 (
    echo ❌ Error instalando pdfjs-dist
    pause
    exit /b 1
)

echo ✅ pdfjs-dist instalado correctamente
echo.
echo 🚀 Listo! Ahora reinicia el servidor:
echo    npm start
echo.
echo 💡 pdfjs-dist es más confiable que pdf-parse
echo    - Desarrollado por Mozilla
echo    - Mejor manejo de PDFs complejos
echo    - Extracción página por página
echo.
pause
