@echo off
echo 📄 Instalando soporte para procesamiento de PDFs
echo ===============================================
echo.

echo 📦 Instalando pdf-parse...
npm install pdf-parse@1.1.1

if %errorlevel% neq 0 (
    echo ❌ Error instalando pdf-parse
    pause
    exit /b 1
)

echo ✅ pdf-parse instalado correctamente
echo.

echo 🔄 Reiniciando servidor para aplicar cambios...
echo 💡 Presiona Ctrl+C para detener el servidor cuando esté listo
echo.

npm start
