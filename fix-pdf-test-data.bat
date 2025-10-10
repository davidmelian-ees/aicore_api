@echo off
echo 🔧 Solucionando error de archivo PDF de prueba
echo =============================================
echo.

echo 📁 Creando directorio test\data...
if not exist "test" mkdir test
if not exist "test\data" mkdir test\data

echo 📄 Copiando archivo PDF de prueba...
copy "node_modules\pdf-parse\test\data\05-versions-space.pdf" "test\data\05-versions-space.pdf"

if %errorlevel% equ 0 (
    echo ✅ Archivo copiado exitosamente
) else (
    echo ❌ Error copiando archivo
    pause
    exit /b 1
)

echo.
echo 📋 Verificando archivos creados:
dir test\data\

echo.
echo ✅ Problema solucionado!
echo 💡 El archivo PDF de prueba ahora está disponible en test\data\
echo.
pause
