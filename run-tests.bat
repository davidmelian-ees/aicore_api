@echo off
chcp 65001 > nul
echo ========================================
echo EJECUTAR TESTS UNITARIOS
echo ========================================
echo.

echo 📦 Verificando dependencias...
if not exist "node_modules" (
    echo ⚠️  node_modules no encontrado
    echo 📥 Instalando dependencias...
    call npm install
    echo.
)

echo ✅ Dependencias listas
echo.

echo 🧪 Ejecutando tests...
echo.

call npm test

echo.

if %errorlevel% equ 0 (
    echo ✅ ¡TODOS LOS TESTS PASARON!
    echo.
    echo 📊 Reporte de cobertura generado en: coverage\
    echo.
    set /p abrir="¿Abrir reporte de cobertura? (s/n): "
    if /i "%abrir%"=="s" start coverage\index.html
) else (
    echo ❌ ALGUNOS TESTS FALLARON
    echo.
    echo 💡 Revisa los errores arriba
)

echo.
pause
