@echo off
chcp 65001 > nul
echo ========================================
echo BUILD MTA PARA BTP CI/CD
echo ========================================
echo.

echo 📦 Verificando instalación de MBT...
where mbt >nul 2>&1
if %errorlevel% neq 0 (
    echo.
    echo ⚠️  MBT no está instalado
    echo 📥 Instalando Cloud MTA Build Tool...
    call npm install -g mbt
    echo.
    echo ✅ MBT instalado
    echo.
    echo ⚠️  IMPORTANTE: Cierra y vuelve a abrir esta ventana
    echo    para que el comando 'mbt' esté disponible.
    echo.
    echo 💡 Después ejecuta de nuevo: build-mta.bat
    echo.
    pause
    exit /b 0
)

echo ✅ MBT ya está instalado
echo.

echo 🔨 Construyendo MTA...
echo.

REM Limpiar builds anteriores
if exist "mta_archives" rmdir /s /q "mta_archives"
if exist ".mta_mbt_build_tmp" rmdir /s /q ".mta_mbt_build_tmp"

REM Build con extensión de producción
echo 🏗️  Ejecutando: mbt build -e mta-prod.mtaext
echo.
mbt build -e mta-prod.mtaext

echo.

if exist "mta_archives\ai-core-api_1.0.0.mtar" (
    echo ✅ ¡BUILD EXITOSO!
    echo.
    echo 📦 Archivo MTA creado:
    echo    mta_archives\ai-core-api_1.0.0.mtar
    echo.
    echo 💡 Siguiente paso:
    echo    1. Sube este archivo a tu repositorio Git
    echo    2. O despliega manualmente con:
    echo       cf deploy mta_archives\ai-core-api_1.0.0.mtar
    echo.
) else (
    echo ❌ Error en el build
    echo.
    echo 💡 Revisa los mensajes de error arriba
    echo.
)

pause
