@echo off
echo ========================================
echo   Limpieza de Archivos .BAT Obsoletos
echo ========================================
echo.

echo Analizando archivos .bat del proyecto...
echo.

echo ARCHIVOS A MANTENER:
echo - start-with-sqlite.bat (PRINCIPAL - inicia servidor con SQLite)
echo - deploy-backend.bat (despliegue a Cloud Foundry)
echo - cleanup-project.bat (limpieza del proyecto)
echo.

echo ARCHIVOS A ELIMINAR (obsoletos/duplicados):
echo.

REM Archivos de instalación PDF (ya están instalados)
if exist "install-pdf-support.bat" (
    del "install-pdf-support.bat"
    echo - install-pdf-support.bat eliminado (dependencias ya instaladas)
) else (
    echo - install-pdf-support.bat no encontrado
)

if exist "install-pdfjs.bat" (
    del "install-pdfjs.bat"
    echo - install-pdfjs.bat eliminado (dependencias ya instaladas)
) else (
    echo - install-pdfjs.bat no encontrado
)

if exist "setup-pdf-support.bat" (
    del "setup-pdf-support.bat"
    echo - setup-pdf-support.bat eliminado (dependencias ya instaladas)
) else (
    echo - setup-pdf-support.bat no encontrado
)

REM Archivos de prueba y debug
if exist "test-cors.bat" (
    del "test-cors.bat"
    echo - test-cors.bat eliminado (prueba temporal)
) else (
    echo - test-cors.bat no encontrado
)

if exist "diagnose-cors.bat" (
    del "diagnose-cors.bat"
    echo - diagnose-cors.bat eliminado (debug temporal)
) else (
    echo - diagnose-cors.bat no encontrado
)

if exist "check-server.bat" (
    del "check-server.bat"
    echo - check-server.bat eliminado (debug temporal)
) else (
    echo - check-server.bat no encontrado
)

if exist "test-pliego.bat" (
    del "test-pliego.bat"
    echo - test-pliego.bat eliminado (prueba temporal)
) else (
    echo - test-pliego.bat no encontrado
)

if exist "fix-pdf-test-data.bat" (
    del "fix-pdf-test-data.bat"
    echo - fix-pdf-test-data.bat eliminado (fix temporal)
) else (
    echo - fix-pdf-test-data.bat no encontrado
)

REM Archivo duplicado (start-backend-local.bat hace lo mismo que start-with-sqlite.bat)
if exist "start-backend-local.bat" (
    del "start-backend-local.bat"
    echo - start-backend-local.bat eliminado (duplicado de start-with-sqlite.bat)
) else (
    echo - start-backend-local.bat no encontrado
)

echo.
echo ========================================
echo   Limpieza de .BAT completada
echo ========================================
echo.

echo ARCHIVOS .BAT FINALES (solo los necesarios):
echo.
echo DESARROLLO:
echo - start-with-sqlite.bat  (USAR ESTE para iniciar servidor)
echo.
echo PRODUCCION:
echo - deploy-backend.bat     (desplegar a Cloud Foundry)
echo.
echo UTILIDADES:
echo - cleanup-project.bat    (limpieza general del proyecto)
echo - cleanup-bat-files.bat  (este archivo - se puede eliminar despues)
echo.

echo COMO USAR:
echo 1. Para desarrollo local: start-with-sqlite.bat
echo 2. Para desplegar: deploy-backend.bat
echo.

pause
