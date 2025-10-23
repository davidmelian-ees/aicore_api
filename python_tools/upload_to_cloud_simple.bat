@echo off
echo ========================================
echo    SUBIDA AUTOMATICA A CLOUD FOUNDRY
echo ========================================
echo.

REM Verificar entorno virtual
if exist "..\venv\Scripts\python.exe" (
    set PYTHON_PATH="..\venv\Scripts\python.exe"
) else if exist "..\venv_upload\Scripts\python.exe" (
    set PYTHON_PATH="..\venv_upload\Scripts\python.exe"
) else (
    echo ERROR: Entorno virtual no encontrado
    echo Ejecuta setup_env.bat primero
    pause
    exit /b 1
)

REM Verificar carpeta FOR_CONTEXT
if not exist "..\FOR_CONTEXT" (
    echo Creando carpeta FOR_CONTEXT...
    mkdir "..\FOR_CONTEXT"
    echo Carpeta creada. Coloca tus archivos ahi.
    pause
    exit /b 0
)

REM Verificar archivos
dir "..\FOR_CONTEXT\*.*" >nul 2>&1
if errorlevel 1 (
    echo La carpeta FOR_CONTEXT esta vacia
    echo Coloca archivos y ejecuta de nuevo
    pause
    exit /b 0
)

echo Archivos encontrados en FOR_CONTEXT
echo Destino: Cloud Foundry
echo.
echo 1. Subir archivos
echo 2. Dry-run (solo mostrar)
echo 3. Salir
echo.
set /p choice="Selecciona (1-3): "

if "%choice%"=="1" goto upload
if "%choice%"=="2" goto dryrun
if "%choice%"=="3" goto exit
echo Opcion invalida
pause
exit /b 1

:upload
echo.
echo Subiendo archivos...
%PYTHON_PATH% upload_context_files.py --folder "..\FOR_CONTEXT"
goto finish

:dryrun
echo.
echo Mostrando archivos...
%PYTHON_PATH% upload_context_files.py --folder "..\FOR_CONTEXT" --dry-run
goto finish

:exit
echo Cancelado
exit /b 0

:finish
echo.
echo Proceso terminado
pause
