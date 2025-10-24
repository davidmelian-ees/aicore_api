@echo off
echo ========================================
echo    SUBIDA AUTOMATICA A CLOUD FOUNDRY
echo ========================================
echo.

REM Activar entorno virtual
if exist "..\venv_upload\Scripts\activate.bat" (
    call ..\venv_upload\Scripts\activate.bat
) else if exist "..\venv\Scripts\activate.bat" (
    call ..\venv\Scripts\activate.bat
) else (
    echo ERROR: No se encontro entorno virtual Python
    echo Ejecuta setup_env.bat primero para configurar el entorno
    pause
    exit /b 1
)

REM Verificar si existe la carpeta FOR_CONTEXT
if not exist "..\FOR_CONTEXT" (
    echo ADVERTENCIA: La carpeta FOR_CONTEXT no existe
    echo    Creando carpeta FOR_CONTEXT...
    mkdir ..\FOR_CONTEXT
    echo    Carpeta creada en: %CD%\..\FOR_CONTEXT
    echo    Coloca tus archivos ahi y ejecuta de nuevo.
    pause
    exit /b 0
)

REM Contar archivos en FOR_CONTEXT
set /a file_count=0
for %%f in (..\FOR_CONTEXT\*.*) do set /a file_count+=1

if %file_count%==0 (
    echo ADVERTENCIA: La carpeta FOR_CONTEXT esta vacia
    echo    Ubicacion: %CD%\..\FOR_CONTEXT
    echo    Coloca tus archivos (txt, pdf, docx, etc.) y ejecuta de nuevo.
    pause
    exit /b 0
)

echo Archivos encontrados: %file_count%
echo Destino: Cloud Foundry (ai_core_api.cfapps.eu10-005.hana.ondemand.com)
echo.

REM Mostrar opciones
echo Selecciona una opcion:
echo.
echo 1. Subir todos los archivos a Cloud Foundry
echo 2. Solo mostrar que archivos se subirian (dry-run)
echo 3. Subir con contexto personalizado
echo 4. Cancelar
echo.
set /p choice="Elige opcion (1-4): "

if "%choice%"=="1" (
    echo.
    echo Subiendo a Cloud Foundry...
    python upload_context_files.py --folder ..\FOR_CONTEXT
    goto end
)
if "%choice%"=="2" (
    echo.
    echo Mostrando archivos que se subirian...
    python upload_context_files.py --folder ..\FOR_CONTEXT --dry-run
    goto end
)
if "%choice%"=="3" (
    echo.
    set /p custom_context="Contexto personalizado: "
    echo.
    echo Subiendo con contexto personalizado...
    python upload_context_files.py --folder ..\FOR_CONTEXT --context "%custom_context%"
    goto end
)
if "%choice%"=="4" (
    echo Cancelado por el usuario
    exit /b 0
)
echo Opcion invalida
pause
exit /b 1

:end

echo.
echo Proceso completado
pause
