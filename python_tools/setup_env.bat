@echo off
echo ========================================
echo    CONFIGURACION ENTORNO PYTHON
echo ========================================
echo.

REM Nota: Este script asume que tienes Python instalado
REM Si no tienes Python en PATH, el entorno virtual se creara manualmente

echo Activando entorno virtual...
if exist "..\venv_upload\Scripts\activate.bat" (
    call ..\venv_upload\Scripts\activate.bat
) else if exist "..\venv\Scripts\activate.bat" (
    call ..\venv\Scripts\activate.bat
) else (
    echo Creando entorno virtual...
    cd ..
    python -m venv venv_upload
    cd python_tools
    call ..\venv_upload\Scripts\activate.bat
)

echo Instalando dependencias...
pip install -r requirements.txt

echo.
echo Entorno configurado correctamente
echo Para usar el script ejecuta: upload_to_cloud.bat
echo.
pause
