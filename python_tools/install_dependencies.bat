@echo off
echo ========================================
echo   INSTALAR DEPENDENCIAS PYTHON
echo ========================================
echo.

REM Buscar Python en el entorno virtual
if exist "..\venv\Scripts\pip.exe" (
    echo Usando entorno virtual: venv
    "..\venv\Scripts\pip.exe" install -r requirements.txt
    echo.
    echo Dependencias instaladas correctamente
) else if exist "..\venv_upload\Scripts\pip.exe" (
    echo Usando entorno virtual: venv_upload
    "..\venv_upload\Scripts\pip.exe" install -r requirements.txt
    echo.
    echo Dependencias instaladas correctamente
) else (
    echo ERROR: No se encontro entorno virtual
    echo.
    echo SOLUCION MANUAL:
    echo 1. Crea entorno virtual: python -m venv venv
    echo 2. Activa entorno: venv\Scripts\activate.bat
    echo 3. Instala dependencias: pip install -r requirements.txt
)

echo.
pause
