@echo off
echo ========================================
echo   ChromaDB Simple Service - Iniciando
echo ========================================
echo.

cd /d "%~dp0"

echo 1. Verificando Python...
python --version >nul 2>&1
if %errorlevel% neq 0 (
    echo ❌ Python no esta instalado
    echo 💡 Descarga Python desde: https://www.python.org/downloads/
    pause
    exit /b 1
)
echo ✅ Python encontrado

echo.
echo 2. Instalando dependencias basicas...
echo 📦 Instalando ChromaDB...
pip install chromadb --quiet
if %errorlevel% neq 0 (
    echo ❌ Error instalando ChromaDB
    echo 💡 Intenta: pip install --user chromadb
    pause
    exit /b 1
)

echo 📦 Instalando FastAPI...
pip install fastapi --quiet
if %errorlevel% neq 0 (
    echo ❌ Error instalando FastAPI
    pause
    exit /b 1
)

echo 📦 Instalando Uvicorn...
pip install uvicorn --quiet
if %errorlevel% neq 0 (
    echo ❌ Error instalando Uvicorn
    pause
    exit /b 1
)

echo ✅ Dependencias instaladas

echo.
echo 3. Iniciando ChromaDB Simple Service...
echo 🌐 Servicio disponible en: http://localhost:8001
echo 🏥 Health check: http://localhost:8001/health
echo 📊 Estadisticas: http://localhost:8001/stats
echo 📖 Documentacion: http://localhost:8001/docs
echo.
echo Para detener el servicio: Ctrl+C
echo ========================================
echo.

python simple_server.py
