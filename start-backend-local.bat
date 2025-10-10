@echo off
echo 🚀 Iniciando Backend RAG en modo local...
echo.

echo 📍 Directorio actual: %CD%
echo.

echo 📦 Verificando dependencias...
if not exist "node_modules" (
    echo 📥 Instalando dependencias...
    cmd /c "npm install"
    if %errorlevel% neq 0 (
        echo ❌ Error instalando dependencias
        pause
        exit /b 1
    )
) else (
    echo ✅ Dependencias ya instaladas
)

echo.
echo 🔧 Configuración:
echo   - Puerto: 4000
echo   - Entorno: desarrollo
echo   - CORS: habilitado para localhost:3000
echo   - Autenticación: deshabilitada
echo.

echo 🚀 Iniciando servidor...
echo 💡 Presiona Ctrl+C para detener
echo.

set NODE_ENV=development
cmd /c "npm start"
