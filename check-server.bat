@echo off
echo 🔍 Verificando estado del servidor backend...
echo.

echo 📍 Verificando si el puerto 4000 está en uso...
netstat -an | findstr :4000
if %errorlevel% equ 0 (
    echo ✅ Puerto 4000 está en uso
) else (
    echo ❌ Puerto 4000 NO está en uso
    echo 💡 El servidor no está ejecutándose
    echo.
    echo 🚀 ¿Quieres iniciarlo ahora? (S/N)
    set /p choice=
    if /i "%choice%"=="S" (
        echo Iniciando servidor...
        start "Backend Server" cmd /c "start-backend-local.bat"
        echo ⏳ Esperando 10 segundos...
        timeout /t 10 /nobreak
    )
)

echo.
echo 🧪 Probando conectividad básica...
curl -s -w "Status: %%{http_code}\n" http://localhost:4000/health
if %errorlevel% neq 0 (
    echo ❌ No se puede conectar al servidor
    echo 💡 Verifica que esté ejecutándose en localhost:4000
) else (
    echo ✅ Servidor responde correctamente
)

echo.
echo 🧪 Probando endpoint de contextos...
curl -s -w "Status: %%{http_code}\n" http://localhost:4000/api/rag/contexts
if %errorlevel% neq 0 (
    echo ❌ Endpoint de contextos no responde
) else (
    echo ✅ Endpoint de contextos responde
)

echo.
pause
