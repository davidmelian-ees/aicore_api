@echo off
echo ========================================
echo   Iniciando servidor con SQLite
echo ========================================

echo Configurando VECTOR_STORE_TYPE=sqlite...
set VECTOR_STORE_TYPE=sqlite

echo Iniciando servidor...
node server.js

pause
