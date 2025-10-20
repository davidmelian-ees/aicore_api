@echo off
echo ========================================
echo   Limpieza del Proyecto RAG
echo ========================================

echo Eliminando carpeta chroma_service...
if exist "chroma_service" (
    rmdir /s /q "chroma_service"
    echo Carpeta chroma_service eliminada
) else (
    echo Carpeta chroma_service no encontrada
)

echo.
echo Eliminando archivos obsoletos...

if exist "start-with-chroma.bat" (
    del "start-with-chroma.bat"
    echo start-with-chroma.bat eliminado
) else (
    echo start-with-chroma.bat no encontrado
)

if exist "services\chromaVectorStore.js" (
    del "services\chromaVectorStore.js"
    echo chromaVectorStore.js eliminado
) else (
    echo chromaVectorStore.js no encontrado
)

if exist "services\chromaPythonClient.js" (
    del "services\chromaPythonClient.js"
    echo chromaPythonClient.js eliminado
) else (
    echo chromaPythonClient.js no encontrado
)

echo.
echo Eliminando archivos de prueba temporales...

if exist "test-context-persistence.js" (
    del "test-context-persistence.js"
    echo test-context-persistence.js eliminado
)

if exist "test-persistence-direct.js" (
    del "test-persistence-direct.js"
    echo test-persistence-direct.js eliminado
)

if exist "test-documents-api.js" (
    del "test-documents-api.js"
    echo test-documents-api.js eliminado
)

echo.
echo ========================================
echo   Limpieza completada
echo ========================================
echo.
echo Archivos mantenidos:
echo - services\sqliteVectorStore.js
echo - services\contextPersistence.js
echo - services\vectorStore.js (backup)
echo - start-with-sqlite.bat
echo - data\ (directorio de datos)
echo - VECTORSTORAGE.md (documentacion)
echo.

pause
