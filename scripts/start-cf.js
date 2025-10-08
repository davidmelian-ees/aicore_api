#!/usr/bin/env node

/**
 * Script de inicio para Cloud Foundry
 * Maneja la inicialización del sistema RAG en entorno cloud
 */

import { spawn } from 'child_process';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import fs from 'fs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

console.log('🚀 Iniciando AI Core API en Cloud Foundry...');

// Configuración para Cloud Foundry
process.env.VECTOR_STORE_TYPE = process.env.VECTOR_STORE_TYPE || 'memory';
process.env.NODE_ENV = process.env.NODE_ENV || 'production';

console.log(`📊 Configuración:`);
console.log(`   - Vector Store: ${process.env.VECTOR_STORE_TYPE}`);
console.log(`   - Environment: ${process.env.NODE_ENV}`);
console.log(`   - Port: ${process.env.PORT || 4000}`);

// Verificar archivos críticos
const criticalFiles = [
    'server.js',
    'package.json',
    'services/ragService.js',
    'services/vectorStore.js'
];

console.log('🔍 Verificando archivos críticos...');
for (const file of criticalFiles) {
    const filePath = join(__dirname, file);
    if (!fs.existsSync(filePath)) {
        console.error(`❌ Archivo crítico no encontrado: ${file}`);
        process.exit(1);
    }
}
console.log('✅ Todos los archivos críticos encontrados');

// Inicializar datos de ejemplo si no existen
console.log('📄 Verificando documentos de ejemplo...');
const sampleDocsPath = join(__dirname, 'sample_documents');
if (!fs.existsSync(sampleDocsPath)) {
    console.log('📁 Creando carpeta de documentos de ejemplo...');
    fs.mkdirSync(sampleDocsPath, { recursive: true });
    
    // Crear documento de ejemplo
    const exampleDoc = `# Políticas de la Empresa

## Políticas de Vacaciones
- Los empleados tienen derecho a 22 días de vacaciones al año
- Las vacaciones deben solicitarse con 15 días de antelación
- No se pueden acumular más de 5 días de un año al siguiente

## Políticas de Trabajo Remoto
- El trabajo remoto está permitido hasta 3 días por semana
- Se requiere aprobación del supervisor para trabajo remoto
- Debe mantenerse disponibilidad durante horario laboral

## Políticas de Seguridad
- Cambiar contraseñas cada 90 días
- No compartir credenciales de acceso
- Reportar incidentes de seguridad inmediatamente

## Contacto
Para más información, contactar a Recursos Humanos.
`;
    
    fs.writeFileSync(join(sampleDocsPath, 'empresa_politicas.md'), exampleDoc);
    console.log('✅ Documento de ejemplo creado');
}

// Función para iniciar el servidor
function startServer() {
    console.log('🌐 Iniciando servidor principal...');
    
    const serverProcess = spawn('node', ['server.js'], {
        stdio: 'inherit',
        env: {
            ...process.env,
            PORT: process.env.PORT || 4000
        }
    });

    serverProcess.on('error', (error) => {
        console.error('❌ Error iniciando servidor:', error);
        process.exit(1);
    });

    serverProcess.on('exit', (code) => {
        console.log(`🔄 Servidor terminó con código: ${code}`);
        if (code !== 0) {
            console.error('❌ Servidor terminó con error');
            process.exit(code);
        }
    });

    // Manejar señales de terminación
    process.on('SIGTERM', () => {
        console.log('📴 Recibida señal SIGTERM, cerrando servidor...');
        serverProcess.kill('SIGTERM');
    });

    process.on('SIGINT', () => {
        console.log('📴 Recibida señal SIGINT, cerrando servidor...');
        serverProcess.kill('SIGINT');
    });
}

// Iniciar el servidor
startServer();
