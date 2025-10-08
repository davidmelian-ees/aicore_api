#!/usr/bin/env node

/**
 * Script para inicializar datos de ejemplo en Cloud Foundry
 * Se ejecuta automáticamente al iniciar la aplicación
 */

import { indexDocument } from '../services/ragService.js';
import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

async function initializeSampleData() {
  try {
    console.log('📄 Inicializando datos de ejemplo...');
    
    const sampleDocsPath = path.join(__dirname, 'sample_documents');
    
    // Verificar si la carpeta existe
    try {
      await fs.access(sampleDocsPath);
    } catch {
      console.log('📁 Creando carpeta de documentos de ejemplo...');
      await fs.mkdir(sampleDocsPath, { recursive: true });
    }
    
    // Crear documentos de ejemplo si no existen
    const documents = [
      {
        filename: 'empresa_politicas.md',
        content: `# Políticas de la Empresa

## Políticas de Vacaciones
- Los empleados tienen derecho a 22 días de vacaciones al año
- Las vacaciones deben solicitarse con 15 días de antelación
- No se pueden acumular más de 5 días de un año al siguiente
- Las vacaciones no utilizadas se pierden al final del año fiscal

## Políticas de Trabajo Remoto
- El trabajo remoto está permitido hasta 3 días por semana
- Se requiere aprobación del supervisor para trabajo remoto
- Debe mantenerse disponibilidad durante horario laboral (9:00-17:00)
- Reuniones importantes requieren presencia física

## Políticas de Seguridad
- Cambiar contraseñas cada 90 días
- No compartir credenciales de acceso con otros empleados
- Reportar incidentes de seguridad inmediatamente al departamento de TI
- Usar autenticación de dos factores para sistemas críticos

## Beneficios para Empleados
- Seguro médico completo cubierto al 100%
- Plan de pensiones con contribución de la empresa del 5%
- Descuentos en gimnasios y actividades deportivas
- Programa de formación continua con presupuesto anual de €2000

## Contacto
Para más información sobre estas políticas, contactar a:
- Recursos Humanos: rrhh@empresa.com
- Departamento de TI: it@empresa.com
- Supervisor directo
`
      },
      {
        filename: 'manual_procedimientos.md',
        content: `# Manual de Procedimientos

## Procedimiento de Onboarding
1. Completar documentación de RRHH el primer día
2. Recibir equipo informático y credenciales de acceso
3. Asistir a sesión de orientación sobre políticas de empresa
4. Reunión con supervisor para definir objetivos
5. Asignación de mentor durante los primeros 3 meses

## Procedimiento de Solicitud de Vacaciones
1. Acceder al portal de empleados
2. Seleccionar fechas deseadas (mínimo 15 días de antelación)
3. Obtener aprobación del supervisor directo
4. Confirmar cobertura de responsabilidades durante ausencia
5. Recibir confirmación por email

## Procedimiento de Trabajo Remoto
1. Solicitar aprobación con 48 horas de antelación
2. Confirmar disponibilidad de herramientas necesarias
3. Establecer horarios de disponibilidad
4. Reportar progreso diario al supervisor
5. Participar en reuniones virtuales según calendario

## Procedimiento de Emergencias
1. Evacuar el edificio por las salidas de emergencia señalizadas
2. Dirigirse al punto de encuentro en el parking
3. Esperar instrucciones del coordinador de emergencias
4. No usar ascensores durante evacuación
5. Reportar personas desaparecidas al coordinador

## Procedimiento de Solicitud de Formación
1. Identificar necesidad de formación con supervisor
2. Buscar cursos apropiados dentro del presupuesto asignado
3. Completar formulario de solicitud de formación
4. Obtener aprobación de RRHH y supervisor
5. Compartir conocimientos adquiridos con el equipo
`
      }
    ];
    
    // Crear archivos si no existen e indexarlos
    for (const doc of documents) {
      const filePath = path.join(sampleDocsPath, doc.filename);
      
      try {
        await fs.access(filePath);
        console.log(`📄 Documento ya existe: ${doc.filename}`);
      } catch {
        console.log(`📝 Creando documento: ${doc.filename}`);
        await fs.writeFile(filePath, doc.content, 'utf8');
      }
      
      // Indexar el documento
      try {
        console.log(`🔍 Indexando: ${doc.filename}`);
        const result = await indexDocument(filePath, 'text/markdown', {
          originalName: doc.filename,
          source: 'sample_data',
          uploadedAt: new Date().toISOString()
        });
        console.log(`✅ Indexado: ${doc.filename} (${result.totalChunks} chunks)`);
      } catch (error) {
        console.warn(`⚠️  Error indexando ${doc.filename}:`, error.message);
      }
    }
    
    console.log('✅ Inicialización de datos de ejemplo completada');
    
  } catch (error) {
    console.error('❌ Error inicializando datos de ejemplo:', error);
  }
}

// Ejecutar si es llamado directamente
if (import.meta.url === `file://${process.argv[1]}`) {
  initializeSampleData();
}

export { initializeSampleData };
