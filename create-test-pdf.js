// Script para crear un PDF de prueba simple
import fs from 'fs';

// Crear contenido de prueba que simule un pliego
const pliegoContent = `PLIEGO DE CONDICIONES TÉCNICAS
DESARROLLO DE APLICACIÓN WEB

1. OBJETO DEL CONTRATO
El presente pliego tiene por objeto establecer las condiciones técnicas para la contratación de servicios de desarrollo de una aplicación web para gestión de documentos.

CARACTERÍSTICAS PRINCIPALES:
- Sistema de autenticación de usuarios
- Gestión de documentos PDF y otros formatos
- Búsqueda avanzada con tecnología RAG
- Interfaz responsive y moderna
- API REST completa

2. REQUISITOS TÉCNICOS

2.1 TECNOLOGÍAS OBLIGATORIAS:
- Frontend: React.js v18+ o Vue.js v3+
- Backend: Node.js v18+ con Express.js
- Base de datos: PostgreSQL v14+ o MongoDB v6+
- Autenticación: JWT o OAuth2
- Hosting: Compatible con Cloud (AWS, Azure, GCP)

2.2 ESPECIFICACIONES TÉCNICAS:
- Responsive design (mobile-first)
- Soporte para navegadores modernos
- Tiempo de carga < 3 segundos
- Disponibilidad 99.9%
- Cifrado HTTPS obligatorio

2.3 INTEGRACIONES REQUERIDAS:
- Sistema de almacenamiento en la nube
- Servicio de notificaciones por email
- API de procesamiento de documentos
- Sistema de backup automático

3. PRESUPUESTO Y CONDICIONES ECONÓMICAS

3.1 PRESUPUESTO MÁXIMO:
El presupuesto máximo para este proyecto es de 45.000€ (IVA incluido).

3.2 FORMA DE PAGO:
- 30% al inicio del proyecto
- 40% en la entrega de la versión beta
- 30% en la entrega final y puesta en producción

3.3 GARANTÍAS:
- Garantía de funcionamiento: 12 meses
- Soporte técnico incluido: 6 meses
- Mantenimiento correctivo: 3 meses

4. PLAZO DE EJECUCIÓN

4.1 DURACIÓN TOTAL:
El plazo máximo de ejecución será de 4 meses desde la firma del contrato.

4.2 HITOS PRINCIPALES:
- Mes 1: Análisis, diseño y arquitectura
- Mes 2: Desarrollo del frontend
- Mes 3: Desarrollo del backend y APIs
- Mes 4: Testing, optimización y despliegue

4.3 ENTREGAS:
- Entrega 1 (Mes 1): Documentación técnica y mockups
- Entrega 2 (Mes 2): Prototipo funcional del frontend
- Entrega 3 (Mes 3): Versión beta completa
- Entrega 4 (Mes 4): Versión final en producción

5. CRITERIOS DE EVALUACIÓN

5.1 FACTORES DE PUNTUACIÓN:
- Propuesta técnica (50%): Máximo 50 puntos
  * Arquitectura y diseño: 20 puntos
  * Tecnologías propuestas: 15 puntos
  * Metodología de desarrollo: 15 puntos

- Propuesta económica (30%): Máximo 30 puntos
  * Relación calidad-precio
  * Desglose detallado de costos

- Experiencia del equipo (20%): Máximo 20 puntos
  * Proyectos similares realizados
  * Certificaciones del equipo
  * Referencias de clientes

5.2 PUNTUACIÓN MÍNIMA:
Para ser considerada, la propuesta debe obtener un mínimo de 60 puntos sobre 100.

6. DOCUMENTACIÓN REQUERIDA

6.1 DOCUMENTACIÓN TÉCNICA:
- Memoria técnica detallada (máximo 20 páginas)
- Arquitectura del sistema con diagramas
- Plan de desarrollo y metodología
- Plan de testing y calidad
- Documentación de APIs

6.2 DOCUMENTACIÓN EMPRESARIAL:
- Propuesta económica detallada
- Referencias de proyectos similares (mínimo 3)
- CV del equipo técnico
- Certificados de empresa y seguros

6.3 DOCUMENTACIÓN LEGAL:
- Declaración responsable de cumplimiento
- Certificado de estar al corriente de pagos
- Póliza de responsabilidad civil
- Compromiso de confidencialidad

7. CONDICIONES DE PARTICIPACIÓN

7.1 REQUISITOS MÍNIMOS:
- Empresa constituida con al menos 2 años de antigüedad
- Facturación mínima de 100.000€ en los últimos 2 años
- Equipo técnico de al menos 3 desarrolladores
- Experiencia demostrable en proyectos similares

7.2 CAUSAS DE EXCLUSIÓN:
- Documentación incompleta o fuera de plazo
- No cumplimiento de requisitos mínimos
- Propuestas que superen el presupuesto máximo
- Antecedentes de incumplimiento contractual

8. PROCESO DE SELECCIÓN

8.1 FASES DEL PROCESO:
1. Recepción de propuestas: 15 días naturales
2. Evaluación técnica: 10 días hábiles
3. Presentaciones orales: 5 días hábiles
4. Decisión final: 5 días hábiles

8.2 COMUNICACIONES:
Todas las comunicaciones se realizarán por correo electrónico a la dirección: contratos@empresa.com

8.3 ADJUDICACIÓN:
La adjudicación se realizará a la propuesta que obtenga la mayor puntuación total, siempre que supere la puntuación mínima establecida.

9. CONDICIONES ESPECIALES

9.1 PROPIEDAD INTELECTUAL:
Todo el código desarrollado será propiedad exclusiva del contratante.

9.2 CONFIDENCIALIDAD:
El adjudicatario se compromete a mantener la confidencialidad de toda la información del proyecto.

9.3 MODIFICACIONES:
Cualquier modificación al alcance del proyecto deberá ser aprobada por escrito por ambas partes.

Este pliego constituye la base técnica y legal para la contratación del servicio descrito.`;

// Guardar como archivo de texto (simula un PDF para pruebas)
fs.writeFileSync('pliego-test.txt', pliegoContent, 'utf8');

console.log('✅ Archivo de prueba creado: pliego-test.txt');
console.log('📄 Contenido:', pliegoContent.length, 'caracteres');
console.log('');
console.log('💡 Para probar con Postman:');
console.log('   1. Renombra el archivo a pliego-test.pdf');
console.log('   2. O usa cualquier PDF real que tengas');
console.log('   3. Usa uno de los prompts de ejemplo');
console.log('');
console.log('🧪 Para test automático:');
console.log('   node test-pliego-endpoint.js');
