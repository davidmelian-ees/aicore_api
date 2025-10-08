# Manual Técnico - Sistema de Gestión de Inventario

## Tabla de Contenidos
1. [Introducción](#introducción)
2. [Arquitectura del Sistema](#arquitectura-del-sistema)
3. [Instalación](#instalación)
4. [Configuración](#configuración)
5. [API Reference](#api-reference)
6. [Troubleshooting](#troubleshooting)

## Introducción

El Sistema de Gestión de Inventario (SGI) es una aplicación web desarrollada en Node.js que permite gestionar el inventario de productos de manera eficiente. El sistema incluye funcionalidades de:

- Gestión de productos
- Control de stock
- Alertas de inventario bajo
- Reportes y analytics
- Integración con sistemas externos

### Tecnologías Utilizadas
- **Backend**: Node.js 18.x, Express.js 4.x
- **Base de Datos**: PostgreSQL 14.x
- **Frontend**: React 18.x, Material-UI
- **Autenticación**: JWT + OAuth 2.0
- **Documentación**: Swagger/OpenAPI 3.0

## Arquitectura del Sistema

### Componentes Principales

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Frontend      │    │   API Gateway   │    │   Microservicios│
│   (React)       │◄──►│   (Express)     │◄──►│   (Node.js)     │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                │
                                ▼
                       ┌─────────────────┐
                       │   Base de Datos │
                       │   (PostgreSQL)  │
                       └─────────────────┘
```

### Microservicios

1. **Product Service**: Gestión de productos y categorías
2. **Inventory Service**: Control de stock y movimientos
3. **Alert Service**: Sistema de notificaciones
4. **Report Service**: Generación de reportes
5. **User Service**: Gestión de usuarios y autenticación

## Instalación

### Prerrequisitos

- Node.js 18.x o superior
- PostgreSQL 14.x
- Redis 6.x (para caché)
- Docker (opcional)

### Instalación Local

```bash
# Clonar el repositorio
git clone https://github.com/empresa/sgi-sistema.git
cd sgi-sistema

# Instalar dependencias
npm install

# Configurar variables de entorno
cp .env.example .env
# Editar .env con tus configuraciones

# Ejecutar migraciones de base de datos
npm run migrate

# Iniciar el servidor de desarrollo
npm run dev
```

### Instalación con Docker

```bash
# Construir y ejecutar con Docker Compose
docker-compose up -d

# Verificar que todos los servicios estén corriendo
docker-compose ps
```

## Configuración

### Variables de Entorno

```env
# Base de datos
DATABASE_URL=postgresql://user:password@localhost:5432/sgi_db
DATABASE_POOL_SIZE=20

# Redis
REDIS_URL=redis://localhost:6379
REDIS_TTL=3600

# Autenticación
JWT_SECRET=your-super-secret-key
JWT_EXPIRATION=24h
OAUTH_CLIENT_ID=your-oauth-client-id
OAUTH_CLIENT_SECRET=your-oauth-client-secret

# API
PORT=3000
API_VERSION=v1
RATE_LIMIT_REQUESTS=100
RATE_LIMIT_WINDOW=900000

# Notificaciones
EMAIL_SERVICE=sendgrid
EMAIL_API_KEY=your-sendgrid-api-key
SLACK_WEBHOOK_URL=your-slack-webhook-url

# Logging
LOG_LEVEL=info
LOG_FORMAT=json
```

### Configuración de Base de Datos

```sql
-- Crear base de datos
CREATE DATABASE sgi_db;

-- Crear usuario
CREATE USER sgi_user WITH PASSWORD 'secure_password';
GRANT ALL PRIVILEGES ON DATABASE sgi_db TO sgi_user;

-- Configurar extensiones necesarias
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pg_trgm";
```

## API Reference

### Autenticación

Todas las rutas protegidas requieren un token JWT en el header:
```
Authorization: Bearer <jwt_token>
```

### Endpoints Principales

#### Productos

**GET /api/v1/products**
```json
{
  "page": 1,
  "limit": 20,
  "search": "optional",
  "category": "optional",
  "status": "active|inactive"
}
```

**POST /api/v1/products**
```json
{
  "name": "Producto Ejemplo",
  "description": "Descripción del producto",
  "sku": "PROD-001",
  "category_id": "uuid",
  "price": 29.99,
  "cost": 15.00,
  "min_stock": 10,
  "max_stock": 100
}
```

**PUT /api/v1/products/:id**
```json
{
  "name": "Producto Actualizado",
  "price": 34.99,
  "status": "active"
}
```

#### Inventario

**GET /api/v1/inventory/stock**
```json
{
  "product_id": "uuid",
  "location": "warehouse_a"
}
```

**POST /api/v1/inventory/movement**
```json
{
  "product_id": "uuid",
  "type": "in|out|adjustment",
  "quantity": 50,
  "location": "warehouse_a",
  "reason": "purchase|sale|adjustment",
  "notes": "Opcional"
}
```

#### Reportes

**GET /api/v1/reports/stock-levels**
```json
{
  "date_from": "2024-01-01",
  "date_to": "2024-01-31",
  "format": "json|csv|pdf"
}
```

### Códigos de Respuesta

- `200` - OK
- `201` - Created
- `400` - Bad Request
- `401` - Unauthorized
- `403` - Forbidden
- `404` - Not Found
- `422` - Validation Error
- `500` - Internal Server Error

## Troubleshooting

### Problemas Comunes

#### Error de Conexión a Base de Datos
```
Error: connect ECONNREFUSED 127.0.0.1:5432
```

**Solución:**
1. Verificar que PostgreSQL esté corriendo
2. Comprobar las credenciales en `.env`
3. Verificar que el puerto 5432 esté disponible

#### Error de Autenticación JWT
```
Error: JsonWebTokenError: invalid token
```

**Solución:**
1. Verificar que el token no haya expirado
2. Comprobar que `JWT_SECRET` sea correcto
3. Verificar formato del header Authorization

#### Performance Lenta en Consultas
```
Query took 5000ms to execute
```

**Solución:**
1. Revisar índices en base de datos
2. Optimizar consultas SQL
3. Implementar caché con Redis
4. Revisar configuración de pool de conexiones

### Logs y Monitoreo

#### Configuración de Logs
```javascript
// logger.js
const winston = require('winston');

const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  transports: [
    new winston.transports.File({ filename: 'logs/error.log', level: 'error' }),
    new winston.transports.File({ filename: 'logs/combined.log' })
  ]
});
```

#### Métricas de Performance
- Tiempo de respuesta promedio: < 200ms
- Throughput: > 1000 requests/min
- Error rate: < 1%
- Uptime: > 99.9%

### Contacto de Soporte

Para soporte técnico:
- **Email**: soporte-tecnico@empresa.com
- **Slack**: #sgi-support
- **Documentación**: https://docs.empresa.com/sgi
- **Issues**: https://github.com/empresa/sgi-sistema/issues

---

**Última actualización**: 10 de octubre de 2024  
**Versión del documento**: 2.1  
**Autor**: Equipo de Desarrollo SGI
