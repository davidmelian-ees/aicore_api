# Sistema de Historial de Chats

## Descripción General

Sistema completo de persistencia de conversaciones por usuario que permite almacenar, consultar y gestionar el historial de chats del sistema RAG.

## Características Principales

### 1. **Persistencia Automática**
- Cada conversación se guarda automáticamente en SQLite
- Sesiones organizadas por usuario y contexto
- Mensajes almacenados con metadata completa

### 2. **Gestión de Sesiones**
- Creación automática de sesiones al iniciar chat
- Títulos editables para organización
- Contador de mensajes por sesión
- Timestamps de creación y última actualización

### 3. **Interfaz Visual Moderna**
- Vista de historial con filtros por contexto
- Estadísticas de uso (total conversaciones y mensajes)
- Edición inline de títulos
- Eliminación de sesiones con confirmación

## Arquitectura Backend

### Base de Datos (SQLite)

**Ubicación:** `./data/chat_history.db`

**Tablas:**

```sql
-- Sesiones de chat
CREATE TABLE chat_sessions (
  id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL,
  context_id TEXT NOT NULL,
  title TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  message_count INTEGER DEFAULT 0
);

-- Mensajes de chat
CREATE TABLE chat_messages (
  id TEXT PRIMARY KEY,
  session_id TEXT NOT NULL,
  role TEXT NOT NULL,
  content TEXT NOT NULL,
  timestamp TEXT NOT NULL,
  metadata TEXT,
  FOREIGN KEY (session_id) REFERENCES chat_sessions(id) ON DELETE CASCADE
);
```

### Servicio: `chatHistoryService.js`

**Métodos principales:**

```javascript
// Crear nueva sesión
createSession(userId, contextId, title = null)

// Añadir mensaje a sesión
addMessage(sessionId, role, content, metadata = null)

// Obtener sesiones de usuario
getUserSessions(userId, contextId = null)

// Obtener mensajes de sesión
getSessionMessages(sessionId)

// Actualizar título de sesión
updateSessionTitle(sessionId, newTitle)

// Eliminar sesión
deleteSession(sessionId)

// Estadísticas de usuario
getUserStats(userId)

// Limpiar sesiones antiguas
cleanOldSessions(daysOld = 30)
```

## API Endpoints

### **POST** `/api/chat-history/sessions`
Crear nueva sesión de chat

**Body:**
```json
{
  "userId": "user@example.com",
  "contextId": "context_123",
  "title": "Conversación sobre pliegos" // Opcional
}
```

**Response:**
```json
{
  "success": true,
  "session": {
    "id": "session_1234567890_abc123",
    "userId": "user@example.com",
    "contextId": "context_123",
    "title": "Conversación sobre pliegos",
    "createdAt": "2024-01-20T10:30:00.000Z",
    "updatedAt": "2024-01-20T10:30:00.000Z",
    "messageCount": 0
  }
}
```

### **GET** `/api/chat-history/sessions`
Obtener sesiones de usuario

**Query Params:**
- `userId` (requerido): ID del usuario
- `contextId` (opcional): Filtrar por contexto

**Response:**
```json
{
  "success": true,
  "sessions": [...],
  "count": 5
}
```

### **GET** `/api/chat-history/sessions/:sessionId`
Obtener sesión específica con mensajes

**Response:**
```json
{
  "success": true,
  "session": {...},
  "messages": [
    {
      "id": "msg_123",
      "role": "user",
      "content": "¿Qué es un pliego?",
      "timestamp": "2024-01-20T10:31:00.000Z",
      "metadata": null
    },
    {
      "id": "msg_124",
      "role": "assistant",
      "content": "Un pliego es...",
      "timestamp": "2024-01-20T10:31:05.000Z",
      "metadata": {
        "chunksUsed": 3,
        "sources": ["doc1.pdf"]
      }
    }
  ]
}
```

### **POST** `/api/chat-history/sessions/:sessionId/messages`
Añadir mensaje a sesión

**Body:**
```json
{
  "role": "user",
  "content": "Mensaje del usuario",
  "metadata": {} // Opcional
}
```

### **PATCH** `/api/chat-history/sessions/:sessionId`
Actualizar título de sesión

**Body:**
```json
{
  "title": "Nuevo título"
}
```

### **DELETE** `/api/chat-history/sessions/:sessionId`
Eliminar sesión

### **GET** `/api/chat-history/stats/:userId`
Obtener estadísticas de usuario

**Response:**
```json
{
  "success": true,
  "stats": {
    "total_sessions": 10,
    "total_messages": 150,
    "last_activity": "2024-01-20T15:30:00.000Z"
  }
}
```

### **POST** `/api/chat-history/cleanup`
Limpiar sesiones antiguas

**Body:**
```json
{
  "daysOld": 30 // Opcional, default 30
}
```

## Integración Frontend

### Componente: `ChatHistory.jsx`

**Características:**
- Lista de conversaciones con metadata
- Filtro por contexto
- Edición de títulos inline
- Eliminación con confirmación
- Estadísticas de uso
- Carga de sesiones en el chat

**Props:**
```javascript
<ChatHistory 
  apiBaseUrl="https://api.example.com"
  onLoadSession={(session, messages) => {
    // Callback al cargar una sesión
  }}
/>
```

### Componente: `ChatInterface.jsx`

**Guardado automático:**
```javascript
// Al enviar mensaje
saveMessageToHistory('user', userMessage.content);

// Al recibir respuesta
saveMessageToHistory('assistant', data.answer, data.metadata);
```

## Flujo de Uso

### 1. **Inicio de Conversación**
```
Usuario selecciona contexto
  ↓
ChatInterface crea nueva sesión
  ↓
Se genera ID único y título automático
  ↓
Sesión lista para recibir mensajes
```

### 2. **Durante la Conversación**
```
Usuario envía mensaje
  ↓
Mensaje guardado en historial
  ↓
IA procesa y responde
  ↓
Respuesta guardada con metadata
  ↓
Contador de mensajes actualizado
```

### 3. **Consulta de Historial**
```
Usuario accede a "Historial de Chats"
  ↓
Sistema carga sesiones del usuario
  ↓
Usuario puede filtrar por contexto
  ↓
Click en sesión carga conversación completa
```

## Sidebar Moderno

### Características del Sidebar

**Diseño:**
- Colapsable con hover (70px → 280px)
- Iconos grandes y visibles
- Transiciones suaves
- Indicador visual de vista activa

**Menú de Navegación:**
- 🗂️ Dashboard RAG
- 💬 Historial de Chats
- 📄 Procesador de Pliegos
- 📋 Análisis de Pliego
- 📊 Analytics
- 💾 Backups

**Footer:**
- Información de usuario
- Avatar y email
- Visible solo cuando está expandido

### Estilos CSS

**Estados:**
```css
.sidebar.collapsed { width: 70px; }
.sidebar.expanded { width: 280px; }
```

**Animaciones:**
- Expansión suave (0.3s cubic-bezier)
- Slide-in de contenido
- Hover effects en items

## Mejores Prácticas

### 1. **Gestión de Sesiones**
```javascript
// Crear sesión al cambiar de contexto
useEffect(() => {
  if (contextId) {
    createNewSession();
  }
}, [contextId]);
```

### 2. **Guardado de Mensajes**
```javascript
// Siempre verificar que existe sesión activa
if (!currentSessionId) return;

// Guardar con metadata cuando esté disponible
saveMessageToHistory('assistant', content, {
  chunksUsed: 3,
  sources: ['doc1.pdf'],
  model: 'gpt-4o'
});
```

### 3. **Limpieza de Datos**
```javascript
// Ejecutar periódicamente (ej: cron job)
chatHistoryService.cleanOldSessions(30); // Eliminar > 30 días
```

## Mantenimiento

### Backup de Base de Datos

```bash
# Copiar base de datos
cp ./data/chat_history.db ./backups/chat_history_backup.db

# Restaurar desde backup
cp ./backups/chat_history_backup.db ./data/chat_history.db
```

### Consultas SQL Útiles

```sql
-- Ver sesiones recientes
SELECT * FROM chat_sessions 
ORDER BY updated_at DESC 
LIMIT 10;

-- Contar mensajes por usuario
SELECT user_id, SUM(message_count) as total_messages
FROM chat_sessions
GROUP BY user_id;

-- Sesiones sin mensajes
SELECT * FROM chat_sessions 
WHERE message_count = 0;

-- Eliminar sesiones antiguas
DELETE FROM chat_sessions 
WHERE updated_at < datetime('now', '-30 days');
```

## Troubleshooting

### Problema: Sesiones no se guardan
**Solución:** Verificar que el directorio `./data` existe y tiene permisos de escritura

### Problema: Mensajes duplicados
**Solución:** Verificar que `currentSessionId` está correctamente inicializado

### Problema: Base de datos bloqueada
**Solución:** Cerrar todas las conexiones activas antes de operaciones de escritura

## Próximas Mejoras

- [ ] Búsqueda de texto en historial
- [ ] Exportar conversaciones a PDF/TXT
- [ ] Compartir sesiones entre usuarios
- [ ] Tags y categorías para sesiones
- [ ] Análisis de sentimiento en conversaciones
- [ ] Resúmenes automáticos de sesiones largas
