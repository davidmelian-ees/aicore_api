# GUÍA DE LOGOS EN PLIEGOS ADMINISTRATIVOS

## 📋 NORMATIVA OBLIGATORIA

### Logo Institucional Obligatorio

**TODOS los pliegos oficiales DEBEN incluir:**

✅ **Logo de "Infraestructuras de Cataluña"**
- **Ubicación**: Parte superior del documento (header)
- **Páginas**: Todas las páginas del documento
- **Tamaño**: Aproximadamente 2-3 cm de alto
- **Posición**: Centrado o alineado a la izquierda en el header
- **Calidad**: Alta resolución, formato vectorial preferiblemente

### ❌ ERRORES CRÍTICOS

Si un pliego oficial NO tiene el logo de "Infraestructuras de Cataluña" en el header:
- **Severidad**: CRÍTICA
- **Estado**: DOCUMENTO INCOMPLETO
- **Acción**: NO PUBLICAR hasta añadir el logo
- **Motivo**: Incumplimiento de normativa institucional

---

## 🏗️ TIPOS DE PLIEGOS Y SUS LOGOS

### 1. Pliegos de Obra Civil - Abierto

**Logos Obligatorios:**
- ✅ Logo "Infraestructuras de Cataluña" (header, todas las páginas)

**Logos Opcionales (según proyecto):**
- Logo del Ayuntamiento (footer izquierdo)
- Logo de la empresa contratista (footer derecho)
- Logos de certificaciones (footer)

**Ejemplo de estructura correcta:**
```
┌─────────────────────────────────────┐
│  [Logo Infraestructuras Cataluña]   │ ← OBLIGATORIO
├─────────────────────────────────────┤
│                                     │
│         Contenido del pliego        │
│                                     │
├─────────────────────────────────────┤
│ [Logo Ayto]    [Logo Empresa]      │ ← OPCIONAL
└─────────────────────────────────────┘
```

### 2. Pliegos de Obra Civil - Simplificado

**Logos Obligatorios:**
- ✅ Logo "Infraestructuras de Cataluña" (header, todas las páginas)

**Logos Opcionales:**
- Logo del Ayuntamiento (footer)

**Características:**
- Formato más simple que el abierto
- Menos logos adicionales
- Logo principal siempre presente

### 3. Pliegos de Edificación - Abierto

**Logos Obligatorios:**
- ✅ Logo "Infraestructuras de Cataluña" (header, todas las páginas)

**Logos Opcionales:**
- Logo del Ayuntamiento (footer centrado)
- Logo de certificación energética (footer derecho)
- Logo de arquitecto/ingeniero (footer izquierdo)

### 4. Pliegos de Edificación - Simplificado

**Logos Obligatorios:**
- ✅ Logo "Infraestructuras de Cataluña" (header, todas las páginas)

**Logos Opcionales:**
- Logo del Ayuntamiento (footer)

---

## 🔍 DETECCIÓN AUTOMÁTICA

### Criterios de Validación

El sistema automático detecta logos mediante:

1. **Análisis de imágenes en el PDF**
   - Identifica todas las imágenes del documento
   - Clasifica por posición: header, footer, body

2. **Validación de posición**
   - Header (85-100% altura de página): Logo obligatorio
   - Footer (0-15% altura de página): Logos opcionales
   - Body (15-85%): Contenido del documento

3. **Consistencia**
   - Logo debe aparecer en múltiples páginas
   - Posición consistente en todo el documento
   - Tamaño apropiado

### Niveles de Confianza

- **High (Alta)**: Logo detectado en 4+ páginas en header
- **Medium (Media)**: Logo detectado en 1-3 páginas en header
- **Low (Baja)**: Imágenes detectadas pero posición inconsistente
- **None (Ninguna)**: No se detectaron imágenes en header

---

## 📊 EJEMPLOS DE VALIDACIÓN

### ✅ EJEMPLO CORRECTO

**Archivo**: CT1078146_plec_edificacio_obert_amb_lots.pdf

**Análisis:**
- Logo "Infraestructuras de Cataluña": ✅ SÍ
- Ubicación: Header, páginas 1-50
- Confianza: Alta
- Logos adicionales: 2 en footer (Ayuntamiento, Empresa)
- **Estado**: VÁLIDO - Cumple normativa

**Descripción para IA:**
```
Este documento es un EJEMPLO PERFECTO de pliego con logos correctos.
Contiene el logo obligatorio de "Infraestructuras de Cataluña" en 
la parte superior de todas las páginas. Además incluye logos 
adicionales de Ayuntamiento y empresa en el pie de página.
Este es el FORMATO ESTÁNDAR que deben seguir todos los pliegos oficiales.
```

### ❌ EJEMPLO INCORRECTO

**Archivo**: CT1078147_plec_obra_civil_simplificat_sense_lots.pdf

**Análisis:**
- Logo "Infraestructuras de Cataluña": ❌ NO
- Ubicación: Ninguna imagen en header
- Confianza: Ninguna
- Logos adicionales: 0
- **Estado**: INVÁLIDO - NO cumple normativa

**Descripción para IA:**
```
Este documento es un EJEMPLO DE ERROR CRÍTICO.
NO contiene el logo obligatorio de "Infraestructuras de Cataluña".
Este pliego NO DEBE ser publicado en su estado actual.
ACCIÓN REQUERIDA: Añadir el logo institucional en el header 
de todas las páginas antes de cualquier uso oficial.
```

### ⚠️ EJEMPLO CON ADVERTENCIA

**Archivo**: CT1078148_plec_edificacio_simplificat_amb_lots.pdf

**Análisis:**
- Logo "Infraestructuras de Cataluña": ⚠️ PARCIAL
- Ubicación: Header, solo páginas 1-5
- Confianza: Media
- Logos adicionales: 1 en footer
- **Estado**: ADVERTENCIA - Revisar manualmente

**Descripción para IA:**
```
Este documento tiene el logo obligatorio pero NO en todas las páginas.
Se detectó el logo de "Infraestructuras de Cataluña" solo en las 
primeras 5 páginas. Debería estar en TODAS las páginas del documento.
ACCIÓN RECOMENDADA: Verificar manualmente y añadir logo a páginas faltantes.
```

---

## 🎯 INSTRUCCIONES PARA LA IA

### Al Validar un Pliego

1. **SIEMPRE verificar primero**: ¿Tiene logo en header?
2. **Si NO tiene logo en header**: ERROR CRÍTICO
3. **Si tiene logo en header**: Verificar consistencia
4. **Comparar con ejemplos**: Usar documentos de referencia del RAG

### Respuestas Esperadas

**Cuando detectes logo correcto:**
```
✅ VALIDACIÓN CORRECTA
El pliego contiene el logo obligatorio de "Infraestructuras de Cataluña" 
en la parte superior del documento. Cumple con la normativa institucional.
```

**Cuando NO detectes logo:**
```
❌ ERROR CRÍTICO DETECTADO
El pliego NO contiene el logo obligatorio de "Infraestructuras de Cataluña".
Este documento NO cumple con la normativa y NO debe ser publicado.
ACCIÓN REQUERIDA: Añadir el logo institucional antes de continuar.
```

**Cuando detectes logo parcial:**
```
⚠️ ADVERTENCIA
El logo institucional está presente pero no en todas las páginas requeridas.
Se recomienda revisión manual para asegurar cumplimiento completo.
```

---

## 📚 CONTEXTOS RAG RECOMENDADOS

### Para Organizar Documentos

1. **PLIEGOS_CON_LOGOS_CORRECTOS**
   - Documentos que cumplen 100% la normativa
   - Usar como referencia para validaciones
   - Ejemplos perfectos para entrenar la IA

2. **PLIEGOS_SIN_LOGOS_ERROR**
   - Documentos que NO cumplen la normativa
   - Ejemplos de lo que NO se debe hacer
   - Requieren corrección antes de uso

3. **PLIEGOS_LOGOS_PARCIALES**
   - Documentos con logos pero inconsistentes
   - Requieren revisión manual
   - Casos especiales

4. **PLANTILLAS_BASE**
   - Plantillas sin personalizar
   - Pueden no tener logos (es correcto)
   - Marcadores {LOGO_AQUI} en lugar de logos

---

## 🔧 SOLUCIÓN DE PROBLEMAS

### Problema: "No se detectan logos pero visualmente están presentes"

**Posibles causas:**
- Logo es texto en lugar de imagen
- Logo está incrustado como forma vectorial no estándar
- PDF tiene capas o protección

**Solución:**
- Revisar manualmente el PDF
- Regenerar PDF con logos como imágenes estándar
- Usar herramientas de edición PDF profesionales

### Problema: "Se detectan muchas imágenes pero no son logos"

**Posibles causas:**
- Documento contiene fotos, diagramas, planos
- Imágenes decorativas o de contenido

**Solución:**
- El sistema filtra por posición (header vs body)
- Solo imágenes en header se consideran logos potenciales
- Imágenes en body se ignoran para validación de logo obligatorio

---

## 📝 NOTAS IMPORTANTES

1. **El logo de "Infraestructuras de Cataluña" es SIEMPRE obligatorio**
   - No hay excepciones para documentos oficiales
   - Plantillas base pueden no tenerlo (están sin personalizar)

2. **Otros logos son opcionales**
   - Dependen del proyecto específico
   - No afectan la validación principal

3. **La posición importa**
   - Logo obligatorio: HEADER (parte superior)
   - Logos opcionales: FOOTER (parte inferior)

4. **Consistencia es clave**
   - Logo debe estar en TODAS las páginas
   - Misma posición y tamaño en todo el documento

---

**Última actualización**: Noviembre 2025
**Versión**: 1.0
**Autor**: Sistema de Validación Automática de Pliegos
