/**
 * Tests unitarios para utilidades de validación
 */

import { describe, test, expect } from '@jest/globals';

describe('Validación de Archivos', () => {
  test('debe validar extensiones de archivo', () => {
    const validarExtension = (filename, extensionesPermitidas) => {
      const ext = filename.toLowerCase().split('.').pop();
      return extensionesPermitidas.includes(`.${ext}`);
    };
    
    const extensionesPermitidas = ['.pdf', '.txt', '.docx'];
    
    expect(validarExtension('documento.pdf', extensionesPermitidas)).toBe(true);
    expect(validarExtension('texto.txt', extensionesPermitidas)).toBe(true);
    expect(validarExtension('archivo.exe', extensionesPermitidas)).toBe(false);
  });

  test('debe validar tamaño de archivo', () => {
    const validarTamano = (size, maxSize) => size <= maxSize;
    const maxSize = 10 * 1024 * 1024; // 10MB
    
    expect(validarTamano(5 * 1024 * 1024, maxSize)).toBe(true);
    expect(validarTamano(15 * 1024 * 1024, maxSize)).toBe(false);
  });
});

describe('Validación de Strings', () => {
  test('debe validar que string no esté vacío', () => {
    const validarNoVacio = (str) => str && str.trim().length > 0;
    
    expect(validarNoVacio('texto')).toBe(true);
    expect(validarNoVacio('')).toBe(false);
    expect(validarNoVacio('   ')).toBe(false);
    expect(validarNoVacio(null)).toBe(false);
  });

  test('debe sanitizar strings', () => {
    const sanitizar = (str) => str.trim().replace(/[<>]/g, '');
    
    expect(sanitizar('  texto  ')).toBe('texto');
    expect(sanitizar('<script>alert()</script>')).toBe('scriptalert()/script');
  });
});

describe('Validación de IDs', () => {
  test('debe validar formato de UUID', () => {
    const validarUUID = (id) => {
      const uuidRegex = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
      return uuidRegex.test(id);
    };
    
    expect(validarUUID('123e4567-e89b-12d3-a456-426614174000')).toBe(true);
    expect(validarUUID('invalid-uuid')).toBe(false);
    expect(validarUUID('12345')).toBe(false);
  });
});

describe('Validación de Contextos', () => {
  test('debe validar nombre de contexto', () => {
    const validarNombreContexto = (nombre) => {
      return nombre && 
             nombre.length >= 3 && 
             nombre.length <= 50 &&
             /^[a-zA-Z0-9\s\-_]+$/.test(nombre);
    };
    
    expect(validarNombreContexto('Contexto Válido')).toBe(true);
    expect(validarNombreContexto('AB')).toBe(false); // Muy corto
    expect(validarNombreContexto('A'.repeat(51))).toBe(false); // Muy largo
    expect(validarNombreContexto('Contexto@Inválido')).toBe(false); // Caracteres no permitidos
  });
});

describe('Validación de Fechas', () => {
  test('debe validar formato ISO de fecha', () => {
    const validarFechaISO = (fecha) => {
      const date = new Date(fecha);
      return date instanceof Date && !isNaN(date);
    };
    
    expect(validarFechaISO('2024-01-01T00:00:00Z')).toBe(true);
    expect(validarFechaISO('invalid-date')).toBe(false);
  });

  test('debe validar que fecha no sea futura', () => {
    const validarNoFutura = (fecha) => {
      const date = new Date(fecha);
      return date <= new Date();
    };
    
    const ayer = new Date(Date.now() - 86400000).toISOString();
    const manana = new Date(Date.now() + 86400000).toISOString();
    
    expect(validarNoFutura(ayer)).toBe(true);
    expect(validarNoFutura(manana)).toBe(false);
  });
});
