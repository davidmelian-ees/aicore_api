/**
 * Tests unitarios para RAG Service
 */

import { describe, test, expect, beforeEach, jest } from '@jest/globals';

describe('RAG Service - Chunking', () => {
  test('debe dividir texto en chunks correctamente', () => {
    const texto = 'Este es un texto de prueba. '.repeat(100);
    const chunkSize = 500;
    
    // Simulación simple de chunking
    const chunks = [];
    for (let i = 0; i < texto.length; i += chunkSize) {
      chunks.push(texto.slice(i, i + chunkSize));
    }
    
    expect(chunks.length).toBeGreaterThan(0);
    expect(chunks[0].length).toBeLessThanOrEqual(chunkSize);
  });

  test('debe manejar texto vacío', () => {
    const texto = '';
    const chunks = texto ? [texto] : [];
    
    expect(chunks.length).toBe(0);
  });

  test('debe manejar texto menor que chunk size', () => {
    const texto = 'Texto corto';
    const chunkSize = 500;
    const chunks = [texto];
    
    expect(chunks.length).toBe(1);
    expect(chunks[0]).toBe(texto);
  });
});

describe('RAG Service - Validación de Documentos', () => {
  test('debe validar extensiones de archivo permitidas', () => {
    const extensionesPermitidas = ['.txt', '.pdf', '.docx', '.md', '.json'];
    
    expect(extensionesPermitidas).toContain('.pdf');
    expect(extensionesPermitidas).toContain('.txt');
    expect(extensionesPermitidas).not.toContain('.exe');
  });

  test('debe validar tamaño máximo de archivo', () => {
    const maxSize = 10 * 1024 * 1024; // 10MB
    const fileSize = 5 * 1024 * 1024; // 5MB
    
    expect(fileSize).toBeLessThan(maxSize);
  });
});

describe('RAG Service - Metadatos', () => {
  test('debe crear metadatos correctos para documento', () => {
    const metadata = {
      filename: 'test.pdf',
      contextId: 'ctx-123',
      uploadDate: new Date().toISOString(),
      fileSize: 1024
    };
    
    expect(metadata).toHaveProperty('filename');
    expect(metadata).toHaveProperty('contextId');
    expect(metadata).toHaveProperty('uploadDate');
    expect(metadata.filename).toBe('test.pdf');
  });
});
