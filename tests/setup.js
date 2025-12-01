/**
 * Setup global para tests
 * Configura variables de entorno y mocks necesarios
 */

// Variables de entorno para testing
process.env.NODE_ENV = 'test';
process.env.VECTOR_STORE_TYPE = 'sqlite';
process.env.PORT = 4001; // Puerto diferente para tests

// Mock de console para tests más limpios (opcional)
global.console = {
  ...console,
  // Descomenta para silenciar logs en tests
  // log: jest.fn(),
  // debug: jest.fn(),
  // info: jest.fn(),
  // warn: jest.fn(),
  error: console.error, // Mantener errores visibles
};

// Timeout global
jest.setTimeout(10000);
