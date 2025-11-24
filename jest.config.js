export default {
  // Usar módulos ES6
  testEnvironment: 'node',
  
  // Extensiones de archivos
  moduleFileExtensions: ['js', 'json'],
  
  // Patrón de archivos de test
  testMatch: [
    '**/tests/**/*.test.js',
    '**/__tests__/**/*.js'
  ],
  
  // Cobertura de código
  collectCoverageFrom: [
    'services/**/*.js',
    'routes/**/*.js',
    'utils/**/*.js',
    '!**/node_modules/**',
    '!**/tests/**',
    '!**/__tests__/**'
  ],
  
  // Umbral de cobertura
  coverageThreshold: {
    global: {
      branches: 50,
      functions: 50,
      lines: 50,
      statements: 50
    }
  },
  
  // Directorio de reportes
  coverageDirectory: 'coverage',
  
  // Formatos de reporte
  coverageReporters: ['text', 'lcov', 'html', 'json'],
  
  // Timeout para tests
  testTimeout: 10000,
  
  // Variables de entorno para tests
  setupFiles: ['<rootDir>/tests/setup.js'],
  
  // Transformaciones (no necesarias para ES modules nativos)
  transform: {},
  
  // Verbose output
  verbose: true
};
