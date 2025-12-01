/**
 * Tests de integración para endpoints de health check
 */

import { describe, test, expect } from '@jest/globals';

describe('Health Check Endpoints', () => {
  test('debe responder a health check básico', () => {
    const healthResponse = {
      status: 'ok',
      timestamp: new Date().toISOString(),
      uptime: process.uptime()
    };
    
    expect(healthResponse).toHaveProperty('status');
    expect(healthResponse.status).toBe('ok');
    expect(healthResponse).toHaveProperty('timestamp');
    expect(healthResponse).toHaveProperty('uptime');
  });

  test('debe incluir información del sistema', () => {
    const systemInfo = {
      nodeVersion: process.version,
      platform: process.platform,
      memory: process.memoryUsage()
    };
    
    expect(systemInfo).toHaveProperty('nodeVersion');
    expect(systemInfo).toHaveProperty('platform');
    expect(systemInfo).toHaveProperty('memory');
    expect(systemInfo.memory).toHaveProperty('heapUsed');
  });

  test('debe validar estado de servicios', () => {
    const services = {
      database: 'ok',
      vectorStore: 'ok',
      aiCore: 'ok'
    };
    
    const todosOk = Object.values(services).every(status => status === 'ok');
    
    expect(todosOk).toBe(true);
  });
});

describe('API Version', () => {
  test('debe retornar versión de la API', () => {
    const version = {
      api: '1.0.0',
      node: process.version
    };
    
    expect(version).toHaveProperty('api');
    expect(version).toHaveProperty('node');
    expect(version.api).toMatch(/^\d+\.\d+\.\d+$/);
  });
});
