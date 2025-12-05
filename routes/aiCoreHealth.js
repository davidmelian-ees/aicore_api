/**
 * Health Check para SAP AI Core
 * Endpoint: GET /api/ai-core/health
 */

import express from 'express';
import { getAiCoreClient } from '../auth/aiCoreClient.js';
import loggerService from '../services/loggerService.js';

const router = express.Router();

/**
 * GET /api/ai-core/health
 * Verifica la conexión con SAP AI Core
 */
router.get('/health', async (req, res) => {
  const startTime = Date.now();
  
  try {
    console.log('[AI-CORE-HEALTH] 🔍 Verificando conexión con SAP AI Core...');
    
    // 1. Inicializar cliente
    const client = getAiCoreClient('gpt-4o', { 
      temperature: 0.2, 
      maxTokens: 50 
    });
    
    console.log('[AI-CORE-HEALTH] ✅ Cliente inicializado');
    
    // 2. Enviar prompt de prueba simple
    const testPrompt = 'Responde solo con "OK"';
    const aiStartTime = Date.now();
    
    const response = await client.run({
      messages: [{ role: 'user', content: testPrompt }],
      temperature: 0.2,
      max_tokens: 50
    });
    
    const aiDuration = Date.now() - aiStartTime;
    const content = response.getContent();
    
    console.log('[AI-CORE-HEALTH] ✅ Respuesta recibida:', content);
    
    // 3. Verificar que la respuesta sea válida
    if (!content || content.trim().length === 0) {
      throw new Error('SAP AI Core devolvió una respuesta vacía');
    }
    
    const totalDuration = Date.now() - startTime;
    
    loggerService.success('AI-CORE-HEALTH', 'Conexión exitosa con SAP AI Core', {
      responseTime: `${aiDuration}ms`,
      totalTime: `${totalDuration}ms`,
      response: content
    });
    
    // 4. Respuesta exitosa
    res.status(200).json({
      success: true,
      status: 'healthy',
      message: 'SAP AI Core está funcionando correctamente',
      details: {
        model: 'gpt-4o',
        responseTime: `${aiDuration}ms`,
        totalTime: `${totalDuration}ms`,
        testResponse: content,
        timestamp: new Date().toISOString()
      }
    });
    
  } catch (error) {
    const totalDuration = Date.now() - startTime;
    
    console.error('[AI-CORE-HEALTH] ❌ Error en SAP AI Core:', error.message);
    
    loggerService.error('AI-CORE-HEALTH', 'Error de conexión con SAP AI Core', {
      error: error.message,
      status: error.status || error.code,
      duration: `${totalDuration}ms`
    });
    
    // Determinar el tipo de error
    let errorType = 'unknown';
    let errorMessage = error.message;
    let suggestions = [];
    
    if (error.message.includes('Failed to fetch the list of deployments')) {
      errorType = 'authentication';
      errorMessage = 'Error de autenticación con SAP AI Core';
      suggestions = [
        'Verifica que el servicio "default_aicore" esté bindeado',
        'Regenera las credenciales en BTP Cockpit',
        'Verifica que el deployment de gpt-4o esté activo'
      ];
    } else if (error.message.includes('timeout')) {
      errorType = 'timeout';
      errorMessage = 'Timeout al conectar con SAP AI Core';
      suggestions = [
        'Verifica la conectividad de red',
        'El deployment puede estar en estado "sleeping"'
      ];
    } else if (error.message.includes('deployment')) {
      errorType = 'deployment';
      errorMessage = 'Deployment no encontrado o inactivo';
      suggestions = [
        'Verifica que el deployment de gpt-4o exista en AI Launchpad',
        'Verifica que el deployment esté en estado RUNNING'
      ];
    }
    
    // 5. Respuesta de error
    res.status(503).json({
      success: false,
      status: 'unhealthy',
      message: errorMessage,
      error: {
        type: errorType,
        message: error.message,
        status: error.status || error.code || 'unknown',
        duration: `${totalDuration}ms`,
        timestamp: new Date().toISOString()
      },
      suggestions
    });
  }
});

/**
 * GET /api/ai-core/status
 * Información detallada del estado de SAP AI Core
 */
router.get('/status', async (req, res) => {
  try {
    const vcapServices = process.env.VCAP_SERVICES;
    const hasVcapServices = !!vcapServices;
    
    let aiCoreConfig = null;
    if (hasVcapServices) {
      try {
        const services = JSON.parse(vcapServices);
        aiCoreConfig = services.aicore?.[0];
      } catch (e) {
        console.error('[AI-CORE-HEALTH] Error parseando VCAP_SERVICES:', e);
      }
    }
    
    const status = {
      environment: process.env.NODE_ENV || 'development',
      vcapServicesAvailable: hasVcapServices,
      aiCoreServiceBound: !!aiCoreConfig,
      serviceDetails: aiCoreConfig ? {
        name: aiCoreConfig.name,
        plan: aiCoreConfig.plan,
        hasCredentials: !!aiCoreConfig.credentials,
        hasClientId: !!aiCoreConfig.credentials?.clientid,
        hasClientSecret: !!aiCoreConfig.credentials?.clientsecret,
        hasUrl: !!aiCoreConfig.credentials?.url,
        apiUrl: aiCoreConfig.credentials?.serviceurls?.AI_API_URL || 'N/A'
      } : null,
      timestamp: new Date().toISOString()
    };
    
    loggerService.info('AI-CORE-HEALTH', 'Estado de configuración consultado', status);
    
    res.status(200).json({
      success: true,
      status,
      message: aiCoreConfig 
        ? 'Servicio AI Core configurado correctamente' 
        : 'Servicio AI Core NO configurado o NO bindeado'
    });
    
  } catch (error) {
    console.error('[AI-CORE-HEALTH] Error obteniendo status:', error);
    
    res.status(500).json({
      success: false,
      error: error.message,
      message: 'Error al obtener el estado de SAP AI Core'
    });
  }
});

export default router;
