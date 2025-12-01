*&---------------------------------------------------------------------*
*& Report ZTEST_HEALTH_CHECK
*&---------------------------------------------------------------------*
*& Programa para verificar la conexión con la API de AI Core
*&---------------------------------------------------------------------*
REPORT ztest_health_check.

DATA: lv_http_status   TYPE i,
      lv_response      TYPE string,
      lv_error_message TYPE string,
      lv_is_online     TYPE abap_bool.

START-OF-SELECTION.

  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: / '   HEALTH CHECK - AI CORE API'.
  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: /.

  WRITE: / '🔄 Verificando conexión con el servidor...'.
  WRITE: /.

  " Llamar al método de health check
  CALL METHOD zcl_pliego_validator=>check_connection
    IMPORTING
      ev_http_status   = lv_http_status
      ev_response      = lv_response
      ev_error_message = lv_error_message
      ev_is_online     = lv_is_online.

  " Mostrar resultado
  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: / '   RESULTADO'.
  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: /.

  WRITE: / 'HTTP Status:', lv_http_status.
  WRITE: / 'Estado:', lv_error_message.
  WRITE: /.

  IF lv_is_online = abap_true.
    WRITE: / '✅ SERVIDOR ONLINE Y DISPONIBLE'.
    WRITE: /.
    WRITE: / 'Respuesta del servidor:'.
    WRITE: / lv_response.
    WRITE: /.
    WRITE: / '🎉 La API está funcionando correctamente.'.
    WRITE: / '   Puedes ejecutar validaciones de pliegos.'.
  ELSE.
    WRITE: / '❌ SERVIDOR NO DISPONIBLE'.
    WRITE: /.
    WRITE: / 'Posibles causas:'.
    WRITE: / '1. El servidor está apagado o en mantenimiento'.
    WRITE: / '2. Problemas de red o firewall'.
    WRITE: / '3. Configuración incorrecta en tabla ZPA_RPAIA_UTILS'.
    WRITE: / '4. Token de autenticación inválido o expirado'.
    WRITE: /.
    WRITE: / 'Soluciones:'.
    WRITE: / '1. Verificar que el servidor esté activo'.
    WRITE: / '2. Ejecutar ZTEST_HTTP_CONNECTION para diagnóstico de red'.
    WRITE: / '3. Verificar tabla ZPA_RPAIA_UTILS (name_service = plecs_rev)'.
    WRITE: / '4. Regenerar token de autenticación (AICOREAUTH)'.
  ENDIF.

  WRITE: /.
  WRITE: / '═══════════════════════════════════════════════════════════════'.
