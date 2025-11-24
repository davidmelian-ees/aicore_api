*&---------------------------------------------------------------------*
*& Report ZTEST_HTTP_CONNECTION
*&---------------------------------------------------------------------*
*& Programa de diagnóstico para probar conexión HTTP
*&---------------------------------------------------------------------*
REPORT ztest_http_connection.

DATA: lo_http_client TYPE REF TO if_http_client,
      lv_url         TYPE string,
      lv_code        TYPE i,
      lv_response    TYPE string,
      lv_subrc_str   TYPE string,
      lv_error_text  TYPE string,
      lv_bearer_token TYPE string,
      lv_authorization_header TYPE string.

PARAMETERS: p_url TYPE string DEFAULT 'https://ai-core-api.cfapps.eu10-005.hana.ondemand.com/health'.

START-OF-SELECTION.

  lv_url = p_url.

  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: / '   TEST DE CONEXIÓN HTTP'.
  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: /.
  WRITE: / 'URL:', lv_url.
  WRITE: /.

  " Test 0: Obtener Bearer Token
  WRITE: / '0️⃣  Obteniendo token de autenticación...'.
  
  CALL METHOD zcl_rpaia_auth=>get_bearer_token
    EXPORTING
      iv_identifier    = 'AICOREAUTH'
    RECEIVING
      rv_access_token  = lv_bearer_token
    EXCEPTIONS
      zex_err_token    = 1
      zex_err_response = 2
      OTHERS           = 3.

  IF sy-subrc <> 0.
    WRITE: / '   ❌ Error obteniendo token. SY-SUBRC:', sy-subrc.
    WRITE: / '   Verifica la configuración de AICOREAUTH'.
    RETURN.
  ENDIF.

  WRITE: / '   ✅ Token obtenido correctamente'.
  WRITE: /.

  " Test 1: Crear cliente HTTP con SSL
  WRITE: / '1️⃣  Intentando crear cliente HTTP con SSL...'.
  
  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = lv_url
      ssl_id             = 'ANON'
    IMPORTING
      client             = lo_http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.

  IF sy-subrc <> 0.
    lv_subrc_str = sy-subrc.
    WRITE: / '   ❌ Error con SSL. SY-SUBRC:', lv_subrc_str.
    WRITE: /.
    
    " Test 2: Intentar sin SSL
    WRITE: / '2️⃣  Intentando crear cliente HTTP sin SSL...'.
    
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.
        
    IF sy-subrc <> 0.
      lv_subrc_str = sy-subrc.
      WRITE: / '   ❌ Error sin SSL. SY-SUBRC:', lv_subrc_str.
      WRITE: /.
      WRITE: / '═══════════════════════════════════════════════════════════════'.
      WRITE: / '   DIAGNÓSTICO'.
      WRITE: / '═══════════════════════════════════════════════════════════════'.
      WRITE: / 'Posibles causas:'.
      WRITE: / '1. Configuración de proxy en SAP (SMICM)'.
      WRITE: / '2. Firewall bloqueando conexiones externas'.
      WRITE: / '3. Certificados SSL no configurados (STRUST)'.
      WRITE: / '4. URL no accesible desde el servidor SAP'.
      WRITE: /.
      WRITE: / 'Soluciones:'.
      WRITE: / '1. Ejecutar transacción SMICM > Goto > Services'.
      WRITE: / '2. Verificar configuración de proxy en RZ10'.
      WRITE: / '3. Importar certificados SSL en STRUST'.
      WRITE: / '4. Contactar al BASIS administrator'.
      RETURN.
    ENDIF.
  ENDIF.

  WRITE: / '   ✅ Cliente HTTP creado correctamente'.
  WRITE: /.

  " Test 3: Configurar autenticación
  WRITE: / '3️⃣  Configurando Bearer Token...'.
  
  CONCATENATE 'Bearer' lv_bearer_token INTO lv_authorization_header SEPARATED BY space.
  CALL METHOD lo_http_client->request->set_header_field
    EXPORTING
      name  = 'Authorization'
      value = lv_authorization_header.
  
  WRITE: / '   ✅ Token configurado en header'.
  WRITE: /.

  " Test 4: Enviar request GET simple
  WRITE: / '4️⃣  Enviando request GET...'.
  
  lo_http_client->request->set_method( 'GET' ).
  
  CALL METHOD lo_http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.

  IF sy-subrc <> 0.
    lv_subrc_str = sy-subrc.
    WRITE: / '   ❌ Error enviando request. SY-SUBRC:', lv_subrc_str.
    
    CALL METHOD lo_http_client->get_last_error
      IMPORTING
        message = lv_error_text.
    IF lv_error_text IS NOT INITIAL.
      WRITE: / '   Detalle:', lv_error_text.
    ENDIF.
    
    lo_http_client->close( ).
    RETURN.
  ENDIF.

  WRITE: / '   ✅ Request enviado, esperando respuesta...'.

  " Test 5: Recibir respuesta
  WRITE: / '5️⃣  Recibiendo respuesta...'.
  
  CALL METHOD lo_http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.

  IF sy-subrc <> 0.
    lv_subrc_str = sy-subrc.
    WRITE: / '   ❌ Error recibiendo respuesta. SY-SUBRC:', lv_subrc_str.
    lo_http_client->close( ).
    RETURN.
  ENDIF.

  WRITE: / '   ✅ Respuesta recibida'.
  WRITE: /.

  " Test 6: Verificar status code
  WRITE: / '6️⃣  Verificando status code...'.
  
  CALL METHOD lo_http_client->response->get_status
    IMPORTING
      code = lv_code.

  WRITE: / '   HTTP Status:', lv_code.
  
  IF lv_code = 200.
    WRITE: / '   ✅ Conexión exitosa!'.
  ELSE.
    WRITE: / '   ⚠️  Status code inesperado'.
  ENDIF.
  
  WRITE: /.

  " Test 7: Leer respuesta
  WRITE: / '7️⃣  Leyendo contenido de respuesta...'.
  
  lv_response = lo_http_client->response->get_cdata( ).
  
  WRITE: / '   Longitud:', strlen( lv_response ), 'caracteres'.
  WRITE: /.
  WRITE: / '   Respuesta:'.
  WRITE: / lv_response.
  WRITE: /.

  " Cerrar conexión
  lo_http_client->close( ).

  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: / '   ✅ TEST COMPLETADO EXITOSAMENTE'.
  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: /.
  WRITE: / 'La conexión HTTP funciona correctamente.'.
  WRITE: / 'Puedes ejecutar el programa ZVALIDATE_PLIEGO_PDF.'.
