*----------------------------------------------------------------------*
* METHOD call_validation_api
*----------------------------------------------------------------------*
  METHOD call_validation_api.

    DATA: lo_http_client          TYPE REF TO if_http_client ##NEEDED,
          lv_url                  TYPE string,
          lv_endpoint             TYPE string,
          lv_bearer_token         TYPE string,
          lv_authorization_header TYPE string,
          lv_boundary             TYPE string,
          lv_body                 TYPE string,
          lv_content_type         TYPE string,
          lv_crlf                 TYPE string,
          lv_response_text        TYPE string,
          lv_response_xstring     TYPE xstring,
          lv_reason               TYPE string.

    " 1. Obtener URL y endpoint desde tabla de configuración
    SELECT SINGLE url, endpoint FROM zpa_rpaia_utils
      INTO @DATA(ls_rpaia_utils)
      WHERE name_service = 'PLECS_REV'.

    IF sy-subrc <> 0.
      ev_http_status = 404.
      ev_error_message = 'Configuración de servicio plecs_rev no encontrada'.
      RETURN.
    ENDIF.

    CONCATENATE ls_rpaia_utils-url ls_rpaia_utils-endpoint INTO lv_url.

    " 2. Obtener Bearer Token
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
      ev_http_status = 401.
      ev_error_message = 'Error obteniendo token de autenticación'.
      RETURN.
    ENDIF.

    " 3. Construir body multipart/form-data
    CONCATENATE '----WebKitFormBoundary' sy-datum sy-uzeit INTO lv_boundary.
    lv_crlf = cl_abap_char_utilities=>cr_lf.

    CONCATENATE
      '--' lv_boundary lv_crlf
      'Content-Disposition: form-data; name="pdfBase64"' lv_crlf
      lv_crlf
      iv_pdf_base64 lv_crlf

      '--' lv_boundary lv_crlf
      'Content-Disposition: form-data; name="fileName"' lv_crlf
      lv_crlf
      iv_filename lv_crlf

      '--' lv_boundary lv_crlf
      'Content-Disposition: form-data; name="contextId"' lv_crlf
      lv_crlf
      iv_context_id lv_crlf

      '--' lv_boundary lv_crlf
      'Content-Disposition: form-data; name="pliegoId"' lv_crlf
      lv_crlf
      iv_pliego_id lv_crlf

      '--' lv_boundary lv_crlf
      'Content-Disposition: form-data; name="username"' lv_crlf
      lv_crlf
      iv_username lv_crlf

      '--' lv_boundary '--'
      INTO lv_body.

    CONCATENATE 'multipart/form-data; boundary=' lv_boundary INTO lv_content_type.

    " 4. Crear cliente HTTP
    TRY.
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
          " Intentar sin SSL
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
            ev_http_status = 500.
            ev_error_message = 'Error creando cliente HTTP'.
            RETURN.
          ENDIF.
        ENDIF.

        " 5. Configurar cliente HTTP
        lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.

        " Configurar Bearer Token
        CONCATENATE 'Bearer' lv_bearer_token INTO lv_authorization_header SEPARATED BY space.
        CALL METHOD lo_http_client->request->set_header_field
          EXPORTING
            name  = 'Authorization'
            value = lv_authorization_header.

        " Configurar request
        lo_http_client->request->set_method( 'POST' ).
        lo_http_client->request->set_content_type( lv_content_type ).
        lo_http_client->request->set_cdata( lv_body ).

        " 6. Enviar request
        CALL METHOD lo_http_client->send
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5.

        IF sy-subrc <> 0.
          ev_http_status = 500.
          ev_error_message = 'Error enviando request HTTP'.
          lo_http_client->close( ).
          RETURN.
        ENDIF.

        " 7. Recibir respuesta
        CALL METHOD lo_http_client->receive
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            OTHERS                     = 4.

        IF sy-subrc <> 0.
          ev_http_status = 500.
          ev_error_message = 'Error recibiendo respuesta HTTP'.
          lo_http_client->close( ).
          RETURN.
        ENDIF.

        " 8. Obtener código de respuesta
        CALL METHOD lo_http_client->response->get_status
          IMPORTING
            code   = ev_http_status
            reason = lv_reason.

        DATA(lv_header_value) = lo_http_client->response->get_header_field( 'fileSize' ).
        IF lv_header_value IS NOT INITIAL.
          ev_filesize = lv_header_value.
        ENDIF.

        " 9. Verificar si hay error HTTP
        IF ev_http_status <> 200.
          lv_response_text = lo_http_client->response->get_cdata( ).

          CASE ev_http_status.
            WHEN 401.
              ev_error_message = 'Error 401: No autorizado. Token inválido o expirado.'.
            WHEN 403.
              ev_error_message = 'Error 403: Acceso prohibido.'.
            WHEN 404.
              ev_error_message = 'Error 404: Endpoint no encontrado.'.
            WHEN 500.
              ev_error_message = 'Error 500: Error interno del servidor.'.
            WHEN 502.
              ev_error_message = 'Error 502: Bad Gateway.'.
            WHEN 503.
              ev_error_message = 'Error 503: Servicio no disponible.'.
            WHEN OTHERS.
              ev_error_message = 'Error HTTP desconocido'.
          ENDCASE.

          IF lv_response_text IS NOT INITIAL.
            CONCATENATE ev_error_message lv_response_text INTO ev_error_message SEPARATED BY ': '.
          ENDIF.

          lo_http_client->close( ).
          RETURN.
        ENDIF.

        " 10. Obtener PDF de respuesta en formato BINARIO (xstring)
        " IMPORTANTE: El servidor devuelve un PDF binario, no texto
        lv_response_xstring = lo_http_client->response->get_data( ).

        " Si no hay datos binarios, intentar obtener como texto y convertir
        IF lv_response_xstring IS INITIAL.
          lv_response_text = lo_http_client->response->get_cdata( ).

          " Intentar decodificar de base64 si viene como texto
          TRY.
              CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
                EXPORTING
                  input  = lv_response_text
                IMPORTING
                  output = lv_response_xstring
                EXCEPTIONS
                  failed = 1
                  OTHERS = 2.
            CATCH cx_root.
              " Si falla, asumir que es binario directo
          ENDTRY.
        ENDIF.

        " Verificar que tenemos datos
        IF lv_response_xstring IS INITIAL.
          ev_http_status = 500.
          ev_error_message = 'Respuesta vacía del servidor'.
          lo_http_client->close( ).
          RETURN.
        ENDIF.

        " Devolver PDF en xstring
        ev_pdf_xstring = lv_response_xstring.

        " Convertir a base64 si se solicita
        CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
          EXPORTING
            input  = lv_response_xstring
          IMPORTING
            output = ev_pdf_base64.

        " Limpiar saltos de línea del base64
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN ev_pdf_base64 WITH ''.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN ev_pdf_base64 WITH ''.

        " Cerrar conexión
        lo_http_client->close( ).

      CATCH cx_root INTO DATA(lx_error).
        ev_http_status = 500.
        ev_error_message = lx_error->get_text( ).
    ENDTRY.

  ENDMETHOD.