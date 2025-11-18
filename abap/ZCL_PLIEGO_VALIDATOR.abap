*&---------------------------------------------------------------------*
*& Class ZCL_PLIEGO_VALIDATOR
*&---------------------------------------------------------------------*
*& Clase para validar pliegos SAP mediante API AI Core
*&---------------------------------------------------------------------*
CLASS zcl_pliego_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    
    " Método principal para validar pliego desde archivo
    CLASS-METHODS validate_pliego_from_file
      IMPORTING
        iv_pdf_path      TYPE string
        iv_pliego_id     TYPE string OPTIONAL
        iv_context_id    TYPE string OPTIONAL
        iv_username      TYPE string OPTIONAL
      EXPORTING
        ev_pdf_xstring   TYPE xstring
        ev_pdf_base64    TYPE string
        ev_http_status   TYPE i
        ev_error_message TYPE string.

    " Método para validar pliego desde base64
    CLASS-METHODS validate_pliego_from_base64
      IMPORTING
        iv_pdf_base64    TYPE string
        iv_filename      TYPE string OPTIONAL
        iv_pliego_id     TYPE string OPTIONAL
        iv_context_id    TYPE string OPTIONAL
        iv_username      TYPE string OPTIONAL
      EXPORTING
        ev_pdf_xstring   TYPE xstring
        ev_pdf_base64    TYPE string
        ev_http_status   TYPE i
        ev_error_message TYPE string.

    " Método para verificar conexión con el servidor (health check)
    CLASS-METHODS check_connection
      EXPORTING
        ev_http_status   TYPE i
        ev_response      TYPE string
        ev_error_message TYPE string
        ev_is_online     TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    " Método interno para hacer la llamada HTTP
    CLASS-METHODS call_validation_api
      IMPORTING
        iv_pdf_base64    TYPE string
        iv_filename      TYPE string
        iv_pliego_id     TYPE string
        iv_context_id    TYPE string
        iv_username      TYPE string
      EXPORTING
        ev_pdf_xstring   TYPE xstring
        ev_pdf_base64    TYPE string
        ev_http_status   TYPE i
        ev_error_message TYPE string.

ENDCLASS.



CLASS zcl_pliego_validator IMPLEMENTATION.

*----------------------------------------------------------------------*
* METHOD validate_pliego_from_file
*----------------------------------------------------------------------*
  METHOD validate_pliego_from_file.
    
    DATA: lv_pdf_xstring TYPE xstring,
          lv_pdf_base64  TYPE string,
          lv_filename    TYPE string,
          lt_binary_tab  TYPE TABLE OF x255,
          lv_filesize    TYPE i.

    " Extraer nombre del archivo
    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = iv_pdf_path
      IMPORTING
        stripped_name = lv_filename
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      lv_filename = 'documento.pdf'.
    ENDIF.

    " Leer archivo desde el frontend
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = iv_pdf_path
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_filesize
      CHANGING
        data_tab                = lt_binary_tab
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.

    IF sy-subrc <> 0.
      ev_http_status = 400.
      ev_error_message = 'Error leyendo archivo PDF'.
      RETURN.
    ENDIF.

    " Convertir tabla binaria a xstring
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filesize
      IMPORTING
        buffer       = lv_pdf_xstring
      TABLES
        binary_tab   = lt_binary_tab
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      ev_http_status = 400.
      ev_error_message = 'Error convirtiendo PDF a xstring'.
      RETURN.
    ENDIF.

    " Convertir xstring a base64
    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = lv_pdf_xstring
      IMPORTING
        output = lv_pdf_base64.

    " Limpiar saltos de línea
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_pdf_base64 WITH ''.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_pdf_base64 WITH ''.

    " Llamar a la API
    call_validation_api(
      EXPORTING
        iv_pdf_base64    = lv_pdf_base64
        iv_filename      = lv_filename
        iv_pliego_id     = iv_pliego_id
        iv_context_id    = iv_context_id
        iv_username      = iv_username
      IMPORTING
        ev_pdf_xstring   = ev_pdf_xstring
        ev_pdf_base64    = ev_pdf_base64
        ev_http_status   = ev_http_status
        ev_error_message = ev_error_message
    ).

  ENDMETHOD.

*----------------------------------------------------------------------*
* METHOD validate_pliego_from_base64
*----------------------------------------------------------------------*
  METHOD validate_pliego_from_base64.

    DATA: lv_filename TYPE string.

    " Usar filename proporcionado o default
    IF iv_filename IS INITIAL.
      lv_filename = 'documento.pdf'.
    ELSE.
      lv_filename = iv_filename.
    ENDIF.

    " Llamar a la API directamente con el base64
    call_validation_api(
      EXPORTING
        iv_pdf_base64    = iv_pdf_base64
        iv_filename      = lv_filename
        iv_pliego_id     = iv_pliego_id
        iv_context_id    = iv_context_id
        iv_username      = iv_username
      IMPORTING
        ev_pdf_xstring   = ev_pdf_xstring
        ev_pdf_base64    = ev_pdf_base64
        ev_http_status   = ev_http_status
        ev_error_message = ev_error_message
    ).

  ENDMETHOD.

*----------------------------------------------------------------------*
* METHOD call_validation_api
*----------------------------------------------------------------------*
  METHOD call_validation_api.

    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_url         TYPE string,
          lv_endpoint    TYPE string,
          lv_bearer_token TYPE string,
          lv_authorization_header TYPE string,
          lv_boundary    TYPE string,
          lv_body        TYPE string,
          lv_content_type TYPE string,
          lv_crlf        TYPE string,
          lv_response_text TYPE string,
          lv_response_xstring TYPE xstring,
          lv_reason      TYPE string.

    " 1. Obtener URL y endpoint desde tabla de configuración
    SELECT SINGLE url, endpoint FROM zpa_rpaia_utils
      INTO @DATA(ls_rpaia_utils)
      WHERE name_service = 'plecs_rev'.

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

*----------------------------------------------------------------------*
* METHOD check_connection
*----------------------------------------------------------------------*
  METHOD check_connection.

    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_url         TYPE string,
          lv_endpoint    TYPE string,
          lv_bearer_token TYPE string,
          lv_authorization_header TYPE string,
          lv_reason      TYPE string.

    " Inicializar valores
    ev_is_online = abap_false.
    ev_http_status = 0.

    " 1. Obtener URL desde tabla de configuración
    SELECT SINGLE url FROM zpa_rpaia_utils
      INTO @DATA(lv_base_url)
      WHERE name_service = 'plecs_rev'.

    IF sy-subrc <> 0.
      ev_http_status = 404.
      ev_error_message = 'Configuración de servicio plecs_rev no encontrada en tabla ZPA_RPAIA_UTILS'.
      RETURN.
    ENDIF.

    " Construir URL del health endpoint
    CONCATENATE lv_base_url '/health' INTO lv_url.

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

    " 3. Crear cliente HTTP
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

        " 4. Configurar cliente HTTP
        lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.

        " Configurar Bearer Token
        CONCATENATE 'Bearer' lv_bearer_token INTO lv_authorization_header SEPARATED BY space.
        CALL METHOD lo_http_client->request->set_header_field
          EXPORTING
            name  = 'Authorization'
            value = lv_authorization_header.

        " Configurar request GET
        lo_http_client->request->set_method( 'GET' ).

        " 5. Enviar request
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

        " 6. Recibir respuesta
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

        " 7. Obtener código de respuesta
        CALL METHOD lo_http_client->response->get_status
          IMPORTING
            code   = ev_http_status
            reason = lv_reason.

        " 8. Obtener respuesta
        ev_response = lo_http_client->response->get_cdata( ).

        " 9. Verificar si está online
        IF ev_http_status = 200.
          ev_is_online = abap_true.
          ev_error_message = 'Conexión exitosa'.
        ELSE.
          ev_is_online = abap_false.
          CASE ev_http_status.
            WHEN 401.
              ev_error_message = 'Error 401: No autorizado'.
            WHEN 403.
              ev_error_message = 'Error 403: Acceso prohibido'.
            WHEN 404.
              ev_error_message = 'Error 404: Endpoint no encontrado'.
            WHEN 500.
              ev_error_message = 'Error 500: Error interno del servidor'.
            WHEN 502.
              ev_error_message = 'Error 502: Bad Gateway'.
            WHEN 503.
              ev_error_message = 'Error 503: Servicio no disponible'.
            WHEN OTHERS.
              ev_error_message = 'Error HTTP desconocido'.
          ENDCASE.
        ENDIF.

        " Cerrar conexión
        lo_http_client->close( ).

      CATCH cx_root INTO DATA(lx_error).
        ev_http_status = 500.
        ev_error_message = lx_error->get_text( ).
        ev_is_online = abap_false.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
