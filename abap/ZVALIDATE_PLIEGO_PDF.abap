*&---------------------------------------------------------------------*
*& Report ZVALIDATE_PLIEGO_PDF
*&---------------------------------------------------------------------*
*& Programa para enviar PDF a validación de pliegos
*& Endpoint: /api/pdf-correction/generate-list
*& Método: POST con multipart/form-data
*&---------------------------------------------------------------------*
REPORT zvalidate_pliego_pdf.

*----------------------------------------------------------------------*
* Declaraciones de datos
*----------------------------------------------------------------------*
DATA: lv_url           TYPE string,
      lv_pdf_base64    TYPE string,
      lv_pdf_xstring   TYPE xstring,
      lv_pdf_length    TYPE i,
      lv_response      TYPE string,
      lv_http_code     TYPE i,
      lv_error_message TYPE string.

DATA: lo_http_client TYPE REF TO if_http_client,
      lo_rest_client TYPE REF TO cl_rest_http_client.

* Parámetros de entrada
PARAMETERS: p_url    TYPE string DEFAULT 'https://ai-core-api.cfapps.eu10-005.hana.ondemand.com/api/pdf-correction/generate-list',
            p_file   TYPE rlgrap-filename OBLIGATORY,
            p_ctx    TYPE string DEFAULT 'DOCUMENTOS_VALIDACION',
            p_pliego TYPE string,
            p_user   TYPE string DEFAULT sy-uname.

*----------------------------------------------------------------------*
* Evento de ayuda para selección de archivo
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_select_file CHANGING p_file.

*----------------------------------------------------------------------*
* Inicio del programa
*----------------------------------------------------------------------*
START-OF-SELECTION.

  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: / '   VALIDACIÓN DE PLIEGOS SAP - AI CORE API'.
  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: /.

  " 1. Leer archivo PDF
  PERFORM f_read_pdf_file USING p_file
                          CHANGING lv_pdf_xstring
                                   lv_pdf_length.

  IF lv_pdf_xstring IS INITIAL.
    WRITE: / '❌ Error: No se pudo leer el archivo PDF'.
    RETURN.
  ENDIF.

  WRITE: / '✅ Archivo PDF leído correctamente'.
  WRITE: / '   Tamaño:', lv_pdf_length, 'bytes'.
  WRITE: /.

  " 2. Convertir PDF a Base64
  PERFORM f_convert_to_base64 USING lv_pdf_xstring
                              CHANGING lv_pdf_base64.

  WRITE: / '✅ PDF convertido a Base64'.
  WRITE: / '   Longitud Base64:', strlen( lv_pdf_base64 ), 'caracteres'.
  WRITE: /.

  " 3. Enviar request HTTP
  PERFORM f_send_http_request USING p_url
                                    lv_pdf_base64
                                    p_file
                                    p_ctx
                                    p_pliego
                                    p_user
                              CHANGING lv_response
                                       lv_http_code
                                       lv_error_message.

  " 4. Mostrar resultado
  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: / '   RESULTADO DE LA VALIDACIÓN'.
  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: /.

  IF lv_http_code = 200.
    WRITE: / '✅ Validación completada exitosamente'.
    WRITE: / '   HTTP Status:', lv_http_code.
    WRITE: / '   PDF de validación recibido'.
    WRITE: /.
    
    " Guardar PDF de respuesta
    PERFORM f_save_response_pdf USING lv_response p_file.
    
  ELSE.
    WRITE: / '❌ Error en la validación'.
    WRITE: / '   HTTP Status:', lv_http_code.
    WRITE: / '   Error:', lv_error_message.
  ENDIF.

  WRITE: / '═══════════════════════════════════════════════════════════════'.

*----------------------------------------------------------------------*
* FORM f_select_file
*----------------------------------------------------------------------*
FORM f_select_file CHANGING p_filename TYPE rlgrap-filename.
  DATA: lt_files TYPE filetable,
        lv_rc    TYPE i,
        lv_action TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Seleccionar archivo PDF'
      file_filter             = 'PDF Files (*.pdf)|*.pdf|All Files (*.*)|*.*'
      multiselection          = abap_false
    CHANGING
      file_table              = lt_files
      rc                      = lv_rc
      user_action             = lv_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc = 0 AND lv_action = cl_gui_frontend_services=>action_ok.
    READ TABLE lt_files INTO DATA(ls_file) INDEX 1.
    IF sy-subrc = 0.
      p_filename = ls_file-filename.
    ENDIF.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM f_read_pdf_file
*----------------------------------------------------------------------*
FORM f_read_pdf_file USING iv_filename TYPE rlgrap-filename
                     CHANGING ev_pdf_xstring TYPE xstring
                              ev_length TYPE i.

  DATA: lt_binary_tab TYPE TABLE OF x255,
        lv_filesize   TYPE i,
        lv_filename_str TYPE string.

  " Convertir filename a string
  lv_filename_str = iv_filename.

  " Leer archivo desde el frontend
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_filename_str
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
    WRITE: / '❌ Error leyendo archivo:', sy-subrc.
    RETURN.
  ENDIF.

  " Convertir tabla binaria a xstring
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lv_filesize
    IMPORTING
      buffer       = ev_pdf_xstring
    TABLES
      binary_tab   = lt_binary_tab
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    WRITE: / '❌ Error convirtiendo a xstring:', sy-subrc.
    RETURN.
  ENDIF.

  ev_length = lv_filesize.

ENDFORM.

*----------------------------------------------------------------------*
* FORM f_convert_to_base64
*----------------------------------------------------------------------*
FORM f_convert_to_base64 USING iv_xstring TYPE xstring
                         CHANGING ev_base64 TYPE string.

  DATA: lv_base64_raw TYPE string.

  " Convertir xstring a base64
  CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
    EXPORTING
      input  = iv_xstring
    IMPORTING
      output = lv_base64_raw.

  " Limpiar saltos de línea que añade la función
  ev_base64 = lv_base64_raw.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN ev_base64 WITH ''.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN ev_base64 WITH ''.

ENDFORM.

*----------------------------------------------------------------------*
* FORM f_send_http_request
*----------------------------------------------------------------------*
FORM f_send_http_request USING iv_url TYPE string
                               iv_pdf_base64 TYPE string
                               iv_filename TYPE rlgrap-filename
                               iv_context_id TYPE string
                               iv_pliego_id TYPE string
                               iv_username TYPE string
                         CHANGING ev_response TYPE string
                                  ev_http_code TYPE i
                                  ev_error TYPE string.

  DATA: lv_boundary    TYPE string,
        lv_body        TYPE string,
        lv_content_type TYPE string,
        lv_filename_only TYPE string,
        lv_subrc_str   TYPE string,
        lv_error_text  TYPE string,
        lv_bearer_token TYPE string,
        lv_authorization_header TYPE string,
        lv_http_code_str TYPE string.

  " Extraer solo el nombre del archivo sin ruta
  CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = iv_filename
    IMPORTING
      stripped_name = lv_filename_only
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
    lv_filename_only = iv_filename.
  ENDIF.

  " Generar boundary único para multipart/form-data
  CONCATENATE '----WebKitFormBoundary' sy-datum sy-uzeit INTO lv_boundary.

  " Construir body multipart/form-data
  DATA: lv_crlf TYPE string.
  lv_crlf = cl_abap_char_utilities=>cr_lf.
  
  CONCATENATE
    '--' lv_boundary lv_crlf
    'Content-Disposition: form-data; name="pdfBase64"' lv_crlf
    lv_crlf
    iv_pdf_base64 lv_crlf
    
    '--' lv_boundary lv_crlf
    'Content-Disposition: form-data; name="fileName"' lv_crlf
    lv_crlf
    lv_filename_only lv_crlf
    
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

  " Obtener Bearer Token para autenticación
  WRITE: / '🔐 Obteniendo token de autenticación...'.
  
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
    ev_http_code = 401.
    ev_error = 'Error obteniendo token de autenticación. Unauthorized.'.
    WRITE: / '❌', ev_error.
    RETURN.
  ENDIF.

  WRITE: / '✅ Token obtenido correctamente'.
  WRITE: /.

  " Crear cliente HTTP
  TRY.
      " Intentar crear cliente HTTP con SSL
      CALL METHOD cl_http_client=>create_by_url
        EXPORTING
          url                = iv_url
          ssl_id             = 'ANON'
        IMPORTING
          client             = lo_http_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4.

      IF sy-subrc <> 0.
        " Si falla con SSL, intentar sin SSL (para desarrollo)
        WRITE: / '⚠️  Error con SSL, reintentando...'.
        
        CALL METHOD cl_http_client=>create_by_url
          EXPORTING
            url                = iv_url
          IMPORTING
            client             = lo_http_client
          EXCEPTIONS
            argument_not_found = 1
            plugin_not_active  = 2
            internal_error     = 3
            OTHERS             = 4.
            
        IF sy-subrc <> 0.
          lv_subrc_str = sy-subrc.
          CONCATENATE 'Error creando cliente HTTP. SY-SUBRC:' lv_subrc_str INTO ev_error SEPARATED BY space.
          WRITE: / '❌', ev_error.
          RETURN.
        ENDIF.
      ENDIF.

      " Configurar propiedades del cliente HTTP
      lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.
      
      " Configurar Bearer Token en el encabezado Authorization
      CONCATENATE 'Bearer' lv_bearer_token INTO lv_authorization_header SEPARATED BY space.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Authorization'
          value = lv_authorization_header.
      
      " Configurar request
      lo_http_client->request->set_method( 'POST' ).
      lo_http_client->request->set_content_type( lv_content_type ).
      
      " Importante: usar set_cdata para texto
      lo_http_client->request->set_cdata( lv_body ).
      
      WRITE: / '📤 Enviando request a:', iv_url.
      WRITE: / '   Content-Type:', lv_content_type.
      WRITE: / '   Tamaño del body:', strlen( lv_body ), 'caracteres'.
      WRITE: /.

      " Enviar request
      CALL METHOD lo_http_client->send
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5.

      IF sy-subrc <> 0.
        lv_subrc_str = sy-subrc.
        CONCATENATE 'Error enviando request HTTP. SY-SUBRC:' lv_subrc_str INTO ev_error SEPARATED BY space.
        WRITE: / '❌', ev_error.
        
        " Intentar obtener más detalles del error
        CALL METHOD lo_http_client->get_last_error
          IMPORTING
            message = lv_error_text.
        IF lv_error_text IS NOT INITIAL.
          WRITE: / '   Detalle:', lv_error_text.
        ENDIF.
        
        lo_http_client->close( ).
        RETURN.
      ENDIF.

      WRITE: / '✅ Request enviado, esperando respuesta...'.

      " Recibir respuesta
      CALL METHOD lo_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4.

      IF sy-subrc <> 0.
        lv_subrc_str = sy-subrc.
        CONCATENATE 'Error recibiendo respuesta HTTP. SY-SUBRC:' lv_subrc_str INTO ev_error SEPARATED BY space.
        WRITE: / '❌', ev_error.
        
        " Intentar obtener más detalles del error
        CALL METHOD lo_http_client->get_last_error
          IMPORTING
            message = lv_error_text.
        IF lv_error_text IS NOT INITIAL.
          WRITE: / '   Detalle:', lv_error_text.
        ENDIF.
        
        lo_http_client->close( ).
        RETURN.
      ENDIF.

      WRITE: / '✅ Respuesta recibida'.

      " Obtener código de respuesta
      DATA: lv_reason TYPE string.
      
      CALL METHOD lo_http_client->response->get_status
        IMPORTING
          code   = ev_http_code
          reason = lv_reason.

      WRITE: / '📊 HTTP Status:', ev_http_code, '-', lv_reason.
      WRITE: /.

      " Verificar si hay error HTTP
      IF ev_http_code <> 200.
        " Obtener mensaje de error del servidor
        ev_response = lo_http_client->response->get_cdata( ).
        
        CASE ev_http_code.
          WHEN 401.
            ev_error = 'Error 401: No autorizado. Token inválido o expirado.'.
          WHEN 403.
            ev_error = 'Error 403: Acceso prohibido.'.
          WHEN 404.
            ev_error = 'Error 404: Endpoint no encontrado.'.
          WHEN 500.
            ev_error = 'Error 500: Error interno del servidor.'.
          WHEN 502.
            ev_error = 'Error 502: Bad Gateway.'.
          WHEN 503.
            ev_error = 'Error 503: Servicio no disponible.'.
          WHEN OTHERS.
            lv_http_code_str = ev_http_code.
            CONCATENATE 'Error HTTP' lv_http_code_str INTO ev_error SEPARATED BY space.
        ENDCASE.
        
        WRITE: / '❌', ev_error.
        IF ev_response IS NOT INITIAL.
          WRITE: / '   Respuesta del servidor:', ev_response(200).
        ENDIF.
        
        lo_http_client->close( ).
        RETURN.
      ENDIF.

      " Obtener contenido de respuesta (PDF)
      ev_response = lo_http_client->response->get_cdata( ).

      " Cerrar conexión
      lo_http_client->close( ).

    CATCH cx_root INTO DATA(lx_error).
      ev_error = lx_error->get_text( ).
  ENDTRY.

ENDFORM.

*----------------------------------------------------------------------*
* FORM f_save_response_pdf
*----------------------------------------------------------------------*
FORM f_save_response_pdf USING iv_response TYPE string
                               iv_original_file TYPE rlgrap-filename.

  DATA: lv_save_path TYPE string,
        lv_save_path_str TYPE string,
        lv_xstring   TYPE xstring,
        lt_binary    TYPE TABLE OF x255,
        lv_length    TYPE i.

  " Generar nombre de archivo de salida
  lv_save_path = iv_original_file.
  REPLACE '.pdf' IN lv_save_path WITH '_VALIDACION.pdf'.
  
  " Convertir a string para gui_download
  lv_save_path_str = lv_save_path.

  " Convertir respuesta base64 a xstring (si viene en base64)
  " Si viene como binario directo, usar directamente
  TRY.
      " Intentar decodificar de base64
      CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
        EXPORTING
          input  = iv_response
        IMPORTING
          output = lv_xstring
        EXCEPTIONS
          failed = 1
          OTHERS = 2.

      IF sy-subrc <> 0.
        " Si falla, asumir que ya es binario
        lv_xstring = iv_response.
      ENDIF.

      " Convertir xstring a tabla binaria
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = lv_xstring
        IMPORTING
          output_length = lv_length
        TABLES
          binary_tab = lt_binary.

      " Guardar archivo
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename     = lv_save_path_str
          filetype     = 'BIN'
          bin_filesize = lv_length
        CHANGING
          data_tab     = lt_binary
        EXCEPTIONS
          OTHERS       = 1.

      IF sy-subrc = 0.
        WRITE: / '💾 PDF de validación guardado en:'.
        WRITE: / '   ', lv_save_path_str.
      ELSE.
        WRITE: / '⚠️  No se pudo guardar el PDF de validación'.
      ENDIF.

    CATCH cx_root.
      WRITE: / '⚠️  Error procesando PDF de respuesta'.
  ENDTRY.

ENDFORM.
