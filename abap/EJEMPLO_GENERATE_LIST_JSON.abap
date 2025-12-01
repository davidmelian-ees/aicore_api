*&---------------------------------------------------------------------*
*& Report ZGENERATE_LIST_HEADERS
*&---------------------------------------------------------------------*
*& Ejemplo de llamada al endpoint /api/pdf-correction/generate-list
*& Respuesta: PDF binario con fileSize en headers HTTP
*&---------------------------------------------------------------------*
REPORT zgenerate_list_headers.

PARAMETERS: p_url    TYPE string DEFAULT 'https://ai-core-api.cfapps.eu10-005.hana.ondemand.com/api/pdf-correction/generate-list',
            p_file   TYPE string OBLIGATORY,
            p_pliego TYPE string DEFAULT 'PLIEGO_001',
            p_ctx    TYPE string,
            p_user   TYPE string DEFAULT sy-uname.

DATA: lv_pdf_xstring     TYPE xstring,
      lv_pdf_base64      TYPE string,
      lv_response        TYPE string,
      lv_http_code       TYPE i,
      lv_error_message   TYPE string,
      lv_pdf_length      TYPE i,
      lv_result_pdf_b64  TYPE string,
      lv_result_filesize TYPE i,
      lv_result_filename TYPE string,
      lv_result_pliego   TYPE string,
      lv_critical_errors TYPE i,
      lv_warnings        TYPE i.

START-OF-SELECTION.

  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: / '   VALIDACIÓN DE PLIEGO PDF - RESPUESTA JSON'.
  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: /.

  " 1. Leer archivo PDF
  PERFORM f_read_pdf_file USING p_file
                          CHANGING lv_pdf_xstring
                                   lv_pdf_length
                                   lv_error_message.

  IF lv_error_message IS NOT INITIAL.
    WRITE: / '❌ Error leyendo archivo:', lv_error_message.
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

  " 3. Enviar request HTTP (respuesta PDF binario)
  PERFORM f_send_http_request USING p_url
                                    lv_pdf_base64
                                    p_file
                                    p_ctx
                                    p_pliego
                                    p_user
                              CHANGING lv_response
                                       lv_http_code
                                       lv_result_filesize
                                       lv_result_filename
                                       lv_result_pliego
                                       lv_critical_errors
                                       lv_warnings
                                       lv_error_message.

  " 4. La respuesta ya es el PDF binario (no JSON)
  IF lv_http_code = 200.

    " 5. Mostrar resultado
    WRITE: / '═══════════════════════════════════════════════════════════════'.
    WRITE: / '   RESULTADO DE LA VALIDACIÓN'.
    WRITE: / '═══════════════════════════════════════════════════════════════'.
    WRITE: /.
    WRITE: / '✅ Validación completada exitosamente'.
    WRITE: / '   Pliego ID:', lv_result_pliego.
    WRITE: / '   Archivo:', lv_result_filename.
    WRITE: / '   Tamaño PDF resultado:', lv_result_filesize, 'bytes'. " ← Campo fileSize
    WRITE: / '   Errores críticos:', lv_critical_errors.
    WRITE: / '   Advertencias:', lv_warnings.
    WRITE: /.

    " 6. Guardar PDF resultado (ya viene en binario, no en base64)
    IF lv_response IS NOT INITIAL.
      PERFORM f_save_result_pdf_binary USING lv_response
                                             lv_result_filename
                                             lv_result_filesize
                                       CHANGING lv_error_message.

      IF lv_error_message IS INITIAL.
        WRITE: / '✅ PDF con correcciones guardado correctamente'.
      ELSE.
        WRITE: / '⚠️ Advertencia guardando PDF:', lv_error_message.
      ENDIF.
    ENDIF.

  ELSE.
    WRITE: / '❌ Error en validación'.
    WRITE: / '   Código HTTP:', lv_http_code.
    WRITE: / '   Mensaje:', lv_error_message.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  f_send_http_request
*&---------------------------------------------------------------------*
FORM f_send_http_request USING p_url TYPE string
                               p_pdf_base64 TYPE string
                               p_filename TYPE string
                               p_context_id TYPE string
                               p_pliego_id TYPE string
                               p_username TYPE string
                         CHANGING p_response TYPE string
                                  p_http_code TYPE i
                                  p_filesize TYPE i
                                  p_result_filename TYPE string
                                  p_result_pliego TYPE string
                                  p_critical_errors TYPE i
                                  p_warnings TYPE i
                                  p_error TYPE string.

  DATA: lo_http_client TYPE REF TO if_http_client,
        lv_url         TYPE string,
        lv_body        TYPE string,
        lv_status      TYPE i,
        lv_reason      TYPE string,
        lv_header_value TYPE string.

  lv_url = p_url.

  TRY.
      " Crear cliente HTTP
      cl_http_client=>create_by_url(
        EXPORTING
          url                = lv_url
        IMPORTING
          client             = lo_http_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).

      IF sy-subrc <> 0.
        p_error = 'Error creando cliente HTTP'.
        RETURN.
      ENDIF.

      " Configurar método POST
      lo_http_client->request->set_method( 'POST' ).

      " Headers
      lo_http_client->request->set_header_field(
        name  = 'Content-Type'
        value = 'application/json' ).

      " Construir JSON body
      DATA: lv_json TYPE string.

      CONCATENATE '{'
                  '"pdfBase64":"' p_pdf_base64 '",'
                  '"fileName":"' p_filename '",'
                  '"pliegoId":"' p_pliego_id '",'
                  '"contextId":"' p_context_id '",'
                  '"username":"' p_username '"'
                  '}' INTO lv_json.

      " Establecer body
      lo_http_client->request->set_cdata( lv_json ).

      " Enviar request
      lo_http_client->send(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).

      IF sy-subrc <> 0.
        p_error = 'Error enviando request HTTP'.
        lo_http_client->close( ).
        RETURN.
      ENDIF.

      " Recibir response
      lo_http_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).

      IF sy-subrc <> 0.
        p_error = 'Error recibiendo response HTTP'.
        lo_http_client->close( ).
        RETURN.
      ENDIF.

      " Obtener código de estado
      lo_http_client->response->get_status(
        IMPORTING
          code   = lv_status
          reason = lv_reason ).

      p_http_code = lv_status.

      " Obtener respuesta (PDF binario)
      p_response = lo_http_client->response->get_cdata( ).

      " Leer headers de respuesta
      " Header: fileSize (campo requerido por SAP)
      lv_header_value = lo_http_client->response->get_header_field( 'fileSize' ).
      IF lv_header_value IS NOT INITIAL.
        p_filesize = lv_header_value.
      ENDIF.

      " Header: X-File-Name
      lv_header_value = lo_http_client->response->get_header_field( 'X-File-Name' ).
      IF lv_header_value IS NOT INITIAL.
        p_result_filename = lv_header_value.
      ENDIF.

      " Header: X-Pliego-Id
      lv_header_value = lo_http_client->response->get_header_field( 'X-Pliego-Id' ).
      IF lv_header_value IS NOT INITIAL.
        p_result_pliego = lv_header_value.
      ENDIF.

      " Header: X-Critical-Errors
      lv_header_value = lo_http_client->response->get_header_field( 'X-Critical-Errors' ).
      IF lv_header_value IS NOT INITIAL.
        p_critical_errors = lv_header_value.
      ENDIF.

      " Header: X-Warnings
      lv_header_value = lo_http_client->response->get_header_field( 'X-Warnings' ).
      IF lv_header_value IS NOT INITIAL.
        p_warnings = lv_header_value.
      ENDIF.

      " Cerrar conexión
      lo_http_client->close( ).

    CATCH cx_root INTO DATA(lx_error).
      p_error = lx_error->get_text( ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_save_result_pdf_binary
*&---------------------------------------------------------------------*
FORM f_save_result_pdf_binary USING p_pdf_binary TYPE string
                                    p_filename TYPE string
                                    p_filesize TYPE i
                              CHANGING p_error TYPE string.

  DATA: lv_pdf_xstring TYPE xstring,
        lv_path        TYPE string,
        lv_fullpath    TYPE string.

  " Convertir string binario a xstring
  " La respuesta HTTP ya viene como binario, no como base64
  lv_pdf_xstring = cl_http_utility=>if_http_utility~decode_x_base64( p_pdf_binary ).

  " Construir nombre de archivo de salida
  CONCATENATE 'correcciones-' p_filename INTO lv_fullpath.

  " Guardar archivo (ajustar según tu sistema)
  " Ejemplo usando GUI_DOWNLOAD
  DATA: lt_binary_tab TYPE TABLE OF x255.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_pdf_xstring
    IMPORTING
      output_length = DATA(lv_length)
    TABLES
      binary_tab    = lt_binary_tab.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename              = lv_fullpath
      filetype              = 'BIN'
      bin_filesize          = p_filesize  " ← Usar fileSize de la API
    TABLES
      data_tab              = lt_binary_tab
    EXCEPTIONS
      file_write_error      = 1
      no_batch_authority    = 2
      gui_refuse_filetransfer = 3
      invalid_type          = 4
      no_authority          = 5
      unknown_error         = 6
      header_not_allowed    = 7
      separator_not_allowed = 8
      filesize_not_allowed  = 9
      header_too_long       = 10
      dp_error_create       = 11
      dp_error_send         = 12
      dp_error_write        = 13
      unknown_dp_error      = 14
      access_denied         = 15
      dp_out_of_memory      = 16
      disk_full             = 17
      dp_timeout            = 18
      file_not_found        = 19
      dataprovider_exception = 20
      control_flush_error   = 21
      OTHERS                = 22.

  IF sy-subrc <> 0.
    p_error = 'Error guardando archivo PDF'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_read_pdf_file
*&---------------------------------------------------------------------*
FORM f_read_pdf_file USING p_filename TYPE string
                     CHANGING p_xstring TYPE xstring
                              p_length TYPE i
                              p_error TYPE string.
  " Implementación de lectura de archivo
  " (Código similar al ejemplo anterior ZVALIDATE_PLIEGO_PDF)
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_convert_to_base64
*&---------------------------------------------------------------------*
FORM f_convert_to_base64 USING p_xstring TYPE xstring
                         CHANGING p_base64 TYPE string.
  " Implementación de conversión a base64
  " (Código similar al ejemplo anterior)
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_convert_from_base64
*&---------------------------------------------------------------------*
FORM f_convert_from_base64 USING p_base64 TYPE string
                           CHANGING p_xstring TYPE xstring.

  CALL FUNCTION 'SSFC_BASE64_DECODE'
    EXPORTING
      b64data = p_base64
    IMPORTING
      bindata = p_xstring
    EXCEPTIONS
      OTHERS  = 1.

ENDFORM.
