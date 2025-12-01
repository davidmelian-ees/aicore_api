*&---------------------------------------------------------------------*
*& Report ZTEST_PLIEGO_VALIDATOR
*&---------------------------------------------------------------------*
*& Programa de prueba para la clase ZCL_PLIEGO_VALIDATOR
*&---------------------------------------------------------------------*
REPORT ztest_pliego_validator.

DATA: lv_pdf_path      TYPE string,
      lv_pdf_xstring   TYPE xstring,
      lv_pdf_base64    TYPE string,
      lv_http_status   TYPE i,
      lv_error_message TYPE string,
      lv_save_path     TYPE string,
      lt_binary        TYPE TABLE OF x255,
      lv_length        TYPE i.

PARAMETERS: p_file   TYPE rlgrap-filename OBLIGATORY,
            p_pliego TYPE string DEFAULT 'PCAP_TEST',
            p_ctx    TYPE string DEFAULT 'DOCUMENTOS_VALIDACION',
            p_user   TYPE string DEFAULT sy-uname.

*----------------------------------------------------------------------*
* Evento de ayuda para selección de archivo
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
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
      p_file = ls_file-filename.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* Inicio del programa
*----------------------------------------------------------------------*
START-OF-SELECTION.

  lv_pdf_path = p_file.

  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: / '   TEST DE VALIDACIÓN DE PLIEGOS - CLASE ZCL_PLIEGO_VALIDATOR'.
  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: /.
  WRITE: / 'Archivo:', lv_pdf_path.
  WRITE: / 'Pliego ID:', p_pliego.
  WRITE: / 'Context ID:', p_ctx.
  WRITE: / 'Usuario:', p_user.
  WRITE: /.

  " Llamar al método de validación
  WRITE: / '🔄 Validando pliego...'.
  WRITE: /.

  CALL METHOD zcl_pliego_validator=>validate_pliego_from_file
    EXPORTING
      iv_pdf_path      = lv_pdf_path
      iv_pliego_id     = p_pliego
      iv_context_id    = p_ctx
      iv_username      = p_user
    IMPORTING
      ev_pdf_xstring   = lv_pdf_xstring
      ev_pdf_base64    = lv_pdf_base64
      ev_http_status   = lv_http_status
      ev_error_message = lv_error_message.

  " Mostrar resultado
  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: / '   RESULTADO'.
  WRITE: / '═══════════════════════════════════════════════════════════════'.
  WRITE: /.

  WRITE: / 'HTTP Status:', lv_http_status.
  WRITE: /.

  IF lv_http_status = 200.
    WRITE: / '✅ Validación completada exitosamente'.
    WRITE: /.

    IF lv_pdf_xstring IS NOT INITIAL.
      " Guardar PDF de validación
      lv_save_path = lv_pdf_path.
      REPLACE '.pdf' IN lv_save_path WITH '_VALIDACION.pdf'.

      " Convertir xstring a tabla binaria
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_pdf_xstring
        IMPORTING
          output_length = lv_length
        TABLES
          binary_tab    = lt_binary.

      " Guardar archivo
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename     = lv_save_path
          filetype     = 'BIN'
          bin_filesize = lv_length
        CHANGING
          data_tab     = lt_binary
        EXCEPTIONS
          OTHERS       = 1.

      IF sy-subrc = 0.
        WRITE: / '💾 PDF de validación guardado en:'.
        WRITE: / '   ', lv_save_path.
        WRITE: /.
      ELSE.
        WRITE: / '⚠️  No se pudo guardar el PDF de validación'.
        WRITE: /.
      ENDIF.

      " Mostrar información del PDF
      WRITE: / '📊 Información del PDF:'.
      WRITE: / '   Tamaño (xstring):', xstrlen( lv_pdf_xstring ), 'bytes'.
      WRITE: / '   Tamaño (base64):', strlen( lv_pdf_base64 ), 'caracteres'.
      WRITE: /.

    ELSE.
      WRITE: / '⚠️  No se recibió PDF de validación'.
      WRITE: /.
    ENDIF.

  ELSE.
    WRITE: / '❌ Error en la validación'.
    WRITE: / '   HTTP Status:', lv_http_status.
    WRITE: / '   Error:', lv_error_message.
    WRITE: /.
  ENDIF.

  WRITE: / '═══════════════════════════════════════════════════════════════'.
