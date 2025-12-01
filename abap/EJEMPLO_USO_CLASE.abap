*&---------------------------------------------------------------------*
*& EJEMPLOS DE USO DE LA CLASE ZCL_PLIEGO_VALIDATOR
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* EJEMPLO 0: Verificar conexión con el servidor (Health Check)
*----------------------------------------------------------------------*
DATA: lv_http_status   TYPE i,
      lv_response      TYPE string,
      lv_error_message TYPE string,
      lv_is_online     TYPE abap_bool.

CALL METHOD zcl_pliego_validator=>check_connection
  IMPORTING
    ev_http_status   = lv_http_status    " 200 = OK
    ev_response      = lv_response       " Respuesta JSON del servidor
    ev_error_message = lv_error_message  " Mensaje descriptivo
    ev_is_online     = lv_is_online.     " X = Online, '' = Offline

IF lv_is_online = abap_true.
  MESSAGE 'Servidor disponible' TYPE 'S'.
ELSE.
  MESSAGE lv_error_message TYPE 'E'.
ENDIF.

*----------------------------------------------------------------------*
* EJEMPLO 1: Validar desde archivo (frontend)
*----------------------------------------------------------------------*
DATA: lv_pdf_path      TYPE string VALUE 'C:\temp\pliego.pdf',
      lv_pdf_xstring   TYPE xstring,
      lv_pdf_base64    TYPE string,
      lv_http_status   TYPE i,
      lv_error_message TYPE string.

CALL METHOD zcl_pliego_validator=>validate_pliego_from_file
  EXPORTING
    iv_pdf_path      = lv_pdf_path
    iv_pliego_id     = 'PCAP_CT1074723'
    iv_context_id    = 'DOCUMENTOS_VALIDACION'
    iv_username      = sy-uname
  IMPORTING
    ev_pdf_xstring   = lv_pdf_xstring    " PDF validado en binario
    ev_pdf_base64    = lv_pdf_base64     " PDF validado en base64
    ev_http_status   = lv_http_status    " 200 = OK, 401 = Unauthorized, etc.
    ev_error_message = lv_error_message. " Mensaje de error si falla

IF lv_http_status = 200.
  " Éxito - usar lv_pdf_xstring o lv_pdf_base64
  MESSAGE 'Validación completada' TYPE 'S'.
ELSE.
  " Error - mostrar lv_error_message
  MESSAGE lv_error_message TYPE 'E'.
ENDIF.

*----------------------------------------------------------------------*
* EJEMPLO 2: Validar desde base64 (backend)
*----------------------------------------------------------------------*
DATA: lv_pdf_base64_input TYPE string,
      lv_pdf_xstring_output TYPE xstring,
      lv_pdf_base64_output TYPE string,
      lv_http_status TYPE i,
      lv_error_message TYPE string.

" Supongamos que ya tienes el PDF en base64
lv_pdf_base64_input = 'JVBERi0xLjQK...'. " Tu PDF en base64

CALL METHOD zcl_pliego_validator=>validate_pliego_from_base64
  EXPORTING
    iv_pdf_base64    = lv_pdf_base64_input
    iv_filename      = 'pliego_obra_civil.pdf'
    iv_pliego_id     = 'PCAP_CT1074723'
    iv_context_id    = 'DOCUMENTOS_VALIDACION'
    iv_username      = 'JESUS.MELIAN'
  IMPORTING
    ev_pdf_xstring   = lv_pdf_xstring_output  " PDF validado en binario
    ev_pdf_base64    = lv_pdf_base64_output   " PDF validado en base64
    ev_http_status   = lv_http_status
    ev_error_message = lv_error_message.

IF lv_http_status = 200.
  " Éxito - usar lv_pdf_xstring_output o lv_pdf_base64_output
  MESSAGE 'Validación completada' TYPE 'S'.
ELSE.
  " Error
  MESSAGE lv_error_message TYPE 'E'.
ENDIF.

*----------------------------------------------------------------------*
* EJEMPLO 3: Integración en tu código existente
*----------------------------------------------------------------------*
METHOD tu_metodo_existente.

  DATA: lv_pdf_base64      TYPE string,
        lv_pdf_validado    TYPE xstring,
        lv_http_status     TYPE i,
        lv_error_message   TYPE string.

  " ... tu código que obtiene el PDF en base64 ...
  lv_pdf_base64 = im_pdf_base64. " O como lo obtengas

  " Validar el pliego
  CALL METHOD zcl_pliego_validator=>validate_pliego_from_base64
    EXPORTING
      iv_pdf_base64    = lv_pdf_base64
      iv_filename      = im_filename
      iv_pliego_id     = im_pliego_id
      iv_context_id    = 'DOCUMENTOS_VALIDACION'
      iv_username      = sy-uname
    IMPORTING
      ev_pdf_xstring   = lv_pdf_validado
      ev_http_status   = ex_num_status      " Tu parámetro de salida
      ev_error_message = ex_msg.            " Tu parámetro de salida

  " Verificar resultado
  IF ex_num_status = 200.
    " Éxito - devolver el PDF validado
    ex_pdf_validado = lv_pdf_validado.
    ex_msg = 'Validación completada exitosamente'.
  ELSE.
    " Error - ex_msg ya contiene el mensaje de error
    " No hacer nada más, el error ya está en ex_msg
  ENDIF.

ENDMETHOD.

*----------------------------------------------------------------------*
* EJEMPLO 4: Guardar PDF validado en servidor (no frontend)
*----------------------------------------------------------------------*
DATA: lv_pdf_xstring   TYPE xstring,
      lv_http_status   TYPE i,
      lv_error_message TYPE string,
      lv_filepath      TYPE string VALUE '/tmp/pliego_validado.pdf'.

CALL METHOD zcl_pliego_validator=>validate_pliego_from_base64
  EXPORTING
    iv_pdf_base64    = lv_pdf_base64
    iv_filename      = 'pliego.pdf'
    iv_pliego_id     = 'PCAP_CT1074723'
    iv_context_id    = 'DOCUMENTOS_VALIDACION'
    iv_username      = sy-uname
  IMPORTING
    ev_pdf_xstring   = lv_pdf_xstring
    ev_http_status   = lv_http_status
    ev_error_message = lv_error_message.

IF lv_http_status = 200.
  " Guardar en servidor (application server)
  OPEN DATASET lv_filepath FOR OUTPUT IN BINARY MODE.
  IF sy-subrc = 0.
    TRANSFER lv_pdf_xstring TO lv_filepath.
    CLOSE DATASET lv_filepath.
    MESSAGE 'PDF guardado en servidor' TYPE 'S'.
  ENDIF.
ENDIF.

*----------------------------------------------------------------------*
* EJEMPLO 5: Manejo completo de errores
*----------------------------------------------------------------------*
DATA: lv_pdf_xstring   TYPE xstring,
      lv_pdf_base64    TYPE string,
      lv_http_status   TYPE i,
      lv_error_message TYPE string.

CALL METHOD zcl_pliego_validator=>validate_pliego_from_base64
  EXPORTING
    iv_pdf_base64    = lv_pdf_base64
    iv_filename      = 'pliego.pdf'
    iv_pliego_id     = 'PCAP_CT1074723'
    iv_context_id    = 'DOCUMENTOS_VALIDACION'
    iv_username      = sy-uname
  IMPORTING
    ev_pdf_xstring   = lv_pdf_xstring
    ev_pdf_base64    = lv_pdf_base64
    ev_http_status   = lv_http_status
    ev_error_message = lv_error_message.

CASE lv_http_status.
  WHEN 200.
    " Éxito
    MESSAGE 'Validación completada' TYPE 'S'.
    
  WHEN 400.
    " Bad Request - Error en los datos enviados
    MESSAGE lv_error_message TYPE 'E'.
    
  WHEN 401.
    " Unauthorized - Token inválido o expirado
    MESSAGE 'Error de autenticación. Contacta al administrador.' TYPE 'E'.
    
  WHEN 404.
    " Not Found - Configuración no encontrada
    MESSAGE 'Servicio no configurado. Verifica tabla ZPA_RPAIA_UTILS.' TYPE 'E'.
    
  WHEN 500.
    " Internal Server Error
    MESSAGE 'Error en el servidor. Intenta más tarde.' TYPE 'E'.
    
  WHEN OTHERS.
    " Otro error
    MESSAGE lv_error_message TYPE 'E'.
ENDCASE.

*----------------------------------------------------------------------*
* EJEMPLO 6: Verificar conexión antes de validar (buena práctica)
*----------------------------------------------------------------------*
DATA: lv_is_online     TYPE abap_bool,
      lv_http_status   TYPE i,
      lv_error_message TYPE string,
      lv_pdf_xstring   TYPE xstring.

" Primero verificar que el servidor esté disponible
CALL METHOD zcl_pliego_validator=>check_connection
  IMPORTING
    ev_is_online = lv_is_online.

IF lv_is_online = abap_true.
  " Servidor disponible - proceder con validación
  CALL METHOD zcl_pliego_validator=>validate_pliego_from_base64
    EXPORTING
      iv_pdf_base64 = lv_pdf_base64
      iv_pliego_id = 'PCAP_CT1074723'
    IMPORTING
      ev_pdf_xstring = lv_pdf_xstring
      ev_http_status = lv_http_status
      ev_error_message = lv_error_message.

  IF lv_http_status = 200.
    MESSAGE 'Validación completada' TYPE 'S'.
  ELSE.
    MESSAGE lv_error_message TYPE 'E'.
  ENDIF.
ELSE.
  " Servidor no disponible - no intentar validación
  MESSAGE 'Servidor no disponible. Intenta más tarde.' TYPE 'E'.
ENDIF.

*----------------------------------------------------------------------*
* NOTAS IMPORTANTES:
*----------------------------------------------------------------------*
* 1. La clase usa la tabla ZPA_RPAIA_UTILS para obtener URL y endpoint
*    Asegúrate de tener un registro con name_service = 'plecs_rev'
*
* 2. La clase usa ZCL_RPAIA_AUTH=>GET_BEARER_TOKEN para autenticación
*    Asegúrate de tener configurado 'AICOREAUTH'
*
* 3. El PDF validado se devuelve en DOS formatos:
*    - ev_pdf_xstring: Binario (xstring) - Usar para guardar archivos
*    - ev_pdf_base64: Base64 (string) - Usar para APIs o transmisión
*
* 4. Los parámetros opcionales tienen valores por defecto:
*    - iv_pliego_id: Vacío (se genera automático en el servidor)
*    - iv_context_id: Vacío (no usa contexto RAG)
*    - iv_username: Vacío (se registra como 'anonymous')
*    - iv_filename: 'documento.pdf' (solo para método from_base64)
*
* 5. HTTP Status codes comunes:
*    - 200: OK - Validación exitosa
*    - 400: Bad Request - Error en los datos
*    - 401: Unauthorized - Error de autenticación
*    - 404: Not Found - Configuración no encontrada
*    - 500: Internal Server Error - Error del servidor
*----------------------------------------------------------------------*
