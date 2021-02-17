FUNCTION zws_sb_update_header_log.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(SB_DOCUMENT_NUMBER) TYPE  ZWSSBNR
*"     REFERENCE(PROCESSING_STATUS) TYPE  ZWSSBPROCSTAT DEFAULT 'X'
*"     REFERENCE(MESSAGE) TYPE  BAPI_MSG
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------

  CONSTANTS:
    rpa_scheduled TYPE c VALUE 'R',
    rpa_error     TYPE c VALUE 'X'.

*--------------------------------------------------------------------*
  SELECT SINGLE *
    FROM zwssbapprhdr_log
    INTO @DATA(wa_header_log)
    WHERE sb_document = @sb_document_number
    AND process_status IN (@rpa_scheduled, @rpa_error).

  IF wa_header_log IS NOT INITIAL.
    wa_header_log-process_status = processing_status.
    wa_header_log-error_message = message.

    UPDATE zwssbapprhdr_log FROM wa_header_log.
    COMMIT WORK AND WAIT.
  ELSE.
    RAISE not_found.
  ENDIF.

ENDFUNCTION.