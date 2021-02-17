FUNCTION ZWS_SET_SB_AS_APPROVED.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(SB_DOC_NUMBER) TYPE  ZWSSBNR
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------

  DATA(date_of_entry) = sy-datum.

  SELECT SINGLE usnam
  FROM zwssbapprhdr_log
  INTO @DATA(approver_id)
  WHERE sb_document = @sb_doc_number.

  IF approver_id IS INITIAL.
    approver_id = sy-uname.
  ENDIF.

  SELECT SINGLE *
  FROM zws_sbheader
  INTO @DATA(header)
  WHERE zsbnr = @sb_doc_number.

  IF header IS INITIAL.
    RAISE not_found.
  ENDIF.

  header-zapprover = approver_id.
  header-zapprdate = date_of_entry.
  header-zappr = 'X'.

  UPDATE zws_sbheader FROM header.
  COMMIT WORK AND WAIT.

ENDFUNCTION.