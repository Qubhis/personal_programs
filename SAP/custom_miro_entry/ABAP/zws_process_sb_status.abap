FUNCTION zws_process_sb_status .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SB_DOC_NUMBER) TYPE  ZWSSBNR
*"     VALUE(PROCESSING_STATUS) TYPE  ZWSSBPROCSTAT
*"     VALUE(MESSAGE) TYPE  BAPI_MSG OPTIONAL
*"  EXCEPTIONS
*"      LOG_NOT_UPDATED
*"      INCORRECT_STATUS_PROVIDED
*"      DOCUMENT_NOT_FOUND
*"      HEADER_NOT_UPDATED
*"----------------------------------------------------------------------
  CONSTANTS success TYPE zwssbprocstat VALUE 'S'.
  CONSTANTS error TYPE zwssbprocstat VALUE 'X'.

*--------------------------------------------------------------------*
  CASE processing_status.
    WHEN success.
      CALL FUNCTION 'ZWS_SET_SB_AS_APPROVED'
        EXPORTING
          sb_doc_number = sb_doc_number     " Self billing document
        EXCEPTIONS
          not_found     = 1                " document not found
          OTHERS        = 2.
      IF sy-subrc = 1.
        RAISE document_not_found.
      ELSEIF sy-subrc = 2.
        RAISE header_not_updated.
      ENDIF.

      CALL FUNCTION 'ZWS_SB_UPDATE_HEADER_LOG'
        EXPORTING
          sb_document_number = sb_doc_number
          processing_status  = processing_status
          message            = message
        EXCEPTIONS
          not_found          = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.
        RAISE log_not_updated.
      ENDIF.

    WHEN error.
      CALL FUNCTION 'ZWS_SB_UPDATE_HEADER_LOG'
        EXPORTING
          sb_document_number = sb_doc_number
          processing_status  = processing_status
          message            = message
        EXCEPTIONS
          not_found          = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.
        RAISE log_not_updated.
      ENDIF.

    WHEN OTHERS.
      RAISE incorrect_status_provided.
  ENDCASE.


ENDFUNCTION.