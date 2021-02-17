FUNCTION zws_get_sb_data_for_rpa.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(S_JSON) TYPE  STRING
*"----------------------------------------------------------------------

  DATA(selfbilling_rpa) = NEW lcl_selfbilling_rpa( ).
  s_json = selfbilling_rpa->get_data_json( ).

ENDFUNCTION.