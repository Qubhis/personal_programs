
CLASS lcx_data_not_found DEFINITION INHERITING FROM cx_dynamic_check FINAL.
ENDCLASS.


CLASS lcl_selfbilling_rpa DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS:
      constructor,
      get_data_json
        RETURNING VALUE(serialized_json) TYPE string,
      get_header_and_items
        IMPORTING VALUE(doc_number)  TYPE zwssbnr
        RETURNING VALUE(sb_doc_data) TYPE zws_sb_document_data_tt
        RAISING   lcx_data_not_found,
      prepare_data_for_rpa,
      invoice_duplicity_check
        IMPORTING sb_doc_data            TYPE zws_sb_document_data_tt
        RETURNING VALUE(duplicity_found) TYPE abap_bool.

  PRIVATE SECTION.

    TYPES: BEGIN OF sb_document,
             doc_number TYPE zwssbnr,
           END OF sb_document.

    CONSTANTS:
      rpa_scheduled TYPE c VALUE 'R',
      rpa_error     TYPE c VALUE 'X',
      rpa_success   TYPE c value 'S'.

    DATA sb_doc_list TYPE TABLE OF sb_document.
    DATA final_sb_doc_data TYPE zws_sb_document_data_tt.
    DATA temp_sb_doc_data TYPE zws_sb_document_data_tt.
    DATA sb_doc_all_lines_zero_value TYPE table of sb_document.
    DATA message_text TYPE bapi_msg.

ENDCLASS.


CLASS lcl_selfbilling_rpa IMPLEMENTATION.

    METHOD constructor.
  
      SELECT sb_document
        FROM zwssbapprhdr_log
        INTO TABLE @sb_doc_list
        WHERE process_status IN ( @rpa_scheduled, @rpa_error ).
  
    ENDMETHOD.
  
    METHOD get_data_json.
  
      LOOP AT sb_doc_list INTO DATA(wa_sb_doc).
        CLEAR temp_sb_doc_data.
        CLEAR message_text.
        clear sb_doc_all_lines_zero_value.
  
        TRY.
            temp_sb_doc_data = me->get_header_and_items( wa_sb_doc-doc_number ).
  
          CATCH lcx_data_not_found.
            " update status
            message_text = |{ TEXT-e05 } { wa_sb_doc-doc_number }|.
  
            CALL FUNCTION 'ZWS_SB_UPDATE_HEADER_LOG'
              EXPORTING
                sb_document_number = wa_sb_doc-doc_number
                processing_status  = rpa_error
                message            = message_text.
  
            CONTINUE.
  
        ENDTRY.
  
        " adjust for RPA
        me->prepare_data_for_rpa( ).
        " check if all lines zero value
        if line_exists( sb_doc_all_lines_zero_value[ 1 ] ).
  
          CALL FUNCTION 'ZWS_PROCESS_SB_STATUS'
              EXPORTING
                sb_doc_number = wa_sb_doc-doc_number
                processing_status  = rpa_success
                message            = TEXT-s01.
  
        else.
          " inv duplicity check
          DATA(duplicity_found) = me->invoice_duplicity_check( temp_sb_doc_data ).
  
          IF duplicity_found = abap_true.
            CALL FUNCTION 'ZWS_SB_UPDATE_HEADER_LOG'
              EXPORTING
                sb_document_number = wa_sb_doc-doc_number
                processing_status  = rpa_error
                message            = TEXT-e01.
  
            CLEAR duplicity_found.
            CONTINUE.
          ENDIF.
  
          MOVE-CORRESPONDING temp_sb_doc_data TO final_sb_doc_data KEEPING TARGET LINES.
        endif.
      ENDLOOP.
  
      " return serialized json data
      serialized_json = /ui2/cl_json=>serialize( data = final_sb_doc_data ).
  
    ENDMETHOD.
  
    METHOD get_header_and_items.
  
      TYPES: BEGIN OF header_data,
               zsbnr   TYPE zwssbnr,
               bukrs   TYPE bukrs,
               invdate TYPE cpudt_vk,
               refnum  TYPE xblnr1,
             END OF header_data.
  
      DATA wa_header TYPE header_data.
      DATA t_sb_doc_data TYPE TABLE OF zws_sb_document_data.
      DATA wa_sb_doc_data LIKE LINE OF t_sb_doc_data.
      "-----------------------------------------------------
      SELECT SINGLE zsbnr, bukrs
        FROM zws_sbheader
        INTO CORRESPONDING FIELDS OF @wa_header
        WHERE zsbnr = @doc_number.
  
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_data_not_found.
      ENDIF.
  
      SELECT zsbitem, ebeln, ebelp, erfmg, netwr
        FROM zws_sbitem
        INTO TABLE @DATA(it_items)
        WHERE zsbnr = @doc_number.
  
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_data_not_found.
      ENDIF.
  
      MOVE-CORRESPONDING wa_header TO wa_sb_doc_data.
      wa_sb_doc_data-zsbitems = it_items.
      APPEND wa_sb_doc_data TO sb_doc_data.
  
    ENDMETHOD.
  
    METHOD prepare_data_for_rpa.
  
      DATA it_zsbitems TYPE zws_sb_items_tt.
      DATA gr_quantity_zsbitems TYPE zws_sb_items_tt.
  
  
  
      LOOP AT temp_sb_doc_data ASSIGNING FIELD-SYMBOL(<temp_sb_doc_data>).
        " delete lines which are not relevant -> zero price
        DELETE <temp_sb_doc_data>-zsbitems WHERE netwr = 0.
        if lines( <temp_sb_doc_data>-zsbitems ) eq 0.
          " skip sbdoc and update log as approved without invoice creation
          append <temp_sb_doc_data>-zsbnr to sb_doc_all_lines_zero_value.
          Continue.
        endif.
        " fetching data for zero quantity lines
        it_zsbitems = <temp_sb_doc_data>-zsbitems.
        DELETE it_zsbitems WHERE erfmg <> 0.
  
        IF lines( it_zsbitems ) <> 0.
          SELECT ekbe~menge AS erfmg
                 zws_sbitem~zsbitem
            FROM ekbe
            INNER JOIN zws_sbitem
            ON ekbe~ebeln = zws_sbitem~ebeln
              AND ekbe~ebelp = zws_sbitem~ebelp
              AND ekbe~belnr = zws_sbitem~mblnr
              AND ekbe~buzei = zws_sbitem~zeile
            INTO CORRESPONDING FIELDS OF TABLE gr_quantity_zsbitems
            FOR ALL ENTRIES IN it_zsbitems
            WHERE zws_sbitem~zsbnr = <temp_sb_doc_data>-zsbnr
              AND zws_sbitem~zsbitem = it_zsbitems-zsbitem
              AND ekbe~vgabe = '1'.
        ENDIF.
  
        LOOP AT <temp_sb_doc_data>-zsbitems ASSIGNING FIELD-SYMBOL(<zsbitems>).
          TRY.
              "update quantity if it was zero (i.e. is in it_zsbites table)
              <zsbitems>-erfmg = gr_quantity_zsbitems[ zsbitem = <zsbitems>-zsbitem ]-erfmg.
            CATCH cx_sy_itab_line_not_found.
              " silently passed
          ENDTRY.
          " cast to absolute value for RPA input
          <zsbitems>-erfmg = abs( <zsbitems>-erfmg ).
          <zsbitems>-netwr = abs( <zsbitems>-netwr ).
        ENDLOOP.
  
        " prepare rest of header fields
        <temp_sb_doc_data>-invdate = sy-datum.
        " reference number preparation
        DATA(text_zsbnr) = CONV string( <temp_sb_doc_data>-zsbnr ).
        SHIFT text_zsbnr LEFT DELETING LEADING '0'.
        <temp_sb_doc_data>-refnum = |{ text_zsbnr }_{ sy-datum }|.
  
        " fetch tax code based on first sb item
        DATA(first_sbitem) = <temp_sb_doc_data>-zsbitems[ 1 ].
        SELECT SINGLE mwskz INTO @DATA(tax_code)
          FROM ekpo
          WHERE ebeln = @first_sbitem-ebeln
            AND ebelp = @first_sbitem-ebelp.
  
        <temp_sb_doc_data>-mwskz = tax_code.
      ENDLOOP.
  
    ENDMETHOD.
  
    METHOD invoice_duplicity_check.
  
      DATA sum TYPE dmbtr.
  
      DATA(doc_number) = sb_doc_data[ 1 ]-zsbnr.
      DATA(it_sbitems) = sb_doc_data[ 1 ]-zsbitems.
      " empty it_sbitems is not expected in this place!
  
      SELECT ekbe~belnr, ekbe~buzei, ekbe~dmbtr, ekbe~shkzg,
             zws_sbitem~zsbnr, zws_sbitem~zsbitem
        FROM ekbe
        INNER JOIN zws_sbitem
          ON ekbe~ebeln = zws_sbitem~ebeln
          AND ekbe~ebelp = zws_sbitem~ebelp
          AND ekbe~lfbnr = zws_sbitem~mblnr
          AND ekbe~lfpos = zws_sbitem~zeile
        INTO TABLE @DATA(po_history)
        FOR ALL ENTRIES IN @it_sbitems
          WHERE zws_sbitem~zsbnr = @doc_number
            AND zws_sbitem~zsbitem = @it_sbitems-zsbitem
            AND ekbe~vgabe EQ '2'.
  
      " loop and check for duplicity by checking a sum of values
      LOOP AT it_sbitems INTO DATA(wa_sbitems).
  
        CLEAR sum.
        LOOP AT po_history INTO DATA(wa_po_history) WHERE zsbitem = wa_sbitems-zsbitem.
          "convert to negative value for correct calculation
          IF wa_po_history-shkzg = 'H'. "Credit
            wa_po_history-dmbtr = wa_po_history-dmbtr * -1.
          ENDIF.
          sum = sum + wa_po_history-dmbtr.
        ENDLOOP.
  
        IF sum <> 0.
          duplicity_found = abap_true.
          RETURN.
        ENDIF.
  
      ENDLOOP.
  
    ENDMETHOD.
  
  ENDCLASS.