import sys
import pywintypes

from definitions import (
    MiroMainScreenError,
    InsertedPoLinesDoNotMatch,
    BalanceNotZero,
    PoLineCountMismatch,
    UnexpectedError,
    TooManyGLAccounts,
)


def zero_amount_incoming_invoice(
    company_code, reference_number, invoice_date, tax_code, po_lines_data, session=None
):

    if session is None:
        # this must be stored or installed locally - https://github.com/Qubhis/sapguirpa
        from sapguirpa import SapGuiRpa

        sap = SapGuiRpa()
        sap.attach_to_session()
        sap.gui_maximize()
        disconnect_when_finished = True
    else:
        sap = session
        disconnect_when_finished = False

    sap.start_transaction("MIRO")

    # in every fresh session, there is a modal window asking for a company code
    # but we must also anticipate situation when there is no modal window
    try:
        sap.insert_value("wnd[1]/usr/ctxtBKPF-BUKRS", company_code)
        sap.send_vkey(0, window="wnd[1]")

    except pywintypes.com_error as modal_error:
        not_found_error = "The control could not be found by id."
        if (
            modal_error.args[0] == -2147352567
            and modal_error.args[2][2] == not_found_error
        ):
            # if there is no modal, we need to ensure that we are working with
            # correct company code. SAP initialize MIRO with company code used earlier
            try:
                sap.press_or_select("wnd[0]/mbar/menu[1]/menu[0]")
                sap.insert_value("wnd[1]/usr/ctxtBKPF-BUKRS", company_code)
                sap.send_vkey(0, window="wnd[1]")

            except pywintypes.com_error as modal_error:
                not_found_error = "The control could not be found by id."
                if (
                    modal_error.args[0] == -2147352567
                    and modal_error.args[2][2] == not_found_error
                ):
                    raise MiroMainScreenError(
                        "Changing a company code in main screen of MIRO was not successfull."
                    )
                else:
                    raise
        else:
            raise

    invoice_date_id = "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/tabsHEADER/tabpHEADER_TOTAL/ssubHEADER_SCREEN:SAPLFDCB:0010/ctxtINVFO-BLDAT"
    sap.insert_value(
        invoice_date_id, invoice_date
    )  # posting date is prefiled with today date
    sap.send_vkey(0)

    reference_number_id = "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/tabsHEADER/tabpHEADER_TOTAL/ssubHEADER_SCREEN:SAPLFDCB:0010/txtINVFO-XBLNR"
    sap.insert_value(reference_number_id, reference_number)
    sap.send_vkey(0)

    tax_code_id = "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/tabsHEADER/tabpHEADER_TOTAL/ssubHEADER_SCREEN:SAPLFDCB:0010/cmbINVFO-MWSKZ"
    sap.insert_value(tax_code_id, tax_code)

    calculate_tax_id = "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/tabsHEADER/tabpHEADER_TOTAL/ssubHEADER_SCREEN:SAPLFDCB:0010/chkINVFO-XMWST"
    sap.press_or_select(calculate_tax_id, check=False)

    # purchase order selection
    sap.insert_value(
        "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/subITEMS:SAPLMR1M:6010/tabsITEMTAB/tabpITEMS_PO/ssubTABS:SAPLMR1M:6020/cmbRM08M-REFERENZBELEGTYP",
        "1",
    )
    # goods/service selection
    sap.insert_value(
        "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/subITEMS:SAPLMR1M:6010/tabsITEMTAB/tabpITEMS_PO/ssubTABS:SAPLMR1M:6020/subREFERENZBELEG:SAPLMR1M:6211/cmbRM08M-XWARE_BNK",
        "1",
    )
    # layout -> all information
    sap.insert_value(
        "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/subITEMS:SAPLMR1M:6010/tabsITEMTAB/tabpITEMS_PO/ssubTABS:SAPLMR1M:6020/subITEM:SAPLMR1M:6310/cmbRM08M-ITEM_LIST_VERSION",
        "7_6310",
    )

    #  using more allocation criteria button (multiple selection) allow us to insert multiple PO numbers
    sap.press_or_select(
        "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/subITEMS:SAPLMR1M:6010/tabsITEMTAB/tabpITEMS_PO/ssubTABS:SAPLMR1M:6020/subREFERENZBELEG:SAPLMR1M:6211/btnRM08M-XMSEL"
    )
    # prepare set of unique PO numbers
    unique_po_numbers = set(po_lines_data.keys())
    assert len(unique_po_numbers) > 0, "unique_po_numbers is empty set!"

    row_index = 0  # for row index used in ID path
    for counter, po_number in enumerate(unique_po_numbers):
        sap.insert_value(
            f"wnd[1]/usr/subMSEL:SAPLMR1M:6221/tblSAPLMR1MTC_MSEL_BEST/ctxtRM08M-EBELN[0,{row_index}]",
            po_number,
        )
        if counter % 7 == 0 and counter != 0:
            # only visible rows are indexed. First visible row has index zero, always.
            row_index = 0
            # scrolling 8 rows at once (simulation of pagedown)
            sap.get_element_by_id(
                "wnd[1]/usr/subMSEL:SAPLMR1M:6221/tblSAPLMR1MTC_MSEL_BEST"
            ).verticalScrollbar.position = (counter + 1)
        else:
            row_index += 1

    # adopt button (to get back to wnd[0])
    sap.press_or_select("wnd[1]/tbar[0]/btn[8]")
    # NOTE: we can expect exception when an invoicing partner is missing or is not created for the company code

    # check if baseline date is filled and fill it if not
    # go to payment tab
    sap.press_or_select(
        "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/tabsHEADER/tabpHEADER_PAY"
    )

    # here we expect warning modal window about Down payment
    try:
        modal_warning = sap.get_element_by_id("wnd[1]/usr")
        warning_text = modal_warning.children(2).text
        expected_text = "Down payment on current assets exist"
        if warning_text == expected_text:
            sap.send_vkey(0)
        else:
            raise UnexpectedError("Unexpected modal window, please check via debugger")

    except pywintypes.com_error as com_error:
        if com_error.args[0] == -2147352567:
            pass
        else:
            raise

    baseline_date_id = "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/tabsHEADER/tabpHEADER_PAY/ssubHEADER_SCREEN:SAPLFDCB:0020/ctxtINVFO-ZFBDT"
    baseline_date = sap.get_element_text(baseline_date_id)
    if not baseline_date:
        sap.insert_value(baseline_date_id, invoice_date)
    # going back to basic data tab
    sap.press_or_select(
        "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/tabsHEADER/tabpHEADER_TOTAL"
    )

    total_items_count = int(
        sap.get_element_text(
            "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/subITEMS:SAPLMR1M:6010/tabsITEMTAB/tabpITEMS_PO/ssubTABS:SAPLMR1M:6020/subITEM:SAPLMR1M:6310/txtRM08M-ANZPS_TOTAL"
        )
    )

    po_number_lines = []
    for po_number, po_data in po_lines_data.items():
        for po_item, _ in po_data.items():
            po_number_lines.append(po_number + "/" + po_item)

    po_lines_count = len(po_number_lines)
    assert (
        po_lines_count <= total_items_count
    ), "There are more PO lines than available in MIRO selection"

    # deselect all items
    sap.press_or_select(
        "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/subITEMS:SAPLMR1M:6010/tabsITEMTAB/tabpITEMS_PO/ssubTABS:SAPLMR1M:6020/subITEM:SAPLMR1M:6310/btnDESELECT_ALL"
    )

    table_control_id = "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/subITEMS:SAPLMR1M:6010/tabsITEMTAB/tabpITEMS_PO/ssubTABS:SAPLMR1M:6020/subITEM:SAPLMR1M:6310/tblSAPLMR1MTC_MR1M"
    table_control = sap.get_element_by_id(table_control_id)
    index = 0
    # for later check if we inserted all lines we've got as input
    inserted_po_lines = []
    # for determining when to scroll down
    modulo = table_control.visibleRowCount - 1
    for counter in range(total_items_count):
        current_po_number = sap.get_element_text(
            table_control_id + f"/txtDRSEG-EBELN[5,{index}]"
        )
        current_po_line = sap.get_element_text(
            table_control_id + f"/txtDRSEG-EBELP[6,{index}]"
        )
        line_data = po_lines_data.get(current_po_number, None).get(
            current_po_line, None
        )
        if line_data is not None:
            po_line_amount = line_data.get("amount")
            po_line_quantity = (
                "1" if line_data.get("quantity") == "0" else line_data.get("quantity")
            )
            inserted_po_lines.append(current_po_number + "/" + current_po_line)
        else:
            po_line_amount, po_line_quantity = "", ""

        sap.insert_value(
            table_control_id + f"/txtDRSEG-WRBTR[1,{index}]", po_line_amount
        )
        sap.insert_value(
            table_control_id + f"/txtDRSEG-MENGE[2,{index}]", po_line_quantity
        )
        # we need to select line manually to consider it in invoice
        # this is done automatically by SAP, however,
        # in some cases it's not -> manual selection is then needed
        if po_line_amount != "":
            sap.select_absolute_row_table_control(table_control_id, counter)

        if index % modulo == 0 and index > 0:
            # scroll to the next position
            # we need to provide absolute position within the table, which will
            # always be counter + 1
            table_control = sap.get_element_by_id(
                table_control_id
            )  # we need to re-instantiate
            table_control.verticalScrollbar.position = counter + 1
            index = 0

            # sometimes a warning message is restricting to continue,
            # and we need to confirm the warning for each concerned row
            while True:
                message_type, message_text = sap.get_status_bar()
                expected_messages = (
                    "Please check: order price quantity set automatically",
                )
                if message_type == "W" and message_text in expected_messages:
                    sap.send_vkey(0)
                # break when there is no message
                elif message_type == "" and message_text == "":
                    break
        else:
            index += 1

    # press enter to refresh the screen and balance field
    sap.press_or_select(
        "wnd[0]/usr/subHEADER_AND_ITEMS:SAPLMR1M:6005/subITEMS:SAPLMR1M:6010/tabsITEMTAB/tabpITEMS_PO/ssubTABS:SAPLMR1M:6020/subITEM:SAPLMR1M:6310/btnREFRESH"
    )

    # Warning message order price quantity set automatically
    # "Please check: order price quantity set automatically"
    while True:
        message_type, message_text = sap.get_status_bar()
        expected_messages = ("Please check: order price quantity set automatically",)
        if message_type == "W" and message_text in expected_messages:
            sap.send_vkey(0)
        # break when there is no message
        elif message_type == "" and message_text == "":
            break

    # here we need to check that we have inserted all po lines
    if len(po_number_lines) == len(inserted_po_lines):
        for line in po_number_lines:
            if not line in inserted_po_lines:
                print(f"not found in inserted_lines: {line}")
                print(f"po_number_lines: {po_number_lines}")
                print(f"inserted_po_lines: {inserted_po_lines}\n")
                raise InsertedPoLinesDoNotMatch("There are diferences in above lists")
    else:
        raise PoLineCountMismatch("There is different number of PO lines than expected")

    # check that balance is zero
    displayed_balance = sap.get_element_text("wnd[0]/usr/txtRM08M-DIFFERENZ")
    # remove trailing whitespace
    displayed_balance = displayed_balance.strip()
    if displayed_balance not in ("0,00", "0.00"):
        raise BalanceNotZero("Balance is not zero")

    sap.press_or_select("wnd[0]/tbar[1]/btn[43]")
    simulate_modal = sap.get_element_by_id("wnd[1]/usr/tblSAPLMR1MTC_MR1M_PB")
    gl_accounts = set()
    gl_account_fields = simulate_modal.findAllByName("ACCIT-HKONT", "GuiTextField")
    for index in range(gl_account_fields.count):
        gl_accounts.add(gl_account_fields.ElementAt(index).text)
    if len(gl_accounts) > 1:
        raise TooManyGLAccounts("There are more then one G/L accounts involved")

    sap.send_vkey(11)

    status_bar = sap.get_status_bar()

    if disconnect_when_finished:
        sap.gui_restore_size()
        sap.disconnect()

    return status_bar


if __name__ == "__main__":
    pass
