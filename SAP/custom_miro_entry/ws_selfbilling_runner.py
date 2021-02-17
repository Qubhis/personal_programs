import datetime
import sys
import pywintypes

from rfc_functions import get_sb_data_for_rpa, set_sb_status
from ws_miro import zero_amount_incoming_invoice
from definitions import (
    MiroMainScreenError,
    InsertedPoLinesDoNotMatch,
    BalanceNotZero,
    PoLineCountMismatch,
    UnexpectedError,
    TooManyGLAccounts,
)

# this must be stored or installed locally - https://github.com/Qubhis/sapguirpa
from sapguirpa import SapGuiRpa

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RFC to SAP
selfbilling_data = get_sb_data_for_rpa()
# instantiate SAP GUI scripting session
sap = SapGuiRpa()
sap.attach_to_session()
sap.gui_maximize()

for selfbilling_document in selfbilling_data:
    company_code = selfbilling_document["BUKRS"]
    reference_number = selfbilling_document["REFNUM"]
    invoice_date_iso = selfbilling_document["INVDATE"]
    invoice_date = ".".join(invoice_date_iso.rsplit("-")[::-1])
    tax_code = selfbilling_document["MWSKZ"]
    document_items = selfbilling_document["ZSBITEMS"]
    # parse data into format for our MIRO entry
    po_lines_data = {}
    for item in document_items:
        po_number = item["EBELN"]
        po_item = item["EBELP"]
        amount = item["NETWR"]
        quantity = item["ERFMG"]
        amount_quantity = {"amount": amount, "quantity": quantity}
        try:
            po_num_dict = po_lines_data[po_number]
            po_num_dict[po_item] = amount_quantity
        except KeyError:
            line_dict = {po_item: amount_quantity}
            po_lines_data[po_number] = line_dict

    try:
        # enter Incoming invoice via MIRO
        status = zero_amount_incoming_invoice(
            company_code=company_code,
            reference_number=reference_number,
            invoice_date=invoice_date,
            tax_code=tax_code,
            po_lines_data=po_lines_data,
            session=sap,
        )
    # each exception makes another RFC to SAP with the error
    except MiroMainScreenError:
        set_sb_status(selfbilling_document["ZSBNR"], "X", "Miro screen error")

    except InsertedPoLinesDoNotMatch:
        set_sb_status(selfbilling_document["ZSBNR"], "X", "Po lines mismatch")

    except BalanceNotZero:
        set_sb_status(selfbilling_document["ZSBNR"], "X", "Balance is not zero")

    except PoLineCountMismatch:
        set_sb_status(
            selfbilling_document["ZSBNR"], "X", "More or less PO lines than expected"
        )

    except PoLineCountMismatch:
        set_sb_status(
            selfbilling_document["ZSBNR"],
            "X",
            "Unexpected error, please check via debugger",
        )

    except TooManyGLAccounts:
        set_sb_status(
            selfbilling_document["ZSBNR"], "X", "Too many G/L accounts involved"
        )

    except AttributeError:
        try:
            modal_window = sap.get_element_by_id("wnd[1]/usr")
            text_list = [element.text for element in modal_window.children]
            error_text = " ".join(text_list)
            set_sb_status(selfbilling_document["ZSBNR"], "X", error_text)

        except pywintypes.com_error as com_error:
            set_sb_status(selfbilling_document["ZSBNR"], "X", com_error.args[1])

    except pywintypes.com_error as com_error:
        status_type, message = sap.get_status_bar()
        if status_type is "E":
            set_sb_status(selfbilling_document["ZSBNR"], "X", message)
        else:
            set_sb_status(selfbilling_document["ZSBNR"], "X", com_error.args[1])

    else:
        if status[1] == "Invoice document still contains messages":
            set_sb_status(selfbilling_document["ZSBNR"], "X", status[1])
        elif status[0] != "S":
            set_sb_status(selfbilling_document["ZSBNR"], "X", status[1])
        else:
            # when success!
            set_sb_status(selfbilling_document["ZSBNR"], "S", status[1])

sap.end_transaction()
sap.gui_restore_size()
sap.disconnect()
