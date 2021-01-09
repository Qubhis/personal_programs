from datetime import date

today = date.today().strftime("%d-%B-%Y")

FILE_FOLDER_PATH = "<YOUR FOLDER>"
MATERIAL_NUMBERS_FILE_NAME = "<TXT FILE WITH MATERIAL NUMBERS>"
FINAL_FILE_NAME = f"analysis_output_{today}.xlsx"

PLANT_CODE = "PL10"
PREVIOUS_PERIOD = "12"
YEAR = "2020"

SAP_DB_TABLES = ["MBEW", "MBEWH", "EKPO", "ISEG", "MCHA"]

MULTIPLE_SELECTION_BUTTONS = {
    "MBEW": "wnd[0]/usr/tblSAPLSE16NSELFIELDS_TC/btnPUSH[4,1]",
    "MBEWH": "wnd[0]/usr/tblSAPLSE16NSELFIELDS_TC/btnPUSH[4,1]",
    "EKPO": "wnd[0]/usr/tblSAPLSE16NSELFIELDS_TC/btnPUSH[4,7]",
    "ISEG": "wnd[0]/usr/tblSAPLSE16NSELFIELDS_TC/btnPUSH[4,4]",
    "MCHA": "wnd[0]/usr/tblSAPLSE16NSELFIELDS_TC/btnPUSH[4,1]",
}

FINAL_DF_ADDITIONAL_COLUMNS = {
    "VALTYPE_N": False,
    "VALTYPE_R": False,
    "PO_LINES": 0,
    "PID_LINES": 0,
    "PREV_STOCK": 0,
    "CAN_WE_START": False,
    "BATCH_EXIST": False,
}

FINAL_DF_COLUMNS = [
    "MATNR",
    "BWKEY",
    "LVORM",
    "BWTTY",
    "VALTYPE_N",
    "VALTYPE_R",
    "PO_LINES",
    "PID_LINES",
    "CURR_STOCK",
    "PREV_STOCK",
    "CAN_WE_START",
    "SALK3",
    "BATCH_EXIST"
]

COLUMN_LABELS = {
    "MATNR": "Material",
    "BWKEY": "Plant",
    "LVORM": "Marked for deletion",
    "BWTTY": "Valuation category",
    "VALTYPE_N": "Val. type 'N'",
    "VALTYPE_R": "Val. type 'R'",
    "PO_LINES": "PO lines count",
    "PID_LINES": "Phys. inv. lines count",
    "CURR_STOCK": "Stock in current period",
    "PREV_STOCK": "Stock in previous period",
    "CAN_WE_START": "Status of the material",
    "SALK3": "Current stock value",
    "BATCH_EXIST": "Batches exists"
}
