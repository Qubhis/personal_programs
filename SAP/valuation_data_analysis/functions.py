import pandas as pd
import sys
import re

from config import (
        SAP_DB_TABLES, FILE_FOLDER_PATH, MATERIAL_NUMBERS_FILE_NAME,
        MULTIPLE_SELECTION_BUTTONS, PLANT_CODE, PREVIOUS_PERIOD, YEAR,
        FINAL_DF_ADDITIONAL_COLUMNS, FINAL_DF_COLUMNS, FINAL_FILE_NAME,
        COLUMN_LABELS)

from helpers import df_update_status, DataFileNotFound

# sys.path.append("LOCAL_FOLDER_PATH_TO_SAPGUIRPA")
from sapguirpa import SapGuiRpa


def _material_multiple_selection_load(**parameters):
    """
    this routine operates on selection screen of SE16N transaction when
    a database table is loaded for selections.
    Nothing to return.

    Parameters
    -------------------
    session : SapGuiRpa
        instance of SapGuiRpa class
    directory : str
        directory path where the file for an input is stored
    materials_file_name : str
        file name of the file containing material numbers
    multiple_selection_btn_id : str
        id of button for multiple selection
    
    """
    # unpack parameters
    sap = parameters["session"]
    directory = parameters["directory"]
    materials_file_name = parameters["materials_file_name"]
    multiple_selection_btn_id = parameters["multiple_selection_btn_id"]
    # material number - multiple selection
    sap.press_or_select(multiple_selection_btn_id)
    # load from txt file button
    sap.press_or_select("wnd[1]/tbar[0]/btn[21]")
    sap.insert_value("wnd[2]/usr/ctxtDY_PATH", directory)
    sap.insert_value("wnd[2]/usr/ctxtDY_FILENAME", materials_file_name)
    # confirm file selection
    sap.press_or_select("wnd[2]/tbar[0]/btn[0]") 
    # info message is given if file doesn't exist
    message_type, status_text = sap.get_status_bar()
    if status_text == "The file does not exist":
        raise DataFileNotFound("File with materials not found in the folder")
    # confirm multiple selection
    sap.press_or_select("wnd[1]/tbar[0]/btn[8]") 


def _export_output_to_excel(sap, table_name):
    """routine for exporting output of SE16N to an excel file

    Parameters
    ----------
    sap : SapGuiRpa
        SaPGuiRpa instance
    table_name : str
        name of database table
    """
    # select export ot spreadsheet from toolbar
    avl_toolbar = sap.get_element_by_id("wnd[0]/usr/cntlRESULT_LIST/shellcont/shell")
    avl_toolbar.pressToolbarContextButton("&MB_EXPORT")
    avl_toolbar.selectContextMenuItem("&XXL")
    # select excel format from dropdown list
    sap.insert_value("wnd[1]/usr/cmbG_LISTBOX", "31")
    # confirm
    sap.press_or_select("wnd[1]/tbar[0]/btn[0]")
    # insert folder path and file name
    sap.insert_value("wnd[1]/usr/ctxtDY_PATH", FILE_FOLDER_PATH)
    sap.insert_value("wnd[1]/usr/ctxtDY_FILENAME", f"{table_name}.xlsx")
    # confirm
    sap.press_or_select("wnd[1]/tbar[0]/btn[0]")
    # overwrite existing file if needed
    message_type, status_text = sap.get_status_bar()
    already_exists = re.search(r"already exists", status_text)
    if already_exists is not None:
        # replace button
        sap.press_or_select("wnd[1]/tbar[0]/btn[11]")


def _maintain_default_settings_for_se16n(sap):
    """loading selection screen variant and maintaining personall settings
    for consistency of GUI scripting

    Parameters
    ----------
    sap : SapGuiRpa
        SaPGuiRpa instance
    """
    # load pre-defined selection screen variant
    sap.press_or_select("wnd[0]/mbar/menu[2]/menu[0]/menu[0]")
    sap.insert_value("wnd[1]/usr/ctxtGS_SE16N_LT-NAME" ,"ValTypeExtend")
    sap.press_or_select("wnd[1]/tbar[0]/btn[0]")
    # change personal settings to see technical names of fields
    sap.press_or_select("wnd[0]/tbar[1]/btn[36]")
    sap.press_or_select("wnd[1]/usr/chkGD-TECH_NAMES", check=True)
    sap.press_or_select("wnd[1]/tbar[0]/btn[0]")


def fetch_db_data_sapguirpa():
    """fetching data using active scripting in SAP GUI (sapguirpa library)
    and stores each database table in separate excel file (as it woul be done
    manually)

    """
    # attach to active SAP session
    sap = SapGuiRpa()
    sap.attach_to_session()
    sap.gui_maximize()

    for table in SAP_DB_TABLES:
        sap.start_transaction("SE16N")
        # insert table name
        sap.insert_value("wnd[0]/usr/ctxtGD-TAB", table)
        # press enter
        sap.send_vkey(0)
        # load pre-defined selection screen variant
        _maintain_default_settings_for_se16n(sap)
        # prepare parameters for multiple selection
        parameters = {
            "session": sap,
            "directory": FILE_FOLDER_PATH,
            "materials_file_name": MATERIAL_NUMBERS_FILE_NAME,
            "multiple_selection_btn_id": MULTIPLE_SELECTION_BUTTONS[table]
        }
        # insert material numbers from the prepared file
        _material_multiple_selection_load(**parameters)

        # different tables have various selection values
        if table == "MBEW":
            # valuation area
            sap.insert_value("wnd[0]/usr/tblSAPLSE16NSELFIELDS_TC/ctxtGS_SELFIELDS-LOW[2,2]", PLANT_CODE)
        
        elif table == "MBEWH":
            sap.insert_value("wnd[0]/usr/tblSAPLSE16NSELFIELDS_TC/ctxtGS_SELFIELDS-LOW[2,2]", PLANT_CODE)
            sap.insert_value("wnd[0]/usr/tblSAPLSE16NSELFIELDS_TC/ctxtGS_SELFIELDS-LOW[2,4]", YEAR)
            sap.insert_value("wnd[0]/usr/tblSAPLSE16NSELFIELDS_TC/ctxtGS_SELFIELDS-LOW[2,5]", PREVIOUS_PERIOD)
        
        elif table == "EKPO":
            sap.insert_value("wnd[0]/usr/tblSAPLSE16NSELFIELDS_TC/ctxtGS_SELFIELDS-LOW[2,10]", PLANT_CODE)

        elif table == "ISEG":
            sap.insert_value("wnd[0]/usr/tblSAPLSE16NSELFIELDS_TC/ctxtGS_SELFIELDS-LOW[2,5]", PLANT_CODE)
        
        elif table == "MCHA":
            sap.insert_value("wnd[0]/usr/tblSAPLSE16NSELFIELDS_TC/ctxtGS_SELFIELDS-LOW[2,2]", PLANT_CODE)
        
        else:
            raise AssertionError(f"selections for table {table} are not defined")

        # execute selections
        sap.press_or_select("wnd[0]/tbar[1]/btn[8]")
        # skip if no values found
        message_type, status_text = sap.get_status_bar()
        if status_text == "No values found":
            continue
        # export to an excel file
        _export_output_to_excel(sap, table)

        sap.end_transaction()
    # detach from sap session
    sap.gui_restore_size()
    sap.disconnect()


def clean_and_analyze_data():
    """processing collected data:
    1. MBEW table (valuation data)
    2. prepares final dataFrame object with additional columns
    3. populate valuation type columns
    4. MBEWH table (previous period valuation data)
    5. EKPO table (Purchasing documents data)
    6. ISEG table (Open physical inventory data) 
    7. MCHA table (material batch stock)
    8. determines final status
    9. export to excel
    """
    # 1. MBEW file load (valuation data)
    mbew_df = pd.read_excel(FILE_FOLDER_PATH + "MBEW.xlsx")
    if mbew_df.empty:
        raise DataFileNotFound("No MBEW file found! Program terminated")

    # 2. start preparing final df with material as the unique key
    final_df = mbew_df[mbew_df["BWTAR"].isnull()].copy()
    # deleted unnecessary column
    final_df.drop(["BWTAR"], axis=1, inplace=True)
    # add new columns
    for column, value in FINAL_DF_ADDITIONAL_COLUMNS.items():
        final_df[column] = value

    final_df = final_df.rename(columns={"LBKUM": "CURR_STOCK"})
    # reorder columns
    final_df = final_df[FINAL_DF_COLUMNS]

    # 3. Populate valutaion type columns
    # get list of all materials having specific val type
    existing_n_val_type = mbew_df[mbew_df['BWTAR'] == 'N']
    materials_n_type = list(existing_n_val_type['MATNR'])
    existing_r_val_type = mbew_df[mbew_df['BWTAR'] == 'R']
    materials_r_type = list(existing_r_val_type['MATNR'])

    if materials_n_type:
        final_df['VALTYPE_N'] = final_df['MATNR'].apply(lambda matnr: True if matnr in materials_n_type else False)

    if materials_r_type:
        final_df['VALTYPE_R'] = final_df['MATNR'].apply(lambda matnr: True if matnr in materials_r_type else False)

    # 4. MBEWH table (previous period valuation data)
    try:
        mbewh_df = pd.read_excel(FILE_FOLDER_PATH + "MBEWH.xlsx")
        # extract only relevant rows and then columns
        mbewh_only_relevant = mbewh_df[(mbewh_df['LBKUM'] > 0) & (mbewh_df['BWTAR'].isnull())].copy()
        mbewh_only_relevant = mbewh_only_relevant[['MATNR', 'LBKUM']].copy()

        mbewh_relevant_list = mbewh_only_relevant.reset_index().values.tolist()
        # fill previous stock to final_df
        if mbewh_relevant_list:
            for [index, matnr, lbkum]  in mbewh_relevant_list:
                final_df.at[final_df['MATNR'] == matnr, 'PREV_STOCK'] = lbkum
    except FileNotFoundError:
        pass
    
    # 5. EKPO table (Purchasing documents data)
    try:
        ekpo_df = pd.read_excel(FILE_FOLDER_PATH + "EKPO.xlsx")
        if not ekpo_df.empty:
            # unique materials and and update document line count
            unique_ekpo_materials = ekpo_df.groupby('MATNR').size()
            for material, count in unique_ekpo_materials.items():
                final_df.at[final_df['MATNR'] == material, 'PO_LINES'] = count
    except FileNotFoundError:
        pass

    # 6. ISEG table (Open physical inventory data) 
    try:
        iseg_df = pd.read_excel(FILE_FOLDER_PATH + "ISEG.xlsx")
        if not iseg_df.empty:
            # remove irelevant lines
            iseg_df = iseg_df[iseg_df['XDIFF'].isnull()].copy()
            iseg_df = iseg_df[iseg_df['XLOEK'].isnull()].copy()
            # unique materials and update document line count
            unique_iseg_materials = iseg_df.groupby('MATNR').size()
            for material, count in unique_iseg_materials.items():
                final_df.at[final_df['MATNR'] == material, 'PID_LINES'] = count
    except FileNotFoundError:
        pass

    # 7. MCHA table (material batch stock)
    try:
        mcha_df = pd.read_excel(FILE_FOLDER_PATH + "MCHA.xlsx")
        if not mcha_df.empty:
            # get unique list of materials
            mcha_materials = mcha_df['MATNR'].unique()
            # update column
            final_df['BATCH_EXIST'] = final_df['MATNR'].apply(lambda matnr: True if matnr in mcha_materials else False)
    except FileNotFoundError:
        pass

    # 8. determine final status
    final_df['CAN_WE_START'] = final_df.apply(df_update_status, axis=1)
    final_df.reset_index(drop=True, inplace=True)
    final_df = final_df.rename(columns=COLUMN_LABELS)

    # 9. export to excel
    final_df.to_excel(FILE_FOLDER_PATH + FINAL_FILE_NAME, sheet_name='Analysis')
