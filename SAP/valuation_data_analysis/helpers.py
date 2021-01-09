class DataFileNotFound(Exception):
    pass


def df_update_status(df_row):
    """
    Function used in dataframe's apply call to determine overall status
    of material as outcome of the analysis

    Parameters:
    -----------
    df_row : dataFrame row
   
    """
    yes_no_movement = 'YES -> without movements'
    yes_curr_stock = 'YES -> decrease current stock'
    yes_both_stock = 'YES -> decrease previous stock first!'
    no_open_po = 'NO -> open PO lines exist'
    no_open_pid = 'NO -> open Phys.Inv. lines exist'
    no_open_docs = 'NO -> PO and PID data exists'
    batches_exist = 'IMPOSSIBLE - batches exist'
    deleted = 'NO - Material marked for deletion'
    done = 'MAINTAINED => DONE'

    if df_row['LVORM'] == 'X':
        return deleted

    elif (df_row['BWTTY'] == 'Y'
            and df_row['VALTYPE_N'] == True
            and df_row['VALTYPE_R'] == True):
        return done

    elif df_row['BWTTY'] == 'Y':
        return yes_no_movement

    elif df_row['BATCH_EXIST'] == True:
        return batches_exist

    elif df_row['PO_LINES'] == 0 and df_row['PID_LINES'] == 0:
        if df_row['PREV_STOCK']:
            return yes_both_stock

        elif df_row['CURR_STOCK']:
            return yes_curr_stock

        else:
            return yes_no_movement

    else:
        if df_row['PO_LINES'] > 0 and df_row['PID_LINES'] > 0:
            return no_open_docs

        elif df_row['PO_LINES'] > 0:
            return no_open_po

        else:
            return no_open_pid
