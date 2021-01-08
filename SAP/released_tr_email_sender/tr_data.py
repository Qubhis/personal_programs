import pandas as pd

from helpers import regex_ticket_number, regex_module, regex_ticket_type
from remote_call import z_get_released_transports

AGE_IN_DAYS = 1

NO_DATA_MEETS_CRITERIA = [["No data selected", "", "", f"Either TR was released earlier than {AGE_IN_DAYS} day(s) ago or is in exceptions", "", "", ""]]
CONFIG_FILE_IS_MISSING = [["Config file", "", "", "Please check the file is mainainted", "", "", ""]]

class TRData:

    def __init__(self):
        self.data = self.get_data()

    def get_data(self):
        rfc_data = z_get_released_transports(AGE_IN_DAYS)
        if not rfc_data: # empty dict
            return NO_DATA_MEETS_CRITERIA
        elif rfc_data is None:
            return CONFIG_FILE_IS_MISSING
        # process data and get ticket and module identification
        transports_df = pd.DataFrame.from_records(rfc_data)
        # add ticket number if found
        transports_df['TICKET_NUM'] = transports_df['DESCRIPTION'].apply(regex_ticket_number)
        # add module identification
        transports_df['MODULE'] = transports_df['DESCRIPTION'].apply(regex_module)
        # add ticket type
        transports_df['TKT_TYPE'] = transports_df['DESCRIPTION'].apply(regex_ticket_type)
        # export as a list
        return transports_df.values.tolist()

    def refresh(self):
        self.data = self.get_data()

    
