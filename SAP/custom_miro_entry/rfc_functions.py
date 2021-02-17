import os
import sys
import json
import win32com.client
import PySimpleGUI as sg
from pyrfc import Connection

from dotenv import load_dotenv

ENV_PATH = "Path to locally stored env file"


def get_sb_data_for_rpa():

    load_dotenv(dotenv_path=ENV_PATH)

    connection_attributes = {
        "user": os.getenv("USER"),
        "passwd": os.getenv("PASSWORD"),
        "ashost": os.getenv("ASHOST"),
        "sysnr": os.getenv("SYSNR"),
        "client": os.getenv("CLIENT"),
    }

    def number_to_string(number):
        """helper for json.loads()"""
        return str(number)

    with Connection(**connection_attributes) as conn:
        selfbilling_json_data = conn.call("ZWS_GET_SB_DATA_FOR_RPA")
        list_format = json.loads(
            selfbilling_json_data["S_JSON"],
            parse_float=number_to_string,
            parse_int=number_to_string,
        )

        return list_format


def set_sb_status(sb_doc_number, processing_status, message):

    connection_attributes = {
        "user": os.getenv("USER"),
        "passwd": os.getenv("PASSWORD"),
        "ashost": os.getenv("ASHOST"),
        "sysnr": os.getenv("SYSNR"),
        "client": os.getenv("CLIENT"),
    }

    with Connection(**connection_attributes) as conn:
        conn.call(
            "ZWS_PROCESS_SB_STATUS",
            SB_DOC_NUMBER=sb_doc_number,
            PROCESSING_STATUS=processing_status,
            MESSAGE=message,
        )


if __name__ == "__main__":
    pass