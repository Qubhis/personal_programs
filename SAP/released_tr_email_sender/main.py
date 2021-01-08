# import os

from tr_data import TRData
from ui import TrEmailSender

if __name__ == "__main__":
    # # change working directory to the main.py directory
    # os.chdir(os.path.dirname(__file__))

    # initial Transport request data object
    transport_requests = TRData()
    # call GUI and pass the instance
    tr_email_sender_ui = TrEmailSender(transport_requests)
