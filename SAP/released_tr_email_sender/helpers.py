import re

def regex(pattern: str, text: str) -> str:
    match = re.search(pattern, text)
    if match:
        return match.group().strip('_')
    else:
        return 'None'

def regex_ticket_number(text: str) -> str:
    pattern = r"_CHG\d{7}_"
    return regex(pattern, text)


def regex_module(text: str) -> str:
    pattern = r"_[A-Z]{2}_"
    return regex(pattern, text)

def regex_ticket_type(text: str) -> str:
    pattern = r"^[A-Z]{2}_"
    return regex(pattern, text)

def send_email(**kwargs):
    '''this function sends email to predefined recipient
    more specifically, this is part of my job automation, and it sends
    request to transport my transport requests from development to test system'''
    import win32com.client as win32

    outlook = win32.Dispatch('Outlook.Application')
    mail = outlook.CreateItem(0)
    mail.To = kwargs['recipient']
    mail.Subject = kwargs['subject']
    mail.HTMLBody = kwargs['html_body']
    mail.Send()

    return True
    
