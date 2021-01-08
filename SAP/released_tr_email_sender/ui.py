import json

from tkinter import *
from tkinter import ttk
from tkinter import messagebox

from tr_data import TRData, NO_DATA_MEETS_CRITERIA
from email_text import email_body_template
from helpers import send_email

RECIPIENT = <email_address>
EXCEPTION_FILE = "tr_number_exceptions.json"

class TrEmailSender:

    def __init__(self, transport_requests: TRData):
        self.transport_requests = transport_requests
        self.exceptions = self.load_exceptions()
        # WINDOW CREATION
        self.window = Tk()
        self.window.title("Send email with import requests to TST")
        self.window.config(padx=20, pady=20)
        # TTILE LABEL
        self.title_lbl = Label(
            text="Please select TRs to be included into email: ",
        )
        # BUTTONS
        self.refresh_btn = Button(text="REFRESH", command=self.refresh)
        self.exceptions_btn = Button(text="Add to exceptions", command=self.add_to_exceptions)
        self.select_all_btn = Button(text="Select All", command=self.select_all)
        self.send_btn = Button(text="SEND", command=self.send_email)

        # list of TRs
        columns_labels = {
            'tr_number': ("TR Number", 100),
            'description': ("Description", 350),
            'tkt_type': ("Ticket Type", 80),
            'ticket_num': ("Ticket Number", 80),
            'module': ("SAP Module", 80),
            'export_datetime': ("Export Timestamp", 150),
            'owner': ("Owner", 80)
        }
        # TREE VIEW for list display
        self.tr_tree_view = ttk.Treeview(columns=tuple(columns_labels.keys()), show='headings')
        # Update columns
        for column, (label, field_length) in columns_labels.items():
            self.tr_tree_view.column(column, minwidth=80, width=field_length, anchor='w', stretch=False)
            self.tr_tree_view.heading(column, text=label)
        # insert data
        self.populate_tree_view_lines()

        #LAYOUT PLACEMENT
        self.title_lbl.grid(row=0, column=0, sticky=W)
        self.tr_tree_view.grid(row=1, column=0, rowspan=4)
        self.refresh_btn.grid(row=1, column=1, sticky=N+S+E+W, padx=2, pady=2)
        self.exceptions_btn.grid(row=2, column=1, sticky=E+W+S, padx=1, pady=2)
        self.select_all_btn.grid(row=3, column=1, sticky=E+W+N, padx=1, pady=2)
        self.send_btn.grid(row=4, column=1, sticky=N+S+E+W, padx=1, pady=2)

        # DISPLAY WINDOW
        self.window.mainloop()
    

    def refresh(self):
        # delete all rows in tree view
        for item in self.tr_tree_view.get_children():
            self.tr_tree_view.delete(item)
        # update with new data
        self.transport_requests.refresh()
        self.exceptions = self.load_exceptions()
        self.populate_tree_view_lines()
    

    def populate_tree_view_lines(self):
        all_are_in_exceptions = True

        for (tr_number, export_timestamp, owner, description, ticket_number, sap_module, ticket_type) in self.transport_requests.data:
            # check if not in exception
            if not tr_number in self.exceptions:
                year = export_timestamp[:4]
                month = export_timestamp[4:6]
                day = export_timestamp[6:8]
                time = f"{export_timestamp[8:10]}:{export_timestamp[10:12]}:{export_timestamp[12:]}"
                export_date_time = f"{day}/{month}/{year} - {time}"

                line_values = (tr_number, description, ticket_type, ticket_number, sap_module, export_date_time, owner)
                self.tr_tree_view.insert('', 'end', values=line_values)

                all_are_in_exceptions = False
        
        # if all TRs are in exceptions, insert only pre-defined information
        if all_are_in_exceptions:
            tr_number = NO_DATA_MEETS_CRITERIA[0][0]
            description = NO_DATA_MEETS_CRITERIA[0][3]
            no_data_information = (tr_number, description, "", "", "", "", "")
            self.tr_tree_view.insert('', 'end', values=no_data_information)


    def select_all(self):
        items = self.tr_tree_view.get_children()
        self.tr_tree_view.selection_add(items)


    def get_selected_item_ids(self):
        return self.tr_tree_view.selection()


    def send_email(self):
        # get selected lines
        selected_ids = self.get_selected_item_ids()
        # get data of each id
        if not selected_ids:
            messagebox.showinfo(
                title="Status Info",
                message="There is nothing to send.\n\nPlease refresh the page."
            )
            return None
        
        email_details = self.prepare_email_details(selected_ids)

        # send email
        if send_email(**email_details):
            messagebox.showinfo(
                title="Status Info", message="Email has been sent!")
            # add trs into exceptions
            return self.add_to_exceptions()
        else:
            return None
        

    def prepare_email_details(self, selected_ids):
        transport_data = [self.tr_tree_view.item(id_tag, 'values') for id_tag in selected_ids]
        # prepare list of transports for email body
        html_list_of_trs = ""
        ticket_numbers = set()
        for (tr_number, description, ticket_type, ticket_number, sap_module, export_timestamp, owner) in transport_data:
            html_list_of_trs += f"<li>{tr_number} - {owner} - {description}</li>"
            ticket_numbers.add(ticket_number)
        # prepare email details
        email_details = {
            'recipient': RECIPIENT,
            'subject': f"Transport requests for: {', '.join(sorted(ticket_numbers)).rstrip(', ')}",
            'html_body': email_body_template.format(html_list_of_trs)
        }
        
        return email_details


    def load_exceptions(self):
        try:
            with open(file=EXCEPTION_FILE, mode='r') as file:
                exception_list = set(json.load(file)['tr_numbers'])
        except FileNotFoundError:
            with open(file=EXCEPTION_FILE, mode='w') as file:
                exception_dict = {'tr_numbers': []}
                json.dump(exception_dict, file, indent=4)
            return set()
        else:
            return exception_list


    def add_to_exceptions(self):
        selected_ids = self.get_selected_item_ids()

        if not selected_ids:
            messagebox.showinfo(
                title="Status Info",
                message="Nothing has been selected.\n\nPlease refresh the page."
            )
            return None

        transport_numbers = [self.tr_tree_view.item(id_tag, 'values')[0] for id_tag in selected_ids]
        # add TR number of selected items to exception json file
        for number in transport_numbers:
            self.exceptions.add(number)
        
        updated_data= {'tr_numbers': list(self.exceptions)}

        with open(file=EXCEPTION_FILE, mode='w') as file:
            json.dump(updated_data, file, indent=4)
        
        return self.refresh()
