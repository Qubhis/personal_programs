# Invoice entry for custom made self-billing document

Whole case is about entering an invoice with total amount equalled to zero which is not possible via available BAPI and can be done only manually. The solution was designed according to customer’s requirement and consists of fetching and processing data in SAP (ABAP), HTTP requests, and SAP GUI Scripting.
This is my personal and working solution, but it had been developed as proof of concept before internal RPA team developed software robot in UiPath as a part of final solution. This solution uses remote function calls instead of HTTP request (no specific reason)

Please note that I'm using my personal wrapper around SAP GUI scripting API which you can find [here](https://github.com/Qubhis/sapguirpa)

Process flow:
  1.	ABAP – fetch data from database, process them, and store them in JSON format.
  2.	Python – Gets data, parses them, and creates Incoming Invoice via transaction MIRO
  
              - switch to correct company code
              - insert mandatory header fields
              - selects concerned Purchase order numbers
              - iterates over table control, identifies correct purchase order lines and inserts values from the received data
              - then selects only relevant lines
              - checks if amount is zero
              - checks that only GR/IR G/L account is presented via simulation
              - posts the transaction and sends created invoice receipt number back to SAP
  3.	ABAP – receives information about success or fails from step 2 and updated necessary tables accordingly.



