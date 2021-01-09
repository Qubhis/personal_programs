This script was developed for analyzing valuation data of material master. 

The requirement was to make material master records split valuated. There are several prerequisites for changing valuation data of material master record
and doing such analysis took quite long time, not talking that the analysis is mostly done several times prior to change of mateiral master recorsd.
Therefore this tedeious task was automated and this script has saved me hours of time already.

What does this script do:
  - Using SAP GUI scripting to export data from several database tables to excel file - code from my other repo [SapGuiRpa](https://github.com/Qubhis/sapguirpa)
  - Cleansing data and performing necessary analysis of data using Pandas library
