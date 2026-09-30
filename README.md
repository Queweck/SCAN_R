# SCAN_R
Code in R for calculating JRC's SCAN methodology indicators for measuring trade dependencies and risk on CN8-code product level.

# MAIN GOAL
Main goal of this project was to create a tool for performing automated calculations of all SCAN indicators, measuring trade dependencies for any chosen country on CN8-code product level.

# SCAN Methodology
To learn more about the SCAN methodology I encourage you to read SCAN methodology.pdf provided in this respository. 

Main components of SCAN methodology

Conetration Indexes:
1. Top source - country that is our country's top source of a given product
2. Max Market share - share of total import of a given product that is imported from the top source
3. Hernfindal-Hirshamann Index - measure of import concetration
Substitutability Indexes:
4. Import/Export Ratio
5. Exposure Index - measures what share of total domestic demand for a product is fullfiled by import.

# DATA description
To calculate all SCAN indicators, COMEXT and PRODCOM data for international trade and production on own account is required. 
**COMEXT** data is loaded using API, which is done automaticaly by the code. 
**PRODCOM** data must be downloaded **manually** from Eurostat database. 

**Input files:**
1. JRC kody.xlsx - file containing list of chosen products that you want to conduct analysis on. It must provide their CN8 codes, PRODCOM codes and description. Original file contains list of 127 products in CN8 from JRC list of products from semiconductor sector. However, this file can be changed complitely.
2. PRODCOM.xlsx - file containing total production on own account of all PRODCOM products. It can be downloaded from Eurostat database.

# CODE description
Code is simple:
1. required libraries and packages downloading
2. input data loading
3. downloading COMEXT data using API
4. downloading monthly COMEXT data using API (for change-in-time analysis)
5. calculating indicators using functions (you can adjust them for your work)
6. merging all dataframes into one table
7. saving table in csv format

Output data is in csv format. If you want to prepare a userfriendly dashboard you can use conditional formating in excel or use R/Python for further work.
