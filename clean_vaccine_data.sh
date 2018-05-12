#!/usr/bin/env bash

# This script is needed since LibreOffice doesn't support the
# spreadsheetML format that the CDC supplies the data in.

# To get the ssconvert utility, do:
# sudo apt-get install Gnumeric

ssconvert --import-encoding Gnumeric_Excel:excel_xml data/dataView2023_17.xls data/MMR.xlsx


ssconvert --import-encoding Gnumeric_Excel:excel_xml data/dataView2020_17.xls data/HepA.xlsx

ssconvert --import-encoding Gnumeric_Excel:excel_xml data/dataView2019_17.xls data/DTaP.xlsx

