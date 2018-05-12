https://noahrjohnson.shinyapps.io/Vaccinations

Vaccine data from the CDC. Excel SpreadsheetML files were downloaded from the following urls.

DTaP Data: https://www.cdc.gov/vaccines/imz-managers/coverage/childvaxview/data-reports/dtap/trend/index.html

MMR Data: https://www.cdc.gov/vaccines/imz-managers/coverage/childvaxview/data-reports/mmr/trend/index.html

Hep A Data: https://www.cdc.gov/vaccines/imz-managers/coverage/childvaxview/data-reports/hepa/trend/index.html

After moving the data files to the data folder, the clean_vaccine_data.sh script can be used to convert the files into the more common .xlsx format (requires Gnumeric). The clean_vaccine_data.R script will then produce clean data frames for visualization use.

