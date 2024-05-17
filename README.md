# fsdkenya-credit
Examining the market for credit in Kenya with FinAccess survey data

**Description**

This repository contains all of the data and code for the set of analyses and figures of the Kenyan credit market presented here.

**Data and Code Availability Statement**

The primary data sets used in this analysis are available publicly from the Kenya National Bureau of Statistics (KNBS), and can be found [here](https://finaccess.knbs.or.ke/reports-and-datasets). 

**Computational requirements**

This analysis was conducted using R/version 4.3.2 (2023-03-31) and RStudio/version 2023.03.0+386 (2023.03.0+386). 

R version 4.3.2 (2023-10-31)
Platform: x86_64-apple-darwin20 (64-bit)
Running under: macOS Sonoma 14.2.1

**Instructions for Data Preparation and Analysis**

The full workflow for data downloading, cleaning, analysis and visuazliation of results is set out in the file: *fsdkenya_credit.qmd*. This is a quarto file which when run, generates as output the *fsdkenya_credit.md* and *fsdkenya_credit.html* files. The individual functions which perform the data downloading, data cleaning, data analysis and data visualization tasks are in the R subfolder. 

**Outputs**

All outputs used for the paper can be found in the files generated automatically when running the main .qmd file in */fsdkenya_credit_files/figure-html*
