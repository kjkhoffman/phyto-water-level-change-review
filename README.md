# phyto-water-level-change-review

Code and data supplements for Hoffman et al., Decreases in lake and reservoir water levels are associated with increased phytoplankton and cyanobacteria: a systematic literature review.

## Files:

There are five files associated with this code and data supplement.

**sys_review_extracted_data.csv** - extracted data from each of the studies in the literature review. This data has not gone through the qaqc scripts that are also included in this supplement.

**systematic_review.qmd** - a quarto document that guides the user through each of the QAQC steps and runs each of the QAQC and data analysis functions. This .qmd also creates each of the figures in the manuscript and supplement.

**1_qaqc_data_sysreview.R** - this function QAQCs all of the data in the sys_review_extracteddata.csv. The primary functions of this script are to exclude any sites that were erroneously extracted, to de-duplicate any sites/data that were included in this review in multiple studies, and to fix assorted typos and mistakes in the raw extracted data. This script is located in the Scripts folder. 

**2_extract_drivers.R** - this function creates a binary categorization for each of the extracted measured water quality variables, which of those variables responded to water level change, and which of those variables were identified as drivers of phytoplankton and/or cyanobacteria. This script is located in the Scripts folder. 

**3_proportion_WL.R** - this function calculates the proportion water level change (depth) for all studies with available water level data (maximum observed depth or maximum depth & maximum increase or decrease in water level). This script is located in the Scripts folder. 

## User instructions: 

To QAQC the data and to create all of the figures associated with this manuscript, users can open and run the code in the quarto document. To see exactly what occurs in each of the 3 functions, please see the script associated with each function (scripts 1-3).

Running the .qmd will create two new folders, one for manuscript figures and one for supplemental figures. 
