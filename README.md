# phyto-water-level-change-review

Code and data supplements for a manuscript by Hoffman et al. on "Declining water levels are associated with increasing phytoplankton and cyanobacteria in a global set of lakes and reservoirs: a systematic literature review."

## Files:

There are six files associated with this code and data supplement.

**systematic_review.qmd** - an overview quarto document that guides the user through each of the QAQC steps and runs each of the QAQC and data analysis functions. This .qmd also creates each of the figures in the manuscript and supplement.

**1_qaqc_data_sysreview.R** - this function QAQCs all of the data in the sys_review_extracted_data.csv file. The primary functions of this script are to exclude any sites that were erroneously extracted, to de-duplicate any sites/data that were included in this review in multiple studies, and to fix assorted typos and mistakes in the raw extracted data. This script is located in the Scripts folder.

**2_extract_drivers.R** - this function creates a binary categorization for each of the extracted measured water quality variables, which of those variables responded to water level change, and which of those variables were identified as drivers of phytoplankton and/or cyanobacteria. This script is located in the Scripts folder.

**3_proportion_WL.R** - this function calculates the proportion water level change (depth) for all studies with available water level data. This script is located in the Scripts folder.

**sys_review_extracted_data_wUSGSnames.csv** - extracted data from each of the studies in the literature review. These data have not gone through the qaqc scripts that are also included in this supplement. This file is in Data folder.

**sys_review_extracted_data_metadata.csv -** Metadata for sys_review_extracted_data.csv. This file associates the column names from sys_review_extracted_data.csv with variable names and the full statement/question used for data extraction in the extraction template. This file also categorizes the data type for each column. For categorical variables, a list of the explicitly extracted categories is provided. For any categorical variable that includes "other" as a response category, an optional text box was given in the extraction template. As such, categorical variables may include more text responses than what is included in this column. This file includes a column for "Preferred units". These are the units that numeric values in the associated columns should have. The script "**1_qaqc_data_sysreview.R"** ensures that preferred units are used. The raw data may have some values that are not in preferred units.

## User instructions:

To QAQC the data and to create all of the figures associated with this manuscript, users can open and run the code in systematic_review.qmd. To understand exactly the steps in each of the three functions, please see the script associated with each function (scripts 1-3).

Running the .qmd will create two new folders, one for manuscript figures and one for supplemental figures.
