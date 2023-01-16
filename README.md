# cleaning_template
Hello!

In case you made it here, you are currently reading a Read Me made by Me 😀. This repository contains the structure and the scripts neededto clean any raw kobo data from scratch and creating final outputs.

This process requires knowledge in R and the REACH Standard Checklist for Quantitative Cleaning, so I will try my best to showcase a step-by-step here to managing all of this data. 

## Some details

- These scripts were built by the amaying team in REACH Syria. 

- The team in Out of Ukraine REACH Regional carried this legacy and optomized all the processes and did major updates to the BackEnd (Utility Repo).

### Outputs

- Final Data (anonymized or non-anonymized)
- In case of working on an assessment that is continuous, a data log can be maintained. 
- Cleaning logs, Deletion Logs, and Enumerator Performance

## Process - Step-by-Step

So! You would like to clean some Data, here you go:😁

### Setup

Get all the necessary [resources](resources/readme.txt)
and R files (including the __utility_repo__ in folder *src/utils*)

### data exports

- Obtain the latest raw data and audit files (if available) and place them in data/inputs/kobo_export and data/inputs/audit respectively

After this, you should be able to run this line in 1_cleaning.R

````source("src/load_Data.R")````

### The actual cleaning


this section will end up with getting a new cleaned data

#### 0: initial cleaning: drop and rename some columns (including any changes in tool or data)

#### 1: remove no-consents and duplicated uuids

#### 2: audit checks: survey durations and "initial" duplicated pcodes (also spatial checks if needed)

Survey durations: standard process. All interviews under ## minutes will be flagged for review. A file will be created in directory **output/checking/audit/** which you can send to the AO (but most of the time there is no reason to remove any submissions because they were too quick)

Soft Duplicates: standard process. All interview with ## similar entries will be flagged for review. There will be another file created in **output/checking/audit/** which you can send to AO, but most of the time you can review these yourself.

#### 3: Creating requests: Others, Translations

Others - Standard process: All select_one & select_multiple questions that includes an (Other please specify) option will be translated (if needed) and checked by AO for recoding possibilities. A file will be created in directory **output/checking/requests/** which you can send and teach AO how to fill it. 

Translation requests - if available Same process as above but for open-ended questions. 

#### 4: recoding others basing on responses from AO

#### 5: logical checks: direct cleaning & followups

#### 6: outliers

#### 7: final changes and recleaning

#### 8: outputting the cleaned data or data log (if available)

Here you are finally. 😇 

But one more step !😅

You can run the *package4validation.R* to prepare the folder to send to HQ for validation.

Now we are done. 
