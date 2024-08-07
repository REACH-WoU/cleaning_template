# utilityR general use and the cleaning process.

This document is based on the standard cleaning procedure of the utilityR package. Please check the documentation of the package for more details

## Table of Contents
- [Open up the cleaning template](#Open-up-the-cleaning-template)
- [Duplicates and no-consents](#Cleaning-duplicates-and-no-consent-entries)
- [Audit checks and soft duplicates](#Audit-checks-and-soft-duplicates)
- [Geospatial checks](#Geospatial-checks)
  - [Fake accuracy check](#Fake-accuracy-check)
  - [Indicated point within polygon check](#Indicated-point-within-polygon-check)
  - [Audit GPS points within polygon check](#Audit-GPS-points-within-polygon-check)
- [Other requests and translations](#Other-requests-and-translations)
  - [Recoding other responses](#The-other-entry-workflow)
  - [Recoding translations](#The-translation-entry-workflow)
  - [Applying changes](#Applying-recode-changes)
  - [recode.relevancy framework](#recode.relevancy-framework)
  - [Recoding translation requests](#Recoding-translation-requests)
  - [Non-english check](#Non-English-check)
  - [Consistency check](#Consistency-check)
  - [Binary check](#Binary-check)
- [999 checks](#999-checks)
- [Logic checks](#Logic-checks)
- [Checks for outliers](#Checks-for-outliers)
- [Finalize the data](#Finalize-the-data)
- [Contributors](#Contributors)

### Open up the cleaning template

1. Load the package by calling `library(utilityR)`. Create a new directory for your cleaning by clicking `File`->`New file`->`R Markdown`->`From Template`->`cleaning template`. Choose the name and the directory of your cleaning folder and create it by clicking `Ok`.
2. Load the kobo tool into the 'resources' folder. Load the raw data into the 'data\inputs\kobo_export' folder and the audit files into 'data\inputs\audit' folder if you have them.
3. Start the `1_cleaning.Rmd` file.

### Initialization

Prior to running anything fill up the `directory_dictionary` list with the relevant names. Load the API key file and run the `init.R` and `load_Data.R` files. If loading the data from the kobo server directly, specify so in the `Base section` block. You'll need your API key and the uid of your project. The script will load the data and the tool automatically and place it in the appropriate folders (the paths you indicate in the directory dictionary).

The raw data are saved in the `kobo.raw.main` and `kobo.raw.loopx` files (x is the number of your loop e.g. kobo.raw.loop1) that are later duplicated into `raw.main` and `raw.loopx` files. This is convenient for cases where you want to re-run your cleaning procedure from scratch and don't want to waste time on reading your dataframes again. Just run the rows that create `raw.main` and `raw.loopx` objects and you'll be set.

If you need to modify your kobo tool in any way, please do so within the `src/sections/tool_modification.R` file. This is reserved for the cases where the tool was changed in the middle of data collection.

### Cleaning duplicates and no-consent entries

All entries marked for deletion are kept in the `deletion.log.new` object - this is our deletion log.

The initial step that the script goes through is the removal of duplicates. The algorithm for finding them in the `raw.main` is standard - finding the duplicate `uuid` entries and dropping them. We cannot do this for the `raw.loopx` objects as their unique identifier `loop_index` works as a row index. We will never be able to find any duplicates by investigating this column. However, each individual loop entry is assigned to some entry from the `raw.main` object through the `uuid` and the `parent_index` columns. Each set of household members should have 1 unique `uuid` and `parent_index`. The script finds those entries that have more than one `parent_index` per a unique `uuid` and marks them as duplicates. 

At the end of this process, those entries are added into the `deletion.log.dupl` object.

***Improtant***

**`deletion.log.new` and `deletion.log.dupl` objects are kept separate until we have added everything we need into the `deletion.log.new` object and deleted all of the needed entries from our dataset. These objecs are merged only prior to writing the deletion.log excel file after Section 3 of the cleaning script**

This is done because most other checks that we run check for the general invalidity of the survey and delete entries if they match the `uuid`. If we were to merge the two objects together, the script would delete all the entries that match the duplicate `uuid` index while ignoring the fact that:
1. While there may be two of these entries in the data, that doesn't mean that it's not a valid entry, we just don't need two of them
2. The deletion of entries by `uuid` within the loop will cause the deletion of all loop entries that match this `uuid`, even if they're not duplicated.  

**No consent entries**

The next bit of the script checks the **No consent** entries in the data and requires the user's input. The user has to define the condition that classifies an entry as a no-consent entry and filter the dataframe by it, thus creating the `no_consents` data.frame object.

The last bit of this script is trying to find the test entries by parsing the enumerator's comments column that you've specified in the `directory_dictionary$enum_comments` element of the list to find entries that say 'test' in Ukrainian, or English.

If you want to add any other checks for general validity of the data, you can add them here and merge the resulting cleaning log files into the `deletion.log.new` object.

### Audit checks and soft duplicates

**Audit checks** 
Prior to running the script you'll have to specify the minimum and maximum time that the respondent can spend answering the questions. All of the interviews that are above/below these thresholds will be marked as suspicious. Additionally, some enumerators can spend too much time on a single question (consent, location, etc.) to make the interview seem longer than it actually was. You can smooth these interview times by passing the `pre_process_audit_files = T` argument and setting `max_length_answer_1_question` parameter. This will make the script run the `pre.process.audits` function, that will replace these long times with the sample average time for answering the given question, without the outliers. Those uuid-question pairs that had their time values replaced will be tagged in the `tag` column so that the user knows that something was wrong in the interview-question pair. This is a best practice, so we encourage the users to do so.

The analysis of audits will create a `audits_summary` excel file in the `directory_dictionary$dir.audits.check` directory. This file is your survey data + audit check columns such as:  
`n.iteration` - The number of iterations per interviews (the number of times the user had to stop and then continue the interview)  
`tot.t` - Total time of the interview. Calculated as `start` of the last `form.exit` event minus the `start` of `form.start` event  
`tot.rt` - The total response time of the interview. The sum of the `duration` column in the loaded audits dataframe  
`tot.rt.inter` - The total time between questions in the interview. The sum of the `inter_q_duration` column in the loaded audits dataframe  
`t` - Time of each iteration. Calculated as `start` of the iteration's `form.exit` event minus the `start` of the iterations`form.start` event  
`rt` - Response time of each iteration. The sum of the `duration` column in the loaded audits dataframe for the iteration  
`q` - Number of questions per iteration  
`j` - Number of jump events per iteration  
`e` - Number of edits per iteration Calculated as the number of non NA entries in the `old.value` column  
`w` - Waiting time - the `start` column of iteration's `form.resume`event - the `start`  for the column of the pervious iterations `form.exit` event  
`tag` - If you've pre-processed files, this column will tag the uuid-question pairs that were outside of the set duration threshold  
As well as `NA`, `DK`, and `_other` (open text answer) columns.  

**All of the suspicious surveys will be written into the `survey_durations` file.**
After the script is done analysing these things, you can browse the `audits_summary` excel file. If you decide to keep an entry despite it being in this file, delete the relevant excel row. Everyting within this file will be deleted when you run the `section_2_run_audit_decisions.R`.

**Soft Duplicates**

The only entry needed from the user for this bit of the script is `min_num_diff_questions` parameter, that is the minimum number of different columns that makes us confident that the entry is not a soft duplicate. The soft duplicate algorithm is based on the Gower distance parameter calculated for subsets of each individual enumerator.

This analysis produces 4 outputs: 
1. `soft.duplicates` excel - the dataframe that contains entries with most similar surveys per enumerator.
2. `soft_duplicates_analysis` excel - the summary file with statistics for soft duplicates per enumerator.
3. `soft_duplicates_outliers` excel - outlier enumerators that have the most soft duplicates.
4. `enumerators_surveys` pdf - a visualisation of the enumerators with outlier values in terms of similarity of surveys.

Once again, if you're fine with some of these duplicates, remove them from the `soft_duplicates` excel file in the `directory_dictionary$dir.audits.check` directory. Everything that is left in the excel will be deleted when you run the `section_2_run_audit_decisions.R`.

Once you've looked through the excel files, double-checked everything and left only those entries that you'd like to delete in audit and soft duplicate files, run the `section_2_run_audit_decisions.R` line in the cleaning script.

The only bit of manual entry that needs to be done when running this file is filling the `ids_incompl` object. It's supposed to host the uuids of those surveys that are incomplete. If you don't have any such surveys, you can leave it blank.

### Geospatial checks

#### Fake accuracy check
The spatial checks section checks for interviews with 0 geo coordinate precision. If these are present in the data, this may mean that the interviewer has installed a fake gps app onto their phone and has used it to fake the interview.  
#### Indicated point within polygon check  
If you've conducted polygon sampling and didn't include the check for wheter the collected datapoint lies within the polyon where it's supposed to be colleced you'll need to specify the following:
- `geo_column` - the name of the column that holds your coordinates (in the data)
- `polygon_file` - the path to the your `.json` polygon file. This should be a file that holds the polygons of your sampling unit (settlements, hromadas or custom polygons, depending on what you need)
- `polygon_file_merge_column` - The name of the column that serves as a unique identifier of your polygon in your `polygon_file` (pcode or the polygon_id)
- `merge_column` - The name of the column that serves as a unique identifier of your polygon in your `raw.main` object (pcode or the polygon_id)  

The script will check if the selected columns are present in the data objects and try to fit the sampled points into the polygons. If any of the interviews were conducted outside of their respective polygons, the algorithm will write the `gps_checks.xlsx` file. The poing can be classified as the following:
- `Outside polygon` - the point wasn't matched to any of the polygons in your  `polygon_file`
- `Wrong polygon` - the point was matched to a polygon in the `polygon_file` but it's not the polygon where the enumerator said the interview was being conducted.  
#### Audit GPS points within polygon check  
After this check is done, the deletion log is written into a `geospatial_check` and `gps_checks` excel files. Look trough them and remove those uuids that you'd like to keep in the data. 


If you want to run **audit** GIS check, you will have to specify the following:
- `use_audit` - whether you want to run the audit GPS check. Set to `TRUE` if using, set to `FALSE` if not.
- `top_allowed_speed` - top speed that the interviewer is allowed to move during the interview in kp/h
- `initial_question` - the question that you consider the start of the interview within audits - at which point in the interview we can be sure that the enumerator has started the interview?
- `final_question` - the question that you consider the end of the interview within audits - at which point in the interview we can be sure that the enumerator has ended the interview?
- `omit_locations` - are there any locations with volatile GPS tracking you'd like to remove from this analysis?
- `location_column` - what is the column in your data that signifies the locations you'd like to omit? It Doesn't have to match the `merge_column` if you're omitting other (larger) geo levels. Leave blank if not using.
- `location_ids` - the IDs of `location_column` you'd like to omit. Leave blank if not using.  
**For this check to work you also have to specify the `polygon_file`, `polygon_file_merge_column` and `merge_column` from the Indicated point within polygon check section**

If you have included collection of GPS coordinates into the audit files you can conduct an audit GIS check process. This process will process the audit data and collect the unique GPS points within the interview bounds (see above for details). Cases where there were no coordinates within the selected interview bounds will be separated from the rest of the check and written into a separate excel sheet.
The GPS accuracy will be used to estimate cases where the accuracy of the coordinate is too low, cases outside of 3 standard deviations of the median will be dropped.   
The time and distance between the start of the new question with a unique GPS coordinate and the end of the previous question with another GPS coordinate will be added into the dataframe. Cases where the time difference is effectively 0 or where the distance is lower than the accuracy will be set to NA. Distance will be converted to kilometers and time will be converted to hours. After this is done, speed is calculated. Cases where speed is higher than 300 km per hour will be removed as they're considered to be errors. In cases where the movement speed is higher than expected maximum speed of an in-person interview will be flagged.  
The last check is concerned with checking whether all unique GPS points in the audit are attributed to the correct polygon. To conduct this, the user will need to specify the same variables as mentioned in the **Indicated point within polygon check** section. The mechanism of the check is basically the same. As a result this check forms an `audit_checks_full` Excel file. It contains the following sheets:  
- `General table` table that contains all valid GPS data in the audit files,
- `Audit issues summary` table that contains the cases where it wasn't possible to gather geospatial data from the interview.
- `Speed issues` table that contains all cases where the movement speed was higher than expected at least one time during the interview.
- `Speed issues summary` - table with average movement speed of each interview in `Speed issues` table.
- `Location issues` - table with all interviews where the location of the interview was outside of the expected polygon at least once.
- `Location issues summary` - a table that shows how many unique points were gathered in the interview and how many of them were collected in the wrong location. Also shows the expected and actual location of the interview.  
Look through all of the tables and work with `summary` sheets. If you think that the interview should be deleted **keep** it in its `summary` sheet. If not, delete the row. The sheets that don't have the `summary` in the name are more for your data exploration needs, feel free to change them as needed.


After you're done, run the `section_3_spatial_decisions.R` and we're done with the deletion bit of the cleaning.

###  Other requests and translations

This section is the most hands-on part of this script. It is also the most complex one, so please take your time running it and be vary of any bugs, errors and warning that you may get. Please go into the scripts themselves when running them instead of just sourcing them.

`section_4_create_other_requests_files.R` is the bit of the script that gathers all of the `text` columns from your kobo questionnaire and translates them. It creates two files each having a different procedure applied to it. One file is dedicated to the `_other` requests the other one works with the open-ended questions.  

**NOTE**  
Each of the abovementioned files is generated by their respective functions in the `get` family - `get.other.db` and `get.trans.db`. If one of the functions didn't find the respective entries (meaning that you didn't follow the bese practices when designing the Kobo tool). You can run `get.text.db` function that will get you all of the text questions in tour kobo form. From that point you can transfer the ommited columns to either `other.db` or `trans.db` objects prior to running `find.responses` functions that follow them.

Prior to running this script please specify the following:
- `partial_clean` - if the user was running translations during the data collection and there are a few `_other` files in the directory, specify this parameters as `TRUE`, this will result in already translated `uuid`s to not be re-translated.
- `other_request_file_pattern` specify the name pattern of your `other` requests files. Later this is used to get the pattern of all of your `_other` request files in `name_clean_others_file`
- `text_request_file_pattern` specify the name pattern of your translation requests files. Later this is used to get the pattern of all of your `_other` request files in `name_clean_trans_file`

#### The other entry workflow. 
The first type of a file that this script produces are the `other_requests_final` file. To produce the list of `text` questions that have `_other` response options the script uses the `get.other.db` function. This functions relies on the fact that in our data these questions have the `_other` suffix and have only one relevancy - their `ref.name` column in the following form - `selected(${ref.name}, 'other')`.  
**If there are multiple relevancies for a given `_other` column or if the text column doesn't have the `_other` suffix, the variable may be ommited from the analysis.**  
This output file has the following structure

| uuid | loop_index| name  | ref.name| full.label| ref.type  | choices.label | choices | response.uk | response.en| true| existing| invalid  | true_elsewhere| true_column| true_column_parent|
|------|-----------| ------|---------|-----------| ----------|---------------|---------|-------------|------------|-----|---------| ---------| ------------- |------------| -----------------------------|
| ID   | loop_ID  | variable_name_other  | variable_name| variable label|`select_one` or `select_multiple` | the labels of all available choices| respondent's choices | the response in Ukrainian/Russian| The translated response in English | Whether the `_other` response is appropriate| Whether the `_other` response already exists within the `choices.label` column | Whether the response is invalid | If the response is appropriate but answers another question| The `name` of the `_other` question that it answers | The `ref.name` column for `true_column`|

After the file is created, the user's task is to open the excel file and look through the `response.en` column to see if the translation and the answer itself is appropriate.

**The regular cases**  
Most of the time the user will be engaging with `true`, `existing` and `invalid` columns. 
- If the translation is good and the answer is appropriate to what was asked in the question stored in `full.label` column, put the correct translation into the `true` column.
- If the answer that the user has given is already present in the `choices.label` column (meaning that the user didn't understand that such option was already available), fill the `existing` column by pasting the exact appropriate option from the `choices.label` column. If you're working with a `select_multiple` question, and the answer is appropriate for a few of the options in the `choices.label` you can add a few of them if you separate them with a semicolon - `;`. Be careful when filling this column and double-check the `choices` column in your excel file. Sometimes the option that you want to put into the `existing` column has already be chosen by the user. This won't break the script but you will get a warning.
- If the answer is invalid - as in, it's not related to the question that is being asked, type `YES` into the `invalid` column.

**The elsewhere cases**  
The elsewhere case is reserved for occurences when the `response.en` is inappropriate for the question asked in the `full.label` but it can be appropriate for some other question in the survey and you want to transfer that response into a new column. If you want to do this you have to ensure the following:
1. The `invalid` column is filled with `YES` for this row.
2. You've inserted the correct translation into the `true_elsewhere`
3. You've inserted the correct `_other` column into the `true_column`
4. You've inserted the correct parent column into the `true_column_parent`

When you're done with this, you can save the excel file and move on to the translation requests.

#### The translation entry workflow. 

Prior to running the translation of the `text` responses, the user needs to specify two parameters:
- `trans_cols_to_skip` - a vector list of columns that need to be omitted from the process and the translations. These may be columns of enumerator comments, names of locations of the interviews, personal data of the respondent, etc. After these are specified the user can run the `get.trans.db` function, which will return the `trans.db` object - a dataframe of variable names that are to be extracted from the data. This function is similar to the abovementioned `get.other.db` function, but it omits the `_other` questions.
- `missing_vars`- a dataframe containing the variables that are not present in the `trans.db` and should be added to it. The user needs to specify the variable and its label.  

After this, the user can continue running and translating the responses, this will produce the `text_requests_final` document in the `directory_dictionary$dir.requests` with the following structure

| uuid | loop_index| name  | responses | response.en| true| invalid  |
|------|-----------| ------|-----------|------------| ----|----------|
| ID   | loop_ID  | variable_name  |the response in Ukrainian/Russian| The translated response in English | Whether the response is appropriate| Whether the response is invalid|

After the file is created, the user's task is to open the excel file and look through the `response.en` column to see if the translation and the answer itself is appropriate.

- If the translation is good and the answer is appropriate, put the correct translation into the `true` column.
- If the answer is invalid - as in, it's not related to the question that is being asked, type `YES` into the `invalid` column.

When you're done with this, you can save the excel file and move on to applying the changes to the dataset.

#### Applying recode changes  
When the user starts running the `section_4_apply_changes_to_requests` file the script will go through a round of checks to see whether the `other_requests_final` file was filled properly. It will check:
- Whether the choices that the user has added in the `existing` column weren't already chosen by the user within the `choices` column (if they were, those entries will be removed from the requests file).
- Whether the entries were filled properly (only 1 column out of `existing`, `true`,`invalid` is filled). If the `_other` response is such that you need to recode the response into both `existing` and `true`, you can fill two columns at once, but you will get a warning to make sure you know what you're doing.
- Whether there are any empty rows
- Whether the choices that the user has added in the `existing` column are actually present in the `tool.choices` object.
- Whether the user has any entries within `existing.v` column that match the `None` criterion. These values include: `None`,`Don't know`,`Prefer not to answer`,etc. These entries need special treatment as the user entering them means that all other replies to the given question are invalid except for the `None` reply. The check looks if any of your entries are similar to these and asks the user to make sure that the 'name' values of these choices (from `tool.choices` object) are present in the `none_selection` object. By default, the object includes c(`do_not_know`, `prefer_not_to_answer`, `none`,`none_of_the_above`,`dont_know`, `do_not_want_to_answer`) and is passed to `recode_others` function that passes it internally to `recode.others_select_multiple` function. If you need to add some other cases, or remove them, feel free to modify this object.
- Whether your `select_one` variables only have one option in the `existing.v` column.
- Whether you've included any spaces or other empty values in any of the columns.

If those checks have passed, the script will split the requests file depending on whether the questions belong to the main or loop dataframe. Each of these pairs of objects (the dataframe and its relevant recode requests) will be passed through the function `recode.others` that will create a cleaning log with the following set of changes for each case:
- If the reply is `true`, it'll replace the Ukrainian/Russian version of the `_other` response with the translated version.
- If the reply is `invalid`, the function will:
  - Replace the text column value with an NA value.
  - The value in the cumulative column will be changed to NA if the `ref.name` is a `select_one`. If `ref.name` is a `select_multiple` and some other choices were also chosen except for `other` it'll remove the `other` response from the cumulative column and replace the value in the binary column of `ref.name/other` with a 0. If the respondent has only chosen `other` when responding to 'ref.name', the function will replace the value of the cumulative column with NA, and change the values of all binary columns to NA as well.
- If the reply is existing, the function will replace the text column value with an NA value, replace the `other` response in the cumulative column with the `existing` choice, recode the `ref.name/other` binary column to 0 and change the `ref.name/existing` column to 1.  

Once this is done, the next bit of code deals with the *elsewhere* cases. This bit of the script creates another cleaning log that does the following:
- As the row is already marked as `invalid` no changes need to be applied to the `_other` and `ref.name` columns.
- The value of the `_other` column specified in the `true_column` will be changed to the value specified in the `true_elsewhere`
- The value of `other` will be added to `true_column_parent`.
- If the value of `true_column_parent` is a `select_multiple` the binary column `true_column_parent/other` will also be changed to 1.  

**This process is applied to the `raw.main` and each of the loops that you have in your environment.**

**Please note that the output of these functions is just the cleaning log. These changes haven't yet been applied to the data**

After this is done, we move on to the `recode.relevancy` framework.

#### recode.relevancy framework
The `recode.relevancy` framework is created for cases where the `ref.name` you are recoding is a `select_multiple` that direct the respondent to other questions depending on their answers. For example `ref.name` is a `select_multiple` asking the respondent to tell us what types of humanitarian aid they have received. After they reply, they are directed to a set of questions asking about the quality of said aid. If we are recoding the `ref.name` it inevitably influences these relevant questions, so we have to recode them as well. This is where the `recode.relevancy` framework comes in.

The first step in this framework is filling up the vector of `select_multiple_list_relevancies`. It stores the names of `select_multiple` variables that open up other relevant questions. The script then calls the `find.relevances` function that searches for the binary columns of the select multiple that open up each sub-question within the relevancy. This function creates `relevancy_dictionary` - a table of relationships between the variables in the following form.

| name | relevancy|
|------|----------|
| variable/option1  | variable_option1_detail  |
| variable/option2  | variable_option2_detail  |

This object, together with the dataframe and its relevant cleaning log is fed to the `recode.other.relevances` function. This function creates a cleaning log documenting the following changes:
- If the binary variable of `_other` was recoded to 0 it replaces the `_detail` variable with NA
- If the `_other` response was classified as `existing`, the `_detail` variable for `_other` choice will be replaced with NA and its answer will be transfered to the `existingOption_detail` variable.  

**Please note that as of now, this feature is experimental, should you encounter any bugs please report them to the package's maintainer.**  

**This process is applied to the `raw.main` and each of the loops that you have in your environment.**

**Please note that the output of these functions is just the cleaning log. These changes haven't yet been applied to the data**

After all of cleaning logs have been created the changes outlined in those objects are applied to the datasets through the `apply.changes` function.

#### Recoding translation requests
Since the changes needed for the translation requests are pretty basic, the cleaning log is created out of them on the stage of loading the dataset through the `recode.trans.requests` function. If the response is deemed `invalid` it is replaced with NA, if it's `true` then the Ukrainian/Russian text is changed to English.

After these cleaning logs are created, the changes outlined in those objects are applied to the datasets through the `apply.changes` function.

#### Non-english check
The next bit of the recoding script checks your dataframe for leftover non-english characters. After the steps above, you shouldn't have any non-english characters left in your dataframe. First you need to specify the column that will be omitted from this check in the vector of the `vars_to_omit` object. The use for this object is the same as the `trans_cols_to_skip` object.  
If any non-english characters have been found in your dataframes it'll be stored in the `cyrillic.main` or `cyrillic.loopx` objects. It is up to you to decide what to do with them.

#### Consistency check
The final bit of the script is running the `select.multiple.check` which tries to find inconsistencies between the cumulative columns of `select_multiple` questions and their respective binary columns. The file will show the differences between what is expected from the cumulative column and what is present in the binaries. It is left up to the user to decide what to do with these inconsistencies.

#### Binary check
This is a small check that loops through all binary values in the dataframe and looks at whether all values are what we expect them to be: either `0`, `1` or `NA`. If there are any inconsistencies, the script will throw an error and ask the user to check the `wrong_values_frame` object for details of the error. It stores variables that have unexpected entries. The user should check the unique values on those to see what caused the error.

### 999 checks
The next bit of the script checks the dataframes for 99 and 999 values in numerical columns as well as any other values that you're suspicious of. You can specify these values in the `code_for_check` vector. These values are suspicious because they are a relic from the SPSS based sociological research. As the .sav values save values as numerics, it became necessary to assign NA or DK values a single code so that they are easily recognisable and you can recode them quickly. Usually, these are maked as 99,98 or 999. This is not applicable for us as we're not working in SPSS. This bit of script produces a `cl_log_999` object and a `output/checking/999_diferences.xlsx` excel file that document these entries. You can look through them and keep the rows that need to be recoded in your opinion.

Once you've deleted everything that needs to be deleted, you can apply these changes to the data and add them to the cleaning log by setthing `apply_999_changes` parameter to `Yes` and running `section_5_finish_999_checks.R`.

### Logic checks
This section is dedicated to user-made checks for the general logical consistency across different columns. This is left for the user to fill in on their own as we cannot universalise this. 

### Checks for outliers
This section runs an algorithm over all numeric columns in the dataframe to see if any of the values are outside of the expected margins. The user needs to specify the following parameters:
- `n.sd` - the number of standard deviations used to determine if an entry is an outlier
- `method` - which method should be used to locate outliers. The function allows for the following methods:
  - method = 'o1' Method based on Z score and logarithmization of the values
  - method = 'o2' Modified Z score which based on the median absolute deviation, recommended n.sd
  - method = 'o3' Method based on the interquartile range
  - method = 'o4' Method based on the median absolute deviation
- `ignore_0` - Whether 0 should be ignored when calculating outliers (needs to be true for methods using logarithmic transformations of variables)
- `cols.integer` - A set of parameters specifying exactly which variables should be checked in a given dataframe. All numeric variables will be checked if these are left blank.  

The script then runs the selected outlier detection algorithm and writes the suspicious responses into `cleaning.log.outliers` object and the `outlier_analysis_` excel file. It also creates a nice visualization of the distribution of the responses with outliers highlighted in red in the `outlier_analysis_` pdf file.

The excel file will have a new column `checked`. It exists to allow the user to let the HQ know that the outlier value was checked even if it's not fixed. If the outlier value is accurate and you with to keep it in the dataset, set the value of the `checked` column to `value checked`, if you want to change the old value to the new one, specify it within the `new.value` column and set the `checked` column value to `value corrected`. Now you can load up the clean excel file as the `cleaning.log.outliers_full` object and run `section_6_finish_outlier_check.R`.

### Finalize the data
The last section goes through removal of PII columns and columns where all values are empty, gathering the cleaning and deletion logs, building the submission excel for HQ validation and writing the submission package. Outside of specifying the `pii.to.remove_main` that holds the names of the PII columns barely any interaction is needed from the users side. The file for HQ submission is written as `Cleaning_logbook` excel in the `output/Cleaning_logbook` directory.

### Contributors 

This set of functions and scripts is a legacy of Reach Syria team. It was later transfered to Reach Regional team in Ukraine/Poland/Moldova. Reach Ukraine team has built a package around those functions by testing, optimizing and improving the legacy scripts and running the presented script on multiple research cycles. If you find any bugs or have any suggestions, please text the package maintainers.

<a href="https://github.com/Nestor-Ch/utilityR/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=Nestor-Ch/utilityR" />
</a>  


<!-- badges: start -->

[![R-CMD-check](https://github.com/Nestor-Ch/utilityR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Nestor-Ch/utilityR/actions/workflows/R-CMD-check.yaml) [![codecov](https://codecov.io/gh/Nestor-Ch/utilityR/graph/badge.svg?token=BYYLEDL4XU)](https://codecov.io/gh/Nestor-Ch/utilityR)

<!-- badges: end -->
