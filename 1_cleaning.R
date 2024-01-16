setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())


directory_dictionary <- list(
  research_cycle_name = 'xxxx',
  round = 'xxxx',
  dir.audits = "data/inputs/audits/reach/", # The directory to your audit files
  dir.audits.check = "output/checking/audit/",# The directory to your audit summary files (you'll be checking these)
  dir.requests = "output/checking/requests/", # the directory of your other_requests file
  dir.responses = "output/checking/responses/", # the directory of your responses to open questions
  enum_colname = "XXX", # the column that contains the enumerator ID,
  enum_comments = 'XXX', # the column that contains the enumerator's comments,
  filename.tool = "resources/XXX.xlsx", # the name of your Kobo tool and its path
  data_name = "XXXX.xlsx", # the name of your dataframe
  data_path = "data/inputs/kobo_export/", # the path to your dataframe
  label_colname = 'label::English', # the name of your label column. Has to be identical in Kobo survey and choices sheets
  dctime_short = "XXXX" # the data of your survey (just for naming)
)

api_key <- source('resources/microsoft.api.key_regional.R')$value

#-------------------------------Initialize packages, load tools -----------------------------
source("src/init.R")

#-------------------------------Load Tools -----------------------------

source("src/load_Data.R")

## Section below only for research cycles that requires cleaning on regular basis and use one kobo server.
cat(paste0("Section below only for research cycles that requires cleaning on regular basis and use one kobo server."))

# --------------------------------Section  0  - Data pre-processing -----------------------------------

# Run if you have any old files that need to be loaded and removed from the final dataframe
source('src/sections/process_old_data.R')


# final preparation
# Rename your dataframes

raw.main <- kobo.raw.main

sheet_names <- sheet_names[sheet_names!='kobo.raw.main']
sheet_names_new <- gsub('kobo.','',sheet_names)

if(length(sheet_names_new)>0){
  for(i in 1:length(sheet_names_new)){
    txt <- paste0(sheet_names_new[i],' <- ',sheet_names[i])
    eval(parse(text=txt))
  }
}


# If there were any changes in the tool during data collection, they can be run here
source('src/sections/tool_modification.R')


# select the columns in your data that contain date elements
date_cols_main <- c("start","end", tool.survey %>% filter(type == "date" & datasheet == "main") %>% pull(name),
                    "submission_time") # add them here

# transform them into the datetime format
raw.main <- raw.main %>%
  mutate_at(date_cols_main, ~ifelse(!str_detect(., '-'), as.character(convertToDateTime(as.numeric(.))), .))

rm(date_cols_main)



# --------------------------------Section  1  - Remove duplicates and No consent entries -----------------------------------

source('src/sections/section_1_remove_duplicates_no_consents.R')


# --------------------------------Section  2  - Audit checks + soft duplicates -----------------------------------
min_duration_interview <- 5 # minimum duration of an interview (screen time in minutes)
max_duration_interview <- 60 # maximum duration of an interview (screen time in minutes)
pre_process_audit_files <- F # whether cases of respondent taking too long to answer 1 question should cleaned.
# if pre_process_audit_files =T, enter the maximum time that  the respondent can spend answering 1 question (in minutes)
max_length_answer_1_question <- 20
# Used during the check for soft duplicates.
# The minimum number of different columns that makes us confident that the entry is not a soft duplicate
min_num_diff_questions <- 8

# run the checks
source('src/sections/section_2_run_audit_checks.R')

# once you've checked all entries in the "output/checking/audit/" directory and only left what needs to be deleted
# implement the decisions below

source('src/sections/section_2_run_audit_decisions.R')

# --------------------------------Section  3  - Loop inconsitencies + spatial checks -----------------------------------

source('src/sections/section_3_loops_and_spatial_checks.R')

write.xlsx(deletion.log.new, make.filename.xlsx("output/deletion_log/", "deletion_log", no_date = T), overwrite=T)

#--------------------------- Section  4 - Others and translations----------------------------------------------------


# if res is empty, data doesn't consist mismatching, if res has a lot of recordings for 1 uuid - probably you have duplicates

source('src/sections/section_4_create_other_requests_files.R')

# name that hosts the clean recode.others file, leave as '' if you don't have this file. Nothing will be recoded that way
name_clean_others_file <- 'XXX'
sheet_name_others <- 'Sheet2' # name of the sheet where you're holding your requests
# name that hosts the clean translation requests file, leave as '' if you don't have this file. Nothing will be recoded that way
name_clean_trans_file <- 'XXX'


source('src/sections/section_4_apply_changes_to_requests.R')

# Check if your data still has any cyrillic entries

# variables that will be omitted from the analysis
vars_to_omit <- c('settlement', directory_dictionary$enum_colname, directory_dictionary$enum_comments) # add more names as needed

source('src/sections/section_4_post_check_for_leftover_cyrillic.R')

# Check that cumulative and binary values in select multiple match each other

cleaning.log.match <- utilityR::select.multiple.check(raw.main, tool.survey, id_col="uuid")

if (nrow(cleaning.log.match) > 0) {
  write.xlsx(cleaning.log.match, "output/checking/select_multiple_match.xlsx", overwrite=T)
}


# Check that cumulative and binary values in select multiple match each other

cleaning.log.match <- utilityR::select.multiple.check(raw.main, tool.survey, id_col="uuid")

if (nrow(cleaning.log.match) > 0) {
  write.xlsx(cleaning.log.match, "output/checking/select_multiple_match.xlsx", overwrite=T)
}
#--------------------------- Section  5 - Check for 999/99 entries----------------------------------------------------

# Check if any columns are equal to '999'/'99', enter any other values you're suspicious of

code_for_check  <- c('99','999')

source('src/sections/section_5_create_999_checks.R')

print(cl_log_999)

# if Anything got into cl_log_999, check it. If you want to delete it from your data run the command below
# set apply_999_changes to 'Yes' if you want to remove the entries from code_for_check
apply_999_changes <- 'No'

source('src/sections/section_5_finish_999_checks.R')

# ----------------------------------Section L - Your logic checks go here--------------------------------

cleaning.log.checks.direct <- tibble()


# ------------------------------------------------------------------------------

# ----------------------------------Section 6 - Check for outliers--------------------------------

# specify the number of standard deviations you want to use
n.sd <- 3

# specify methods for  detecting outliers
method <- "o1"

# ignore 0 values or not
ignore_0 <- T

# specify columns for check or leave them empty
cols.integer_raw.main <- c()
cols.integer_raw.loop1 <- c()
cols.integer_raw.loop2 <- c()
cols.integer_raw.loop3 <- c()

# if you need one more loop you can add it here and in the src/section_6_detect_and_visualise_outliers.R
# You can also remove redundant loop

source('src/sections/section_6_detect_and_visualise_outliers.R')

write.xlsx(cleaning.log.outliers, paste0("output/checking/outliers/outlier_analysis_", n.sd, "sd.xlsx"), overwrite=T)


#------------------------------------------------------------------------------------------------------------
# --> edit the file
# --> Manually check outliers and change to NA (Decision made with country FPS)
# --> save new file as outliers_responses_edited.xlsx in output/checking/responses/
#------------------------------------------------------------------------------------------------------------

# RUN ONLY IF Anything need to be changed

cleaning.log.outliers <- read.xlsx(paste0("output/checking/outliers/outlier_analysis_", n.sd, "sd.xlsx"))

source('src/sections/section_6_finish_outlier_check.R')

cleaning.log <- rbind(cleaning.log,cleaning.log.outliers)


# ----------------------------------Section 7 - Remove PII columns, apply any last changes, then save cleaned dataset--------------

# finalize cleaning log:
cleaning.log <- cleaning.log %>% distinct() %>%
  filter(old.value %!=na% new.value) %>% left_join(raw.main %>% select(uuid, any_of(directory_dictionary$enum_colname)))

if (length(list.files(make.filename.xlsx("output/cleaning_log", "cleaning_log", no_date = T))) > 0) {
  cleaning.log.previous <- read_xlsx(make.filename.xlsx("output/cleaning_log", "cleaning_log"))
  cleaning.log.whole <- rbind(cleaning.log.previous, cleaning.log)
} else {
  cleaning.log.whole <- cleaning.log
}
# Output Cleaning Log
write.xlsx(cleaning.log.whole, make.filename.xlsx("output/cleaning_log", "cleaning_log", no_date = T), overwrite = T)


# combine new and previous data:
# ------------------------------------------------------------------------------

pii.to.remove_main <- c(
  "deviceid",
  "staff_other",
  "audit",
  "audit_URL",
  "username")

raw.main  <- raw.main %>% select(-any_of(pii.to.remove_main))

# remove from raw.

if(length(ls)>1){
  ls_loops <- ls[2:length(ls)]
}else{ls_loops <- c()}

if(length(ls)>1){
txt <- paste0(
  'datasheets <-list("main" =kobo.raw.main,',
  paste0('"',ls_loops,'" = ',sheet_names, collapse = ','),')'
)
}else{
  txt <- 'datasheets <-list("main" =kobo.raw.main)'
}
eval(parse(text= txt))

write.xlsx(datasheets, make.filename.xlsx("output/data_log", "full_data"), overwrite = T,
           zoom = 90, firstRow = T)


# final (pii removed)

if(length(ls)>1){
txt <- paste0(
  'datasheets_anon <-list("main" =raw.main,',
  paste0('"',ls_loops,'" = ',sheet_names_new, collapse = ','),')'
)}else{
  txt <- 'datasheets_anon <-list("main" =raw.main)'
}
eval(parse(text= txt))

write.xlsx(datasheets_anon, make.filename.xlsx("output/final", "final_anonymized_data"), overwrite = T,
           zoom = 90, firstRow = T)

source("src/count_enum_performance.R")
source("package4validation.R")

cat("\nD O N E\n")
