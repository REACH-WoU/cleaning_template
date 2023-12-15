setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())


directory_dictionary <- list(
  dir.audits = "data/inputs/audits/reach/", # The directory to your audit files
  dir.audits.check = "output/checking/audit/",# The directory to your audit summary files (you'll be checking these)
  dir.requests = "output/checking/requests/", # the directory of your other_requests file 
  dir.responses = "output/checking/responses/", # the directory of your responses to open questions
  enum_colname = "enumerator_id", # the column that contains the enumerator ID,
  enum_comments = 'enum_comms', # the column that contains the enumerator's comments,
  filename.tool = "resources/Reach_UKR2306_CCCM_DS_tool_r2_v3 1.xlsx", # the name of your Kobo tool and its path
  data_name = "Reach_UKR2306_CCCM_DS_tool_r2_-_all_versions_-_False_-_2023-11-25-09-58-22 (1).xlsx", # the name of your dataframe
  data_path = "data/inputs/kobo_export/", # the path to your dataframe
  label_colname = 'label::English', # the name of your label column. Has to be identical in Kobo survey and choices sheets
  dctime_short = "2023_01_01" # the data of your survey (just for naming)
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
max_length_answer_1_question <- 20 # if pre_process_audit_files =T, enter the maximum time that 
# the respondent can spend answering 1 question (in minutes) 
min_num_diff_questions <- 8 # Used during the check for soft duplicates. 
# The minimum number of different columns that makes us confident that the entry is not a soft duplicate

# run the checks
source('src/sections/section_2_run_audit_checks.R')

# once you've checked everything - implement the decisions below

source('src/sections/section_2_run_audit_decisions.R')

# --------------------------------Section  3  - Loop inconsitencies + spatial checks -----------------------------------

source('src/sections/section_3_loops_and_spatial_checks.R')

#--------------------------- Section  4 - Others and translations----------------------------------------------------

source('src/sections/section_4_create_other_requests_files.R')

name_clean_others_file <- 'DS_r2_other_requests_final_2023_12_04'
name_clean_trans_file <- '???'


source('src/sections/section_4_apply_changes_to_requests.R')

# Check if your data still has any cyrillic entries

vars_to_omit <- c('uuid','loop_index') # add more names as needed

source('src/sections/section_4_post_check_for_leftover_cyrillic.R')

#--------------------------- Section  5 - Check for 999/99 entries----------------------------------------------------

# Check if any columns are equal to '999'

code_for_check  <- '99'

source('src/sections/section_5_create_999_checks.R')


# ----------------------------------Section 6 - Your logic checks go here--------------------------------

cleaning.log.checks.direct <- tibble()



# ------------------------------------------------------------------------------

#############################################################################################################
# 5) Outliers
#############################################################################################################
# save.image(file = "Environment.RData")
# load("Environment.RData")

cleaning.log.outliers <- data.frame()
# define columns to check for outliers

cols.integer_main <- filter(tool.survey, type == "integer")
cols.integer_raw.main <- cols.integer_main[cols.integer_main$name %in% colnames(raw.main),] %>% pull(name)
cols.integer_raw.loop1 <- cols.integer_main[cols.integer_main$name %in% colnames(raw.loop1),] %>% pull(name)
cols.integer_raw.loop2 <- cols.integer_main[cols.integer_main$name %in% colnames(raw.loop2),] %>% pull(name)
cols.integer_raw.loop3 <- cols.integer_main[cols.integer_main$name %in% colnames(raw.loop3),] %>% pull(name)

# cols <- filter(tool.survey, str_starts(name, "G_3")) %>% pull(name)

n.sd <- 2

df.all <- data.frame()
#------------------------------------------------------------------------------------------------------------
# [MAIN SHEET] -> detect outliers 



# Outliers 

raw.main.outliers <- detect.outliers(
  df = raw.main,
  id = 'uuid',
  colnames = cols.integer_raw.main,
  is.loop = F,
  n.sd = 2)

raw.loop2.outliers <- detect.outliers(
  df = raw.loop2,
  id = 'loop_index',
  colnames = cols.integer_raw.loop2,
  is.loop = T,
  n.sd = 2)


raw.loop3.outliers <- detect.outliers(
  df = raw.loop3,
  id = 'loop_index',
  colnames = cols.integer_raw.loop3,
  is.loop = T,
  n.sd = 2)


all_outliers <- rbind(raw.main.outliers,raw.loop2.outliers,raw.loop3.outliers)

write.xlsx(raw.main.outliers, paste0("output/checking/outliers/main_outlier_prices_analysis_", n.sd, "sd.xlsx"), overwrite=T)


# Outliers Boxplot generator

# Outliers Boxplot generator

raw.main.df <- raw.main %>% 
  select(uuid, cols.integer_raw.main) %>% 
  mutate_at(cols.integer_raw.main, as.numeric) %>% 
  pivot_longer(cols.integer_raw.main, names_to = 'variable', values_to = 'value') %>% 
  filter(!is.na(value) & value>0) %>% 
  mutate(value.log = log10(value)) %>% 
  left_join(
    raw.main.outliers %>% select(uuid,variable,old.value,issue) %>% 
      rename(is.outlier=issue,
             value = old.value)
  ) %>% mutate(is.outlier = ifelse(is.na(is.outlier), 'Regular', is.outlier))

raw.loop2.df <- raw.loop2 %>% 
  select(uuid, loop_index , cols.integer_raw.loop2) %>% 
  mutate_at(cols.integer_raw.loop2, as.numeric) %>% 
  pivot_longer(cols.integer_raw.loop2, names_to = 'variable', values_to = 'value') %>% 
  filter(!is.na(value) & value>0) %>% 
  mutate(value.log = log10(value)) %>% 
  left_join(
    raw.loop2.outliers %>% select(loop_index,variable,old.value,issue) %>% 
      rename(is.outlier=issue,
             value = old.value)
  ) %>% mutate(is.outlier = ifelse(is.na(is.outlier), 'Regular', is.outlier))

raw.loop3.df <- raw.loop3 %>% 
  select(uuid, loop_index , cols.integer_raw.loop3) %>% 
  mutate_at(cols.integer_raw.loop3, as.numeric) %>% 
  pivot_longer(cols.integer_raw.loop3, names_to = 'variable', values_to = 'value') %>% 
  filter(!is.na(value) & value>0) %>% 
  mutate(value.log = log10(value)) %>% 
  left_join(
    raw.loop3.outliers %>% select(loop_index,variable,old.value,issue) %>% 
      rename(is.outlier=issue,
             value = old.value)
  ) %>% mutate(is.outlier = ifelse(is.na(is.outlier), 'Regular', is.outlier))

df.all <- bind_rows(raw.main.df, raw.loop2.df, raw.loop3.df)

# generating boxplots
g.outliers_main <- ggplot(df.all) +
  geom_boxplot(aes(x = variable, y = value), width = 0.2) + ylab("Values") +
  geom_point(aes(x  =variable, y = value, colour =is.outlier)) +
  facet_wrap(~variable, ncol = 4, scales = "free_y")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_color_manual(values = c('red','black'))

# Save
ggsave(paste0("output/checking/outliers/main_outlier_prices_analysis_", n.sd, "sd.pdf"), g.outliers_main, 
       width = 40, height = 80, units = "cm", device="pdf")


cleaning.log.outliers <- rbind(cleaning.log.outliers,res.outliers_main)

save.outlier.responses_msna(cleaning.log.outliers)  

#------------------------------------------------------------------------------------------------------------
# --> edit the file
# --> Manually check outliers and change to NA (Decision made with country FPS)
# --> save new file as outliers_responses_edited.xlsx in output/checking/responses/
#------------------------------------------------------------------------------------------------------------

# RUN ONLY IF Anything need to be changed

outlier.recode <- load.edited(dir.responses, "outliers")
outlier.check <- load.edited(dir.requests, "outliers")

if (nrow(outlier.check) != nrow(outlier.recode)) warning("Number of rows are not matching")

cleaning.log.outliers <- outlier.recode %>%
  select(uuid,loop_index,variable,issue,old.value,new.value) %>%
  filter(is.na(new.value))

raw.main <- raw.main %>% 
  apply.changes(cleaning.log.outliers)

cleaning.log <- rbind(cleaning.log,cleaning.log.outliers)


#-------------------------------------------------------------------------------
# 6) Remove PII columns, apply any last changes, then save cleaned dataset
################################################################################

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
  
data.list <- ls()[grepl('^kobo.raw.loop[[:digit:]]$',ls())]

txt <- paste0(
  'datasheets <-list("main" =kobo.raw.main,',
  paste0('"',ls_loops,'" = ',data.list, collapse = ','),')'
)
eval(parse(text= txt))

write.xlsx(datasheets, make.filename.xlsx("output/data_log", "full_data"), overwrite = T,
           zoom = 90, firstRow = T)


# final (pii removed)

data.list <- ls()[grepl('^raw.loop[[:digit:]]$',ls())]

txt <- paste0(
  'datasheets_anon <-list("main" =raw.main,',
  paste0('"',ls_loops,'" = ',data.list, collapse = ','),')'
)
eval(parse(text= txt))

write.xlsx(datasheets_anon, make.filename.xlsx("output/final", "final_anonymized_data"), overwrite = T,
           zoom = 90, firstRow = T)

source("src/count_enum_performance.R")
source("package4validation.R")

cat("\nD O N E\n")
