
# load your audit files
audits <- utilityR::load.audit.files(directory_dictionary$dir.audits, uuids = raw.main$uuid, track.changes = F) 

# add 2 more columns to make readable start and end columns
audits$start_readable <- as.POSIXct(audits$start / 1000, origin = "1970-01-01")
audits$end_readable <- as.POSIXct(audits$end / 1000, origin = "1970-01-01")

# if you want to pre-process audits, do it here
if(pre_process_audit_files){
  audits <- utilityR::pre.process.audits(audits, threshold = max_length_answer_1_question)
}


# process your audit files to get the duration of each interview. To understand what each column means, run help(process.uuid)
if(nrow(audits) == 0) {
  audits.summary <- tibble(uuid = raw.main$uuid, tot.rt = NA)
}else{
  audits.summary <- audits %>% 
    group_by(uuid) %>% 
    group_modify(~utilityR::process.uuid(.x))
}

# get the additional data from the main df
data.audit <- raw.main %>%
  mutate(duration_mins = difftime(end, start, units = 'mins'),
         num_NA_cols = rowSums(is.na(raw.main)),
         num_dk_cols = rowSums(select(., matches("dk_undec")), na.rm = T),
         num_other_cols = rowSums(!is.na(raw.main[str_ends(colnames(raw.main), "_other")]), na.rm = T)) %>%
  select(uuid, !!sym(directory_dictionary$enum_colname), start, end, duration_mins, num_NA_cols, num_dk_cols, num_other_cols)

# Generate the audits_summary file - General info about each interview
audits.summary <- data.audit %>% 
  left_join(audits.summary, by="uuid") %>% select(-contains("/")) %>% 
  relocate(uuid, duration_mins, num_NA_cols, num_dk_cols, num_other_cols, tot.rt) %>% 
  arrange(duration_mins)

write.xlsx(audits.summary, make.filename.xlsx(directory_dictionary$dir.audits.check, "audits_summary"))


# follow up with FPs if there are surveys under 10 minutes or above 1 hour
survey_durations_check <- audits.summary %>% filter(tot.rt < min_duration_interview | tot.rt > max_duration_interview)
if(nrow(survey_durations_check) > 0){
  write.xlsx(survey_durations_check, make.filename.xlsx(directory_dictionary$dir.audits.check, "survey_durations"),
             zoom = 90, firstRow = T)
}else cat("\nThere are no survey durations to check :)")


## Soft duplicates (less than 12 different columns?)

res.soft_duplicates <- find.similar.surveys(raw.main, tool.survey, uuid = "uuid") %>% 
  filter(number_different_columns <= min_num_diff_questions) %>% 
  relocate(uuid, num_cols_not_NA,num_cols_idnk,`_id_most_similar_survey`,
           number_different_columns) %>% 
  arrange(number_different_columns)

if(nrow(res.soft_duplicates) > 0){
  write.xlsx(res.soft_duplicates, make.filename.xlsx(directory_dictionary$dir.audits.check, "soft_duplicates"))
}else{
  cat("\nThere are no soft duplicates to check :)")
}
rm(audits, data.audit)
