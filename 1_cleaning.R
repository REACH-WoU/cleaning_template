setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
JMMI_variable <- "Retailers"
####################
##  LOAD Tool     ##
####################
source("src/init.R")

####################
##  LOAD Data     ##
####################

source("src/load_Data.R")

# small utility functions
make.short.name <- function(name, no_date = F) return(gsub("__","_", paste0("JMMI_", JMMI_variable, "_R15_", name, ifelse(no_date, "", paste0("_", dctime_short)))))
make.filename.xlsx <- function(dir = ".", name, no_date = F) return(gsub("//","/", paste0(dir, "/", make.short.name(name, no_date), ".xlsx")))

#cleaning.log <- read.xlsx("resources/cleaning_log.xlsx")

#########################
##  Delete old columns ##
#########################

raw.main <- kobo.raw.main


#-------------------------------------------------------------------------------
# 
# uuids_to_remove <- c()
# 
# # update this to the latest data log for this country or leave as-is
# filename_dataset_previous <- "output/data_log/..."
# 
# if(filename_dataset_previous != "output/data_log/..."){
#   main.data.previous <- read_xlsx(filename_dataset_previous, col_types = "text", sheet = 1)
#   loop1.previous <- read_xlsx(filename_dataset_previous, col_types = "text", sheet = 2)
#   loop2.previous <- read_xlsx(filename_dataset_previous, col_types = "text", sheet = 3)
#   uuids_to_remove <- main.data.previous$uuid
# }
# 
# deletion.log.previous <- load.requests("output/deletion_log", paste0(country_short, ".*_deletion_log"))
# uuids_to_remove <- c(uuids_to_remove, deletion.log.previous$uuid)

# ########################################
# ##  load & remove previous data        ##
# raw.main <- kobo.raw.main %>%          ##
#   filter(!(uuid %in% uuids_to_remove)) 
# raw.loop1 <- kobo.raw.loop1 %>%        ##
#   filter(!(uuid %in% uuids_to_remove)) 
# # raw.loop2 <- kobo.raw.loop2 %>%        ##
# #   filter(!(uuid %in% uuids_to_remove)) 
#                                        ##
########################################
raw.main <- kobo.raw.main

#######################  Customers ############################################
if (JMMI_variable == "Customers") {
# Change in the tool 
cols_remove <- c(
  "b3_warm_clothes",
  "b4_fuel",
  "b5_medication",
  "b6_coal_price",
  "b7_regular_fuel",
  "b7_regular_fuel/petrol",
  "b7_regular_fuel/diesel",
  "b7_regular_fuel/gas_vehicles",
  "b7_regular_fuel/gas_heating",
  "b7_regular_fuel/coal",
  "b7_regular_fuel/firewood",
  "b7_regular_fuel/wood_pellets",
  "b7_regular_fuel/none",
  "b7_coal_possible_buy",
  "b8_firewood_price",
  "b9_firewood_possible_buy",
  "b10_access_stores",
  "b10_access_stores/no_impact",
  "b10_access_stores/movement_restrictions",
  "b10_access_stores/fighting_shelling",
  "b10_access_stores/buildings_damaged",
  "b10_access_stores/road_damage",
  "b10_access_stores/lack_transportation",
  "b10_access_stores/feel_unsafe",
  "b10_access_stores/other",
  "b10_access_stores/prefer_not_answer",
  "b10_1_access_stores_other",
  "b11_financial_factors",
  "b11_financial_factors/not_affect",
  "b11_financial_factors/items_not_available",
  "b11_financial_factors/not_afford",
  "b11_financial_factors/not_enough_cash",
  "b11_financial_factors/public_transportation",
  "b11_financial_factors/fuel_expensive",
  "b11_financial_factors/other",
  "b11_financial_factors/prefer_not_answer",
  "b11_financial_factors/prices_increased",
  "b11_1_financial_factors_other",
  "b15_access_stores",
  "b15_access_stores/no_impact",
  "b15_access_stores/movement_restrictions",
  "b15_access_stores/fighting_shelling",
  "b15_access_stores/buildings_damaged",
  "b15_access_stores/road_damage",
  "b15_access_stores/lack_transportation",
  "b15_access_stores/feel_unsafe",
  "b15_access_stores/other",
  "b15_access_stores/prefer_not_answer",
  "b15_1_access_stores_other",
  "b16_financial_factors",
  "b16_financial_factors/not_affect",
  "b16_financial_factors/items_not_available",
  "b16_financial_factors/not_afford",
  "b16_financial_factors/not_enough_cash",
  "b16_financial_factors/public_transportation",
  "b16_financial_factors/fuel_expensive",
  "b16_financial_factors/prices_increased",
  "b16_financial_factors/other",
  "b16_financial_factors/prefer_not_answer",
  "b16_1_financial_factors_other"
  
)
}
##############################################################################

############### Retailers #####################################################

##Change in the tool
if (JMMI_variable == "Retailers") {
cols_remove <- c(
  "u2_food_supply",
  "u2_1_food_supply_hromada",
  "u2_2_food_supply_raion",
  "u2_3_food_supply_oblast",
  "u2_4_food_supply_ukraine",
  "u2_5_food_supply_ru_bel",
  "u2_6_food_supply_foreign",
  "u2_6_1_food_supply_foreign_other",
  "food_check",
  "food_check_note",
  "u4_nfi_supply",
  "u4_1_nfi_supply_hromada",
  "u4_2_nfi_supply_raion",
  "u4_3_nfi_supply_oblast",
  "u4_4_nfi_supply_ukraine",
  "u4_5_nfi_supply_ru_bel",
  "u4_6_nfi_supply_foreign",
  "u4_6_1_nfi_supply_foreign_other",
  "nfi_check",
  "nfi_check_note",
  "x2_cash_limitation",
  "x3_credit_cards_limitation",
  "x4_debit_cards_limitation",
  "x5_mobile_apps_limitation",
  "x6_vouchers_limitation",
  "x7_other_limitation",
  "x8_cash_markup",
  "x9_credit_cards_markup",
  "x10_debit_cards_markup",
  "x11_mobile_apps_markup",
  "x12_vouchers_markup",
  "x13_other_markup"

)
}

###############################################################################
## Add any changes to the tool?
raw.main <- raw.main %>%
  select(-any_of(cols_remove, vars = NULL))
tool.survey <- tool.survey %>% 
  mutate(datasheet = "main")


#-------------------------------------------------------------------------------
# 0) INITIAL CLEANING: drop and rename columns
################################################################################


raw.main <- raw.main %>% 
  rename(submission_time = "_submission_time") %>% 
  rename_all(~sub("_geolocation","geolocation", .x)) 


cols_to_drop_main <- c(
  tool.survey %>% filter(type == "note") %>% pull(name),      # all note columns
  colnames(raw.main)[str_starts(colnames(raw.main), "_")]     # all columns with names starting with underscore
)


raw.main <- raw.main %>% select(-any_of(cols_to_drop_main, vars=NULL)) %>% relocate(uuid)
# raw.loop1 <- raw.loop1 %>% select(-starts_with("_")) %>% relocate(uuid)  # all columns with names starting with underscore
# raw.loop2 <- raw.loop2 %>% select(-starts_with("_")) %>% relocate(uuid)  # all columns with names starting with underscore


# fix dates:
date_cols_main <- c("date_assessment","start","end", tool.survey %>% filter(type == "date" & datasheet == "main") %>% pull(name),
                    "submission_time")


raw.main <- raw.main %>% 
  mutate_at(date_cols_main, ~ifelse(!str_detect(., '-'), as.character(convertToDateTime(as.numeric(.))), .))

# 
# date_cols_loop2 <- c("protection_incidents_when")
# raw.loop2 <- raw.loop2 %>%
#   mutate_at(date_cols_loop2, ~ifelse(!str_detect(., '-'), as.character(convertToDateTime(as.numeric(.))), .))


rm(cols_to_drop_main, date_cols_main)  

# # for ROM: move staff to enumerator_num
# if(country == "Romania"){
#   raw.main <- raw.main %>% 
#     mutate(enumerator_num = ifelse(isna(enumerator_num), staff, enumerator_num)) %>% 
#     select(-staff)
# }

#-------------------------------------------------------------------------------



# Addded this piece to add the missing data due to some old version submissions

################ Customers ####################################################
if (JMMI_variable == "Customers") {
missing_data <- raw.main %>%
  filter(is.na(raw.main["b17_access_stores/air_alert"]) | is.na(raw.main["b18_financial_factors/public_transportation"]) | is.na(is.na(raw.main["b7_vehicle_fuel/none"])))
cols_missing_yn <- colnames(raw.main) %>%
  str_subset(pattern = "\\_yn")
cols_missing_01 <- c(
  "b17_access_stores/power_outages",
  "b17_access_stores/air_alert",
  "b18_financial_factors/public_transportation"
)
cols_missing_none <- c(
  "b7_vehicle_fuel/none","b7_1_heating_fuel/none"
)
missing_data[cols_missing_01] <- "0"
missing_data["b7_vehicle_fuel/none"] <- missing_data["b7_vehicle_fuel/none_vehicles"] 
missing_data["b7_1_heating_fuel/none"] <- missing_data["b7_1_heating_fuel/none_heating"] 



for (col_yn in cols_missing_yn) {
  col_next <- substr(col_yn,1,nchar(col_yn)-3)
  col_yn_name <- col_yn
  missing_data[col_yn_name] <- case_when(missing_data[col_next] != "999" ~ "Yes",
                                         missing_data[col_next] == "999" ~ "No")
  
}


cleaning.log.missing <- recode.missing(raw.main,c(cols_missing_01,cols_missing_none,cols_missing_yn),missing_data,
                                       "correction due to old questionnaire version") %>% 
  distinct() %>% 
  filter(old.value %!=na% new.value)
#int_cols_main  <- tool.survey %>% filter(type == "decimal" & datasheet == "main") %>% pull(name)
#cl_999s <- recode.set.NA.if(raw.main, int_cols_main, "999", "replacing 999 with NA")
#cleaning.log <- bind_rows(cleaning.log.missing, cl_999s)
cleaning.log <- cleaning.log.missing

# ## Changing location (hromada name) due to enumerator error
# location_error <- raw.main %>%
#   filter(a7_current_hromada == "UA6802005" & a8_current_settlement == "other")
# location_error["a7_current_hromada"] <- "UA6802017"

# cleaning.log.location <- location_error %>% select("uuid") %>% mutate(variable = "a7_current_hromada", 
#                                                                       old.value="UA6802005", new.value= "UA6802017",
#                                                                       issue= "correction due to enumerator error")
# 
# 
# cleaning.log <- bind_rows(cleaning.log,cleaning.log.location)


raw.main <- raw.main %>%
  apply.changes(cleaning.log)
## enum entry error
# enum_error <- raw.main %>%
#   filter(a8_1_current_settlement_other == "Кіцмань")
# enum_error["a7_current_hromada"] <- "UA7306029"
# 
# cleaning.log.enum <- enum_error %>% select("uuid") %>% mutate(variable = "a7_current_hromada", 
#                                                               old.value="UA7306061", new.value= "UA7306029",
#                                                               issue= "correction due to enumerator error")
# 
# raw.main <- raw.main %>%
#   apply.changes(cleaning.log.enum)
# 
# cleaning.log <- bind_rows(cleaning.log,cleaning.log.enum)
}
###############################################################################


########################  Retailers  ###########################################
if (JMMI_variable == "Retailers") {
missing_data <- raw.main %>%
  filter(is.na(raw.main["w3_access_stores/no_access_due_to_power_outages"]))
cols_missing_yn <- colnames(raw.main) %>%
  str_subset(pattern = "\\_yn")
cols_missing_yn <- cols_missing_yn[1:length(cols_missing_yn)-1]
cols_missing_01 <- c(
  "v1_difficulties/storage_during_power_outages",
  "w3_access_stores/no_access_due_to_power_outages",
  "w3_access_stores/no_access_during_air_alert"
)
missing_data[cols_missing_01] <- "0"
for (col_yn in cols_missing_yn) {
  col_next <- substr(col_yn,1,nchar(col_yn)-3)
  col_yn_name <- col_yn
  missing_data[col_yn_name] <- case_when(missing_data[col_next] != "999" ~ "Yes",
                                         missing_data[col_next] == "999" ~ "No")

}



 cleaning.log.missing <- recode.missing(raw.main,c(cols_missing_01,cols_missing_yn),missing_data,
                                        "correction due to old questionnaire version") %>%
 distinct() %>%
 filter(old.value %!=na% new.value)
 int_cols_main  <- tool.survey %>% filter(type %in% c("decimal","integer") & datasheet == "main") %>% pull(name)
 cl_999s <- recode.set.NA.if(raw.main, int_cols_main, "999", "replacing 999 with NA")


 final_price <- colnames(raw.main) %>%
   str_subset(pattern = "\\_final_price")
 cols_final_price  <- tool.survey %>% filter(name %in% c(final_price) & datasheet == "main") %>% pull(name)
 cl_nan <- recode.set.NA.if(raw.main, cols_final_price, "NaN", "replacing NaN with NA")
 


 #cleaning.log <- bind_rows(cleaning.log.missing,cl_999s, cl_nan, cleaning.log)
 cleaning.log <- cleaning.log.missing

 raw.main <- raw.main %>%
   apply.changes(cleaning.log)

# Here we recode labels to variables, which appeared because something wrong went with half of KIIS submissions

# kiis_dictionary <- tool.choices %>%
#   rename("from" = "label::English",
#          "to" = "name") %>%
#   subset(select = -list_name)
# kiis_dictionary$col <- ".global"
# tf <- data.frame( from = c("TRUE","FALSE"),
#                   to = c("1","0"),
#                   col = c(".global",".global")
# )
# kiis_dictionary <- rbind(kiis_dictionary,tf)
# select_one_multiple <- tool.survey %>% filter(str_starts(tool.survey$type, "select_"))
# cols_to_recode <- c(colnames(raw.main)[grepl("/",colnames(raw.main))],select_one_multiple$name)
# 
# cleaning.log.labels <- recode.label.to.value(raw.main, cols_to_recode, kiis_dictionary$from, kiis_dictionary, "Recoding label to variable")
# 
# raw.main <- raw.main %>%
#   matchmaker::match_df(dictionary = kiis_dictionary, from = "from",
#                        to = "to",
#                        by = "col")
# #cleaning.log <- bind_rows(cleaning.log,cleaning.log.labels)
# raw.main <- raw.main %>%
#   apply.changes(cleaning.log.labels)
# 
# ### Changing location (hromada name) due to enumerator error
# location_error <- raw.main %>%
#   filter(a7_current_hromada == "UA6802005" & a2_1_enum_id =="KIIS_004" & a8_current_settlement == "other")
# location_error["a7_current_hromada"] <- "UA6802017"
# 
# cleaning.log.location <- location_error %>% select("uuid") %>% mutate(variable = "a7_current_hromada",
#                                                                       old.value="UA6802005", new.value= "UA6802017",
#                                                                       issue= "correction due to enumerator error")
# 
# 
# cleaning.log <- bind_rows(cleaning.log,cleaning.log.location)
# 
# raw.main <- raw.main %>%
#   apply.changes(cleaning.log)
# 
# ##Cleaning KIIS part of data
# 
# kiis_dictionary_part2 <- data.frame()
# kiis_dictionary_part2 <- tool.choices %>% select(name)
# colnames(kiis_dictionary_part2)[1] <- "from"
# kiis_dictionary_part2$to <- "1"
# kiis_dictionary_part2$col <- ".global"
# 
# 
# 
# multiple_choice <- tool.survey %>% filter(str_detect(type, "select_multiple"))
# multiple_choice_col <- c(multiple_choice$name)
# 
# 
# # Initialize a list to store the cleaning logs
# cleaning_logs <- list()
# cleaning_logs1 <- list()
# 
# # Iterate over each value in multiple_choice_col_values
# for (value in multiple_choice_col) {
#   multiple_choice_col_values <- strsplit(multiple_choice_col, ", ")[[value]]
#   raw.main.multiple_choice_col <- colnames(raw.main) %>%
#     str_subset(pattern = paste0(value, "(?!_other)"))
#   raw.main.multiple_choice_col_1 <- raw.main.multiple_choice_col[-1]
# 
#   cleaning.log.labels_part2 <- recode.label.to.value(raw.main, raw.main.multiple_choice_col_1, kiis_dictionary_part2$from, kiis_dictionary_part2, "correction due to different type of data")
# 
#   cleaning_logs1[[value]] <- cleaning.log.labels_part2
#   raw.main <- raw.main %>%
#     apply.changes(cleaning_logs1[[value]])
# 
#   kiis_data <- raw.main[!is.na(raw.main[, raw.main.multiple_choice_col[1]]), ]
# 
#   cols_to_replace <- names(kiis_data) %in% raw.main.multiple_choice_col_1
# 
#   kiis_data[, cols_to_replace][is.na(kiis_data[, cols_to_replace])] <- "0"
# 
#   cleaning.log.kiis_data <- recode.missing(raw.main, c(raw.main.multiple_choice_col_1), kiis_data, "correction due to different type of data") %>%
#     distinct() %>%
#     filter(old.value %!=na% new.value)
# 
#   # Store the cleaning log for each value
#   cleaning_logs[[value]] <- cleaning.log.kiis_data
# }
# 
# # Combine all cleaning logs into a single list
# cleaning_log_combined <- do.call(rbind, cleaning_logs)
# cleaning_log_combined1 <- do.call(rbind, cleaning_logs1)
# 
# cleaning_log_combined <- as.data.frame(cleaning_log_combined)
# cleaning_log_combined1 <- as.data.frame(cleaning_log_combined1)
# 
# 
# 
# raw.main <- raw.main %>%
#   apply.changes(cleaning_log_combined)
# 
# cleaning.log <- bind_rows(cleaning.log,cleaning_log_combined, cleaning_log_combined1, cleaning.log.labels)

}
###############################################################################

#-------------------------------------------------------------------------------
# 1) NO CONSENT + DUPLICATES --> deletion log
################################################################################


# check for duplicates 
ids <- raw.main$uuid[duplicated(raw.main$uuid)]
if (length(ids)>0) warning("Duplicate uuids detected: ", length(ids))
# add to deletion log
#changed here to remove only the first duplicate
deletion.log.new <- create.deletion.log(raw.main[!duplicated(raw.main$uuid),] %>% filter(uuid %in% ids),enum_colname, "Duplicate") # a brand new deletion log
rm(ids)

# check for no consent
no_consents <- raw.main %>% filter(a4_informed_consent == "no")
if (nrow(no_consents) > 0) warning("No-consent detected: ", nrow(no_consents))
# add to deletion log
if("no_consent_reasons" %in% colnames(raw.main)){
  deletion.log.no_consents <- no_consents %>% 
    mutate(reason = paste0("no consent", ifelse(is.na(no_consent_reasons), "", paste0(": ", no_consent_reasons)))) %>% select(uuid, !!sym(enum_colname), reason)
  # translate the no-consents reasons :)
  deletion.log.no_consents <- deletion.log.no_consents %>% translate.responses("reason") %>% 
    mutate(reason = str_to_lower(response.en.from.uk)) %>% select(-response.en.from.uk)
}else{
  deletion.log.no_consents <- no_consents %>% create.deletion.log(enum_colname, "no consent")
}
deletion.log.new <- rbind(deletion.log.new, deletion.log.no_consents)

### Deleting submissions due to wrong location
# 
# submissions_to_delete <- c(
# 
# )
# wrong_location <- raw.main %>%
#   filter(uuid %in% submissions_to_delete)
# deletion.log.wrong_location <- wrong_location %>% create.deletion.log(enum_colname, "wrong location")
# deletion.log.new <- rbind(deletion.log.new, deletion.log.wrong_location)

####################################################
## run this to remove duplicates and no-consents  ##

raw.main  <- raw.main[!duplicated(raw.main$uuid,fromLast=TRUE),]
raw.main  <- raw.main[!(raw.main$uuid %in% deletion.log.no_consents$uuid),]

#raw.main <- raw.main[!(raw.main$uuid %in% deletion.log.wrong_location$uuid),]
# raw.loop1 <- raw.loop1[!(raw.loop1$uuid %in% deletion.log.new$uuid),]
# raw.loop2 <- raw.loop2[!(raw.loop2$uuid %in% deletion.log.new$uuid),]
####################################################

rm(no_consents, deletion.log.no_consents)


################ Retailers ####################################################

### Deleting submissions due to wrong location
# 
# submissions_to_delete <- c(
# 
#   
# )
# wrong_location <- raw.main %>%
#   filter(uuid %in% submissions_to_delete)
# deletion.log.wrong_location <- wrong_location %>% create.deletion.log(enum_colname, "wrong location")
# deletion.log.new <- rbind(deletion.log.new, deletion.log.wrong_location)
# 
# 
# ## run this to remove wrong_location  ##
# 
# raw.main <- raw.main[!(raw.main$uuid %in% deletion.log.wrong_location$uuid),]
# 


rm(no_consents, deletion.log.no_consents, cleaning_log_combined, cleaning_log_combined1, cleaning.log.location, cleaning.log.labels)
###############################################################################


#-------------------------------------------------------------------------------
# 2) AUDIT CHECKS
################################################################################
### Survey durations 
#
## FOR POL: WE WERE NOT GIVEN ACCESS TO AUDIT FILES, so no audit logs will be found
#
# audits <- load.audit.files(dir.audits, uuids = raw.main$uuid, track.changes = F) 
# 
# # save.image("environment.RData")
# # load("environment.RData")
# 
# if(nrow(audits) == 0) {audits.summary <- tibble(uuid = raw.main$uuid, tot.rt = NA)
# }else{
#   audits.summary <- audits %>% 
#     group_by(uuid) %>% 
#     group_modify(~process.uuid(.x))
# }
# 
# data.audit <- raw.main %>% 
#   mutate(duration_mins = difftime(as.POSIXct(end), as.POSIXct(start), units = 'mins'),
#          num_NA_cols = rowSums(is.na(raw.main)),
#          num_dk_cols = rowSums(raw.main == "dont_know", na.rm = T),
#          num_other_cols = rowSums(!is.na(raw.main[str_ends(colnames(raw.main), "_other")]), na.rm = T))  %>%
#  select(uuid, !!sym(enum_colname), start, end, duration_mins, num_NA_cols, num_dk_cols, num_other_cols)
# 
# audits.summary <- data.audit %>% 
#   left_join(audits.summary, by="uuid") %>% select(-contains("/")) %>% 
#   relocate(uuid, duration_mins, num_NA_cols, num_dk_cols, num_other_cols, tot.rt) %>% 
#   arrange(duration_mins)
# 
# write.xlsx(audits.summary, "output/checking/audits/audits_summary.xlsx")
# 
# # follow up with FPs if there are surveys under 10 minutes or above 1 hour
# survey_durations_check <- audits.summary %>% filter(duration_mins < 10 | duration_mins > 60)
# if(nrow(survey_durations_check) > 0){
#   write.xlsx(survey_durations_check, "output/checking/audits/survey_durations.xlsx",
#              zoom = 90, firstRow = T)
# }else cat("\nThere are no survey durations to check :)")

## Soft duplicates (less than 12 different columns?)


res.soft_duplicates <- find.similar.surveys(raw.main, tool.survey, uuid = "uuid") %>% 
  filter(number_different_columns <= 12)

if(nrow(res.soft_duplicates) > 0){
  write.xlsx(res.soft_duplicates, make.filename.xlsx(paste0("output/", JMMI_variable, "/checking/audit/"), "soft_duplicates"))
}else cat("\nThere are no soft duplicates to check :)")

rm(audits, data.audit)


## Soft duplicates (less than 10 different columns?)

res.soft_duplicates <- find.similar.surveys(raw.main, tool.survey, uuid = "uuid") %>% 
  filter(number_different_columns <= 3)

if(nrow(res.soft_duplicates) > 0){
  write.xlsx(res.soft_duplicates, make.filename.xlsx(paste0("output/", JMMI_variable, "/checking/audit/"), "soft_duplicates"))
}else cat("\nThere are no soft duplicates to check :)")

rm(audits, data.audit)

#-------------------------------------------------------------------------------
if (JMMI_variable == "Customers") {
# DECISIONs:
# interviews that were too fast and decided to be removed based on these uuids:
ids <- c(

)
deletion.log.too.fast <- create.deletion.log(raw.main %>% filter(uuid %in% ids),
                                             enum_colname, "Survey duration deemed too fast.")
# soft duplicates to remove:
ids <- c(
  "0505b714-bef4-444a-ac6d-7e7771009fb3"
)
deletion.log.softduplicates <- create.deletion.log(raw.main %>% filter(uuid %in% ids),
                                                   enum_colname, "Soft duplicate")
# incomplete submissions to remove:
ids <- c(
 
)
deletion.log.incomplete <- create.deletion.log(raw.main %>% filter(uuid %in% ids), enum_colname, "Incomplete submission")

deletion.log.audits <- bind_rows(deletion.log.too.fast, deletion.log.softduplicates, deletion.log.incomplete)
deletion.log.new <- bind_rows(deletion.log.new, deletion.log.audits)

#################################################
##   removing fast submissions and duplicates  ##
raw.main  <- raw.main[! (raw.main$uuid  %in% deletion.log.audits$uuid),]
# raw.loop1 <- raw.loop1[!(raw.loop1$uuid %in% deletion.log.audits$uuid),]
# raw.loop2 <- raw.loop2[!(raw.loop2$uuid %in% deletion.log.audits$uuid),]
#################################################

rm(ids, deletion.log.too.fast, deletion.log.softduplicates)
}
if (JMMI_variable == "Retailers") {
  # DECISIONs:
  # interviews that were too fast and decided to be removed based on these uuids:
  ids <- c(
    
  )
  deletion.log.too.fast <- create.deletion.log(raw.main %>% filter(uuid %in% ids),
                                               enum_colname, "Survey duration deemed too fast.")
  # soft duplicates to remove:
  ids <- c(
    "0505b714-bef4-444a-ac6d-7e7771009fb3"
  )
  deletion.log.softduplicates <- create.deletion.log(raw.main %>% filter(uuid %in% ids),
                                                     enum_colname, "Soft duplicate")
  # incomplete submissions to remove:
  ids <- c(
    
  )
  deletion.log.incomplete <- create.deletion.log(raw.main %>% filter(uuid %in% ids), enum_colname, "Incomplete submission")
  
  deletion.log.audits <- bind_rows(deletion.log.too.fast, deletion.log.softduplicates, deletion.log.incomplete)
  deletion.log.new <- bind_rows(deletion.log.new, deletion.log.audits)
  
  #################################################
  ##   removing fast submissions and duplicates  ##
  raw.main  <- raw.main[! (raw.main$uuid  %in% deletion.log.audits$uuid),]
  # raw.loop1 <- raw.loop1[!(raw.loop1$uuid %in% deletion.log.audits$uuid),]
  # raw.loop2 <- raw.loop2[!(raw.loop2$uuid %in% deletion.log.audits$uuid),]
  #################################################
  
  rm(ids, deletion.log.too.fast, deletion.log.softduplicates)
}

#-------------------------------------------------------------------------------
# 3) LOOP INCONSITENCIES + SPATIAL CHECKS
################################################################################

## check for inconsistency in loops:

#counts_loop1 <- raw.loop1 %>% 
#group_by(uuid) %>% 
#summarize(loop1_count = n())
#loop_counts_main <- raw.main %>% select(uuid, !!sym(enum_colname), date_interview, hh_size) %>% left_join(counts_loop1) %>% 
#mutate(hh_size = ifelse(hh_size == "999", NA, as.numeric(hh_size))) %>% 
#filter(hh_size > 1 & loop1_count %!=na% (hh_size - 1))
#
#if(nrow(loop_counts_main) > 0){
## look at the loop_counts (perhaps just send a screenshot to AO)
#loop_counts_main %>% view(title = "Inconsistencies in loop1")
## find loops for inconsistent uuids:
#inconsistent_loop1 <- loop_counts_main %>% left_join(raw.loop1)
#}else{ cat("No inconsistencies with loops! :)") }
#
#
## DECISION: what to do with these inconsistencies?
#
#ids_to_clean <- c(
## put here the uuids for which hh_size should be adjusted
## "5a161089-3d5f-4994-868e-62f2c4d91dbe" # POL
#"5745d0f1-3f8f-4eb6-87a2-1ae7832f6216",
#"9d568420-8c2d-44fc-a9ba-62a13ec1ba45"
#)
#loop_indexes_to_delete <- c(
## put here the loop indexes which should be removed
#)
#ids_to_delete <- c(
## uuids of submissions that will be totally removed
## "0383549a-71f5-4657-87a6-041dc457d36b"  # POL
#)
#
#cleaning.log.loop_inconsitency <- loop_counts_main %>% 
#filter(uuid %in% ids_to_clean) %>% 
#mutate(variable = "hh_size", loop_index = NA,
#old.value = as.character(hh_size), new.value = ifelse(is.na(loop1_count),"1",as.character(loop1_count + 1)), issue = "Inconsistency in number of entries in hh loop") %>% 
#select(any_of(CL_COLS))
#
#deletion.log.loop_inconsistency <- tibble()
#dl_inconsistency1 <- create.deletion.log(pull.raw(loop_indexes_to_delete), 
#enum_colname, "Inconsistency in number of entries in hh loop")
#dl_inconsistency2 <- create.deletion.log(pull.raw(ids_to_delete), 
#enum_colname, "Inconsistency in number of entries in hh loop") %>% 
#mutate(loop_index = NA)
#deletion.log.loop_inconsistency <- rbind(dl_inconsistency1, dl_inconsistency2)
#
#####################################################
### run this to delete/clean entries               ##
#raw.loop1 <- raw.loop1[!raw.loop1$loop_index %in% dl_inconsistency1$uuid,]
###                                                ##
#raw.main  <- raw.main[! (raw.main$uuid  %in% dl_inconsistency2$uuid),]
#raw.loop1 <- raw.loop1[!(raw.loop1$uuid %in% dl_inconsistency2$uuid),]
#raw.loop2 <- raw.loop2[!(raw.loop2$uuid %in% dl_inconsistency2$uuid),]
###                                                ##
#raw.main <- raw.main %>% apply.changes(cleaning.log.loop_inconsitency)
#####################################################
#
#deletion.log.new <- bind_rows(deletion.log.new, deletion.log.loop_inconsistency) %>% 
#relocate(loop_index, .after = uuid)
#cleaning.log <- cleaning.log.loop_inconsitency   # a brand new cleaning log
#
#rm(ids_to_clean, loop_indexes_to_delete, counts_loop1)
#
##-------------------------------------------------------------------------------
#
### GPS checks
#
#
### In case any needed 
## run this section always (if POL) ------------------------  -------------------
#
#if(country == "Poland"){
#if(!"admin2" %in% colnames(raw.main)){ stop("admin2 column is missing from data")
#}else{
## find if there are any "geopoint" variables in this data:
#gps_vars <- tool.survey %>% filter(type == "geopoint") %>% pull(name)
#if(length(gps_vars) == 0){ stop("there are no geopoint variables in tool.survey.")
#} else if(length(gps_vars) > 1){
#warning("Found more than one geopoint variable in tool.survey: ", paste(gps_vars, collapse = ", "),"\nOnly the first one will be used to match admin2!")
#gps_vars <- gps_vars[1]
#} else{
#gps_cols <- raw.main %>% select(contains(gps_vars)) %>% names   # there should be 4 of these
#if(length(gps_cols) < 2) {stop("columns with geopoint data were not found in data")}
#}
#}
#sf_use_s2(TRUE)
#admin2_boundary <- st_read(dsn = "resources/pol_admbnda_adm2.json") %>% 
#mutate(ADM2_PCODE = ifelse(is.na(ADM2_PCODE), NA, paste0("20", str_replace(ADM2_PCODE, "PL", "POL"))))  # this is to standardize geo data to our pcode format
#
## TODO additional check for low precision??
#
#collected_pts <- raw.main %>% filter(!is.na(!!sym(gps_cols[1]))) %>% 
#select(uuid, !!sym(enum_colname), admin2, contains(gps_vars)) %>% 
#left_join(admin2.list) %>%
#rename(selected_admin2_label = label, selected_admin2 = admin2) %>% 
#mutate(selected_admin2_label = str_to_title(selected_admin2_label, "pl")) 
#
#collected_sf <- collected_pts %>% st_as_sf(coords = paste0(gps_vars, c("_longitude", "_latitude")), crs = "+proj=longlat +datum=WGS84")
#
#sf_use_s2(FALSE)
#
#spatial_join <- st_join(collected_sf, admin2_boundary, join = st_within) %>% 
#select(-geometry) %>% select(-contains(gps_vars)) %>% 
#mutate(GPS_MATCH = ifelse(is.na(ADM2_PCODE), "outside POL", ifelse(ADM2_PCODE == selected_admin2, "match", "WRONG")))
#
#if(any(spatial_join$GPS_MATCH != "match")){
#
#check_spatial <- tibble(spatial_join) %>% 
#rename(within_admin2 = ADM2_PCODE, within_admin2_label = ADM2_PL) %>% 
#left_join(collected_pts %>% select(uuid, contains(gps_vars))) %>% 
#select(uuid, !!sym(enum_colname), GPS_MATCH, contains(gps_vars), contains("admin2")) %>% 
#filter(GPS_MATCH != "match")
## %>% view
#
#write.xlsx(check_spatial, make.filename.xlsx("output/checking/audit/", "gps_checks"), overwrite = T)
#rm(collected_sf, spatial_join, admin2_boundary)
#
#}else cat("All GPS points are matching their selected poviat :)")
#}
#
##-------------------------------------------------------------------------------
#
## run this section only if there is need to recode spatial data 
#
#cleaning.log.spatial <- tibble()
#
#if(country == "Poland"){
## for gps points outside Poland, set them to NA immediately
#check_outside_POL <- check_spatial %>% filter(GPS_MATCH == "outside POL")
#
#cleaning.log.spatial <- rbind(cleaning.log.spatial, check_outside_POL %>% 
#recode.set.NA.regex(gps_cols, ".*", "GPS point is falling outside of Poland"))
#
## how about the other points?
#check_wrong_admin2 <- check_spatial %>% filter(GPS_MATCH == "WRONG")
#}
#
## DECISION: what to do with these inconsistencies:
#
## for these uuids, admin2 will be recoded to match the geolocation:
#ids <- c(
### POL
#"1f43f45b-2dd8-4553-9d1d-a50dc79b841e",
#"88ff086f-32dc-48c2-b791-b65e85246fa9",    
#"e1cb77b8-abfe-485f-b3cd-709baa88c419"
###
#)
#cl.spatial_recode <- check_wrong_admin2 %>% filter(uuid %in% ids) %>%  
#mutate(old.value = selected_admin2, new.value = within_admin2, variable = "admin2", issue = "Enumerator selected wrong poviat by mistake") %>% 
#select(any_of(CL_COLS))
#
## for these uuids, remove geolocation data
#ids <- c(
#
#)
#cl.spatial_remove_geo <- recode.set.NA.regex(pull.raw(ids), gps_cols, ".*", "Mismatch between selected admin2 and GPS location")
#
#cleaning.log.spatial <- rbind(cleaning.log.spatial, cl.spatial_recode, cl.spatial_remove_geo)
#
## do we remove any suspicious surveys because of GPS mismatch?
#ids_remove <- c(
#
#)
#deletion.log.new <- rbind(deletion.log.new,
#create.deletion.log(pull.raw(ids_remove), enum_colname, "Mismatch between selected admin2 and GPS location"))
#
## ------------------------------------
#raw.main <- raw.main %>% apply.changes(cleaning.log.spatial)
#cleaning.log <- bind_rows(cleaning.log, cleaning.log.spatial)
#
##################################################
#raw.main  <- raw.main[! (raw.main$uuid  %in% deletion.log.new$uuid),]
#raw.loop1 <- raw.loop1[!(raw.loop1$uuid %in% deletion.log.new$uuid),]
#raw.loop2 <- raw.loop2[!(raw.loop2$uuid %in% deletion.log.new$uuid),]
##################################################
#
## deletion log should be now finalized
#
## Save deletion.log file
#deletion.log.whole <- rbind(deletion.log.previous, deletion.log.new)
#write.xlsx(deletion.log.whole, make.filename.xlsx("output/deletion_log/", "deletion_log", no_date = T), overwrite=T)
#
#
#-------------------------------------------------------------------------------
# 3) OTHERS AND TRANSLATIONS
################################################################################

other.db <- get.other.db()

other.db.main  <- other.db[other.db$name %in% colnames(raw.main),]
#Skipping this since there are no loops
#other.db.loop2 <- other.db[other.db$name %in% colnames(raw.loop2),]


other.responses <- find.other.responses(raw.main, other.db.main)

#Skipping automatic translations for now 
other.responses.j <- other.responses #%>% translate.responses
other.responses.j[["response.en"]] <- NA

#Making other requests without translations, other.responses instead of other.responses.j
save.other.requests(create.translate.requests(other.db, other.responses.j, is.loop = F),
                    make.short.name("other_requests"), use_template = F)

# ------------------------------------------------------------------------------

# THERE IS NOTHING TO TRANSLATE UNTIL WE GET SOME DATA FROM LOOP2

## translate all text questions, but skip these columns:
#trans_cols_to_skip <- c("identification", "no_consent_reasons")
#trans.db <- get.trans.db() %>% filter(!name %in% trans_cols_to_skip)
#
## trans.db.main <- trans.db[trans.db$name %in% colnames(raw.main),]
## trans.responses.main <- rbind(find.responses(raw.main, trans.db.main))
#
## there are actually no text questions in main or loop1
#
## trans.db.loop2 <- trans.db[trans.db$name %in% colnames(raw.loop2),]
## trans.responses.loop2 <- find.responses(raw.loop2, trans.db.loop2, is.loop = T)
#
#trans.responses <- rbind(trans.responses.loop2)
#trans.responses.j <- trans.responses %>% translate.responses
#
#save.trans.requests(create.translate.requests(trans.db, trans.responses.j, is.loop = T), make.short.name("translate_requests"))
#
#


###########  Retailers ########################################################

#To translate additional columns
if (JMMI_variable == "Retailers") {

to_translatex2_1 <- raw.main %>% select(uuid, x2_1_paymet_limitation) %>% mutate(variable = "x2_1_paymet_limitation") %>%
  filter(!is.na(x2_1_paymet_limitation))
colnames(to_translatex2_1)[2] <- "old.value"
to_translateu3_7 <- raw.main %>% select(uuid, u3_7_nfi_supplier_another_country) %>% mutate(variable = "u3_7_nfi_supplier_another_country") %>%
  filter(!is.na(u3_7_nfi_supplier_another_country))
colnames(to_translateu3_7)[2] <- "old.value"
to_translateu3_4 <- raw.main %>% select(uuid, u3_4_nfi_supply_another_country) %>% mutate(variable = "u3_4_nfi_supply_another_country")%>%
  filter(!is.na(u3_4_nfi_supply_another_country))
colnames(to_translateu3_4)[2] <- "old.value"
to_translateu2_7 <- raw.main %>% select(uuid, u2_7_food_supplier_another_country) %>% mutate(variable = "u2_7_food_supplier_another_country")%>%
  filter(!is.na(u2_7_food_supplier_another_country))
colnames(to_translateu2_7)[2] <- "old.value"
to_translateu2_4 <- raw.main %>% select(uuid, u2_4_food_supply_another_country) %>% mutate(variable = "u2_4_food_supply_another_country")%>%
  filter(!is.na(u2_4_food_supply_another_country))
colnames(to_translateu2_4)[2] <- "old.value"
to_translate <- rbind(to_translatex2_1, to_translateu3_7, to_translateu3_4, to_translateu2_7, to_translateu2_4)
write.xlsx(to_translate, "output/Retailers/checking/requests/to_translate.xlsx")

rm(to_translatex2_1, to_translateu3_7, to_translateu3_4, to_translateu2_7, to_translateu2_4, to_translate)
}

###############################################################################
# ------------------------------------------------------------------------------
# AFTER RECEIVING FILLED-OUT OTHER requests:
#cleaning.log <- data.frame() #### Please remove if you hve loops
cleaning.log.other <- data.frame()
or.request <- load.requests(dir.requests,  make.short.name("other_requests", no_date = T), sheet = "Sheet2") 
or.edited  <- load.requests(dir.responses, make.short.name("other_response", no_date = T),
                            sheet = "Sheet2", validate = T) # specify Sheet2 because the first one is a readme
or.edited$loop_index <- NA   #added this to overcome the problem of not finding loop_index column
if (nrow(or.request) != nrow(or.edited)) stop("Number of rows differs between or.request and or.edited!")

or.true.and.recode <- filter(or.edited, check == 1)
if (nrow(or.true.and.recode) > 0){
  cat(paste0("Multiple columns selected in ", nrow(or.true.and.recode)," or.edited entries:\n",
             paste0(or.true.and.recode %>% pull(uuid), collapse = "\n")), sep = "\n")
  if(any(or.true.and.recode$ref.type != "select_multiple")) stop("One of those is not a select_multiple!!!")
  # if(any(!is.na(or.true.and.recode$loop_index))) stop("Deal with loop code INSTEAD of UUID-LOOP-INDEX")
  issue <- "Recoding other response"
  for(r in 1:nrow(or.true.and.recode)){
    x <- or.true.and.recode[r,]
    # get list of choices from other response
    if (str_detect(x$existing.v, ";")) {
      choices <- str_trim(str_split(x$existing.v, ";")[[1]])
    } else {
      choices <- str_trim(str_split(x$existing.v, "\r\n")[[1]])
    }
    choices <- choices[choices!=""]
    if(is.na(x$loop_index)){
      old.value <- as.character(raw.main[raw.main$uuid==x$uuid[1], x$ref.name[1]])
    } else {
      old.value <- as.character(raw.loop1[raw.loop1$loop_index==x$loop_index[1], x$ref.name[1]])
    }
    
    l <- str_split(old.value, " ")[[1]]
    # add to the cleaning log each choice in the other response
    for (choice in choices){
      # set corresponding variable to "1" if not already "1"
      list.name <- get.choice.list.from.name(x$ref.name)
      new.code <- filter(tool.choices, list_name==list.name & !!sym(label_colname)==choice)
      if (nrow(new.code)!=1){
        warning(paste0("Choice is not in the list. UUID: ", x$uuid,"; recode.into: ", choice))
        return("err")
      }
      variable.name <- paste0(x$ref.name, "/", new.code$name)
      if(is.na(x$loop_index)){
        if (variable.name %in% colnames(raw.main)){
          old.boolean <- raw.main[[variable.name]][raw.main$uuid==x$uuid[1]]
        } else warning("Column not found")
      } else {
        if (variable.name %in% colnames(raw.loop1)){
          old.boolean <- raw.loop1[[variable.name]][raw.loop1$loop_index==x$loop_index[1]]
        } else warning("Column not found")
      }
      df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=variable.name, issue=issue,
                       old.value=old.boolean, new.value="1")
      cleaning.log.other <<- rbind(cleaning.log.other, df)
      l <- unique(c(l, new.code$name))
    }
    # update cumulative variable
    new.value <- paste(sort(l), collapse=" ")
    df <- data.frame(uuid=x$uuid, loop_index=x$loop_index, variable=x$ref.name, issue=issue,
                     old.value=old.value, new.value=new.value)
    cleaning.log.other <<- rbind(cleaning.log.other, df)
  }
  or.edited <- or.edited %>% filter(check == 2)
}

or.true <- filter(or.edited, !is.na(true.v))
or.recode <- filter(or.edited, !is.na(existing.v))
or.remove <- filter(or.edited, !is.na(invalid.v))
#cleaning.log.other <- subset(cleaning.log.other, select = -loop_index)

# 1) handle invalid
print(paste("Number of responses to be deleted:", nrow(or.remove)))
if (nrow(or.remove)>0){
  for (r in 1:nrow(or.remove)) {
    if(is.na(or.remove$loop_index[r])){

      add.to.cleaning.log.other.remove(raw.main, or.remove[r,])
#      recode.others(raw.main, or.remove[r,])
    } else{
      add.to.cleaning.log.other.remove(raw.loop1, or.remove[r,])
    } 
  }
} 


# 2) handle recoding
print(paste("Number of responses to be recoded:", nrow(or.recode)))
if (nrow(or.recode)>0){
  for (r in 1:nrow(or.recode)) {
    if(is.na(or.recode$loop_index[r])){
      add.to.cleaning.log.other.recode(raw.main, or.recode[r,])
    } else {
      add.to.cleaning.log.other.recode(raw.loop1, or.recode[r,])
    }
  }
}

# 3) handle true\
or.true <- rbind(or.true, or.true.and.recode)
print(paste("Number of responses to be translated:", nrow(or.true)))
t <- or.true %>%
  mutate(issue = "Translating other responses") %>%
  rename(variable=name, old.value=response.uk, new.value=true.v) %>%
  select(uuid, variable,issue, old.value, new.value)
####################### Customers ############################################
if (JMMI_variable == "Customers") {
cleaning.log.other <- rbind(cleaning.log.other, t)

raw.main <- raw.main %>% 
  apply.changes(cleaning.log.other)
}
##############################################################################

###################  Retailers ################################################
if (JMMI_variable == "Retailers") {
  to_translate <- read_xlsx(paste0("output/", JMMI_variable, "/checking/responses/to_translate.xlsx"))
  
 cleaning.log.other <- rbind(cleaning.log.other, t, to_translate)
 
 raw.main <- raw.main %>% 
   apply.changes(cleaning.log.other)
}
###############################################################################

#################################################################
#raw.main <- raw.main  %>%  apply.changes(cleaning.log.other_main)
#raw.loop2 <- raw.loop2 %>% apply.changes(cleaning.log.other_loop2, is.loop = T)
#################################################################

cleaning.log <- bind_rows(cleaning.log, cleaning.log.other #, cleaning.log.other_loop2
) 

### ADD translation cleaning


#-------------------------------------------------------------------------------
# 4) LOGIC CHECKS
################################################################################

## 4A) direct cleaning
#
#cleaning.log.checks.direct <- tibble()
#
### replace all '999's with NA:
#int_cols_main  <- tool.survey %>% filter(type == "decimal" & datasheet == "main") %>% pull(name)
##int_cols_loop1 <- tool.survey %>% filter(type == "integer" & datasheet != "main") %>% pull(name)
##
##check_hh_size_999 <- raw.main %>% 
#  #mutate(flag = ifelse(hh_size == "999",T,F),
#         #variable = "hh_size",
#         #loop_index = NA,
#         #issue = "Change 999 in HH size to 1",
#         #old.value = hh_size,
#         #new.value = "1") %>% 
#  #filter(flag) %>% select(uuid,loop_index,variable,old.value,new.value,issue,enumerator_num)
##raw.main <- raw.main %>% apply.changes(check_hh_size_999)
#cl_999s <- bind_rows(recode.set.NA.if(raw.main, int_cols_main, "999", "replacing 999 with NA"))
##                      recode.set.NA.if(raw.loop1, int_cols_loop1, "999", "replacing 999 with NA"))
#cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_999s)
#
#
######
#raw.main  <- raw.main  %>% apply.changes(cleaning.log.checks.direct)
##raw.loop1 <- raw.loop1 %>% apply.changes(cleaning.log.checks.direct, is.loop = T)
##raw.loop2 <- raw.loop2 %>% apply.changes(cleaning.log.checks.direct, is.loop = T)
#
##cleaning.log.checks.direct <- rbind(cleaning.log.checks.direct,check_hh_size_999)
#cleaning.log <- bind_rows(cleaning.log, cleaning.log.checks.direct)


# ------------------------------------------------------------------------------

## 4B) checks for followups
#
#checks_followups <- tibble()
#
## if hh member is a child/grandchild, then their age should be lower than respondent's
## and vice versa if they are parent/grandparent
#check_hh_relations <- raw.loop1 %>% select(uuid, loop_index, age_hh_member, relation_hh_member) %>% 
#left_join(raw.main %>% select(uuid, resp_age, !!sym(enum_colname), today, place_of_interview)) %>% mutate(resp_age = as.numeric(resp_age), age_hh_member = as.numeric(age_hh_member)) %>% 
#filter(resp_age < age_hh_member & relation_hh_member %in% c("child", "grandchild") | resp_age > age_hh_member & relation_hh_member %in% c("parent", "grandparent"))
#
#checks_followups <- rbind(checks_followups,
#make.logical.check.entry(check_hh_relations, 1,  c("age_hh_member", "relation_hh_member"), 
#"HH member is younger/older than respondent, but is their parent/child",
#cols_to_keep = c("today", enum_colname, "place_of_interview", "resp_age"), T))
#
## if currently or previously retired, check if age is below 40
#check_retired <- raw.main %>% filter(as.numeric(resp_age) < 40 & (work_coa == "retired" | resp_activity == "retired"))
#
#checks_followups <- rbind(checks_followups,
#make.logical.check.entry(check_retired, 2,  c("work_coa", "resp_activity"), 
#"Respondent is under 40 years old and reported being retired",
#cols_to_keep = c("today", enum_colname, "place_of_interview", "resp_age"), T))
#
## if age is > 60, check if they are a student or intern
#check_students_interns <- raw.main %>% filter(resp_age >= 60 & (work_coa %in% c("student", "intern") | resp_activity == "student"))
#
#checks_followups <- rbind(checks_followups,
#make.logical.check.entry(check_students_interns, 3,  c("work_coa", "resp_activity"), 
#"Respondent is over 60 years old and reported being a student or intern",
#cols_to_keep = c("today", enum_colname, "place_of_interview", "resp_age"), T))
#
## if asked to return to Ukraine by employer, check if unemployed/retired
#check_returned_employer <- raw.main %>% filter(str_detect(temp_return_reason, "asked_empl") & 
#work_coa %in% c("retired", "unemployed" ))
#
#checks_followups <- rbind(checks_followups,
#make.logical.check.entry(check_returned_employer, 4,  c("work_coa", "temp_return_reason"), 
#"Respondent stated they returned to Ukraine after employer's request, bus is currently unemployed/retired",
#cols_to_keep = c("today", enum_colname, "place_of_interview", "resp_age"), T))
#
#####
#create.follow.up.requests(checks_followups, make.short.name(paste0("follow-up_requests_",dctime_short,".xlsx"), no_date = T))
#

# ------------------------------------------------------------------------------
# AFTER RECEIVING filled followup requests:

#fu.edited <- load.requests(dir.responses, make.short.name("follow-up", no_date = T)) %>%
#  mutate(modified = !is.na(invalid) | (!is.na(new.value) & new.value != old.value),
#         check = !is.na(invalid) & !is.na(new.value)) 
#
#fu.check <- fu.edited %>% filter(check)
#if(nrow(fu.check)>0) warning("Two entries in invalid and new.value columns:\n", paste0(fu.check$uuid, collapse = "\n"))
#
## go ahead only if no warnings found above...
#fu.edited <- fu.edited %>% filter(modified) %>%
#  mutate(new.value = ifelse(!is.na(invalid) & invalid == "yes", NA, new.value),
#         issue = ifelse(!is.na(explanation), paste0("Information followed up with field team: ", explanation), issue))
#
#if (nrow(fu.edited)> 0) {
#  # check that new.value for select_one and select_multiple is one of the existing option
#  list.options <- tool.choices %>% filter(list_name %in% get.choice.list.from.name(fu.edited$variable)) %>% 
#    group_by(list_name) %>% summarise(options=paste0(paste0(name, collapse=";"), ";"))
#  fu.check.all <- fu.edited %>%
#    left_join(select(tool.survey, name, q.type, list_name), by=c("variable"="name")) %>%
#    left_join(list.options, by="list_name")
#  if (nrow(fu.check.all)!=nrow(fu.edited)) stop("Something went wrong with left_join")
#}
#
## recode select_multiples
#fu.multiple <- fu.edited %>% filter(get.type(variable) == "select_multiple")
#
#if(any(is.na(fu.multiple$invalid))) stop("Select multiples cannot be invalid. Set them to 0 instead!")
#cleaning.log.followups_multiple <- tibble()
#
#if(nrow(fu.multiple)>0){
#  main_row <- pull.raw(ifelse(is.na(fu.multiple[1,]$loop_index), fu.multiple[1,]$uuid, fu.multiple[1,]$loop_index))
#  for(r in 1:nrow(fu.multiple)){
#    x <- fu.multiple[r,]
#    if(is.na(x$loop_index)){
#      if(main_row$uuid %!=na% x$uuid) main_row <- pull.raw(x$uuid)
#    } else{
#      if (main_row$loop_index %!=na% x$loop_index) {
#        main_row <- pull.raw(x$loop_index)
#      }
#    } 
#    cummulative_variable  <-  str_split(x$variable, "/", simplify = T)[1]
#    cchoice  <-  str_split(x$variable, "/", simplify = T)[2]
#    # TODO: improve recoding here (use group_by and add/remove.choices) to not have to apply changes in every iteration
#    if(x$new.value == "0"){
#      cl <- recode.multiple.remove.choices(main_row, cummulative_variable, cchoice, issue = x$issue)
#      main_row <- main_row %>% apply.changes(cl)
#      cleaning.log.followups_multiple <- rbind(cleaning.log.followups_multiple, cl)
#    }else{
#      cl <- recode.multiple.add.choices(main_row, cummulative_variable, cchoice, issue = x$issue)
#      main_row <- main_row %>% apply.changes(cl)
#      cleaning.log.followups_multiple <- rbind(cleaning.log.followups_multiple, cl)
#    }
#  }
#}
#cleaning.log.followups <- fu.edited %>% filter(get.type(variable) != "select_multiple") %>% select(any_of(CL_COLS))
#cleaning.log.followups <- rbind(cleaning.log.followups, cleaning.log.followups_multiple)
#
## apply changes
#raw.main  <- raw.main  %>% apply.changes(cleaning.log.followups %>% filter(variable %in% names(raw.main)))
#raw.loop1 <- raw.loop1 %>% apply.changes(cleaning.log.followups %>% filter(variable %in% names(raw.loop1)))
#raw.loop2 <- raw.loop2 %>% apply.changes(cleaning.log.followups %>% filter(variable %in% names(raw.loop2)))
#
#cleaning.log <- bind_rows(cleaning.log, cleaning.log.followups)
#
# ------------------------------------------------------------------------------

#############################################################################################################
# 5) Outliers
#############################################################################################################
# save.image(file = "Environment.RData")
# load("Environment.RData")

################  Retailers ###################################################
if (JMMI_variable == "Retailers") {
prices <- c(
  "b3_bread_price",
  "c3_eggs_price",
  "d3_milk_price",
  "e3_potatoes_price",
  "f3_carrots_price",
  "g3_onions_price",
  "h3_cabbage_price",
  "i3_chicken_price",
  "j3_oil_price",
  "k3_flour_price",
  "l3_rice_price",
  "m3_buckwheat_price",
  "n3_water_price",
  "y3_cereal_porridge_price",
  "o3_diapers_price",
  "p3_body_soap_price",
  "r3_laundry_soap_price",
  "q3_powder_price",
  "s3_toothpaste_price",
  "t3_pads_price"
)
prices_final <- paste(prices,"_final_price",sep="")




cleaning.log.outliers <- data.frame()
# # define columns to check for outliers
#
cols.integer_main <- filter(tool.survey, type %in% c("decimal","integer"))
cols.integer_raw.main <- cols.integer_main[cols.integer_main$name %in% colnames(raw.main),] %>% pull(name)
cols.integer_raw.main <- cols.integer_raw.main[!cols.integer_raw.main %in% prices]
cols.integer_raw.main <- c(cols.integer_raw.main,prices_final)

n.sd <- 3

res.outliers_main <- data.frame()
# # res.outliers_loop1 <- data.frame()
df.all <- data.frame()
# #------------------------------------------------------------------------------------------------------------
# # [MAIN SHEET] -> detect outliers
# 
raw.main.outliers <- raw.main %>%
  select("uuid", cols.integer_raw.main) %>%
  mutate_at(cols.integer_raw.main, as.numeric)
#
# # Outliers per country

for (col in cols.integer_raw.main) {
  print(col)
  values <- raw.main.outliers %>%
    filter(!!sym(col) %_>_% 0) %>%
    rename(value=col) %>%  select(uuid, value) %>%
    mutate(value.log=log10(value)) %>%  mutate(variable=col) %>%
    mutate(is.outlier.log = (value > mean(value) + n.sd*sd(value)) |   ##change is.outlier.log to is.outlier.lin
             (value < mean(value) - n.sd*sd(value)),
           is.outlier.lin = (value.log > mean(value.log) + n.sd*sd(value.log)) |
             (value.log < mean(value.log) - n.sd*sd(value.log)))
  values <- filter(values, is.outlier.log) %>%  select(uuid, variable, value)
  if (nrow(values)>0) print(paste0(col, ": ", nrow(values), " outliers detected"))
  res.outliers_main <- rbind(res.outliers_main, values)
}



# Output requests to check
res.outliers_main <- res.outliers_main %>%
  mutate(issue = "Outliers",
         loop_index = NA,
         new.value = NA,
         explanation=NA) %>%
  rename("old.value"=value) %>%
  select(uuid,loop_index,variable,issue,old.value,new.value,explanation)

cleaning.log.outliers <- rbind(cleaning.log.outliers,res.outliers_main)
cleaning.log.outliers$check.id <- 1
save.follow.up.requests(cleaning.log.outliers,"JMMI_Retailers_R15_outliers_log_requests.xlsx")

################################################################################
write.xlsx(raw.main, "output/Retailers/data_log/data.xlsx")
}

################### Customers #################################################
if (JMMI_variable == "Customers") {
  # save.image(file = "Environment.RData")
  # load("Environment.RData")
  
  cleaning.log.outliers <- data.frame()
  # define columns to check for outliers
  
  cols.integer_main <- filter(tool.survey, type == "decimal")
  cols.integer_raw.main <- cols.integer_main[cols.integer_main$name %in% colnames(raw.main),] %>% pull(name)
  # cols.integer_raw.loop1 <- cols.integer_main[cols.integer_main$name %in% colnames(raw.loop1),] %>% pull(name)
  
  # cols <- filter(tool.survey, str_starts(name, "G_3")) %>% pull(name)
  
  n.sd <- 2
  
  
  
  res.outliers_main <- data.frame()
  # res.outliers_loop1 <- data.frame()
  df.all <- data.frame()
  #------------------------------------------------------------------------------------------------------------
  # [MAIN SHEET] -> detect outliers 
  
  raw.main.outliers <- raw.main %>%
    select("uuid", cols.integer_raw.main) %>%
    mutate_at(cols.integer_raw.main, as.numeric)
  
  # Outliers per country
  
  for (col in cols.integer_raw.main) {
    values <- raw.main.outliers %>% 
      filter(!!sym(col) %_>_% 0) %>% 
      rename(value=col) %>%  select(uuid, value) %>% 
      mutate(value.log=log10(value)) %>%  mutate(variable=col) %>% 
      mutate(is.outlier.lin = (value > mean(value) + n.sd*sd(value)) |
               (value < mean(value) - n.sd*sd(value)),
             is.outlier.log = (value.log > mean(value.log) + n.sd*sd(value.log)) |
               (value.log < mean(value.log) - n.sd*sd(value.log)))
    values <- filter(values, is.outlier.lin) %>%  select(uuid, variable, value)
    if (nrow(values)>0) print(paste0(col, ": ", nrow(values), " outliers detected"))
    res.outliers_main <- rbind(res.outliers_main, values)
  }
  
  #f.alpha <- function(x) return(ifelse(x, 1, 0))
  #
  ## Outliers Boxplot generator per country
  #
  #df <- raw.main.outliers %>% 
  #  select(uuid, all_of(cols.integer_raw.main)) %>% 
  #  pivot_longer(-uuid, names_to = "variable", values_to = "value") %>% 
  #  mutate(value.log = log10(value)) %>% 
  #  left_join(select(res.outliers_main, -value) %>% mutate(is.outlier=T), by = c("uuid","variable")) %>% 
  #  mutate(is.outlier = ifelse(is.na(is.outlier), F, is.outlier)) %>% 
  #  filter(!is.na(value) & value>0)
  #df <- gather(df, key = "measure", value = "value", variable)
  #df.all <- rbind(df.all, df)
  #
  #
  #write.xlsx(df.all, paste0("output/checking/outliers/main_outlier_prices_analysis_", n.sd, "sd.xlsx"), overwrite=T)
  #
  ## generating prices boxplots for same locations
  #g.outliers_main <- ggplot(df.all) +
  #  geom_boxplot(aes(x= measure, y=value.log), width = 0.2) + ylab("Values (log10)") +
  #  geom_point(aes(x=measure, y=value.log, group = measure), alpha=f.alpha(df.all$is.outlier), colour="red") +
  #  facet_wrap(~value, ncol = 4, scales = "free_y")+
  #  theme(axis.text.x = element_blank(),
  #        axis.ticks.x = element_blank())
  #
  #
  ## Save
  #ggsave(paste0("output/checking/outliers/main_outlier_prices_analysis_", n.sd, "sd.pdf"), g.outliers_main, 
  #       width = 40, height = 80, units = "cm", device="pdf")
  #
  
  # Output requests to check
  res.outliers_main <- res.outliers_main %>% 
    mutate(issue = "Outliers",
           loop_index = NA,
           new.value = NA,
           explanation=NA) %>% 
    rename("old.value"=value) %>% 
    select(uuid,loop_index,variable,issue,old.value,new.value,explanation)
  
  cleaning.log.outliers <- rbind(cleaning.log.outliers,res.outliers_main)
  cleaning.log.outliers$check.id <- 1
  save.follow.up.requests(cleaning.log.outliers,"JMMI_Customers_R15_outliers_lin_requests.xlsx")  

  write.xlsx(raw.main, "output/Customers/data_log/data.xlsx")
}




########## USE ONLY IF YOU ALREADY HAVE CLEANED OUTLIERS
#Added this part to pull the data from the manually cleaned dataset to create the cleaning log
#cleaning.log.outliers <- load.requests(dir.requests, make.short.name("outliers_requests", no_date = T))
#uuid_outliers <- unique(cleaning.log.outliers$uuid)
#variable_outliers <- unique(cleaning.log.outliers$variable)
#clean_data <- read_xlsx("output/checking/responses/JMMI_Customers_R10_almost_clean_data_230222_.xlsx")
#clean_data <- clean_data[c(variable_outliers,"_uuid")] %>% rename(uuid="_uuid")
#clean_data <- clean_data %>% filter(uuid %in% uuid_outliers)
#clean_data_long <- gather_(clean_data, "condition", "measurement", variable_outliers)
#cleaning.log.outliers_clean <- left_join(cleaning.log.outliers,clean_data_long, by=c("uuid"="uuid","variable"="condition"))
#cleaning.log.outliers_clean$new.value <- cleaning.log.outliers_clean$measurement
#cleaning.log.outliers_clean <- subset(cleaning.log.outliers_clean, select = -measurement)
#cleaning.log.outliers_clean <- cleaning.log.outliers_clean %>%
#  mutate(explanation = case_when(is.na(new.value) ~ "error removed",
#                                                     new.value==old.value ~ "true value",
#                                                     new.value!=old.value ~ "error solved")
#  )
#cleaning.log.outliers_clean$check.id <- 1
#save.follow.up.requests(cleaning.log.outliers_clean,"JMMI_Customers_R9_outliers_response_from_data")

#------------------------------------------------------------------------------------------------------------
# --> edit the file
# --> Manually check outliers and change to NA (Decision made with country FPS)
# --> save new file as outliers_responses_edited.xlsx in output/checking/responses/
#------------------------------------------------------------------------------------------------------------
###############################################################################

# RUN ONLY IF Anything need to be changed

outlier.recode <- load.requests(dir.responses, make.short.name("outliers_response", no_date = T))
outlier.check <- load.requests(dir.requests, make.short.name("outliers_requests", no_date = T))

if (nrow(outlier.check) != nrow(outlier.recode)) warning("Number of rows are not matching")

cleaning.log.outliers <- outlier.recode %>%
  select(uuid, variable,issue,old.value,new.value) #%>%
  #filter(is.na(new.value))

#cleaning.log.outliers <- subset(cleaning.log.outliers, select = -loop_index)

#Added the following line to convert to character, so the apply.changes can work properly
#raw.main[cols.integer_raw.main] <- sapply(raw.main[cols.integer_raw.main],as.character) 

raw.main <- raw.main %>% 
  apply.changes(cleaning.log.outliers)

cleaning.log <- rbind(cleaning.log,cleaning.log.outliers)

####################### Retailers #############################################
if (JMMI_variable == "Retailers") {

raw.main <- raw.main %>%
  mutate(b3_bread_price_final_price = b3_bread_price %_/_% b3_1_bread_size * 500,
         c3_eggs_price_final_price = c3_eggs_price %_/_% c3_1_eggs_size * 10,
         d3_milk_price_final_price = d3_milk_price %_/_% d3_1_milk_size * 0.9,
         e3_potatoes_price_final_price = e3_potatoes_price %_/_% e3_1_potatoes_size * 1,
         f3_carrots_price_final_price = f3_carrots_price %_/_% f3_1_carrots_size * 1,
         g3_onions_price_final_price = g3_onions_price %_/_% g3_1_onions_size * 1,
         h3_cabbage_price_final_price = h3_cabbage_price %_/_% h3_1_cabbage_size * 1,
         i3_chicken_price_final_price = i3_chicken_price %_/_% i3_1_chicken_size * 1,
         j3_oil_price_final_price = j3_oil_price %_/_% j3_1_oil_size * 0.9,
         k3_flour_price_final_price = k3_flour_price %_/_% k3_1_flour_size * 1,
         l3_rice_price_final_price = l3_rice_price %_/_% l3_1_rice_size * 1,
         m3_buckwheat_price_final_price = m3_buckwheat_price %_/_% m3_1_buckwheat_size * 1,
         n3_water_price_final_price = n3_water_price %_/_% n3_1_water_size * 1.5,
         y3_cereal_porridge_price_final_price = y3_cereal_porridge_price %_/_% y3_1_cereal_porridge_size * 200,
         o3_diapers_price_final_price = o3_diapers_price %_/_% o3_1_diapers_size * 50,
         p3_body_soap_price_final_price = p3_body_soap_price %_/_% p3_1_body_soap_size * 75,
         r3_laundry_soap_price_final_price = as.numeric(r3_laundry_soap_price) %_/_% r3_1_laundry_soap_size * 200,
         q3_powder_price_final_price = q3_powder_price %_/_% q3_1_powder_size * 500,
         s3_toothpaste_price_final_price = s3_toothpaste_price %_/_% s3_1_toothpaste_size * 75,
         t3_pads_price_final_price = t3_pads_price %_/_% t3_1_pads_size * 10)
}
###############################################################################
#-------------------------------------------------------------------------------
# 6) Remove PII columns, apply any last changes, then save cleaned dataset
################################################################################


# finalize cleaning log:
cleaning.log <- cleaning.log %>% distinct() %>% 
  #filter(old.value %!=na% new.value) %>% 
  left_join(raw.main %>% select(uuid, any_of(enum_colname)))

if (length(list.files(make.filename.xlsx(paste0("output/", JMMI_variable, "/cleaning_log"), "cleaning_log", no_date = T))) > 0) {
  cleaning.log.previous <- read_xlsx(make.filename.xlsx(paste0("output/", JMMI_variable, "/cleaning_log"), "cleaning_log"))
  cleaning.log.whole <- rbind(cleaning.log.previous, cleaning.log)
} else cleaning.log.whole <- cleaning.log

# Output Cleaning Log
write.xlsx(cleaning.log, make.filename.xlsx(paste0("output/", JMMI_variable, "/cleaning_log"), "cleaning_log", no_date = T), overwrite = T)

# Output deletion Log
write.xlsx(deletion.log.new, make.filename.xlsx(paste0("output/", JMMI_variable, "/deletion_log"), "deletion_log", no_date = T), overwrite = T)



# combine new and previous data:
# ------------------------------------------------------------------------------

#if(!"main.data.previous" %in% ls()) {
#warning("main.data.previous was not found! Are you sure you don't have a previous data_log that you can load?")
#new.main <- raw.main
## new.loop1 <- raw.loop1
## new.loop2 <- raw.loop2
#}else{
## check if there are any columns somehow added during this cleaning process
#for(cc in colnames(raw.main)){
#if(!(cc %in% colnames(main.data.previous))){
#warning(paste("column",cc,"found in raw.main but not in main.data.previous!"))
#}}
## and the other way around:
#for(cc in colnames(main.data.previous)){
#if(!(cc %in% colnames(raw.main))){
#warning(paste("column",cc,"found in main.data.previous but not in raw.main!"))
#}}
#
#new.main  <- bind_rows(main.data.previous, raw.main) %>% filter(!uuid %in% deletion.log.whole$uuid)
## new.loop1 <- bind_rows(loop1.previous, raw.loop1) %>% filter(!uuid %in% deletion.log.whole$uuid)
## new.loop2 <- bind_rows(loop2.previous, raw.loop2) %>% filter(!uuid %in% deletion.log.whole$uuid)
#}


if (JMMI_variable == "Customers") {

## revive columns due to old version of questionary
cols_remove <- c(
  "b7_vehicle_fuel/none_vehicles",
  "b7_1_heating_fuel/none_heating"
  
)

## Add any changes to the tool?
raw.main <- raw.main %>%
  select(-any_of(cols_remove, vars = NULL))
tool.survey <- tool.survey %>% 
  mutate(datasheet = "main")

pii.to.remove_main <- c(
  "deviceid",
  "audit",
  "audit_URL",
  "comments_enumerators")
new.main.removed  <- raw.main %>% select(-any_of(pii.to.remove_main))
}

##################### Retailers ##############################################

if (JMMI_variable == "Retailers") {

pii.to.remove_main <- c(
  "a10_name_retailer",
  "a11_branch_retailer",
  "a12_phone_number",
  "deviceid",
  "audit",
  "audit_URL",
  "z4_like_name_retailer",
  "z5_like_phone_number",
  "z6_like_hromada",
  "comments_enumerators"
)

new.main.removed  <- raw.main %>% select(-any_of(pii.to.remove_main))
}
##############################################################################



# All data write excel
datasheets <- list("main" = raw.main
               )
write.xlsx(datasheets, make.filename.xlsx(paste0("output/", JMMI_variable, "/data_log"), "full_data"), overwrite = T,
           zoom = 90, firstRow = T)

# final (pii removed)
datasheets_anon <- list("main" = new.main.removed
                        )
raw.main <- raw.main %>% select(-any_of(pii.to.remove_main))
write.xlsx(datasheets_anon, make.filename.xlsx(paste0("output/", JMMI_variable, "/final"), "final_clean_data"), overwrite = T,
           zoom = 90, firstRow = T)

source("src/count_enum_performance.R")
source("package4validation.R")

cat("\nD O N E\n")
