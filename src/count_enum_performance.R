source("src/init.R")

filename.dataset <- "data/inputs/kobo_export/UKR2203_JMMI_Questionnaire_Retailers_R14_10MAY2023_SB.xlsx"

kobo.raw <- read_excel(filename.dataset, col_types = "text") %>%
  rename(uuid ="_uuid", index = "_index")

deletion.log <- read_excel(paste0("output/deletion_log/JMMI_Retailers_R14_deletion_log.xlsx"), col_types = "text")
cleaning.log <- read_excel(paste0("output/cleaning_log/JMMI_Retailers_R14_cleaning_log.xlsx"), col_types = "text")

create.count_collected_enu(kobo.raw,     "a2_1_enum_id")
create.count_deleted_enu(deletion.log, "a2_1_enum_id")
create.count_enu_cleaning(cleaning.log, "a2_1_enum_id")

cat("\n> Done. Created 3 files in output/enum_performance.")
