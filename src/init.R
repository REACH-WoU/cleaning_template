# loading all packages, function and list of variables.

# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(docstring, tidyverse, readxl, writexl, openxlsx, stringr, 
               sf, geosphere, qdapRegex, cluster, randomcoloR, svDialogs, scales)

source("src/utils/misc_utils.R")
source("src/utils/check_kobo.R")
source("src/utils/kobo_utils.R")
source("src/utils/regional_detect_data_falsification.R")
source("src/utils/utils_audit.R")
source("src/utils/utils_cleaning.R")
source("src/utils/utils_cleaning_loops.R")

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)
enum_colname <- "enum_id"


dir.audits <- "data/inputs/audits/reach/"
dir.requests <- "output/checking/requests/"
dir.responses <- "output/checking/responses/"


label_colname <- "label::English" ### CHANGE the label_colname to whatever fits properly

###############################################################################

# input filnenames
filename.tool <- "resources/tool.xlsx" ### CHANGE the tool name to whatever fits properly
filename_path <- "hsm_data_r5.xlsx"
###############################################################################

# load TOOL Refuggess
cat("\n- LOADING tool ...\n")

cat("\nLoading Kobo tool from file", filename.tool, "...\n")
tool.survey  <- load.tool.survey(filename.tool)
tool.choices <- load.tool.choices(filename.tool)

# tool.survey <- read_excel(filename.tool, sheet = "survey", col_types = "text") %>% 
#   filter(!is.na(type)) %>%
#   mutate(q.type=as.character(lapply(type, function(x) str_split(x, " ")[[1]][1])),
#          list_name=as.character(lapply(type, function(x) str_split(x, " ")[[1]][2])),
#          list_name=ifelse(str_starts(type, "select_"), list_name, NA))
# tool.choices <- read_excel(filename.tool, sheet = "choices", col_types = "text") %>% 
#   filter(!is.na(list_name)) %>% 
#   select(list_name, name, all_of(label_colname)) %>% distinct()

cat("..OK\n")


