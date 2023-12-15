
# DECISIONs:
# Enter uuids of the interviews that didn't pass the audit check

duration_check <- readxl::read_excel(make.filename.xlsx(directory_dictionary$dir.audits.check, "survey_durations"))

ids <- c(duration_check$uuid)


deletion.log.too.fast <- utilityR::create.deletion.log(raw.main %>% filter(uuid %in% ids),
                                                       directory_dictionary$enum_colname, "Survey duration deemed too fast.")
# Enter uuids of the interviews that are soft duplicates to remove:
soft_duplicates <- readxl::read_excel(make.filename.xlsx(directory_dictionary$dir.audits.check, "soft_duplicates"))

ids <- c(soft_duplicates$uuid)

deletion.log.softduplicates <- utilityR::create.deletion.log(raw.main %>% filter(uuid %in% ids),
                                                             directory_dictionary$enum_colname, "Soft duplicate")

# Enter uuids of the interviews that are incomplete submissions to remove:
ids <- c(
  
)
deletion.log.incomplete <- utilityR::create.deletion.log(raw.main %>% filter(uuid %in% ids), directory_dictionary$enum_colname, "Incomplete submission")

deletion.log.audits <- bind_rows(deletion.log.too.fast, deletion.log.softduplicates, deletion.log.incomplete)
deletion.log.new <- bind_rows(deletion.log.new, deletion.log.audits)

#################################################
##   removing fast submissions and duplicates  ##
## run this to remove duplicates and no-consents  ##
raw.main  <- raw.main[!(raw.main$uuid %in% deletion.log.audits$uuid),]
if(exists('raw.loop1')){
  raw.loop1 <- raw.loop1[!(raw.loop1$uuid %in% deletion.log.audits$uuid),]
}
if(exists('raw.loop2')){
  raw.loop2 <- raw.loop2[!(raw.loop2$uuid %in% deletion.log.audits$uuid),]
}
if(exists('raw.loop3')){
  raw.loop3 <- raw.loop3[!(raw.loop3$uuid %in% deletion.log.audits$uuid),]
}
#################################################

rm(ids, deletion.log.too.fast, deletion.log.softduplicates)
