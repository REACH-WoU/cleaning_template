
# get all of the other questions
other.db <- utilityR::get.other.db(tool.choices = tool.choices,
                                   tool.survey = tool.survey,
                                   label_colname = label_colname)


# Separate the other questions files by loop
other.db.main  <- other.db[other.db$name %in% colnames(raw.main),]
# find _other responses in main
other.responses <- utilityR::find.responses(raw.main, other.db.main)

other.responses <- raw.main %>% 
  select(uuid,all_of(na.omit(other.responses$ref.name))) %>% 
  pivot_longer(cols =all_of(na.omit(other.responses$ref.name)), names_to = 'ref.name', values_to = 'selected.ref.value') %>% 
  right_join(other.responses)

if(exists('raw.loop1')){
  other.db.loop1 <- other.db[other.db$name %in% colnames(raw.loop1),]
  other.responses.loop1 <- utilityR::find.responses(raw.loop1, other.db.loop1, is.loop = T)
  
  if(nrow(other.responses.loop1)>0){
    other.responses.loop1 <- raw.loop1 %>% 
      select(loop_index,all_of(na.omit(other.responses.loop1$ref.name))) %>% 
      pivot_longer(cols =all_of(na.omit(other.responses.loop1$ref.name)), names_to = 'ref.name', values_to = 'selected.ref.value') %>% 
      right_join(other.responses.loop1)
  }else{other.responses.loop1<- data.frame()}
}else{other.responses.loop1 <- data.frame()}

if(exists('raw.loop2')){
  other.db.loop2 <- other.db[other.db$name %in% colnames(raw.loop2),]
  other.responses.loop2 <- utilityR::find.responses(raw.loop2, other.db.loop2, is.loop = T)
  if(nrow(other.responses.loop2)>0){
    other.responses.loop2 <- raw.loop2 %>% 
      select(loop_index,all_of(na.omit(other.responses.loop2$ref.name))) %>% 
      pivot_longer(cols =all_of(na.omit(other.responses.loop2$ref.name)), names_to = 'ref.name', values_to = 'selected.ref.value') %>% 
      right_join(other.responses.loop2)
  }else{other.responses.loop2<- data.frame()}
}else{other.responses.loop2 <- data.frame()}
if(exists('raw.loop3')){
  other.db.loop3 <- other.db[other.db$name %in% colnames(raw.loop3),]
  other.responses.loop3 <- utilityR::find.responses(raw.loop3, other.db.loop3, is.loop = T)
  if(nrow(other.responses.loop3)>0){
    other.responses.loop3 <- raw.loop3 %>% 
      select(loop_index,all_of(na.omit(other.responses.loop3$ref.name))) %>% 
      pivot_longer(cols =all_of(na.omit(other.responses.loop3$ref.name)), names_to = 'ref.name', values_to = 'selected.ref.value') %>% 
      right_join(other.responses.loop3)
  }else{other.responses.loop3<- data.frame()}
}else{other.responses.loop3 <- data.frame()}

# bind them all together
other.responses <- rbind(other.responses, other.responses.loop1, other.responses.loop2, other.responses.loop3)

# translate your data
other.responses.j <- utilityR::translate.responses(responses = other.responses,
                                                   values_from = 'responses',
                                                   directory = directory_dictionary$dir.requests,
                                                   api.key = api_key
)

utilityR::save.other.requests(utilityR::create.translate.requests(other.responses.j),
                              directory = directory_dictionary$dir.requests,
                              make.short.name("other_requests_final"), use_template = F)

# ------------------------------------------------------------------------------


# translate all text questions, but skip these columns:
trans_cols_to_skip <- c(
  # add columns to skip
  "enum_comms"
)
trans.db <- utilityR::get.trans.db(tool.choices = tool.choices,
                                   tool.survey = tool.survey,
                                   label_colname = label_colname) %>% 
  filter(!name %in% trans_cols_to_skip)


# if there are any variables that have text columns and are missing from trans DB - add them here
missing_vars <- data.frame(name = c('conditions_to_pursue_option_other',
                                    'reasons_feeling_of_safety_non_idp_other'),
                           label = c('c2_6 [Space to expand or add further information given on reasons behind wanting to STAY or LEAVE their current CS]',
                                     'd1_2 [Space to expand or add further information given on reasons why they feel safe or not]'))

trans.db <- rbind(trans.db, missing_vars)


# separate all open questions by loop and locate the answers to these questions
trans.db.main <- trans.db[trans.db$name %in% colnames(raw.main),]
trans.responses.main <- utilityR::find.responses(raw.main, trans.db.main)

if(exists('raw.loop1')){
  trans.db.loop1 <- trans.db[trans.db$name %in% colnames(raw.loop1),]
  if(nrow(trans.db.loop1)>0){
    trans.responses.loop1 <- utilityR::find.responses(raw.loop1, trans.db.loop1, is.loop = T)
  }else{trans.responses.loop1 <- data.frame()}
}else{trans.responses.loop1 <- data.frame()}

if(exists('raw.loop2')){
  trans.db.loop2 <- trans.db[trans.db$name %in% colnames(raw.loop2),]
  if(nrow(trans.db.loop2)>0){
    trans.responses.loop2 <- utilityR::find.responses(raw.loop2, trans.db.loop2, is.loop = T)
  }else{trans.responses.loop2 <- data.frame()}
}else{trans.responses.loop2 <- data.frame()}


if(exists('raw.loop3')){
  trans.db.loop3 <- trans.db[trans.db$name %in% colnames(raw.loop3),]
  if(nrow(trans.db.loop3)>0){
    trans.responses.loop3 <- utilityR::find.responses(raw.loop3, trans.db.loop3, is.loop = T)
  }else{trans.responses.loop3 <- data.frame()}
}else{trans.responses.loop3 <- data.frame()}

# bind them all together
trans.responses <- rbind(trans.responses.main,
                         trans.responses.loop1,
                         trans.responses.loop2,
                         trans.responses.loop3)

rm(trans.responses.main, trans.responses.loop1, trans.db.loop2, trans.responses.loop3)

# translate all of the responses
trans.responses.j <- utilityR::translate.responses(responses = trans.responses,
                                                   values_from = 'responses',
                                                   directory = directory_dictionary$dir.requests,
                                                   api.key = api_key)

# save the translation requests
utilityR::save.trans.requests(utilityR::create.translate.requests(trans.responses.j),
                              directory = directory_dictionary$dir.requests,
                              make.short.name("text_requests_final"), use_template = F)
