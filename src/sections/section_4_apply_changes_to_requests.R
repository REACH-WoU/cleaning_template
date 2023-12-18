cleaning.log <- data.frame()

if(name_clean_others_file != ''){
  or.edited  <- utilityR::load.requests(directory_dictionary$dir.requests, 
                                        name_clean_others_file,
                                        sheet = "Sheet2", validate = T)  # specify Sheet2 because the first one is a readme
  
  
  if(any(or.edited$check == 1)){
    issue <- paste0('uuid: ', or.edited[or.edited$check == 1,]$uuid,', variable: ',or.edited[or.edited$check == 1,]$name)
    stop(paste0('Some of your entries have errors, please double-check: ', paste0(issue,collapse = '\n')))
  }
  
  # this is for the test purposes. You won't have to run it when you've worked through everything
  or.edited <- or.edited %>% 
    filter(!(is.na(true.v)&is.na(existing.v) & is.na(invalid.v)))
  
  # run the bits below
  
  # separate the other translations to fit each individual dataframe that you have - no unnecessary variables in each
  raw.main_requests <- or.edited %>% 
    filter(name %in% names(raw.main))
  if(exists('raw.loop1')){
    raw.loop1_requests <- or.edited %>% 
      filter(name %in% names(raw.loop1))
    if(nrow(raw.loop1_requests)==0){
      rm(raw.loop1_requests)
    }
  }
  if(exists('raw.loop2')){
    raw.loop2_requests <- or.edited %>% 
      filter(name %in% names(raw.loop2))
    if(nrow(raw.loop2_requests)==0){
      rm(raw.loop2_requests)
    }
  }
  if(exists('raw.loop3')){
    raw.loop3_requests <- or.edited %>% 
      filter(name %in% names(raw.loop3))
    if(nrow(raw.loop3_requests)==0){
      rm(raw.loop3_requests)
    }
  }
  
  
  if(!(nrow(raw.main_requests)+nrow(raw.loop1_requests)+nrow(raw.loop2_requests)+nrow(raw.loop3_requests)==nrow(or.edited))){
    warning('The number of rows in each of the separated dataframes does not match the number of total rows. This may mean
          that some of the variables in your edited file are not present in the dataframes')
    all_names <- unique(c(
      names(raw.main),names(raw.loop1),names(raw.loop2),names(raw.loop3)
    ))
    print('Variables that may be causing this issue:')
    print(setdiff(or.edited$name,all_names))
  }
  
  # If you face any weird double spaces
  #tool.choices$`label::English`=str_squish(tool.choices$`label::English`)
  
  # Create a cleaning log file for each loop if there's a need for it.
  cleaning.log.other.main <- utilityR::recode.others(data = raw.main,
                                                     or.edited = raw.main_requests,
                                                     orig_response_col = 'responses',
                                                     is.loop = F,
                                                     tool.choices = tool.choices,
                                                     tool.survey = tool.survey)
  
  if(exists('raw.loop1_requests')){
    cleaning.log.other.loop1 <- utilityR::recode.others(data = raw.loop1,
                                                        or.edited = raw.loop1_requests,
                                                        orig_response_col = 'responses',
                                                        is.loop = T,
                                                        tool.choices = tool.choices,
                                                        tool.survey = tool.survey)
  }else{cleaning.log.other.loop1 <- data.frame()}
  
  if(exists('raw.loop2_requests')){
    cleaning.log.other.loop2 <- utilityR::recode.others(data = raw.loop2,
                                                        or.edited = raw.loop2_requests,
                                                        orig_response_col = 'responses',
                                                        is.loop = T,
                                                        tool.choices = tool.choices,
                                                        tool.survey = tool.survey)
  }else{cleaning.log.other.loop2 <- data.frame()}
  if(exists('raw.loop3_requests')){
    cleaning.log.other.loop3 <- utilityR::recode.others(data = raw.loop3,
                                                        or.edited = raw.loop3_requests,
                                                        orig_response_col = 'responses',
                                                        is.loop = T,
                                                        tool.choices = tool.choices,
                                                        tool.survey = tool.survey)
  }else{cleaning.log.other.loop3 <- data.frame()}
  
  
  ## Apply changes from the cleaning log onto our raw data
  raw.main <- utilityR::apply.changes(raw.main, clog = cleaning.log.other.main,is.loop = F)
  
  if(nrow(cleaning.log.other.loop1>0)){
    raw.loop1 <- utilityR::apply.changes(raw.loop1,clog = cleaning.log.other.loop1,is.loop = T)
  }
  if(nrow(cleaning.log.other.loop2>0)){
    raw.loop2 <- utilityR::apply.changes(raw.loop2,clog = cleaning.log.other.loop2,is.loop = T)
  }
  if(nrow(cleaning.log.other.loop3>0)){
    raw.loop3 <- utilityR::apply.changes(raw.loop3,clog = cleaning.log.other.loop3,is.loop = T)
  }
  
  # Create the cleaning log for recoding others
  cleaning.log.other <- rbind(cleaning.log.other.main,cleaning.log.other.loop1,
                              cleaning.log.other.loop2,
                              cleaning.log.other.loop3
  )
  # bind it with the main cleaning log
  cleaning.log <- bind_rows(cleaning.log, cleaning.log.other)
}

### Add translation cleaning if needed. ------------------------------ 

if(name_clean_trans_file!= ''){
  
  trans <-  utilityR::load.requests(directory_dictionary$dir.requests, name_clean_trans_file, validate = T)
  
  # run the bits below
  cleaning.log.trans <- utilityR::recode.trans.requests(trans, response_col = 'responses')
  
  # Separate the cleaning log files so that we only apply changes to those data that need it.
  raw.main_trans <- cleaning.log.trans %>% 
    filter(variable %in% names(raw.main))
  
  if(exists('raw.loop1')){
    raw.loop1_trans <- cleaning.log.trans %>% 
      filter(variable %in% names(raw.loop1))
  }
  if(exists('raw.loop2')){
    raw.loop2_trans <- cleaning.log.trans %>% 
      filter(variable %in% names(raw.loop2))
  }
  if(exists('raw.loop3')){
    raw.loop3_trans <- cleaning.log.trans %>% 
      filter(variable %in% names(raw.loop3))
  }
  
  # apply changes to the frame
  raw.main <- utilityR::apply.changes(raw.main,clog = raw.main_trans,is.loop = F)
  if(exists('raw.loop1_trans')){
    raw.loop1 <- utilityR::apply.changes(raw.loop1,clog = raw.loop1_trans,is.loop = T)
  }
  if(exists('raw.loop2_trans')){
    raw.loop2 <- utilityR::apply.changes(raw.loop2,clog = raw.loop2_trans,is.loop = T)
  }
  if(exists('raw.loop3_trans')){
    raw.loop3 <- utilityR::apply.changes(raw.loop3,clog = raw.loop3_trans,is.loop = T)
  }
  
  cleaning.log <- bind_rows(cleaning.log, cleaning.log.trans)
}