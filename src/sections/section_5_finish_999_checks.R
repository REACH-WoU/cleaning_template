if(nrow(cl_log_999>0)){
  # Separate the cleaning log files so that we only apply changes to those data that need it.
  raw.main_999 <- cl_log_999 %>% 
    filter(variable %in% names(raw.main))
  
  if(exists('raw.loop1')){
    raw.loop1_999 <- cl_log_999 %>% 
      filter(variable %in% names(raw.loop1))
  }else{raw.loop1_999 <- data.frame()}
  if(exists('raw.loop2')){
    raw.loop2_999 <- cl_log_999 %>% 
      filter(variable %in% names(raw.loop2))
  }else{raw.loop2_999 <- data.frame()}
  if(exists('raw.loop3')){
    raw.loop3_999 <- cl_log_999 %>% 
      filter(variable %in% names(raw.loop3))
  }else{raw.loop3_999 <- data.frame()}
  
  # apply changes to the frame
  raw.main <- utilityR::apply.changes(raw.main,clog = raw.main_999,is.loop = F)
  if(nrow(raw.loop1_999)>0){
    raw.loop1 <- utilityR::apply.changes(raw.loop1,clog = raw.loop1_999,is.loop = T)
  }
  if(nrow(raw.loop2_999)>0){
    raw.loop2 <- utilityR::apply.changes(raw.loop2,clog = raw.loop2_999,is.loop = T)
  }
  if(nrow(raw.loop3_999)>0){
    raw.loop3 <- utilityR::apply.changes(raw.loop3,clog = raw.loop3_999,is.loop = T)
  }
  cleaning.log <- bind_rows(cleaning.log, cl_log_999)
}