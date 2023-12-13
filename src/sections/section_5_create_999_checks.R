
cl_log_999 <- data.frame()

# get the integers from main
int_cols_main  <- tool.survey %>% 
  filter(type == "integer" & datasheet == "main") %>% 
  pull(name)
# recode if any
cl_log_999_main <- utilityR::recode.set.NA.if(raw.main,int_cols_main, code = 'code_for_check',issue = 'Wrong entry')
#bind the logs
cl_log_999 <- bind_rows(cl_log_999,cl_log_999_main)
if(exists('raw.loop1')){
  int_cols_loop1 <- tool.survey %>% 
    filter(type == "integer" & datasheet != "main" & name %in% names(raw.loop1)) %>% 
    pull(name) 
  
  cl_log_999_loop1 <- utilityR::recode.set.NA.if(raw.loop1,int_cols_loop1, code = 'code_for_check',issue = 'Wrong entry')
  cl_log_999 <- bind_rows(cl_log_999,cl_log_999_loop1)
}

if(exists('raw.loop2')){
  int_cols_loop2 <- tool.survey %>% 
    filter(type == "integer" & datasheet != "main" & name %in% names(raw.loop2)) %>% 
    pull(name) 
  
  cl_log_999_loop2 <- utilityR::recode.set.NA.if(raw.loop2,int_cols_loop2, code = 'code_for_check',issue = 'Wrong entry')
  cl_log_999 <- bind_rows(cl_log_999,cl_log_999_loop2)
}

if(exists('raw.loop3')){
  int_cols_loop3 <- tool.survey %>% 
    filter(type == "integer" & datasheet != "main" & name %in% names(raw.loop3)) %>% 
    pull(name) 
  
  cl_log_999_loop3 <- utilityR::recode.set.NA.if(raw.loop3,int_cols_loop3, code = 'code_for_check',issue = 'Wrong entry')
  cl_log_999 <- bind_rows(cl_log_999,cl_log_999_loop3)
}

cleaning.log <- bind_rows(cleaning.log, cl_log_999)