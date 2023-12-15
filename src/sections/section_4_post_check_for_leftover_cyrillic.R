cyrillic.main <- raw.main %>% 
  pivot_longer(cols= names(raw.main)[!names(raw.main) %in% vars_to_omit], names_to = 'variable', values_to = 'value') %>% 
  filter(grepl('[а-яА-я]',value))

if(nrow(cyrillic.main)>0){
  warning('Your data sill has cyrillic entries, please check file cyrillic.main for details')
}


if(length(sheet_names_new)>0){
  for(i in sheet_names_new){
    txt <- paste0('cyrillic.',i,' <- ',i,' %>% 
    pivot_longer(cols= names(',i,')[!names(',i,') %in% vars_to_omit], names_to = "variable", values_to = "value") %>% 
                  filter(grepl("[а-яА-я]",value))')
    eval(parse(text=txt))
    txt <- paste0('if(nrow(cyrillic.',i,')>0){
    warning("Your data sill has cyrillic entries, please check file cyrillic.',i,' for details")
    }')
    
    eval(parse(text=txt))
    }
}
