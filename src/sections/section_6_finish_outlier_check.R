if(nrow(cleaning.log.outliers>0)){
  
  # apply changes to the frame
  raw.main <- utilityR::apply.changes(raw.main,clog = raw.main.outliers,is.loop = F)
  if(exists('raw.loop1.outliers')){
    if(nrow(raw.loop1.outliers)>0){
      raw.loop1 <- utilityR::apply.changes(raw.loop1,clog = raw.loop1.outliers,is.loop = T)
    }
  }
  if(exists('raw.loop2.outliers')){
    if(nrow(raw.loop2.outliers)>0){
      raw.loop2 <- utilityR::apply.changes(raw.loop2,clog = raw.loop2.outliers,is.loop = T)
    }
  }
  if(exists('raw.loop3.outliers')){
    if(nrow(raw.loop3.outliers)>0){
      raw.loop3 <- utilityR::apply.changes(raw.loop3,clog = raw.loop3.outliers,is.loop = T)
    }
  }
  
}