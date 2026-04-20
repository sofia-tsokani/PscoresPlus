prep<-function(x){
  if(length(x)==1){r<-prep_single(x)}
  if(length(x)!=1){r<-prep_multi(x)}
  return(r)}
