prep_multi<-function(x){
  treatnames<-list()
  leaguetables<-list()
  seleaguetables<-list()
  treatnames =lapply(x, FUN =  function(x){row.names(x$TE.random)})
  comm<-intersect2(list(treatnames))
  
  for (i in 1:length(x)) {
    leaguetables[[i]]<-league_table(x[[i]])[comm,comm]
    seleaguetables[[i]]<-league_table_var((x[[i]]))[comm,comm]}
  
  outcomes=array(unlist(leaguetables),dim=c(length(comm),length(comm),length(x)))
  var.outcomes=array(unlist(seleaguetables),dim=c(length(comm),length(comm),length(x)))
  row.names(outcomes)<-comm
  colnames(outcomes)<-comm
  row.names(var.outcomes)<-comm
  colnames(var.outcomes)<-comm
  ret<-list(outcomes,var.outcomes,comm)
  
  names(ret)<-c("outcomes","var.outcomes","comm")
  return(ret)}


