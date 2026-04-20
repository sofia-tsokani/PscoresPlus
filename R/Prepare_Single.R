prep_single<-function(x){
  treatnames<-list()
  treatnames =lapply(x, FUN =  function(x){row.names(x$TE.random)})
  leaguetables<-league_table(x[[1]])
  seleaguetables<-league_table_var((x[[1]]))
  comm<-treatnames
  outcomes=array(unlist(leaguetables),dim=c(length(comm[[1]]),length(comm[[1]]),length(x)))
  var.outcomes=array(unlist(seleaguetables),dim=c(length(comm[[1]]),length(comm[[1]]),length(x)))
  row.names(outcomes)<-comm[[1]]
  colnames(outcomes)<-comm[[1]]
  row.names(var.outcomes)<-comm[[1]]
  colnames(var.outcomes)<-comm[[1]]
  ret<-list(outcomes,var.outcomes,comm[[1]])
  
  names(ret)<-c("outcomes","var.outcomes","comm")
  return(ret)}