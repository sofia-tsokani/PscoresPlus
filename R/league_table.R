league_table=function(net)
{
  league.table=net$TE.random
  league.table[which(lower.tri(league.table==T))]<--league.table[which(lower.tri(league.table==T))]
  
  return(league.table)
}
