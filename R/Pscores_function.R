
pscores<-function(outcomes,var.outcomes,correlation,beta,type,label)
{
  # package mvtnorm is needed (https://cran.r-project.org/web/packages/mvtnorm/index.html)
  # this function estimates P-scores (presented in this paper https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-015-0060-8) for i) ranking many outcomes simultaneously ii) taking into account differences of a certain effect across treatments
  # outcomes is an array with all the league tables of the K outcomes we consider
  # var.outcomes is an array with the standard errors of the relative effects shown in the league table
  # correlation is KxK correlation matrix for the outcomes considered
  # beta is a K-vector with the benefits and costs (what we are willing to tolerate for specific benefits). Primary analysis should be that beta is a zero vector  
  # type is a K-vector for the type of outcome Harmful=H or Benebicial=B. For harmful outcomes, small values are good whereas for beneficial outcomes, large values are good
  # label is a vector with the lables of the interventions  
  p=dim(outcomes)[[1]]
  
  k=dim(outcomes)[[3]]
  
  for (h in 1:k)
  {
    if (type[h]=="H")
    {
      outcomes[,,h]=-outcomes[,,h]
    }
  }
  
  z.table=array(rep(0,p^2*k),c(p,p,k),dimnames=list(label))
  
  for (h in 1:k)
  {
    for (i in 1:p)
    {
      for (j in 1:p)  
      {
        if (i>j)
        {
          z.table[i,j,h]=(outcomes[i,j,h]-beta[h])/sqrt(var.outcomes[i,j,h])
        }
        else if (j>i)
        {
          z.table[i,j,h]=(outcomes[i,j,h]+beta[h])/sqrt(var.outcomes[i,j,h])
          
        }
      }
    }
  }
  
  prob=matrix(rep(0,p^2),p,p,dimnames=list(label))
  
  if (dim(outcomes)[[3]]==1)
  {
    for (i in 1:p)
    {for (j in 1:p)
    {if (i==j)
    {prob[i,j]=0.5}  else if (j>i)
    {prob[i,j]=pnorm(z.table[i,j,1])}
      else
      {prob[i,j]=1-pnorm(z.table[i,j,1])}
    }
    }
  }
  
  else
  {
    for (i in 1:p)
    {
      for (j in 1:p)
      {
        if (i==j)
        {
          prob[i,j]=0.5
        }  
        else if (j>i)
        {
          prob[i,j]=pmvnorm(lower=rep(-Inf,k),upper=c(z.table[i,j,]),mean=rep(0,k),correlation)
          
        }
        else
        {
          prob[i,j]=pmvnorm(lower=c(z.table[i,j,]),upper=rep(Inf,k),mean=rep(0,k),correlation)
        }
      }
    }
  }
  pscore=(rowSums(prob)-0.5)/(p-1)
  
}