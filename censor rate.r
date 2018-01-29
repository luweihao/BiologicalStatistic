## calculate the censoring rate####

censor.rate=function(size, censor.para)
{
set.seed(666)
nsim=1000
censor.rate=as.vector(rep(0,nsim))

for(i in 1:nsim)
{
  Fdata=generate.cohort(size, censor.para)
  censor.rate[i]=1-mean(Fdata$delta)
}
censor0=mean(censor.rate)
return(censor0)
}
