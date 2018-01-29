CP.indicator=function(parameter, parameter.hat, SE.hat, alpha=0.05)
{
## the covering rate of the estimation interval ##################
## parameter     = the true value of parameter
## parameter.hat = the estimation of parameter
## SE.hat        = the estimated SE
## alpha         = confidential level 1-alpha
###################################################################
np.para=length(parameter)
cp.ind=as.vector(rep(0, np.para))

for (j in 1:np.para)
{
parameter.left.j =parameter.hat[j]-qnorm(1-alpha/2)*(SE.hat[j])
parameter.right.j=parameter.hat[j]+qnorm(1-alpha/2)*(SE.hat[j])

if (parameter[j]<=parameter.right.j&parameter[j]>=parameter.left.j)
{cp.ind[j]=1}
else
{cp.ind[j]=0}
}
return(cp.ind)
}