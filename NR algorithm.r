######################         NR Algorithm     ###########################################
######## NR algorithm for parameter estimation based on likelihood      ###################
NR.itera=function(beta.step1, full.data, cova.ind)
{
## Newton-Raphson iteration ###############################################################
########## Input ##############
## beta.step1 = the m-th step beta
## full.data  = observed data
## cova.ind   = index set for covariates in full.data
######### Output ##############
## beta.step2 = the (m+1)-th step beta calculated Newton-Raphson iteration 
## Hessian.ind= the index for Hessian matrix (nonsingular=0, singular=1)
###########################################################################################
##### gradient and hessian       ##########################################################
deriva=deriva.cox(beta.step1, full.data, cova.ind)
Gradient=deriva$Gradient
Hessian =deriva$Hessian

######   NR iterative            ##########################################################
Hessian.ind=0                                  ## the index for Hessian 0 for nonsingular 
if(rcond(Hessian)>.Machine$double.eps)         ## the reciprocal condition number 
{                                              ## 2.220446e-16 is the default
beta.step2=beta.step1+solve(Hessian, Gradient) ## Newton Raphson iterative
}
else
{
beta.step2=beta.step1
Hessian.ind=1                                  ## mark the singular Hessian by 1
}
return(list(beta.step2=beta.step2, Hessian.ind=Hessian.ind))
}


#################### 
NR.estimate=function(beta.initial, full.data, cova.ind, iter.max, epsilon)
{
## Estimation By likelihood based NR iterative#############################################
########## Input ##############
## beta.initial = the initial value for calculation
## full.data    = observed data
## cova.ind     = index set for covariates in full.data
## iter.max     = the max times of iterative of NR algorithm, the fault value=200
## epsilon      = stop distance, the fault value =0.0001
######### Output ##############
## beta.initial = estimation by NR Algorithm
## Hessian.ind  = the index for Hessian matrix (nonsingular=0, singular=1)
## flag         = iteration flag (0 for algorithm converge)
###########################################################################################
iter.flag=1
for (iter in 1:iter.max)
{
itera.est=NR.itera(beta.initial, full.data, cova.ind)                    
beta.est   =itera.est$beta.step2                                   
Hessian.ind=itera.est$Hessian.ind

if(Hessian.ind==0)
{
d=sum(abs(beta.est-beta.initial))
if(d<=epsilon) {iter.flag=0;break}
}
else
{break}

beta.initial=beta.est
}
return(list(beta.initial=beta.initial, Hessian.ind=Hessian.ind, iter.flag=iter.flag))
}


