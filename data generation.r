#################    generate data   #######################################################
generate.cox=function(sample.size, beta0, cova.para1, cova.para2, censor.para)
{
#### generate survival data matrix by Exponential Distribution #############################
########## Input ##############
## sample.size= the sample size should be generated
## beta0      = parameter true value
## cova.para1 = c(a1,a2) = parameters for generating X1
## cova.para2 = c(a1,a2) = parameters for generating X2
## censor.para= c(a1,a2) = censoring time C_i~U(0, censor.para)
######### Output ##############
## surv.data  = (Otime, delta, covaZ)
## surv.sort  = (Otime, delta, covaZ) = sorted by Observed Time 
############################################################################################
Z1=rnorm(sample.size, cova.para1[1], cova.para1[2])             ## Z1~N(0, 1)
Z2=rbinom(sample.size, cova.para2[1], cova.para2[2])            ## Z2~B(1, 0.5)
covaZ=as.matrix(cbind(Z1, Z2))                                  ## covariates

betaZ=apply(beta0*t(covaZ), 2, sum)
exp.betaZ=exp(betaZ)

Ftime=rexp(sample.size, exp.betaZ)                              ## failure time E(exp{betaZ})
Ctime=runif(sample.size, censor.para[1], censor.para[2])        ## censoring time U(0,1)
Otime=apply(as.matrix(cbind(Ftime, Ctime)), 1, min)             ## observed time

delta=rep(1, sample.size)                    
delta[Ftime>Ctime]=0                                                 ## censoring indicator     

surv.matrix=as.data.frame(cbind(Otime, delta, covaZ))                ## observed data
surv.data=as.data.frame(surv.matrix[sort.list(surv.matrix$Otime), ]) ## sorted by observed time
return(surv.data)  
}







