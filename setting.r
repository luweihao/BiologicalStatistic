source("data generation.r")
source("censor rate.r")

size=100
para1=1
para2=0.21
censor.para=c(para1, para2)

##para2=3.15, censoring rate=0.3
##para2=1.55, censoring rate=0.5
##para2=0.754, censoring rate=0.7
##para2=0.21, censoring rate=0.9
survdata=generate.cohort(size, censor.para)
print(survdata)
rate=censor.rate(size, censor.para)
print(rate)

