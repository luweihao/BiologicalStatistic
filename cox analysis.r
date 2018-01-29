library(survival)  

## Example 1
kidney=read.table("Kidney.txt", header=T)   
model1=coxph(Surv(Time, delta)~Gender+Race+Age, data=kidney)
summary(model1)
step(model1)
 
model2=coxph(Surv(Time, delta)~Age, data=kidney)
summary(model2) 

## Example 2
model1=coxph(Surv(time, status)~age+sex+ph.ecog+ph.karno+meal.cal+wt.loss, data=lung)
summary(model1)
 
model2=coxph(Surv(time, status)~sex+ph.ecog, data=lung)
summary(model2) 


###¡¡others 
fit2=cox.zph(model2)
print(fit2)
plot(fit2)
plot(resid(model2))
