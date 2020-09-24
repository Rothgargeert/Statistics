mytable<-Multiple_Regression_Analysis_Assignment_Data
myvars=c("Price", "Size", "Tax")
mytable2=mytable[myvars]
mytable2
plot(mytable2)
results=lm(Price~Size+Tax, data=mytable2)
results
summary(results)
reduced=lm(Price~Size+Tax, data=mytable2)
full=lm(Price~Size+Tax+Bedroom, data=mytable)
reduced
full
plot(mytable)
anova(reduced, full)
