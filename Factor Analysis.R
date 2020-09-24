#Files then More, setwd; check spelling of specific data set

raqData<-read.delim("raq.Data.dat", header=TRUE)
#correlation matrix
raqMatrix <- cor(raqData)
#round correlation values in matrix to 2 decimal places
round(raqMatrix,2)

library(psych)
cortest.bartlett(raqMatrix, n=2571)

library(GPArotation)
library(psych)

#running the factor analysis using the correlation matrix
pc4 <- principal(raqMatrix,nfactors=4,rotate="oblimin")

#examine the factor loadings (the bs)

print.psych(pc4, cut=0.30, sort=TRUE)
