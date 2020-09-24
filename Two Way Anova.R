Input=("Instructor        Supplement  Sodium
'Brendon Small'   A           1200
       'Brendon Small'   A           1400
       'Brendon Small'   A           1350
       'Brendon Small'   A            950
       'Brendon Small'   A           1400
       'Brendon Small'   B           1150
       'Brendon Small'   B           1300
       'Brendon Small'   B           1325
       'Brendon Small'   B           1425
       'Brendon Small'   B           1500
       'Brendon Small'   C           1250
       'Brendon Small'   C           1150
       'Brendon Small'   C            950
       'Brendon Small'   C           1150
       'Brendon Small'   C           1600
       'Brendon Small'   D           1300
       'Brendon Small'   D           1050
       'Brendon Small'   D           1300
       'Brendon Small'   D           1700
       'Brendon Small'   D           1300
       'Coach McGuirk'   A           1100
       'Coach McGuirk'   A           1200
       'Coach McGuirk'   A           1250
       'Coach McGuirk'   A           1050
       'Coach McGuirk'   A           1200
       'Coach McGuirk'   B           1250
       'Coach McGuirk'   B           1350
       'Coach McGuirk'   B           1350
       'Coach McGuirk'   B           1325
       'Coach McGuirk'   B           1525
       'Coach McGuirk'   C           1225
       'Coach McGuirk'   C           1125
       'Coach McGuirk'   C           1000
       'Coach McGuirk'   C           1125
       'Coach McGuirk'   C           1400
       'Coach McGuirk'   D           1200
       'Coach McGuirk'   D           1150
       'Coach McGuirk'   D           1400
       'Coach McGuirk'   D           1500
       'Coach McGuirk'   D           1200
       'Melissa Robins'    A            900
       'Melissa Robins'    A           1100
       'Melissa Robins'    A           1150
       'Melissa Robins'    A            950
       'Melissa Robins'    A           1100
       'Melissa Robins'    B           1150
       'Melissa Robins'    B           1250
       'Melissa Robins'    B           1250
       'Melissa Robins'    B           1225
       'Melissa Robins'    B           1325
       'Melissa Robins'    C           1125
       'Melissa Robins'    C           1025
       'Melissa Robins'    C            950
       'Melissa Robins'    C            925
       'Melissa Robins'    C           1200
       'Melissa Robins'    D           1100
       'Melissa Robins'    D            950
       'Melissa Robins'    D           1300
       'Melissa Robins'    D           1400
       'Melissa Robins'    D           1100
       ")
Data = read.table(textConnection(Input),header=TRUE)

interaction.plot(Data$Instructor, Data$Supplement, Data$Sodium)

interaction.plot(Data$Supplement, Data$Instructor, Data$Sodium)

Data$Instructor = factor(Data$Instructor,
                         levels=unique(Data$Instructor))

Data$Supplement       = factor(Data$Supplement,
                               levels=unique(Data$Supplement))
library(psych)

headTail(Data)

str(Data)

summary(Data)

rm(Input)

library(FSA)

Summarize(Sodium ~ Supplement + Instructor,
          data=Data,
          digits=3)

plot(Sodium~Instructor+Supplement, data = Data)
par(mfrow=c(1,2))
plot(Sodium~Instructor+Supplement, data = Data)

results<- lm(Sodium~Instructor+Supplement+Instructor*Supplement, data =
                 Data)
anova(results)
qqnorm(results$residuals)
