#---
#  title: 'Homework #4'
#Roger Geertz Gonzalez: "ANLY 715"
#date: "`r Sys.Date(December 5, 2019)`"
#output: html_document
#---
  
#  This in-class exercise that we complete during the December 5, 2019 class will be your Homework #4. When you have completed all your responses and provided all the R code, knit your this Markdown file to a .HTML file and upload it to the Homework #4 assignment link under the Week #14/Homework #4 tab in Moodle.

#This assignment is worth 100 points; each problem is worth 10 points each.

#We are going to use the data and scenario from Task 1 on p. 747 from Chapter 16 (Multivariate analysis of variance (MANOVA)) in the Discovering Statistics Using R text. 

#Take a few minutes and read the scenario on p.747. 

#Answer the questions below using the scenario from Task 1 on p. 747.

#a. Import the chicken data file into R. 

library(readxl)
chicken <- read_excel("chicken.xlsx")
View(chicken)

#Convert group into factor variable and provide meaningful labels 
#for group=1 and group=2
chicken$group<-factor(chicken$group, levels = c(1:2), labels=c("Manic
                      Psychosis", "Sussex Lecturers"))
str(chicken)


#b. Based on the Task 1 scenario on p. 747, 
#Why is MANOVA the appropriate analytical technique?
  
#Because the explanatory variable, group, is categorical and the 
#dependent variables quality and quantity are interval.
  
#  c. We are going to use this data to perform a MANOVA. 
#What is the appropriate null and alternative hypothesis for the MANOVA?
#The null hypothesis is that the mean number of chicken impersonations
#for all manic psychotic patients is the same as the mean quality score
#of chicken impersonations for all manic psychotic patients, and, that the
#mean number of chicken impersonations for all Sussex lecturers is the same as 
#the mean quality score of chicken impersonations for all Sussex lecturers. 
#The alternative hypothesis is that at least the two mean populations above
#are different.
  
#  You do not have to put a response here, please just 
#follow along with Dr. Anderson. And, ask any questions you have.



#d. Let's examine our data. For the in-class example, we are not going to formally check all the assumptions of the MANOVA test. However, in practice, make sure you perform this task. See notes from Executive Session #3 for assumption checking for MANOVA.

#Currently our data is in a wide format, to construct a bar chart, we need the data
#to be in a long format, we will use the melt() function to convert the data frame to a long format.



library(reshape2)
chickenmelt <- melt(chicken, id=c("group"), measured=c("quality", "quantity"))
names(chickenmelt) <- c("group", "Outcome_Measure", "Value")
View(chickenmelt)

#construct bar chart
library(ggplot2)
my_bar <- ggplot(chickenmelt, aes(group, Value))
my_bar +
  stat_summary(fun.y=mean, geom="bar", position="dodge", fill="white")+
  labs(x="Group", y="Value")+
  facet_wrap(~Outcome_Measure, ncol=2, scales="free_y")



#e. Perform a MANOVA test using this data. Do not forget that 
#for R, the outcome (dependent) variables must be in one column 
#(refer back to example worked during Executive Session #3). Use Hotelling T-squared as the test statistic for the MANOVA test.
outcome<-cbind(chicken$quality, chicken$quantity)
chickenModel<-manova(outcome ~ group, data=chicken)
summary(chickenModel, intercept=TRUE, test = "Hotelling")


#f. Report the results from the MANOVA test that you ran 
#in part e. Use terminology/similar languae as that on p. 737 from Chapter 16. 
#Your response should be one sentence. Place your single sentence response 
#in the context of the problem. For example, do not use terminology like 
#'group' or 'dependent/outcome variable'. 
#Using Hotelling's trace statistic, there was a significant effect of the 
#type of group, Manic Psychosis or Sussex Lecturers, on their number
#of impersonations (Quantity), and their impersonations score (Quality),T=0.50, 
#F(2,17)=4.25, p<.05.



#g. Since the MANOVA test was significant, you are going to follow-up with a discriminant analysis. Perform a discriminant analysis using this data.
library (MASS)
chickenDA<-lda(group ~ quantity + quality, data=chicken)
chickenDA
#Call:
lda(group ~ quantity + quality, data = chicken)




#h. Using the output from the discriminant analysis from part g, 
#specify (that is, write) the formula for the variate. 

#first variate:
#V1i=b0-0.425Quantityi+0.830Qualityi






#i. Using the values of the coefficients for the variate, describe 
#how the quality and quanitity of the chicken impersonations distinguishes 
#between the groups (Manic Psychosis and Sussex Lecturers).

#Place your responses back in the context of the problem. For example,
#do not use terminology like 'group' or 'dependent/outcome variable'. 

#first variate: Quantity and Quality have the opposite effect.
#Quantity has a negative effect while quality has
#a positive effect on the Manic Psychosis and Sussex Lecturer groups. The
#first variate differentiates these groups by some factor that affects
#Quantity and Quality differently.




