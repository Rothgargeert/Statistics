y1 = c(18.2, 20.1, 17.6, 16.8, 18.8, 19.7, 19.1) 
y2 = c(17.4, 18.7, 19.1, 16.4, 15.9, 18.4, 17.7)
y3 = c(15.2, 18.8, 17.7, 16.5, 15.9, 17.1, 16.7)
scores = data.frame(y1, y2, y3)
scores
boxplot(scores)
scores=stack(scores)
names(scores)
scores
oneway.test(values ~ ind, data=scores, var.equal = T)
grades=c(18.2, 20.1, 17.6, 16.8, 18.8, 19.7, 19.1, 17.4,
        18.7, 19.1, 16.4, 15.9, 18.4, 17.7, 15.2, 18.8, 17.7, 
        16.5, 15.9, 17.1, 16.7)
class=c(rep("A",7), rep("B",7), rep("C",7)) 
school = data.frame(grades, class) 
plot(grades~class, data = school)
results = aov(grades ~ class, data = school)
summary(results)
pairwise.t.test(grades, class, p.adjust.method = "bonferroni")
