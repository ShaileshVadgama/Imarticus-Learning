mistime <- read.csv("C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/DataSet imarticus/day5 hypothesis/ONE SAMPLE t TEST.csv")
head(mistime)
t.test(mistime$Time,alternative = "greater", mu= 90)


#time tilda group need to try.

mistime3 <- read.csv("C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/DataSet imarticus/day5 hypothesis/PAIRED t TEST.csv")
head(mistime3)
t.test(mistime3$time_before,mistime3$time_after, alternative = "greater" , paired = TRUE)

#conclusion - the training program improved efficiency has been increase.


vas <- read.csv("C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/DataSet imarticus/day5 hypothesis/VAS DATA.csv")

head(vas)

A <- subset(vas,Group == "A") 
head(A)
B <- subset(vas, Group == "B")
head (B)
t.test(A$VAS_before, A$VAS_after, alternative = "greater", paired = TRUE)
t.test(B$VAS_before, B$VAS_after, alternative = "greater", paired = TRUE)

vas$change <-  vas$VAS_before - vas$VAS_after
head(vas)

t.test(vas$change ~ vas$Group, alternative = "greater", paired = TRUE)

boxplot(change~Group, data = vas)

# 5 types of hypothesis test

?t.test 
?prop.test
?aov
?var.test.formula
?chisq.test

prop.test(140, 30, p = NULL, alternative = "two.sided", conf.level = 0.95, correct = TRUE)



# Paired sample T-test 

df <- read.csv("C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/DataSet imarticus/day5 hypothesis/PAIRED t TEST.csv")
head(df)

t.test(df$time_before,df$time_after, alternative = "greater",paired = TRUE)










