satlevel <- read.csv("C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/DataSet imarticus/Day 6 ANOVA/One way anova.csv")

head(satlevel)
View(satlevel)

anovatable <- aov(formula =satindex~dept , data = satlevel )

summary(anovatable)

?aov


#2 way ANOVA

df <- read.csv("C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/DataSet imarticus/Day 6 ANOVA/Two way anova.csv")

head(df)

table <- aov(formula =satindex~dept+exp+dept*exp,data = df)
summary(table)


# to make 

gf <- read.csv("C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/DataSet imarticus/Day 6 ANOVA/onewaynew.csv")

head(gf)

mygf <- aov(formula = No~Region ,data = gf)

summary(mygf)

#conclusion - means sales increament are diffrent in all ZONE/Region or its not equally same among them


# 2 way anova



mydata <- read.csv("C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/DataSet imarticus/Day 6 ANOVA/CSI_Telecom.csv")

head(mydata)

dataage <- aov(formula = CSI~Type +Age_grp + Type*Age_grp, data = mydata)

summary(dataage)

# example 2

data7 <- read.csv("C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/DataSet imarticus/Day 6 ANOVA/ExcerciseIntencity.csv")

head(df_Exerandinten)

data_exer <- aov(formula = weight_loss~gender+excercise+gender*excercise, data = data7)

summary(data_exer)

#3 outputs for null 
#means we are rejecting null hypthesis that means there wieght loss among male and female diffrent
#there is siginificant diffrence among the male and female

#as.numeric(df_Exerandinten$gender)

data7$gender <- factor(data7$gender)

data7$scode[data7$gender=="M"] <- "1"
data7$scode[data7$gender=="F"] <- "2"

data7$scode <- factor(data7$scode)

head(data7)

boxplot(weight_loss~gender,data7)
barplot(weight_loss~gender,data7)


