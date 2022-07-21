#EDA

mydata <- iris

head(mydata)
str(mydata) #stretcher very IMP

levels(mydata$Species) #displaying kitna diffrent variation he wo
unique(mydata$Species) #max value in column

table(mydata$Species) #giving frequency and count

SL <- mydata$Sepal.Length # ungroup data
SL

mean(SL)
median(SL)
mode(SL)

y <- table(SL)
y

m <- names(y)[which(y == max(y))] #most frequent no. eg mode
m   

sd(SL) #sd and var calculate
var(SL)

summary(SL)
quantile(SL) #calcutate IQR
summary(mydata)

hist(SL)
barplot(SL)
?hist

MS <- mydata$Species

t <- table(MS)
barplot(t)

typeof(t)
typeof(mydata$Species)

head(mydata) #bcz all data in diff so doing in same scale 
mydata_numeric <- mydata[,c(1,2,3,4)] #blank means taking all rows 
mydata_numeric
zscore_data <- scale(mydata_numeric)
head(zscore_data)
hist(zscore_data)
