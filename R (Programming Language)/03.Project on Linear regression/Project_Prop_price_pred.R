df <- read.csv("C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/DataSet imarticus/day 9 Project/USA_Housing.csv")
head(df)

summary(df)
str(df)
attach(df)

colSums(is.na(df))

# removing NA and replacing NA

df$Avg..Area.Income[is.na(df$Avg..Area.Income)] <- median(df$Avg..Area.Income,na.rm = TRUE)

colSums(is.na(df))

df$Avg..Area.House.Age[is.na(df$Avg..Area.House.Age)] <- median(df$Avg..Area.House.Age,na.rm = TRUE)

colSums(is.na(df))

df$Avg..Area.Number.of.Rooms[is.na(df$Avg..Area.Number.of.Rooms)] <- median(df$Avg..Area.Number.of.Rooms,na.rm = TRUE)
colSums(is.na(df))

df$Avg..Area.Number.of.Bedrooms[is.na(df$Avg..Area.Number.of.Bedrooms)] <- median(df$Avg..Area.Number.of.Bedrooms,na.rm = TRUE)
colSums(is.na(df))

df$Area.Population[is.na(df$Area.Population)] <- median(df$Area.Population,na.rm = TRUE)
colSums(is.na(df))

df$Address <- NULL #removing Address

head(df)

library(corrplot)

cr <- cor(df)


corrplot(cr,type = "lower")
corrplot(cr,method = "number")

library(corrgram)
corrgram(cr)

#par(mfrow=c(2,2))

#scatter &boxplot

boxplot(df$Avg..Area.Income)

#outlier
dim(df)

# removing outlier of avg income

q1 <- quantile(df$Avg..Area.Income, .25)
q1
q3 <- quantile(df$Avg..Area.Income, .75)
q3

IQRing <- IQR(df$Avg..Area.Income)
IQRing

dim(df)

no_outlier <- subset(df,df$Avg..Area.Income >(q1 -1.5*IQRing) & df$Avg..Area.Income < (q3 +1.5*IQRing))

boxplot(no_outlier$Avg..Area.Income)

# removing outlier of age 

q1_a <- quantile(no_outlier$Avg..Area.House.Age, .25)

q3_a <- quantile(no_outlier$Avg..Area.House.Age, .75)


IQRing_a <- IQR(no_outlier$Avg..Area.House.Age)

no_outlier_a <- subset(no_outlier,no_outlier$Avg..Area.House.Age >(q1_a -1.5*IQRing_a) & no_outlier$Avg..Area.House.Age < (q3_a +1.5*IQRing_a))

dim(no_outlier_a)

boxplot(no_outlier_a$Avg..Area.House.Age)

# removing outlier of age rooms

q1_r <- quantile(no_outlier_a$Avg..Area.Number.of.Rooms, .25)

q3_r <- quantile(no_outlier_a$Avg..Area.Number.of.Rooms, .75)


IQRing_r <- IQR(no_outlier_a$Avg..Area.Number.of.Rooms)

no_outlier_r <- subset(no_outlier_a,no_outlier_a$Avg..Area.Number.of.Rooms >(q1_r -1.5*IQRing_r) & no_outlier_a$Avg..Area.Number.of.Rooms < (q3_r +1.5*IQRing_r))

dim(no_outlier_r)
boxplot(no_outlier_r$Avg..Area.Number.of.Rooms)


# removing outlier of age pop

q1_p <- quantile(no_outlier_r$Area.Population, .25)

q3_p <- quantile(no_outlier_r$Area.Population, .75)


IQRing_p <- IQR(no_outlier_r$Area.Population)

no_outlier_p <- subset(no_outlier_r,no_outlier_r$Area.Population >(q1_p -1.5*IQRing_p) & no_outlier_r$Area.Population < (q3_p +1.5*IQRing_p))

dim(no_outlier_p)
boxplot(no_outlier_p$Area.Population)

# removing outlier of age rooms bedrom

q1_b <- quantile(no_outlier_p$Avg..Area.Number.of.Bedrooms, .25)

q3_b <- quantile(no_outlier_p$Avg..Area.Number.of.Bedrooms, .75)


IQRing_b <- IQR(no_outlier_p$Avg..Area.Number.of.Bedrooms)

no_outlier_b <- subset(no_outlier_p,no_outlier_p$Avg..Area.Number.of.Bedrooms >(q1_b -1.5*IQRing_b) & no_outlier_p$Avg..Area.Number.of.Bedrooms < (q3_b +1.5*IQRing_b))

dim(no_outlier_b)
boxplot(no_outlier_p$Avg..Area.Number.of.Bedrooms)

# removing outlier of age rooms price

q1_pr <- quantile(no_outlier_b$Price, .25)

q3_pr <- quantile(no_outlier_b$Price, .75)


IQRing_pr <- IQR(no_outlier_p$Price)

no_outlier_pr <- subset(no_outlier_b,no_outlier_b$Price >(q1_pr -1.5*IQRing_pr) & no_outlier_b$Price < (q3_pr +1.5*IQRing_pr))

dim(no_outlier_pr)
boxplot(no_outlier_pr$Price)

#final 4854

# splting

set.seed(123)
library(caTools)

split<- sample.split(no_outlier_pr, SplitRatio = 0.7)
split

traing <- subset(no_outlier_pr, split ==TRUE)

testing <- subset(no_outlier_pr, split ==TRUE)

attach(no_outlier_pr)

lmmodel <- lm(Price~Avg..Area.House.Age +Avg..Area.Income + Avg..Area.Number.of.Bedrooms + Avg..Area.Number.of.Rooms, data = traing)

coef(lmmodel)
summary(lmmodel)

#since p-value of bedroom is not less than 0.5 thats why we are droping bedrooms 

lmmodel1 <- lm(Price~Avg..Area.House.Age +Avg..Area.Income + Avg..Area.Number.of.Rooms, data = traing)

coef(lmmodel1)
summary(lmmodel1)
res <- residuals(lmmodel1) #residual is showing showing diffrent between orignal and predicted 

res <- as.data.frame(res)

library(ggplot2)

testing$predictedPrice <- predict(lmmodel1,testing)
head(testing)


pl1 <- testing %>%
  ggplot(aes(Price,predictedPrice))+
  geom_point(alpha =0.5)+
  stat_smooth(aes(colour = 'black'))+
  xlab('Actual value of medv')+
  ylab('Predicted value of medv')+
  theme_bw()

library(plotly)
ggplotly(pl1) #showing value 

error <- testing$Price - testing$predictedPrice
rmse <- sqrt(mean(error^2))
print(error)
print(rmse)

