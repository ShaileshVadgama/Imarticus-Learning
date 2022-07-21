# Project on Logistic Regression 


dfc <- read.csv("C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/DataSet imarticus/day 11 Project on Logistic regression/default of credit card clients.csv")

head(dfc)
typeof(dfc)
summary(dfc)



colnames(dfc) <- as.character(unlist(dfc[1,])) #removing 1st colum and making heading
dfc = dfc[-1,]

head(dfc)

colnames(dfc)[colnames(dfc)=="default payment next month"] <- "DEFAULT_PAYMENT"

head(dfc)
summary(dfc)
dim(dfc)

colSums(is.na(dfc)) #showing No NA 

#boxplot(dfc$AGE) #ghar pe

str(dfc)

# removing NA
df$Avg..Area.Income[is.na(df$Avg..Area.Income)] <- median(df$Avg..Area.Income,na.rm = TRUE)

dfc[,1:25] <- sapply(dfc[,1:25],as.numeric) #convertng everthing in numeric
str(dfc)


dfc$AGE[is.na(dfc$AGE)] <- round(mean(dfc$AGE,na.rm = TRUE),0)

colSums(is.na(dfc))
str(dfc)
head(dfc)

dfc <- na.omit(dfc)
colSums(is.na(dfc))

table(dfc$EDUCATION)

# replace 0's with NAN, replace others too


dfc$EDUCATION[dfc$EDUCATION == 0] <- 4
dfc$EDUCATION[dfc$EDUCATION == 5] <- 4
dfc$EDUCATION[dfc$EDUCATION == 6] <- 4

table(dfc$EDUCATION) #checking all perfect

table(dfc$MARRIAGE) 

dfc$MARRIAGE[dfc$MARRIAGE == 0] <- 3

table(dfc$MARRIAGE) #checking all perfect

dim(dfc)

dfc[,1:24] <- scale(dfc[,1:24])

head(dfc)

set.seed(123)

library(caTools)

split<- sample.split(dfc, SplitRatio = 0.7)
split

traing <- subset(dfc, split ==TRUE)

testing <- subset(dfc, split ==FALSE)

dim(dfc)
dim(traing)
dim(testing)

cr <- cor(dfc)

library(corrplot)
?corrplot

corrplot(cr,type = "lower")
corrplot(cr,method = "number")

null <- glm(DEFAULT_PAYMENT~1, family = binomial, data = traing)
full <- glm(DEFAULT_PAYMENT~.,family = binomial, data = traing)

step(full,scope=list(lower = null, upper = full),direction = "both")

summary(full) # with full or null its giving us final model by formula

model_final <-   glm(formula = DEFAULT_PAYMENT ~ LIMIT_BAL + SEX + EDUCATION + 
             MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3  + BILL_AMT1 + 
               PAY_AMT1 + PAY_AMT2 + PAY_AMT4, 
           family = binomial, data = traing)

summary(model_final) # is final model

#checing multicolinearity

library(car)
vif(model_final) # should be always less then 5

colnames(traing)

# calculate prediected probabilitues using data

traing$Prediction_probab <- predict(model_final,traing, type = 'response')
head(traing$Prediction_probab)
head(traing$DEFAULT_PAYMENT) #just checking with sample

traing$Prediction_Y <- ifelse(traing$Prediction_probab > 0.5,1,0) # replacing value by ifelse by greater than 0.5 then 1 or else 0
head(traing)

#table(traing$DEFAULT_PAYMENT, traing$Prediction_probab)

accuracy <- table(traing$Prediction_Y, traing$DEFAULT_PAYMENT) #making accuracy table
accuracy

sum(accuracy)
sum(diag(accuracy))
sum(diag(accuracy)/sum(accuracy)) # finding accuracy of model
