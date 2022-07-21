# dealing with NA

#replacing with NA

x <- c(1,23,45,NA,115,78,NA)
sum(x)

sum(x,na.rm = TRUE)

is.na(x)

x = x[!is.na(x)] #removing on R
x

# NA replacing with median
set.seed(1112)
x1 <- LETTERS[1:20]
x1
x2 <- sample(c(NA,rpois(19,8)),20,replace=TRUE)
x2
df<- data.frame(x1,x2)

df$x2[is.na(df$x2)] <- median(df$x2,na.rm = TRUE) #right side median and left side storing
df


bike <- read.csv("C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/DataSet imarticus/day 7 EDA/bike_buyers.csv")

head(bike)
View(bike)

str(bike)

colSums(is.na(bike))

str(bike$Children)

t<- ceiling(mean(bike$Children, na.rm = TRUE)) #ceiling roudn of value

#ceiling(mean(bike$Children, na.rm = TRUE))


#median(bike$Children, na.rm = TRUE)

bike$Children[is.na(bike$Children)] <- t

#bike$Children[is.na(bike$Children)] <- ceiling(mean(bike$Children, na.rm = TRUE))
summary(bike$Children)

colSums(is.na(bike))


temp <- na.omit(bike) #creating duplicate and removing all NA

str(temp)

colSums(is.na(temp))

table(bike$Gender)

table(bike$Marital.Status)

#df$maritial.status <- as.character(df$maaritial.status)

# NA ko replace with blank

barplot(bike~bike$Marital.Status)



hist(bike$Income)
hist(bike$Age)

genqty <- table(temp$Gender)
genqty

barplot(genqty) #now u have to remove or replace 10 blank 

#write.csv task 1 

write.csv(temp,"C:/Users/Admin.DESKTOP-Q9CVR13/Desktop/DataSet imarticus/day 7 EDA//temp_data.csv")
