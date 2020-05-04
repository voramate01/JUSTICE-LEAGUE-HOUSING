#import Data
data <- read.csv('Train-house.csv')

#Explore the data
str(data)
head(data)

#Plot Histogram
hist(data$SALE.PRICE)
hist(data$LAND.SQUARE.FEET)
hist(data$GROSS.SQUARE.FEET)

#Summary of Continous features
summary(data$SALE.PRICE)
summary(data$LAND.SQUARE.FEET)
summary(data$GROSS.SQUARE.FEET)

#Plot Sale price with some features
plot(data$GROSS.SQUARE.FEET,data$SALE.PRICE)
title(main="Sale Price vs Gross Square Feet")
plot(data$LAND.SQUARE.FEET,data$SALE.PRICE)
title(main="Sale Price vs Land Square Feet")
plot(data$AGE_at_SaleDate,data$SALE.PRICE)

plot(data$BOROUGH, data$SALE.PRICE)
title(main="Sale Price vs BOROUGH")

data['Adj_Neighborhood']<-substring(data$NEIGHBORHOOD,2,4)
plot(data$Adj_Neighborhood, data$SALE.PRICE, ylab = 'Sale price', xlab = 'NeighborhoodCode')
title(main="Sale Price vs Neighborhood")


#Correlation between Target and some covariate
cor(data$SALE.PRICE,data$LAND.SQUARE.FEET)
cor(data$SALE.PRICE,data$GROSS.SQUARE.FEET)


# Check irregular value
sum(data$SALE.PRICE == 0)
sum(data$LAND.SQUARE.FEET == 0)
sum(data$GROSS.SQUARE.FEET == 0)


#Data Manupulation

#Remove rows that have area = 0
data <- data[data$LAND.SQUARE.FEET != 0 & data$GROSS.SQUARE.FEET != 0,]

#Remove outlier
data <- data[!(data$SALE.PRICE %in% boxplot(data$SALE.PRICE)$out),]
data <- data[!(data$LAND.SQUARE.FEET %in% boxplot(data$LAND.SQUARE.FEET)$out),]
data <- data[!(data$GROSS.SQUARE.FEET %in% boxplot(data$GROSS.SQUARE.FEET)$out),]

data$BUILDING.CLASS.CATEGORY <- as.numeric(factor(data$BUILDING.CLASS.CATEGORY))

unique(data$BUILDING.CLASS.CATEGORY)


type1 = data[data$BUILDING.CLASS.CATEGORY == 1,] 
type2 = data[data$BUILDING.CLASS.CATEGORY == 2,]
type3 = data[data$BUILDING.CLASS.CATEGORY == 3,] 
type4 = data[data$BUILDING.CLASS.CATEGORY == 4,] 
type5 = data[data$BUILDING.CLASS.CATEGORY == 5,] 
type6 = data[data$BUILDING.CLASS.CATEGORY == 6,] 
type7 = data[data$BUILDING.CLASS.CATEGORY == 7,] 
type8 = data[data$BUILDING.CLASS.CATEGORY == 8,] 
type9 = data[data$BUILDING.CLASS.CATEGORY == 9,] 
type10 = data[data$BUILDING.CLASS.CATEGORY == 10,] 
type11 = data[data$BUILDING.CLASS.CATEGORY == 11,]
type12 = data[data$BUILDING.CLASS.CATEGORY == 12,]
type13 = data[data$BUILDING.CLASS.CATEGORY == 13,]
type14 = data[data$BUILDING.CLASS.CATEGORY == 14,]
type15 = data[data$BUILDING.CLASS.CATEGORY == 15,]
type16 = data[data$BUILDING.CLASS.CATEGORY == 16,]
type17 = data[data$BUILDING.CLASS.CATEGORY == 17,]
type18 = data[data$BUILDING.CLASS.CATEGORY == 18,]
type19 = data[data$BUILDING.CLASS.CATEGORY == 19,]


par(mfrow=c(1,1))

plot(type1$GROSS.SQUARE.FEET,type1$SALE.PRICE,ylim=c(0,2000000),ylab = 'Sale price', xlab = 'Gross Square Feet')
points(type2$GROSS.SQUARE.FEET,type2$SALE.PRICE, col = 'red')
points(type3$GROSS.SQUARE.FEET,type3$SALE.PRICE, col = 'green')
title(main="Sale Price vs Gross Square Feet of Top3 property type")


#####Models######

#Split data for 70% as training dataset and 30% is testing dataset
train = data[1:14155,]
test = data[14156:20222,]

#Use mean the calculate RMSE, so we can use it to compare with models
mean(train$SALE.PRICE)
mean.predict = rep(mean(train$SALE.PRICE),6067)
sqrt(mean((mean.predict-test$SALE.PRICE)^2))

#Multiple Regression
mlm <- lm(SALE.PRICE ~ factor(BOROUGH)+factor(BUILDING.CLASS.AT.TIME.OF.SALE)+GROSS.SQUARE.FEET + factor(NEIGHBORHOOD), data = train)
summary(mlm)

test[!(test$NEIGHBORHOOD %in% train$NEIGHBORHOOD),2] <- NA
which(is.na(test['NEIGHBORHOOD']))
test[!(test$BUILDING.CLASS.AT.TIME.OF.SALE %in% train$BUILDING.CLASS.AT.TIME.OF.SALE),16] <- NA

pd_test <- predict(mlm,test)
pd_test[which(is.na(pd_test))] <- mean(train$SALE.PRICE)
sqrt(mean((pd_test-test$SALE.PRICE)^2))

#Polynomial regression
plm <- glm(SALE.PRICE ~ factor(BOROUGH)+poly(GROSS.SQUARE.FEET,5) + factor(BUILDING.CLASS.AT.TIME.OF.SALE)+factor(NEIGHBORHOOD), data = train)
summary(plm)

plm_test <- predict(plm, test)
plm_test[which(is.na(plm_test))] <- mean(train$SALE.PRICE)
sqrt(mean((plm_test-test$SALE.PRICE)^2))

#Ridge Regression
library(glmnet)

# Predictor variables
x <- model.matrix(SALE.PRICE ~ factor(BOROUGH) + poly(GROSS.SQUARE.FEET,5) + factor(BUILDING.CLASS.AT.TIME.OF.SALE) + factor(NEIGHBORHOOD),data)[,-1]
# Outcome variable
y <- data$SALE.PRICE


trainset=c(1:14155)
testset=(-trainset)
y.test=y[testset]


# Find the best lambda using cross-validation
set.seed(0) 
cv <- cv.glmnet(x[trainset,], y[trainset], alpha = 0)
# Display the best lambda value
cv$lambda.min

#Ridge Model
model <- glmnet(x[trainset,], y[trainset], alpha = 0, lambda = cv$lambda.min)
coef(model)

ridge.pred=predict(model,s=cv$lambda.min,newx=x[testset,])

sqrt(mean((ridge.pred-y.test)^2))

#Lasso Regression
# Find the best lambda using cross-validation
set.seed(0) 
cv.ls <- cv.glmnet(x[trainset,], y[trainset], alpha = 1)
# Display the best lambda value
cv.ls$lambda.min

# Lasso Model
model.ls <- glmnet(x[trainset,], y[trainset], alpha = 1, lambda = cv.ls$lambda.min)
coef(model.ls)

lasso.pred=predict(model.ls,s=cv.ls$lambda.min,newx=x[testset,])

sqrt(mean((lasso.pred-y.test)^2))

# Natural Splines 
NS=lm(SALE.PRICE~ns(GROSS.SQUARE.FEET, df =5)+NEIGHBORHOOD+BOROUGH+BUILDING.CLASS.AT.TIME.OF.SALE,data=train) # we can also specify the knots instead of df
NS.predict=predict(NS,test,se=T)
NS.predict$fit[which(is.na(NS.predict$fit))] <- mean(train$SALE.PRICE)
sqrt(mean((NS.predict$fit-y.test)^2))


########Prediction##################
testdata <- read.csv('Test.csv')

data$SALE.PRICE <- NULL
data <- rbind(data,testdata)

dim(test_x)
dim(x)

test_x <- model.matrix(~factor(BOROUGH) + poly(GROSS.SQUARE.FEET,5) + factor(BUILDING.CLASS.AT.TIME.OF.SALE) + factor(NEIGHBORHOOD),data)[,-1]

test_x <- test_x[,-c(which(!(colnames(test_x) %in% colnames(x))))]

remove=c(1:20222)
newtest=(-remove)

test_predict = predict(model.ls,s=cv.ls$lambda.min,newx=test_x[newtest,])

write.xlsx(test_predict, file = 'predict.xlsx', sheetName = 'predict', append = FALSE)

