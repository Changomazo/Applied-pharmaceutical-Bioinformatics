# Setting the working directory
setwd("C:/Users/Albert Blanchart/Desktop/Applied Pharmaceuticals Bioinformatics/")

# Download the data and load into R
# Downloading the data
fileURL <- "http://pele.farmbio.uu.se/practicalpharmbio/resources/exam.Rdata"
if(!file.exists("./exam.Rdata")) {
        download.file(fileURL, destfile = "exam.Rdata")
}

load(file = "exam.Rdata") #generates a data frame call dataset (original name :P)

# Lets proceed to clean the database object a bit
data1 <- dataset[,apply(dataset,2, function(x){!any(is.na(x))})] #remove na values
data.clean <- data1[,which(apply(data1, 2, var) > 0.5)] #variables with var higher than 0.5

# Splitting the resulting dataset in training and test sets
library(caret)
set.seed(1234)
inTrain <- createDataPartition(y = data.clean$response, p = .6, list = FALSE)
data.training <- data.clean[inTrain, ]
data.test <- data.clean[-inTrain, ]

# Linear Regression model 1
fit1 <- lm(response ~ XLogP, data = data.training)

# Linear regression model 2
fit2 <- lm(response ~ XLogP + TopoPSA + MW, data = data.training)

# RandomForest model
library(randomForest)
fit3 <- randomForest(response ~ ., data = data.training, importance = TRUE)

# Now lets check how well these models have performed 
summary(fit1) #a R squared of 0.71
plot(data.training$XLogP, fit1$residuals) #no patterns seen
summary(fit2) # R squared 0.80
par(mfrow = c(1, 3))
plot(data.training$XLogP, fit2$residuals)
plot(data.training$TopoPSA, fit2$residuals) #Some patterns can be seen here
plot(data.training$MW, fit2$residuals)
fit3 # % Var explained is 89.4 which expressed as R-aquared corresponds to 0.89
varImpPlot(fit3) #and XlogP wins the price of being up to 2.5x times greater impact that the second most influencial variable ALogP

# Lets see how do they perfomr with the validation data
fit1.predict <- predict(fit1, newdata = data.test)
fit2.predict <- predict(fit2, newdata = data.test)
fit3.predict <- predict(fit3, newdata = data.test)

# Lets see how the predicted values correlate with the ones in the validation dataset
par(mfrow = c(1, 3))
plot(data.test$response, fit1.predict)
abline(c(0,1), col="red")
plot(data.test$response, fit2.predict)
abline(c(0,1), col="red")
plot(data.test$response, fit3.predict) #It shows the highest correlation pattern
abline(c(0,1), col="red")

cor(data.test$response, fit1.predict)
cor(data.test$response, fit2.predict)
cor(data.test$response, fit3.predict) #the correlation is 95%

#Rondomforest is our winner here since the performance of the predictions
#are really good.

save.image(file = "task2.Rdata")
