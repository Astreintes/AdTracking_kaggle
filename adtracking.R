library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(caTools)

# Load train Dataset

df <- fread('train_sample.csv')

# Split the column click_time into other columns 

df <- separate(df, click_time, c("year", "month", "day", "hour", "minute", "second"), 
               sep = "([- :])")

# Transforming the main columns into factor 
# (I till don't figure out how to make a function to do it automaticaly)

df$is_attributed <- as.factor(df$is_attributed)
df$device <- as.factor(df$device)
df$app <- as.factor(df$app)
df$os <- as.factor(df$os)
df$channel<-as.factor(df$channel)
df$day<-as.factor(df$day)
df$hour<-as.factor(df$hour)
df$minute<-as.factor(df$minute)
df$second<-as.factor(df$second)

# Deleting the columns won't use

df$year <- NULL
df$month <- NULL
df$attributed_time <- NULL

# Visualizing the class of all columns

str(df)

# Split data into train and test

split <- sample.split(df$is_attributed, SplitRatio = .75)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)


# Creating a support vector machine model

library(e1071)
model1 <- svm(is_attributed ~ device + app + os + channel, 
              data = train, 
              type = 'C-classification', 
              kernel = 'radial') 

# Prediction train data
pred_train <- predict(model1, train) 

# Mean correct prediction
mean(pred_train == train$is_attributed)  

# Prediction test data
pred_test <- predict(model1, test) 

# mean correct prediction 
mean(pred_test == test$is_attributed)  

# Confusion Matrix
table(pred_test, test$is_attributed)

# Creating a REgression tree model 
library(rpart)
model2 = rpart(is_attributed ~ device + app + os + channel, data = train, control = rpart.control(cp = .01)) 

# Test prediction
tree_pred = predict(model2, test, type='class')

# Mean correct prediction
mean(tree_pred==test$is_attributed) 

# Confusion Matrix
table(tree_pred, test$is_attributed)

##### Deploy the model in the real test data


real_test <- fread('test.csv')
real_test$ip <- NULL
real_test <- separate(real_test, click_time, c("year", "month", "day", "hour", "minute", "second"), 
                      sep = "([- :])")



real_test$device <- as.factor(real_test$device)
real_test$app <- as.factor(real_test$app)
real_test$os <- as.factor(real_test$os)
real_test$channel<-as.factor(real_test$channel)
real_test$day<-as.factor(real_test$day)
real_test$hour<-as.factor(real_test$hour)
real_test$minute<-as.factor(real_test$minute)
real_test$second<-as.factor(real_test$second)


real_test$click_time <- NULL
real_test$month <- NULL
real_test$attributed_time <- NULL

real_test$is_attributed <- predict(model2, real_test, type = 'class')


write.csv(real_test, 'sample_submission.csv', row.names = FALSE)
