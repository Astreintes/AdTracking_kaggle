library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(caTools)

df <- fread('train_sample.csv')


count(df$device==1)
count(df$app)
count(df$os)



df <- separate(df, click_time, c("year", "month", "day", "hour", "minute", "second"), 
               sep = "([- :])")


df$is_attributed <- as.factor(df$is_attributed)
df$device <- as.factor(df$device)
df$app <- as.factor(df$app)
df$os <- as.factor(df$os)
df$channel<-as.factor(df$channel)
df$day<-as.factor(df$day)
df$hour<-as.factor(df$hour)
df$minute<-as.factor(df$minute)
df$second<-as.factor(df$second)


df$year <- NULL
df$month <- NULL
df$attributed_time <- NULL
str(df)


split <- sample.split(df$is_attributed, SplitRatio = .75)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)
########
real_test <- fread('test.csv')
real_test <- separate(real_test, click_time, c("year", "month", "day", "hour", "minute", "second"), 
               sep = "([- :])")



real_test$device <- as.factor(real_test$device)
real_test$app <- as.factor(real_test$app)
real_test$os <- as.factor(real_test$os)
df$channel<-as.factor(df$channel)
df$day<-as.factor(df$day)
df$hour<-as.factor(df$hour)
df$minute<-as.factor(df$minute)
df$second<-as.factor(df$second)


df$year <- NULL
df$month <- NULL
df$attributed_time <- NULL

######
library(e1071)
model1 <- svm(is_attributed ~ ., 
                     data = train, 
                     type = 'C-classification', 
                     kernel = 'radial') 
# Previsões
# Previsões nos dados de treino
pred_train <- predict(model1, train) 
# Percentual de previsões corretas com dataset de treino
mean(pred_train == train$is_attributed)  
# Previsões nos dados de teste
pred_test <- predict(model1, test) 
# Percentual de previsões corretas com dataset de teste
mean(pred_test == test$is_attributed)  
# Confusion Matrix
table(pred_test, test$is_attributed)

# Criando o modelo
library(rpart)
model2 = rpart(is_attributed ~ . -ip, data = train, control = rpart.control(cp = .01)) 
# Previsões nos dados de teste
tree_pred = predict(model2, test, type='class')
# Percentual de previsões corretas com dataset de teste
mean(tree_pred==test$is_attributed) 
# Confusion Matrix
table(tree_pred, test$is_attributed)
