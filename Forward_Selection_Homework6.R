library(tidyverse)
library(caret)
library(mlbench)
library(glmnet)
library(glmnetUtils)

read_csv("/Users/vidit/Desktop/Masters/Sem_3/MSCS 6520/Homework6/criteo_ad_click.csv", col_names = FALSE)

criteo_ad_click <- read_csv("/Users/vidit/Desktop/Masters/Sem_3/MSCS 6520/Homework6/criteo_ad_click.csv", col_names = FALSE)
View(criteo_ad_click)

criteo_ad_click <- as.tibble(criteo_ad_click)

head(criteo_ad_click)

col <- c("X1", "X15", "X16", "X17", "X18", "X19", "X20","X21", "X22")
criteo_ad_click[col] <- lapply(criteo_ad_click[col], factor)
head(criteo_ad_click)

# BoxPlot

ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X2))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X3))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X4))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X5))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X6))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X7))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X8))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X9))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X10))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X11))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X12))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X13))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X14))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X15))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X16))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X17))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X18))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X19))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X20))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X21))
ggplot(data = criteo_ad_click) + geom_boxplot(mapping = aes(x = X1, y = X22))

# JitterPlot

ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X2))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X3))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X4))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X5))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X6))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X7))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X8))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X9))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X10))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X11))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X12))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X13))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X14))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X15))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X16))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X17))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X18))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X19))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X20))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X21))
ggplot(data = criteo_ad_click) + geom_jitter(mapping = aes(x = X1, y = X22))

set.seed(221)
trainIndex <- createDataPartition(criteo_ad_click$X1, p = 0.8, list = FALSE, times = 1)

criteo_ad_click[trainIndex, ]
criteoTrain <- criteo_ad_click[trainIndex, ]

criteo_ad_click[-trainIndex, ]
criteoTest <- criteo_ad_click[-trainIndex, ]

preProcess(criteoTrain, method = c("center", "scale"))
scaler <- preProcess(criteoTrain, method = c("center", "scale"))

predict(scaler, criteoTrain)
criteoTrain <- predict(scaler, criteoTrain)
predict(scaler, criteoTest)
criteoTest <- predict(scaler, criteoTest)

# Round: 1

lr <- glmnet(X1 ~ .,
             data = criteoTrain,
             family = "binomial",
             na.action = na.pass)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X3,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X4,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X5,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)


lr <- glmnet(X1 ~ X2 + X6,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X7,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X8,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X9,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)
predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)


lr <- glmnet(X1 ~ X2 + X10, 
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X12, 
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X13,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X14,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X15,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X16, 
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X17, 
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X18, 
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X19, 
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)


lr <- glmnet(X1 ~ X2 + X20, 
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X21, 
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X22, 
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

# Round: 2

lr <- glmnet(X1 ~ X2 + X3 + X4,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X3 + X5,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X3 + X6,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X3 + X7,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X3 + X8,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X9,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X10,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X3,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X12,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X13,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X14,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X15,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X16,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X18,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X19,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X20,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X21,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X22,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                      criteoTest,
                      type = "class",
                      na.action = na.pass,
                      s = 0.01)

predictions1 <- predict(lr,
                       criteoTest,
                       type = "response",
                       na.action = na.pass,
                       s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

# Round: 3

lr <- glmnet(X1 ~ X2 + X11 + X17 + X3,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X4,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X5,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X6,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X7,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X8,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X9,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X12,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X13,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X14,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X15,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X16,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X18,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X19,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X20,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X21,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X22,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

# Round: 4

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X3,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)
 
lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X4,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X5 ,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X6,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X7,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X8,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X9,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X12,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X13,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)


lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X15,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)


lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X16,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X18,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X19,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X20,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X21,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X22,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

#Round: 5

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X3,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X4,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X5,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X6,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X7,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X8,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X9,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X12,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X13,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X15,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X16,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X18,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X19,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X20,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X22,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

# Round: 6

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X3,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X4,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X5,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X6,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X7,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X8,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X9,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X12,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X13,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X15,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X16,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X19,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X20,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X22,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

# Raound: 7

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X3,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X4,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X5,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X6,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X7,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X8,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X9,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X12,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X15,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X16,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X19,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X20,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X22,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

# Round: 8

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13 + X3,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13 + X4,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13 + X5,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13 + X6,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13 + X7,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13 + X8,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13 + X9,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13 + X12,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13 + X15,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13 + X16,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13 + X19,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13 + X20,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ X2 + X11 + X17 + X10 + X14 + X21 + X18 + X13 + X22,
             data = criteoTrain,
             family = "binomial",
             na.action = na.omit)
predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)

predictions1 <- predict(lr,
                        criteoTest,
                        type = "response",
                        na.action = na.pass,
                        s = 0.01)

head(predictions)
head(predictions1)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)


# Rpund: 9 Multinomial

lr <- glmnet(X1 ~ . - X2 - X3 + X4 : X5,
             data = criteoTrain,
             family = "multinomial",
             na.action = na.omit)

 predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ . - X4 - X5 + X6 : X7,
             data = criteoTrain,
             family = "multinomial",
             na.action = na.omit)

predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ . - X6 - X7 + X8 : X9,
             data = criteoTrain,
             family = "multinomial",
             na.action = na.omit)

predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ . - X8 - X9 + X10 : X11,
             data = criteoTrain,
             family = "multinomial",
             na.action = na.omit)

predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ . - X10 - X11 + X12 : X13,
             data = criteoTrain,
             family = "multinomial",
             na.action = na.omit)

predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ . - X12 - X13 + X14 : X15,
             data = criteoTrain,
             family = "multinomial",
             na.action = na.omit)

predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ . - X14 - X15 + X16 : X17,
             data = criteoTrain,
             family = "multinomial",
             na.action = na.omit)

predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ . - X16 - X17 + X18 : X19,
             data = criteoTrain,
             family = "multinomial",
             na.action = na.omit)

predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)

lr <- glmnet(X1 ~ . - X18 - X19 + X20 : X21,
             data = criteoTrain,
             family = "multinomial",
             na.action = na.omit)

predictions <- predict(lr,
                       criteoTest,
                       type = "class",
                       na.action = na.pass,
                       s = 0.01)
head(predictions)
confusionMatrix(as.factor(predictions),
                criteoTest$X1)






