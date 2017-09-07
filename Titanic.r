# STEP - 1 START #

install.packages("titanic")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("DAAG")
library(titanic)
library(rpart.plot)
library(gmodels)
library(Hmisc)
library(pROC)
library(ResourceSelection)
library(car)
library(caret)
library(dplyr)
library(InformationValue)
library(rpart)
library(randomForest)
library("DAAG")

cat("\014")

getwd()
setwd("D:\\vivek\\AMMA17\\Data") 
titan_train_m<-read.csv('train.csv')
titan_train<-titan_train_m
titan_train_3 <- read.csv('train.csv')

titan_test_mb <-read.csv('test-3.csv')


set.seed(1234) 
titan_train$rand <- runif(nrow(titan_train))
titan_train_start <- titan_train[titan_train$rand <= 0.7,]
titanic_test_start <- titan_train[titan_train$rand > 0.7,]

CrossTable(titan_train$Survived)

titan_train <- titan_train[!apply(titan_train[,c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Age")], 1, anyNA),]
titan_train_NA_allcols <- titan_train_m[!apply(titan_train_m[,c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Age")], 1, anyNA),]
nrow(titan_train_m)

mean_age = mean(titan_train_m$Age)
titan_trainmean_mb <- titan_train_start
titan_trainmean_mb2 <- titan_train_start
titan_trainmean_mb$Age[is.na(titan_trainmean_mb$Age)] = mean(titan_trainmean_mb$Age, na.rm = TRUE)
titan_trainmean_mb2$Age[is.na(titan_trainmean_mb2$Age)] = mean(titan_trainmean_mb2$Age, na.rm = TRUE)


full.model.titanic.mean <- glm(formula = Survived ~ Pclass + Sex + SibSp + Parch + Fare + Age,
                               data=titan_trainmean_mb, family = binomial) 

fit.train.mean <- lm(formula = Survived ~ Pclass + Sex + SibSp + Parch + Fare + Age,
                     data=titan_trainmean_mb2)
summary(fit.train.mean)

vif(fit.train.mean) 

titan_trainmean_mb$Parch<-NULL
full.model.titanic.mean <- glm(formula = Survived ~ Pclass + Sex + SibSp + Fare + Age,
                               data=titan_trainmean_mb, family = binomial) #family = binomial implies that the type of regression is logistic
summary(full.model.titanic.mean)

titan_trainmean_mb$Fare<-NULL
full.model.titanic.mean <- glm(formula = Survived ~ Pclass + Sex + SibSp + Age,
                               data=titan_trainmean_mb, family = binomial) #family = binomial implies that the type of regression is logistic
summary(full.model.titanic.mean)


titan_trainmean_mb$prob = predict(full.model.titanic.mean, type=c("response"))
titan_trainmean_mb$Survived.pred = ifelse(titan_trainmean_mb$prob>=.5,'pred_yes','pred_no')
table(titan_trainmean_mb$Survived.pred,titan_trainmean_mb$Survived)

nrow(titanic_test)
nrow(titan_testmean_mb)
titan_testmean_mb <- titanic_test_start

titan_testmean_mb$Age[is.na(titan_testmean_mb$Age)] = mean(titan_testmean_mb$Age, na.rm = TRUE)

titan_testmean_mb$prob = predict(full.model.titanic.mean, newdata=titan_testmean_mb, type=c("response"))
titan_testmean_mb$Survived.pred = ifelse(titan_testmean_mb$prob>=.5,'pred_yes','pred_no')
table(titan_testmean_mb$Survived.pred,titan_testmean_mb$Survived)


df.jackandrose <- read.csv('Book1.csv')
df.jackandrose$prob = predict(full.model.titanic.mean, newdata=df.jackandrose, type=c("response"))
df.jackandrose$Survived.pred = ifelse(df.jackandrose$prob>=.5,'pred_yes','pred_no')
head(df.jackandrose)


# Defining the K Fold function
kfoldfunc <- function(dataset,formula,family,k)
{
  object <- glm(formula=formula, data=dataset, family = family)
  CVbinary(object, nfolds= k, print.details=TRUE)
}

#calculate Mean Squared Error
MeanSquareError <- function(dataset,formula)
{
  LM_Object <- lm(formula=formula, data=dataset)
  LM_Object_sum <-summary(LM_Object)
  MSE <- mean(LM_Object_sum$residuals^2)
  print("Mean squared error")
  print(MSE)
}

Kobject <- kfoldfunc(titan_trainmean_mb,Survived ~ Pclass + Sex + SibSp + Age,binomial,10)

train_mse <-MeanSquareError(titan_trainmean_mb,Survived ~ Pclass + Sex + SibSp + Age)

table(titan_trainmean_mb$Survived,round(Kobject$cvhat))
print("Estimate of Accuracy")
print(Kobject$acc.cv)


Ktest_obj <- kfoldfunc(titan_testmean_mb,Survived ~ Pclass + Sex + SibSp + Age,binomial,10)


MSE_Test <-MeanSquareError(titan_testmean_mb,Survived ~ Pclass + Sex + SibSp + Age)

table(titan_testmean_mb$Survived,round(Ktest_obj$cvhat))
print("Estimate of Accuracy")
print(Ktest_obj$acc.cv)
© 2017 GitHub, Inc.
