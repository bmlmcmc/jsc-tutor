setwd('D:/R')
head(iris)

install.packages('dplyr')

install.packages("ggplot2") # for visualization
install.packages("dplyr") # for data manipulation
install.packages("lmtest") # for model diagnostics
install.packages("stanglm") # for Bayesian linear model

library(ggplot2) # for visualization
library(dplyr) # for data manipulation
library(lmtest) # for diagnostics

## Linear regression

data_reg <- read.csv("https://raw.githubusercontent.com/bmlmcmc/jsc-tutor/main/Covid-Vaccine-data.csv") ## read data
str(data_reg) ## assessing the data structure

# Fit a linear regression model
model <- lm(Mortality ~ Positive, data = data_reg)
# View model summary
summary(model)

## dignostics
library(nortest)

cor(data_reg[,c('Positive','Mortality')]) # multicollinearity test
lillie.test(model$residuals) # normality test of residuals
bptest(model) # homoscedasticity test using Breusch-Pagan
bgtest(model) # autocorrelation test using Breusch-Godfrey

library(rstanarm)
model2 = stan_glm(Mortality ~ Positive, data = data_reg)
summary(model2)
model2$stan_summary[,c('mean','2.5%','97.5%')]
summary()


# Load the COVID-19 data
data_logreg <- read.csv("https://raw.githubusercontent.com/bmlmcmc/jsc-tutor/main/data_covid_synthetic.csv") ## read data
str(data_logreg) ## assessing the data structure
# Fit a logistic regression model
model <- glm(Lab_Result~., data = data_logreg)
# View model summary
summary(model)

exp(coefficients(model)[c('Fever','Cough','Cold','Headache','Malaise')])

library(ResourceSelection)
hoslem.test(data_logreg$Lab_Result,model$fitted.values)

anova(model)

library(rstanarm)
model2 = stan_glm(Lab_Result~., data = data_logreg,family = binomial(link = "logit"))
summary(model2)
model2$stan_summary[,c('mean','2.5%','97.5%')]

library(dplyr)
library(ggplot2)
library(reshape2)
library(fastDummies)
library(caret)

data <- read.csv('https://raw.githubusercontent.com/bmlmcmc/tabular-synthetic-data/main/data_covid_synthetic.csv')
data$HASILLAB <- factor(data$HASILLAB,c(0,1))

colnames(data)
set.seed(123)
trainIndex <- createDataPartition(data$HASILLAB,p = 0.8,list=F,times =1)
covid_train <- data[trainIndex,]
covid_test <- data[-trainIndex,]

## model, data, method, training control

model = x
model.fit(x_train,y_train)

fitControl <- trainControl(method='cv',number=5)
model1 <- train(HASILLAB~JenisKelamin+Demam,
                data=covid_train,method='svmRadial',
                trControl = fitControl) ## y=a+bx -> y~x
pred1 <- predict(model1,covid_test[,c('JenisKelamin','Demam')])
cm1 <- confusionMatrix(covid_test$HASILLAB,pred1,positive='1')
eval_res1 <- data.frame(eval=names(cm1$byClass),value=cm1$byClass,method='partial')

model2 <- train(HASILLAB~.,
                data=covid_train,method='svmRadial',
                trControl = fitControl) ## y=a+bx -> y~x
pred2 <- predict(model2,covid_test[,-15])
cm2 <- confusionMatrix(covid_test$HASILLAB,pred2,positive='1')
eval_res2 <- data.frame(eval=names(cm2$byClass),value=cm2$byClass,method='full')

eval_res <- rbind.data.frame(eval_res1,eval_res2)
ggplot(eval_res,aes(x=eval,y=value,fill=method))+
  geom_bar(stat='identity',position='dodge')

save(model2)
