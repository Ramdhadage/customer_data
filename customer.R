#import data
data1 <- read.csv("C:\\Users\\ASUS\\Downloads\\customer\\customer_data.csv", header=TRUE, stringsAsFactors=FALSE)
head(data1)

library(ggplot2)
library(tidyverse)
library(palmerpenguins)
library(ggbeeswarm)
library(ggforce)
library(caret)

##item1 
##tab1
#Density plot on gender and age of the customer
cols <- c("#66CC99", "#F76D5E", "#FFFFBF", "#72D8FF" )
# Basic density plot in ggplot2
ggplot(data1, aes(x = age, colour = gender)) +
  geom_density(lwd = 1.2, linetype = 1) + 
  scale_color_manual(values = cols)

#Density plot on education and income of the customer
cols <- c("#66CC99", "#F76D5E", "#FFFFBF", "#72D8FF" )
# Basic density plot in ggplot2
ggplot(data1, aes(x = income, colour = education)) +
  geom_density(lwd = 1.2, linetype = 1) + 
  scale_color_manual(values = cols)

#barplot 
ggplot(data1, aes(x=education)) + geom_bar(fill="Blue") + labs(x="Team")

#boxplot 
ggplot(data1, aes(x=gender, y=spending)) + geom_boxplot(fill='steelblue')


## tab 2
#Consider one of continous attributes, and compute central and variational measures

var1=income

summary(var1)

#variance
variance=var(var1)
variance

#standard deviation
std=sqrt(variance)
std

##tab c

#Chebychev's Rule :- It states that for any dataset, at least (1 - 1/k^2)
#of the data falls within k standard deviations of the mean, where k is any
#positive real number greater than 1.

#To propose a one-sigma interval using Chebyshev's rule, we can set k = 1.
#In this case, at least (1 - 1/1^2) = 0% of the data falls within one standard
#deviation from the mean. Since this result is not meaningful, we use empirical 
#method.

# Empirical Rule :- Calculate the mean (μ) and standard deviation (σ) of the variable in the dataset.
# The one-sigma interval would be [μ - σ, μ + σ].

var1=income
mn=mean(var1)
mn

vr1=var(var1)
std=sqrt(vr1)
std

ll=mn-std
ll
ul=mn+std
ul

plot(var1)
abline(h=ll,col='red')
abline(h=ul,col='red')


##tab d

var1=income

#by boxplot
boxplot(var1)

#by procedure 
q1=quantile(var1,0.25)
q1

q3=quantile(var1,0.75)
q3

iqr=q3-q1
ll=q1-1.5*iqr
ul=q3+1.5*iqr

plot(var1)
abline(h=ll,col='red')
abline(h=ul,col='red')



#item 2
##tab1, tab2 and tab3
#####split data into train and test
set.seed(1)
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(data1), replace=TRUE, prob=c(0.7,0.3))
train  <- data1[sample, ]
test   <- data1[!sample, ]


# Building the model 

#train the model by using age column as target variable and other as independent variables
model_age = lm(age ~ income + purchase_frequency + spending,  data = train)
model_age$coefficients
# prediction of the target variable using test dataset
model_age_pred <- predict(model_age, test)
#model performance metrics
data.frame( R2 = R2(model_age_pred, test $ age),
            RMSE = RMSE(model_age_pred, test $ age),
            MAE = MAE(model_age_pred, test $ age))

#train the model by using income column as target variable and other as independent variables
model_income = lm(income ~ age + purchase_frequency + spending,  data = train)
model_income$coefficients
# prediction of the target variable using test dataset
model_income_pred <- predict(model_income, test)
#model performance metrics
data.frame( R2 = R2(model_income_pred, test $ income),
            RMSE = RMSE(model_income_pred, test $ income),
            MAE = MAE(model_income_pred, test $ income))

#train the model by using purchase_frequency column as target variable and other as independent variables
model_purch = lm(purchase_frequency ~ income + age + spending,  data = train)
model_purch$coefficients
# prediction of the target variable using test dataset
model_purch_pred <- predict(model_purch, test)
#model performance metrics
data.frame( R2 = R2(model_purch_pred, test $ purchase_frequency),
            RMSE = RMSE(model_purch_pred, test $ purchase_frequency),
            MAE = MAE(model_purch_pred, test $ purchase_frequency))

#train the model by using spending column as target variable and other as independent variables
model_spend = lm(spending ~ income + purchase_frequency + age ,  data = train)
model_spend$coefficients
# prediction of the target variable using test dataset
model_spend_pred <- predict(model_spend, test)
#model performance metrics
data.frame( R2 = R2(model_spend_pred, test $ spending),
            RMSE = RMSE(model_spend_pred, test $ spending),
            MAE = MAE(model_spend_pred, test $ spending))



#item 3


## tab1
var1=gender
var2=education

chisq.test(var1,var2)


##tab2

var2=gender

n=length(unique(var2))

k=as.matrix(table(var2))

chisq.test(k,p=rep(1/n,n))


##tab c

var1=income
mu=60000
t.test(var1,mu=60000, conf.level = 0.95)




