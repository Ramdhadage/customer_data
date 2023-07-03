#import data
data1 <- read.csv("customer_data.csv", header=TRUE, stringsAsFactors=FALSE)
head(data1)
attach(data1) #s

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




#Item 2

#Gender, Education, Age, Spending

# Tab a: Propose model
#Age: Normal model
#Spending : Normal Model
#Education : Multinomial Model
#Gender : Bernouli Model

#Convert gender variable into numeric 1 as male and 0 as female.
gender_=ifelse(gender=="Male",1,0)

# Tab b: Estimate parameters

age_mean = mean(age, na.rm = TRUE)
age_sd = sd(age, na.rm = TRUE)
spending_mean = mean(spending, na.rm = TRUE)
spending_sd = sd(spending, na.rm = TRUE)
edu_probs = table(education) / nrow(data1)
gender_prob = mean(gender_)

est_param <- paste("Age: mean =", age_mean, ", standard deviation =", age_sd,
                   "Spending: mean =", spending_mean, ", standard deviation =", spending_sd,
                   "Education: probabilities =", paste(edu_probs, collapse = ", "),
                   "Gender: probability =", gender_prob)

# Tab c: Predictive analytics

k=unique(education)

age_prediction = rnorm(1, mean = age_mean, sd = age_sd)
spending_prediction = rnorm(1, mean = spending_mean, sd = spending_sd)
edu_prediction <- sample(k, size = 1, prob = edu_probs)
gender_prediction = rbinom(1, size = 1, prob = gender_prob)

pred <- paste("Age: prediction =", age_prediction, "\n",
              "Spending: prediction =", spending_prediction, "\n",
              "Education: prediction =", edu_prediction, "\n",
              "Gender: prediction =", gender_prediction)

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
mean_=60000 #user input//s
t.test(var1,mu=mean_, conf.level = 0.95)




