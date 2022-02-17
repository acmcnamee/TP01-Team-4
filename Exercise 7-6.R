#### APPLIED EXERCISE 7-6
rm(list=ls())









# Loading and inspecting the dataset.
# This dataset contains income and demographic information for males who
# reside in the central Atlantic region of the United States.
library(ISLR2)
data(Wage)
View(Wage)

summary(Wage)
str(Wage)
dim(Wage)








# (1) Perform polynomial regression to predict wage using age. Use
# cross-validation to select the optimal degree d for the polynomial. What degree was chosen, and how does this compare to
# the results of hypothesis testing using ANOVA? Make a plot of the resulting polynomial fit to the data.


# (a) Using cross-validation:
set.seed(1)
train <- sample(3000,1500)
train_wage <- Wage[train,]
test_wage <- Wage[-train,]
lm.fit <- lm(wage~age, data=train_wage)









# calculating our MSE
lm.fit.mse <- mean((wage - predict(lm.fit,Wage))[-train]^2)

# Using the poly() function to create additional lm models with different degree polynomials
lm.fit2 <- lm(wage~poly(age,2), data=train_wage)
lm.fit2.mse <- mean((wage - predict(lm.fit2,Wage))[-train]^2)

lm.fit3 <- lm(wage~poly(age,3), data=train_wage)
lm.fit3.mse <- mean((wage - predict(lm.fit3,Wage))[-train]^2)

lm.fit4 <- lm(wage~poly(age,4), data=train_wage)
lm.fit4.mse <- mean((wage - predict(lm.fit4,Wage))[-train]^2)

lm.fit5 <- lm(wage~poly(age,5), data=train_wage)
lm.fit5.mse <- mean((wage - predict(lm.fit5,Wage))[-train]^2)

lm.fit.mse; lm.fit2.mse; lm.fit3.mse; lm.fit4.mse; lm.fit5.mse

# Based on calculating mse through cross-validation, our optimal degree d 
# for the polynomial predicting wage using age appears to be 4.



# (b) Hypothesis testing using ANOVA:

fit.1 <- lm(wage~age, data=Wage)
fit.2 <- lm(wage~poly(age,2), data=Wage)
fit.3 <- lm(wage~poly(age,3), data=Wage)
fit.4 <- lm(wage~poly(age,4), data=Wage)
fit.5 <- lm(wage~poly(age,5), data=Wage)


# generating f-stats using ANOVA
anova(fit.1,fit.2,fit.3,fit.4,fit.5)


# The anova test also shows that a fourth-degree polynomial is appropriate for this dataset.



# (c) Plotting the result

# We now create a grid of values for age at which we want predictions.

agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(lm.fit4,newdata=list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)

# Plotting the data and adding the fit from the degree-4 polynomial
par(mfrow=c(1,1), mar=c(4.5, 4.5, 1, 1), oma=c(0,0,0,0))
plot(age, wage, xlime=agelims, cex=.5, col="darkgrey")
title("Degree-4 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)





## (2) Fit a step function to predict wage using age, and perform cross-validation to choose 
# the optimal number of cuts. Make a plot of the fit obtained.

# In order to fit a step function, we use the cut() function
# The function cut() returns an ordered categorical variable; the lm() function then
# creates a set of dummy variables for use in the regression.

fit.step2 <- lm(wage~cut(age,2), data=train_wage)
fit.step2.mse <- mean((wage - predict(fit.step2,Wage))[-train]^2)

fit.step3 <- lm(wage~cut(age,3), data=train_wage)
fit.step3.mse <- mean((wage - predict(fit.step3,Wage))[-train]^2)

fit.step4 <- lm(wage~cut(age,4), data=train_wage)
fit.step4.mse <- mean((wage - predict(fit.step4,Wage))[-train]^2)

fit.step5 <- lm(wage~cut(age,5), data=train_wage)
fit.step5.mse <- mean((wage - predict(fit.step5,Wage))[-train]^2)

fit.step6 <- lm(wage~cut(age,6), data=train_wage)
fit.step6.mse <- mean((wage - predict(fit.step6,Wage))[-train]^2)

fit.step7 <- lm(wage~cut(age,7), data=train_wage)
fit.step7.mse <- mean((wage - predict(fit.step7,Wage))[-train]^2)

fit.step8 <- lm(wage~cut(age,8), data=train_wage)
fit.step8.mse <- mean((wage - predict(fit.step8,Wage))[-train]^2)

fit.step2.mse; fit.step3.mse; fit.step4.mse; fit.step5.mse; fit.step6.mse; fit.step7.mse; fit.step8.mse



# 7 appears to be our optimal number of cut-points


# Producing predictions and se bands
agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit.step7,newdata=list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)

# Plotting the data
par(mfrow=c(1,1), mar=c(4.5, 4.5, 1, 1), oma=c(0,0,0,0))
plot(age, wage, xlime=agelims, cex=.5, col="darkgrey")
title("7-Cut Step Function", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

