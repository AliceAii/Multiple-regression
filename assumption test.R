#################Exercise 2##################3
#clean environment and set work directory
rm(list=ls())
setwd("/Users/aishuhan/Desktop")

#read data
df <- read.csv("Ex2.csv")
head(df)

#plot
plot(df$X, df$Y1)
plot(df$X, df$Y2)
plot(df$X, df$Y3)
plot(df$X, df$Y4)
plot(df$X, df$Y5)

#Univariate descriptive statistics (means, standard deviations, skewness, kurtosis, range)
#Skew should be between -1 and 1 and kurtosis between -2 and 2
library(psych)
describe(df$X)
describe(df$Y1)
describe(df$Y2)
describe(df$Y3)
describe(df$Y4) #skew is -1.07
#skew: Skewness measures the asymmetry of the data distribution. A negative skew (like -1.07 here) indicates that the data is skewed to the left, meaning it has a longer tail on the left side.
#Non-Normality: One potential problem for linear regression could be the non-normality of the "Y4" variable. The negative skewness and positive kurtosis values suggest that the data may not follow a perfectly normal distribution. Linear regression assumes that the residuals (the differences between observed and predicted values) are normally distributed. Departures from normality can affect the validity of statistical tests and confidence intervals in regression analysis.
describe(df$Y5) #kurtosis is 2.72
# Kurtosis measures the heaviness of the tails of the distribution. A kurtosis value of 2.72 is relatively high and suggests that the data has heavier tails compared to a normal distribution. High kurtosis can indicate the presence of outliers or extreme values in the data.

#check outliers
df$Y1Z <- scale(df$Y1) #195, 80

#Bivariate descriptive statistics (e.g., Pearson correlation coefficient, simple linear regression coefficients)
model1 <- lm(Y1 ~ X, df)
summary(model1)

#Examination of the residual values (and/or standardized residuals) 
#Model 1
resids1 <- residuals(model1)
rstandard1 <- scale(resids1)
hist(rstandard1, breaks=15, prob=TRUE)
curve(dnorm(x, mean = mean(rstandard1), sd = sd(rstandard1)), add = TRUE)

plot(model1)


