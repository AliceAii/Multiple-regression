#########Exercise 1################
#clean environment and set work directory
rm(list=ls())
setwd("/Users/aishuhan/Desktop")

#read data
df <- read.csv("API.csv")
head(df)

#disply summary statistics 
nrow(df)
summary(df)

summary(df$api)
sd(df$api)
summary(df$pctcred)
sd(df$pctcred)
summary(df$pctel)
sd(df$pctel)

#generate histrograms
hist(df$api, prob = TRUE, main = "Histogram of API", xlab = "API Score", breaks = 50)
curve(dnorm(x, mean = mean(df$api), sd = sd(df$api)), add = TRUE)

hist(df$pctcred, prob = TRUE, main = "Histogram of pctcred", xlab = "pctcred", breaks = 50)
curve(dnorm(x, mean = mean(df$pctcred), sd = sd(df$pctcred)), add = TRUE)

hist(df$pctel, prob = TRUE, main = "Histogram of pctel", xlab = "pctel", breaks = 50)
curve(dnorm(x, mean = mean(df$pctel), sd = sd(df$pctel)), add = TRUE)

#linear regression api ~ pctcred
model1 <- lm(api ~ pctcred, df)
summary(model1)
b0_1 <- coefficients(model1)[1]
b0_1
b1_1 <- coef(model1)[2]
b1_1
X_1 <- c(mean(df$pctcred)-sd(df$pctcred), min(df$pctcred), mean(df$pctcred), max(df$pctcred), max(df$pctcred)+sd(df$pctcred))
Yhat_1 <- b0_1 + b1_1*X_1
tbl_1 <- cbind(X_1, Yhat_1)
colnames(tbl_1) <- c("pctcred", "api")
rownames(tbl_1) <- c("lpv", "min", "mean", "max", "hpv")
tbl_1

#linear regression api ~ pctel
model2 <- lm(api ~ pctel, df)
summary(model2)
b0_2 <- coefficients(model2)[1]
b0_2
b1_2 <- coef(model2)[2]
b1_2
X_2 <- c(mean(df$pctel)-sd(df$pctel), min(df$pctel), mean(df$pctel), max(df$pctel), max(df$pctel)+sd(df$pctel))
Yhat_2 <- b0_2 + b1_2*X_2
tbl_2 <- cbind(X_2, Yhat_2)
colnames(tbl_2) <- c("pctel", "api")
rownames(tbl_2) <- c("lpv", "min", "mean", "max", "hpv")
tbl_2



