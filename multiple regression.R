#########Exercise 3############
#Clean Environment
rm(list=ls())
setwd("/Users/aishuhan/Desktop")

#read data
df <- read.csv("Ex3-HSB1.csv")

#Bivariate regression model
model_bi1 <- lm(socst ~ read, df)
summary(model_bi1)
model_bi2 <- lm(socst ~ locus, df)
summary(model_bi2)

#multiple regression model
model_mul1 <- lm (socst ~ read + locus, data=df)
summary(model_mul1)

#read data
df2 <- read.csv("Ex3-HSB2.csv")

model2_mul1 <- lm (socst ~ read + locus, data=df2)
summary(model2_mul1)

