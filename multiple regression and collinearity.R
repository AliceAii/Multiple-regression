#########Exercise 4 Multiple Regression and Collinearity###########
rm(list = ls())
setwd("/Users/aishuhan/Desktop/UCLA PhD/UCLA Course/EDUC 230B Multiple Regression/Assignments/Exercise 4_Multiple Regression and Exam Collinearity")

#read data
df <- read.csv("Ex4-HSB.csv")
#summarize the variables
mean(df$socst)
sd(df$socst)
mean(df$locus)
sd(df$locus)
mean(df$read)
sd(df$read)
mean(df$write)
sd(df$write)
# get the correlation matrix
cmatrix <- data.frame(cor(df[,c("socst", "locus", "read", "write")]))
View(cmatrix)

#get the VIF and Tolerance score
regress <- lm(socst ~ locus + read + write, data=df)
summary(regress)
library(car)
vif_values <- vif(regress) 
tolerance_values <- 1/vif(regress)

collinearity_indices <- data.frame(
  Tolerance = round(tolerance_values, 3),
  VIF = round(vif_values, 3))
  
collinearity_indices

#locus model to check the multicollinearity
model_locus <- lm(locus ~ read + write, data=df)
summary(model_locus)

#Comparisons of Nested Models
# Fit the first model with only 'read' as the predictor
model1 <- lm(socst ~ read, data = df)
summary(model1)

# Calculate the standard deviation of 'read' (SD_x) and 'socst' (SD_y)
SD_read <- sd(df$read)
SD_socst <- sd(df$socst)

# Unstandardized slope (b) for 'read' from the model summary
b_read <- 0.57686

# Calculate the standardized slope (beta) for 'read'
beta_read <- b_read * (SD_read / SD_socst)

# Output the standardized slope
print(beta_read)

# Fit the second model with 'read' and 'write' as the predictors
model2 <- lm(socst ~ read + write, data = df)
summary(model2)

beta_read2 <- 0.35896*(sd(df$read)/sd(df$socst))
beta_read2

beta_write2 <- 0.36007*(sd(df$write)/sd(df$socst))
beta_write2

# Fit the third model with 'read', 'write', and 'loc' as the predictors
model3 <- lm(socst ~ read + write + locus, data = df)
summary(model3)

beta_read3 <- 0.35391*(sd(df$read)/sd(df$socst))
beta_read3

beta_write3 <- 0.35568*(sd(df$write)/sd(df$socst))
beta_write3 

beta_locus3 <- 0.31068*(sd(df$locus)/sd(df$socst))
beta_locus3

#Get standardized slopes (betas)
df$socst_z <- scale(df$socst) #standardize all used vars
df$read_z <- scale(df$read)
df$write_z <- scale(df$write)
df$locus_z <- scale(df$locus)

regressall_st <- lm(socst_z ~ read_z + write_z + locus_z, data=df) #enter standardized vars into model
summary(regressall_st)




