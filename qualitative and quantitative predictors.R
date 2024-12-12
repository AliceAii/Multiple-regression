####Exercies 6_Linear Regression with Qualitative and Quantitative Predictors####
rm(list = ls())
library(dplyr)

setwd("/Users/aishuhan/Desktop")
df <- read.csv("Ex6-HSB.csv")

#compare means by Race, set factor first (will generate dummy variable automatically)
df$RACE_ <- factor(df$race,
                  levels = c(4, 1, 2, 3), #set white as reference group
                  labels = c("White", "Hispanic", "Asian", "African-American"))

str(df$race)
str(df$RACE)

df %>% 
  group_by(RACE_) %>%
  summarise(
    mean = mean(science, na.rm = TRUE),
    n = n(),
    sd = sd(science, na.rm = TRUE)
  )

#model 1: Qualitative variable (RACE) as the only predictor for science score
model_1 <- lm(science ~ RACE_, data = df)
summary(model_1)

model_1b <- lm(science ~ locus, data = df)
summary(model_1b)

#another methods to do it: creat dummy variable in a direct way 
data <- df %>%
  mutate(hispanic = as.numeric(race == 1),
         asian = as.numeric(race == 2),
         african_american = as.numeric(race == 3),
         # Not creating a dummy for 'White' to avoid the dummy variable trap
         other = as.numeric(race == 4))

model_1a <- lm(science ~ hispanic + asian + african_american, data = data)
summary(model_1a)

#model 2: Qualitative (race) and quantitative (locus) both as predictors for science score
model_2 <- lm(science ~ RACE_ + locus, data = df)
summary(model_2)

model_2b <- lm(science ~ locus + RACE_, data = df)
summary(model_2b)

#model 3: including Interaction terms
model_3 <- lm(science ~ RACE_ * locus, data = df)
summary(model_3)



#delta R2 from model2 to model1a
R2gain <- summary(model_2)$r.squared - summary(model_1)$r.squared
R2gain
anova(model_1,model_2)

#delta R2 from model2 to model1b
R2gain_1b <- summary(model_2)$r.squared - summary(model_1b)$r.squared
R2gain_1b

#delta R2 from model 3 to model 2
R2gain_inter <- summary(model_3)$r.squared - summary(model_2)$r.squared
R2gain_inter
anova(model_2,model_3)

anova(model_1, model_2, model_3)

anova(model_1, model_2)
anova(model_1b, model_2)
