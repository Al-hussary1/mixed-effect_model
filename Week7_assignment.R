
library(tidyverse)
library(broom)
library(lme4)


# Task 1: Data Exploration

# a. Load the "sleepstudy" dataset
data("sleepstudy")

# b. Explore the structure of the dataset
str(sleepstudy)

# sleep deprivation started from day 2
 sleep_depr<- subset(sleepstudy, Days >= 2) %>%
              mutate(days_deprived = Days - 2L)
 
# sleep adaptation and training day 0 and 1
 sleep_adapt <- subset(sleepstudy, Days >=0 & Days < 2)

summary(sleepstudy)
summary(sleep_depr)

#mean reaction time per subject
sleepstudy %>%
  group_by(Subject) %>%
  summarise(mean_RT = mean(Reaction))
      
#mean reaction time per subject based on Day 2-baseline           
sleep_depr %>%
  group_by(Subject) %>%
  summarise(mean_RT = mean(Reaction))


# c. Visualize the data using appropriate plots

ggplot(sleepstudy) +
  aes(x = Days, y = Reaction) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Subject) 


ggplot(sleep_depr) +
  aes(x = days_deprived, y = Reaction) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Subject) 


# Plotting Reaction Time over Days for each Participant
library(ggplot2)
ggplot(sleepstudy, aes(x = Days, y = Reaction, color = as.factor(Subject))) +
  geom_line() +
  geom_point() +
  facet_wrap(~Subject)+
  labs(title = "Reaction Time over Days for each Participant",
       x = "Days deprived of sleep",
       y = "Avg Reaction Time")

# With the exception of subject 335,  it looks like reaction time increases with each additional day of sleep deprivation.

# Plotting a boxplot to show the distribution of Reaction Time
ggplot(sleepstudy, aes(x = factor(Days), y = Reaction)) +
  geom_boxplot(aes(fill = "PuOr")) +
  labs(title = "Distribution of Reaction Time over Days",
       x = "Days",
       y = "Reaction Time") 

# It shows that the reaction time increases with an increase in the number of days deprived of sleep

# Plotting a boxplot to show the distribution of Reaction Time
ggplot(sleepstudy, aes(x = Subject, y = Reaction)) +
  geom_boxplot(aes(fill = Subject)) +
  labs(title = "Distribution of Reaction Time over Days",
       x = "Subject",
       y = "Reaction Time") 


# Plotting a scatter plot to visualize the relationship between Days and Reaction Time
ggplot(sleep_depr, aes(x = days_deprived, y = Reaction)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter plot of Days vs Reaction Time",
       x = "Days Deprived of sleep",
       y = "Reaction Time")


# Task 2: Descriptive Statistics

# a. Compute and report summary statistics for the key variables
summary(sleepstudy$Reaction)
summary(sleepstudy$Days)



# b. Create visualizations to better understand the distribution of reaction times over different days

# Histogram of Reaction Time
hist(sleepstudy$Reaction, main = "Histogram of Reaction Time", xlab = "Reaction Time")


# Normality test for sleep_depr dataset
par(mfrow = c(1, 2))

#Histogram

hist(sleep_depr$Reaction, col='steelblue', main= 'Histogram of Reaction time', xlab = "Reaction Time")

qqnorm(sleep_depr$Reaction)
qqline(sleep_depr$Reaction)


#Using Shapiro-wilk's test
shapiro.test(sleep_depr$Reaction)
# 
# Shapiro-Wilk normality test
# 
# data:  sleep_depr$Reaction
# W = 0.97674, p-value = 0.01489

# based on the p-value of 0.015,  the distribution isn't normal. Hence, there is a need to normalize the distribution.

# Task 3: Fit an adequate Model(s)

# For simplicity, let's fit a linear mixed-effects model using the lme4 package (Random intercept)
model <- lmer(Reaction ~ days_deprived + (1 | Subject), data = sleep_depr)

# Task 4: Interpret the results

summary(model)
# 
# Linear mixed model fit by REML ['lmerMod']
# Formula: Reaction ~ days_deprived + (1 | Subject)
# Data: sleep_depr
# 
# REML criterion at convergence: 1430
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.6261 -0.4450  0.0474  0.5199  4.1378 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# Subject  (Intercept) 1746.9   41.80   
# Residual              913.1   30.22   
# Number of obs: 144, groups:  Subject, 18
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)    267.967     10.871   24.65
# days_deprived   11.435      1.099   10.40
# 
# Correlation of Fixed Effects:
#   (Intr)
# days_deprvd -0.354


# Random Intercepts with random slopes
model2 <- lmer(Reaction ~ days_deprived + (1 + days_deprived | Subject), data= sleep_depr)
summary(model2)
# 
# Linear mixed model fit by REML ['lmerMod']
# Formula: Reaction ~ days_deprived + (1 + days_deprived | Subject)
# Data: sleep_depr
# 
# REML criterion at convergence: 1404.1
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.0157 -0.3541  0.0069  0.4681  5.0732 
# 
# Random effects:
#   Groups   Name          Variance Std.Dev. Corr
# Subject  (Intercept)   958.35   30.957       
# days_deprived  45.78    6.766   0.18
# Residual               651.60   25.526       
# Number of obs: 144, groups:  Subject, 18
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)    267.967      8.266  32.418
# days_deprived   11.435      1.845   6.197
# 
# Correlation of Fixed Effects:
#   (Intr)
# days_deprvd -0.062

# Task 5: Residual Analysis

# Residuals vs Fitted Values Plot

par (mfrow = c (1, 3))
# histogram
hist(resid(model2))

# QQ Plot
qqnorm(residuals(model2))
qqline(residuals(model2))

# plot residual and fitted 
plot(fitted(model2), residuals(model2))
########################################################
par (mfrow = c (1, 3))
# histogram
hist(residuals(model))

# QQ Plot
qqnorm(residuals(model))
qqline(residuals(model))

# plot residual and fitted 
plot(fitted(model), residuals(model))
