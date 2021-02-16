# set working directory
setwd("~/Desktop/PA 541")

library(tidyverse)
library(haven)

#load the data
fatal = read_csv("Fatalities.csv") 

# Question 1

# a. Create a new dataset for fatality2
fatalities2 <- fatal %>% 
  select(fatal, state, year, spirits, unemp, income, dry, pop, miles) 

fatalities2  

# b. how many fatalities in each year
fatalities2 %>% 
  group_by(year) %>% 
  summarise(total_fatal = sum(fatal)) 

# c. state with the highest fatalities in 1982
max_fatal <- fatalities2 %>% 
  filter(year == 1982) %>% 
  group_by(state) %>% 
  summarise(total_fatal = sum(fatal))  %>%  
  top_n(n=1) 
max_fatal 
# California with 4,615 fatalities

# d. states that had more than 1,000 fatalities and more than 20% of its population residing in dry countries and the years
df_d <- fatalities2 %>% 
  group_by(state, year) %>% 
  summarise(total_fatal = sum(fatal), dry) %>% 
  filter(total_fatal > 1000 & dry > 20) 
df_d 

# e. average number of fatalities in each state
avg_fatal <- fatalities2 %>% 
  group_by(state) %>% 
  summarise(avg_fatal = mean(fatal)) 
avg_fatal 

# Question 2 - Create a new variable 'fatal.cat' for three categories as low, mid, and high

fatalities2 <- fatalities2 %>% 
  mutate(fatal.cat =
           case_when(fatal <= 300 ~ "low", 
                     fatal > 300 & fatal <= 1000 ~ "mid", 
                     fatal > 1000 ~ "high")) 

fatalities2$fatal.cat <- factor(fatalities2$fatal.cat, levels = c("low", "mid", "high")) 

levels(fatalities2$fatal.cat) 

miles_mean <- fatalities2 %>% 
  group_by(fatal.cat) %>% 
  summarise(mean_miles = mean(miles)) 
miles_mean 


# Part Two

fatalities3 <- fatalities2 %>% 
  filter(year == 1987)  

# Question 3

cor.test(fatalities3$miles, fatalities3$fatal)

# Interpretation
# Average miles driven per driver and number of fatal crashes are negatively correlated, at -.20
# This is not significant

# Question 4

fatalities3$pop_100k <- fatalities3$pop/100000
lm2 <- lm(fatal ~ pop_100k, data = fatalities3)
summary(lm2)

# A) Interpret slopes and co-efficients
# When population is 0, fatalities is 66.84. This does not make a whole lot of sense, but population has been adjusted to be in 100,000 and a population of 0 is unrealistic anyway
# Slope: For each additional 100k people in the state, the predicted number of fatalities increases by 17.8. This effect is significant at the .001 level.
# For every 100,000 increase in population size, fatalities increase by 17.7922. This relationship is significant and we reject the null

# B) Percentage of variation is 93%

# C) Number of fatalities if population is 8 million
8000000 / 100000
80 * 17.7922 + 66.8469
# answer: 1490.223

# Question 5

fatalities3$lm2.resids <- resid(lm2) 
max <- fatalities3 %>%
  group_by(state) %>%
  summarise(lm2.resids) %>%
  arrange(desc(lm2.resids))
max

min <- fatalities3 %>%
  group_by(state) %>%
  summarise(lm2.resids) %>%
  arrange(lm2.resids)
min

# The state with the largest negative residual is New York with -905
# The state with the largest positive residual is Florida with 633
# Residuals refer to the difference between the value predicted by the model and the actual value. To have an accurate model, we want our residual to be as close to zero as possible. This means that both the largest negative and largest positive residual values are very inaccurate.
# In the context of our data and model, this means that the model predicting fatalities by population (in 100,000) is predicted to be much higher than actual in New York and much lower than actual in Florida.

# Question 6

lm3 <- lm(fatal ~ pop_100k + miles +dry, data = fatalities3)
summary(lm3)

# A) What percentage of variation is explained by the predictors?
# About 95% of our model is explained by our predictors

# B) Interpret coefficients
# For every 100,000 that population increases by, the number of fatalities is expected to increase by 1.878e+01
# For every one mile increase, the number of fatalities is expected to increase by 1.464e-01
# For every additional percent residing in dry countries, the number of fatalities is expected to increase by 6.990e+00

# C) Interpret the p-value for dry
# The p-value for dry is .04, meaning it is strong enough to reject the null hypothesis that the coefficient is zero / there is no effect. However, it is not as significant as the other variables
# Dry is significant at the .05 level

# D) Change in R-squared from initial model with only pop_100k
# initial - 0.9239 (adjusted R-squared)
# new - 0.9464 

# Question 7

lm4 <- lm(fatal ~ miles, data = fatalities3)
summary(lm4)

lm5 <- lm(fatal ~ pop_100k + miles, data = fatalities3)
summary(lm5)

cor.test(fatalities3$miles, fatalities3$pop_100k)

# Size and direction of mile coefficient:
# The coefficient for miles in the first model is -0.1879, meaning that for each additional mile driven, fatalities decrease by 0.1879
# The coefficient for miles in the second model is 1.382e-01, meaning that for each additional mile driven, fatalities increase by 1.382e-01
# The second model is more logical, since it makes sense that more miles driven will result in more fatalities. This is happening because the first model omits the varibale of population, and thus it has bias
# Because of this, we cannot trust the relationship demonstrated by the first model
# There is omitted variable bias. The bias is negative. 