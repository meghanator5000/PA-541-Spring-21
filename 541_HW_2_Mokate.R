library(tidyverse)
library(haven)
library(fastDummies)
car <- read_csv("car_data.csv")

# What is the average selling price for automatic versus manual cars? (2 pts)

mean(car$selling_price[car$transmission == "Manual"])
mean(car$selling_price[car$transmission == "Automatic"])

  # The average selling price for Manual is 400,066.7.
  # The average selling price for Automatic is 1,408,154.

# Of the automatic cars, which model was sold at the highest price? (2pts)
car %>%
  filter(transmission == "Automatic") %>%
  arrange(desc(selling_price)) 
  # The Audi RS7 2015-2019 Sportback Performance was sold for the highest price of the automatic cars.

# Plot the average selling price for each type of transmission. (3 pts)
price_by_trans <- car %>%
  group_by(transmission) %>%
  summarize(avg_sp = mean(selling_price))

ggplot(data = price_by_trans,
       mapping = aes(x = transmission, y = avg_sp))  +
  geom_col() +
  labs(title = "Average Selling Price by Transmission",
       x = "Transmission Type",
       y = "Average Selling Price")

# Plot the relationship between selling price and year for the automatic and manual cars on the same plot. (3 pts)
price_yr_trans <- car %>%
  group_by(transmission, year) %>%
  summarize(avg_sp = mean(selling_price),
            year)

ggplot(price_yr_trans, aes(x = year, y = avg_sp, colour = transmission)) +
  geom_line() +
  labs(title = "Average Selling Price by Year per Transmission",
       x = "Transmission Type",
       y = "Average Selling Price",
       color = "Transmission Type")


# Estimate a model with selling price as the dependent variable 
# and kilometers driven and transmission as the independent variables. 

lm1 <- lm(data = car, selling_price ~ km_driven + as.factor(transmission))
summary(lm1)

# Now add year to the model. 

lm2 <- lm(data = car, selling_price ~ km_driven + as.factor(transmission) + year)  
summary(lm2)

cor.test(car$km_driven, car$year)
# I can see that km_driven and year are negatively correlated. 

# Now add the categorical variable owner to the previous model 
# (the one that included km_driven, transmission, and year). 
# Make “first owner” the reference group for the owner variable 
# (hint: you would need to tranform the variable “owner” into a factor 
# before determining the reference group). 

lm3 <- lm(data = car, selling_price ~ km_driven + as.factor(transmission) + year + as.factor(owner))
summary(lm3)

# What would be the predicted selling price of an automatic 2012 
# car with 100,000 kilometers and whose owner category is first owner?

-9.045e+07 + 2.485e-01*100000 + -9.152e+05*0 + 4.559e+04*2012 + -2.404e+04*0 + -5.282e+04*0 + 1.955e+05*0 +  -5.775e+04*0
# 1301930

# The model above implicitly assumes the effect of year is the same regardless 
# of the kilometers driven. Test if this is true

lm4 <- lm(data = car, selling_price ~ km_driven + year + km_driven*year + as.factor(transmission) + as.factor(owner))  
summary(lm4)


# Part Two 

insurance <- read_csv('insurance.csv')

# Run a model that predicts the charges based on age, sex, bmi and smoker. 
lm5 <- lm(data = insurance, charges ~ age + as.factor(sex) + as.factor(smoker) + bmi)
summary(lm5)

colSums(is.na(imm_by_region))

# Look at standard errors on coefficients for sex and smoker. 

# The model above implicitly assumes the effect of bmi is the same for both 
# smokers and nonsmokers. Test whether this assumption is true and briefly discuss your results 
# What are the estimated charges for a 38 years old non smoker man with 25 bmi? 
# What are the estimated charges for a 25 years old smoker woman with 30 bmi? 

lm6 <- lm(data = insurance, charges ~ age + as.factor(sex) + as.factor(smoker)*bmi + as.factor(smoker) + bmi)
summary(lm6)

mean(insurance$bmi)

-2071.077 + 266.372*38 + -473.495*1 + -20193.152*0 + 7.969*25 + 1435.608*0

-2071.077 + 266.372*25 + -473.495*0 + -20193.152*1 + 7.969*30 + 1435.608*30
  







  









