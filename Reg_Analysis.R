install.packages("car")     
install.packages("e1071")   
install.packages("MASS")
install.packages("lmtest") 

library(readr)
library(ggplot2)
library(car)
library(e1071)
library(MASS)
library(lmtest)


house <- read.csv("C:/Users/gabma/OneDrive/Documents/MSBA Program/MSBA 205/project/housing.csv")

# Regression Model
# Residuals show symmetric spread around zero
# Multiple R-squared and Adjusted R-squared are both very high, indicating excellent model fit
# High F-statistic and low p-value means model is statistically significant overall
# All predictors except Neighborhood_Quality are highly significant (p-value < 0.001)
# Neighborhood_Quality has p = 0.454, so its not statistically sginificant in this model

model <- lm(House_Price ~ Square_Footage + Num_Bedrooms + Num_Bathrooms +
              Year_Built + Lot_Size + Garage_Size + Neighborhood_Quality,
            data = house)

summary(model)

# Check Variance Inflation Factors (VIF)
# all VIF values are near 1, indicating very low multicollinearity
vif(model)

# Histogram of residuals
# Residual histogram looks roughly normal
hist(residuals(model), col = "lightblue", main = "Histogram of Residuals")

# Q-Q Plot for normality
# Residuals are normally distributed, model is capturing the main patterns in the data and follows a normal distribution
qqnorm(residuals(model))
qqPlot(residuals(model), col = "red")

# Residuals vs Fitted values
# Linearity awssumption appears to be satisfied. Assumption of homoscedasticity is satisfied. No major signs of heteroscedasticity
plot(fitted(model), residuals(model),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Dwtest statistic is close to 2, indicating no autocorrelation in residuals
dwtest(model)

# Skewness near 0 means that the data is symmetrically distributed
skewness(house$House_Price)

#
#
#
#
#
#
#new_model without Neighborhood_Quality variable (not significant)
# Residuals show symmetric spread around zero
# Multiple R-squared and Adjusted R-squared are both very high, indicating excellent model fit
# High F-statistic and low p-value means model is statistically significant overall
# All predictors except Neighborhood_Quality are highly significant (p-value < 0.001)

new_model <- lm(House_Price ~ Square_Footage + Num_Bedrooms + Num_Bathrooms +
              Year_Built + Lot_Size + Garage_Size,
            data = house)

summary(new_model)

# Check Variance Inflation Factors (VIF)
# all VIF values are near 1, indicating very low multicollinearity
vif(new_model)

# Histogram of residuals
# Residual histogram looks roughly normal
hist(residuals(new_model), col = "lightblue", main = "Histogram of Residuals")

# Q-Q Plot for normality
# Residuals are normally distributed, model is capturing the main patterns in the data and follows a normal distribution
qqnorm(residuals(new_model))
qqPlot(residuals(new_model), col = "red")

# Residuals vs Fitted values
# Linearity awssumption appears to be satisfied. Assumption of homoscedasticity is satisfied. No major signs of heteroscedasticity
plot(fitted(new_model), residuals(new_model),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Dwtest statistic is close to 2, indicating no autocorrelation in residuals
dwtest(new_model)


