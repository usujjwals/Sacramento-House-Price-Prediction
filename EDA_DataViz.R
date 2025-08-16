library('readr')
library(ggplot2)
install.packages('e1071')
library(e1071)
library(MASS)
install.packages('reshape2')
library(reshape2)
library(readr)

#reading the file and assigning house as variable 
house<-read.csv("housing.csv")
head(house)

#checking summary for the house 
summary(house)



# Dataset Overview:
# Use summary() to examine each variable’s range, central tendency, and spread:
# - Square_Footage spans 503–4999 sqft (mean ≈ 2815 sqft)
# - Num_Bedrooms ranges from 1 to 5 bedrooms
# - Num_Bathrooms ranges from 1 to 3 bathrooms
# - Year_Built spans 1950–2022 (mean ≈ 1987)
# - Garage_Size evenly distributed among 0, 1, and 2-car garages
# - Neighborhood_Quality scored 1–10 (mean ≈ 5.6)

# Visualize distributions of explanatory variables:
# checking distribution for number of bedrooms
ggplot(house, aes(x = Num_Bedrooms)) +
  geom_histogram(bins = 30, fill = "red", color = "black") +
  theme_minimal()

#checking for bathroom distribution 
ggplot(house, aes(x = Num_Bathrooms)) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  theme_minimal()

# Examine construction year in decadal intervals:
breaks <- seq(1950, 2020, by = 10)                 # decade ticks

ggplot(house, aes(x = Year_Built)) +
  geom_histogram(binwidth = 10,                    # 10-year bins
                 boundary  = 1950,                 # make 1950 the left edge
                 closed    = "left",               # [1950,1960) style intervals
                 fill      = "blue",
                 color     = "black") +
  scale_x_continuous(limits = c(1950, 2022),       # keep the view tight
                     breaks = breaks,
                     labels = paste0(breaks, "-", breaks + 10)) +
  labs(x = "Year Built (10-year ranges)",          # nicer axis title
       y = "Count") +
  theme_minimal()

# checking for garage distribution 
ggplot(house, aes(x = Garage_Size)) +
  geom_histogram(bins = 30, fill = "purple", color = "black") +
  theme_minimal()

# Inspect response variable distribution (House_Price):
ggplot(house, aes(x = House_Price)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  theme_minimal()


# Quantify skewness of House_Price: 
skewness(house$House_Price)

# Check for missing data:
colSums(is.na(house))


# Assess pairwise correlations to detect multicollinearity:
cor(house)


# Prepare and plot a heatmap of correlations among numeric features:
numeric_df <- house[sapply(house, is.numeric)]
cor_matrix <- cor(numeric_df, use = "complete.obs")
melted_cor <- melt(cor_matrix)

# Plot the heatmap
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", mid = "lightgoldenrod", high = "red", 
    midpoint = 0, limit = c(-1, 1), space = "Lab", 
    name = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  ggtitle("Correlation Heatmap of Housing Features")



# Generate boxplots for each variable to check for outliers:
ggplot(house, aes(y = House_Price)) +
  geom_boxplot(fill = "salmon") +
  theme_minimal()

# no outlier 
ggplot(house, aes(y = Square_Footage)) +
  geom_boxplot(fill = "red") +
  theme_minimal()

# no outlier 
ggplot(house, aes(y = Num_Bedrooms)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal()

# no outlier 
ggplot(house, aes(y = Num_Bathrooms)) +
  geom_boxplot(fill = "blue") +
  theme_minimal()

# no outlier 
ggplot(house, aes(y = Year_Built)) +
  geom_boxplot(fill = "green") +
  theme_minimal()

# no outlier 
ggplot(house, aes(y = Lot_Size)) +
  geom_boxplot(fill = "purple") +
  theme_minimal()


# no outlier 
ggplot(house, aes(y = Garage_Size)) +
  geom_boxplot(fill = "orange") +
  theme_minimal()

# no outlier 
ggplot(house, aes(y = Neighborhood_Quality)) +
  geom_boxplot(fill = "salmon") +
  theme_minimal()





