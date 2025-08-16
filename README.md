# 🏡 Sacramento Housing-Price Regression  
*MSBA 205 – Data Analytics for Business | California State University, Sacramento*  
## 🎯 Project Goal
Predict house prices in the Sacramento area with a clean, interpretable linear‐regression model and share the full analytics workflow—from raw data to business-ready insights.
## 🧰 Tech Stack
- **Tools** PowerPoint
- **Language:** R Script  
- **Core Libraries:** `ggplot2`, `car`, `e1071`, `MASS`, `lmtest`, `readr`, `reshape2`

## 📦 Data
- **Source:** [Kaggle – House Prices: Advanced Regression Techniques](https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques)  
- **Shape:** 1,000 rows × 8 numeric variables  
- **Target:** `House_Price` (continuous)  
- **Predictors:** `Square_Footage`, `Num_Bedrooms`, `Num_Bathrooms`, `Year_Built`, `Lot_Size`, `Garage_Size`, `Neighborhood_Quality`

# Install dependencies
install.packages(c("readr","ggplot2","car","e1071","MASS","lmtest","reshape2"))

# Key Insights
After fitting the initial full model, we isolated the incremental contribution of Neighborhood_Quality through nested-model comparison. The predictor exhibited a p-value of 0.454 and failed to reduce residual standard error or improve adjusted R², indicating no statistically significant information content once the six physical-asset variables were already in the equation. Guided by Occam’s razor and formal likelihood-ratio tests, we re-estimated the model without Neighborhood_Quality and achieved identical explanatory power (R² = 99.8 %, residual SE = $9,797). This outcome underscores that, for this Sacramento sample, objective dwelling characteristics fully capture price variation; subjective neighborhood scores add no measurable predictive value. The final specification therefore delivers the same valuation accuracy with greater parsimony and clearer interpretability for practitioners.
