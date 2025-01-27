# House-price-prediction-using-R

Prediction of property prices using Machine Learning

This repository contains a machine learning pipeline for predicting property sale prices using historical property data. The project leverages multiple models, including linear regression and random forests, to predict the sale price of properties based on various features such as property characteristics, geographical data, and economic indicators.

## Project Overview

This project aims to predict property prices using two datasets:
- **Historic Property Data**: Contains historical sale prices and property-related features.
- **Predict Property Data**: A dataset for which the sale prices need to be predicted.

The pipeline includes several stages:
1. **Data Loading and Exploration**
2. **Data Preprocessing and Cleaning**
3. **Feature Engineering**
4. **Model Training and Evaluation**
5. **Prediction and Output Generation**

## Key Steps

### 1. Data Loading and Exploration
- The historical and prediction datasets are loaded from raw URLs on GitHub.
- Initial exploration is done to check the structure of the data, including the number of rows and columns, as well as missing values.
- Missing values are identified and visualized for further handling.

### 2. Data Preprocessing
- **Handling Missing Values**: 
    - Numerical columns with missing values are imputed using the median.
    - Categorical columns are imputed using the mode.
- **Feature Engineering**:
    - Multicollinearity is handled by removing highly correlated features.
    - Features with weak correlation to the target variable (`sale_price`) are removed.
    - Categorical variables are label-encoded to convert them into numeric representations.
- **Scaling**:
    - Numerical features are scaled to standardize the data for machine learning models.

### 3. Model Training and Evaluation
- **Training Models**:
    - A **Linear Regression** model is trained to predict `sale_price` based on the selected features.
    - A **Random Forest** model is trained as a more robust alternative.
- **Evaluation**:
    - Models are evaluated using Mean Squared Error (MSE) and R-Squared metrics.
    - A comparison is made between both models to determine which one provides the best fit.

### 4. Prediction and Output Generation
- Predictions for the sale prices are generated using the Random Forest model.
- The predictions undergo a series of transformations (e.g., inverse scaling and inverse log transformation) to get the final predicted sale prices.
- The results are saved into a CSV file, including the `pid` (property identifier) and the predicted sale price (`final_sale_price`).


## Conclusion

This project demonstrates how to use machine learning models to predict property prices using historical data. By applying proper data preprocessing, feature engineering, and model evaluation, we can obtain reliable predictions that can be useful in the real estate industry for property assessment and valuation.
