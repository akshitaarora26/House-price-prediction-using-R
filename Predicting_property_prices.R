library(readr)
# Define the GitHub raw URLs
historic_data_url <- "https://raw.githubusercontent.com/aamish29/bigdata/main/historic_property_data.csv"
predict_data_url <- "https://raw.githubusercontent.com/aamish29/bigdata/main/predict_property_data.csv"

# Read the CSV files
historic_data <- read.csv(historic_data_url)
predict_data <- read.csv(predict_data_url)
# Temporary directory for intermediate files
#EDA

# Check the number of rows and columns
nrow(historic_data)
ncol(historic_data)
nrow(predict_data)
ncol(predict_data)

# Count missing values
colSums(is.na(historic_data))
colSums(is.na(predict_data))

# Visualize missing data
# Count missing values for each column in historic_data
missing_historic <- colSums(is.na(historic_data))
missing_historic <- data.frame(Variable = names(missing_historic), Missing_Values = missing_historic)
print("Missing Values in historic_data:")
print(missing_historic[missing_historic$Missing_Values > 0, ])

# Count missing values for each column in predict_data
missing_predict <- colSums(is.na(predict_data))
missing_predict <- data.frame(Variable = names(missing_predict), Missing_Values = missing_predict)
print("Missing Values in predict_data:")
print(missing_predict[missing_predict$Missing_Values > 0, ])

# Summary statistics
summary(historic_data$sale_price)

# Histogram of sale_price
hist(historic_data$sale_price, breaks = 50, main = "Distribution of Sale Prices", 
     xlab = "Sale Price", col = "skyblue")

# Boxplot to identify outliers
boxplot(historic_data$sale_price, main = "Boxplot of Sale Prices", ylab = "Sale Price")

#Data Pre-Processing
##Handling Missing Values
# Define the threshold for missing values
missing_threshold <- 5000

# Step 1: Identify columns with missing values > 10,000
cols_with_high_na <- colnames(historic_data)[colSums(is.na(historic_data)) > missing_threshold]

# Step 2: Define redundant identifier columns to drop
identifier_cols <- c("pid", "geo_fips")

# Step 3: Combine columns to drop
columns_to_drop <- unique(c(cols_with_high_na, identifier_cols))

# Step 4: Drop the identified columns
historic_data <- historic_data[, !(colnames(historic_data) %in% columns_to_drop)]
predict_data <- predict_data[, !(colnames(predict_data) %in% columns_to_drop)]

# Step 5: Print summary of changes
cat("Columns removed from historic_data:", columns_to_drop, "\n")
nrow(historic_data)
ncol(historic_data)

#Impute Missing Values 
# Define numerical and categorical columns to handle missing values
numeric_missing_cols <- c("char_bsmt", "char_bsmt_fin", "geo_tract_pop", "geo_white_perc", 
                          "geo_black_perc", "geo_asian_perc", "geo_his_perc", "geo_other_perc", 
                          "geo_fs_flood_factor", "econ_midincome")

categorical_missing_cols <- c("char_roof_cnst", "char_air", "char_frpl", 
                              "char_cnst_qlty", "char_repair_cnd", "geo_property_zip", 
                              "ind_garage", "geo_property_city", "geo_municipality", 
                              "geo_school_elem_district", "geo_school_hs_district")


# Step 1: Impute numerical columns with the median
for (col in numeric_missing_cols) {
  if (col %in% colnames(historic_data)) {
    historic_data[[col]][is.na(historic_data[[col]])] <- median(historic_data[[col]], na.rm = TRUE)
  }
}

# Step 2: Impute categorical columns with mode
for (col in categorical_missing_cols) {
  if (col %in% colnames(historic_data)) {
    mode <- names(sort(table(historic_data[[col]]), decreasing = TRUE))[1]
    historic_data[[col]][is.na(historic_data[[col]])] <- mode
  }
}



##Handling Multi-colinearity
install.packages("caret")
library(caret)
# Step 1: Compute the correlation matrix for numerical columns
numeric_cols <- sapply(historic_data, is.numeric)
cor_matrix <- cor(historic_data[, numeric_cols], use = "complete.obs")

# Step 2: Identify highly correlated features (above 0.7 threshold)

high_corr <- findCorrelation(cor_matrix, cutoff = 0.7)  # Identify column indices
high_corr_cols <- colnames(cor_matrix)[high_corr]       # Get column names


#Removing columns with no significant relationship with sales price
# Compute correlations of numerical features with sale_price
numeric_cols <- sapply(historic_data, is.numeric)
cor_with_sale_price <- cor(historic_data[, numeric_cols], use = "complete.obs")[, "sale_price"]

# Set a correlation threshold (e.g., |correlation| < 0.1)
low_corr_cols <- names(cor_with_sale_price[abs(cor_with_sale_price) < 0.1])

# Drop low-correlation variables from historic_data
historic_data <- historic_data[, !(colnames(historic_data) %in% low_corr_cols)]

# Ensure the same columns are dropped in predict_data
predict_data <- predict_data[, !(colnames(predict_data) %in% low_corr_cols)]

# Print summary of changes
cat("Removed low-correlation columns:", low_corr_cols, "\n")
cat("Remaining columns in historic_data:", colnames(historic_data), "\n")

missing_historic <- colSums(is.na(historic_data))
missing_historic <- data.frame(Variable = names(missing_historic), Missing_Values = missing_historic)
print("Missing Values in historic_data:")
print(missing_historic[missing_historic$Missing_Values > 0, ])

#Encode Categorical Variables
categorical_cols <- colnames(historic_data)[!sapply(historic_data, is.numeric)]

# Subset categorical variables
categorical_data <- historic_data[, categorical_cols]
# Function to label encode categorical variables
# Function to label encode categorical variables in historic_data
label_encode <- function(data) {
  for (col in colnames(data)) {
    if (!is.numeric(data[[col]])) {
      data[[col]] <- as.numeric(as.factor(data[[col]]))
    }
  }
  return(data)
}

# Apply label encoding directly to the historic_data
historic_data <- label_encode(historic_data)

for (col in colnames(historic_data)) {
  if (!is.numeric(historic_data[[col]])) {
    historic_data[[col]] <- as.numeric(as.factor(historic_data[[col]]))
  }
}

sale_price_mean <- mean(historic_data$sale_price)  
sale_price_sd <- sd(historic_data$sale_price)

#Splitting Data
# Split data (80% training, 20% validation)
set.seed(123)
train_index <- createDataPartition(historic_data$sale_price, p = 0.8, list = FALSE)
train_data <- historic_data[train_index, ]
validation_data <- historic_data[-train_index, ]

#Scale and Normalise data
# Scale numerical features
numeric_cols <- sapply(train_data, is.numeric)

# Scale training data
train_data[, numeric_cols] <- scale(train_data[, numeric_cols])


# Recalculate numeric columns for each dataset
numeric_cols_validation <- sapply(validation_data, is.numeric)
numeric_cols_predict <- sapply(predict_data, is.numeric)

# Scale numeric columns in validation_data
validation_data[, numeric_cols_validation] <- scale(validation_data[, numeric_cols_validation])

# Scale numeric columns in predict_data
predict_data[, numeric_cols_predict] <- scale(predict_data[, numeric_cols_predict])

train_data$sale_price <- log1p(train_data$sale_price)  # log(sale_price + 1)
validation_data$sale_price <- log1p(validation_data$sale_price)
train_x <- train_data[, !colnames(train_data) %in% c("sale_price")]
train_y <- train_data$sale_price
validation_x <- validation_data[, !colnames(validation_data) %in% c("sale_price")]
validation_y <- validation_data$sale_price

# Train models and compare
install.packages("Metrics")
library(Metrics)

linear_model <- lm(sale_price ~ ., data = train_data)
linear_predictions <- predict(linear_model, newdata = validation_data)
linear_mse <- mse(validation_y, linear_predictions)
cat("Linear Regression MSE:", linear_mse, "\n")


install.packages("randomForest")


library(randomForest)


# Random Forest
set.seed(123)
rf_model <- randomForest(sale_price ~ ., data = train_data, ntree = 4)
rf_predictions <- predict(rf_model, newdata = validation_data)
rf_mse <- mean((validation_y - rf_predictions)^2)
cat("Random Forest MSE:", rf_mse, "\n")

# Function to compute R-Squared
compute_r2 <- function(actual, predicted) {
  ss_residual <- sum((actual - predicted)^2)
  ss_total <- sum((actual - mean(actual))^2)
  r2 <- 1 - (ss_residual / ss_total)
  return(r2)
}

# Linear Regression R-Squared
linear_predictions <- predict(linear_model, newdata = validation_data)
linear_r2 <- compute_r2(validation_y, linear_predictions)
cat("Linear Regression R-Squared:", linear_r2, "\n")

# Random Forest R-Squared
rf_predictions <- predict(rf_model, newdata = validation_data)
rf_r2 <- compute_r2(validation_y, rf_predictions)
cat("Random Forest R-Squared:", rf_r2, "\n")

# Compare R-Squared values for all models
r2_comparison <- data.frame(
  Model = c("Linear Regression", "Random Forest"),
  R2 = c(linear_r2, rf_r2)
)
print(r2_comparison)

# Find common columns
common_columns <- intersect(colnames(historic_data), colnames(predict_data))

#Keep only common columns in predict_data
predict_data <- predict_data[, common_columns]

#Pre-process predict_data

# Step 1: Retain only common columns
common_columns <- intersect(colnames(historic_data), colnames(predict_data))
predict_data <- predict_data[, common_columns]

# Step 2: Dynamically identify numerical and categorical columns
numeric_cols <- sapply(predict_data, is.numeric)
categorical_cols <- !numeric_cols

# Step 3: Impute missing values
# Impute numerical columns with the median from historic_data
for (col in names(numeric_cols[numeric_cols == TRUE])) {
  if (col %in% colnames(historic_data)) {
    predict_data[[col]][is.na(predict_data[[col]])] <- median(historic_data[[col]], na.rm = TRUE)
  }
}

# Impute categorical columns with the mode from historic_data
for (col in names(categorical_cols[categorical_cols == TRUE])) {
  if (col %in% colnames(historic_data)) {
    mode <- names(sort(table(historic_data[[col]]), decreasing = TRUE))[1]
    predict_data[[col]][is.na(predict_data[[col]])] <- mode
  }
}

# Step 4: Label encode categorical variables
label_encode <- function(data) {
  for (col in colnames(data)) {
    if (!is.numeric(data[[col]])) {
      data[[col]] <- as.numeric(as.factor(data[[col]]))
    }
  }
  return(data)
}

# Apply label encoding to predict_data
predict_data <- label_encode(predict_data)

# Step 5: Scale numerical features
# Scale numerical columns in predict_data
predict_data[, numeric_cols] <- scale(predict_data[, numeric_cols])


# Predict on predict_data with all models and reverse transformations

if (!"pid" %in% colnames(predict_data)) {
  predict_data$pid <- seq(1, nrow(predict_data))
}

# Create the 'final_sale_price' column using Random Forest predictions
rf_scaled_predictions <- predict(rf_model, newdata = predict_data)

# Reverse log transformation
rf_log_predictions <- expm1(rf_scaled_predictions)

# Reverse scaling using mean and SD from untransformed sale_price
predict_data$final_sale_price <- (rf_log_predictions * sale_price_sd) + sale_price_mean

# Round the 'final_sale_price' to the nearest integer and ensure two decimal places
predict_data$final_sale_price <- sprintf("%.2f", round(predict_data$final_sale_price))

# Create a 'pid' column with values from 1 to the length of the data
predict_data$pid <- seq(1, nrow(predict_data))

# Extract 'pid' and 'final_sale_price' columns
output_data <- predict_data[, c("pid", "final_sale_price")]

# Save the output to a CSV file
write.csv(output_data, "assessed_value.csv", row.names = FALSE)

