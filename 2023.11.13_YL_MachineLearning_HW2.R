#Clean the environment
rm(list = ls())

# Libraries
library(randomForest)
library(ROCit)
library(xgboost)
library(dplyr)
library(caret)


# Getting the path of your current open file
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

# Read data (the file must be in the same path of the Rmd file)
training <- read.csv("insurance_t.csv")

# Convert columns to categorical variables
factor_columns <- c('DDA','DIRDEP', 'SAV', 'ATM', 'CD', 'IRA', 'INV', 
                    'MM', 'CC', 'SDB', 'INAREA', 'INS', 'NSF', 'MMCRED', 
                    'CCPURC', 'BRANCH')
training[factor_columns] <- lapply(training[factor_columns], as.factor)

# Impute median values for continuous and flag missing for both continuous and categorical
# Impute NAs in continuous columns with median
# ACCTAGE, PHONE, POS, POSAMT, INVBAL, CCBAL, CCPURC, INCOME, LORES, HMVAL, AGE, CRSCORE

con_columns_to_impute <- c("ACCTAGE", "PHONE", "POS", "POSAMT", "INVBAL", 
                           "CCBAL", "INCOME", "LORES", "HMVAL", 
                           "AGE", "CRSCORE")


# Create a function to find median, impute median for NAs, and flag
impute_and_flag <- function(data, columns_to_impute){
  for(col in columns_to_impute){
    # Find median value
    median_value <- median(data[[col]], na.rm = TRUE) 
    
    # Create a flag column name
    flag_column <- paste(col, "_", 'flagMissing', sep = '')
    
    # Create new flag columns
    data[[flag_column]] <- ifelse(is.na(data[[col]]), 1, 0)
    
    # Impute median for NAs
    data[[col]] <- ifelse(is.na(data[[col]]), median_value, data[[col]])
    
    # Turn flag columns into factor
    data[[flag_column]] <- factor(data[[flag_column]])
  }
  return(data)
}


training = impute_and_flag(training, con_columns_to_impute)

# Impute three categorical columns
categorical_columns_to_impute <- c('INV', 'CC', 'CCPURC')
for (col in categorical_columns_to_impute){
  training[[col]] <- ifelse(is.na(training[[col]]), 'Missing', training[[col]])
}

# Copy the data for the use of another model
# Training is the dataset being tagged missing and factored to whichever 
# needed but no random or prediction
set.seed(54321)
training_withRandom <- training
set.seed(54321)
training_xgb <- training
set.seed(54321)
training_xgb_withRandom <- training
#training2 <- training
#training3 <- training


#training$INV <- ifelse(is.na(training$INV), 'Missing', training$INV)
#training$CC <- ifelse(is.na(training$CC), 'Missing', training$CC)
#training$CCPURC <- ifelse(is.na(training$CCPURC), 'Missing', training$CCPURC)
# sum(is.na(training))

# Create formula
target <- 'INS'
column_names <- colnames(training)
predictors <- setdiff(column_names, target)
model <- as.formula(paste(target, '~', paste(predictors, collapse = " + ")))

# Check if the training is dataframe and the target variable is a factor
# class(training)
# class(training$INS)

# Set seed for random forest model
set.seed(54321)

# Build random forest model
insurance_rf <- randomForest(model, data = training, 
                             ntree = 250, importance = TRUE)

# Read model results
plot(insurance_rf, main = "Number of Trees Compared to MSE")
varImpPlot(insurance_rf,
           sort = TRUE,
           n.var = 20,
           main = "Top 20 - Variable Importance")
importance(insurance_rf)

# Tune tree models, x is a dataframe and y is a vector
set.seed(54321)
tuneRF(x = training[predictors], y = training[[target]], 
       plot = TRUE, ntreeTry = 250, stepFactor = 0.5)
### 6 OOB with an OOBError of 0.257681
# mtry:Number of variables randomly sampled as candidates at each split


# tuneRF(x = training[predictors], y = training[[target]], 
#        plot = TRUE, ntreeTry = 500, stepFactor = 0.8)
# 
# tuneRF(x = training[predictors], y = training[[target]], 
#        plot = TRUE, ntreeTry = 500, stepFactor = 0.2)
# 
# tuneRF(x = training[predictors], y = training[[target]], 
#        plot = TRUE, ntreeTry = 500, stepFactor = 0.3)

# Re-train the model using the tuning parameter
set.seed(54321)
insurance_rf <- randomForest(model, data = training, 
                             ntree = 250, mtry = 6,
                             importance = TRUE)
plot(insurance_rf, main = "Number of Trees Compared to MSE")
varImpPlot(insurance_rf,
           sort = TRUE,
           n.var = 20,
           main = "Top 20 - Variable Importance")
importance(insurance_rf)

# Plot the ROC curve and report the AUC
predict_rf <- predict(insurance_rf, type = "prob")[,'1']
training_roc <- rocit(predict_rf, training[[target]])

summary(training_roc) # AUC = 0.7915
plot(training_roc) # ROC curve


# Use random variable to find potential removable variables
training_withRandom$random <- rnorm(nrow(training_withRandom))

# Create formula after adding the 'random'
# target <- 'INS'

column_names <- colnames(training_withRandom)
predictors <- setdiff(column_names, target)
model <- as.formula(paste(target, '~', paste(predictors, collapse = " + ")))


set.seed(54321)
insurance_rf_withRandom <- randomForest(model, data = training_withRandom, 
                             ntree = 250, mtry = 6,
                             importance = TRUE)
plot(insurance_rf_withRandom, main = "Number of Trees Compared to MSE")
varImpPlot(insurance_rf_withRandom,
           sort = TRUE,
           n.var = 20,
           main = "Top 20 - Variable Importance")

importance_scores_withRandom <- importance(insurance_rf_withRandom, type = 1)

# Rank variables by importance
set.seed(54321)
ranked_importance <- importance_scores_withRandom[order(importance_scores_withRandom[, "MeanDecreaseAccuracy"], 
                                                        decreasing = TRUE), ]



# Use the above result to remove the following variables, should include "random"
columns_to_remove <- c('SDB', 
                       'LORES',
                       'NSF',
                       'HMVAL_flagMissing',
                       'NSFAMT','CRSCORE_flagMissing',
                       'ACCTAGE_flagMissing','INAREA',
                       'random')

training_afterVariableSelection <- training_withRandom[, !colnames(training_withRandom) %in% columns_to_remove]

# Create formula after removing the columns
# target <- 'INS'
column_names <- colnames(training_afterVariableSelection)
predictors <- setdiff(column_names, target)
model <- as.formula(paste(target, '~', paste(predictors, collapse = " + ")))

set.seed(54321)
insurance_rf_afterVariableSelection <- randomForest(model, data = training_afterVariableSelection, 
                              ntree = 350, mtry = 6,
                              importance = TRUE)
plot(insurance_rf_afterVariableSelection, main = "Number of Trees Compared to MSE")

set.seed(54321)
importance_scores_withSelection <- importance(insurance_rf_afterVariableSelection, type = 1)
ranked_importance2 <- importance_scores_withSelection[order(importance_scores_withSelection[, "MeanDecreaseAccuracy"],
                                                            decreasing = TRUE), ]
test = as.data.frame(ranked_importance2)

# Plot the ROC curve and report the AUC
predict_rf_afterVariableSelection <- predict(insurance_rf_afterVariableSelection, type = "prob")[,'1']
predict_rf_afterVariableSelection <- as.numeric(as.character(predict_rf_afterVariableSelection))
training_roc_afterVariableSelection <- rocit(c(predict_rf_afterVariableSelection), training_afterVariableSelection[[target]])

summary(training_roc_afterVariableSelection) # AUC = 0.7913
plot(training_roc_afterVariableSelection) # ROC curve


#____________________XGBOOST MODEL__________

# Create formula
# target <- 'INS'
column_names <- colnames(training_xgb)
predictors <- setdiff(column_names, target)
model <- as.formula(paste(target, '~', paste(predictors, collapse = " + ")))

train_x <- model.matrix(model, data = training_xgb)[, -1]
train_y <- as.numeric(as.character(training_xgb[[target]]))

# Tune XGBoost
set.seed(54321)
insurance_xgbcv <- xgb.cv(data = train_x, label = train_y, 
                          nrounds = 50, nfold = 10, eval_metric = 'auc')
## 17 is the optimal rounds

# Grid search with caret
tune_grid <- expand.grid(
  nrounds = 17,
  eta = seq(0.05, 0.4, 0.05),
  max_depth = c(1:10),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = seq(0.25, 1, 0.25)
)

# Turn other parameters that's set at tune_grid
set.seed(54321)
insurance_xgb_caret <- train(x = train_x, y = train_y,
                             method = "xgbTree",
                             tuneGrid = tune_grid,
                             trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                      number = 10),
                             eval_metric = 'auc')

# Read the result
plot(insurance_xgb_caret)
insurance_xgb_caret$bestTune
## max_depth = 5
## eta = 0.25
## subsample = 1

# Implement model with tuned parameters
set.seed(54321)
insurance_xgb <- xgboost(data = train_x, label = train_y, 
                         nrounds = 17, subsample = 1,
                         eta = 0.25, max_depth = 5,
                         objective = "binary:logistic",
                         eval_metric = 'auc')

# Show importancee of the features
xgb.importance(feature_names = colnames(train_x), model = insurance_xgb)


# Add random variables to do variable selection
set.seed(54321)
training_xgb_withRandom$random <- rnorm(nrow(training_xgb_withRandom))

# Create formula
column_names <- colnames(training_xgb_withRandom)
predictors <- setdiff(column_names, target)
model <- as.formula(paste(target, '~', paste(predictors, collapse = " + ")))

train_x <- model.matrix(model, data = training_xgb_withRandom)[, -1]
train_y <- as.numeric(as.character(training_xgb[[target]]))

# Build model with random variable
insurance_xgb_withRandom <- xgboost(data = train_x, label = train_y, 
                         nrounds = 17, subsample = 1,
                         eta = 0.25, max_depth = 5,
                         objective = "binary:logistic",
                         eval_metric = 'auc')

# Show importance of the features
xgb.importance(feature_names = colnames(train_x), model = insurance_xgb_withRandom)

## These variables have high importance
## than random
## SAVBAL, DDABAL, CDBAL, DDA1, MM1
## MMBAL, ACCTAGE, CHECKS, ATMAMT
## DEPAMT, TELLER, IRABAL, CCBAL

# Subset data with important variables
training_xgb_select <- training_xgb %>% 
  select(INS, SAVBAL, DDABAL, CDBAL, DDA, MM, MMBAL, 
         ACCTAGE, CHECKS, ATMAMT, DEPAMT, TELLER, IRABAL, CCBAL)

# Create formula
column_names <- colnames(training_xgb_select)
predictors <- setdiff(column_names, target)
model <- as.formula(paste(target, '~', paste(predictors, collapse = " + ")))

train_x <- model.matrix(model, data = training_xgb_select)[, -1]
train_y <- as.numeric(as.character(training_xgb_select[[target]]))

# Build model with selected variables
set.seed(54321)
insurance_xgb_withSelectedVariable <- xgboost(data = train_x, label = train_y, 
                                    nrounds = 17, subsample = 1,
                                    eta = 0.25, max_depth = 5,
                                    objective = "binary:logistic",
                                    eval_metric = 'auc')

# Plot the ROC curve and report the AUC
predict_xgb_withSelectedVariable <- predict(insurance_xgb_withSelectedVariable, newdata = train_x)
xgb_roc_afterVariableSelection <- rocit(predict_xgb_withSelectedVariable, training_xgb_select[[target]])

summary(xgb_roc_afterVariableSelection) # AUC = 0.8438
plot(xgb_roc_afterVariableSelection) # ROC curve

# Re-tune XGBoost
set.seed(54321)
insurance_xgbcv <- xgb.cv(data = train_x, label = train_y, 
                          nrounds = 25, nfold = 10, eval_metric = 'auc')
## 12 is the optimal rounds

# Grid search with caret
tune_grid <- expand.grid(
  nrounds = 12,
  eta = seq(0.05, 0.4, 0.05),
  max_depth = c(1:10),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = seq(0.25, 1, 0.25)
)

# Turn other parameters that's set at tune_grid
set.seed(54321)
insurance_xgb_caret <- train(x = train_x, y = train_y,
                             method = "xgbTree",
                             tuneGrid = tune_grid,
                             trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                      number = 10),
                             eval_metric = 'auc')

# Read the result
plot(insurance_xgb_caret)
insurance_xgb_caret$bestTune

## max_depth = 4
## eta = 0.3
## subsample = 0.75

# Implement tuned model
set.seed(54321)
insurance_xgb_retuned <- xgboost(data = train_x, label = train_y, 
                         nrounds = 12, subsample = 0.75,
                         eta = 0.3, max_depth = 4,
                         objective = "binary:logistic",
                         eval_metric = 'auc')


xgb.importance(feature_names = colnames(train_x), model = insurance_xgb_retuned)

# Plot the ROC curve and report the AUC
predict_xgb_withSelectedVariable_retuned <- predict(insurance_xgb_retuned, newdata = train_x)
xgb_roc_afterVariableSelection <- rocit(predict_xgb_withSelectedVariable_retuned, training_xgb_select[[target]])

summary(xgb_roc_afterVariableSelection) # AUC = 0.8182
plot(xgb_roc_afterVariableSelection) # ROC curve




