library(ggplot2)
library(earth)
library(mgcv)
library(ROCit)
library(pROC)

#Clean the environment
rm(list = ls())

# Getting the path of your current open file
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

# Read data (the file must be in the same path of the Rmd file)
train <- read.csv("insurance_t.csv")
head(train)
summary(train)

# Note: no initial variable selection before model building is necessary
# Briefly look at the distribution of the data
ggplot(train, aes(x = ACCTAGE)) +
  geom_histogram()

ggplot(train, aes(x = CRSCORE)) +
  geom_histogram()

# Convert columns to categorical variables
factor_columns <- c('DDA','DIRDEP', 'SAV', 'ATM', 'CD', 'IRA', 'INV', 'MM', 'CC', 'SDB', 'INAREA', 'INS', 
                    'NSF', 'MMCRED', 'CCPURC', 'BRANCH')
train[factor_columns] <- lapply(train[factor_columns], as.factor)


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


train_edit = impute_and_flag(train, con_columns_to_impute)


# Impute three categorical columns
train_edit$INV <- ifelse(is.na(train_edit$INV), 'Missing', train_edit$INV)
train_edit$CC <- ifelse(is.na(train_edit$CC), 'Missing', train_edit$CC)
train_edit$CCPURC <- ifelse(is.na(train_edit$CCPURC), 'Missing', train_edit$CCPURC)

# MARS algorithm
mars <- earth(INS~., data = train_edit, glm=list(family=binomial))
summary(mars)

evimp(mars)

# GAM approach
gam1 <- mgcv::gam(INS~s(ACCTAGE)+
                    s(DDABAL)+
                    s(DEP)+
                    s(DEPAMT)+
                    s(CHECKS)+
                    s(NSFAMT)+
                    s(PHONE)+
                    s(TELLER)+
                    s(SAVBAL)+
                    s(ATMAMT)+
                    s(POS)+
                    s(POSAMT)+
                    s(CDBAL)+
                    s(IRABAL)+
                    s(INVBAL)+
                    s(MMBAL)+
                    #s(MMCRED)+
                    s(CCBAL)+
                    #s(CCPURC)+
                    s(INCOME)+
                    s(LORES)+
                    s(HMVAL)+
                    s(AGE)+
                    s(CRSCORE)+
                    NSF + MMCRED + CCPURC + BRANCH +
                    ACCTAGE_flagMissing +
                    PHONE_flagMissing +
                    POS_flagMissing +
                    POSAMT_flagMissing +
                    INVBAL_flagMissing +
                    CCBAL_flagMissing +
                    INCOME_flagMissing +
                    LORES_flagMissing +
                    HMVAL_flagMissing +
                    AGE_flagMissing +
                    CRSCORE_flagMissing , data = train_edit, family = binomial(link = "logit"), method = 'REML')

summary(gam1)
AIC(gam1)
AIC(mars)

gam2 <- mgcv::gam(INS~s(ACCTAGE)+
                    s(DDABAL)+
                    s(DEP)+
                    s(DEPAMT)+
                    s(CHECKS)+
                    s(NSFAMT)+
                    s(PHONE)+
                    s(TELLER)+
                    s(SAVBAL)+
                    s(ATMAMT)+
                    s(POS)+
                    s(POSAMT)+
                    s(CDBAL)+
                    s(IRABAL)+
                    s(INVBAL)+
                    s(MMBAL)+
                    #s(MMCRED)+
                    s(CCBAL)+
                    #s(CCPURC)+
                    s(INCOME)+
                    s(LORES)+
                    s(HMVAL)+
                    s(AGE)+
                    s(CRSCORE)+
                    NSF + MMCRED + CCPURC + BRANCH +
                    DDA + DIRDEP + SAV + ATM + CD + IRA + INV + MM + CC +
                    SDB + INAREA +
                    NSF + MMCRED + CCPURC + BRANCH +
                    ACCTAGE_flagMissing +
                    PHONE_flagMissing +
                    POS_flagMissing +
                    POSAMT_flagMissing +
                    INVBAL_flagMissing +
                    CCBAL_flagMissing +
                    INCOME_flagMissing +
                    LORES_flagMissing +
                    HMVAL_flagMissing +
                    AGE_flagMissing +
                    CRSCORE_flagMissing , data = train_edit, family = binomial(link = "logit"), method = 'REML')

summary(gam2)

train$p_hat_mars <- predict(mars, type = "response")
train_mars_roc <- rocit(c(train$p_hat_mars), train$INS)

summary(train_mars_roc)

jpeg("mars_roc.jpeg", width = 8, height = 6, units = 'in', res = 300)

plot(train_mars_roc, col = "cornflowerblue")

dev.off()

#AUC = 0.7994


train$p_hat_gam1 <- predict(gam1, type = "response")
train_mars_gam1 <- rocit(c(train$p_hat_gam1), train$INS)
summary(train_mars_gam1)
plot(train_mars_gam1)
#AUC = 0.7934

train$p_hat_gam2 <- predict(gam2, type = "response")
train_mars_gam2 <- rocit(c(train$p_hat_gam2), train$INS)
summary(train_mars_gam2)
plot(train_mars_gam2)
# AUC = 0.8034

