# Clean the environment
rm(list = ls())

# Import libraries
library(dplyr)


library(survival)
library(foreign)
library(ggplot2)
library(survminer)
library(rms)
library(flexsurv)
library(ciTools)
library(here)
library(visreg)
library(cmprsk)
library(RColorBrewer)
library(reticulate)

# Import data
df_original <- read.csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv")
View(df_original)

# Make row number pump ID
df_original$pump_ID <- 1:nrow(df_original)

# Columns want to subset or exclude
columns_hseries <- paste("h", 1:48, sep="")
columns_df1 <- c(columns_hseries, 'survive', 'hour', 'pump_ID')
columnsToExclude_df2 <-  c(columns_df1, 'reason2')

# Ensure 'pump_ID' is included in the columns to exclude
columnsToExclude_df2 <- setdiff(columnsToExclude_df2, 'pump_ID')

# Subset two data set
# df1 is for creating start and stop 
# df2 is the data for later modeling
df1 <- df_original[, columns_df1]
df2 <- df_original[, !(names(df_original) %in% columnsToExclude_df2)]

# Turn df2 from wide to long
df2_long <- df2 %>%
  pivot_longer(cols = -pump_ID, names_to = "variable", values_to = "value" )

# Make all the h(after the fail hour) 0
# check how many NA are still in there
# wide to long
# count data
# Attach the data back to the original data set but took out h1 and h48



















#__________________________________________________________
# Question: if the motor is more likely to fail if it has been running for continuous 12 hours.
# Question: what factors drive differences in failure time between groups.
# Pumps can be turned on and off during the time frame

# if the pump survive or if the hour <12, event = 0,
# when hour go back 12 hours ahead hour-12, check if there is NA in this window

df <- 
  mutate(df,longHourFail = ifelse(survive == 1 | hour <= 12, 0, NA)) %>%
  mutate(df, ID = row_number())

filtered_df <-
  filter(df, is.na(longHourFail))
## 405 observations


# proposonal linear will fail because of age, need to fix that.
for (i in seq_along(filtered_df$ID)) {
  failedTime <-  filtered_df$hour[i]
  col_start <- paste0('h',failedTime-12)
  col_end <- paste0('h',failedTime-1)
  #print(i)
  #print(failedTime)
  #print(col_start)
  #print(col_end)
  #print(i,col_start,col_end)
  filtered_df$sum_result[i] <- rowSums(select(filtered_df[i,], col_start:col_end), na.rm = F)
  #filtered_df <- filtered_df %>%
  #  mutate(sum_result = rowSums(select(:, col_start:col_end), na.rm = F))
}

sum(is.na(filtered_df$sum_result))
sum(filtered_df$sum_result<12)
test = (filtered_df$sum_result<12)
sum(test)
# Total 3 rows has NA

# Check what those rows are
filtered_df_NA <-
  filter(filtered_df, is.na(sum_result))
## Decided to drop them because too many NA 

filtered_df$longHourFail <- ifelse(is.na(filtered_df$sum_result)|filtered_df$sum_result<12, 0, 1)

#sum(is.na(filtered_df$longHourFail)) check if there is still NA
#filtered_df[filtered_df$ID == 442,] check if 442 has been assigned to 0

# pumpID <- filtered_df$ID
# df[df$ID == pumpID,]$longHourFail <- filtered_df[filtered_df$ID == pumpID,]$longHourFail
# 
# sum(is.na(df$longHourFail))
# sum(is.na(filtered_df[pumpID,]$longHourFail))
# sum(is.na(filtered_df$longHourFail))

# Merge df and filtered_df based on the 'ID' column
merged_df <- merge(df, filtered_df, by = "ID", all.x = TRUE)

# Update values in the 'longHourFail' column of df with the values from filtered_df
df$longHourFail <- ifelse(!is.na(merged_df$longHourFail.y), merged_df$longHourFail.y, df$longHourFail)


