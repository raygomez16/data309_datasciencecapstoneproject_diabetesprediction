library(ggplot2)
library(tidyverse)

# retrieve data from GithUb repository
source <- "https://raw.githubusercontent.com/arinbasu/data309/refs/heads/main/diabetes_prediction_data.csv"
dataset_original = read.csv(source)

# check if there is any missing values
dataset_original %>% 
  summarise(across(everything(), ~ sum(is.na(.))))

# check data types
glimpse(dataset_original)

### PER VARIABLE CLEANING ###

# gender
dataset_original %>%
  distinct(gender)
dataset_original <- dataset_original %>%
  mutate(gender = factor(tolower(gender)))

# age 
dataset_original <- dataset_original %>%
  mutate(age = as.integer(age))

# hypertension
dataset_original %>%
  distinct(hypertension) 
dataset_original <- dataset_original %>%
  mutate(hypertension = as.integer(hypertension))

# heart disease
dataset_original %>%
  distinct(heart_disease)
dataset_original <- dataset_original %>%
  mutate(heart_disease = as.integer(heart_disease))

# smoking history
dataset_original %>%
  distinct(smoking_history)
dataset_original <- dataset_original %>%
  mutate(smoking_history = tolower(smoking_history))

# bmi
dataset_original <- dataset_original %>%
  mutate(bmi = as.numeric(bmi))

# HbA1c level
dataset_original %>%
  distinct(HbA1c_level)
dataset_original <- dataset_original %>%
  mutate(HbA1c_level = as.numeric(HbA1c_level))

# blood glucose level 
dataset_original <- dataset_original %>%
  mutate(blood_glucose_level = as.numeric(blood_glucose_level))

# diabetes
dataset_original %>%
  distinct(diabetes)
dataset_original <- dataset_original %>%
  mutate(diabetes = as.integer(diabetes))

glimpse(dataset_original)

# determine if there are any duplicate rows
dataset_new <- dataset_original %>%
  group_by(across(everything())) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

# creates a new dataset without the duplicated rows
dataset_new <- dataset_original %>%
  filter(!duplicated(.))

# samples 80% of data for training set
# remaining 20% is for test set

set.seed(123)
dataset_train_80 <- dataset_new %>%
  sample_frac(0.8)

dataset_test_20 <- dataset_new %>%
  anti_join(dataset_train_80, by = names(dataset_new))




