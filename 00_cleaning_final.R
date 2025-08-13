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
  mutate(hypertension = factor(hypertension, levels = c(0, 1), labels = c('no', 'yes')))

# heart disease
dataset_original %>%
  distinct(heart_disease)
dataset_original <- dataset_original %>%
  mutate(heart_disease = factor(heart_disease, levels = c(0, 1), labels = c('no', 'yes')))

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
  mutate(diabetes = factor(diabetes, levels = c(0, 1), labels = c('no', 'yes')))

glimpse(dataset_original)

# determine if there are any duplicate rows
dataset_new <- dataset_original %>%
  group_by(across(everything())) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

# creates a new dataset without the duplicated rows
dataset_new <- dataset_original %>%
  filter(!duplicated(.))

# counts
nrow(dataset_original)
nrow(dataset_new)

 

         