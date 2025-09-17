library(ggplot2)
library(tidyverse)
library(tidyr)
library(scales)
library(patchwork)

# retrieve data from GithUb repository
source <- "https://raw.githubusercontent.com/arinbasu/data309/refs/heads/main/diabetes_prediction_data.csv"
dataset_original = read.csv(source)

dataset_original %>%  # check if there is any missing values
  summarise(across(everything(), ~ sum(is.na(.))))

glimpse(dataset_original) # check data types
  
############################################################
# gender - categorical variable
dataset_original %>% # checking observation counts on each gender
  count(gender)

dataset_new <- dataset_original %>% #changing values to lower caps
  mutate(gender = factor(tolower(gender)))

encoded_gender <- model.matrix(~ gender - 1, data = dataset_new) # creates a matrix of binary columns for gender values

dataset_new <- dplyr::bind_cols(dataset_new,as.data.frame(encoded_gender)) # binds the matrix to data frame

dataset_new <- dataset_new %>% # removes original gender column
  select(-gender)

colSums(dataset_new[, c('gendermale', 'genderfemale', 'genderother')]) # checking if observation counts match from before

############################################################
# age - continuous variable
dataset_new <- dataset_new %>% # change type from dbl to int
  mutate(age = as.integer(age)) 

############################################################
# hypertension - binary 0/1 variable
dataset_original %>% # check distinct values
  distinct(hypertension) 

dataset_new <- dataset_new %>% #change type from int to dbl
  mutate(hypertension = as.numeric(hypertension))

############################################################
# heart disease - binary 0/1 variable
dataset_original %>% # check distinct values
  distinct(heart_disease)

dataset_new <- dataset_new %>% # change type from int to dbl
  mutate(heart_disease = as.numeric(heart_disease))

############################################################
# smoking history - categorical variable
dataset_original %>% # check distinct values
  distinct(smoking_history)

encoded_smoking_history <- model.matrix(~ smoking_history - 1, data = dataset_new) # creates a matrix of binary columns for gender values

dataset_new <- dplyr::bind_cols(dataset_new,as.data.frame(encoded_smoking_history)) # binds the matrix to data frame

dataset_new <- dataset_new %>% # removes original smoking_history column
  select(-smoking_history) 

############################################################
# bmi - continuous variable
dataset_new <- dataset_new %>%
  mutate(bmi = as.numeric(bmi))

############################################################
# HbA1c level - continuous variable
dataset_new <- dataset_new %>%
  mutate(HbA1c_level = as.numeric(HbA1c_level))

############################################################
# blood glucose level - continuous variable
dataset_new <- dataset_new %>%
  mutate(blood_glucose_level = as.numeric(blood_glucose_level))

############################################################
# diabetes - binary 0/1 variable
dataset_new %>%
  distinct(diabetes)

dataset_new <- dataset_new %>% # change type from int to dbl
  mutate(diabetes = as.numeric(diabetes))
############################################################
# compare changes to value data types from original to new
# and check names
glimpse(dataset_original) 
glimpse(dataset_new)

############################################################
# change column names
colnames(dataset_new) <- c("age",
                                "hypertension",
                                "heart_disease",
                                "bmi",
                                "HbA1c_level",
                                "blood_glucose_level",
                                "diabetes",
                                "female",
                                "male",
                                "other",
                                "current",
                                "ever",
                                "former",
                                "never",
                                "no info",
                                "not current")
                          
############################################################
duplicates <- dataset_new %>% # determine if there are any duplicate rows
  group_by(across(everything())) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

dataset_new <- dataset_new %>% # creates a new dataset without the duplicated rows
  filter(!duplicated(.))

############################################################
# counts of observations
nrow(dataset_original)
nrow(dataset_new)

############################################################
#split the cleaned dataset into training and test
set.seed(42)  # for reproducibility

n <- nrow(dataset_new) # total rows
train_idx <- sample.int(n, size = floor(0.8 * n)) # pick 80% of rows

dataset_training <- dataset_new[train_idx, ] # training set (80%)
dataset_test  <- dataset_new[-train_idx, ] # test set (remaining 20%)


nrow(dataset_training)   # should be about 80% of n
nrow(dataset_test)    # should be about 20% of n
 

         
