library(ggplot2)
library(tidyverse)
library(tidyr)

# retrieve data from GithUb repository
source <- "https://raw.githubusercontent.com/npradaschnor/Pima-Indians-Diabetes-Dataset/master/diabetes.csv"
practice_dataset_original = read.csv(source)

practice_dataset_original %>%  # check if there is any missing values
  summarise(across(everything(), ~ sum(is.na(.))))

glimpse(practice_dataset_original) # check data types

practice_dataset_new <- practice_dataset_original %>%
  rename_with(tolower)

############################################################
# pregnancies - numerical variable
practice_dataset_new %>% # checking observation counts on each gender
  count(pregnancies) %>%
  mutate(pregnancies = as.integer(pregnancies))

############################################################
# age - continuous variable
dataset_new <- dataset_new %>% # change type from dbl to int
  mutate(age = as.integer(age)) 

############################################################
# glucose  - continuous variable #### NOTE THIS USES DIFFERENT MEASURE TO ACTUAL DATASET
practice_dataset_new <- practice_dataset_new %>% #change type from int to dbl
  mutate(glucose = as.numeric(glucose))

############################################################
# blood pressure - continuous variable
practice_dataset_new <- practice_dataset_new %>% # change type from int to dbl
  mutate(bloodpressure = as.numeric(bloodpressure))

############################################################
# skin thickness - continuous variable
practice_dataset_new <- practice_dataset_new %>%
  mutate(skinthickness = as.numeric(skinthickness))
############################################################
# insulin - continuous variable
practice_dataset_new <- practice_dataset_new %>%
  mutate(insulin = as.numeric(insulin))

############################################################
# bmi - continuous variable
practice_dataset_new <- practice_dataset_new %>%
  mutate(bmi = as.numeric(bmi))

############################################################
# diabetes pedigree function - continuous variable
practice_dataset_new <- practice_dataset_new %>%
  mutate(diabetespedigreefunction = as.numeric(diabetespedigreefunction))

############################################################
# age - continuous variable
practice_dataset_new <- practice_dataset_new %>% # change type from dbl to int
  mutate(age = as.integer(age)) 

############################################################
# outcome - binary 0/1 variable
practice_dataset_new %>% # check distinct values
  distinct(outcome) 

practice_dataset_new <- practice_dataset_new %>% 
  mutate(outcome = as.numeric(outcome))

############################################################
# compare changes to value data types from original to new
# and check names
glimpse(practice_dataset_original) 
glimpse(practice_dataset_new)
############################################################
practice_duplicates <- practice_dataset_new %>% # determine if there are any duplicate rows
  group_by(across(everything())) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

practice_dataset_new <- practice_dataset_new %>% # creates a new dataset without the duplicated rows
  filter(!duplicated(.))

############################################################
# counts of observations
nrow(practice_dataset_original)
nrow(practice_dataset_new)

############################################################
#split the cleaned dataset into training and test
set.seed(42)  # for reproducibility

n <- nrow(practice_dataset_new) # total rows
train_idx <- sample.int(n, size = floor(0.8 * n)) # pick 80% of rows

practice_dataset_training <- practice_dataset_new[train_idx, ] # training set (80%)
practice_dataset_test  <- practice_dataset_new[-train_idx, ] # test set (remaining 20%)


nrow(practice_dataset_training)   # should be about 80% of n
nrow(practice_dataset_test)    # should be about 20% of n


