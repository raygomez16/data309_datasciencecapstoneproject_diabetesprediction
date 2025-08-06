library(tidyverse)
library(ggplot2)

dataset_raw = read.csv('diabetes_prediction_data.csv')

# check data types
sapply(dataset_raw, class)

# check distinct values for hypertension
dataset_raw %>%
  distinct(hypertension)

# check missing values for hypertension
dataset_raw %>%
  filter(is.na(hypertension))
sum(is.na(dataset_raw$hypertension))

# change 0/1 to No/Yes 
dataset_raw <- dataset_raw %>%
  mutate(hypertension = factor(hypertension,
                               levels = c(0, 1),
                               labels = c("No", "Yes")))

# frequency counts for each result
dataset_raw %>%
  count(hypertension)

# plot barplot
ggplot(dataset_raw, aes(x = hypertension)) +
  geom_bar(fill = "steelblue") +
  scale_y_continuous(breaks = seq(0, 100000, by = 10000)) +
  labs(title = "Hypertension Status Distribution",
       x = "Hypertension",
       y = "Observations") +
  theme_minimal()

# check missing values for HbA1c
dataset_raw %>%
  filter(is.na(HbA1c_level))
sum(is.na(dataset_raw$HbA1c_level))

# prints out summary stats
summary(dataset_raw$HbA1c_level)

# plots histogram of HbA1c levels
ggplot(dataset_raw, aes(x = HbA1c_level)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "white") +
  scale_x_continuous(breaks = seq(floor(min(dataset_raw$HbA1c_level, na.rm = TRUE)),
                                  ceiling(max(dataset_raw$HbA1c_level, na.rm = TRUE)),
                                  by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 30000, by = 5000)) +
  labs(title = "Distribution of HbA1c Levels",
       x = "HbA1c Level",
       y = "Count") +
  theme_minimal()










