#Importing the dataset and required libraries
library('tidyverse')
library('dplyr')
library('ggplot2')
library('here')

#env <- new.env(parent = emptyenv())
#sys.source(here("00_cleaning_final.R"), envir = env)

diab_dataset <- source("00_cleaning_final.R")$value
head(diab_dataset)


#Cleaning the data
clean_dataset <- na.omit(diab_dataset)
class(clean_dataset) #should be dataframe
#Checking unique values of data (looking for any unrequired values). Important for categorical data
unique(clean_dataset$smoking_history)
clean_dataset$gender <- as.factor(clean_dataset$gender)
clean_dataset$bmi <- as.numeric(clean_dataset$bmi)



#Fixing punctuation for cleaner look
clean_dataset$smoking_history <- tolower(clean_dataset$smoking_history)
class(clean_dataset$smoking_history)
class(clean_dataset$bmi)
 

###


#Data summaries
clean_dataset %>% count(clean_dataset$smoking_history)

#Box plot for BMI
bmi_ranges <- boxplot(clean_dataset$bmi, main = "Individual's BMI Levels", xlab="BMI Level", horizontal = TRUE)
summary(clean_dataset$bmi)

#Filtering out 'other' gender as the population is too small to be considered effective.
gender_subset <- filter(clean_dataset, clean_dataset$gender %in% c('female','male'))

#smoking history count graph.
smoking_history_graph <- ggplot(gender_subset, aes(x=smoking_history)) + geom_bar(fill="steelblue") + labs(title = "Smoking History Bar Plot", x="Smoking History", y="Count") + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5)
smoking_history_graph

#smoking_history plot based on gender
ggplot(clean_dataset, aes(smoking_history, fill=gender)) + labs(title = "Smoking History based on Gender", x = "Smoking history",
                                                                y = "Count") + geom_bar(position = "stack") + scale_fill_manual(values = c("female" = "pink", "male" = "lightblue"))

#bmi plot with gender comparison
#ggplot(gender_subset, aes(age, bmi)) + geom_point(aes(colour = gender), alpha = 0.5) + facet_wrap(~gender)

#generating value ranges for BMI data
custom_intervals <- c(0, 18.5, 24.9, 29.9, 49.9, 100)
bmi_data <- gender_subset$newcol %>% 
  cut(gender_subset$bmi, breaks = custom_intervals, include.lowest = TRUE, labels = c("0-18.5", "18.5-24.9", "25-29.9", "30-49.9", "50-100"))

ggplot(bmi_data, aes(age, bmi)) + geom_histogram(aes(colour = gender), alpha = 0.5) + facet_wrap(~gender)




