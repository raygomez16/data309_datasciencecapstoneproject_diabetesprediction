#Importing the dataset and required libraries
library('tidyverse')
library('dplyr')
library('ggplot2')

data <- 'https://raw.githubusercontent.com/arinbasu/data309/refs/heads/main/diabetes_prediction_data.csv'
diab_dataset <- read.csv(data)
head(diab_dataset)

#Getting the assigned variables 
#jma_dataset <- diab_dataset[,c('smoking_history', 'bmi')]
#jma_dataset

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

bmi_ranges <- boxplot(clean_dataset$bmi, main = "Individual's BMI Levels", xlab="BMI Level", horizontal = TRUE)
summary(clean_dataset$bmi)

smoking_history_graph <- ggplot(clean_dataset, aes(x=smoking_history)) + geom_bar(fill="steelblue") + labs(title = "Smoking History Bar Plot", x="Smoking History", y="Count") + geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)
smoking_history_graph

#bmi plot with gender comparison
ggplot(clean_dataset, aes(age, bmi)) + geom_point(aes(colour = gender), alpha = 0.5) + facet_wrap(~gender)

#smoking_history plot based on gender
ggplot(clean_dataset, aes(smoking_history, fill=gender)) + labs(title = "Smoking History based on Gender", x = "Smoking history",
                                                                y = "Count") + geom_bar(position = "stack") + scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue"))



