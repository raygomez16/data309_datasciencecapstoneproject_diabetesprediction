source("00_cleaning_final.R")

#Summarise the variables of mine
dataset_new %>%
  summarise(
    Gender_Count = list(table(gender)),
    Heart_Disease_Count = list(table(heart_disease)),
    Diabetes_Count = list(table(diabetes))
  )

#gender
summary(dataset_new$gender)

#heart disease
summary(dataset_new$heart_disease)

#diabetes
summary(dataset_new$diabetes)

#Count distribution
dataset_new %>%
  ggplot(aes(x = heart_disease)) +
  geom_bar() +
  labs(title = "Heart Disease Distribution", x = "Heart Disease", y = "Count") +
  theme_minimal()

dataset_new %>%
  ggplot(aes(x = gender)) +
  geom_bar() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal()

dataset_new %>%
  ggplot(aes(x = diabetes)) +
  geom_bar() +
  labs(title = "Diabetes Distribution", x = "Diabetes", y = "Count") +
  theme_minimal()

#Group by Diabetes and Gender, then count Heart Disease status
#Plots
summary_counts <- dataset_new %>%
  group_by(heart_disease, diabetes, gender) %>%
  summarise(Count = n(), .groups = "drop")

print(summary_counts)

#Heart Disease and Diabetes Gender breakdown
#each bar is a count of heart disease of each gender with also who has diabetes or not
ggplot(summary_counts, aes(x = gender, y = Count, fill = diabetes)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = Count),
    position = position_stack(vjust = 0.5), 
    color = "black", size = 2.5) +
  facet_wrap(~ heart_disease) +
  labs(
    title = "Count of Gender Associated with Heart Disease and Diabetes",
    y = "Count",
    x = "Gender",
    fill = "Diabetes Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.2, size = 12)  
  )

#same thing but count of diabetes of each gender with those who have heart disease or not
ggplot(summary_counts, aes(x = gender, y = Count, fill = heart_disease)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = Count),
    position = position_stack(vjust = 0.5), 
    color = "black", size = 2.5) +
  facet_wrap(~ diabetes) +
  labs(
    title = "Count of Gender Associated with Diabetes and Heart Disease",
    y = "Count",
    x = "Gender",
    fill = "Heart Disease Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.2, size = 12) 
  )

summary_counts2 <- dataset_new %>%
  group_by(diabetes, gender) %>%
  summarise(Count = n(), .groups = "drop")

print(summary_counts2)

#Diabetes and Gender Breakdown
ggplot(summary_counts2, aes(x = gender, y = Count, fill = diabetes)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = Count),
    position = position_stack(vjust = 0.5), 
    color = "black", size = 2.5) +
  labs(
    title = "Count of Gender Associated with Diabetes",
    y = "Count",
    x = "Gender",
    fill = "Diabetes Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.2, size = 12)  
  )

summary_counts3 <- dataset_new %>%
  group_by(heart_disease, diabetes) %>%
  summarise(Count = n(), .groups = "drop")

print(summary_counts3)

#Those who have heart disease count with a diabetes breakdown
ggplot(summary_counts3, aes(x = heart_disease, y = Count, fill = diabetes)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = Count),
    position = position_stack(vjust = 0.5), 
    color = "black", size = 2.5) +
  labs(
    title = "Count of Heart Disease Associated with Diabetes",
    y = "Count",
    x = "Heart Disease",
    fill = "Diabetes Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.2, size = 12)  
  )

#other way around for the previous plot
ggplot(summary_counts3, aes(x = diabetes, y = Count, fill = heart_disease)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = Count),
    position = position_stack(vjust = 0.5), 
    color = "black", size = 2.5) +
  labs(
    title = "Count of Diabetes Associated with Heart Disease",
    y = "Count",
    x = "Diabetes",
    fill = "Heart Disease Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.2, size = 12)  
  )

summary_counts4 <- dataset_new %>%
  group_by(heart_disease, gender) %>%
  summarise(Count = n(), .groups = "drop")

print(summary_counts4)

#Heart disease gender breakdown
ggplot(summary_counts4, aes(x = gender, y = Count, fill = heart_disease)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = Count),
    position = position_stack(vjust = 0.5), 
    color = "black", size = 2.5) +
  labs(
    title = "Count of Gender Associated with Heart Disease",
    y = "Count",
    x = "Gender",
    fill = "Heart Disease"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.2, size = 12)  
  )
