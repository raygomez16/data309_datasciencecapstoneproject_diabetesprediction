source("00_cleaning_final.R")

#2. Summary and Plotting
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

ggplot(summary_counts, aes(x = gender, y = Count, fill = diabetes)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ heart_disease) +
  labs(
    title = "Count by Gender, Diabetes, and Heart Disease",
    y = "Count",
    x = "Gender",
    fill = "Diabetes Status"
  ) +
  theme_minimal()

summary_counts2 <- dataset_new %>%
  group_by(diabetes, gender) %>%
  summarise(Count = n(), .groups = "drop")

print(summary_counts2)

ggplot(summary_counts2, aes(x = gender, y = Count, fill = diabetes)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Count by Gender and Diabetes",
    y = "Count",
    x = "Gender",
    fill = "Diabetes Status"
  ) +
  theme_minimal()

summary_counts3 <- dataset_new %>%
  group_by(heart_disease, diabetes) %>%
  summarise(Count = n(), .groups = "drop")

print(summary_counts3)

ggplot(summary_counts3, aes(x = heart_disease, y = Count, fill = diabetes)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Count by Diabetes, and Heart Disease",
    y = "Count",
    x = "Heart Disease",
    fill = "Diabetes Status"
  ) +
  theme_minimal()

summary_counts4 <- dataset_new %>%
  group_by(heart_disease, gender) %>%
  summarise(Count = n(), .groups = "drop")

print(summary_counts4)

ggplot(summary_counts4, aes(x = heart_disease, y = Count, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Count by Gender, and Heart Disease",
    y = "Count",
    x = "Heart Disease",
    fill = "Diabetes Status"
  ) +
  theme_minimal()

