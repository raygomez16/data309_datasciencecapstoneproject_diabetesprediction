library(tidyverse)

data <- read.csv("https://raw.githubusercontent.com/arinbasu/data309/refs/heads/main/diabetes_prediction_data.csv")

View(data)

#data1 <- data %>%
 # rename(
 #   Gender = gender,
 #   Age = age,
 #   Hypertension = hypertension,
 #   "Heart Disease" = heart_disease,
 #   "Smoking History" = smoking_history,
 #   "BMI (kg/m^2)" = bmi,
 #   "HbA1c Level (mmol)" = HbA1c_level,
 #   "Blood Glucose Level (mg/dl)" = blood_glucose_level,
  #  Diabetes = diabetes)

View(data)
glimpse(data)

duplicated_data <- data %>%
  filter(duplicated(.))
View(duplicated_data)

duplicate_count <- nrow(duplicated_data)
print(paste("Total Duplicated Data:", duplicate_count))

anyNA(data$gender)
anyNA(data$heart_disease)
anyNA(data$diabetes)

missing_data <- sum(is.na(data))
print(paste("Total Missing Data:", missing_data))

glimpse(data %>%
      select("gender", "heart_disease", "diabetes"))

data1 <- data %>%
  mutate(
    Gender = as.factor(gender),
    `Heart Disease` = factor(heart_disease, levels = c(0, 1), labels = c("No", "Yes")),
    Diabetes = factor(diabetes, levels = c(0, 1), labels = c("No", "Yes")))

data1 %>%
  summarise(
    Gender_Count = list(table(Gender)),
    Heart_Disease_Count = list(table(`Heart Disease`)),
    Diabetes_Count = list(table(Diabetes))
  )

#gender
summary(data1$Gender)

#heart disease
summary(data1$`Heart Disease`)

#diabetes
summary(data1$Diabetes)

data1 %>%
  ggplot(aes(x = `Heart Disease`)) +
  geom_bar() +
  labs(title = "Heart Disease Distribution", x = "Heart Disease", y = "Count") +
  theme_minimal()

data1 %>%
  ggplot(aes(x = Gender)) +
  geom_bar() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal()

data1 %>%
  ggplot(aes(x = Diabetes)) +
  geom_bar() +
  labs(title = "Diabetes Distribution", x = "Diabetes", y = "Count") +
  theme_minimal()
################################################################################