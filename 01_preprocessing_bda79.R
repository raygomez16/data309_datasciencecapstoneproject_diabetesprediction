source("00_cleaning_final.R")

#gender
summary(dataset_new$male)
summary(dataset_new$female)
summary(dataset_new$other)

#heart disease
summary(dataset_new$heart_disease)

#diabetes
summary(dataset_new$diabetes)

#Count distribution (0 is no and 1 is yes)
dataset_new %>%
  ggplot(aes(x = factor(heart_disease))) +
  geom_bar() +
  labs(title = "Heart Disease Distribution", x = "Heart Disease", y = "Count") +
  scale_x_discrete(limits = c("0", "1")) +  
  scale_y_continuous(breaks = seq(0, max(table(dataset_new$heart_disease)), by = 10000)) +
  theme_minimal()

dataset_new2 <- dataset_new %>%
  pivot_longer(cols = c(male, female, other), names_to = "gender", values_to = "flag") %>%
  filter(flag == 1)

ggplot(dataset_new2, aes(x = gender)) +
  geom_bar() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  scale_y_continuous(breaks = seq(0, max(table(dataset_new2$gender)), by = 10000)) +
  theme_minimal()

dataset_new2 %>%
  ggplot(aes(x = factor(diabetes))) + 
  geom_bar() +
  labs(title = "Diabetes Distribution", x = "Diabetes", y = "Count") +
  scale_x_discrete(limits = c("0", "1")) + 
  scale_y_continuous(breaks = seq(0, max(table(dataset_new2$diabetes)), by = 10000)) +
  theme_minimal()

##############################################################################

#Group by Diabetes and Gender, then count Heart Disease status
#Plots
#Because the variables are considered binary and not continuous I won't be using
#mean, median and stdev. 
summary_counts <- dataset_new2 %>%
  group_by(heart_disease, diabetes, gender) %>%
  summarise(Count = n(), .groups = "drop")

print(summary_counts)

#Heart Disease and Diabetes Gender breakdown
#each bar is a count of heart disease of each gender with also who has diabetes or not
ggplot(summary_counts, aes(x = factor(gender), y = Count, fill = factor(diabetes))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = Count),
    position = position_dodge(width = 0.9), 
    vjust = -0.3, size = 3
  ) +
  facet_wrap(~ heart_disease, labeller = labeller(heart_disease = c("0" = "No Heart Disease", "1" = "Heart Disease"))) +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "tomato"),
                    labels = c("0" = "No Diabetes", "1" = "Diabetes")) +
  scale_y_continuous(breaks = seq(0, max(table(dataset_new$heart_disease)), by = 5000)) +
  labs(
    title = "Gender and Heart Disease Breakdown (Associated with Diabetes)",
    x = "Gender Status",
    y = "Count",
    fill = "Diabetes Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )

#same thing but count of diabetes of each gender with those who have heart disease or not
ggplot(summary_counts, aes(x = factor(gender), y = Count, fill = factor(heart_disease))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = Count),
    position = position_dodge(width = 0.9), 
    vjust = -0.3, size = 3
  ) +
  facet_wrap(~ diabetes, labeller = labeller(diabetes = c("0" = "No Diabetes", "1" = "Diabetes"))) +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "tomato"),
                    labels = c("0" = "No Heart Disease", "1" = "Heart Disease")) +
  scale_y_continuous(breaks = seq(0, max(table(dataset_new$heart_disease)), by = 5000)) +
  labs(
    title = "Gender and Diabetes Breakdown (Associated with Heart Disease)",
    x = "Gender Status",
    y = "Count",
    fill = "Heart Disease Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )

summary_counts2 <- dataset_new2 %>%
  group_by(diabetes, gender) %>%
  summarise(Count = n(), .groups = "drop")

print(summary_counts2)

#Diabetes and Gender Breakdown
ggplot(summary_counts2, aes(x = gender, y = Count, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = Count),
    position = position_dodge(width = 0.8),
    vjust = -0.3, size = 3, color = "black"
  ) +
  facet_wrap(~ diabetes, labeller = labeller(diabetes = c("0" = "No Diabetes", "1" = "Diabetes"))) +
  labs(
    title = "Gender Distribution by Diabetes Status",
    y = "Count",
    x = "Gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    legend.position = "none"
  )

summary_counts3 <- dataset_new %>%
  group_by(heart_disease, diabetes) %>%
  summarise(Count = n(), .groups = "drop")

print(summary_counts3)

#Those who have heart disease count with a diabetes breakdown
ggplot(summary_counts3, aes(x = factor(heart_disease), y = Count, fill = factor(diabetes))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = Count),
    position = position_dodge(width = 0.9), 
    vjust = -0.3, size = 3
  ) +
  scale_x_discrete(labels = c("0" = "No Heart Disease", "1" = "Heart Disease")) +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "tomato"),
                    labels = c("0" = "No Diabetes", "1" = "Diabetes")) +
  scale_y_continuous(breaks = seq(0, max(table(dataset_new$heart_disease)), by = 10000)) +
  labs(
    title = "Heart Disease and Diabetes Breakdown",
    x = "Heart Disease Status",
    y = "Count",
    fill = "Diabetes Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )

#other way around for the previous plot
ggplot(summary_counts3, aes(x = factor(diabetes), y = Count, fill = factor(heart_disease))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = Count),
    position = position_dodge(width = 0.9), 
    vjust = -0.3, size = 3
  ) +
  scale_x_discrete(labels = c("0" = "No Diabetes", "1" = "Diabetes")) +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "tomato"),
                    labels = c("0" = "No Heart Disease", "1" = "Heart Disease")) +
  scale_y_continuous(breaks = seq(0, max(table(dataset_new$heart_disease)), by = 10000)) +
  labs(
    title = "Heart Disease and Diabetes Breakdown",
    x = "Diabetes Status",
    y = "Count",
    fill = "Heart Disease Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )

summary_counts4 <- dataset_new2 %>%
  group_by(heart_disease, gender) %>%
  summarise(Count = n(), .groups = "drop")

print(summary_counts4)

#Heart disease gender breakdown
ggplot(summary_counts4, aes(x = gender, y = Count, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = Count),
    position = position_dodge(width = 0.8),
    vjust = -0.3, size = 3, color = "black"
  ) +
  facet_wrap(~ heart_disease, 
        labeller = labeller(heart_disease = c("0" = "No Heart Disease", "1" = "Heart Disease"))) + 
  scale_y_continuous(breaks = seq(0, max(table(dataset_new$heart_disease)), by = 5000)) +
  labs(
    title = "Gender Distribution by Heart Disease Status",
    y = "Count",
    x = "Gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    legend.position = "none"
  )
