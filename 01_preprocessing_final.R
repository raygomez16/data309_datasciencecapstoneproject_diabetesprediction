source("00_cleaning_final.R")

# DATA PRE-PROCESSING AND EXPLORATION
# FILTER THE CLEAN DATASET INTO TWO: OBSERVATIONS WITH AND no DIABETES

dataset_diabetes <- dataset_new %>%
  filter(diabetes == 1)
dataset_no_diabetes <- dataset_new %>%
  filter(diabetes == 0)

############################################################
# GENDER
############################################################

# FREQUENCY COUNTS FOR EACH GENDER - COMPLETE DATASET
gender_all <- colSums(dataset_new[c("male", "female", "other")])
gender_all <- data.frame(gender = names(gender_all), count = as.numeric(gender_all))
barchart_gender_all <- ggplot(gender_all, aes(x = gender, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, size = 3.5) +
  scale_y_continuous(limits = c(0, 60000), breaks = seq(0, 60000, by = 5000), labels = label_comma()) +
  labs(title = "Total Patients", x = "Gender", y = "Count") +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink", "other" = "darkgreen")) +
  theme(legend.position = "none")
barchart_gender_all

# FREQUENCY COUNTS FOR EACH GENDER - no DIABETES
gender_no_diabetes <- colSums(dataset_no_diabetes[c("male", "female", "other")])
gender_no_diabetes <- data.frame(gender = names(gender_no_diabetes), count = as.numeric(gender_no_diabetes))
barchart_gender_no_diabetes <- ggplot(gender_no_diabetes, aes(x = gender, y = count, fill = gender)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = count), vjust = -0.5, size = 3.5) +
  scale_y_continuous(limits = c(0, 60000), breaks = seq(0, 60000, by = 5000), labels = label_comma()) +
  labs(title = "Total Patients \nno Diabetes", x = "Gender", y = "Count") +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink", "other" = "darkgreen")) +
  theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
barchart_gender_no_diabetes

# FREQUENCY COUNTS FOR EACH GENDER - WITH DIABETES
gender_diabetes <- colSums(dataset_diabetes[c("male", "female", "other")])
gender_diabetes <- data.frame(gender = names(gender_diabetes), count = as.numeric(gender_diabetes))
barchart_gender_diabetes <- ggplot(gender_diabetes, aes(x = gender, y = count, fill = gender)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = count), vjust = -0.5, size = 3.5) +
  scale_y_continuous(limits = c(0, 60000), breaks = seq(0, 60000, by = 5000), labels = label_comma()) +
  labs(title = "Total Patients \nwith Diabetes", x = "Gender", y = "Count") + 
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink", "other" = "darkgreen")) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
barchart_gender_diabetes

barchart_gender_all + barchart_gender_no_diabetes + barchart_gender_diabetes

# BECAUSE THERE ARE INSUFFICIENT SAMPLE SIZE FOR THE 'OTHER' GENDER AT 18 PATIENTS, WE WILL EXCLUDE THEM IN THE ANALYSIS AND MODEL DESIGN

dataset_new <- dataset_new %>%
  filter(other == 0)

#   CALL FUNCTION TO FILTER WITH AND no
dataset_diabetes <- dataset_new %>%
  filter(diabetes == 1)
dataset_no_diabetes <- dataset_new %>%
  filter(diabetes == 0)

############################################################
# HYPERTENSION
############################################################

# FREQUENCY COUNTS BY HYPERTENSION STATUS - COMPLETE DATASET
hypertension_by_gender_all <- dataset_new %>%
  select(hypertension, male, female) %>%
  pivot_longer(cols = c(male, female), names_to = "gender", values_to = "flag") %>%
  filter(flag == 1) %>%
  count(hypertension, gender)
barchart_hypertension_all <- ggplot(hypertension_by_gender_all, aes(x = factor(hypertension, levels = c(0, 1), labels = c("No", "Yes")), y = n, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) + 
  geom_text(aes(label = n), position = position_dodge(width = 1), vjust = -0.3) +
  scale_y_continuous(limits = c(0, 55000), breaks = seq(0, 55000, by = 5000), labels = label_comma()) +
  labs(title = "Total Patients", x = "Hypertension", y = "Count", fill = "Gender") +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +
  theme(legend.position = 'none')
barchart_hypertension_all

# FREQUENCY COUNTS BY HYPERTENSION STATUS - no DIABETES
hypertension_by_gender_no_diabetes <- dataset_no_diabetes %>%
  select(hypertension, male, female) %>%
  pivot_longer(cols = c(male, female), names_to = "gender", values_to = "flag") %>%
  filter(flag == 1) %>%
  count(hypertension, gender)
barchart_hypertension_no_diabetes <- ggplot(hypertension_by_gender_no_diabetes, aes(x = factor(hypertension, levels = c(0, 1), labels = c("No", "Yes")), y = n, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) + 
  geom_text(aes(label = n), position = position_dodge(width = 1),  vjust = -0.3) +
  scale_y_continuous(limits = c(0, 55000), breaks = seq(0, 55000, by = 5000), labels = label_comma()) +
  labs(title = "Total Patients \nno Diabetes", x = "Hypertension", y = "Count", fill = "Gender") +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +
  theme(legend.position = 'none', axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
barchart_hypertension_no_diabetes

# FREQUENCY COUNTS BY HYPERTENSION STATUS - WITH DIABETES
hypertension_by_gender_diabetes <- dataset_diabetes %>%
  select(hypertension, male, female) %>%
  pivot_longer(cols = c(male, female), names_to = "gender", values_to = "flag") %>%
  filter(flag == 1) %>%
  count(hypertension, gender)
barchart_hypertension_diabetes <- ggplot(hypertension_by_gender_diabetes, aes(x = factor(hypertension, levels = c(0, 1), labels = c("No", "Yes")), y = n, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) + 
  geom_text(aes(label = n), position = position_dodge(width = 1),  vjust = -0.3) +
  scale_y_continuous(limits = c(0, 55000), breaks = seq(0, 55000, by = 5000), labels = label_comma()) +
  labs(title = "Total Patients \nwith Diabetes", x = "Hypertension", y = "Count", fill = "Gender") +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
barchart_hypertension_diabetes

barchart_hypertension_all + barchart_hypertension_no_diabetes + barchart_hypertension_diabetes

############################################################
# HEART DISEASE
############################################################

# FREQUENCY COUNTS BY HEART DISEASE - COMPLETE DATASET
heart_disease_by_gender_all <- dataset_new %>%
  select(heart_disease, male, female) %>%
  pivot_longer(cols = c(male, female), names_to = "gender", values_to = "flag") %>%
  filter(flag == 1) %>%
  count(heart_disease, gender)
barchart_heart_disease_all <- ggplot(heart_disease_by_gender_all, aes(x = factor(heart_disease, levels = c(0, 1), labels = c("No", "Yes")), y = n, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) + 
  geom_text(aes(label = n), position = position_dodge(width = 1), vjust = -0.3) +
  scale_y_continuous(limits = c(0, 55000), breaks = seq(0, 55000, by = 5000), labels = label_comma()) +
  labs(title = "Total Patients", x = "Heart Disease", y = "Count", fill = "Gender") +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +
  theme(legend.position = 'none')
barchart_heart_disease_all

# FREQUENCY COUNTS BY HEART DISEASE - no DIABETES
heart_disease_by_gender_no_diabetes <- dataset_no_diabetes %>%
  select(heart_disease, male, female) %>%
  pivot_longer(cols = c(male, female), names_to = "gender", values_to = "flag") %>%
  filter(flag == 1) %>%
  count(heart_disease, gender)
barchart_heart_disease_no_diabetes <- ggplot(heart_disease_by_gender_no_diabetes, aes(x = factor(heart_disease, levels = c(0, 1), labels = c("No", "Yes")), y = n, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) + 
  geom_text(aes(label = n), position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0, 55000), breaks = seq(0, 55000, by = 5000), labels = label_comma()) +
  labs(title = "Total Patients \nno Diabetes", x = "Heart Disease", y = "Count", fill = "Gender") +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +
  theme(legend.position = 'none')
barchart_heart_disease_no_diabetes

# FREQUENCY COUNTS BY HYPERTENSION STATUS - WITH DIABETES
hypertension_by_gender_diabetes <- dataset_diabetes %>%
  select(hypertension, male, female) %>%
  pivot_longer(cols = c(male, female), names_to = "gender", values_to = "flag") %>%
  filter(flag == 1) %>%
  count(hypertension, gender)
barchart_hypertension_diabetes <- ggplot(hypertension_by_gender_diabetes, aes(x = factor(hypertension, levels = c(0, 1), labels = c("No", "Yes")), y = n, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) + 
  geom_text(aes(label = n), position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0, 55000), breaks = seq(0, 55000, by = 5000), labels = label_comma()) +
  labs(title = "Total Patients \nwith Diabetes", x = "Hypertension", y = "Count", fill = "Gender") +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
barchart_hypertension_diabetes

barchart_hypertension_all + barchart_hypertension_no_diabetes + barchart_hypertension_diabetes

############################################################
# SMOKING HISTORY
############################################################

# DATA FRAME THAT TOTALS SMOKING HISTORY OF PATIENTS PER GENDER
smoking_history_by_gender <- dataset_new %>%
  pivot_longer(cols = c("current", "ever", "former", "never", "no info", "not current"), names_to = "smoking_history", values_to = "smoking_flag") %>%
  pivot_longer(cols = c("male", "female"), names_to = "gender", values_to = "gender_flag") %>%
  filter(smoking_flag == 1, gender_flag == 1) %>%
  group_by(gender, smoking_history) %>%
  summarise(count = n(), .groups = "drop")
smoking_history_by_gender

# DATA FRAME THAT TOTALS SMOKING HISTORY OF PATIENTS PER GENDER PER DIAGNOSIS
smoking_history_by_gender_by_diagnosis  <- dataset_new %>%
  pivot_longer(cols = c("current", "ever", "former", "never", "no info", "not current"), names_to = "smoking_history", values_to = "smoking_flag") %>%
  pivot_longer(cols = c("male", "female"), names_to = "gender", values_to = "gender_flag") %>%
  filter(smoking_flag == 1, gender_flag == 1) %>%
  group_by(diabetes, gender, smoking_history) %>%
  summarise(count = n(), .groups = "drop")
smoking_history_by_gender_by_diagnosis

# MULTIPLE BAR CHART - ALL
barchart_smoking_history_by_gender_all <- ggplot(smoking_history_by_gender, aes(x = smoking_history, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.3, size = 2) +
  labs(title = "Smoking History of \nAll Patients by Gender", x = "Smoking History", y = "Count", fill = "Gender") +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +
  scale_y_continuous(limits = c(0, 25000), breaks = seq(0, 25000, by = 5000), labels = label_comma()) +
  theme(legend.position = 'none', axis.text.x = element_text(size = 7))
barchart_smoking_history_by_gender_all

# MULTIPLE BAR CHART - no DIABETES
smoking_history_by_gender_no_diabetes <- smoking_history_by_gender_by_diagnosis %>%
  filter(diabetes == 0)
barchart_smoking_history_by_gender_no_diabetes <- ggplot(smoking_history_by_gender_no_diabetes, aes(x = smoking_history, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.3, size = 2) +
  labs(title = "Smoking History of Non \nDiabetic Patients by Gender", x = "Smoking History", y = "Count", fill = "Gender") +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +
  scale_y_continuous(limits = c(0, 25000), breaks = seq(0, 25000, by = 5000), labels = label_comma()) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = 'none', axis.text.x = element_text(size = 7))
barchart_smoking_history_by_gender_no_diabetes

# MULTIPLE BAR CHART - WITH DIABETES
smoking_history_by_gender_diabetes <- smoking_history_by_gender_by_diagnosis %>%
  filter(diabetes == 1)
barchart_smoking_history_by_gender_diabetes <- ggplot(smoking_history_by_gender_diabetes, aes(x = smoking_history, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.3, size = 2) +
  labs(title = "Smoking History of \nDiabetic Patients by Gender", x = "Smoking History", y = "Count", fill = "Gender") +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +
  scale_y_continuous(limits = c(0, 25000), breaks = seq(0, 25000, by = 5000), labels = label_comma()) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.text.x = element_text(size = 7))
barchart_smoking_history_by_gender_diabetes

barchart_smoking_history_by_gender_all + barchart_smoking_history_by_gender_no_diabetes + barchart_smoking_history_by_gender_diabetes

############################################################
# NUMERICAL VARIABLES
############################################################

# AGE
summary(dataset_new$age)

binwidth_age_friedmandraconis_all <- 2 * IQR(dataset_new$age) / length(dataset_new$age)^(1/3)
binwidth_age_scott_all <- 3.5 * sd(dataset_new$age) / ((length(dataset_new$blood_glucose_level))^(1/3))
bins_age_sturges_all <- ceiling(log2(length(dataset_new$age)) + 1)

histogram_age_friedmandraconis_all <- ggplot(dataset_new, aes(x = age)) + 
  geom_histogram(binwidth = binwidth_age_friedmandraconis_all, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = breaks_width(1000), labels = label_comma()) +
  labs(title = "Distribution of Age using Friedman Draconis Rule", x = "Age",y = "Count")

histogram_age_scott_all <- ggplot(dataset_new, aes(x = age)) + 
  geom_histogram(binwidth = binwidth_age_scott_all, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = breaks_width(1000), labels = label_comma()) +
  labs(title = "Distribution of Age using Scott's Rule", x = "Age",y = "Count")

histogram_age_sturges_all <- ggplot(dataset_new, aes(x = age)) + 
  geom_histogram(bins = bins_age_sturges_all, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = breaks_width(1000), labels = label_comma()) +
  labs(title = "Distribution of Age using Sturges' Rule", x = "Age",y = "Count")

histogram_age_friedmandraconis_all
histogram_age_scott_all
histogram_age_sturges_all

binwidth_age_friedmandraconis_no_diabetes <- 2 * IQR(dataset_no_diabetes$age) / length(dataset_no_diabetes$age)^(1/3)
binwidth_age_scott_no_diabetes <- 3.5 * sd(dataset_no_diabetes$age) / ((length(dataset_no_diabetes$blood_glucose_level))^(1/3))
bins_age_sturges_no_diabetes <- ceiling(log2(length(dataset_no_diabetes$age)) + 1)

histogram_age_friedmandraconis_no_diabetes <- ggplot(dataset_no_diabetes, aes(x = age)) + 
  geom_histogram(binwidth = binwidth_age_friedmandraconis_no_diabetes, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = breaks_width(1000), labels = label_comma()) +
  labs(title = "Distribution of Age using Friedman Draconis Rule", x = "Age",y = "Count")

histogram_age_scott_no_diabetes <- ggplot(dataset_no_diabetes, aes(x = age)) + 
  geom_histogram(binwidth = binwidth_age_scott_no_diabetes, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = breaks_width(1000), labels = label_comma()) +
  labs(title = "Distribution of Age using Scott's Rule", x = "Age",y = "Count")

histogram_age_sturges_no_diabetes <- ggplot(dataset_no_diabetes, aes(x = age)) + 
  geom_histogram(bins = bins_age_sturges_no_diabetes, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = breaks_width(1000), labels = label_comma()) +
  labs(title = "Distribution of Age using Sturges' Rule", x = "Age",y = "Count")

histogram_age_friedmandraconis_no_diabetes
histogram_age_scott_no_diabetes
histogram_age_sturges_no_diabetes

binwidth_age_friedmandraconis_diabetes <- 2 * IQR(dataset_diabetes$age) / length(dataset_diabetes$age)^(1/3)
binwidth_age_scott_diabetes <- 3.5 * sd(dataset_diabetes$age) / ((length(dataset_diabetes$blood_glucose_level))^(1/3))
bins_age_sturges_diabetes <- ceiling(log2(length(dataset_diabetes$age)) + 1)

histogram_age_friedmandraconis_diabetes <- ggplot(dataset_diabetes, aes(x = age)) + 
  geom_histogram(binwidth = binwidth_age_friedmandraconis_diabetes, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = breaks_width(1000), labels = label_comma()) +
  labs(title = "Distribution of Age using Friedman Draconis Rule", x = "Age",y = "Count")

histogram_age_scott_diabetes <- ggplot(dataset_diabetes, aes(x = age)) + 
  geom_histogram(binwidth = binwidth_age_scott_diabetes, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = breaks_width(1000), labels = label_comma()) +
  labs(title = "Distribution of Age using Scott's Rule", x = "Age",y = "Count")

histogram_age_sturges_diabetes <- ggplot(dataset_diabetes, aes(x = age)) + 
  geom_histogram(bins = bins_age_sturges_diabetes, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = breaks_width(1000), labels = label_comma()) +
  labs(title = "Distribution of Age using Sturges' Rule", x = "Age",y = "Count")

histogram_age_friedmandraconis_diabetes
histogram_age_scott_diabetes
histogram_age_sturges_diabetes

histogram_age_sturges_all + histogram_age_sturges_no_diabetes + histogram_age_sturges_diabetes

dataset_long_diabetes <- dataset_diabetes %>%
  pivot_longer(cols = c(male, female), names_to = "gender", values_to = "flag") %>%
  filter(flag == 1)

ggplot(dataset_long_diabetes, aes(x = "", y = age)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = gender), width = 0.2, alpha = 0.6) +
  scale_color_manual(values = c("male" = "skyblue", "female" = "pink")) +
  labs(title = "Distribution of Age (Colored by Gender)", x = "", y = "Age", color = "Gender") +
  theme_minimal() + 
  coord_flip()

dataset_long_no_diabetes <- dataset_no_diabetes %>%
  pivot_longer(cols = c(male, female), names_to = "gender", values_to = "flag") %>%
  filter(flag == 1)

ggplot(dataset_long_no_diabetes %>% filter(gender == "male"), aes(x = "", y = age)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "skyblue", width = 0.2, alpha = 0.6) +
  labs(title = "Distribution of Age (Males Only)", x = "", y = "Age") +
  theme_minimal() +
  coord_flip()

ggplot(dataset_long_no_diabetes %>% filter(gender == "female"), aes(x = "", y = age)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "pink", width = 0.2, alpha = 0.6) +
  labs(title = "Distribution of Age (Females Only)", x = "", y = "Age") +
  theme_minimal() +
  coord_flip()

ggplot(dataset_long_diabetes %>% filter(gender == "male"), aes(x = "", y = age)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "skyblue", width = 0.2, alpha = 0.6) +
  labs(title = "Distribution of Age (Males Only)", x = "", y = "Age") +
  theme_minimal() +
  coord_flip()

ggplot(dataset_long_diabetes %>% filter(gender == "female"), aes(x = "", y = age)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "pink", width = 0.2, alpha = 0.6) +
  labs(title = "Distribution of Age (Females Only)", "", y = "Age") +
  theme_minimal() +
  coord_flip()





# BLOOD GLUCOSE
summary(dataset_new$blood_glucose_level)
binwidth_blood_glucose_friedmandraconis <- 2 * IQR(dataset_new$blood_glucose_level) / (length(dataset_new$blood_glucose_level)^(1/3))
binwidth_blood_glucose_scott <- 3.5 * sd(dataset_new$blood_glucose_level) / (length(dataset_new$blood_glucose_level))^(1/3)
bins_blood_glucose_sturges <- ceiling(log2(length(dataset_new$blood_glucose_level)) + 1)

histogram_blood_glucose_friedmandraconis <- ggplot(dataset_new, aes(x = blood_glucose_level)) + 
  geom_histogram(binwidth = binwidth_blood_glucose_friedmandraconis, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Blood Glucose Levels using Friedman Draconis Rule", x = "Blood Glucose Level",y = "Count")

histogram_blood_glucose_scott <- ggplot(dataset_new, aes(x = blood_glucose_level)) +
  geom_histogram(binwidth = binwidth_blood_glucose_scott, fill = "steelblue", color = "black") +
  scale_y_continuous(breaks = seq(0, max(hist(dataset_new$blood_glucose_level, plot = FALSE)$counts), by = 5000)) +
  labs(title = "Distribution of Blood Glucose Levels using Scott's Rule", x = "Blood Glucose Level", y = "Count")

histogram_blood_glucose_sturges <- ggplot(dataset_new, aes(x = blood_glucose_level)) +
  geom_histogram(bins = bins_blood_glucose_sturges, fill ="steelblue", color = "black") +
  labs(title = "Distribution of Blood Glucose Levels using Sturges' Rule", x = "Blood Glucose Level", y = "Count")

histogram_blood_glucose_friedmandraconis
histogram_blood_glucose_scott
histogram_blood_glucose_sturges






# BMI
summary(dataset_new$bmi)

binwidth_bmi_friedmandraconis <- 2 * IQR(dataset_new$bmi) / (length(dataset_new$bmi)^(1/3))
binwidth_bmi_scott <- 3.5 * sd(dataset_new$bmi) / (length(dataset_new$bmi))^(1/3)
bins_bmi_sturges <- ceiling(log2(length(dataset_new$bmi)) + 1)

histogram_bmi_friedmandraconis <- ggplot(dataset_new, aes(x = bmi)) + 
  geom_histogram(binwidth = binwidth_bmi_friedmandraconis, fill = "steelblue", color = "black") +
  labs(title = "Distribution of BMI using Friedman Draconis Rule", x = "BMI",y = "Count")

histogram_bmi_scott <- ggplot(dataset_new, aes(x = bmi)) +
  geom_histogram(binwidth = binwidth_bmi_scott, fill = "steelblue", color = "black") +
  scale_y_continuous(breaks = seq(0, max(hist(dataset_new$bmi, plot = FALSE)$counts), by = 5000)) +
  labs(title = "Distribution of BMI using Scott's Rule", x = "BMI", y = "Count")

histogram_bmi_sturges <- ggplot(dataset_new, aes(x = bmi)) +
  geom_histogram(bins = bins_bmi_sturges, fill ="steelblue", color = "black") +
  labs(title = "Distribution of BMI using Sturges' Rule", x = "BMI", y = "Count")

histogram_bmi_friedmandraconis
histogram_bmi_scott
histogram_bmi_sturges






