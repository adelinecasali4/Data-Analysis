# Load packages, read in data, and select quasi-identifiers --------
# Load packages
library(tidyverse)
library(readxl)
library(kableExtra)
library(rmarkdown)
library(knitr)

# Read in dataset
customer_df <- read_excel("Data/Customer_Data.xlsx")

# Select variables to be shared 
customer_selects_df <- customer_df %>% 
  select(gender = Gender, age = Age, phoneco_tenure = PhoneCoTenure, 
         education_years = EducationYears, marital_status = MaritalStatus)

# Perform data quality control - Check for outliers and null values -----------

cat(ifelse(any(customer_selects_df$gender != 0 & customer_selects_df$gender != 1), 
           paste("Numbers in 'gender' column that are not 0 or 1:", 
                 customer_selects_df$gender[customer_selects_df$gender != 0 & customer_selects_df$gender != 1], sep = "\n"), 
           "None found in 'gender' column"), "\n")
cat(ifelse(length(customer_selects_df$gender[is.na(customer_selects_df$gender)]) > 0,
           paste("NULL or NA values in 'gender' column:", 
                 customer_selects_df$gender[is.na(customer_selects_df$gender)], 
                 sep = "\n"), "None found in 'gender' column"), "\n")
# No outliers or null values found in the gender column 

customer_selects_df %>% 
  arrange(age) %>% 
  head(5) %>% 
  print()
customer_selects_df %>% 
  arrange(desc(age)) %>% 
  head(5) %>% 
  print()
cat(ifelse(length(customer_selects_df$age[is.na(customer_selects_df$age)]) > 0,
           paste("NULL or NA values in 'age' column:", 
                 customer_selects_df$age[is.na(customer_selects_df$age)], 
                 sep = "\n"), "None found in 'age' column"), "\n")
# No outliers or null values found in the age column 

customer_selects_df %>% 
  arrange(phoneco_tenure) %>% 
  head(5) %>% 
  print()
customer_selects_df %>% 
  arrange(desc(phoneco_tenure)) %>% 
  head(5) %>% 
  print()
cat(ifelse(length(customer_selects_df$phoneco_tenure[is.na(customer_selects_df$phoneco_tenure)]) > 0,
           paste("NULL or NA values in 'phoneco_tenure' column:", 
                 customer_selects_df$phoneco_tenure[is.na(customer_selects_df$phoneco_tenure)], 
                 sep = "\n"), "None found in 'phoneco_tenure' column"), "\n")
# No outliers or null values found in the phoneco_tenure column 

customer_selects_df %>% 
  arrange(education_years) %>% 
  head(5) %>% 
  print()
customer_selects_df %>% 
  arrange(desc(education_years)) %>% 
  head(5) %>% 
  print()
cat(ifelse(length(customer_selects_df$education_years[is.na(customer_selects_df$education_years)]) > 0,
           paste("NULL or NA values in 'education_years' column:", 
                 customer_selects_df$education_years[is.na(customer_selects_df$education_years)], 
                 sep = "\n"), "None found in 'education_years' column"), "\n")
# No outliers or null values found in the education_years column 

cat(ifelse(any(customer_selects_df$marital_status != 0 & customer_selects_df$marital_status != 1), 
           paste("Numbers in 'marital_status' column that are not 0 or 1:", 
                 customer_selects_df$marital_status[customer_selects_df$marital_status != 0 & customer_selects_df$marital_status != 1], sep = "\n"), 
           "None found in 'marital_status' column"), "\n")
cat(ifelse(length(customer_selects_df$marital_status[is.na(customer_selects_df$marital_status)]) > 0,
           paste("NULL or NA values in 'marital_status' column:", 
                 customer_selects_df$marital_status[is.na(customer_selects_df$marital_status)], 
                 sep = "\n"), "None found in 'marital_status' column"), "\n")
# No outliers or null values found in the marital_status column 

# Check current equivalence classes ---------------------------------------

equivalence_classes <- customer_selects_df %>%
  group_by_all() %>%
  tally(name = "class_size")
print(equivalence_classes %>% arrange(class_size))
# There are currently many equivalence classes with only one customer, and a maximum of 6 customers in an equivalence class


# Perform grouping to decrease the number of unique customers -------------

# Group age by a 15 year range, with <25 and 55+ as the upper and lowers
customer_selects_df$age <- cut(customer_selects_df$age, 
                                     breaks = c(0, 30, 45, 60, 100),
                                     labels = c("<30", "30-44", "45-59", "60+"))

# Check equivalence classes
equivalence_classes <- customer_selects_df %>%
  group_by_all() %>%
  tally(name = "class_size")
print(equivalence_classes %>% arrange(class_size))
# There are still many unique customers, so more grouping needs to be done

# Group education_years by <15 (no college degree) or 15+ (college degree)
customer_selects_df$education_years <- cut(customer_selects_df$education_years, 
                               breaks = c(0, 15, 100),
                               labels = c("<15", "15+"))

# Check equivalence classes
equivalence_classes <- customer_selects_df %>%
  group_by_all() %>%
  tally(name = "class_size")
print(equivalence_classes %>% arrange(class_size))
# There are still many unique customers, so we will now group within the phoneco_tenure column

# Group phoneco_tenure by 24 months
customer_selects_df$phoneco_tenure <- cut(customer_selects_df$phoneco_tenure, 
                                          breaks = c(-1, 24, 48, 72, 100),
                                          labels = c("<24", "24-47", "48-72", "72+"))

# Check equivalence classes
equivalence_classes <- customer_selects_df %>%
  group_by_all() %>%
  tally(name = "class_size")
print(equivalence_classes %>% arrange(class_size))

# Add in column for probability of re-identification ----------------------

equivalence_classes <- equivalence_classes %>%
  mutate(p_re_id = round(1/ class_size, 2))

# Calculate the maximum and median risks
p_max <- max(equivalence_classes$p_re_id)
print(p_max)
p_med <- median(equivalence_classes$p_re_id)
print(p_med)

# Scenario 1 Calculations -------------------------------------------------
# Formula: p(re-id|attempt) * p(attempt)
# Being conservative and assuming the mitigating controls are low and the motives and capacity are high, p(attempt) = 0.6
p_attempt <- 0.6

# Calculation with max probability of re-identification
s1_max_risk <- ((p_max*p_attempt)/p_attempt)*p_attempt
print(s1_max_risk)

# Calculation with median probability of re-identification
s1_med_risk <- ((p_med*p_attempt)/p_attempt)*p_attempt
print(s1_med_risk)

# Scenario 2 Calculations -------------------------------------------------
# Formulas: p(re-id|acquanit) * p(acquaint), p(acquaint) = 1-(1-p)^m
p <- 0.97
m <- 150
p_acquaint <- 1-(1-p)^m

# Calculation with max probability of re-identification
s2_max_risk <- ((p_max*p_acquaint)/p_acquaint)*p_acquaint
print(s2_max_risk)

# Calculation with median probability of re-identification
s2_med_risk <- ((p_med*p_acquaint)/p_acquaint)*p_acquaint
print(s2_med_risk)

# Scenario 3 Calculations -------------------------------------------------
# Formula: p(re-id|breach)*p(breach)
# Being conservative, we will assume the likelihood of a breach is the same as that of a healthcare company at 27%
p_breach <- 0.27

# Calculation with max probability of re-identification
s3_max_risk <- ((p_max*p_breach)/p_breach)*p_breach
print(s3_max_risk)

# Calculation with median probability of re-identification
s3_med_risk <- ((p_med*p_breach)/p_breach)*p_breach
print(s3_med_risk)

# Scenario 4 Calculations -------------------------------------------------
# Formula: p(re-id)

# Calculation with max probability of re-identification
s4_max_risk <- p_max
print(s4_max_risk)

# Calculation with median probability of re-identification
s4_med_risk <- p_med
print(s4_med_risk)

# Create a table with results ---------------------------------------------

risk_results <- data.frame(
  Results = c("Max Risk", "Median Risk", "Assessment"), 
  S1 = c("30%", "1.2%", "Tolerable"), 
  S2 = c("50%", "2%", "Unacceptable"), 
  S3 = c("13.5%", "0.05%", "Tolerable"), 
  S4 = c("50%", "2%", "Unacceptable")
)

risk_table <- kable(risk_results, format = "html") %>%
  kable_styling(full_width = FALSE)
risk_table <- risk_table %>%
  column_spec(1, background = "lightgray") %>% 
  column_spec(2, background = "green") %>% 
  column_spec(3, background = "red") %>% 
  column_spec(4, background = "green") %>% 
  column_spec(5, background = "red")

# Print the formatted table
print(risk_table)

# Calculate risk ranges ---------------------------------------------------

# Scenario 1
equivalence_classes <- equivalence_classes %>%
  mutate(s1_risk = round(((p_re_id*p_attempt)/p_attempt)*p_attempt, 2))
# Scenario 2
equivalence_classes <- equivalence_classes %>%
  mutate(s2_risk = round(((p_re_id*p_acquaint)/p_acquaint)*p_acquaint, 2))
# Scenario 3
equivalence_classes <- equivalence_classes %>%
  mutate(s3_risk = round(((p_re_id*p_breach)/p_breach)*p_breach, 2))
# Scenario 4
equivalence_classes <- equivalence_classes %>%
  mutate(s4_risk = round(p_re_id, 2))

# Create risk ranges
equivalence_classes$s1_risk <- cut(equivalence_classes$s1_risk, 
                                   breaks = c(-1, 0.06, 0.11, 0.21, 0.34, 0.51, 1),
                                   labels = c("<5%", "<10%", "<20%", "<33%", "<50%", ">50%"))
equivalence_classes$s2_risk <- cut(equivalence_classes$s2_risk, 
                                   breaks = c(-1, 0.06, 0.11, 0.21, 0.34, 0.51, 1),
                                   labels = c("<5%", "<10%", "<20%", "<33%", "<50%", ">50%"))
equivalence_classes$s3_risk <- cut(equivalence_classes$s3_risk, 
                                   breaks = c(-1, 0.06, 0.11, 0.21, 0.34, 0.51, 1),
                                   labels = c("<5%", "<10%", "<20%", "<33%", "<50%", ">50%"))
equivalence_classes$s4_risk <- cut(equivalence_classes$s4_risk, 
                                   breaks = c(-1, 0.06, 0.11, 0.21, 0.34, 0.51, 1),
                                   labels = c("<5%", "<10%", "<20%", "<33%", "<50%", ">50%"))

# Count the class size for each risk range
s1_sum_class_size <- equivalence_classes %>%
  group_by(s1_risk) %>%
  summarize(percent_risk = (sum(class_size)/5000)) %>%
  ungroup()
print(s1_sum_class_size)
s2_sum_class_size <- equivalence_classes %>%
  group_by(s2_risk) %>%
  summarize(percent_risk = (sum(class_size)/5000)) %>%
  ungroup()
print(s2_sum_class_size)
s3_sum_class_size <- equivalence_classes %>%
  group_by(s3_risk) %>%
  summarize(percent_risk = (sum(class_size)/5000)) %>%
  ungroup()
print(s3_sum_class_size)
s4_sum_class_size <- equivalence_classes %>%
  group_by(s4_risk) %>%
  summarize(percent_risk = (sum(class_size)/5000)) %>%
  ungroup()
print(s4_sum_class_size)

# Create a table with risk ranges
risk_ranges <- data.frame(
  Risk = c("<5%", "<10%", "<20%", "<33%", "<50%", ">50%"), 
  S1 = c("99.2%", "0.05%", "0.03%", "0.0004", "0", "0"), 
  S2 = c("96.5%", "2.7%", "0.06%", "0.02%", "0.0004", "0"), 
  S3 = c("99.8%", "0.02%", "0.0004", "0", "0", "0"), 
  S4 = c("96.5%", "2.7%", "0.06%", "0.02%", "0.0004", "0")
)

risk_ranges_table <- kable(risk_ranges, format = "html") %>%
  kable_styling(full_width = FALSE)
print(risk_ranges_table)

