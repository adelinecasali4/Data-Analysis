# Load data and packages
library(tidyverse)
library(openxlsx)
library(rpart)
library(rpart.plot)
data <- read.xlsx("clean_data.xlsx")

# Value segmentation
# Create new column with average spent per month
data$SpendingPerMonth <- ifelse(data$PhoneCoTenure == 0, 0, data$TotalOverTenure / data$PhoneCoTenure)
# Convert NAs to 0
data <- data %>%
  mutate(SpendingPerMonth = replace_na(SpendingPerMonth, 0))

# Value #1 = PhoneCoTenure
# Value #2 = SpendingPerMonth
# Bin data into high/med/low for each value
# Value 1
phone_co_tenure_bins <- c(-1, 24, 52, Inf)
phone_co_tenure_labels <- c("Low", "Medium", "High")
data$Value1 <- cut(data$PhoneCoTenure, breaks = phone_co_tenure_bins, labels = phone_co_tenure_labels)

# Count N of customers in each
phone_co_tenure_counts <- table(data$Value1)
print(phone_co_tenure_counts)

# Value 2
spending_per_month_bins <- c(-1, 12, 38, Inf)
spending_per_month_labels <- c("Low", "Medium", "High")
data$Value2 <- cut(data$SpendingPerMonth, breaks = spending_per_month_bins, labels = spending_per_month_labels)

# Count N of customers in each
spending_per_month_counts <- table(data$Value2)
print(spending_per_month_counts)

# Count total N for combined groups
customer_counts <- data %>%
  group_by(Value1, Value2) %>%
  summarise(Count = n())
print(customer_counts)

# Name customer groups
data <- data %>%
  mutate(Segment = case_when(
    Value1 == "High" & Value2 == "High" ~ "Loyal Spenders",
    Value1 == "High" & Value2 == "Medium" ~ "Steady Investors",
    Value1 == "High" & Value2 == "Low" ~ "Economic Veterans",
    Value1 == "Medium" & Value2 == "High" ~ "Active Enthusiasts",
    Value1 == "Medium" & Value2 == "Medium" ~ "Balanced Supporters",
    Value1 == "Medium" & Value2 == "Low" ~ "Modest Maintainers",
    Value1 == "Low" & Value2 == "High" ~ "Emerging Engagers",
    Value1 == "Low" & Value2 == "Medium" ~ "Fresh Connectors",
    Value1 == "Low" & Value2 == "Low" ~ "Entry Economizers"
  ))

# Export to clean_data2 for Tableau
write.xlsx(data, "clean_data2.xlsx")
cat("data exported to clean_data2.xlsx successfully.\n")

# Decision Tree Segmentation
# Create value score column (PhoneCoTenure aka loyalty * TotalSpentLastMonth aka monetary value)
data$ValueScore <- data$PhoneCoTenure * data$TotalSpentLastMonth
summary(data$ValueScore)
hist(data$ValueScore)

# Bin into high/med/low value
value_score_bins <- c(-1, 800, 3000, Inf)
value_score_labels <- c("Low", "Medium", "High")
data$ValueScoreCat <- cut(data$ValueScore, breaks = value_score_bins, labels = value_score_labels)

# Count N of customers in each
value_score_counts <- table(data$ValueScoreCat)
print(value_score_counts)

# Create the decision tree model
data$ValueScoreCat <- as.factor(data$ValueScoreCat)
data$EquipmentRental <- as.factor(data$EquipmentRental)
data$WirelessData <- as.factor(data$WirelessData)
tree_model <- rpart(ValueScoreCat ~ Gender + JobCategory + LoanDefault + Generation + EducationYears + HHIncome + DebtToIncomeRatio + HouseholdSize + CardTenure + EquipmentRental + WirelessData, data = data)

# Plot the decision tree
options(repr.plot.width=10, repr.plot.height=8)
plot(tree_model, uniform=TRUE, main="Decision Tree", margin=0.05)
text(tree_model, cex=0.8, splits = TRUE)
prp(tree_model, cex = 1, compress = FALSE, varlen = 0)











