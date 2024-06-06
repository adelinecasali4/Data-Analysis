# Read in data and packages
data <- read.csv("Customer_Dataset_Data.csv")
library(tidyverse)
library(openxlsx)

# Data exploration
# View the data
view(data)

# Remove empty column x
data <- data[, -15]
view(data)

# Separate into qualitative and quantitative data
dimensions <- data[, c(1, 2, 3, 4, 7, 8, 10, 15, 16, 24, 25, 28, 29, 30, 34, 38, 41, 42, 45, 46, 47, 48, 49, 50, 51, 52, 53, 55, 56, 57, 58, 59)]
view(dimensions)
measures <- data[, c(1, 5, 6, 9, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 23, 26, 27, 31, 32, 33, 35, 36, 37, 39, 40, 43, 44, 54)]
view(measures)

# Check column data types
str(dimensions)
# Convert TownSize to int
dimensions$TownSize <- as.integer(dimensions$TownSize)
str(measures)
# Remove $ and , from HHIncome, CarValue, CardSpendMonth, VoiceLastMonth, VoiceOverTenure, EquipmentLastMonth, EquipmentOverTenure, DataLastMonth, and DataOverTenure and convert to numeric
clean_and_convert <- function(x) {
  as.integer(gsub("[,$-]", "", x))
}
# Apply the function to each column
measures[, c("HHIncome", "CarValue", "CardSpendMonth", "VoiceLastMonth", "VoiceOverTenure", "EquipmentLastMonth", "EquipmentOverTenure", "DataLastMonth", "DataOverTenure")] <- as.data.frame(lapply(data[, c("HHIncome", "CarValue", "CardSpendMonth", "VoiceLastMonth", "VoiceOverTenure", "EquipmentLastMonth", "EquipmentOverTenure", "DataLastMonth", "DataOverTenure")], clean_and_convert))
# Replace NA values with 0
measures$HHIncome[is.na(measures$HHIncome)] <- 0
measures$CarValue[is.na(measures$CarValue)] <- 0
measures$CardSpendMonth[is.na(measures$CardSpendMonth)] <- 0
measures$VoiceLastMonth[is.na(measures$VoiceLastMonth)] <- 0
measures$VoiceOverTenure[is.na(measures$VoiceOverTenure)] <- 0
measures$EquipmentLastMonth[is.na(measures$EquipmentLastMonth)] <- 0
measures$EquipmentOverTenure[is.na(measures$EquipmentOverTenure)] <- 0
measures$DataLastMonth[is.na(measures$DataLastMonth)] <- 0
measures$DataOverTenure[is.na(measures$DataOverTenure)] <- 0
# Convert CommuteTime to int
measures$CommuteTime <- as.integer(measures$CommuteTime)

# View unique values of dimensional data
for (col in names(dimensions[, -1])) {
  cat("Distinct values in column", col, ": ", unique(dimensions[[col]]), "\n")
}
# Convert Region from number to character
region_mapping <- c("NorthEast", "Midwest", "West", "Southwest", "Southeast")
dimensions$Region_char <- region_mapping[dimensions$Region]
dimensions$Region_char <- as.character(dimensions$Region_char)
# Convert TownSize from number to character
townsize_mapping <- c("Very Large", "Large", "Medium", "Small", "Very Small")
dimensions$TownSize_desc <- townsize_mapping[dimensions$TownSize]
# Check how many -1 values in CarBrand and convert to No Car
sum(grepl("-1", dimensions$CarBrand, fixed = TRUE))
dimensions$CarBrand <- gsub("-1", NA, dimensions$CarBrand, fixed = TRUE)
# Check how many -1 values in CarOwnership and convert to NA
sum(grepl("-1", dimensions$CarOwnership, fixed = TRUE))
dimensions$CarOwnership <- gsub("-1", NA, dimensions$CarOwnership, fixed = TRUE)
# Check how many 2, 3, 4, values in Internet
sum(grepl("2", dimensions$Internet, fixed = TRUE))
sum(grepl("3", dimensions$Internet, fixed = TRUE))
sum(grepl("4", dimensions$Internet, fixed = TRUE))
dimensions$Internet <- gsub("2", NA, dimensions$Internet, fixed = TRUE)
dimensions$Internet <- gsub("3", NA, dimensions$Internet, fixed = TRUE)
dimensions$Internet <- gsub("4", NA, dimensions$Internet, fixed = TRUE)

# Search for outliers in measures
summary(measures)
# Number of pets doesn't add up to dogs + cats + birds, but people could have other pets

# Confirm all values make sense
for (col in names(dimensions[, -1])) {
  cat("Distinct values in column", col, ": ", unique(dimensions[[col]]), "\n")
}
summary(measures)
str(dimensions)
str(measures)
view(measures)
view(dimensions)

# Combine into clean_data based on CustomerID
clean_data <- merge(dimensions, measures, by = "CustomerID")
view(clean_data)

# Search for NA values
colSums(is.na(clean_data))
# Remove NA values for columns with <250 values
na_counts <- colSums(is.na(clean_data))
cols_to_filter <- names(which(na_counts < 250))
clean_data <- clean_data[complete.cases(clean_data[cols_to_filter]), ]

# Confirm data is clean
for (col in names(clean_data[, -1])) {
  cat("Distinct values in column", col, ": ", unique(clean_data[[col]]), "\n")
}
summary(clean_data)
str(clean_data)
colSums(is.na(clean_data))
view(clean_data)

# Complete feature engineering
# Rural and Urban based on town size
clean_data$Location <- ifelse(clean_data$TownSize %in% c("Small", "Very Small"), "Rural", "Urban")
# Generation based on age
age_ranges <- c(18, 28, 44, 60, 80)
generation_labels <- c("Gen Z", "Millennial", "Gen X", "Boomer")
clean_data$Generation <- cut(clean_data$Age, breaks = age_ranges, labels = generation_labels, right = FALSE)
clean_data$Generation <- as.character(clean_data$Generation)
# Education level based on education years
education_ranges <- c(-Inf, 12, 15, 17, 18, Inf)
education_labels <- c("Less than high school", "High school", "Some college or associate's", "Bachelor's degree", "Graduate degree")
clean_data$EducationLevel <- cut(clean_data$EducationYears, breaks = education_ranges, labels = education_labels)
clean_data$EducationLevel <- as.character(clean_data$EducationLevel)
# All debt from credit debt + other debt
clean_data$AllDebt <- clean_data$CreditDebt + clean_data$OtherDebt
# Pets per person from household size and number of pets
clean_data$PetsPerPerson <- clean_data$NumberPets / clean_data$HouseholdSize
clean_data$PetsPerPerson <- round(clean_data$PetsPerPerson, 2)
# Total over tenure based on Data, Equipment, and Voice over tenure
clean_data$TotalOverTenure <- clean_data$DataOverTenure + clean_data$EquipmentOverTenure + clean_data$VoiceOverTenure
# Avg price card spend based on cardspendmonth and carditemsmonthly
clean_data$AvgCardSpendPerItem <- clean_data$CardSpendMonth / clean_data$CardItemsMonthly
clean_data$AvgCardSpendPerItem <- round(clean_data$AvgCardSpendPerItem, 2)
# Age to employment length ratio
clean_data$AgeToEmploymentRatio <- clean_data$Age / clean_data$EmploymentLength
clean_data$AgeToEmploymentRatio <- round(clean_data$AgeToEmploymentRatio, 2)
clean_data$AgeToEmploymentRatio <- replace(clean_data$AgeToEmploymentRatio, is.infinite(clean_data$AgeToEmploymentRatio), 0)
# Amount spent from data, equipment, and voice last month
clean_data$TotalSpentLastMonth <- clean_data$DataLastMonth + clean_data$EquipmentLastMonth + clean_data$VoiceLastMonth
# Card spending to income ratio (ie what percent of income is card spending)
clean_data$CardSpendingToIncomeRatio <- clean_data$CardSpendMonth / clean_data$HHIncome
clean_data$CardSpendingToIncomeRatio <- round(clean_data$CardSpendingToIncomeRatio, 2)

# View new columns
for (col in names(clean_data[, -1])) {
  cat("Distinct values in column", col, ": ", unique(clean_data[[col]]), "\n")
}
summary(clean_data)
str(clean_data)
colSums(is.na(clean_data))
# Remove NA values for columns with <50 values
na_counts <- colSums(is.na(clean_data))
cols_to_filter <- names(which(na_counts < 50))
clean_data <- clean_data[complete.cases(clean_data[cols_to_filter]), ]
colSums(is.na(clean_data))
view(clean_data)

# Export to Excel
write.xlsx(clean_data, "clean_data.xlsx")
cat("clean_data exported to clean_data.xlsx successfully.\n")

# Export to Excel with original data 
wb <- createWorkbook()
addWorksheet(wb, "clean_data")
writeData(wb, sheet = "clean_data", clean_data)
addWorksheet(wb, "data")
writeData(wb, sheet = "data", data)
saveWorkbook(wb, "all_data.xlsx", overwrite = TRUE)
cat("clean_data and data exported to all_data.xlsx successfully.\n")

