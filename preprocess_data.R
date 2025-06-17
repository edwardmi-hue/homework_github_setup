# ==============================================================================
# title: "Economic, Social, Environmental Analysis"
# subtitle: "STAT40830 - Advanced Data Programming with R"
# author: "Michael Edwards"
# date: 11-06-2025
# ==============================================================================


# Economic, Social, and Environmental Analysis Dashboard
# R Script to preprocess data into csv file analysis_ready_data.csv


# ==============================================================================
# CREATE ANALYSIS_READY_DATA.CSV FROM INDIVIDUAL COUNTRY FILES
# ==============================================================================

# Load required libraries
library(dplyr)
library(readr)

# Setup the working directory 
getwd()
#setwd("H:/WORKSPACE/UCD/STAT40830/ASSIGNMENT 1")
#getwd()

# ==============================================================================
# STEP 1: LOAD AND COMBINE INDIVIDUAL COUNTRY FILES
# ==============================================================================

cat("=== LOADING INDIVIDUAL COUNTRY FILES ===\n")

# Define file paths
files <- c(
  "indicators_deu.csv",
  "indicators_gbr.csv", 
  "indicators_usa.csv"
)

# Check if files exist
for(file in files) {
  if(!file.exists(file)) {
    stop("File not found: ", file, ". Please ensure all files are in the working directory.")
  }
}

# Load each file
cat("Loading Germany data (indicators_deu.csv)...\n")
deu_data <- read_csv("indicators_deu.csv", show_col_types = FALSE)
cat("- Germany data loaded:", nrow(deu_data), "rows\n")

cat("Loading UK data (indicators_gbr.csv)...\n")
gbr_data <- read_csv("indicators_gbr.csv", show_col_types = FALSE)
cat("- UK data loaded:", nrow(gbr_data), "rows\n")

cat("Loading USA data (indicators_usa.csv)...\n")
usa_data <- read_csv("indicators_usa.csv", show_col_types = FALSE)
cat("- USA data loaded:", nrow(usa_data), "rows\n")

# Display structure of first file to understand the data
cat("\nStructure of Germany data (sample):\n")
str(deu_data)
cat("\nFirst few rows of Germany data:\n")
print(head(deu_data))

# ==============================================================================
# STEP 2: COMBINE ALL COUNTRY DATA
# ==============================================================================

cat("\n=== COMBINING COUNTRY DATA ===\n")

# Combine all datasets
combined_data <- bind_rows(deu_data, gbr_data, usa_data)

cat("Combined data dimensions:", nrow(combined_data), "rows x", ncol(combined_data), "columns\n")
cat("Countries in combined data:", paste(unique(combined_data$`Country Name`), collapse = ", "), "\n")

# ==============================================================================
# STEP 3: DATA TYPE CONVERSIONS AND CLEANING
# ==============================================================================

cat("\n=== CLEANING AND CONVERTING DATA TYPES ===\n")

# Clean and convert the data
analysis_data <- combined_data %>%
  # Rename columns to match the target format (remove spaces)
  rename(
    Country.Name = `Country Name`,
    Country.ISO3 = `Country ISO3`,
    Indicator.Name = `Indicator Name`,
    Indicator.Code = `Indicator Code`
  ) %>%
  # Convert Year from string to integer
  mutate(
    Year = as.integer(Year),
    # Convert Value from string to numeric (float)
    Value = as.numeric(Value),
    # Add Country Code (same as ISO3)
    Country.Code = Country.ISO3,
    # Extract Category from first 2 characters of Indicator Code
    Category = substr(Indicator.Code, 1, 2)
  ) %>%
  # Remove rows where conversion failed (NA values)
  filter(
    !is.na(Year),
    !is.na(Value)
  ) %>%
  # Reorder columns to match target format
  select(
    Country.Name,
    Country.ISO3, 
    Year,
    Indicator.Name,
    Indicator.Code,
    Value,
    Country.Code,
    Category
  ) %>%
  # Remove any duplicate rows
  distinct()

cat("After cleaning and conversion:\n")
cat("- Final dimensions:", nrow(analysis_data), "rows x", ncol(analysis_data), "columns\n")
cat("- Year range:", min(analysis_data$Year, na.rm = TRUE), "to", max(analysis_data$Year, na.rm = TRUE), "\n")
cat("- Countries:", paste(unique(analysis_data$Country.Name), collapse = ", "), "\n")
cat("- Number of indicators:", length(unique(analysis_data$Indicator.Name)), "\n")
cat("- Categories found:", paste(sort(unique(analysis_data$Category)), collapse = ", "), "\n")

# ==============================================================================
# STEP 4: DATA QUALITY CHECKS
# ==============================================================================

cat("\n=== DATA QUALITY CHECKS ===\n")

# Check for missing values
missing_counts <- analysis_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Missing_Count")

cat("Missing values by column:\n")
print(missing_counts)

# Check for duplicates
duplicates <- analysis_data %>%
  group_by(Country.Name, Year, Indicator.Name) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n > 1)

cat("\nDuplicate entries:", nrow(duplicates), "\n")
if(nrow(duplicates) > 0) {
  cat("Sample duplicates:\n")
  print(head(duplicates))
}

# Check value ranges
cat("\nValue statistics:\n")
cat("- Min value:", min(analysis_data$Value, na.rm = TRUE), "\n")
cat("- Max value:", max(analysis_data$Value, na.rm = TRUE), "\n")
cat("- Number of negative values:", sum(analysis_data$Value < 0, na.rm = TRUE), "\n")
cat("- Number of zero values:", sum(analysis_data$Value == 0, na.rm = TRUE), "\n")

# Show sample of key indicators
key_indicators <- c(
  "GDP per capita (current US$)",
  "Life expectancy at birth, total (years)",
  "School enrollment, tertiary (% gross)",
  "Unemployment, total (% of total labor force) (modeled ILO estimate)"
)

cat("\nSample data for key indicators:\n")
for(indicator in key_indicators) {
  count <- sum(analysis_data$Indicator.Name == indicator, na.rm = TRUE)
  if(count > 0) {
    cat("- '", indicator, "': ", count, " observations\n", sep = "")
    sample_data <- analysis_data %>%
      filter(Indicator.Name == indicator) %>%
      slice_head(n = 3) %>%
      select(Country.Name, Year, Value)
    print(sample_data)
  } else {
    cat("- '", indicator, "': NOT FOUND\n", sep = "")
  }
}

# ==============================================================================
# STEP 5: SAVE THE FINAL FILE
# ==============================================================================

cat("\n=== SAVING ANALYSIS_READY_DATA.CSV ===\n")

# Save the cleaned and combined data
write_csv(analysis_data, "analysis_ready_data.csv")

cat("File saved successfully as 'analysis_ready_data.csv'\n")
cat("Final file contains", nrow(analysis_data), "rows and", ncol(analysis_data), "columns\n")

# ==============================================================================
# STEP 6: VERIFICATION
# ==============================================================================

cat("\n=== VERIFICATION ===\n")

# Read back the saved file to verify
verification_data <- read_csv("analysis_ready_data.csv", show_col_types = FALSE)

cat("Verification read successful:\n")
cat("- Dimensions:", nrow(verification_data), "rows x", ncol(verification_data), "columns\n")
cat("- Column names:", paste(colnames(verification_data), collapse = ", "), "\n")
cat("- Column types:\n")
str(verification_data)

cat("\n=== PROCESS COMPLETE ===\n")
cat("Your analysis_ready_data.csv file is ready for use!\n")
cat("You can now run the correlation heatmap script with this cleaned data.\n")

# ==============================================================================
# OPTIONAL: SHOW SAMPLE OF FINAL DATA
# ==============================================================================

cat("\n=== SAMPLE OF FINAL DATA ===\n")
print(head(analysis_data, 10))

cat("\nSummary by country:\n")
country_summary <- analysis_data %>%
  group_by(Country.Name) %>%
  summarise(
    Records = n(),
    Year_Range = paste(min(Year), "-", max(Year)),
    Indicators = n_distinct(Indicator.Name),
    .groups = 'drop'
  )
print(country_summary)

#================================================
# END OF FILE
#================================================