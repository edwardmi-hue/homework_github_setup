# ==============================================================================
# title: "Economic, Social, Environmental Analysis"
# subtitle: "STAT40830 - Advanced Data Programming with R"
# author: "Michael Edwards"
# date: 11-06-2025
# ==============================================================================

# Economic, Social, and Environmental Analysis Dashboard
# R Script to clean the data before visualizing a Heatmap


# ==============================================================================
# DATA CLEANING AND CORRELATION HEATMAP
# ==============================================================================

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)

# ==============================================================================
# STEP 1: COMPREHENSIVE DATA CLEANING
# ==============================================================================

clean_data_for_heatmap <- function(df) {
  
  cat("=== ORIGINAL DATA DIAGNOSTICS ===\n")
  cat("Original data dimensions:", nrow(df), "rows x", ncol(df), "columns\n")
  cat("Countries:", paste(unique(df$Country.Name), collapse = ", "), "\n")
  cat("Value column type:", class(df$Value), "\n")
  
  # Check for duplicates BEFORE cleaning
  duplicates_before <- df %>%
    group_by(Country.Name, Year, Indicator.Name) %>%
    summarise(n = n(), .groups = 'drop') %>%
    filter(n > 1)
  
  cat("Duplicate entries found:", nrow(duplicates_before), "\n")
  if(nrow(duplicates_before) > 0) {
    cat("Sample duplicates:\n")
    print(head(duplicates_before))
  }
  
  cat("\n=== CLEANING DATA ===\n")
  
  # Step 1: Convert Value column to numeric
  cat("Step 1: Converting Value column to numeric...\n")
  df$Value <- suppressWarnings(as.numeric(as.character(df$Value)))
  
  # Check how many values were lost in conversion
  na_after_conversion <- sum(is.na(df$Value))
  cat("NA values after numeric conversion:", na_after_conversion, "\n")
  
  # Step 2: Remove rows with NA values
  cat("Step 2: Removing NA values...\n")
  df_clean <- df %>%
    filter(!is.na(Value)) %>%
    filter(!is.na(Country.Name)) %>%
    filter(!is.na(Year)) %>%
    filter(!is.na(Indicator.Name))
  
  cat("Rows after removing NAs:", nrow(df_clean), "\n")
  
  # Step 3: Handle duplicates by keeping the most recent/reliable entry
  cat("Step 3: Handling duplicate entries...\n")
  df_clean <- df_clean %>%
    # Sort by year to prioritize more recent data
    arrange(Country.Name, Indicator.Name, desc(Year)) %>%
    # Remove exact duplicates first
    distinct() %>%
    # For remaining duplicates, keep the first entry (most recent due to sorting)
    group_by(Country.Name, Year, Indicator.Name) %>%
    slice(1) %>%
    ungroup()
  
  cat("Rows after removing duplicates:", nrow(df_clean), "\n")
  
  # Verify no duplicates remain
  duplicates_after <- df_clean %>%
    group_by(Country.Name, Year, Indicator.Name) %>%
    summarise(n = n(), .groups = 'drop') %>%
    filter(n > 1)
  
  cat("Remaining duplicates:", nrow(duplicates_after), "\n")
  
  cat("\n=== CLEANED DATA SUMMARY ===\n")
  cat("Final dimensions:", nrow(df_clean), "rows x", ncol(df_clean), "columns\n")
  cat("Countries:", paste(unique(df_clean$Country.Name), collapse = ", "), "\n")
  cat("Year range:", min(df_clean$Year, na.rm = TRUE), "to", max(df_clean$Year, na.rm = TRUE), "\n")
  cat("Number of indicators:", length(unique(df_clean$Indicator.Name)), "\n")
  
  return(df_clean)
}

# ==============================================================================
# STEP 2: CORRELATION HEATMAP FUNCTION (ROBUST VERSION)
# ==============================================================================

create_correlation_heatmap <- function(data) {
  
  cat("\n=== CREATING CORRELATION HEATMAP ===\n")
  
  # Define indicators for correlation analysis
  key_indicators <- c(
    "GDP per capita (current US$)",
    "Life expectancy at birth, total (years)",
    "School enrollment, tertiary (% gross)",
    "Unemployment, total (% of total labor force) (modeled ILO estimate)",
    "Urban population (% of total population)"
  )
  
  cat("Looking for indicators:\n")
  for(indicator in key_indicators) {
    count <- sum(data$Indicator.Name == indicator, na.rm = TRUE)
    cat("-", indicator, ":", count, "observations\n")
  }
  
  # Filter and prepare data for correlation
  cat("\nPreparing correlation data...\n")
  
  corr_data <- data %>%
    filter(Indicator.Name %in% key_indicators) %>%
    filter(!is.na(Value)) %>%
    # Make sure we have the required columns
    select(Country.Name, Year, Indicator.Name, Value) %>%
    # Pivot to wide format with explicit handling of multiple values
    pivot_wider(
      names_from = Indicator.Name, 
      values_from = Value,
      values_fn = mean  # Take mean if multiple values exist
    ) %>%
    # Remove rows with any missing values
    drop_na()
  
  cat("Data after pivot_wider:", nrow(corr_data), "rows x", ncol(corr_data), "columns\n")
  
  # Remove non-numeric columns for correlation
  numeric_data <- corr_data %>%
    select(-Country.Name, -Year)
  
  cat("Numeric columns for correlation:", ncol(numeric_data), "\n")
  cat("Column names:", paste(colnames(numeric_data), collapse = ", "), "\n")
  
  # Check if we have enough data
  if(nrow(numeric_data) < 5) {
    stop("Insufficient data for correlation analysis. Need at least 5 complete observations.")
  }
  
  if(ncol(numeric_data) < 2) {
    stop("Need at least 2 indicators for correlation analysis.")
  }
  
  # Verify all columns are numeric
  non_numeric_cols <- sapply(numeric_data, function(x) !is.numeric(x))
  if(any(non_numeric_cols)) {
    cat("Warning: Non-numeric columns detected:", paste(names(non_numeric_cols)[non_numeric_cols], collapse = ", "), "\n")
    # Force conversion to numeric
    numeric_data <- numeric_data %>%
      mutate(across(everything(), as.numeric))
  }
  
  # Calculate correlation matrix
  cat("Calculating correlation matrix...\n")
  corr_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Rename for better display
  short_names <- c(
    "GDP per capita (current US$)" = "GDP per Capita",
    "Life expectancy at birth, total (years)" = "Life Expectancy",
    "School enrollment, tertiary (% gross)" = "Tertiary Education", 
    "Unemployment, total (% of total labor force) (modeled ILO estimate)" = "Unemployment",
    "Urban population (% of total population)" = "Urbanization"
  )
  
  # Apply new names if columns exist
  current_names <- colnames(corr_matrix)
  new_names <- current_names
  for(i in seq_along(current_names)) {
    if(current_names[i] %in% names(short_names)) {
      new_names[i] <- short_names[current_names[i]]
    }
  }
  colnames(corr_matrix) <- new_names
  rownames(corr_matrix) <- new_names
  
  cat("Correlation matrix created successfully!\n")
  print(corr_matrix)
  
  # Create heatmap using corrplot
  if(require(corrplot, quietly = TRUE)) {
    cat("\nCreating heatmap with corrplot...\n")
    corrplot(corr_matrix, 
             method = "color",
             type = "full",
             order = "original",
             tl.col = "black",
             tl.srt = 45,
             addCoef.col = "black",
             number.cex = 0.8,
             title = "Cross-Indicator Correlation Matrix",
             mar = c(0, 0, 3, 0))
  }
  
  # Create heatmap using ggplot2
  cat("Creating heatmap with ggplot2...\n")
  corr_df <- as.data.frame(as.table(corr_matrix))
  names(corr_df) <- c("Var1", "Var2", "Correlation")
  
  p <- ggplot(corr_df, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(
      low = "#2166AC", 
      mid = "#F7F7F7", 
      high = "#B2182B", 
      midpoint = 0, 
      limit = c(-1, 1),
      name = "Correlation"
    ) +
    geom_text(aes(label = sprintf("%.2f", Correlation)), 
              color = "black", size = 3, fontface = "bold") +
    labs(
      title = "Cross-Indicator Correlation Matrix",
      x = "", y = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid = element_blank()
    ) +
    coord_fixed()
  
  print(p)
  
  return(list(matrix = corr_matrix, plot = p, data = corr_data))
}

# ==============================================================================
# STEP 3: EXECUTION
# ==============================================================================

# Load your data
cat("Loading data...\n")
df <- read.csv("analysis_ready_data.csv", stringsAsFactors = FALSE)

# Clean the data
cat("Cleaning data...\n")
df_clean <- clean_data_for_heatmap(df)

# Create the heatmap
cat("Creating heatmap...\n")
heatmap_result <- create_correlation_heatmap(df_clean)

# Save the plot
if(!is.null(heatmap_result$plot)) {
  ggsave("correlation_heatmap.png", heatmap_result$plot, 
         width = 10, height = 8, dpi = 300)
  cat("Heatmap saved as 'correlation_heatmap.png'\n")
}

# Display the correlation matrix
cat("\n=== FINAL CORRELATION MATRIX ===\n")
print(round(heatmap_result$matrix, 3))

#================================================
# END OF FILE
#================================================