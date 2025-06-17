# ==============================================================================
# title: "Economic, Social, Environmental Analysis"
# subtitle: "STAT40830 - Advanced Data Programming with R"
# author: "Michael Edwards"
# date: 11-06-2025
# ==============================================================================

# Economic, Social, and Environmental Analysis Dashboard
# R Script to reproduce the visualizations

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(corrplot)
library(fmsb)
library(viridis)
library(scales)
library(RColorBrewer)


# Setup the working directory 
getwd()
#setwd("H:/WORKSPACE/UCD/STAT40830/ASSIGNMENT 1")
#getwd()

# Set theme for consistent plotting
theme_set(theme_minimal() + 
            theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
                  axis.title = element_text(size = 10),
                  axis.text = element_text(size = 9),
                  legend.title = element_text(size = 10),
                  legend.text = element_text(size = 9)))

# Load and prepare data
df <- read.csv("analysis_ready_data.csv", stringsAsFactors = FALSE)

# Define country colors for consistency
country_colors <- c("United States" = "#2E8B57", 
                    "United Kingdom" = "#9370DB", 
                    "Germany" = "#4682B4")

# ==============================================================================
# VISUALIZATION 1: COMPREHENSIVE DASHBOARD
# ==============================================================================

create_dashboard <- function(data) {
  
  # 1. GDP Per Capita Trends
  gdp_data <- data %>%
    filter(Indicator.Name == "GDP per capita (current US$)") %>%
    filter(!is.na(Value)) %>%
    mutate(Value = Value / 1000)  # Convert to thousands
  
  p1 <- ggplot(gdp_data, aes(x = Year, y = Value, color = Country.Name)) +
    geom_line(size = 1) +
    scale_color_manual(values = country_colors) +
    labs(title = "GDP Per Capita Trends (Thousands USD)",
         x = "Year", y = "GDP per Capita (000s USD)", color = "") +
    theme(legend.position = "top") +
    scale_x_continuous(breaks = seq(1960, 2020, 20))
  
  # 2. Life Expectancy Trends
  life_exp_data <- data %>%
    filter(Indicator.Name == "Life expectancy at birth, total (years)") %>%
    filter(!is.na(Value))
  
  p2 <- ggplot(life_exp_data, aes(x = Year, y = Value, color = Country.Name)) +
    geom_line(size = 1) +
    scale_color_manual(values = country_colors) +
    labs(title = "Life Expectancy Trends (Years)",
         x = "Year", y = "Life Expectancy (Years)", color = "") +
    theme(legend.position = "top") +
    scale_x_continuous(breaks = seq(1960, 2020, 20))
  
  # 3. Tertiary Education Enrollment
  tertiary_data <- data %>%
    filter(Indicator.Name == "School enrollment, tertiary (% gross)") %>%
    filter(!is.na(Value))
  
  p3 <- ggplot(tertiary_data, aes(x = Year, y = Value, color = Country.Name)) +
    geom_line(size = 1) +
    scale_color_manual(values = country_colors) +
    labs(title = "Tertiary Education Enrollment (%)",
         x = "Year", y = "Enrollment (%)", color = "") +
    theme(legend.position = "top") +
    scale_x_continuous(breaks = seq(1970, 2020, 10))
  
  # 4. Unemployment Rate
  unemp_data <- data %>%
    filter(Indicator.Name == "Unemployment, total (% of total labor force) (modeled ILO estimate)") %>%
    filter(!is.na(Value)) %>%
    filter(Year >= 1990)  # Focus on recent data
  
  p4 <- ggplot(unemp_data, aes(x = Year, y = Value, color = Country.Name)) +
    geom_line(size = 1) +
    scale_color_manual(values = country_colors) +
    labs(title = "Unemployment Rate (%)",
         x = "Year", y = "Unemployment Rate (%)", color = "") +
    theme(legend.position = "top") +
    scale_x_continuous(breaks = seq(1990, 2020, 10))
  
  # 5. Forest Area
  forest_data <- data %>%
    filter(Indicator.Name == "Forest area (% of land area)") %>%
    filter(!is.na(Value)) %>%
    filter(Year >= 1990)
  
  p5 <- ggplot(forest_data, aes(x = Year, y = Value, color = Country.Name)) +
    geom_line(size = 1) +
    scale_color_manual(values = country_colors) +
    labs(title = "Forest Area (% of Land)",
         x = "Year", y = "Forest Area (%)", color = "") +
    theme(legend.position = "top") +
    scale_x_continuous(breaks = seq(1990, 2020, 10))
  
  # 6. Health Expenditure
  health_data <- data %>%
    filter(Indicator.Name == "Current health expenditure (% of GDP)") %>%
    filter(!is.na(Value)) %>%
    filter(Year >= 2000)
  
  p6 <- ggplot(health_data, aes(x = Year, y = Value, color = Country.Name)) +
    geom_line(size = 1) +
    scale_color_manual(values = country_colors) +
    labs(title = "Health Expenditure (% of GDP)",
         x = "Year", y = "Health Expenditure (%)", color = "") +
    theme(legend.position = "top") +
    scale_x_continuous(breaks = seq(2000, 2020, 5))
  
  # Combine all plots
  grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, 
               top = "Economic, Social, and Environmental Analysis Dashboard")
}

# ==============================================================================
# VISUALIZATION 2: ECONOMIC CONVERGENCE ANALYSIS
# ==============================================================================

create_convergence_analysis <- function(data) {
  
  # Prepare GDP per capita data for convergence analysis
  gdp_data <- data %>%
    filter(Indicator.Name == "GDP per capita (current US$)") %>%
    filter(!is.na(Value)) %>%
    select(Country.Name, Year, Value) %>%
    arrange(Country.Name, Year)
  
  # Calculate relative GDP (base year 2000 = 100)
  gdp_base <- gdp_data %>%
    filter(Year == 2000) %>%
    select(Country.Name, base_value = Value)
  
  gdp_relative <- gdp_data %>%
    left_join(gdp_base, by = "Country.Name") %>%
    mutate(relative_gdp = (Value / base_value) * 100) %>%
    filter(!is.na(relative_gdp))
  
  # Main trend plot
  p1 <- ggplot(gdp_relative, aes(x = Year, y = relative_gdp, color = Country.Name)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = country_colors) +
    labs(title = "Economic Convergence Analysis: GDP Per Capita Trends",
         subtitle = "Relative GDP Per Capita (2000 = 100)",
         x = "Year", y = "Relative GDP Per Capita (2000 = 100)", color = "") +
    theme(legend.position = "top") +
    scale_x_continuous(breaks = seq(1960, 2020, 10))
  
  # Calculate sigma-convergence (coefficient of variation over time)
  sigma_conv <- gdp_data %>%
    group_by(Year) %>%
    summarise(
      mean_gdp = mean(Value, na.rm = TRUE),
      sd_gdp = sd(Value, na.rm = TRUE),
      cv = sd_gdp / mean_gdp,
      .groups = 'drop'
    ) %>%
    filter(!is.na(cv))
  
  p2 <- ggplot(sigma_conv, aes(x = Year, y = cv)) +
    geom_line(color = "#4682B4", size = 1) +
    geom_point(color = "#4682B4", size = 1.5) +
    labs(title = "σ-Convergence: Dispersion Over Time",
         x = "Year", y = "Coefficient of Variation") +
    scale_x_continuous(breaks = seq(1960, 2020, 10))
  
  # Calculate average annual growth rates
  growth_rates <- gdp_data %>%
    group_by(Country.Name) %>%
    arrange(Year) %>%
    filter(Year >= 1960, Year <= 2020) %>%
    summarise(
      start_value = first(Value),
      end_value = last(Value),
      years = last(Year) - first(Year),
      growth_rate = ((end_value / start_value) ^ (1/years) - 1) * 100,
      .groups = 'drop'
    )
  
  p3 <- ggplot(growth_rates, aes(x = Country.Name, y = growth_rate, fill = Country.Name)) +
    geom_col() +
    scale_fill_manual(values = country_colors) +
    labs(title = "Average Annual Growth Rates",
         x = "", y = "Growth Rate (%)") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = paste0(round(growth_rate, 2), "%")), 
              vjust = -0.5, size = 3)
  
  # Combine plots
  grid.arrange(p1, 
               arrangeGrob(p2, p3, ncol = 2), 
               ncol = 1, heights = c(2, 1))
}

# ==============================================================================
# VISUALIZATION 3: CROSS-INDICATOR CORRELATION MATRIX
# ==============================================================================

# Combined in data_cleaning.R 

# ==============================================================================
# VISUALIZATION 4: SOCIAL DEVELOPMENT RADAR CHART
# ==============================================================================

create_radar_chart <- function(data) {
  
  # Select social development indicators
  social_indicators <- c(
    "Life expectancy at birth, total (years)",
    "School enrollment, tertiary (% gross)",
    "Current health expenditure (% of GDP)"
  )
  
  # Get latest available data for each country
  radar_data <- data %>%
    filter(Indicator.Name %in% social_indicators) %>%
    filter(!is.na(Value)) %>%
    group_by(Country.Name, Indicator.Name) %>%
    filter(Year == max(Year)) %>%
    ungroup() %>%
    select(Country.Name, Indicator.Name, Value) %>%
    pivot_wider(names_from = Indicator.Name, values_from = Value)
  
  # Normalize to 0-100 scale
  radar_normalized <- radar_data %>%
    mutate(
      `Life Expectancy` = scales::rescale(`Life expectancy at birth, total (years)`, to = c(0, 100)),
      `Tertiary Education` = scales::rescale(`School enrollment, tertiary (% gross)`, to = c(0, 100)),
      `Health Expenditure` = scales::rescale(`Current health expenditure (% of GDP)`, to = c(0, 100))
    ) %>%
    select(Country.Name, `Life Expectancy`, `Tertiary Education`, `Health Expenditure`)
  
  # Prepare data for fmsb radar chart
  radar_matrix <- as.data.frame(t(radar_normalized[, -1]))
  colnames(radar_matrix) <- radar_normalized$Country.Name
  
  # Add max and min rows (required by fmsb)
  radar_matrix <- rbind(rep(100, 3), rep(0, 3), radar_matrix)
  
  # Create radar chart
  colors_border <- c("#2E8B57", "#9370DB", "#4682B4")
  colors_fill <- alpha(colors_border, 0.3)
  
  radarchart(radar_matrix,
             axistype = 1,
             pcol = colors_border,
             pfcol = colors_fill,
             plwd = 2,
             plty = 1,
             cglcol = "grey",
             cglty = 1,
             axislabcol = "grey",
             caxislabels = seq(0, 100, 20),
             cglwd = 0.8,
             vlcex = 0.8,
             title = "Social Development Indicators Comparison\n(Normalized 0-100 Scale)")
  
  # Add legend
  legend(x = 0.8, y = 1.3, 
         legend = colnames(radar_matrix), 
         bty = "n", pch = 20, col = colors_border, 
         text.col = "black", cex = 0.9, pt.cex = 1.5)
}

# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

# Execute all visualizations
cat("Creating Comprehensive Dashboard...\n")
create_dashboard(df)

cat("Creating Economic Convergence Analysis...\n")
create_convergence_analysis(df)

cat("Creating Correlation Matrix...\n")
source("data_cleaning.R")

cat("Creating Social Development Radar Chart...\n")
create_radar_chart(df)

# Additional analysis: Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")

# Latest GDP per capita
latest_gdp <- df %>%
  filter(Indicator.Name == "GDP per capita (current US$)") %>%
  filter(!is.na(Value)) %>%
  group_by(Country.Name) %>%
  filter(Year == max(Year)) %>%
  arrange(desc(Value))

cat("Latest GDP per capita (USD):\n")
print(latest_gdp[, c("Country.Name", "Year", "Value")])

# Beta-convergence coefficient calculation
cat("\nBeta-convergence analysis:\n")
gdp_conv <- df %>%
  filter(Indicator.Name == "GDP per capita (current US$)") %>%
  filter(!is.na(Value)) %>%
  group_by(Country.Name) %>%
  arrange(Year) %>%
  summarise(
    initial_gdp = first(Value),
    final_gdp = last(Value),
    years = last(Year) - first(Year),
    log_initial = log(initial_gdp),
    growth_rate = log(final_gdp / initial_gdp) / years,
    .groups = 'drop'
  )

if(nrow(gdp_conv) > 1) {
  beta_model <- lm(growth_rate ~ log_initial, data = gdp_conv)
  cat("Beta-convergence coefficient:", round(coef(beta_model)[2], 4), "\n")
  cat("Convergence rate:", round(-coef(beta_model)[2] * 100, 2), "% per year\n")
  cat("R-squared:", round(summary(beta_model)$r.squared, 3), "\n")
}

cat("\nAnalysis complete! All visualizations have been generated.\n")

#================================================
# END OF FILE
#================================================
