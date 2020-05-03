# Package: pc.analysis
# Title: Phillips Curve Analysis
# Version: 0.1
# Authors@R: person("Cory", "Combs", email = "cory.j.combs@outlook.com",
#                   role = c("aut", "cre"))
# Description: Custom functions for analysis of Phillps curve data, compiled for FI Consulting
# Depends: R (>= 3.1.0)


#### Data Preparation ####

## Basic Data

# Phillips Curve Data Cleaning
prepare.pc.data <- function(data = pc_raw){

  # Create working copy to preserve original data
  pc_cleaning <- data

  # Join the separate CPI listings and convert value names to tibble-appropriate forms
  pc_cleaning <- pc_cleaning %>%
    mutate(Data = replace(Data, Data == "Consumer Price Index" | Data == "CPI", "cpi"))
  pc_cleaning <- pc_cleaning %>%
    mutate(Data = replace(Data, Data == "Civilian Labor Force", "civ_labor_force"))
  pc_cleaning <- pc_cleaning %>%
    mutate(Data = replace(Data, Data == "Unemployment Level", "unemp_level"))

  # Transform variable listings to separate columns for analysis
  pc_cleaning <- pc_cleaning %>%
    spread(Data, Value)

  # Rename columns for consistency
  colnames(pc_cleaning)[1:2] <- c("year", "month")

  # Confirm user's sepcification of data granularity; if monthly, import, clean and integrate the FRED CLF data
  if (grain == "monthly") {
    print("Updating analysis with monthly labor data.")
    fred_clf <- read_csv(fred_clf_data)
    pc_cleaning <- full_join(pc_cleaning, fred_clf, by = c("year", "month"))
    pc_cleaning <- pc_cleaning %>% select(year, month, civ_labor_force = CLF16OV, cpi, unemp_level)
  } else if (grain == "quarterly") {
    print("Continuing analysis with quarterly labor data.")
  } else {
    warning("Please confirm choice of monthly or quarterly resolution. Monthly resolution requires fred_clf_data.")
  }

  # Transform date information into class Date
  pc_cleaning$date <- with(pc_cleaning, sprintf("%d-%02d", year, month))
  pc_cleaning$date <- as.Date(as.yearmon(pc_cleaning$date))

  # Calculate inflation and unemployment rate (U3)
  pc_cleaning$inflation <- (pc_cleaning$cpi / lag(pc_cleaning$cpi) - 1) * 100
  pc_cleaning$u3 <- (pc_cleaning$unemp_level / pc_cleaning$civ_labor_force) * 100

  # Simplify tibble
  pc_cleaning <- pc_cleaning %>% select(date, inflation, u3)

  # Save non-null values as final working tibble, "pc", and bind date to time series format
  # For quarterly data, null values include missing month values; for monthly data, this includes only the initial month used to calculate inflation.
  pc <- na.omit(pc_cleaning)

  return(pc)
}


## Natural Unemployment Rate Data

# Prepare natural unemployment rate data
prepare.nrou.data <- function (data = fred_nrou) {
  data$date <- with(data, sprintf("%d-%02d", year, month)) # Establish usable date values
  data$date <- as.Date(as.yearmon(data$date)) # Convert date values to class time series
  data <- data %>% select(date, nrou = NROU)
}

# Join natural unemployment rate data with base phillips curve data
join.nrou.data <- function(left = pc, right = fred_nrou_clean) {
  pc_nrou <- full_join(left, right, by = "date")
  pc_nrou <- na.omit(pc_nrou)
}


## U6 Unemployment Data

# Prepare U6 unemployment rate data
prepare.u6.data <- function (data = bls_u6) {
  bls_u6_melt <- melt(data, id = "Year", variable.name = "Month", value.name = "u6")
  bls_u6_melt$date <- with(bls_u6_melt, sprintf("%d-%02d", Year, Month)) # Establish usable date values
  bls_u6_melt$date <- as.Date(as.yearmon(bls_u6_melt$date)) # Convert date values to class time series
  bls_u6 <- bls_u6_melt %>% select(date, u6) %>% arrange(date)
}

# Join U6 unemployment rate data with base phillips curve data
join.u6.data <- function (left = pc, right = bls_u6_clean) {
  pc_u6 <- full_join(left, right, by = "date")
  pc_u6 <- na.omit(pc_u6)
}


#### Calculation ####

pc.model.linear <- function (data = pc, period_start = 1949, period_end = 2017) {

  # Slice selected period
  model_select_period <- data %>% filter(year(data$date) >= period_start & year(data$date) <= period_end)
  pc_model_linear <- lm(inflation ~ u3, data = model_select_period)

  return(pc_model_linear)
}

pc.model.nonlinear <- function (data = pc, period_start = 1949, period_end = 2017) {

  # Slice selected period
  model_select_period <- data %>% filter(year(data$date) >= period_start & year(data$date) <= period_end)
  pc_model_nonlinear <- lm(inflation ~ u3 + I(u3^2), data = model_select_period)

  return(pc_model_nonlinear)
}

# Calculate expected inflation values based on three different beta parameters (indicated as b1, b2, and b3)
#  Expected inflation infl_e is calculated as: infl_e = infl + b(U-U_n),
#  where U is the unemployment rate, U_n is the natural unemployment rate,
#  and b is the response parameter beta
expected.inflation.range <- function (data = pc_nrou, b1 = 1.25, b2 = 1.5, b3 = 2) {

  # Create working copy and presever original data
  pc_infl_exp <- pc_nrou

  # Calculate expected inflation values given each estimate of beta (b1, b2, b3)
  pc_infl_exp$infl_exp_b1 <- pc_infl_exp$inflation + b1*(pc_infl_exp$u3 - pc_infl_exp$nrou)
  pc_infl_exp$infl_exp_b2 <- pc_infl_exp$inflation + b2*(pc_infl_exp$u3 - pc_infl_exp$nrou)
  pc_infl_exp$infl_exp_b3 <- pc_infl_exp$inflation + b3*(pc_infl_exp$u3 - pc_infl_exp$nrou)

  return(pc_infl_exp)
}


#### Data Visualization ####

# Jointly plot inflation and the U3 unemployment rate over the specified period
plot.infl.u3 <- function (data = pc, period_start = 1948, period_end = 2017) {

  # Reshape data to enable both variables to appear on the y axis
  pc_melt <- melt(data, id = c("date"))

  # Filter selected period
  pc_melt_select_period <- pc_melt %>% filter(year(date) >= period_start & year(date) <= period_end)

  ggplot(data = pc_melt_select_period) +
    aes(x = date, y = value,
        color = variable, group = variable) +
    geom_line() +
    scale_color_manual(name = "Rates",
                       labels = c("Inflation", "Unemployment"),
                       values = c(custom_palette[1], custom_palette[2])) +
    labs(title = "Inflation and Unemployment Rates Over Time",
         subtitle = "1948-2017",
         x = "Year",
         y = "Rate",
         caption = "Sources: FI Consulting, Federal Reserve Bank of St. Louis") +
    scale_y_continuous(labels = function(x) format(paste0(x, "%"))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # Centers title
          plot.subtitle = element_text(hjust = 0.5),
          axis.line = element_line(size = 1, color = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5)) # Rotates and centers x labels
}

# Create plots of inflation over time for each decade in the dataset
# To access a plot for a single decade, select as var_name[i]
multiplot.infl.decades <- function (data = pc) {

  # Calculate decade groupings
  pc_decades <- mutate(data, decade = (year(data$date) %/% 10) * 10)

  # Construct imap of plots for each decade calculated
  pc_decades %>% group_split(decade) %>%
    imap(~ ggplot(data = .x ) +
           aes(date, inflation) +
           geom_line() +
           labs(title = "Inflation Over Time",
                subtitle = "By Decade",
                x = "Year",
                y = "Inflation Rate",
                caption = "Sources: FI Consulting, Federal Reserve Bank of St. Louis") +
           scale_y_continuous(labels = function(x) format(paste0(x, "%"))) +
           theme_minimal() +
           theme(plot.title = element_text(hjust = 0.5), # Centers title
                 plot.subtitle = element_text(hjust = 0.5),
                 axis.line = element_line(size = 1, color = "black"),
                 axis.text.x = element_text(angle = 90, vjust = 0.5))) # Rotates and centers x labels
}

# Create plots of unemployment (U3) over time for each decade in the dataset
# To access a plot for a single decade, select as var_name[i]
multiplot.unemp.decades <- function (data = pc) {

  # Calculate decade groupings
  pc_decades <- mutate(data, decade = (year(data$date) %/% 10) * 10)

  # Construct imap of plots for each decade calculated
  pc_decades %>% group_split(decade) %>%
    imap(~ ggplot(data = .x ) +
           aes(date, u3) +
           geom_line() +
           labs(title = "Unemployment Over Time",
                subtitle = "By Decade",
                x = "Year",
                y = "Unemployment Rate (U3)",
                caption = "Sources: FI Consulting, Federal Reserve Bank of St. Louis") +
           scale_y_continuous(labels = function(x) format(paste0(x, "%"))) +
           theme_minimal() +
           theme(plot.title = element_text(hjust = 0.5), # Centers title
                 plot.subtitle = element_text(hjust = 0.5),
                 axis.line = element_line(size = 1, color = "black"),
                 axis.text.x = element_text(angle = 90, vjust = 0.5))) # Rotates and centers x labels
}

# Plot inflation over time for the specified period
plot.inflation <- function(data = pc, period_start = 1948, period_end = 2017) {

  # Filter selected period
  infl_select_period <- data %>% filter(year(data$date) >= period_start & year(data$date) <= period_end)

  ggplot(data = infl_select_period) +
    aes(x = date, y = inflation) +
    geom_line() +
    # geom_smooth(method = "lm", # Commented out for final product, but retained for future use
    #             formula = y ~ x,
    #             se = TRUE) +
    labs(title = "Inflation Over Time",
         subtitle = paste("From", period_start, "to", period_end),
         x = "Year",
         y = "Inflation",
         caption = "Sources: FI Consulting, Federal Reserve Bank of St. Louis") +
    scale_y_continuous(labels = function(x) format(paste0(x, "%"))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # Centers title
          plot.subtitle = element_text(hjust = 0.5),
          axis.line = element_line(size = 1, color = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5)) # Rotates and centers x labels
}

# Plot the U3 unemployment rate over time for the specified period
plot.u3 <- function(data = pc, period_start = 1948, period_end = 2017) {

  # Filter selected period
  u3_select_period <- data %>% filter(year(data$date) >= period_start & year(data$date) <= period_end)

  ggplot(data = u3_select_period) +
    aes(x = date, y = u3) +
    geom_point() +
    # geom_smooth(method = "lm", # Commented out for final product, but retained for future use
    #             formula = y ~ x + I(x^2),
    #             se = TRUE) +
    scale_color_manual(values = c(custom_palette[1], custom_palette[2])) +
    labs(title = "U3 Unemployment Rate Over Time",
         subtitle = paste("From", period_start, "to", period_end),
         x = "Year",
         y = "U3 Unemployment Rate",
         caption = "Sources: FI Consulting, Federal Reserve Bank of St. Louis") +
    scale_y_continuous(labels = function(x) format(paste0(x, "%"))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # Centers title
          plot.subtitle = element_text(hjust = 0.5),
          axis.line = element_line(size = 1, color = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5)) # Rotates and centers x labels
}

# Plot the base Phillips curve for the specified period
plot.pc <- function(data = pc, period_start = 1949, period_end = 2017) {

  # Filter selected period
  pc_select_period <- data %>% filter(year(data$date) >= period_start & year(data$date) <= period_end)

  ggplot(data = pc_select_period) +
    aes(x = u3, y = inflation) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x + I(x^2),
                se = TRUE) +
    scale_color_manual(values = c(custom_palette[1], custom_palette[2])) +
    labs(title = "Phillips Curve",
         subtitle = paste("From", period_start, "to", period_end),
         x = "Unemployment Rate (U3)",
         y = "Inflation",
         caption = "Sources: FI Consulting, Federal Reserve Bank of St. Louis") +
    scale_x_continuous(labels = function(x) format(paste0(x, "%"))) +
    scale_y_continuous(labels = function(x) format(paste0(x, "%"))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # Centers title
          plot.subtitle = element_text(hjust = 0.5),
          axis.line = element_line(size = 1, color = "black"))
}

# Plot Phillips curves using three different expected inflation rate scenarios for the specified period
plot.infl.exp.nrou <- function(data = pc_nrou, b1 = 0.75, b2 = 1.5, b3 = 2.25, period_start = 1949, period_end = 2017, method = "lm") {

  # Calculate expected inflation scenarios
  pc_infl_exp <- expected.inflation.range(data = data, b1, b2, b3)

  # Reshape data to enable multiple scenarios to appear together on the y axis
  infl_exp_melt <- melt(pc_infl_exp, id = c("date", "inflation", "u3", "nrou"))

  # Filter selected period
  infl_exp_select_period <- infl_exp_melt %>%
    filter(year(infl_exp_melt$date) >= period_start & year(infl_exp_melt$date) <=  period_end)

  ggplot(data = infl_exp_select_period) +
    aes(x = nrou,
        y = value,
        color = variable,
        group = variable) +
    geom_point() +
    geom_smooth(method = method,
                formula = y ~ x + I(x^2),
                se = FALSE) +
    scale_color_manual(name = "Beta Scenarios",
                       labels = c(paste("B1 =",b1), paste("B2 =",b2), paste("B3 =",b3)),
                       values = c(custom_palette[1], custom_palette[2], custom_palette[3])) +
    labs(title = "Modified Phillips Curve: Three Scenarios",
         subtitle = paste("Expected Inflation and Natural Rate of Unemployment from", period_start, "to", period_end),
         x = "Modeled Natural Rate of Unemployment",
         y = "Inflation",
         caption = "Sources: FI Consulting, Federal Reserve Bank of St. Louis, Federal Reserve Bank of Cleveland") +
    scale_x_continuous(labels = function(x) format(paste0(x, "%"))) +
    scale_y_continuous(labels = function(x) format(paste0(x, "%"))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # Centers title
          plot.subtitle = element_text(hjust = 0.5),
          axis.line = element_line(size = 1, color = "#777777"))
}

# Plot the Phillips curve using the natural unemployment rate over time for the specified period
plot.pc.nrou <- function(data = pc_nrou, period_start = 1949, period_end = 2017) {

  # Filter selected period
  pc_nrou_select_period <- data %>% filter(year(data$date) >= period_start & year(data$date) <=  period_end)

  ggplot(data = pc_nrou_select_period) +
    aes(x = nrou, y = inflation) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x + I(x^2),
                se = TRUE) +
    scale_color_manual(values = c(custom_palette[1], custom_palette[2])) +
    labs(title = "Modified Phillips Curve",
         subtitle = paste("Expected Inflation and Natural Rate of Unemployment from", period_start, "to", period_end),
         x = "Natural Rate of Unemployment",
         y = "Inflation",
         caption = "Sources: FI Consulting, Federal Reserve Bank of St. Louis") +
    scale_x_continuous(labels = function(x) format(paste0(x, "%"))) +
    scale_y_continuous(labels = function(x) format(paste0(x, "%"))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # Centers title
          plot.subtitle = element_text(hjust = 0.5),
          axis.line = element_line(size = 1, color = "black"))
}

# Plot the natural unemployment rate over time for the specified period
plot.nrou <- function(data = pc_nrou_clean, period_start = 1949, period_end = 2017) {

  # Filter selected period
  nrou_select_period <- data %>% filter(year(data$date) >= period_start & year(data$date) <= period_end)

  ggplot(data = nrou_select_period) +
    aes(x = date, y = nrou) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x + I(x^2),
                se = TRUE) +
    scale_color_manual(values = c(custom_palette[1], custom_palette[2])) +
    labs(title = "Natural Rate of Unemployment Over Time",
         subtitle = paste("From", period_start, "to", period_end),
         x = "Year",
         y = "Natural Rate of Unemployment",
         caption = "Sources: FI Consulting, Federal Reserve Bank of St. Louis") +
    scale_y_continuous(labels = function(x) format(paste0(x, "%"))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # Centers title
          plot.subtitle = element_text(hjust = 0.5),
          axis.line = element_line(size = 1, color = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5)) # Rotates and centers x labels
}

# Plot the Phillips curve using the U6 unemployment rate over time for the specified period
plot.pc.u6 <- function(data = pc_u6, period_start = 2009, period_end = 2017) {

  # Filter selected period
  pc_u6_select_period <- data %>% filter(year(data$date) >= period_start & year(data$date) <= period_end)

  ggplot(data = pc_u6_select_period) +
    aes(x = u6, y = inflation) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x + I(x^2),
                se = TRUE) +
    scale_color_manual(values = c(custom_palette[1], custom_palette[2])) +
    labs(title = "Phillips Curve Using U6 Unemployment Rate",
         subtitle = paste("From", period_start, "to", period_end),
         x = "U6 Unemployment Rate",
         y = "Inflation",
         caption = "Sources: FI Consulting, Federal Reserve Bank of St. Louis, BLS") +
    scale_y_continuous(labels = function(x) format(paste0(x, "%"))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # Centers title
          plot.subtitle = element_text(hjust = 0.5),
          axis.line = element_line(size = 1, color = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5)) # Rotates and centers x labels
}

# Plot the U6 unemployment rate over time for the specified period
plot.u6 <- function(data = pc_u6, period_start = 2009, period_end = 2017) {

  # Filter selected period
  u6_select_period <- data %>% filter(year(data$date) >= period_start & year(data$date) <= period_end)

  ggplot(data = u6_select_period) +
    aes(x = date, y = u6) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x + I(x^2),
                se = TRUE) +
    scale_color_manual(values = c(custom_palette[1], custom_palette[2])) +
    labs(title = "U6 Unemployment Rate Over Time",
         subtitle = paste("From", period_start, "to", period_end),
         x = "Year",
         y = "U6 Unemployment Rate",
         caption = "Sources: FI Consulting, Federal Reserve Bank of St. Louis, BLS") +
    scale_y_continuous(labels = function(x) format(paste0(x, "%"))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # Centers title
          plot.subtitle = element_text(hjust = 0.5),
          axis.line = element_line(size = 1, color = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5)) # Rotates and centers x labels
}

# Plot the U3 and U6 unemployment rates over time for the specified period
plot.unemployment <- function(data = pc_u6, period_start = 2009, period_end = 2017) {

  # Filter selected period
  unemp_select_period <- data %>% filter(year(data$date) >= period_start & year(data$date) <= period_end)

  ggplot(data = unemp_select_period) +
    aes(x = u3, y = u6) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x + I(x^2),
                se = TRUE) +
    scale_color_manual(values = c(custom_palette[1], custom_palette[2])) +
    labs(title = "Comparison of U3 and U6 Unemployment Rates Over Time",
         subtitle = paste("From", period_start, "to", period_end),
         x = "Year",
         y = "Unemployment Rates",
         caption = "Sources: FI Consulting, Federal Reserve Bank of St. Louis, BLS") +
    scale_y_continuous(labels = function(x) format(paste0(x, "%"))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # Centers title
          plot.subtitle = element_text(hjust = 0.5),
          axis.line = element_line(size = 1, color = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5)) # Rotates and centers x labels
}
