# Load the tidyverse package
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readxl)
library(httr)
library(zoo)
library(scales)
library(bsts)

# Specify the URL of the CSV file:
# Have to go to https://scotland.shinyapps.io/phs-respiratory-covid-19/ on a browser and configure, then right click to get download link and paste below.
url <- "https://scotland.shinyapps.io/phs-respiratory-covid-19/_w_068756e9/session/990eeb4822369c9581913869bdd74abe/download/data_download_output?w=068756e9"
# Read the CSV file from the URL into a tibble
raw_data <- read_csv(url)
wastewater <- raw_data

wastewater$Date <- ymd(wastewater$Date)

# Plot the data
ggplot(wastewater, aes(x = Date, y = WastewaterSevenDayAverageMgc)) +
  geom_line() +  # Add a line layer
  theme_minimal() +  # Optional: a minimal theme for a nicer look
  labs(
    x = "Date", 
    y = "Wastewater Seven Day Average (Mgc)", 
    title = "Wastewater Analysis Over Time, Scotland"
  ) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y")  # Format the date axis

#Now ONS data
url2 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/covid19infectionsurveyscotland/2023/20230310covid19infectionsurveydatasetsscotland.xlsx"
# Specify the path to save the temporary Excel file
temp_file <- tempfile(fileext = ".xlsx")
# Download the file to the temp path
GET(url2, write_disk(temp_file, overwrite = TRUE))

#Sheet name and ranges for Scotland data
sheet_name <- "1a"
cell_range <- "A5:G129" 

ons_prevalence_raw <- read_excel(temp_file, range = cell_range, sheet = sheet_name)

# Don't forget to remove the temporary file after you're done
unlink(temp_file)

ons_prevalence <- ons_prevalence_raw %>% 
  rename(Weeks = 'Time period') %>%
  mutate(date_parts = str_split(Weeks, " to "),
  first_date = map_chr(date_parts, 1) %>% dmy(),
  last_date = map_chr(date_parts, 2) %>% dmy(),
  mean_date = as.Date((as.numeric(first_date) + as.numeric(last_date)) / 2, origin = "1970-01-01")
) %>% rename(Date = mean_date)

#Now generate combined tibble

# Find the minimum and maximum dates
min_date <- min(wastewater$Date, na.rm = TRUE)
max_date <- max(wastewater$Date, na.rm = TRUE)

# Generate the sequence of dates
date_sequence <- seq.Date(from = min_date, to = max_date, by = "day")

# Create a new tibble with the sequence of dates
date_tibble <- tibble(Date = date_sequence)

combined_data <- date_tibble %>%
  full_join(wastewater, by = "Date") %>%
  full_join(select(ons_prevalence, Date, c(names(ons_prevalence)[5], names(ons_prevalence)[6], names(ons_prevalence)[7])), by = "Date") %>%
  arrange(Date) 

names(combined_data) <- c('Date', 'Wastewater7days', 'ONSinfections', 'lc', 'uc')
# Replace NA values with linearly interpolated values based on existing 'Date' and 'Value'
combined_data$ONSinfectionsip <- na.approx(combined_data$ONSinfections, combined_data$Date, na.rm = FALSE)
combined_data$Wastewater7daysip <- na.approx(combined_data$Wastewater7days, combined_data$Date, na.rm = FALSE)
combined_data$uc <- na.approx(combined_data$uc, combined_data$Date, na.rm = FALSE)
combined_data$lc <- na.approx(combined_data$lc, combined_data$Date, na.rm = FALSE)


combined_data$ONSinfections7daysip <- rollapply(combined_data$ONSinfectionsip, width = 7, FUN = mean, na.rm = TRUE, align = 'right', fill = NA)
combined_data$uc <- rollapply(combined_data$uc, width = 7, FUN = mean, na.rm = TRUE, align = 'right', fill = NA)
combined_data$lc <- rollapply(combined_data$lc, width = 7, FUN = mean, na.rm = TRUE, align = 'right', fill = NA)


combined_data <- combined_data %>%
  select(Date, Wastewater7daysip, ONSinfections7daysip, uc, lc) %>%
  # Exclude rows with NAs
  filter(!if_any(c(Date, Wastewater7daysip, ONSinfections7daysip), ~is.na(.x) | is.nan(.x)))

#And plot the result
ggplot(combined_data, aes(x = Date)) +
  geom_ribbon(aes(ymin = lc, ymax = uc), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = ONSinfections7daysip), color = "blue") +
  theme_minimal() +
  labs(x = "Date", y = "Value", title = "Ribbon Plot with Central, Upper, and Lower Range Values")

#Now plot them together
# Calculate a factor for scaling
max_wastewater <- max(combined_data$Wastewater7daysip, na.rm = TRUE)
max_ons_infections <- max(combined_data$ONSinfections7daysip, na.rm = TRUE)
factor <- max_wastewater / max_ons_infections

  
  # Create the base plot with the adjusted axes
  p <- ggplot(combined_data, aes(x = Date)) +
    geom_ribbon(aes(ymin = lc, ymax = uc), fill = "blue", alpha = 0.2) +
    geom_line(aes(y = ONSinfections7daysip, color = "ONS Infections")) +
    geom_line(aes(y = Wastewater7daysip / factor, color = "Wastewater Levels")) +
    scale_color_manual(values = c("ONS Infections" = "blue", "Wastewater Levels" = "red")) +
    scale_y_continuous(
      name = "ONS prevalence 7-day average (blue)",
      labels = comma, # Use the scales package to format labels without scientific notation
      sec.axis = sec_axis(~ . * factor, name = "Wastewater viral RNA, Mgc/p/d (red)", labels = comma)
    ) +
    theme_minimal() +
    labs(
      x = "Date",
      title = "ONS Infections and Wastewater Levels - Scotland",
      color = NULL
    ) + theme(legend.position = "none") # Hide the legend
  
  # Print the plot with the adjusted axes
  print(p)
  
  
  #model
  
  iter <- 1000
  model_frame <- combined_data
  model_frame <- model_frame %>% mutate(Wastewater_lag = lag(Wastewater7daysip, n = 7))
  model_frame <- drop_na(model_frame)
  ss <- list()
  ss <- AddDynamicRegression(ss, model_frame$Wastewater_lag ~ model_frame$ONSinfections7daysip)
  model <- bsts(Wastewater_lag ~ ONSinfections7daysip, state.specification = ss, data = model_frame, niter = iter)
  plot(model, "dynamic", burn = 200)
  
  
  model_frame$coeffs <- apply(model$dynamic.regression.coefficients, c(3), mean) * 1000000
  #model_frame$coeffs <- rollapply(model_frame$coeffs, width = 21, FUN = mean, na.rm = TRUE, align = 'right', fill = NA)
  
  model_frame
  q <- ggplot(model_frame, aes(x = Date, y = coeffs)) +
    geom_line(colour = "red") +
    theme_minimal() + 
    #scale_x_date(limits = c(min(model_frame$Date), as.Date("2024-01-01"))) +
    labs(x = "Date", y = "gc/p/d", title = "Coefficient estimating Scotland wastewater output per infection")
  
  # Print the plot
  print(q)  
  