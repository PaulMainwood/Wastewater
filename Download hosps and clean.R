# Load the tidyverse package
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readxl)
library(httr)
library(zoo)
library(scales)
library(bsts)

# Specify the URL of the CSV file
# Have to go to https://scotland.shinyapps.io/phs-respiratory-covid-19/ on a browser and configure, then right click to get download link and paste below.
url <- "https://scotland.shinyapps.io/phs-respiratory-covid-19/_w_6c559566/session/38303edbd23f4a53c915b5730c3b9f61/download/data_download_output?w=6c559566"
# Read the CSV file from the URL into a tibble
raw_data <- read_csv(url)
hosps <- raw_data

hosps$Date <- ymd(hosps$Date)

# Plot the data
ggplot(hosps, aes(x = Date, y = SevenDayAverage)) +
  geom_line(colour = "orange") +  # Add a line layer
  theme_minimal() +  # Optional: a minimal theme for a nicer look
  labs(
    x = "Date", 
    y = "Hospitalisations", 
    title = "Hospital occupancy over time, Scotland"
  ) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y")  # Format the date axis


# Find the minimum and maximum dates
min_date <- min(hosps$Date, na.rm = TRUE)
max_date <- max(hosps$Date, na.rm = TRUE)

# Generate the sequence of dates
date_sequence <- seq.Date(from = min_date, to = max_date, by = "day")

# Create a new tibble with the sequence of dates
date_tibble <- tibble(Date = date_sequence)

combined_data <- date_tibble %>%
  full_join(select(hosps, Date, SevenDayAverage), by = "Date") %>%
  full_join(wastewater, by = "Date") %>%
  arrange(Date) 

names(combined_data) <- c('Date', 'Hosps', 'Wastewater')
# Replace NA values with linearly interpolated values based on existing 'Date' and 'Value'
combined_data$Wastewaterip <- na.approx(combined_data$Wastewater, combined_data$Date, na.rm = FALSE)

combined_data$Hosps7 <- rollapply(combined_data$Hosps, width = 7, FUN = mean, na.rm = TRUE, align = 'right', fill = NA)


combined_data <- combined_data %>%
  select(Date, Wastewaterip, Hosps7) %>%
  # Exclude rows with NAs
  filter(!if_any(c(Date, Wastewaterip, Hosps7), ~is.na(.x) | is.nan(.x)))

#Now plot them together
# Calculate a factor for scaling
max_wastewater <- max(combined_data$Wastewaterip, na.rm = TRUE)
max_ons_infections <- max(combined_data$Hosps7, na.rm = TRUE)
factor <- max_wastewater / max_ons_infections


# Create the base plot with the adjusted axes
p <- ggplot(combined_data, aes(x = Date)) +
  geom_line(aes(y = Hosps7, color = "Hospitalisation occupancy")) +
  geom_line(aes(y = Wastewaterip / factor, color = "Wastewater Levels")) +
  scale_color_manual(values = c("Hospitalisation occupancy" = "orange", "Wastewater Levels" = "red")) +
  scale_y_continuous(
    name = "Hospital bed occupancy (orange)",
    labels = comma, # Use the scales package to format labels without scientific notation
    sec.axis = sec_axis(~ . * factor, name = "Wastewater viral RNA, Mgc/p/d (red)", labels = comma)
  ) +
  theme_minimal() +
  labs(
    x = "Date",
    title = "Hospitalisations and Wastewater Levels - Scotland",
    color = NULL
  ) + theme(legend.position = "none") # Hide the legend

# Print the plot with the adjusted axes
print(p)



