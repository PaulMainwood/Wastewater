library(tidyverse)
library(lubridate)
library(zoo)
library(bsts)

#Some useful tables/lookups
incidence_death_lookups <- list( '05-09' = c('2-11'), 
                   '10-14' = c('2-11', '12-16'),
                   '15-19' = c('12-16', '17-24'),
                   '20-24' = c('17-24'),
                   '25-29' = c('25-34'),
                   '30-34' = c('25-34'),
                   '35-39' = c('35-49'),
                   '40-44' = c('35-49'),
                   '45-49' = c('35-49'),
                   '50-54' = c('50-69'),
                   '55-59' = c('50-69'),
                   '60-64' = c('50-69'),
                   '65-69' = c('50-69'),
                   '70-74' = c('70+'),
                   '75-79' = c('70+'),
                   '80-84' = c('70+'),
                   '85-89' = c('70+'),
                   '90+' = c('70+'),
                   'All ages' = c('All'))

population_lookups <- list('05-09' = 3524600, 
                                '10-14' = 3595900,
                                '15-19' = 3394700,
                                '20-24' = 3602100,
                                '25-29' = 3901800,
                                '30-34' = 4148800,
                                '35-39' = 3981600,
                                '40-44' = 3755700,
                                '45-49' = 3788700,
                                '50-54' = 4123400,
                                '55-59' = 4029000,
                                '60-64' = 3455700,
                                '65-69' = 2945100,
                                '70-74' = 2978000,
                                '75-79' = 2170300,
                                '80-84' = 1517000,
                                '85-89' = 925100,
                                '90+' = 527900,
                                'All ages' = 59597300)

optimum_lags <- list('05-09' = 17, 
                         '10-14' = 17,
                         '15-19' = 37,
                         '20-24' = 45,
                         '25-29' = 35,
                         '30-34' = 37,
                         '35-39' = 32,
                         '40-44' = 33,
                         '45-49' = 34,
                         '50-54' = 34,
                         '55-59' = 33,
                         '60-64' = 33,
                         '65-69' = 28,
                         '70-74' = 29,
                         '75-79' = 29,
                         '80-84' = 27,
                         '85-89' = 27,
                         '90+' = 32,
                         'All ages' = 34)


# Function to calculate the average based on death_age
get_average <- function(death_age, ons_data, lookup_list) {
  # Identify columns based on death_age
  columns <- lookup_list[[death_age]]
  # Subset ons_data
  relevant_data <- ons_data[, columns, drop = FALSE]
  # Calculate row-wise average
  avg_values <- rowMeans(relevant_data)
  return(avg_values)
}

#Script starts here: select first date

start_week <- as.Date("2020-06-19")
age_list <- names(incidence_death_lookups)
start_num <- 2
dyn_coeff_frame = data.frame()

#Loop around all the ages
for (i in start_num:length(age_list)){

death_age <- age_list[i]
print(death_age)
offset_val <- optimum_lags[[death_age]]
print(offset_val)

average_incidence <- get_average(death_age, ons_incidence, incidence_death_lookups)
number_of_infections <- population_lookups[[death_age]] * average_incidence 
pre_offset_selected_infections <- tibble(Date = ons_incidence$Date, Incidence = number_of_infections)
offset_selected_infections <- pre_offset_selected_infections %>% 
  mutate(Incidence_lag = lag(Incidence, n = offset_val))
  


ons_weekly_incidence <- offset_selected_infections %>%
  filter(Date >= start_week) %>%
  mutate(week_offset = as.numeric(difftime(Date, start_week, units = "days")) %/% 7) %>%
  group_by(week_start = start_week + (week_offset * 7)) %>%
  summarize(value = sum(Incidence_lag, na.rm = TRUE)) %>%
  select(Date = week_start, Incidence = value)


# Joining the tibbles based on the date columns and selecting only the relevant columns
result <- ons_weekly_incidence %>%
  inner_join(ons_deaths, by = c("Date" = "Week ending")) %>%
  select(Date, Incidence_lag = Incidence, Deaths = death_age) %>%
  #mutate(Incidence_lag = lag(Incidence_raw, n = offset_val)) %>%
  drop_na()

#  Plot
ggplot(result, aes(x = Date)) +
  geom_line(aes(y = Incidence_lag, color = "Incidence")) +
  geom_line(aes(y = Deaths , color = "Deaths")) + 
  scale_y_continuous(name = "Incidence", labs(title = "Incidence and Deaths over Time", color = "Metric")) +
  theme_minimal()

iter <- 1000
model_frame <- drop_na(result)
ss <- list()
ss <- AddDynamicRegression(ss, model_frame$Deaths ~ model_frame$Incidence_lag)
model <- bsts(Deaths ~ Incidence_lag, state.specification = ss, data = model_frame, niter = iter)
plot(model, "dynamic", burn = 200)

column_name <- paste0("dyn_coeffs_", death_age)

#Only if this is the first loop - add Date column
if (i == start_num){
  dyn_coeff_frame <- data.frame(Date = model_frame$Date)
}

dyn_coeff_frame <- dyn_coeff_frame %>%
  mutate(!!column_name := apply(model$dynamic.regression.coefficients, c(3), mean))

# #Plot
# ggplot(data = model_frame, aes(x = Date)) +
#   geom_line(aes(y = dyn_coefs), color = "red", size = 1) +
#   theme_minimal()
}

# Reshape the dataframe from wide format to long format
long_data <- dyn_coeff_frame %>%
  gather(key = "Variable", value = "Value", -Date) %>%
  mutate(log_Value = log(Value))

# Plot using ggplot2
ggplot(data = long_data, aes(x = Date, y = Value, color = Variable)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Dynamic Coefficients over Time",
       y = "Coefficient Value",
       x = "Date") +
  theme(legend.title = element_blank())


# Plot using ggplot2 - log scale
ggplot(data = long_data, aes(x = Date, y = log_Value, color = Variable)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Dynamic Coefficients over Time",
       y = "Coefficient Value",
       x = "Date") +
  theme(legend.title = element_blank())


# Plot using ggplot2 with facet_wrap - same scales
ggplot(data = long_data, aes(x = Date, y = Value)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Dynamic Coefficients over Time",
       y = "Coefficient Value",
       x = "Date") +
  theme(legend.title = element_blank()) +
  facet_wrap(~ Variable, ncol = 3)

sig_fig_percent <- function(x) {
  paste0(formatC(x * 100, format = "g", digits = 2), "%")
}


# Plot using ggplot2 with facet_wrap
ggplot(data = long_data, aes(x = Date, y = Value, group = Variable, color = Variable)) +
  geom_line(size = 1.5, show.legend = FALSE) +  # Set size for thicker line, and no legend for the lines
  scale_color_viridis_d(end = 0.85, direction = -1, option = "D", guide = "none") +  # Set colors
  theme_minimal() +
  labs(title = "Dynamic Coefficients - IFR estimates - over Time",
       y = "Coefficient Value (%)",
       x = "Date") +
  theme(legend.title = element_blank()) +
  scale_x_date(breaks = "1 year", date_labels = "%Y", minor_breaks = "1 month") +
  scale_y_log10(labels = sig_fig_percent) +
  facet_wrap(~ Variable, ncol = 4, scales = "free_y")

