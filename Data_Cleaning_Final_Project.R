#data cleaning for final project

library(tidyverse)
library(readxl)
library(readr)
library(ggplot2)


EV_1 <- read_excel("EV battery pack capacity per vehicle in the US market 2022.xlsx")
View(EV_1)


EV_2 <- read_csv("EV Range _Value_ - General.csv")
View(EV_2)

EV_3 <- read_csv("EV Release Database - EV List.csv")


file_path <- "EV Release Database - EV List.csv"
start_line <- 10
EV_3 <- read_csv(file_path, skip = start_line - 1)
view(EV_3)

names(EV_3)


EV_3$`Release Date` <- as.Date(EV_3$`Release Date`, format = "%m/%d/%Y")
EV_3$`Batt kWh` <- as.numeric(EV_3$`Batt kWh`)

EV_3$`Release Date` <- as.Date(EV_3$`Release Date`, format = "%Y-%m-%d")
EV_3$`Release Date` <- format(EV_3$`Release Date`, "%m/%d/%Y")

EV_3$`Comb. Range` <- as.numeric(EV_3$`Comb. Range`)
EV_3$`EVR (mi)` <- as.numeric(EV_3$`EVR (mi)`)


EV_3 <- EV_3[EV_3$'EV Type' != "PHEV", , drop = FALSE]

EV_3 <- EV_3[EV_3$'EV Type' != "FCV", , drop = FALSE]


view(EV_3)

str(EV_3)

names(EV_3)








ggplot(EV_3, aes(x = as.Date(`Release Date`, format="%m/%d/%Y"), y = `Batt kWh`)) +
  geom_point() +
  labs(title = "Release Date vs. Batt kWh",
       x = "Release Date",
       y = "Batt kWh")
#battery kwh with release date in years
ggplot(EV_3, aes(x = as.Date(`Release Date`, format="%m/%d/%Y"), y = `Batt kWh`)) +
  geom_point(color = "#4CAF50", size = 3, alpha = 0.7) +
  labs(title = "Release Date vs. Batt kWh",
       x = "Release Date",
       y = "Batt kWh") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 year")

#combine range and release date in year 

ggplot(EV_3, aes(x = as.Date(`Release Date`, format="%m/%d/%Y"), y = `Comb. Range`)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  labs(title = "Release Date vs. Comb. Range",
       x = "Release Date",
       y = "Comb. Range") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 year")

#this is batt kwh and EVR(mi)

ggplot(EV_3, aes(x = `Batt kWh`, y = `EVR (mi)`)) +
  geom_point() +
  labs(title = "Comparison of Batt kWh and EVR(mi)",
       x = "Batt kWh",
       y = "EVR(mi)") +
  theme_minimal()

#this is batt kwh and EVR(mi) with linear and slope

ggplot(EV_3, aes(x = `Batt kWh`, y = `EVR (mi)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(title = "Comparison of Batt kWh and EVR(mi)",
       x = "Batt kWh",
       y = "EVR(mi)") +
  theme_minimal()

# Assuming EV_3 is your data frame
model <- lm(`EVR (mi)` ~ `Batt kWh`, data = EV_3)

# Print the model summary
summary(model)

ggplot(EV_3, aes(x = `Batt kWh`, y = `EVR (mi)`, color = factor(`Model Year`))) +
  geom_point() + geom_smooth(method='lm') +
  labs(title = "Comparison of Batt kWh and EVR(mi) Every 3 Years",
       x = "Batt kWh",
       y = "EVR(mi)",
       color = "Release Date") +
  theme_minimal()

#model that compares batt kwh evr mi and last 3 years

# Convert 'Release Date' to Date format if not already done
EV_3$`Release Date` <- as.Date(EV_3$`Release Date`, format = "%m/%d/%Y")

# # Subset data for the year 2023
# year_2023_data <- EV_3[year(EV_3$`Release Date`) == 2022, ]

# Fit linear regression model
model <- lm(`EVR (mi)` ~ `Batt kWh` + `Release Date`, data = EV_3)



# Print the model summary
summary(model)
