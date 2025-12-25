
# Project-----------------------------------------------------------
# Exploratory Data Analysis - NYC Subway Performance
# Author: Subhechha Khatri
# Tools: Tidyverse, Dplyr, Ggplot2



# 1) Load Libraries and Data ----------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)


subway_data <- read.csv("MTA_Subway_Customer_Journey-Focused_Metrics__2020-2024.csv", header=T, sep=",")



# 2) Data Inspection  -----------------------------------------------------

# Checking the structure (Dimensions, Column Types)
str(subway_data)

# Viewing the first six rows to understand the data format
head(subway_data)

# Checking for missing values (NA) in the entire dataset
sum(is.na(subway_data))

# Check specifically for NAs in our target column: performance
sum(is.na(subway_data$customer.journey.time.performance))

# Getting a quick statistical summary of the columns to identify outliers
summary(subway_data)



# 3) Data Cleaning and Wrangling ------------------------------------------


# CLEANING: Converting the 'month' column into a date format 
subway_data$month <- as.Date(subway_data$month)

# WRANGLING: Creating a 'Total_Delay' column (Platform time + Train time)
subway_data <- subway_data %>%
  mutate(total_delay = additional.platform.time + additional.train.time)

# FILTERING: Focusing on the year 2023 for a specific analysis
data_2023 <- subway_data %>% 
  filter(month >= "2023-01-01" & month <= "2023-12-31")



# 4) Descriptive Analytics ------------------------------------------------

# Calculating average performance and delays by division
summary_stats <- data_2023 %>%
  group_by(division) %>%
  summarise(
    Avg_Performance = mean(customer.journey.time.performance),
    Avg_Delay = mean(total_delay),
    Total_Passengers = sum(num_passengers)
  )

print(summary_stats)




# 5) Statistical Inference ---------------------------------------------------

# Question: Is there a significant difference in performance between Peak and Off-Peak at 95% Confidence?
# H0: mu1 = mu2 (Means are the same)
# H1: mu1 != mu2 (Means are different)
t.test(customer.journey.time.performance ~ period, data = data_2023, conf.level = 0.95)

# INTERPRETATION: Since p-value is less than 0.05, we reject H0. 
# Time of day (Peak vs Off-Peak) significantly impacts subway performance.


# 6) Correlation and Regression -------------------------------------------

# Determining the correlation between total_delay and performance
cor(data_2023$total_delay, data_2023$customer.journey.time.performance)
# INTERPRETATION: Strong negative correlation found. As delays increase, performance decreases.


# Creating a simple linear model to predict performance based on total_delay
performance_model <- lm(customer.journey.time.performance ~ total_delay, data = data_2023)
summary(performance_model)

# INTERPRETATION: The model shows total_delay is a significant predictor of performance score.
#The Linear Model: 
#performance= 0.967381 - 0.068734 * total_delay



# 7) Visualization --------------------------------------------------------

# Creating a scatterplot with a regression line
# Following the style used in the Final Exam
ggplot(data_2023, aes(x = total_delay, y = customer.journey.time.performance)) +
  geom_point(color = "skyblue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "pink") +
  labs(title = "Relationship Between Delays and Journey Performance (2023)",
       x = "Total Delay (Minutes)",
       y = "Performance Score (%)") +
  theme_minimal()

# Saving the plot for the GitHub repository
ggsave("subway_performance_analysis.png")

