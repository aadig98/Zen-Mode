# Assignment 7 Case study 2

install.packages("readxl")
library(readxl)
customer_satisfaction_data <- read_excel("customer_satisfaction_data.xlsx")
View(customer_satisfaction_data)

#Identify the total sample size of the dataset.
# Counting the total number of rows to find the sample size.
# Specify the path to your Excel file
excel_file_path <- "customer_satisfaction_data.xlsx"

# Read the Excel file into a data frame
survey_data <- readxl::read_excel("customer_satisfaction_data.xlsx")
View(customer_satisfaction_data)

# Count the total sample size
total_sample_size <- nrow(survey_data)

# Print the total sample size
print(total_sample_size)

#Sample proportion of the satisfied customers.
# We need to find the total numbers of satisfied customers.

# Count the number of satisfied customers (where 1 represents satisfied)
total_satisfied_customers <- sum(customer_satisfaction_data$Satisfaction)
print(total_satisfied_customers)

# Calculations for test statistic 
satisfied_customers <- 775
total_sample_size <- 1000
historical_satisfaction_rate <- 0.75  # Historical average satisfaction rate

# Calculating the z_stat from the formulas given by getting the success proportion from the data and the expected proportion
p1 <- 775/1000
p0 <- 0.75
z_value <- (p1 - p0)/(sqrt((p0*(1-p0))/1000))
print(z_value)

# Finding the p-value to determine the probability of obtaining the required mean
p_value <- 2*(1 - pnorm(abs(z_value)))
print(p_value)

