# Assignment 7 by Aaditya Gupta
#Case study 1 -> Objective: To analyze a dataset of participant weights and perform 
                      #  statistical analysis to draw conclusions about the population.
# Reading dataset
library(readxl)
library(BSDA)
library(ggplot2)

weights_data <- read_excel("weights_data.xlsx")
View(weights_data)


# Finding mean,standard deviation
# mean_dataset <- mean(weights_data) This code did not work because it was not in numeric.
mean <- mean(unlist(weights_data), na.rm = TRUE)
print(mean)

SD <- sd(weights_data$Weights, na.rm = TRUE)
print(SD)

median <- median(weights_data$Weights, na.rm = TRUE)

# Create a table of frequencies
 frequency_table <- table(weights_data$Weights)
 
# Convert the table to a data frame
    mode_table <- as.data.frame(frequency_table)
 
# Rename the columns for clarity
   colnames(mode_table) <- c("Weight", "Frequency")

 # Print the mode table
  print(mode_table)

#Assuming significance level = 0.05 or 5%
Significance_level = 0.05 

# Calculating Z value
Z_value <- z.test(weights_data$Weights, mu = 75, sigma.x = 1.496117)
print(Z_value)

# Generate values for the x-axis
x_values <- seq(-5, 5, by = 0.1)

z_score <- Z_value$statistic 
 print(z_score)

 p_value_two_tailed <- 2 * pnorm(z_score)
 print(p_value_two_tailed)

# Create a data frame for plotting
plot_data <- data.frame(x = x_values, y = dnorm(x_values))

# Create the plot
ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  geom_text(x = z_score, y = 0.02, label = paste("p =", round(p_value_two_tailed, 7)),
            vjust = -0.5, hjust = 0.5, size = 4, color = "blue")+
  geom_vline(xintercept = 1.96, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = -1.96, linetype = "dashed", color = "blue")+
  geom_text(aes(x = -1.96, y = 0.25, label = "Z = -1.96"), hjust = 1.2, color = "blue")+
  geom_text(aes(x = 1.96, y = 0.25, label = "Z = 1.96"), hjust = -0.2, color = "blue")+
  geom_ribbon(aes(ymax = ifelse(x < -1.96, y, 0), ymin = 0),
              fill = "red", alpha = 0.3) +
  geom_ribbon(aes(ymax = ifelse(x > 1.96, y, 0), ymin = 0),
              fill = "red", alpha = 0.3) +
  geom_vline(xintercept = z_score, linetype = "dashed", color = "green")+
  geom_text(aes(x = z_score, y = 0.15, label = "Z_Stat"), hjust = -0.2, color = "orange")+
  labs(title = "Standard Normal Distribution with Two-Tailed p-Value",
       x = "Z-Score", y = "Density") +
  theme_minimal()

# Set the confidence level
confidence_level <- 0.95

# Calculate the margin of error
margin_of_error <- qnorm((1 + confidence_level) / 2) * (SD / sqrt(length(weights_data$Weights)))

# Create a histogram
hist(weights_data$Weights, 
     main = "Histogram of Weights with Confidence Interval",
     xlab = "Weight",
     ylab = "Frequency",
     col = "skyblue",
     border = "black",
     xlim = c(min(weights_data$Weights) - 1, max(weights_data$Weights) + 1),
     breaks = seq(min(weights_data$Weights) - 0.5, max(weights_data$Weights) + 0.5, by = 1))

# Add vertical lines for the confidence interval
abline(v = mean - margin_of_error, col = "red", lty = 2)
abline(v = mean + margin_of_error, col = "red", lty = 2)

# Add labels for mean and confidence interval bounds
text(mean - margin_of_error, 0, round(mean - margin_of_error, 2), col = "red", pos = 3)
text(mean + margin_of_error, 0, round(mean + margin_of_error, 2), col = "red", pos = 2)

#END

