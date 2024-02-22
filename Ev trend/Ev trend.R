library(tidyverse)
library(readxl)
Vehicle_dataset_ <- read_excel("~/Library/CloudStorage/OneDrive-NortheasternUniversity/Project R/Group 4/Vehicle_dataset .xlsx")
View(Vehicle_dataset_)
Path <- "~/Library/CloudStorage/OneDrive-NortheasternUniversity/Project R/Group 4/Vehicle_dataset .xlsx"
sheet_names <- excel_sheets(Path)
total_data <- read_excel(Path, sheet = sheet_names[8])
total_data <- as.data.frame(total_data)
colnames(total_data) <- c("Year", "EV sales", "Alternative Vehicle sales", "Conventional Vehicle sales","EV+Alt", "Electric Sales Difference", "Alternative Sales Difference", "Conventional Sales Difference")

#Total data 2016-2022
total_data$`EV sales` <- as.numeric(gsub(",","",total_data$`EV sales`))
total_data$`Alternative Vehicle sales` <- as.numeric(gsub(",","",total_data$`Alternative Vehicle sales`))
total_data$`Conventional Vehicle sales` <- as.numeric(gsub(",","",total_data$`Conventional Vehicle sales`))
sample_mean <- mean(total_data$`Electric Sales Difference`)
print(sample_mean)

#Data from 2016-2019 Pre-covid
population_data <- total_data[total_data$Year<2020,]
population_data$`EV sales` <- as.numeric(gsub(",","",population_data$`EV sales`))
population_data$`Alternative Vehicle sales` <- as.numeric(gsub(",","",population_data$`Alternative Vehicle sales`))
population_data$`Conventional Vehicle sales` <- as.numeric(gsub(",","",population_data$`Conventional Vehicle sales`))

#Data from 2020-2022 Post-covid
sample_data <- total_data[total_data$Year>2019,]
sample_data$`EV sales` <- as.numeric((gsub(",","", sample_data$`EV sales`)))
sample_data$`Alternative Vehicle sales` <- as.numeric((gsub(",","", sample_data$`Alternative Vehicle sales`)))
sample_data$`Conventional Vehicle sales` <- as.numeric((gsub(",","", sample_data$`Conventional Vehicle sales`)))

#Mean of Electric sales and Conventional sales
sample_mean <- mean(total_data$`Electric Sales Difference`)
print(sample_mean)
population_mean <- mean(total_data$`Conventional Sales Difference`)
print(population_mean)

#To check if there is significance difference between mean of EV Sales and mean of conventional vehicle sales
t.test(total_data$`Electric Sales Difference`, mu = population_mean, alternative = "two.sided", conf.level = 0.95)

# Conducting the test for a samples of sales between 2016 and 2019, the other sample being sales between 2020 and 2022
pre_data <- total_data[total_data$Year < 2020, ]
post_data <- total_data[total_data$Year >2019, ]

# Checking if the EV sales mean is significantly different from the conventional sales between 2016 and 2019
t.test(pre_data$`Electric Sales Difference`, pre_data$`Conventional Sales Difference`, 
       mu = mean(pre_data$`Conventional Sales Difference`), alternative = "two.sided",
       conf.level = 0.95)

# Checking if the EV sales mean is significantly different from conventional sales between 2020 and 2022
t.test(post_data$`Electric Sales Difference`, post_data$`Conventional Sales Difference`,
       mu = mean(post_data$`Conventional Sales Difference`), alternative = "two.sided",
       conf.level = 0.95)

# Inference: Between 2016 and 2019 there is a significant difference between EV sales and conventional vehicle sales,
# however in the second analysis of sales between 2020 and 2021, there seems to be insufficient proof to accurately reject the null
# hypothesis. Hence to conclude the sales mean after COVID hit is not significantly different thereby indicating 
# how buyer preference for EV is climbing with a decline in conventional vehicle preferences.


# Plot for Electric Sales
ggplot(plot_data, aes(x = Year, y = Electric_Difference)) +
  geom_point(color = "blue", size = 3, shape = 16) +
  geom_line(color = "blue", linetype = "solid", size = 1) +
  labs(title = "Electric Sales Differences Over Time",
       x = "Year",
       y = "Electric Sales Difference") +
  theme_minimal()

# Plot for Conventional Sales
ggplot(plot_data, aes(x = Year, y = Conventional_Difference)) +
  geom_point(color = "red", size = 3, shape = 17) +
  geom_line(color = "red", linetype = "dashed", size = 1) +
  labs(title = "Conventional Sales Differences Over Time",
       x = "Year",
       y = "Conventional Sales Difference") +
  theme_minimal()

#Plots showing Trend line of Conventional sales and Electric sales
plot_data <- data.frame(Year = total_data$Year,
                        Electric_Difference = total_data$`Electric Sales Difference`,
                        Conventional_Difference = total_data$`Conventional Sales Difference`)
ggplot(plot_data, aes(x = Year)) +
  geom_point(aes(y = Electric_Difference), color = "blue", size = 3, shape = 16) +
  geom_line(aes(y = Electric_Difference), color = "blue", linetype = "solid", size = 1) +
  geom_point(aes(y = Conventional_Difference), color = "red", size = 3, shape = 17) +
  geom_line(aes(y = Conventional_Difference), color = "red", linetype = "dashed", size = 1) +
  geom_text(aes(x = max(Year), y = max(Electric_Difference), label = "Electric Sales"),
            color = "blue", hjust = 1.2, vjust = 0.5) +
  geom_text(aes(x = max(Year), y = max(Conventional_Difference), label = "Conventional Sales"),
            color = "red", hjust = 1.2, vjust = 0.5) +
  labs(title = "Electric and Conventional Sales Differences Over Time",
       x = "Year",
       y = "Sales Difference") +
  scale_shape_manual(values = c(16, 17)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

# Pre COVID Pie chart
pie_data_pre <- data.frame(X = c("EV sales", "Conventional sales"),
                       Y = c(sum(pre_data$`Electric Sales Difference`),
                             sum(pre_data$`Conventional Sales Difference`)),
                       Z = c((sum(pre_data$`Electric Sales Difference`)/sum(pre_data$`Electric Sales Difference`, pre_data$`Conventional Sales Difference`))*100,
                             (sum(pre_data$`Conventional Sales Difference`)/sum(pre_data$`Electric Sales Difference`, pre_data$`Conventional Sales Difference`))*100))
pie_data_pre$Z <- round(pie_data_pre$Z, 0)
ggplot(pie_data_pre, aes(x = "", y = Y, fill = X)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(Z, "%")), position = position_stack(vjust = 0.5))+
  ggtitle("Proportion of Sales pre COVID") +
  theme_void()                       

# Post COVID Pie chart
pie_data <- data.frame(X = c("EV sales", "Conventional sales"),
                       Y = c(sum(post_data$`Electric Sales Difference`),
                             sum(post_data$`Conventional Sales Difference`)),
                       Z = c((sum(post_data$`Electric Sales Difference`)/sum(post_data$`Electric Sales Difference`, post_data$`Conventional Sales Difference`))*100,
                             (sum(post_data$`Conventional Sales Difference`)/sum(post_data$`Electric Sales Difference`, post_data$`Conventional Sales Difference`))*100))
pie_data$Z <- round(pie_data$Z, 0)
ggplot(pie_data, aes(x = "", y = Y, fill = X)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(Z, "%")), position = position_stack(vjust = 0.5))+
  ggtitle("Proportion of Sales post-covid") +
  theme_void()                       


## Plotting trend lines for 5 states

state_ev_data <- read_excel("~/Library/CloudStorage/OneDrive-NortheasternUniversity/Project R/Group 4/State_wise_EV.xlsx")
state_ev_data <- as.data.frame(state_ev_data)
trend_line_data <- data.frame(X = state_ev_data$Year,
                              S1 = state_ev_data$MA,
                              S2 = state_ev_data$NY,
                              S3 = state_ev_data$GE,
                              S4 = state_ev_data$NJ,
                              S5 = state_ev_data$FL)
ggplot(trend_line_data, aes(x = X))+
  geom_point(aes(y = S1), color ="blue", size = 3, shape = 15)+
  geom_line(aes(y = S1), color = "blue")+
  geom_text(aes(x = max(X), y = max(S1), label = "MA"), color = "blue", hjust = 0.5, vjust = 2)+
  geom_point(aes(y = S2), color ="red", size = 3, shape = 14)+
  geom_line(aes(y = S2), color = "red")+
  geom_text(aes(x = max(X), y = max(S2), label = "NY"), color = "red", hjust = 1.2, vjust = 0.5)+
  geom_point(aes(y = S3), color ="green", size = 3, shape = 16)+
  geom_line(aes(y = S3), color = "green")+
  geom_text(aes(x = max(X), y = max(S3), label = "GE"), color = "green", hjust = 1.2, vjust = -0.5)+
  geom_point(aes(y = S4), color ="brown", size = 3, shape = 17)+
  geom_line(aes(y = S4), color = "brown")+
  geom_text(aes(x = max(X), y = max(S4), label = "NJ"), color = "brown", hjust = 1.2, vjust = 0.5)+
  geom_point(aes(y = S5), color ="orange", size = 3, shape = 18)+
  geom_line(aes(y = S5), color = "orange")+
  geom_text(aes(x = max(X), y = max(S5), label = "FL"), color = "orange", hjust = 1.2, vjust = 0.5)+
  labs(title = "Trend of EV sales of New England States",
       x = "Year",
       y = "Sales")+
  theme_minimal()

# Creating trend lines to verify conventional vehicle distribution
state_nev_data <- read_excel("~/Library/CloudStorage/OneDrive-NortheasternUniversity/Project R/Group 4/State_wise_nonEV.xlsx")
state_nev_data <- as.data.frame(state_nev_data)
trend_line_ndata <- data.frame(X = state_nev_data$Year,
                               S1 = state_nev_data$MA,
                               S2 = state_nev_data$NY,
                               S3 = state_nev_data$GE,
                               S4 = state_nev_data$NJ,
                               S5 = state_nev_data$FL)
ggplot(trend_line_ndata, aes(x = X))+
  geom_point(aes(y = S1), color ="blue", size = 3, shape = 15)+
  geom_line(aes(y = S1), color = "blue")+
  geom_text(aes(x = max(X), y = S1[7], label = "MA"), color = "blue", hjust = 0.5, vjust = 2)+
  geom_point(aes(y = S2), color ="red", size = 3, shape = 14)+
  geom_line(aes(y = S2), color = "red")+
  geom_text(aes(x = max(X), y = S2[7], label = "NY"), color = "red", hjust = 1.2, vjust = 0.5)+
  geom_point(aes(y = S3), color ="green", size = 3, shape = 16)+
  geom_line(aes(y = S3), color = "green")+
  geom_text(aes(x = max(X), y = S3[7], label = "GE"), color = "green", hjust = 0.25, vjust = -0.75)+
  geom_point(aes(y = S4), color ="brown", size = 3, shape = 17)+
  geom_line(aes(y = S4), color = "brown")+
  geom_text(aes(x = max(X), y = S4[7], label = "NJ"), color = "brown", hjust = 0.5, vjust = -1)+
  geom_point(aes(y = S5), color ="orange", size = 3, shape = 18)+
  geom_line(aes(y = S5), color = "orange")+
  geom_text(aes(x = max(X), y = S5[7], label = "FL"), color = "orange", hjust = 1.2, vjust = 0.5)+
  labs(title = "Trend of Non EV sales of New England States",
       x = "Year",
       y = "Sales")+
  theme_minimal()



