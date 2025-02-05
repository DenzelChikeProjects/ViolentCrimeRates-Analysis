#Denzel Chike Project over Violent Crime Rates in California

#Libraries
library(dplyr)
library(ggplot2)
library(caret)
library(cluster)
library("hydroGOF")
library(tidyr)

install.packages("ggplot2")
install.packages("dplyr")
install.packages("caret")
install.packages("cluster") 
install.packages("hydroGOF")
install.packages("tidyr")


#Data Loaded 
violent_crime_data <- read.csv("C:/Users/Chike/Downloads/ViolentCrimeRateInCalifornia(ViolentCrime).csv")


#Statistical Summary
statistical_summary <- violent_crime_data %>%
  filter(strata_level_name == "Violent crime total",
         race_eth_name == "Total",
         geotype == "CA") %>%
  summarise(
    Mean_Rate = mean(rate, na.rm = TRUE),
    Median_Rate = median(rate, na.rm = TRUE),
    SD_Rate = sd(rate, na.rm = TRUE),
    Min_Rate = min(rate, na.rm = TRUE),
    Max_Rate = max(rate, na.rm = TRUE),
    Q1 = quantile(rate, 0.25, na.rm = TRUE),
    Q3 = quantile(rate, 0.75, na.rm = TRUE)
  )
print("Statistical Summary")
print(statistical_summary)




#Question 1: Are there significant increases or decreases during specific time periods?
crime_trend <- violent_crime_data %>%
  filter(strata_level_name == 'Violent crime total', 
         race_eth_name == 'Total',
         geotype == 'CA') %>%
  select(reportyear, rate) %>%
  arrange(reportyear)

q1_plot <- ggplot(crime_trend, aes(x = as.numeric(reportyear), y = rate)) +
  geom_line(color = 'blue') +
  geom_point(color = 'red') +
  theme_minimal() +
  labs(title = 'Violent Crime Rate Trend in California',
       x = 'Year',
       y = 'Rate per 1,000 Population') +
  theme(plot.title = element_text(hjust = 0.5))

#Linear regression model
model <- lm(rate ~ reportyear, data = crime_trend)
print("Linear Regression Summary:")
print(summary(model))

print(q1_plot)





#Question 2: Which counties or cities in California have the highest and lowest violent crime rates?

county_crime <- violent_crime_data %>%
  filter(strata_level_name == 'Violent crime total',
         race_eth_name == 'Total',
         !is.na(county_name)) %>%
  group_by(county_name) %>%
  summarise(avg_rate = mean(rate, na.rm = TRUE)) %>%
  arrange(desc(avg_rate))

highest_crime <- county_crime %>% slice(1)
lowest_crime <- county_crime %>% slice(n())


#Bar plot to find average crime rate by county
q2_plot <- ggplot(county_crime, aes(x = reorder(county_name, avg_rate), y = avg_rate)) +
  geom_bar(stat = 'identity', fill = 'gold') +
  coord_flip() +
  theme_minimal() +
  labs(title = 'Average Violent Crime Rates by County',
       x = 'County',
       y = 'Average Rate per 1,000 Population') +
  theme(plot.title = element_text(hjust = 0.5))

print("Highest Crime Rate County:")
print(highest_crime)
print("Lowest Crime Rate County:")
print(lowest_crime)

print(q2_plot)







#Question 3:Did the Total Violent Crime rate increase or decrease in California from 2000 to 2013? 


crime_trend <- violent_crime_data %>%
  filter(
    strata_level_name != 'Violent Crime Total',
    !is.na(rate),
    geotype == "CA"
  )%>%
  arrange(reportyear)


#Line Plot for the total crime rate from 2000 to 2013
q3_plot <- ggplot(crime_trend, aes(x = reportyear, y = rate,)) +
  geom_line(color = "blue") +
  geom_point(color = "green") +
  labs(title = 'Trend in Total Violent Crime Rates in Calirfornia from 2000-2013',
       x = 'Year',
       y = 'Rate per 1,000 Population') +
  theme_minimal() +
  theme(legend.position = "bottom")

print(q3_plot)


summary(q3_plot)



#Question 4: Are there seasonal patterns in violent crime rates?

#Linear regression model and Diagnostic Plots
crime_model <- lm(rate ~ reportyear, data = crime_trend)

par(mfrow = c(2,2))
plot(crime_model, main = "Diagnostic Plots for Linear Regression")

q4_plot <- ggplot(crime_trend, aes(x = as.factor(reportyear), y = rate)) +
  geom_boxplot(fill = "green") +
  theme_minimal() +
  labs(title = 'Distribution of Crime Rates by Year',
       x = 'Year',
       y = 'Rate per 1,000 Population') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45))


#The box plot
print(q4_plot)

#Model Diagnostics Summary 
print("Model Diagnstics:")
print(summary(crime_model))





#Question 5: Can we predict whether a county has a high crime rate based on its population size?
simple_model_data <- violent_crime_data %>%
  filter(
    race_eth_name == "Total",
    !is.na(rate),
    !is.na(dof_population),
    strata_level_name == "Violent crime total"
  ) %>%
  mutate(
    crime_threshold = quantile(rate, 0.75, na.rm = TRUE),
    high_crime = ifelse(rate > crime_threshold, 1, 0),
    population_scaled = scale(log(dof_population))
  )

#Divide as training and testing: 20% test 80% train and get the training data size

set.seed(123)
sample_size <- floor(0.8 * nrow(simple_model_data))
train_ind <- sample(seq_len(nrow(simple_model_data)), size = sample_size)

train_data <- simple_model_data[train_ind, ]
test_data <- simple_model_data[-train_ind, ]

#Logistic Regression Model
glm_model <- glm(high_crime ~ population_scaled,
                 data = simple_model_data,
                 family = binomial(link = 'logit'))

print("GLM Model Summary:")
print(summary(glm_model))


#Predict
predicted_prob <- predict(glm_model, newdata = test_data, type = 'response')
print("Predicted Probabilities:")
print(head(predicted_prob))

#Newdata dataframe
newdata <- data.frame(
  population_scaled = test_data$population_scaled,
  high_crime = test_data$high_crime,
  predicted_prob = predicted_prob
)
print("Head of newdata:")
print(head(newdata)) #Answer is low aka No


#Visualization 
ggplot(newdata, aes(x = population_scaled, y = predicted_prob)) +
  geom_point() +
  labs(title = "Predicted Probability vs Population Size",
       x = "Scaled Population (log)",
       y = "Predicted Probability of High Crime") +
  theme_minimal()

#Prediction Results
glm_pred <- factor(ifelse(predicted_prob > 0.5, 'high', 'low'),
                   levels = levels(test_data$high_crime))


simple_conf_matrix <- table(Predicted = glm_pred, Actual = test_data$high_crime)

print("Confusion Matrix:")
print(simple_conf_matrix) 

