Violent Crime Rates in California
================
Denzel Chike
2024-12-09

## by Denzel Chike

### Introduction

Throughout the years, California’s crime rates have risen and gotten
worse. High crime rates can affect the safety of communities. By
analyzing California crime rate patterns and predictors, I can provide
law enforcement with valuable resources. My project primarily focuses on
the crime rates in California and focuses on the analytical questions I
came up with. The questions are,

- Are there significant increases or decreases during specific time
  periods?

- Which counties or cities in California have the highest and lowest
  violent crime rates?

- Did the Total Violent Crime rate increase or decrease in California
  from 2000 to 2013?

- Are there seasonal patterns in violent crime rates?

- Can we predict whether a county has a high crime rate based on its
  population size?

This project aims to find important insights regarding crime trends and
patterns that will help with decision-making.

\#Denzel Chike Project over Violent Crime Rates in California

**\#Libraries**

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(caret)
```

    ## Loading required package: lattice

``` r
library(cluster)
library("hydroGOF")
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## Attaching package: 'hydroGOF'

    ## The following object is masked from 'package:caret':
    ## 
    ##     R2

``` r
library(tidyr)
```

install.packages(“ggplot2”) install.packages(“dplyr”)
install.packages(“caret”) install.packages(“cluster”)
install.packages(“hydroGOF”) install.packages(“tidyr”)

**\#Data Loaded**

``` r
violent_crime_data <- read.csv("C:/Users/Chike/Downloads/ViolentCrimeRateInCalifornia(ViolentCrime).csv")
```

**\#Statistical Summary**

``` r
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
```

    ## [1] "Statistical Summary"

``` r
print(statistical_summary)
```

    ##   Mean_Rate Median_Rate   SD_Rate Min_Rate Max_Rate       Q1       Q3
    ## 1  5.135382    5.253545 0.7580097 3.959039 6.217499 4.483367 5.702369

\#**Question 1: Are there significant increases or decreases during
specific time periods?**

``` r
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
```

    ## [1] "Linear Regression Summary:"

``` r
print(summary(model))
```

    ## 
    ## Call:
    ## lm(formula = rate ~ reportyear, data = crime_trend)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.223468 -0.080754 -0.001201  0.069088  0.194959 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 364.220349  16.445185   22.15 4.23e-11 ***
    ## reportyear   -0.178961   0.008196  -21.84 4.99e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1236 on 12 degrees of freedom
    ## Multiple R-squared:  0.9754, Adjusted R-squared:  0.9734 
    ## F-statistic: 476.8 on 1 and 12 DF,  p-value: 4.993e-11

``` r
print(q1_plot)
```

![](DenzelChikeViolentCrimeRatesInCalifornia_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

\#**Question 2: Which counties or cities in California have the highest
and lowest violent crime rates?**

``` r
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
```

    ## [1] "Highest Crime Rate County:"

``` r
print(highest_crime)
```

    ## # A tibble: 1 × 2
    ##   county_name avg_rate
    ##   <chr>          <dbl>
    ## 1 Los Angeles     12.5

``` r
print("Lowest Crime Rate County:")
```

    ## [1] "Lowest Crime Rate County:"

``` r
print(lowest_crime)
```

    ## # A tibble: 1 × 2
    ##   county_name avg_rate
    ##   <chr>          <dbl>
    ## 1 Marin           1.65

``` r
print(q2_plot)
```

![](DenzelChikeViolentCrimeRatesInCalifornia_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

\#**Question 3:Did the Total Violent Crime rate increase or decrease in
California from 2000 to 2013?**

``` r
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
```

![](DenzelChikeViolentCrimeRatesInCalifornia_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
summary(q3_plot)
```

    ## data: ind_id, ind_definition, reportyear, race_eth_code, race_eth_name,
    ##   geotype, geotypevalue, geoname, county_fips, county_name,
    ##   region_code, region_name, strata_name_code, strata_name,
    ##   strata_level_name_code, strata_level_name, numerator, denominator,
    ##   rate, ll_95ci, ul_95ci, se, rse, ca_decile, ca_rr, dof_population,
    ##   version [14x27]
    ## mapping:  x = ~reportyear, y = ~rate
    ## faceting: <ggproto object: Class FacetNull, Facet, gg>
    ##     compute_layout: function
    ##     draw_back: function
    ##     draw_front: function
    ##     draw_labels: function
    ##     draw_panels: function
    ##     finish_data: function
    ##     init_scales: function
    ##     map_data: function
    ##     params: list
    ##     setup_data: function
    ##     setup_params: function
    ##     shrink: TRUE
    ##     train_scales: function
    ##     vars: function
    ##     super:  <ggproto object: Class FacetNull, Facet, gg>
    ## -----------------------------------
    ## geom_line: na.rm = FALSE, orientation = NA
    ## stat_identity: na.rm = FALSE
    ## position_identity 
    ## 
    ## geom_point: na.rm = FALSE
    ## stat_identity: na.rm = FALSE
    ## position_identity

\#**Question 4: Are there seasonal patterns in violent crime rates?**

``` r
#Linear regression model and Diagnostic Plots
crime_model <- lm(rate ~ reportyear, data = crime_trend)

par(mfrow = c(2,2))
plot(crime_model, main = "Diagnostic Plots for Linear Regression")
```

![](DenzelChikeViolentCrimeRatesInCalifornia_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
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
```

![](DenzelChikeViolentCrimeRatesInCalifornia_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
#Model Diagnostics Summary 
print("Model Diagnostics:")
```

    ## [1] "Model Diagnostics:"

``` r
print(summary(crime_model))
```

    ## 
    ## Call:
    ## lm(formula = rate ~ reportyear, data = crime_trend)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.223468 -0.080754 -0.001201  0.069088  0.194959 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 364.220349  16.445185   22.15 4.23e-11 ***
    ## reportyear   -0.178961   0.008196  -21.84 4.99e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1236 on 12 degrees of freedom
    ## Multiple R-squared:  0.9754, Adjusted R-squared:  0.9734 
    ## F-statistic: 476.8 on 1 and 12 DF,  p-value: 4.993e-11

\#**Question 5: Can we predict whether a county has a high crime rate
based on its population size?**

``` r
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
```

    ## [1] "GLM Model Summary:"

``` r
print(summary(glm_model))
```

    ## 
    ## Call:
    ## glm(formula = high_crime ~ population_scaled, family = binomial(link = "logit"), 
    ##     data = simple_model_data)
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       -1.09920    0.02709 -40.575   <2e-16 ***
    ## population_scaled  0.04043    0.02699   1.498    0.134    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 8179.2  on 7272  degrees of freedom
    ## Residual deviance: 8176.9  on 7271  degrees of freedom
    ## AIC: 8180.9
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
#Predict
predicted_prob <- predict(glm_model, newdata = test_data, type = 'response')
print("Predicted Probabilities:")
```

    ## [1] "Predicted Probabilities:"

``` r
print(head(predicted_prob))
```

    ##         4         7         8        10        11        12 
    ## 0.2495644 0.2466256 0.2654733 0.2566917 0.2646307 0.2482298

``` r
#Newdata dataframe
newdata <- data.frame(
  population_scaled = test_data$population_scaled,
  high_crime = test_data$high_crime,
  predicted_prob = predicted_prob
)
print("Head of newdata:")
```

    ## [1] "Head of newdata:"

``` r
print(head(newdata)) #Answer is low aka No
```

    ##    population_scaled high_crime predicted_prob
    ## 4        -0.04287016          0      0.2495644
    ## 7        -0.43256571          0      0.2466256
    ## 8         2.01572094          0      0.2654733
    ## 10        0.88968564          0      0.2566917
    ## 11        1.90873666          1      0.2646307
    ## 12       -0.21946467          0      0.2482298

``` r
#Visualization 
ggplot(newdata, aes(x = population_scaled, y = predicted_prob)) +
  geom_point() +
  labs(title = "Predicted Probability vs Population Size",
       x = "Scaled Population (log)",
       y = "Predicted Probability of High Crime") +
  theme_minimal()
```

![](DenzelChikeViolentCrimeRatesInCalifornia_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
#Prediction Results
glm_pred <- factor(ifelse(predicted_prob > 0.5, 'high', 'low'),
                   levels = levels(test_data$high_crime))


simple_conf_matrix <- table(Predicted = glm_pred, Actual = test_data$high_crime)

print("Confusion Matrix:")
```

    ## [1] "Confusion Matrix:"

``` r
print(simple_conf_matrix) 
```

    ## < table of extent 0 x 2 >
