install.packages(c("tidyverse",
                   "wbstats",
                   "janitor",
                   "skimr",
                   "countrycode",
                   "scales"))

library(tidyverse)
library(polisciols)
library(wbstats)
library(janitor)
library(skimr)
library(countrycode)
library(scales)

polisciols::nes
distinct(nes, income_gap)
skim(nes$income_gap)

##categorical variables = factors

tabyl(nes, income_gap)

## Has income gap grown over 20 years?
ggplot(nes, aes(y = income_gap)) + 
  geom_bar() +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot") + 
  labs(
    title = "Do you think the difference in incomes between rich people and poor people in the United States today is larger, \nsmaller, or about the same as it was 20 years ago?", 
    x = "Count of respondents",
    y = NULL,
    caption = "Source: ANES 2020 Survey"
  ) + 
  scale_x_continuous(labels = scales::label_comma())

## How much each country spends on education as a proportion of its gross domestic product (GDP)
perc_edu <- wb_data(
  "SE.XPD.TOTL.GD.ZS", start_date = 2020, end_date = 2020, return_wide = F
) |> 
  transmute(
    country, 
    region = countrycode(country, "country.name", "region"),
    year = date,
    value
  )

perc_edu

## Graph it
ggplot(perc_edu, aes(x = value)) + 
  geom_histogram(binwidth = 1) + 
  theme_minimal() + 
  labs(
    x = "Expenditure on education as a proportion of GDP",
    y = "Number of countries"
  )

ggplot(perc_edu, aes(x = value)) + 
  geom_histogram(binwidth = 0.25) + 
  theme_minimal() + 
  labs(
    x = "Expenditure on education as a proportion of GDP",
    y = "Number of countries"
  )

ggplot(perc_edu, aes(x = value)) + 
  geom_density() + 
  theme_minimal() + 
  labs(
    x = "Expenditure on education as a proportion of GDP",
    y = "Density"
  )

## Descriptive statistics
mean(perc_edu$value, na.rm = T)
median(perc_edu$value, na.rm = T)

x <- c(1, 1, 2, 4, 5, 32, 5, 1, 10, 3, 4, 6, 10)

table(x)
x <- 1:10
x
mean(x)
median(x)
x <- c(x, 1000)
x
mean(x)
median(x)

skim(perc_edu$value)
ggplot(perc_edu, aes(x = value)) + 
  geom_boxplot() + 
  theme_minimal() + 
  theme(
    axis.text.y = element_blank()
  ) + 
  labs(
    x = "Expenditure on education as a proportion of GDP",
    y = NULL
  )

max(perc_edu$value, na.rm = T) - min(perc_edu$value, na.rm = T)
wide_dist
narrow_dist

wide_dist_mean <- mean(wide_dist$x)

wide_var_calc <- wide_dist |> 
  mutate(
    mean = wide_dist_mean,
    diff = x - mean,
    diff_2 = diff^2
  )
wide_var_calc

wide_var <- sum(wide_var_calc$diff_2) / (nrow(wide_var_calc) - 1)
wide_var

narrow_var_calc <- narrow_dist |> 
  mutate(
    mean = mean(narrow_dist$x),
    diff = x - mean,
    diff_2 = diff^2
  )

narrow_var <- sum(narrow_var_calc$diff_2) / (nrow(narrow_var_calc) - 1)
narrow_var

var(wide_dist)
var(narrow_dist)
var(wide_dist) > var(narrow_dist)
sqrt(wide_var)
sqrt(narrow_var)
sd(wide_dist$x)
sd(narrow_dist$x)
