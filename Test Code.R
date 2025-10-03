library(tidyverse)
library(wbstats)
library(poliscidata)
library(countrycode)
library(broom)
library(janitor)
library(ggridges)
library(modelsummary)

gapminder_df <- wb_data(
  indicator = c("SP.DYN.LE00.IN", "NY.GDP.PCAP.CD"),
  start_date = 2016,
  end_date = 2016
) |> 
  rename(
    life_exp = SP.DYN.LE00.IN,
    gdp_per_cap = NY.GDP.PCAP.CD
  ) |> 
  mutate(
    log_gdp_per_cap = log(gdp_per_cap),
    region = countrycode(country, "country.name", "region", custom_match = c("Turkiye" = "Europe & Central Asia"))
  ) |> 
  relocate(region, .after = country)

gapminder_df

ggplot(gapminder_df, aes(x = gdp_per_cap, y = life_exp)) + 
  geom_point() + 
  theme_minimal() + 
  labs(x = "GDP per capita (USD current)",
       y = "Average life expectancy (years)") + 
  scale_x_continuous(labels = label_dollar())

