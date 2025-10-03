#### Question 1 ####
####  Part A    ####
## Save dataset + edu variable
polisciols::nes2024
nes2024 <- nes2024
education <- data$education

## Review proportion of respondents in each education level (numerically and visually)
tabyl(nes2024, education)

ggplot(nes2024, aes(y = education)) + 
  geom_bar() +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot") + 
  labs(
    x = "Count of respondents",
    y = NULL,
    caption = "Source: ANES 2024 Survey"
  ) + 
  scale_x_continuous(labels = scales::label_comma())

## "Some post-high school, no bachelor's degree" is the highest level of education obtained by respondents


#### Question 2 ####
####  Part A    ####
tabyl(nes2024, attention_to_politics)

ggplot(nes2024, aes(y = attention_to_politics)) + 
  geom_bar() +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot") + 
  labs(
    x = "Count of respondents",
    y = NULL,
    caption = "Source: ANES 2024 Survey"
  ) + 
  scale_x_continuous(labels = scales::label_comma())

## Let's look at the mean

attention_to_politics <- as.factor(nes2024$attention_to_politics)
attention_to_politics_num <- as.numeric(nes2024$attention_to_politics)

mean(attention_to_politics_num, na.rm = T)
median(attention_to_politics_num, na.rm = T)

#### Question 3 ####
####  Part A    ####

iq <- c(
  145, 139, 126, 122, 125, 130, 96, 110, 118, 118, 101, 142, 134, 124, 112, 109, 
  134, 113, 81, 113, 123, 94, 100, 136, 109, 131, 117, 110, 127, 124, 106, 124, 
  115, 133, 116, 102, 127, 117, 109, 137, 117, 90, 103, 114, 139, 101, 122, 105, 
  97, 89, 102, 108, 110, 128, 114, 112, 114, 102, 82, 101
)

table(iq)
mean(iq)
median(iq)

skim(iq)

## Turn list into data frame in order make a boxplot
iq_df <- as.data.frame(iq)

ggplot(iq_df, aes(x = iq)) + 
  geom_boxplot() + 
  theme_minimal() + 
  theme(
    axis.text.y = element_blank()
  )

#### Question 4 ####
glimpse(nes2024)
names(nes2024)

#### Question 5 ####
## In order to plot two columns of data on a single density plot, I will need to transform the data
nes_new <- nes2024 %>%
  pivot_longer(
    cols = c(therm_harris, therm_trump),
    names_to = "candidate",
    values_to = "therm_rating"
  )

## Plot data with distinguishing colors
ggplot(nes_new, aes(x = therm_rating, color = candidate)) + 
  geom_density() + 
  theme_minimal() + 
  labs(
    x = "Rating of Presidential Candidates",
  )  +
  scale_color_manual(values = c("therm_harris" = "blue", "therm_trump" = "red")) +
  theme(legend.title = element_blank())
  
skim(nes2024)
tttest <- t.test(nes2024$therm_harris, nes2024$therm_trump, paired = TRUE)
tttest
