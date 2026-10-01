# Load packages
library(dplyr)
library(ggplot2)
library(NHANES)
library(mosaic)

# Filter dataset for research variables
NHANES2 <- NHANES %>%
  filter(
    !is.na(Poverty),
    !is.na(HHIncomeMid),
    !is.na(AgeDecade),
    !is.na(Education),
    Education %in% c("Some College", "College Grad")
  ) %>%
  select(Poverty, HHIncomeMid, AgeDecade, Education)

# Basic tallies
tally(NHANES2$AgeDecade)
tally(NHANES2$Education)
tally(NHANES2$Poverty)
tally(NHANES2$HHIncomeMid)

# Boxplot: Income by education level across age groups
ggplot(NHANES2, aes(x = Education, y = HHIncomeMid)) +
  geom_boxplot() +
  facet_wrap(~ AgeDecade) +
  labs(
    title = "Income by Education Level Across Age Groups",
    x = "Education Level",
    y = "Household Income (Midpoint)"
  )

# Mean income summary
NHANES_summary <- NHANES2 %>%
  group_by(AgeDecade, Education) %>%
  summarise(mean_income = mean(HHIncomeMid))

# Bar chart: Mean income by education level and age group
ggplot(NHANES_summary, aes(x = Education, y = mean_income, fill = Education)) +
  geom_col() +
  facet_wrap(~ AgeDecade) +
  labs(
    title = "Mean Income by Education Level and Age Group",
    x = "Education Level",
    y = "Mean Household Income"
  )

# Density plot: Poverty ratio distribution
ggplot(NHANES2, aes(x = Poverty, fill = Education)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ AgeDecade) +
  labs(
    title = "Distribution of Poverty Income Ratio by Education Level",
    x = "Poverty Income Ratio",
    y = "Density"
  )
