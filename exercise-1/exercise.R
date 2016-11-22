# Exercise-1
# Implement code from this book chapter: http://r4ds.had.co.nz/many-models.html

# Packages
# install.packages('modelr')
# install.packages('tidyverse')
# install.packages('gapminder')
library(gapminder)
library(modelr)
library(tidyverse)

# Initial view of the data with ggplot
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

# Look only at new zealand
gapminder %>% filter(country == "New Zealand") %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)


# Better yet, write your own function to accept a country as a parameter,
# and produce the same graphics
makeGraph <- function(input.country){
  gapminder %>% filter(country == input.country) %>% 
    ggplot(aes(year, lifeExp, group = country)) +
    geom_line(alpha = 1/3)
}


# Nest the data by country/continent
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()


# Define a statistical model, and store it in a function
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

# Use the `map` functionality to run the same model for each country separately
by_country <- by_country %>% 
  mutate(model = map(data, country_model))

# Add additional columns to store your residuals (distance between data and prediction)
by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )

# Unnest your residual
resids <- unnest(by_country, resids)


# Plot the residuals
resids %>% 
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1/3) +
  facet_wrap(~continent)


# Plot residuals by continent
glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)

# Use `glance` to look at model quality
glance %>% 
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)

# Compare model quality to continent


# View country that have an r.squared value of less than .25

