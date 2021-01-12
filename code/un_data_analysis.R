library(tidyverse)

# load data
gapminder_data <- read_csv("data/gapminder_data.csv")
View(gapminder_data)

# use the summarize function to find summary statistics
summarize(gapminder_data, averageLifeExp = mean(lifeExp))

# piping function: %>%
gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))

# Exercise: find the mean population of the gapminder dataset
gapminder_data %>% summarize(averagePopulation = mean(pop), 
                             recent_year = max(year))

# filters rows where year is 2007
gapminder_data %>% filter(year == 2007) %>% 
  summarize(averageLifeExp = mean(lifeExp))

# Exercise: find the average GDP per capita for the first year in the dataset
gapminder_data %>% 
  filter(year == min(year)) %>% 
  summarize(averageGDP = mean(gdpPercap), first_year = min(year)) # the last expression adds a row with the first year

# we can use: >, <, >=, <=

# group_by: organizes and groups my data
gapminder_data %>% 
  group_by(year) %>% 
  summarize(averageLifeExp = mean(lifeExp))

# Exercise: find the mean life expectancy for each continent
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(averageLifeExp = mean(lifeExp))

# mutate - add more column to your dataset
gapminder_data %>% 
  mutate(gdp = gdpPercap * pop)

# Exercise: make a new column using mutate() that is population in millions
gapminder_data %>% 
  mutate(pop_in_millions = pop/1000000)

# select() - specify which column we want to keep
year_pop <- gapminder_data %>% 
  select(year, pop) #year_pop <- would save my data of interest into a new variable

gapminder_data %>% 
  select(-continent) #I just want to drop the continent column

# Exercise: create a dataset with the country, continent, year, and lifeExp columns
gapminder_data %>% 
  select(country, continent, year, lifeExp)

gapminder_data %>% 
  select(-pop, -gdpPercap) #just drop the columns that I don't want

# arrange(year) - arrange rows

# long vs wide
# pivot_longer, and pivot_wider

gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp) # if we don't drop the population column --> it will give us
# a lot of NA because it will also use the population values

# rename() = rename columns

# Exercise: Create a new dataset with only data from the Americas and 2007
# drop the continent and year columns
gapminder_Americas_2007 <- gapminder_data %>% 
  filter(continent == 'Americas', year == 2007) %>%
  select(-continent, -year)

gapminder_data <- read_csv("data/gapminder_data.csv") %>% 
  filter(continent == 'Americas', year == 2007) %>%
  select(-continent, -year)
View(gapminder_data)

# Goals: data from a year that is close to 2007
# A column for country and we want columns for different types for Co2 emissions
# (total, per capita)
# Exercise: select only the country, year, series, value
co2_emissions <- read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region", "country", "year", "series", "value",
                       "footnotes", "source")) %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year) %>% 
  mutate(country = recode(country, "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States",
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))
View(co2_emissions)

# inner_join() uses info that overlaps, out_join() keeps data that do not overlap
inner_join(gapminder_data, co2_emissions, by = "country")
# or
gapminder_data %>% inner_join(co2_emissions, by = "country")

anti_join(gapminder_data, co2_emissions) # what rows in the first dataset are not in the second dataset

# change Puerto Rico to be a part of the US
gapminder_data <- read_csv("data/gapminder_data.csv") %>%
  filter(continent == 'Americas', year == 2007) %>%
  select(-continent, -year) %>% 
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>% 
  group_by(country) %>% 
  summarize(lifeExp = sum(lifeExp * pop)/sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop))

gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by = "country")

# mutate and the if else
gap_co2_region <- gapminder_co2 %>% 
  mutate(region = if_else(country == "Canada" | 
                            country == "United States" |
                            country == "Mexico", "north", "south"))
View(gap_co2_region)

# country %in% c("Canada", "United States", "Mexico")
# | - or
# && - and
# ! - not

# is there a relationship between gdp and co2
# exercise: create a scatter plot of gdp vs co2 emissions, color it by region
ggplot(gap_co2_region, aes(x = per_capita_emissions, y = gdpPercap, color = region)) +
  geom_point() +
  labs(x = "CO2 Emissions", y = "GDP per capita")
