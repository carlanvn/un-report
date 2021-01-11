# load tidyverse packages
library(tidyverse)

# read in data
# the data at the right of the arrow will be assigned to the left
gapminder_1997 <- read_csv("data/gapminder_1997.csv")

# learn more about a function
?read_csv

read_csv(file = "data/data/gapminder_1997.csv")

round(x = 3.1415)
round(x = 3.1415, digits = 2)
round(digits = 2, x = 3.1415)
round(2, 3.1415) # if digits is decimal, then R rounds it to the nearest integer
# be careful with spaces between functions - they change

# make a plot
# aes can be inside or outside ggplot; if inside, will apply to everything else
# this code is collapsed so that it is more concise
ggplot(data = gapminder_1997, aes(x = gdpPercap, y = lifeExp, color = continent,size = pop/1000000)) +
  labs(x = "GDP Per Capita", y = "Life Expectancy", 
       size = "Population (in millions)", title = "Do people in wealthy 
       countries live longer?") +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  aes(shape = continent) # Looks ugly, but good for illustration purposes

# can use single or double quotes and it doesn't matter
# backticks for variables which have spaces in them: `gdp per capita`

# read in full dataset
gapminder_data <- read_csv("data/gapminder_data.csv")
dim(gapminder_data)

ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent, group = country) + 
  geom_line()

# different color palettes
RColorBrewer::display.brewer.all()

# open variable in new tab
View(gapminder_data)

# plot categorical variables
# use gapminder_1997 data with geom_boxplot() to make boxplot where continent is
# the x axis and life expectancy is the y axis
ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  labs(x = 'Continent', y = 'Life Expectancy (years)', title = "Life expectancy by continent (1997)") +
  geom_jitter(aes(size=pop)) + geom_violin(alpha = 0.5, aes(fill = continent)) # plots first geom_jitter and adds another layer of geom_violin
# we could do it viceversa and we could see the points (not blocking the violins), or we could also use 
# alpha - transparency, for the color of the violins - inside color--> fill, outside color--> color
# we can also use geom_boxplot()
# cool color = 'thistle2'

ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp, fill = continent)) +
  labs(x = 'Continent', y = 'Life Expectancy (years)', title = "Life expectancy by continent (1997)") +
  geom_jitter(aes(size=pop)) + geom_violin(alpha = 0.5)

sample(colors(), size = 10)

# univariate plots
ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# saving plots
ggsave("figures/awesome_plot.jpg", width = 6, height = 4)

violin_plot <- ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  labs(x = 'Continent', y = 'Life Expectancy (years)', title = "Life expectancy by continent (1997)") +
  geom_jitter(aes(size=pop)) + 
  geom_violin(alpha = 0.5, aes(fill = continent))

violin_plot
violin_plot + theme_bw()
violin_plot

violin_plot <- violin_plot + theme_bw()
violin_plot

ggsave("figures/awesome_violin_plot.jpg", plot = violin_plot, width = 6, height = 4)

# faceting plots
ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))

ggsave("figures/my_awesome_plot.jpg", width = 6, height = 4)
