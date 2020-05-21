# gapminderGraph.R
# About - This script uses data from Gapminder libarary to plot GDP Per Capita vs Life Expenctancy for countries. The size of the circle is proportional to the size of the country
# Author - @repulsivestud
# Date - 21/05/2020

library(tidyverse)
library(hrbrthemes)
library(gganimate)
library(gapminder)

gapminder %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, colour = country, size = pop)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_color_manual(values = country_colors) +
  scale_x_log10() +
  labs(title = 'GDP Per Capita vs Life Expectancy - Year: {frame_time}', subtitle = "Size proportional to population of the country", x = 'GDP per capita', y = 'Life Expectancy', caption="© Omkar Shukla (@shukla_omkar)\n Data source: gapminder.org") +
  transition_time(year) +
  ease_aes('linear') + 
  theme_ipsum_ps() +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10))-> gapminderGraph

animate(gapminderGraph, fps = 15, width = 8, height = 8, units = "in", res = 300, renderer = gifski_renderer())

anim_save("gapminderGraph.gif")