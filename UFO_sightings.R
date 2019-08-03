# Tidy Tuesday UFOs
library(tidyverse)
library(lubridate)
library(skimr)
library(maps)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

ufo_sightings
glimpse(ufo_sightings)

ufo <- ufo_sightings %>% 
   mutate(date_time = mdy_hm(date_time),
          day = day(date_time),
          month = month(date_time),
          quarter = quarter(date_time),
          semester = semester(date_time),
          year = year(date_time),
          hour = hour(date_time),
          minute = minute(date_time),
          date_documented = mdy(date_documented),
          ufo_shape = factor(tolower(ufo_shape)))


ggplot(ufo, aes(x = fct_infreq(fct_lump(ufo_shape, n = 5)))) + 
   geom_bar() + 
   coord_flip()

filter(ufo, year >= 2000) %>% 
   drop_na(ufo_shape) %>% 
ggplot(aes(x = year)) + 
   geom_bar(aes(fill = fct_lump(ufo_shape, 4)))

filter(ufo, year >= 2000) %>% 
   drop_na(ufo_shape) %>% 
ggplot(aes(x = month)) +
   geom_bar(aes(fill = fct_lump(ufo_shape, 4)))

filter(ufo, year >= 2000) %>% 
   drop_na(ufo_shape) %>%  
   ggplot(aes(x = hour)) + 
   geom_bar(aes(fill = fct_lump(ufo_shape, 4)))


