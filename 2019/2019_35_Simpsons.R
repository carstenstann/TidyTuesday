# Carsten Stann 
# Tidy Tuesday Simpsons -  Week 35
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-08-27

library(tidyverse)
library(tvthemes)

# import --------------------------------------------------------------------------------
simpsons <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv")

simpsons

# EDA -----------------------------------------------------------------------------------

import_simpsons()
library(extrafont)
loadfonts() 

simpsons %>% 
   mutate(guest_star = fct_infreq(fct_lump(guest_star, n = 9)),
          season = recode(season, Movie = "31"),
          season = as.numeric(season)) %>% 
ggplot(aes(x = fct_recode(factor(season), Movie = "31"))) +
   geom_bar(mapping = aes(fill = fct_relevel(guest_star, "Other", after = Inf)),
            position = "fill"
   ) +
   labs(title = "The Simpsons - top 10 recurring guest stars by season",
        x = "Season",
        y = "Proportion of Appearances",
        fill = "Guest Star"
   ) +
   scale_y_continuous(limits = c(0, 1), 
                      expand = expand_scale(mult = c(.03, 0))
   ) +
   scale_fill_simpsons() +
   theme_simpsons(title.font = "Akbar", 
                  text.font = "Akbar")

ggsave("./README_figs/Simpsons.png", width = 12, height = 7, dpi = 320)


