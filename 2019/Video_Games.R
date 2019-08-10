# Carsten Stann 
# Tidy Tuesday Video Games: 2019-07-23
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-07-30

library(tidyverse)
library(lubridate)
library(skimr)
library(RColorBrewer)
library(ggExtra)
library(ggrepel)

# import --------------------------------------------------------------------------------

import <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

import

# clean ---------------------------------------------------------------------------------

games <- import %>% 
   mutate(release_date = parse_date(release_date, format = "%b %d, %Y"),
          month = month(release_date),
          year = year(release_date) %>% factor(), 
          days_since_release = ifelse(is.na(release_date), NA, today() - release_date))

# summarise -----------------------------------------------------------------------------

games

skim(games)

# exploratory plots ---------------------------------------------------------------------

games %>% 
   group_by(year) %>% 
   summarise(n_games = n()) %>% 
ggplot(aes(x = year, y = n_games)) +
   geom_col() +
   labs(title = "PC Games Released by Steam",
        subtitle = "By Release Year", 
        y = "Number of Games", 
        x = "Release Year")

games %>% 
   group_by(year) %>% 
   summarise(n_games_played = sum(median_playtime > 0, na.rm = TRUE)) %>% 
   ggplot(aes(x = year, y = n_games_played)) +
      geom_col() +
      labs(title = "PC Games Played from July 1 - 15, 2019", 
           subtitle = "By Release Year", 
           y = "Number of Games Played (July 1 - 15, 2019)",
           x = "Release Year")


plot_data <- filter(games, average_playtime > 0, !is.na(year))
(plot <-  ggplot(plot_data, aes(x = year, y = average_playtime)) +
      geom_point() +
      scale_y_log10() +
      theme_minimal()
)   
   
ggMarginal(plot, type = "density")











filter(games, average_playtime > 0, !is.na(year)) %>% 
ggplot(aes(x = year, y = average_playtime)) +
   geom_jitter(alpha = 0.4) +
   geom_boxplot(alpha = 0) +
   scale_y_log10() +
   coord_flip() +
   labs(title = "PC Game Average Play Time By Release Year", 
        x = NULL, 
        y = "Average Playing Time") +
   theme_minimal()




display.brewer.all(colorblindFriendly = T)


filter(games, year == 2005) %>% 
   arrange(desc(average_playtime))







