# Carsten Stann 
# Tidy Tuesday Video Games: 2019-07-23
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-07-30

library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)
library(ggbeeswarm)

# import --------------------------------------------------------------------------------

import <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

import

# clean ---------------------------------------------------------------------------------

games <- import %>% 
   mutate(release_date = parse_date(release_date, format = "%b %d, %Y"),
          month = month(release_date),
          year = year(release_date), 
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

ggplot(games, aes(x = factor(year), y = average_playtime, fill = factor(year))) +
   geom_violin() +
   scale_y_log10() +
   coord_flip()

filter(games, average_playtime > 0) %>% 
ggplot(aes(x = factor(year), y = average_playtime)) +
   geom_boxplot(alpha = 0) +
   geom_jitter(aes(col = factor(year)), alpha = 0.5) +
   scale_y_log10() +
   coord_flip() +
   labs(title = "PC Game Average Play Time By Release Year", 
        x = NULL, 
        y = "Average Playing Time") +
   theme_minimal()


filter(games, year == 2005) %>% 
   arrange(desc(average_playtime))

games %>% 
   group_by(year) %>% 
   arrange(desc(average_playtime)) %>% 
   top_n(1, average_playtime) %>% 
   ungroup() %>% 
   arrange(year) %>% 
   ggplot(aes(x = year, y = average_playtime)) +
      geom_point() +
      geom_label_repel(aes(x = year, y = average_playtime, label = game), cex = 2.5)


today() - as.Date("2019-08-01", format = "%Y-%m-%d")

today() - games$release_date

ggplot(games, aes(x = days_since_release, y = average_playtime)) +
   geom_point() +
   scale_y_log10()






