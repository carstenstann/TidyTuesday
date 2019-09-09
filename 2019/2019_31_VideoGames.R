# Carsten Stann 
# Tidy Tuesday Video Games: 2019-07-23
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-07-30

library(tidyverse)
library(lubridate)
library(skimr)
library(RColorBrewer)
library(ggExtra)
library(ggrepel)
library(patchwork)

## ggplot theme updates
source("./theme/theme_nasa.R")

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


plot_data <- filter(games, average_playtime > 0, !is.na(year))
ggplot(plot_data, aes(x = year, y = average_playtime)) +
      geom_point() +
      scale_y_log10() +
      theme_minimal()

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

# filter for highest average playtime in each year for data labels ----------------------

top_game_by_year <- games %>% 
   filter(!is.na(year), year >= 2010) %>% 
   group_by(year) %>% 
   top_n(1, average_playtime) %>% 
   mutate(label = paste0(game, " - ", average_playtime, " minutes"))
   
# Plot: playtime by year ----------------------------------------------------------------

filter(games, !is.na(year), year >= 2010, average_playtime > 0) %>% 
ggplot(aes(x = factor(year), y = average_playtime, col = factor(year))) +
   geom_point(alpha = 0.8, show.legend = FALSE) +
   geom_label_repel(data = top_game_by_year, 
                    mapping = aes(x = factor(year),
                                  y = average_playtime, 
                                  label = label), 
                    cex = 3.2,
                    fill = "transparent",
                    segment.alpha = 0.7,
                    box.padding = 0.1,
                    label.padding = 0.15, 
                    point.padding = .2,
                    direction = "y",
                    nudge_y = 20,
                    min.segment.length = unit(0, 'lines'),
                    show.legend = FALSE) +
   labs(
      title = "Steam Spy Games: Average Playtime (July 1 - 15, 2019)",
      subtitle = "Older games' popularity persists over time with dedicated fans",
      x = "Release Year",
      y = "Average Playtime",
      caption = "Visualization by @carstenstann | Data: Steam Spy"
   ) + 
   theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank()
   )

ggsave("./README_figs/VideoGames.png", width = 36, height = 18, dpi = 320, units = "cm")
