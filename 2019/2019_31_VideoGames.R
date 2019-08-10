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
source("./Theme/theme_nasa.R")

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
   filter(!is.na(year)) %>% 
   group_by(year) %>% 
   top_n(1, average_playtime) %>% 
   mutate(label = paste0(game, " (", year, "): ", average_playtime, " minutes"))
   
# Plot: playtime by year ----------------------------------------------------------------
filter(games, !is.na(year), average_playtime > 0) %>% 
ggplot(aes(x = factor(year), y = average_playtime, col = factor(year))) +
   geom_point(alpha = 0.5) +
   geom_label_repel(data = top_game_by_year, 
                    mapping = aes(x = factor(year),
                                  y = average_playtime, 
                                  label = label), 
                    cex = 3,
                    box.padding = 0.25,
                    label.padding = 0.25, 
                    point.padding = 0.5,
                    direction = "x",
                    min.segment.length = unit(0, 'lines'),
                    show.legend = FALSE) +
   scale_y_continuous(expand = c(0,0), limits = c(-50, NA)) +
   labs(x = "Release Year",
        y = "Average Playtime",
        title = "Steam Spy PC Games By Release Year",
        subtitle = "Average Playtime From July 1 - 15, 2019",
        caption = "Visualization by @carstenstann") +
   coord_flip() +
   theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank()
   )
      
title <- ggplot(data.frame(1:2, y = 1:10)) +
   labs(x = NULL,
        y = NULL,
        title = "Steam Spy PC Games By Release Year: Average Playtime From July 1 - 15, 2019") +
   theme(line = element_blank(),
         plot.background = element_rect(fill = "transparent", color = "transparent"),
         panel.background = element_rect(fill = "transparent"),
         panel.border = element_rect(color = "transparent"),
         axis.text = element_blank())
   
   
caption <- ggplot(data.frame(x = 1:2, y = 1:10)) +
   labs(x = NULL, y = NULL,
        caption = "Visualization by @carstenstann | Data: Steam Spy")
   

title + playtime_by_year + caption + plot_layout(widths = c(0, 1, 0), nrow = 1)




