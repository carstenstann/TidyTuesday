# Carsten Stann 
# Tidy Tuesday Video Games: 2019-07-23
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-07-30

library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)
library(patchwork)

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



# filter for highest average playtime in each year for data labels ----------------------

top_game_by_year <- games %>% 
   filter(!is.na(year)) %>% 
   group_by(year) %>% 
   top_n(1, average_playtime) %>% 
   mutate(label = paste0(game, " (", year, "): ", average_playtime, " minutes"))
   
# custom theme --------------------------------------------------------------------------
# based on NASA Design: https://nasa.github.io/nasawds-site/components/colors/

my_theme <- theme(
   rect = element_rect(fill = "#323a45", 
                       colour = "#f1f1f1", 
                       size = 0.4, 
                       linetype = 1),
   text = element_text(family = "Source Sans Pro Semibold", 
                       face = "plain", 
                       colour = "white", 
                       size = 10, 
                       lineheight = 0.9, 
                       hjust = 0.5, 
                       vjust = 0.5, 
                       angle = 0, 
                       margin = margin(),  
                       debug = FALSE),
   axis.line = element_blank(), 
   axis.line.x = NULL, 
   axis.line.y = NULL,
   axis.text = element_text(size = 10, 
                            colour = "#d6d7d9"),
   axis.ticks = element_line(colour = "#d6d7d9", 
                             size = 0.3),
   axis.title = element_text(margin = unit(c(3.5, 0, 0, 0), "mm"), 
                               vjust = 1, 
                               size = 12, 
                               face = "bold"),
   legend.background = element_rect(colour = NA),
   legend.text = element_text(size = rel(0.9)), 
   legend.title = element_text(size = 12), 
   legend.position = "right", 
   panel.background = element_rect(fill = NA, 
                                   colour = NA), 
   panel.border = element_rect(colour = "#d6d7d9", 
                               fill = NA, 
                               size = rel(1)),
   panel.grid = element_blank(),
   panel.grid.major = element_line(colour = "transparent"), 
   panel.grid.minor = element_line(colour = "transparent"), 
   strip.background = element_rect(fill = "#d6d7d9", 
                                   colour = "#d6d7d9"), 
   strip.text = element_text(colour = "white", 
                             size = 12, 
                             face = "bold"),
   plot.background = element_rect(colour = NA), 
   plot.title = element_text(size = 20, 
                             face = "bold"),
   plot.subtitle = element_text(size = 16), 
   plot.caption = element_text(size = rel(0.9), 
                               color = "white")
)

# Plot: playtime by year ----------------------------------------------------------------
filter(games, !is.na(year), average_playtime > 0) %>% 
ggplot(aes(x = factor(year), y = average_playtime, col = factor(year))) +
   geom_point(alpha = 0.4) +
   geom_label_repel(
      data = top_game_by_year, 
                    aes(
                       x = factor(year), 
                       y = average_playtime, 
                       label = label
                     ), 
                    cex = 3,
                    box.padding = 0.25,
                    label.padding = 0.25, 
                    point.padding = 0.5,
                    direction = "x",
                    min.segment.length = unit(0, 'lines')) +
   coord_flip() +
   labs(x = "Release Year",
        y = "Average Playtime",
        title = "Steam Spy PC Games By Release Year",
        subtitle = "Average Playtime From July 1 - 15, 2019") +
   my_theme
      
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
        caption = "Visualization by @carstenstann | Data: Steam Spy") +
   theme(line = element_blank(),
         plot.background = element_rect(fill = "transparent", color = "transparent"),
         panel.background = element_rect(fill = "transparent"),
         panel.border = element_rect(color = "transparent"),
         axis.text = element_blank())

title + playtime_by_year + caption + plot_layout(widths = c(0, 1, 0), nrow = 1)




