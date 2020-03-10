#########################################################################################
## Project: TidyTuesday
## Script purpose: MNHL Goals, 2020 Week 10
## Date: 09.09.2019
## Author: Carsten Stann
#########################################################################################

library(tidyverse)
library(ggrepel)
library(wesanderson)
library(ggtext)

# import data ---------------------------------------------------------------------------
game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')
top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')
season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

# examine data --------------------------------------------------------------------------
glimpse(game_goals)
glimpse(top_250)
glimpse(season_goals)

# clean ---------------------------------------------------------------------------------
# calculate cumulative career goals by player
cum_goals <- season_goals %>%
   # filter for only NHL goals
   filter(league == "NHL") %>% 
   # get year in which season started
   separate(season, into = c("season_yr1", "season_yr2"), sep = "-", remove = FALSE) %>% 
   mutate(season_yr1 = as.integer(season_yr1)) %>% 
   group_by(player) %>% 
   # sort by player and season starting year
   arrange(player, season_yr1) %>%   
   ungroup() %>% 
   group_by(player) %>% 
   mutate(cumulative_goals = cumsum(goals))

# plot ----------------------------------------------------------------------------------
ggplot() + 
   # horizontal line to demark 700 goal threshold
   geom_hline(
      yintercept = 700,
      lty = "dashed"
   ) +
   # plot players with < 700 career goals
   geom_line(
      data = filter(cum_goals, max(cumulative_goals) < 700), 
      aes(
         x = season_yr1, 
         y = cumulative_goals, 
         group = player
      ), 
      alpha = 0.9,
      color = "gray"
   ) +
   # plot players with >= 700 career goals
   geom_line(
      data = filter(cum_goals, max(cumulative_goals) >= 700), 
      aes(
         x = season_yr1, 
         y = cumulative_goals, 
         col = player
      ),
      alpha = 1,
      cex = 1,
      show.legend = FALSE
   ) +
   # label players with >= 700 career goals
   geom_label_repel(
      data = filter(cum_goals, cumulative_goals >= 700) %>% 
         group_by(player) %>% 
         slice(n()), 
      mapping = aes(
         x = season_yr1,
         y = cumulative_goals,
         col = player,
         label = paste0(player, ", ", age)
      ), 
      cex = 3.2,
      segment.alpha = 0.7,
      box.padding = 0.1,
      label.padding = 0.15, 
      point.padding = .2,
      direction = "both",
      nudge_y = 20,
      min.segment.length = unit(0, 'lines'),
      show.legend = FALSE
   ) + 
   # set y breaks every 100 goals
   scale_y_continuous(
      breaks = seq(0, max(cum_goals$cumulative_goals), 100),
      minor_breaks = NULL
   ) + 
   scale_color_manual(values = wes_palette(name = "Zissou1", n = 12, type = "continuous")) +
   xlim(1940, 2022) +
   labs(
      title = "Chasing the Great One: Will Alex Ovechkin surpass Wayne Gretzky?",
      subtitle = "NHL players with 700+ cumulative career goals (Player, Age)",
      x = "NHL Season",
      y = "Cumulative Career Goals",
      caption = "Visualization by @carstenstann | Data: HockeyReference.com"
   ) +
   theme_minimal() +
   theme(
      axis.title.y = element_text(
         angle = 90, 
         margin = unit(c(0, 3, 0, 3), "mm"), 
         vjust = 1
      ),
      axis.title.x = element_text(
         margin = unit(c(3, 0, 0, 0), "mm"), 
      ),
      plot.title = element_text(
         size = 16, 
         face = "bold",
         hjust = 0, 
         vjust = 1,
         margin = margin(t = 4, b = 6)
      ),
      plot.subtitle = element_text(
         size = 12,
         hjust = 0, 
         vjust = 1,
         margin = margin(0, 0, 3, 0),
         face = "plain"
      ),
      plot.caption = element_text(
         size = 10, 
         color = "black",
         margin = margin(t = 3),
         hjust = 1, 
         vjust = 1,
         face = "plain"
      )
   )

ggsave("./README_figs/2020_10_NHL_Goals.png", width = 12, height = 7, dpi = 320)
