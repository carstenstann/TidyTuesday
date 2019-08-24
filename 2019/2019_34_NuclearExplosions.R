# Carsten Stann 
# Tidy Tuesday Nuclear Explosions: 2019-08-20
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-08-20

library(tidyverse)
library(skimr)
library(lubridate)
library(wesanderson)

# import --------------------------------------------------------------------------------
nuclear_explosions <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

nuclear_explosions
skim(nuclear_explosions)

# clean ---------------------------------------------------------------------------------

nuclear <- nuclear_explosions %>% 
   mutate(date = ymd(date_long),
          country = factor(country, 
                           levels = c("CHINA", "FRANCE", "INDIA", "PAKIST", "UK", "USA", "USSR"),
                           labels = c("China", "France", "India", "Pakistan", "UK", "USA", "USSR"))
   ) %>% 
   select(-date_long) %>% 
   group_by(year, country) %>% 
   summarise(total_yield = max(yield_upper, na.rm = T),
             total = n())

nuclear

ggplot(nuclear, aes(x = year, y = total, col = country)) +
   geom_vline(xintercept = 1963, colour = "grey40", size = .5) +
   geom_vline(xintercept = 1996, colour = "grey40", size = .5) +
   annotate("text", x = 1974, y = 70, label = "Partial Test Ban Treaty (1963)", size = 4, fontface = 2) +
   geom_curve(aes(x = 1974, y = 68, xend = 1964, yend = 62.5),
              size = .3,
              color = "grey40",
              arrow = arrow(length = unit(0.02, "npc")),
              curvature = -0.25,
              inherit.aes = FALSE,
              show.legend = FALSE
   ) +
   annotate("text", x = 1986, y = 50, label = "Comprehensive Nuclear Test \nBan Treaty (1996)", size = 4, fontface = 2) +
   geom_curve(aes(x = 1986, y = 53.5, xend = 1995, yend = 59),
             size = 0.3,
             color = "grey40",
             arrow = arrow(length = unit(0.02, "npc")),
             curvature = -0.25,
             inherit.aes = FALSE,
             show.legend = FALSE
   ) +
   geom_jitter(aes(size = total_yield)) +
   scale_color_manual(values = wes_palette(name = "Rushmore1", n = 7, type = "continuous")) +
   guides(color = guide_legend(ncol = 7, 
                               label.position = "bottom", 
                               override.aes = list(shape = 16, size = 15)),
          size = guide_legend(ncol = 6, 
                              title.position = "left", 
                              label.position = "bottom")
   ) +
   scale_x_continuous(limits = c(1945, 1999), 
                      expand = c(0.02, 0),
                      breaks = seq(1945, 1999, 5),
                      minor_breaks = NULL
   ) +
   scale_y_continuous(limits = c(-1.5, 84),
                      expand = c(0, 0),
                      breaks = seq(0, 100, 20),
                      minor_breaks = seq(0, 100, 5),
                      labels = abs(seq(0, 100, 20))
   ) +
   scale_size(range = c(1, 15)) +
   labs(title = "Nuclear explosions, 1945 - 1998",
        subtitle = "Total number of explosions grouped by year and country \nwhere size reflects the largest annual explosion yield estimate",
        x = NULL, 
        y = "Number of Explosions", 
        col = NULL, 
        size = "Kilotons",
        caption = "Visualization by @carstenstann | Data: Stockholm International Peace Research Institute"
   )+ 
   theme_minimal() +
   theme(axis.title.y = element_text(angle = 90, 
                                    margin = unit(c(0, 3.5, 0, 3), "mm"), 
                                    vjust = 1),
         legend.position = "bottom",
         legend.box = "horizontal",
         legend.margin = margin(0.1, 0.5, 0.15, 0, unit = "cm"),
         plot.title = element_text(size = 24, 
                                   face = "bold",
                                   hjust = 0, 
                                   vjust = 1,
                                   margin = margin(t = 4, b = 6)),
         plot.subtitle = element_text(size = 20,
                                      hjust = 0, 
                                      vjust = 1,
                                      margin = margin(0, 0, 10, 0),
                                      face = "plain"),
         plot.caption = element_text(size = 10, 
                                    color = "black",
                                    margin = margin(t = 10),
                                    hjust = 1, 
                                    vjust = 1,
                                    face = "plain"))

ggsave("./README_figs/NuclearExplosions.png", width = 12, height = 9, dpi = 320)


