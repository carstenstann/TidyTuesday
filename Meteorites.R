# Tidy Tuesday Meteorites 

library(tidyverse)
library(maps)

meteor_import <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

meteor <- meteor_import %>%
   drop_na(long, lat) %>%
   filter(between(long, -200, 200) & lat >= -85.1 & year <= 2019)

summary(meteor)

world <- map_data("world")

ggplot(world) +
   geom_polygon(aes(x = long, y = lat, group = group)) +
   geom_point(data = meteor, aes(x = long, y = lat), size = 0.05, alpha = 0.5, color = "#dd361c") +
   ylim(c(-84.9, 83.59961)) +
   labs(title = "Known Meteorite Landings: 860 - 2019", 
        subtitle = "Over 36,000 meteorites have struck earth since 860",
        y = "Latitude",
        x = "Longitute",
        caption = "Data: NASA Open Data Portal") + 
   theme(
      plot.title = element_text(size = 18, 
                                hjust = 0.5, 
                                family = "Source Sans Pro Semibold",
                                margin = margin(.1, 0, .1, 0, unit = "cm")),
      plot.subtitle = element_text(size = 10, 
                                   hjust = 0.5, 
                                   family = "Source Sans Pro Semibold",
                                   margin = margin(.1, 0, .1, 0, unit = "cm")),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor = element_line(color = "#d6d7d9", size = 0.2),
      panel.grid.major = element_line(color = "#d6d7d9", size = 0.2),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "#f1f1f1"), 
      plot.background = element_rect(fill = "#f1f1f1"),
      plot.caption = element_text(size = 8, family = "Source Sans Pro"),
      plot.margin = unit(c(.2, .2, .2, .2), "cm")
   ) 

