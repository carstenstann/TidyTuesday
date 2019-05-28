library(tidyverse)
library(skimr)
library(ggridges)
library(wesanderson)
library(countrycode)

## import -------------------------------------------------------------------------------
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv") 

glimpse(wine_ratings)
skim(wine_ratings)

# add continent -------------------------------------------------------------------------
wine_ratings <- wine_ratings %>% 
   mutate(continent = countrycode(country, 
                                  origin = "country.name", 
                                  destination = "continent", 
                                  warn = FALSE))

# filter for 4 red and white wines ------------------------------------------------------
top_wines <- wine_ratings %>% 
   filter(variety %in% c("Bordeaux-style Red Blend",
                         "Cabernet Sauvignon",
                         "Pinot Noir",
                         "Merlot",
                         "Chardonnay",
                         "Sauvignon Blanc",
                         "Pinot Grigio",
                         "Riesling")) %>% 
   drop_na(continent) %>%
   filter(continent != "Oceania") %>%
   mutate(variety = factor(variety,
                           levels = c("Bordeaux-style Red Blend",
                                      "Cabernet Sauvignon",
                                      "Pinot Noir",
                                      "Merlot",
                                      "Chardonnay",
                                      "Sauvignon Blanc",
                                      "Pinot Grigio",
                                      "Riesling")))

# plot data -----------------------------------------------------------------------------
ggplot(top_wines, aes(x = points, y = continent)) + 
   geom_density_ridges(aes(fill = continent, color = continent), 
                       alpha = 0.8, 
                       show.legend = FALSE) + 
   facet_wrap(~variety, ncol = 2, dir = "v") + 
   scale_fill_manual(values = wes_palette("Royal1", n = 4), 
                     aesthetics = c("fill", "color")) +
   labs(title = "Wine Ratings by Continent and Variety",
        x = "Score",
        y = NULL,
        caption = "WineEnthusiast ratings, source: kaggle.com") +
   theme(plot.title = element_text(size = 14),
         axis.title.x = element_text(size = 10),
         plot.caption = element_text(size = 6),
         axis.line.y = element_blank(), 
         axis.line.x = element_line(color = "white"),
         axis.ticks.y = element_blank(),
         panel.grid = element_blank(),
         text = element_text(color = "white"),
         axis.text = element_text(color = "white"),
         axis.ticks = element_line(color = "white"),
         strip.background = element_rect(fill = "#1a1a1a"),
         strip.text = element_text(color = "white", size = "10"),
         plot.background = element_rect(fill = "#1a1a1a"),
         panel.background = element_rect(fill = "#1a1a1a"))
