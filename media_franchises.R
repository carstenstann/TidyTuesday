# Tidy Tuesday 2019-07-02
# Media Franchise Powerhouses

library(tidyverse)
library(cowplot)
library(skimr)
library(ggthemes)


# import --------------------------------------------------------------------------------
media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

# summarise -----------------------------------------------------------------------------
skim(media_franchises)


# plot theme ----------------------------------------------------------------------------

my_theme <- theme_wsj() +
   theme(text = element_text(family = "mono"), 
         title = element_text(size = 10),
         plot.title = element_text(size = 12),
         plot.subtitle = element_text(size = 10, face = "plain"),
         axis.text = element_text(size = 8.5, family = "mono", face = "plain"),
         legend.text = element_text(size = 8),
         legend.position = "bottom",
         legend.margin = margin(0,0,0,0, "cm"),
         panel.grid.major.x = element_blank(),
         panel.grid.major.y = element_line(size = .2),
         panel.grid.minor.x = element_blank(),
         plot.caption = element_text(hjust = 1, face = "italic")
   )

# media revenues ------------------------------------------------------------------------

rev_cat <- ggplot(media_franchises, aes(x = reorder(revenue_category, revenue, sum), y = revenue)) + 
   geom_col() +
   coord_flip() +
   labs(title = "Media revenues by category", x = NULL, y = "Revenue (billions)") +
   my_theme

media_type <- ggplot(media_franchises, aes(x = reorder(original_media, revenue, sum), y = revenue)) +
   geom_col() +
   coord_flip() +
   labs(title = "Media Revenues by Type", 
        x = NULL, 
        y = "Revenue (billions)") +
   my_theme

# top franchises by revenue -------------------------------------------------------------

top_franchises <- media_franchises %>% 
   group_by(franchise) %>% 
   summarise(total_revenue = sum(revenue, na.rm = TRUE)) %>% 
   top_n(10, total_revenue) %>% 
   pull(franchise)

top_franchise_rev <- media_franchises %>% 
   filter(franchise %in% top_franchises)

top_franchises <- ggplot(top_franchise_rev, 
       aes(x = reorder(franchise, revenue, sum), y = revenue)) +
   geom_col() +
   coord_flip() +
   labs(title = "Media Franchises: Revenues by category",
        x = "Franchise", 
        y = "Revenues (billions)") +
   my_theme

# Plot grid -----------------------------------------------------------------------------

plot_grid(top_franchises, plot_grid(rev_cat, media_type), ncol = 1)


plot_grid(coefs, plot_grid(num_reviews, density), ncol = 1, rel_heights = c(1.45, 1))
