#########################################################################################
## Project: TidyTuesday
## Script purpose: Moore's Law, Week 36
## Date: 09.09.2019
## Author: Carsten Stann
#########################################################################################

library(tidyverse)
library(broom)
library(ggtext)
library(wesanderson)

source("./theme/theme_nasa.R")

# import --------------------------------------------------------------------------------
cpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv")
gpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/gpu.csv")
ram <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/ram.csv")

cpu 

# linear model --------------------------------------------------------------------------
log_cpu <- cpu %>% 
   mutate(log_transistor = log(transistor_count, base = 2),
          date = date_of_introduction) %>% 
   filter(date >= 1970)

model <- lm(log_transistor ~ date, data = log_cpu) %>% 
   augment() %>% 
   mutate(color = ifelse(log_transistor < .fitted, 1, 0) %>% factor(labels = c("under-estimated", "over-estimated"))) 

ggplot(model, aes(x = date, y = log_transistor, color = fct_rev(color))) +
   geom_smooth(method = "lm", se = FALSE, col = "lightgrey") +
   geom_segment(aes(xend = date, yend = .fitted), alpha = 0.3) +  # alpha 
   geom_point() +
   geom_point(aes(x = date, y = .fitted), color = "black", alpha = 0.4, shape = 1) +
   geom_text(aes(label = "CPU", x = 2005, y = log(2^12, base = 2)), 
             color = "white",
             family = "Source Sans Pro Semibold",
             size = 38,
             hjust = 0) +
   geom_richtext(aes(label = "The simple log-linear model log<sub>2</sub>*transistors* = \u03B1 + \u03B2*year* <br>yields a coefficient of 0.49, remarkably close to <br>Moore's theoretical value of 0.5", 
                 x = 1970, 
                 y = log(2^32, base = 2)),
                 hjust = 0,
                 fill = NA,
                 label.color = NA, 
                 color = "white"
   ) +
   scale_color_manual(values = c("#02bfe7", "#dd361c")) +
   scale_y_continuous(breaks = seq(10, 35, by = 2),
                      labels = parse(text = glue::glue("2^{seq(10, 35, by = 2)}"))) +
   labs(title = "Moore's Law",
        subtitle = "In 1965, Gordon Moore predicted that the number of transistors in dense integrated circuits doubles about every two years. \nThus far, his prediction has stood the test of time.",
        x = NULL,
        y = "Number of transistors",
        color = "Residuals") +
   theme(plot.title = element_text(size = 32),
         plot.subtitle = element_text(size = 14),
         legend.title = element_text(hjust = 0),
         axis.text = element_text(size = 12))

ggsave("./README_figs/2019_36_MooresLaw.png", width = 12, height = 7, dpi = 320)

