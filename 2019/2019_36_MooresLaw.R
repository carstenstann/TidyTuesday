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

cpu_data <- cpu %>% 
   filter(!is.na(transistor_count)) %>% 
   select(date = date_of_introduction, everything()) %>% 
   mutate(quinquennial = case_when(date < 1975 ~ 1972.5,
                                   date < 1980 ~ 1977.5,
                                   date < 1985 ~ 1982.5,
                                   date < 1990 ~ 1987.5,
                                   date < 1995 ~ 1992.5,
                                   date < 2000 ~ 1997.5,
                                   date < 2005 ~ 2002.5,
                                   date < 2010 ~ 2007.5,
                                   date < 2015 ~ 2012.5,
                                   date < 2020 ~ 2017.5)) 

ggplot(cpu_data, aes(x = quinquennial, y = transistor_count, col = factor(quinquennial))) +
   geom_jitter(width = 1.5, alpha = 0.7) +
   geom_boxplot(alpha = 0.0, show.legend = FALSE, col = "white", aes(group = quinquennial)) +
   geom_text(aes(label = "CPU", x = 2000, y = 2^12), 
             color = "white",
             family = "Source Sans Pro Semibold",
             size = 30,
             hjust = 0) +
   scale_y_continuous(trans = "log2",
                      breaks = 2^seq(10, 36, by = 2),
                      labels = parse(text = glue::glue("2^{seq(10, 36, by = 2)}"))) +
   scale_color_manual(values = wes_palette(name = "Zissou1", n = 10, type = "continuous"),
                      labels = c("1970 ~ 1974",
                                 "1975 ~ 1979",
                                 "1980 ~ 1984",
                                 "1985 ~ 1989",
                                 "1990 ~ 1994",
                                 "1995 ~ 1999",
                                 "2000 ~ 2004",
                                 "2005 ~ 2009",
                                 "2010 ~ 2014",
                                 "2015 ~ 2019")) +
   guides(color = guide_legend(override.aes = list(alpha = 1))) +
   labs(title = "Moore's Law",
        subtitle = "The number of transistors in dense integrated \ncircuits doubles about every two years",
        x = NULL,
        y = "Number of transistors",
        color = "Quinquennial\n periods") + 
   theme(legend.title = element_text(hjust = 0))

ggsave("./README_figs/2019_36_MooresLaw.png", width = 8, height = 10, dpi = 320)


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
                 x = 1972.5, 
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

