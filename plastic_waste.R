# TidyTuesday: Global Plastic Waste (2019-05-21)
library(readr)
library(janitor)
library(skimr)
library(dplyr)
library(tidyr)
library(countrycode)
library(ggplot2) 
library(ggrepel)
library(grid)

options(scipen = 999)

# import files --------------------------------------------------------------------------
pop_mismanaged_waste <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")
gdp_mismanaged_waste <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
gdp_waste <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

# pop_mismanaged_waste <- read_csv("/Users/Carsten/Documents/GitHub/TidyTuesdaySubmissions/data/pop_vs_waste.csv")
# gdp_mismanaged_waste <- read_csv("/Users/Carsten/Documents/GitHub/TidyTuesdaySubmissions/data/gdp_mismanaged_waste.csv")
# gdp_waste <- read_csv("/Users/Carsten/Documents/GitHub/TidyTuesdaySubmissions/data/gdp_vs_waste.csv")

# clean and join data ----------------------------------------------------------------------------
pop_mismanaged_waste <- clean_names(pop_mismanaged_waste)
gdp_mismanaged_waste <- clean_names(gdp_mismanaged_waste)
gdp_waste <- clean_names(gdp_waste)

# waste data only pertains to 2010
pop_mismanaged_waste %>% filter(!is.na(coastal_population)) %>% skim()
gdp_mismanaged_waste %>% filter(!is.na(per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day)) %>% skim()
gdp_waste %>% filter(!is.na(per_capita_plastic_waste_kilograms_per_person_per_day)) %>% skim()

# join data 
waste_join <- gdp_waste %>% 
   filter(year == 2010) %>%
   left_join(gdp_mismanaged_waste, 
             by = c("entity", "year"), .id = TRUE) %>%
   select(code = code.x, 
          entity, 
          year, 
          total_pop = total_population_gapminder.x,
          gdp_pc = gdp_per_capita_ppp_constant_2011_international_constant_2011_international,
          mismanaged_plastic_waste_kg_pc_pd = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day,
          plastic_waste_kg_pc_pd = per_capita_plastic_waste_kilograms_per_person_per_day
          ) %>%
   left_join(pop_mismanaged_waste,
             by = c("entity", "year"), .id = TRUE) %>%
   select(code = code.x, 
          entity, 
          year, 
          total_pop,
          coastal_pop = coastal_population,
          gdp_pc,
          plastic_waste_kg_pc_pd,
          mismanaged_plastic_waste_kg_pc_pd,
          mismanaged_plastic_waste_tonnes
          ) %>%
   drop_na()

# add continent variable 
waste <- waste_join %>%
   mutate(gdp = gdp_pc * total_pop,
          continent1 = countrycode(entity, origin = "country.name", 
                                   destination = "continent", warn = FALSE),
          continent2 = countrycode(code, origin = "iso3c", 
                                   destination = "continent", warn = FALSE),
          Continent = coalesce(continent1, continent2),
          continent1 = NULL,
          continent2 = NULL,
          total_plastic_waste_kg_pd = plastic_waste_kg_pc_pd * total_pop, # kg/per day
          total_mismanaged_plastic_waste_kg_pd = mismanaged_plastic_waste_kg_pc_pd * total_pop
          ) %>%
   select(code, entity, Continent, year, total_pop, coastal_pop, gdp, gdp_pc,
          plastic_waste_kg_pc_pd, total_plastic_waste_kg_pd, mismanaged_plastic_waste_tonnes,
          mismanaged_plastic_waste_kg_pc_pd, total_mismanaged_plastic_waste_kg_pd)

# plots ---------------------------------------------------------------------------------
glimpse(waste)
# choose countries to label
labs <- filter(waste, entity %in% c("United States",
                                    "China",
                                    "India",
                                    "Germany",
                                    "Japan",
                                    "Sri Lanka",
                                    "Mexico",
                                    "Canada",
                                    "Trinidad and Tobago",
                                    "Nigeria", 
                                    "Haiti"))

# plot for Plastic Waste vs GDP
p1 <- ggplot(waste, aes(x = gdp_pc, y = plastic_waste_kg_pc_pd * 365, label = entity)) +
   geom_point(aes(size = mismanaged_plastic_waste_tonnes, col = Continent)) +
   stat_smooth(geom = "line", 
               color = "blue", 
               alpha = 0.9, 
               method = "lm", 
               show.legend = FALSE) +
   geom_smooth(method = "lm", 
               color = NA, 
               alpha = 0.2, 
               show.legend = FALSE)  + 
   geom_label_repel(data = labs,
                    size = 2.5,
                    point.padding = 0.2,
                    box.padding = 0.4,
                    segment.size = 0.3,
                    segment.color = "grey50",
                    direction = "both") +
   scale_y_log10()+
   scale_x_log10() +
   labs(x = "GDP per capita", 
        y = "Total Plastic Waste per capita (kg/year)",
        title = "Global Plastic Waste in 2010",
        subtitle = "Wealthier countries produce more plastic waste per capita",
        size = "Mismanaged Plastic Waste \n(tons)") +
   theme(panel.background = element_blank(),
         panel.grid.major = element_line(size = 0.2, 
                                         linetype = 'solid',
                                         colour = "light gray"),
         panel.grid.minor = element_line(size = 0.1, 
                                         linetype = 'solid',
                                         colour = "light gray"),
         legend.position = "right", 
         axis.line.x.bottom = element_line(colour = "black"),
         axis.line.y.left = element_line(colour = "black")) +
   guides(col = guide_legend(override.aes = list(size = 2.5)),
          size = guide_legend(override.aes = list()))

# plot for Mismanaged Waste vs GDP
p2 <- ggplot(waste, aes(x = gdp_pc, y = mismanaged_plastic_waste_kg_pc_pd * 365, label = entity)) +
   geom_point(aes(size = mismanaged_plastic_waste_tonnes, col = Continent)) +
   stat_smooth(geom = "line", 
               color = "blue", 
               alpha = 0.9, 
               method = "loess", 
               show.legend = FALSE) +
   geom_smooth(method = "loess", 
               color = NA, 
               alpha = 0.2, 
               show.legend = FALSE) + 
   geom_label_repel(data = labs,
                    size = 2.5,
                    point.padding = 0.2,
                    box.padding = 0.4,
                    segment.size = 0.3,
                    segment.color = "grey50",
                    direction = "both") +
   scale_y_log10() +
   scale_x_log10() +
   labs(x = "GDP per capita", 
        y = "Mismanaged Plastic Waste per capita (kg/year)",
        title = "Global Mismanaged Plastic Waste in 2010",
        subtitle = "Wealthier countries are better at managing plastic waste",
        size = "Mismanaged Plastic Waste \n(tons)") +
   theme(panel.background = element_blank(),
         panel.grid.major = element_line(size = 0.2, 
                                         linetype = "solid",
                                         colour = "light gray"),
         panel.grid.minor = element_line(size = 0.1, 
                                         linetype = "solid",
                                         colour = "light gray"),
         legend.position = "right", 
         axis.line.x.bottom = element_line(colour = "black"),
         axis.line.y.left = element_line(colour = "black")) +
   guides(col = guide_legend(override.aes = list(size = 2.5)),
          size = guide_legend(override.aes = list()))

ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom")
