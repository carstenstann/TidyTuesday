# Carsten Stann
# Tidy Tuesday: 2019-07-23 FAA Wildlife Strike Data base
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-07-23

library(tidyverse)
library(skimr)

# import --------------------------------------------------------------------------------

wildlife_impacts <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv")

# summarise -----------------------------------------------------------------------------

# reported wildlife strikes since 1990 
# data for American Airlines, Delta, Southwest and United (~70% of US passengers)
wildlife_impacts

summary(wildlife_impacts)

skim(wildlife_impacts)

# cleaning ------------------------------------------------------------------------------

impacts <- wildlife_impacts %>% 
   mutate(damage = factor(damage),
          operator = factor(operator), 
          incident_month = factor(incident_month),
          phase_of_flt = tolower(phase_of_flt) %>% factor() %>% 
             fct_recode(NULL = "unknown",
                        "local" = "departure", 
                        "local" = "arrival", 
                        "local" = "approach",
                        "local" = "climb",
                        "local" = "descent",
                        "take off" = "take-off run") %>% 
             fct_relevel("parked", 
                         "taxi", 
                         "take off", 
                         "local",
                         "en route",
                         "landing roll"
               ),
          repair_buckets = case_when(
             cost_repairs_infl_adj < 1000 ~ "< 1000",
             cost_repairs_infl_adj < 100000 ~ "< 100,000",
             cost_repairs_infl_adj < 1000000 ~ "< 1,000,000",
             cost_repairs_infl_adj > 10000000 ~ "> 10,000,000"
          ),
          repair_buckets = fct_relevel(repair_buckets, 
                                       "< 1000", 
                                       "< 100,000", 
                                       "< 1,000,000", 
                                       "> 10,000,000")
          )

# exploratory plots ---------------------------------------------------------------------

ggplot(impacts, aes(x = incident_year, fill = phase_of_flt)) +
   geom_bar(position = "fill") 

ggplot(impacts, aes(x = damage)) +
   geom_bar()


ggplot(impacts, aes(x = incident_month)) +
   geom_bar()

