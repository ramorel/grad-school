library(tidyverse)
library(tidycensus)
library(gganimate)
library(sf)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

## Requires pre-cleaned data from NYS Department of Education, sorry to have to hard code this!
load("/Users/rap168/Box Sync (RichardPaquinMorel2013@u.northwestern.edu)/Paquin-Morel Lab/dissertation_study_3/databases/ny_cleaned_data181120.RData")

## Find county-level means by year
partic_means <-
  nydata %>% 
  group_by(county_name, year) %>% 
  summarize(
    mean_ela = mean(ela_all_students_per_partic, na.rm = T),
    mean_math = mean(math_all_students_per_partic, na.rm = T),
    mean_overall = mean(c(ela_all_students_per_partic, math_all_students_per_partic), na.rm = T)
  )

ny_counties <-
  get_acs(
    state = "NY",
    geography = "county",
    variables = "B19013_001",
    geometry = TRUE
  ) %>%
  mutate(NAME = toupper(str_remove(NAME, " County, New York"))) %>% 
  mutate(NAME = str_replace(NAME, "ST\\.", "SAINT"))

ny_cnty_partic <-
  ny_counties %>%
  left_join(partic_means, by = c("NAME" = "county_name"))

p <-
  ny_cnty_partic %>% 
  ggplot() +
  geom_sf(aes(fill = mean_overall)) +
  scale_fill_viridis_c(name = "Mean Participation Rate", option = "plasma") +
  theme_void() +
  labs(title = "Mean Participation Rate", subtitle = "Year: {current_frame}") +
  transition_manual(year)

animate(p, fps = 5)

anim_save("ny_cont.gif")

## Below X% categorical animation

cate_partic <-
  partic_means %>% 
  mutate(
    cate = 
      cut(
        mean_overall, 
        breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 1), 
        labels = c("Below 50%","50-60%", "60-70%", "70-80%", "80-90%", "90-95%", "Above 95%")
        )
    ) %>% 
  mutate(
    cate = forcats::fct_rev(cate)
  )

ny_cnty_cate <-
  ny_counties %>%
  left_join(cate_partic, by = c("NAME" = "county_name"))

p <- 
  ny_cnty_cate %>% 
  ggplot() +
  geom_sf(aes(fill = cate)) +
  scale_fill_viridis_d(name = "Mean Participation Rate", option = "plasma", direction = -1) +
  theme_void() +
  labs(title = "Mean Participation Rate", subtitle = "Year: {current_frame}") +
  transition_manual(year) 

animate(p, fps = 5)

anim_save("ny_cate.gif")


cate_partic <-
  partic_means %>% 
  mutate(
    cate = 
      ifelse(mean_overall < 0.95, "Below 95%", "95% or above")
  ) 

ny_cnty_cate <-
  ny_counties %>%
  left_join(cate_partic, by = c("NAME" = "county_name"))

p <- 
  ny_cnty_cate %>% 
  ggplot() +
  geom_sf(aes(fill = cate)) +
  scale_fill_viridis_d(name = "Mean Participation Rate", option = "plasma", direction = -1) +
  theme_void() +
  labs(title = "Mean Participation Rate", subtitle = "Year: {current_frame}") +
  transition_manual(year) 

animate(p, fps = 5)

anim_save("ny_cate2.gif")