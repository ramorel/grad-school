library(tidyverse)
library(tidycensus)
library(sf)
library(gganimate)
library(cartogram)
library(rvest)
library(transformr)

options(tigris_use_cache = TRUE)


va_election_dat <- 
  read_html("https://results.elections.virginia.gov/vaelections/2019%20November%20General/Site/Locality/Index.html") %>% 
  html_nodes("a") %>%
  html_attr("href") %>% 
  str_subset("COUNTY|CITY") %>% 
  str_replace("\\.", "http://results.elections.virginia.gov/vaelections/2019 November General/Json/Locality") %>% 
  str_replace("html", "json") %>% 
  map(
    ~ .x %>% 
      jsonlite::fromJSON() %>% 
      flatten()
  )

county <- map(va_election_dat, "LocalityName")
race_name <- map(va_election_dat, "RaceName")

va_election <-
  va_election_dat %>% 
  map(
    ~ map2(.x$Candidates, .x$RaceName, function(y, z) mutate(y, race_name = z)) %>% 
      bind_rows()
  ) %>% 
  map2(county, function(x, y) mutate(x, county = y)) %>% 
  map(~ filter(.x, str_detect(race_name, "Senate"))) %>% 
  bind_rows() %>% 
  select(Votes:last_col()) %>% 
  as_tibble() %>% 
  pivot_wider(id_cols = c(-Votes, -PoliticalParty, -Percentage), names_from = PoliticalParty, values_from = Votes) %>% 
  group_by(county) %>%
  summarize_at(vars(Republican:Libertarian), sum, na.rm = TRUE) %>% 
  pivot_longer(cols = -county) %>% 
  group_by(county) %>% 
  summarize(winner = name[which(value == max(value, na.rm = TRUE))]) %>% 
  ungroup() %>% 
  mutate(county = str_replace(county, "\\&", "AND"))

va_census <- 
  get_acs(geography = "county", state = "VA", variables = "B01001_001", geometry = TRUE) %>% 
  mutate(NAME = str_to_upper(str_remove(NAME, ", .+$")))

va_election <- 
  left_join(va_census, va_election, by = c("NAME" = "county"))


va_election %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = estimate)) + 
  scale_fill_viridis_c() +
  theme_void()

va_election <-
  va_election %>% 
  st_transform(3395)

va_election %>% 
  st_transform(3395) %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = winner)) + 
  scale_fill_manual(values = c("blue", "gray","red")) +
  theme_void()

va_election_cart <- 
  va_election %>% 
  st_transform(3395) %>%  
  cartogram_dorling("estimate")

va_election_cart$id <- seq(1,nrow(va_election_cart))
va_election$id <- seq(1,nrow(va_election))

data <- rbind(va_election, va_election_cart, va_election)

data$ease <- "cubic-in-out"
data$time <- rep(c(1:3), each=nrow(va_election))

dt <- tween_elements(data, time='time', group='id', ease='ease', nframes = 30)

morph <- tween_sf(va_election, va_election_cart,
                  ease = 'linear',
                  exit = va_election_cart,
                  nframes = 30)

end <- morph %>% filter(.frame == 30)

morph <- 
  morph %>% 
  rbind(end %>% mutate(.frame = 31)) %>% 
  rbind(end %>% mutate(.frame = 32)) %>% 
  rbind(end %>% mutate(.frame = 33))

p <- 
  ggplot() + 
  geom_sf(data = morph, aes(fill = winner, geometry = geometry) , size=0, alpha=0.9) +
  scale_fill_manual(values = c("blue", "gray","red")) +
  theme_void() +
  coord_sf() +
  transition_manual(.frame) 

animate(p, fps = 30)

anim_save("va_election.gif")