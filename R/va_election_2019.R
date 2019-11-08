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

# Population
va_election %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = estimate)) + 
  scale_fill_viridis_c() +
  theme_void()

va_election <-
  va_election %>% 
  st_transform(3395)

# Election results
va_election %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = winner)) + 
  scale_fill_manual(values = c("blue", "gray","red")) +
  theme_void()

va_election_cart <- 
  va_election %>% 
  st_transform(3395) %>%  
  cartogram_dorling("estimate")

morph <- tween_sf(va_election, va_election_cart,
                  ease = 'linear',
                  exit = va_election_cart,
                  nframes = 75)

# Let that last frame repeat a few times -- probably a better way to do this
end <- morph %>% filter(.frame == 75)

morph <- 
  morph %>% 
  rbind(end %>% mutate(.frame = 76)) %>% 
  rbind(end %>% mutate(.frame = 77)) %>% 
  rbind(end %>% mutate(.frame = 78)) %>% 
  rbind(end %>% mutate(.frame = 79)) %>% 
  rbind(end %>% mutate(.frame = 80)) %>% 
  rbind(end %>% mutate(.frame = 81)) %>% 
  rbind(end %>% mutate(.frame = 82)) %>% 
  rbind(end %>% mutate(.frame = 83)) %>% 
  rbind(end %>% mutate(.frame = 84))

p <- 
  ggplot() + 
  geom_sf(data = morph, aes(fill = winner, geometry = geometry) , size=0, alpha=0.9) +
  scale_fill_manual(values = c("blue", "gray","red")) +
  theme_void() +
  coord_sf() +
  transition_manual(.frame) 

animate(p, fps = 25)

anim_save("va_election.gif")
