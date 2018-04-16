library(tidyverse)
library(ggrepel)
library(sf)
library(rnaturalearth)
library(opencage)

places <- tibble(place = c("London", "Antwerp", "Hamburg", "Verona")) %>% 
  oc_forward_df(placename = place)

countries_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(sovereignt, continent)

ggplot() + 
  geom_sf(data = countries_sf, fill = gray(0.75), color = gray(0.75)) + 
  coord_sf(xlim = c(-4, 14), ylim = c(44, 55), datum = NA) + 
  geom_point(data = places, aes(x = lng, y = lat), size = 3) +
  geom_text_repel(data = places, aes(x = lng, y = lat, label = place)) + 
  theme_void()

ggsave("df-trade.png", width = 10, height = 8)
