library(tidyverse)
library(timetk)
library(xts)
library(tibbletime)
library(opencage)

peeter_movements <- read_csv("data-raw/peeter-janssen.csv") %>% 
  select(place, date)

peeter_id <- peeter_movements %>% 
  select(place) %>% 
  distinct(place) %>% 
  rowid_to_column("id")

peeter_movements_id <- left_join(peeter_movements, peeter_id, by = "place")


peeter_xts <- tk_xts(peeter_movements_id)
peeter_xts <- na.locf(merge(peeter_xts, seq(min(peeter_movements$date), max(peeter_movements$date), by = 1)))
peeter_tbl <- tk_tbl(peeter_xts, rename_index = "date")
peeter_tbltime <- as_tbl_time(peeter_tbl, index = date)
peeter_monthly <- as_period(peeter_tbltime, period = "1 month") %>% 
  left_join(peeter_id, by = "id") %>% 
  as_tibble()

# Geocode
peeter_locations <- peeter_movements %>% 
  select(place) %>% 
  distinct(place) %>% 
  oc_forward_df(placename = place)

peeter_locations <- read_csv("data/peeter-locations.csv")

peeter_geo <- left_join(peeter_monthly, peeter_locations, by = "place") %>% 
  select(-id, -formatted) %>% 
  rowid_to_column("frame")

p <- ggplot() + 
  geom_point(data = peeter_geo, aes(x = lng, y = lat, frame = date), size = 3)

peeter_month <- peeter_geo %>% 
  mutate(month = paste(month(date), year(date)))

p <- ggplot() + 
  geom_point(data = peeter_month, aes(x = lng, y = lat, frame = month), size = 3)

peeter_tween <- peeter_geo %>% 
  select(-place) %>% 
  rename(x = lng, y = lat) %>% 
  rowid_to_column("time") %>% 
  mutate(ease = "linear") %>% 
  tween_elements(time = "time", group = "ease", ease = "ease", nframe = 500)

## map
library(sf)
library(rnaturalearth)

countries_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(sovereignt, continent)

peeter_map <- ggplot() +
  geom_sf(data = countries_sf, fill = gray(0.7), color = gray(0.7)) +
  geom_point(data = peeter_geo, aes(x = lng, y = lat, frame = date), size = 3) +
  coord_sf(xlim = c(-4, 20), ylim = c(48, 62)) +
  theme_minimal()


library(tidygraph)
library(ggraph)
library(lubridate)

peeter_network <- read_csv("data-raw/peeter-janssen-network.csv")
edges <- mutate(peeter_network, month = paste(month(date, label = TRUE), year(date)))
nodes <- rename(peeter_locations, x = lng, y = lat)
routes <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

pfunc <- function(i) {
  ggraph(routes, layout = "nicely") + 
    geom_node_point() + 
    geom_edge_fan()
}



routes_list <- vector("list", nrow(peeter_network))
for (i in 1:nrow(peeter_network)) {
  routes_list[[i]] <- routes %>% 
    activate(edges) %>% 
    filter(between(row_number(), 1, i)) %>% 
    activate(nodes) %>%
    filter(!node_is_isolated())
}

# First frame
ggraph(routes_list[[1]], layout = "nicely") + 
  geom_sf(data = countries_sf, fill = gray(0.75), color = gray(0.75)) + 
  coord_sf(xlim = c(-2, 20), ylim = c(49, 60), datum = NA) +
  geom_node_point(data = filter(nodes, place == "Stade")) + 
  geom_node_text(data = filter(nodes, place == "Stade"), aes(label = place), repel = TRUE) +
  annotate("text", x = 18, y = 49, label = "Jun 1589", size = 5) + 
  theme_graph()

ggraph(routes_list[[17]], layout = "nicely") + 
  geom_sf(data = countries_sf, fill = gray(0.75), color = gray(0.75)) + 
  coord_sf(xlim = c(-2, 20), ylim = c(49, 60), datum = NA) +
  geom_node_point() + 
  geom_edge_fan(edge_linetype = 3) + 
  geom_node_text(data = filter(nodes, place == "Haarlem"), aes(label = place), repel = TRUE) +
  annotate("text", x = 18, y = 49, label = "Aug 1598", size = 5) + 
  theme_graph()

ggsave("peeterjanssen17.png", width = 6, height = 5)
