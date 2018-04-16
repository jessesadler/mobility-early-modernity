library(tidyverse)
library(timetk)
library(xts)
library(lubridate)
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
peeter_monthly <- filter(peeter_tbl, mday(date) == 1) %>% 
  left_join(peeter_id, by = "id")


# Geocode
peeter_locations <- peeter_movements %>% 
  select(place) %>% 
  distinct(place) %>% 
  oc_forward_df(placename = place)

peeter_locations <- read_csv("data/peeter-locations.csv")

peeter_geo <- left_join(peeter_monthly, peeter_locations, by = "place") %>% 
  select(-id, -formatted) %>% 
  rowid_to_column("frame")

peeter_month <- peeter_geo %>% 
  mutate(month = paste(month(date, label = TRUE), year(date))) %>% 
  mutate(month = as_factor(month))

ggplot() + 
  geom_point(data = peeter_month, aes(x = lng, y = lat, color = year(date)), size = 3) +
  scale_color_viridis_c()

## map
library(sf)
library(rnaturalearth)

countries_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(sovereignt, continent)

ggplot() +
  geom_sf(data = countries_sf, fill = gray(0.75), color = gray(0.75)) +
  geom_point(data = peeter_geo, aes(x = lng, y = lat, color = year(date)), size = 3) +
  scale_color_viridis_c() + 
  coord_sf(xlim = c(-4, 20), ylim = c(48, 62)) +
  theme_minimal()

### Create ggraph of places Peeter visited ###

library(tidygraph)
library(ggraph)

# Create tbl_graph object
peeter_network <- read_csv("data-raw/peeter-janssen-network.csv") %>% 
  mutate(month = paste(month(date, label = TRUE), year(date))) %>% 
  mutate(month = as_factor(month))
nodes <- peeter_locations %>% 
  rowid_to_column("id") %>% 
  rename(x = lng, y = lat)
routes <- tbl_graph(nodes = nodes, edges = peeter_network, directed = TRUE)

# Create list of tbl_graph objects. Each piece of the list has the same number of edges
# as its place in the list
routes_list <- vector("list", nrow(peeter_network))
for (i in 1:nrow(peeter_network)) {
  routes_list[[i]] <- routes %>% 
    activate(edges) %>% 
    filter(between(row_number(), 1, i)) %>% 
    activate(nodes) %>%
    filter(!node_is_isolated())
}

# Function to create plot
make_plot <- function(location, date_period) {
  ggraph(routes_list[[i]], layout = "nicely") + 
    geom_sf(data = countries_sf, fill = gray(0.75), color = gray(0.75)) + 
    coord_sf(xlim = c(-2, 20), ylim = c(49, 60), datum = NA) +
    geom_node_point() + 
    geom_edge_fan(edge_linetype = 3) + 
    geom_node_text(data = filter(nodes, place == location), aes(label = place), repel = TRUE) +
    annotate("text", x = 18, y = 49, label = date_period, size = 5) + 
    theme_graph()
}

# For loop to make and save plots
# Change save location by adding directory before "frame"
p <- vector("list", nrow(peeter_network))

for (i in 1:nrow(peeter_network)) {
  # Get character vector of location to use to filter location for label
  location[[i]] <- peeter_network %>% 
    filter(row_number() == i) %>% 
    select(to) %>% 
    as_vector()
  p[[i]] <- make_plot(location = location[[i]], 
                      date_period = peeter_network$month[[i]])
  png(paste0("frame-", 
             str_pad(i, width = 2, side = "left", pad = "0"),
             ".png"),
      width = 1800, height = 1200, res = 300)
  print(p[[i]])
  dev.off()
}

# First frame with only Stade
frame1 <- ggraph(routes_list[[1]], layout = "nicely") + 
  geom_sf(data = countries_sf, fill = gray(0.75), color = gray(0.75)) + 
  coord_sf(xlim = c(-2, 20), ylim = c(49, 60), datum = NA) +
  geom_node_point(data = filter(nodes, place == "Stade")) + 
  geom_node_text(data = filter(nodes, place == "Stade"), aes(label = place), repel = TRUE) +
  annotate("text", x = 18, y = 49, label = "Jun 1589", size = 5) + 
  theme_graph()

png("frame-00.png",  width = 1800, height = 1200, res = 300)
print(frame1)
dev.off()
