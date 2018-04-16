library(tidyverse)
library(timetk)
library(xts)
library(tibbletime)
library(opencage)
library(lubridate)
library(gganimate)

sibling_movements <- read_csv("data-raw/sibling-movements.csv")
siblings <- read_csv("data-raw/siblings.csv")

# Geocode
sibling_locations <- sibling_movements %>% 
  select(place) %>% 
  distinct(place) %>% 
  rowid_to_column("id") %>% 
  oc_forward_df(placename = place) %>% 
  select(-formatted)

write_csv(sibling_locations, "data/sibling-locations.csv")
sibling_locations <- read_csv("data/sibling-locations.csv")

# Subset of Van der Meulens
vdms <- c("Andries van der Meulen", "Daniel van der Meulen", "Sara van der Meulen", "Anna van der Meulen")
filter(sibling_movements, person %in% vdms)

# Create numeric ids for place
sibling_movements_numeric <- sibling_movements %>% 
  left_join(sibling_locations, by = "place")

# Function to fill dates
fill_dates <- function(x) {
  enquo(x)
  person_xts <- sibling_movements_numeric %>% 
    filter(person == !!x) %>% 
    select(date, id) %>% 
    tk_xts(date_var = date)
  
  xts_fill <- na.locf(merge(person_xts, 
                            seq(min(sibling_movements$date), 
                                max(sibling_movements$date), by = 1)))
  
  tk_tbl(xts_fill, rename_index = "date") %>% 
    as_tbl_time(index = date) %>% 
    as_period(period = "1 month") %>% 
    as_tibble() %>% 
    left_join(sibling_locations, by = "id") %>% 
    mutate(person = !!x) %>% 
    select(person, place, date, lat, lng)
}

sibling_monthly <- map(vdms, fill_dates) %>% 
  bind_rows() %>% 
  mutate(month = paste(month(date, label = TRUE), year(date))) %>% 
  mutate(month = as_factor(month))


# Plots
library(sf)
library(rnaturalearth)

countries_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(sovereignt, continent)

ggplot() + 
  geom_sf(data = countries_sf, fill = gray(0.75), color = gray(0.75)) + 
  coord_sf(xlim = c(-1, 11), ylim = c(50, 55), datum = NA) + 
  geom_point(data = sibling_monthly,
             aes(x = lng, y = lat, color = person, frame = month), 
             size = 4, alpha = 0.8, position = position_dodge(width = 0.2)) +
  labs(x = NULL, y = NULL, color = NULL) + 
  theme_minimal()

ggsave("sibling-movements.png", width = 6, height = 5)

# Function for plot
make_plot <- function(date_period) {
  enquo(date_period)
  ggplot() + 
  geom_sf(data = countries_sf, fill = gray(0.75), color = gray(0.75)) + 
  coord_sf(xlim = c(-1, 11), ylim = c(50, 55), datum = NA) + 
  geom_point(data = filter(sibling_monthly, month == !!date_period),
             aes(x = lng, y = lat, color = person), 
             size = 3, position = position_dodge(width = 0.2)) +
  annotate("text", x = 10, y = 50, label = date_period, size = 5) + 
  labs(x = NULL, y = NULL, color = NULL) + 
  theme_minimal()
}

# For loop to make and save plots
p <- vector("list", length(levels(sibling_monthly$month)))

for (i in 1:length(levels(sibling_monthly$month))) {
  p[[i]] <- make_plot(levels(sibling_monthly$month)[i])
  png(paste0("vdm-movements/frame", i, ".png"), width = 1800, height = 1200, res = 300)
  print(p[[i]])
  dev.off()
}

png("try1234.png", width = 1800, height = 1200, res = 300)
print(p)
dev.off()
