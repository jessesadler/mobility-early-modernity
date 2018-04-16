library(tidyverse)
library(timetk)
library(xts)
library(opencage) # development version
library(lubridate)


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

# Create numeric ids for place to keep this info when converted to xts
sibling_movements_numeric <- sibling_movements %>% 
  left_join(sibling_locations, by = "place")

# Function to fill dates
# movement_df is movement data frame with columns that you want to keep as numbers
# individual is the person to do this for, because dates have to be filled in for
# one person at a time
fill_dates <- function(movement_df, individual) {
  # Convert to xts
  person_xts <- movement_df %>% 
    filter(person == individual) %>% 
    select(date, id) %>% 
    tk_xts(date_var = date)
  
  # Fill in dates
  xts_fill <- na.locf(merge(person_xts, 
                            seq(min(movement_df$date), 
                                max(movement_df$date), by = 1)))
  
  # Convert to tibble, filter one date per month, add back in information
  tk_tbl(xts_fill, rename_index = "date") %>% 
    filter(mday(date) == 1) %>% 
    left_join(sibling_locations, by = "id") %>% 
    mutate(person = individual) %>% 
    select(person, place, date, lat, lng)
}

# Create tibble of location of siblings per month and factor colum with date written out
# Use map to do one person at a time
sibling_monthly <- map(siblings$person, ~fill_dates(sibling_movements_numeric, .)) %>% 
  bind_rows() %>% 
  mutate(month = paste(month(date, label = TRUE), year(date))) %>% 
  mutate(month = as_factor(month))

# How to create subset of sibling_monthly with correct number of factor levels
filter(sibling_monthly, year(date) == 1585) %>% 
  mutate(month = fct_drop(month))

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
  ggplot() + 
  geom_sf(data = countries_sf, fill = gray(0.75), color = gray(0.75)) + 
  coord_sf(xlim = c(-1, 11), ylim = c(50, 55), datum = NA) + 
  geom_point(data = filter(sibling_monthly, month == date_period),
             aes(x = lng, y = lat, color = person), 
             size = 3, position = position_dodge(width = 0.2)) +
  annotate("text", x = 10, y = 50, label = date_period, size = 5) + 
  labs(x = NULL, y = NULL, color = NULL) + 
  theme_minimal()
}

# For loop to make and save plots
# Change save location by adding directory before "frame"
p <- vector("list", length(levels(sibling_monthly$month)))

for (i in 1:length(levels(sibling_monthly$month))) {
  p[[i]] <- make_plot(levels(sibling_monthly$month)[i])
  png(paste0("frame-", 
             str_pad(i, width = 3, side = "left", pad = "0"),
             ".png"),
      width = 1800, height = 1200, res = 300)
  print(p[[i]])
  dev.off()
}

# Save a single png
png("try1234.png", width = 1800, height = 1200, res = 300)
print(p)
dev.off()
