#map demo 3 insipred by https://github.com/reinholdsson/swemaps/blob/master/README.md
library(tidyverse)
library(swemaps)
library(leaflet)  # devtools::install_github("rstudio/leaflet")

map_data <- map_kn


map_data <- map_data %>%
  mutate(Code_map = substr(knkod, 1, 2))


map_object_for_plot <- leaflet() %>% addTiles()

for (kn in unique(map_data$knkod)) {
  map_data_i <- map_data[map_data$knkod == kn,]
  map_object_for_plot <- map_object_for_plot %>% addPolygons(map_data_i$leaflet_long, map_data_i$leaflet_lat, color = 'blue', weight = 1)
}

map_object_for_plot  # plot!


####Make sure to change value and then try to create a continious color scale according to the below example and implement it as in the second example below

# Create a continuous palette function #from https://rstudio.github.io/leaflet/colors.html
pal <- colorNumeric(
  palette = "Blues",
  domain = countries$gdp_md_est)

# Apply the function to provide RGB colors to addPolygons
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(gdp_md_est))