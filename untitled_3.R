#map demo 3 insipred by https://github.com/reinholdsson/swemaps/blob/master/README.md
library(tidyverse)
library(swemaps)
library(leaflet)  # devtools::install_github("rstudio/leaflet")

map_data <- map_kn


map_data <- map_data %>%
  mutate(Code_map = substr(knkod, 1, 2))


map_object_for_plot <- leaflet() %>% addTiles()

##### sum yearly money county
yearly_2008_county <- university_yearly_sek_data %>%
  group_by(Code, County, FundingYear) %>%
  summarise(
    yearly_funding_sek_county = sum(yearly_funding_sek)
  ) %>% filter(FundingYear == c(2008)) %>% arrange(yearly_funding_sek_county)

# Define the color palette
color_palette <- colorRampPalette(c("grey", "black"))

# Generate the gradient color scale
gradient_colors <- color_palette(length(yearly_2008_county$yearly_funding_sek_county))

yearly_2008_county <- add_column(yearly_2008_county, color = gradient_colors)

# Check the color scale
plot(1, 1, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE)
for (i in 1:length(yearly_2008_county$yearly_funding_sek_county)) {
  rect((i - 1)/length(yearly_2008_county$yearly_funding_sek_county), 0, i/length(yearly_2008_county$yearly_funding_sek_county), 1, col = gradient_colors[i], border = NA)
}

for (kn in unique(map_data$knkod)) {
  map_data_i <- map_data[map_data$knkod == kn,]
  if (isTRUE(substr(kn, 1, 2) == '04' ||  substr(kn, 1, 2) == '06')) {
    col = "grey"
  } else {
    col = toString(yearly_2008_county[which(yearly_2008_county$Code == substr(kn, 1, 2)), "color"])
  }
  map_object_for_plot <- map_object_for_plot %>% 
    addPolygons(map_data_i$leaflet_long,
                map_data_i$leaflet_lat,
                fillColor = col,
                color = "grey",
                opacity = 100,
                weight = 1)
}


map_object_for_plot  # plot!
rm(map_object_for_plot)

devtools::install_github("reinholdsson/rkolada")
library(rkolada)
library(plotrix)
# function to merge kolada data with map data from swemaps
prepare_map_data <- function(x) {
  x$knkod <- x$municipality.id
  data <- merge(map_kn, x, by = 'knkod')
  data[order(data$order),]  # make sure it's sorted by "order" column
}

# rkolada conn
a <- rkolada::rkolada()

# Get data from Kolada
x <- a$values('N00941', year = 2010)  # make sure "kpi.municipality_type == 'K'"
x <- prepare_map_data(x)

# Plot it!
ggplot(x, aes_string('ggplot_long', 'ggplot_lat', group = 'knkod', fill = 'value')) +
  geom_polygon() +
  coord_equal()

x <- a$values('N01963', year = 2010)
x <- subset(x, gender == 'T')
x <- prepare_map_data(x)
x$color <- substring(color.scale(x$value, c(1,1,0), c(0,1,1), 0), 1, 7)

m <- leaflet() %>% addTiles()
for (kn in unique(x$knkod)) {
  i <- x[x$knkod == kn,]
  m <- m %>% addPolygons(i$leaflet_long, i$leaflet_lat, color = i$color[[1]], weight = 1)
}
m