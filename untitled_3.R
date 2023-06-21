#map demo 3 insipred by https://github.com/reinholdsson/swemaps/blob/master/README.md
library(tidyverse)
library(swemaps)
library(leaflet)  # devtools::install_github("rstudio/leaflet")
library(leaflet.extras) # remotes::install_github("bhaskarvk/leaflet.extras")
library(sf)
library(htmlwidgets)

################################################################################
# new polygons per county
carto_vectors_sweden <- st_read("sweden-counties_1680.geojson")

# Convert the spatial object to an sf object
sf_vectors <- st_as_sf(carto_vectors_sweden) %>%
  rename(County = NAME_1)

# Create a Leaflet map
map <- leaflet() %>%
  addTiles()

# sum yearly money county
years <- c(2012, 2013, 2014)
yearly_2008_county <- university_yearly_sek_data %>%
  filter(FundingYear %in% years) %>%
  group_by(Code, County) %>%
  summarise(
    yearly_funding_sek_county = sum(yearly_funding_sek)
  ) %>% arrange(yearly_funding_sek_county)

# Another color palette
pal <- colorNumeric("viridis", NULL)

#join the colour information into the sf_vectors df
## need to change the names first
# change County names in sf_vectors
v_sf <- c("Östergötlands län", "Blekinge län", "Dalarnas län", "Gävleborgs län", "Gotlands län", "Hallands län", "Jämtlands län", "Jönköpings län",
             "Kalmar län", "Kronobergs län", "Norrbottens län", "Örebro län", "Södermanlands län", "Skåne län", "Stockholms län", "Uppsala län",
             "Värmlands län", "Västerbottens län", "Västernorrlands län", "Västmanlands län", "Västra Götalands län")

sf_vectors$County <- v_sf

#now join
sf_vectors <- left_join(sf_vectors, 
                        data.frame(County = yearly_2008_county$County,
                                   funding = yearly_2008_county$yearly_funding_sek_county),
                        by = 'County')  


# Add the polygons to the Leaflet map
map <- map %>%
  addPolygons(data = sf_vectors,
              fillColor = ~pal(funding),
              fillOpacity = 0.5,
              color = "black",
              weight = 1,
              layerId = ~ID_1) 

# Extract the centroids of polygons, be careful, they assumer same geometry for each polygon
centroids <- st_centroid(sf_vectors)

# Convert the centroids to a data frame
centroids_df <- st_coordinates(centroids) %>%
  as.data.frame() %>% add_column(label = paste("County:", sf_vectors$County, "<br>",
                                               "Funding:", sf_vectors$funding, "<br>"))

# Add labels to the polygons
map <- map %>%
  addLabelOnlyMarkers(data = centroids_df,
                      lat = ~Y,  # column containing the latitude coordinates
                      lng = ~X,  # column containing the longitude coordinates
                      label = ~label,  # column containing the names
                      labelOptions = labelOptions(
                        noHide = TRUE,  # show labels by default
                        direction = "auto",
                        textOnly = FALSE),
                      clusterOptions = markerClusterOptions(
                        spiderfyOnMaxZoom = FALSE,
                        disableClusteringAtZoom = 12  # show labels at zoom level >= 12
                      )
  )
                        
       
                      

  
  

# Add legend
map <- map %>% addLegend(data = sf_vectors, pal = pal, values = ~funding,
                         opacity = 0.7,
                         title = 'Funding per County',
                         position = "bottomleft") 

# Display the map
map

################################################################################
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

# assign colours to polygons
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
                fillOpacity = 0.65,
                color = "grey",
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