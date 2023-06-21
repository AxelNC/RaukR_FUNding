#Map demo from https://github.com/reinholdsson/swemaps/blob/master/README.md readme
# install.packages('devtools')
#devtools::install_github('reinholdsson/swemaps')
library(tidyverse)
library(rkolada)  # devtools::install_github("reinholdsson/rkolada")
# rkolada conn
a <- rkolada::rkolada()

library(swemaps)
library(leaflet)  # devtools::install_github("rstudio/leaflet")




library(plotrix) #install.packages("plotrix")


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


####Make sure to change value and then try to create a continious color scale according to the below example and implement it as in the second example below

# Create a continuous palette function #from https://rstudio.github.io/leaflet/colors.html
pal <- colorNumeric(
  palette = "Blues",
  domain = countries$gdp_md_est)

# Apply the function to provide RGB colors to addPolygons
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(gdp_md_est))