#Map demo from https://github.com/reinholdsson/swemaps/blob/master/README.md readme

library(leaflet)  # devtools::install_github("rstudio/leaflet")


x <- map_kn

m <- leaflet() %>% addTiles()

for (kn in unique(x$knkod)) {
  i <- x[x$knkod == kn,]
  m <- m %>% addPolygons(i$leaflet_long, i$leaflet_lat, color = 'blue', weight = 1)
}

m  # plot!
