#Map demo from https://github.com/reinholdsson/swemaps/blob/master/README.md readme
# install.packages('devtools')
#devtools::install_github('reinholdsson/swemaps')
library(rkolada)  # devtools::install_github("reinholdsson/rkolada")
# rkolada conn
a <- rkolada::rkolada()

library(swemaps)
library(leaflet)  # devtools::install_github("rstudio/leaflet")


#x <- map_kn

#m <- leaflet() %>% addTiles()

#for (kn in unique(x$knkod)) {
#  i <- x[x$knkod == kn,]
#  m <- m %>% addPolygons(i$leaflet_long, i$leaflet_lat, color = 'blue', weight = 1)
#}
#
#m  # plot!




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