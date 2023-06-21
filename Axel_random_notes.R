#Axel random notes
library(swecris)
AA_all_swecris_data <- swecris_fundings()
#te

# install.packages('devtools')
#devtools::install_github('reinholdsson/swemaps')


library(ggplot2)
library(swemaps)
library(rkolada)  # devtools::install_github("reinholdsson/rkolada")

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


###
library(leaflet)  # devtools::install_github("rstudio/leaflet")

x <- map_kn

m <- leaflet() %>% addTiles()

for (kn in unique(x$knkod)) {
  i <- x[x$knkod == kn,]
  m <- m %>% addPolygons(i$leaflet_long, i$leaflet_lat, color = 'blue', weight = 1)
}

m  # plot!



library(ggplot2)
library(swemaps)
library(rkolada)  # devtools::install_github("reinholdsson/rkolada")

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


library(leaflet)  # devtools::install_github("rstudio/leaflet")

x <- map_kn

m <- leaflet() %>% addTiles()

for (kn in unique(x$knkod)) {
  i <- x[x$knkod == kn,]
  m <- m %>% addPolygons(i$leaflet_long, i$leaflet_lat, color = 'blue', weight = 1)
}

m  # plot!

install.packages("plotrix")
library(plotrix)

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




lan_data <- list(
  `01 Stockholms län` = c(
    "Stockholms universitet",
    "Kungliga Tekniska högskolan (KTH)",
    "Karolinska Institutet",
    "Handelshögskolan i Stockholm"
  ),
  `03 Uppsala län` = c(
    "Uppsala universitet",
    "Sveriges lantbruksuniversitet (SLU)"
  ),
  `04 Södermanlands län` = c(
    "Mälardalens högskola"
  ),
  `05 Östergötlands län` = c(
    "Linköpings universitet"
  ),
  `06 Jönköpings län` = c(
    "Högskolan i Jönköping"
  ),
  `07 Kronobergs län` = c(
    "Linnéuniversitetet"
  ),
  `08 Kalmar län` = c(
    "Linnéuniversitetet"
  ),
  `09 Gotlands län` = c(
    "Uppsala universitet (Gotlands campus)"
  ),
  `10 Blekinge län` = c(
    "Blekinge Tekniska Högskola (BTH)"
  ),
  `12 Skåne län` = c(
    "Lunds universitet",
    "Malmö universitet"
  ),
  `13 Hallands län` = c(
    "Halmstads högskola"
  ),
  `14 Västra Götalands län` = c(
    "Göteborgs universitet",
    "Chalmers tekniska högskola"
  ),
  `17 Värmlands län` = c(
    "Karlstads universitet"
  ),
  `18 Örebro län` = c(
    "Örebro universitet"
  ),
  `19 Västmanlands län` = c(
    "Mälardalens högskola (Västerås campus)"
  ),
  `20 Dalarnas län` = c(
    "Högskolan Dalarna"
  ),
  `21 Gävleborgs län` = c(
    "Högskolan i Gävle"
  ),
  `22 Västernorrlands län` = c(
    "Mittuniversitetet (Sundsvall campus)"
  ),
  `23 Jämtlands län` = c(
    "Mittuniversitetet (Östersund campus)"
  ),
  `24 Västerbottens län` = c(
    "Umeå universitet"
  ),
  `25 Norrbottens län` = c(
    "Luleå tekniska universitet",
    "Umeå universitet (Campus Skellefteå)"
  )
)

