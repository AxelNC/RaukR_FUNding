# install and load packages
# What packages are strictly required?
# library("shiny")
# library("bslib")
#library(tidyverse)
#library(swemaps)
#library(leaflet)  # devtools::install_github("rstudio/leaflet")


# Map3 from Axel 2023-06-17 16:53
x <- map_kn

m <- leaflet() %>% addTiles()

for (kn in unique(x$knkod)) {
  i <- x[x$knkod == kn,]
  m <- m %>% addPolygons(i$leaflet_long, i$leaflet_lat, color = 'blue', weight = 1)
}
# To plot: call 
# m


######## Logos
html_chalmers <- "https://upload.wikimedia.org/wikipedia/en/3/3f/Formal_Seal_of_Chalmers_tekniska_h%C3%B6gskola%2C_G%C3%B6teborg%2C_V%C3%A4stra_G%C3%B6talands_l%C3%A4n%2C_Sverige.svg"
########


placeholder_plot <- hist(rnorm(1000))


# Here is the defintion of the ui for the first module
# i.e., correlation of funds with various data
tab1 <- tabPanel("Correlation of variables",
                 sidebarPanel(
                   selectInput("select_input",
                               label = "Please select your variable",
                               choices = c("Year", 
                                           "University", 
                                           "Funding organization name",
                                           "Funding organization type",
                                           "Type of grant",
                                           "Research field",
                                           "Gender of principal investigator")),
                 ),
                 mainPanel(
                   hr(),
                   textOutput("text_selection_output"),
                   hr(),
                   plotOutput("plot_output",width="400px")
                 )
                 )

# Here is the defintion of the ui for the first module
# i.e., mapping the funding onto Sweden
tab2 <- tabPanel("Geography",
                 headerPanel("Map of Sweden: "),
                 uiOutput("map_output", width="720px"))


# HERE IS THE ACTUAL APP:
shinyApp(
  ui=fluidPage(#theme = bslib::bs_theme(primary = "#78C2AD"),
    titlePanel("Welcome to the FUNding application"),
    imageOutput("photo"),
    headerPanel("Please select your analysis"),
    mainPanel(tabsetPanel(
      tab1,
      tab2,
      tabPanel("Grant applicants"),
      tabPanel("Analysis of abstracts")
    ))),
  
  
  server=function(input,output) {
    # MODULE 1 [correlation]:
    output$text_selection_output <- renderText(paste0("You selected: ", input$select_input))
    output$plot_output <- renderPlot({
      hist(rnorm(1000))
    })
    
    # ======== temporary!
    output$photo <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path('./images/rauk.jpeg'))
      
      # Return a list containing the filename
      list(src = filename, width = "200px", height = "200px")
    }, deleteFile = FALSE)
    # ======== temporary!
    
    # MODULE 2 [map]:
    output$map_output <- renderUI(m)
  })


# tabsetPanel(
#   div(tabPanel("Table", dataTableOutput("table")), style = 'width:5500px;')
# )