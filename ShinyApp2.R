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
  ui=navbarPage("Welcome to the FUNding application",#theme = bslib::bs_theme(primary = "#78C2AD"),
                tab1,
                tab2,
                tabPanel("Grant applicants"),
                tabPanel("Analysis of abstracts")
    ),
  
  
  server=function(input,output) {
    # MODULE 1 [correlation]:
    output$text_selection_output <- renderText(paste0("You selected: ", input$select_input))
    output$plot_output <- renderPlot({
      hist(rnorm(1000))
    })
    
    # MODULE 2 [map]:
    output$map_output <- renderUI(m)
  })