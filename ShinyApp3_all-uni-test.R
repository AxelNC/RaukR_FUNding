# install and load packages
# What packages are strictly required?
# devtools::install_github("rstudio/bslib") # I think this is a development version
# library("shiny")
# library("bslib")
# library(tidyverse)
# library(swemaps)
# library(leaflet)  # devtools::install_github("rstudio/leaflet")


# all_university_projects <- read_csv("all_university_projects.csv")
small_dataset <- head(all_university_projects, n = 100L) %>% 
  select(FundingYear,
         FundingsSek,
         CoordinatingOrganisationNameEn,
         CoordinatingOrganisationTypeOfOrganisationEn,
         TypeOfAwardDescrEn)


# Map3 from Axel 2023-06-17 16:53
x <- map_kn

m <- leaflet() %>% addTiles()

for (kn in unique(x$knkod)) {
  i <- x[x$knkod == kn,]
  m <- m %>% addPolygons(i$leaflet_long, i$leaflet_lat, color = 'blue', weight = 1)
}
# To plot: call 
# m


# Here is the defintion of the ui for the first module
# i.e., correlation of funds with various data
tab1 <- tabPanel("Correlation of variables",
                 sidebarPanel(
                   selectInput("select_input_x",
                               label = "Please select your x axis variable",
                               # choices = c("Year", 
                               #             "University", 
                               #             "Funding organization name",
                               #             "Funding organization type",
                               #             "Type of grant",
                               #             "Research field",
                               #             "Gender of principal investigator")
                               choices = colnames(small_dataset)
                               ),
                   selectInput("select_input_y",
                               label = "Please select your y axis variable",
                               choices = colnames(small_dataset)
                   ),
                 ),
                 mainPanel(
                   textOutput("text_selection_output"),
                   plotOutput("plot_output")
                 )
                 )

# Here is the defintion of the ui for the first module
# i.e., mapping the funding onto Sweden
tab2 <- tabPanel("Geography",
                 headerPanel("Map of Sweden: "),
                 uiOutput("map_output", width="720px"))

tab3 <- tabPanel("Testing card",
                 card(
                   card_header(h2("This is a header")),
                   card_body(),
                   card_footer("This is a footer")
                 ))





# HERE IS THE ACTUAL APP:
shinyApp(
  ui=navbarPage("Welcome to the FUNding application",#theme = bslib::bs_theme(primary = "#78C2AD"),
                tab1,
                tab2,
                tab3,
                tabPanel("Grant applicants"),
                tabPanel("Analysis of abstracts")
    ),
  
  
  server=function(input,output) {
    # MODULE 1 [correlation]:
    output$text_selection_output <- renderText(paste0("You selected: ", input$select_input_x,  " and ", input$select_input_y))

    output$plot_output <- renderPlot({
      ggplot(all_university_projects,
             mapping = aes(x = !!sym(input$select_input_x),
                           y = !!sym(input$select_input_y))) +
        geom_point()})
    
    # MODULE 2 [map]:
    output$map_output <- renderUI(m)
  })

