# install and load packages
# What packages are strictly required?
# devtools::install_github("rstudio/bslib") # I think this is a development version
# library("shiny")
# library("bslib")
# library(tidyverse)
# library(swemaps)
# library(leaflet)  # devtools::install_github("rstudio/leaflet")


######## Functions ######
# Function from Mauricio 2023-06-20 15:05
plot_variable <- function(data, variable) {
  top_5 <- data %>%
    group_by({{ variable }}) %>%
    summarise(Total_funding = sum(FundingsSek)) %>%
    arrange(desc(Total_funding)) %>%
    top_n(5, Total_funding) %>%
    select({{ variable }})
  
  top_5 <- as.vector(top_5[[1]])
  
  per_univ <- data %>%
    group_by(FundingYear, {{ variable }}) %>%
    summarise(Total_funding = sum(FundingsSek)) %>%
    arrange(desc(Total_funding)) %>%
    filter({{ variable }} %in% top_5)
  
  per_univ %>% ggplot(aes(x = FundingYear, y = Total_funding, fill = {{ variable }}, col = {{ variable }})) +
    geom_line() +
    geom_point() +
    ggtitle("Sweden Research Funding by Year") +
    scale_x_continuous(name="Year",
                       breaks = seq(from = min(per_univ$FundingYear), to= max(per_univ$FundingYear), by = 2)) +
    scale_y_continuous(name = "Total Funding (billion SEK)",
                       breaks = seq(from = 0, to = max(per_univ$Total_funding), by = 1000000000),
                       labels = function(x) paste0(x / 1000000000))
}

# Usage example: projdata is the data frame with all data
# plot_variable(all_university_projects, CoordinatingOrganisationTypeOfOrganisationEn)
# 
# plot_variable(all_university_projects, CoordinatingOrganisationNameEn)
# 
# plot_variable(all_university_projects, FundingOrganisationNameEn)
# 
# plot_variable(all_university_projects, TypeOfAwardDescrEn)



# Map3 from Axel 2023-06-17 16:53
x <- map_kn

m <- leaflet() %>% addTiles()

for (kn in unique(x$knkod)) {
  i <- x[x$knkod == kn,]
  m <- m %>% addPolygons(i$leaflet_long, i$leaflet_lat, color = 'blue', weight = 1)
}
# To plot: call 
# m

####### END #######


####### Datasets #######

# all_university_projects <- read_csv("all_university_projects.csv")
small_dataset <- all_university_projects %>% 
  select(FundingYear,
         FundingsSek,
         CoordinatingOrganisationNameEn,
         CoordinatingOrganisationTypeOfOrganisationEn,
         FundingOrganisationNameEn,
         TypeOfAwardDescrEn)
####### END #######


####### Global variables #######
x_variable_names <- list("Funding year",
                      "Funding [SEK]",
                      "Name of coordinating organization",
                      "Type of coordinating organization",
                      "Name of funding organization",
                      "Type of award")
x_variables <- colnames(small_dataset)
names(x_variables) <- x_variable_names
####### END #######


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
                               # choices = colnames(small_dataset)
                               choices = x_variables,
                               selected = x_variables[3]
                               ),
                   selectInput("select_input_y",
                               label = "Please select your y axis variable",
                               choices = x_variables
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
    output$text_selection_output <- renderText(paste0("You selected: ", 
                                                      names(input$select_input_x),  
                                                      " and ", 
                                                      input$select_input_y))

    output$plot_output <- renderPlot({
      plot_variable(small_dataset, !!sym(input$select_input_x))
      })
    
    # MODULE 2 [map]:
    output$map_output <- renderUI(m)
  })

