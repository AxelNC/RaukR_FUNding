placeholder_plot <- hist(rnorm(1000))

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

shinyApp(
  ui=fluidPage(
    titlePanel("Welcome to the FUNding application"),
    headerPanel("Please select your analysis"),
    mainPanel(tabsetPanel(
      tab1,
      tabPanel("Grant applicants"),
      tabPanel("Analysis of abstracts"),
      tabPanel("Geography")
    ))),
  server=function(input,output) {
    output$text_selection_output <- renderText(paste0("You selected: ", input$select_input))
    output$plot_output <- renderPlot({
      hist(rnorm(1000))
    })
    
  },
  options=list(height=600))


