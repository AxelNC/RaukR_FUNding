# install and load packages
# What packages are strictly required?
# devtools::install_github("rstudio/bslib") # I think this is a development version
# library("shiny")
# library("bslib")
# library(tidyverse)
# library(swemaps)
# library(leaflet)  # devtools::install_github("rstudio/leaflet")
# library(tm)
# library(wordcloud)
# library(memoise)
# library(ggthemes)


######## Functions ######
#==== Function from Mauricio 2023-06-20 15:05
# plot_variable <- function(data, variable) {
#   top_5 <- data %>%
#     group_by({{ variable }}) %>%
#     summarise(Total_funding = sum(FundingsSek)) %>%
#     arrange(desc(Total_funding)) %>%
#     top_n(5, Total_funding) %>%
#     select({{ variable }})
# 
#   top_5 <- as.vector(top_5[[1]])
# 
#   per_univ <- data %>%
#     group_by(FundingYear, {{ variable }}) %>%
#     summarise(Total_funding = sum(FundingsSek)) %>%
#     arrange(desc(Total_funding)) %>%
#     filter({{ variable }} %in% top_5)
# 
#   per_univ %>% ggplot(aes(x = FundingYear, y = Total_funding, fill = {{ variable }}, col = {{ variable }})) +
#     geom_line() +
#     geom_point() +
#     ggtitle("Sweden Research Funding by Year") +
#     scale_x_continuous(name="Year",
#                        breaks = seq(from = min(per_univ$FundingYear), to= max(per_univ$FundingYear), by = 2)) +
#     scale_y_continuous(name = "Total Funding (billion SEK)",
#                        breaks = seq(from = 0, to = max(per_univ$Total_funding), by = 1000000000),
#                        labels = function(x) paste0(x / 1000000000))
# }

# Usage example: projdata is the data frame with all data
# plot_variable(all_university_projects, CoordinatingOrganisationTypeOfOrganisationEn)
# 
# plot_variable(all_university_projects, CoordinatingOrganisationNameEn)
# 
# plot_variable(all_university_projects, FundingOrganisationNameEn)
# 
# plot_variable(all_university_projects, TypeOfAwardDescrEn)

# #==== Function from Mauricio 2023-06-20 18:20
plot_variable <- function(data, variable) {
  variable_name <- data %>% select({{ variable }}) %>% colnames()

  if (variable_name == "FundingYear") {
    df <- data %>%
      group_by({{ variable }}) %>%
      summarise(Total_funding = sum(FundingsSek))

    df %>% ggplot(aes(x = {{ variable }}, y = Total_funding)) +
      geom_line(aes(col = "blue")) +
      geom_point() +
      ggtitle("Sweden Research Funding by Year") +
      scale_x_continuous(name = "Year",
                         breaks = seq(from = min(df$FundingYear), to = max(df$FundingYear), by = 2)) +
      scale_y_continuous(name = "Total Funding (billion SEK)",
                         breaks = seq(from = 0, to = max(df$Total_funding)*1.1, by = 2000000000),
                         labels = function(x) paste0(x / 1000000000)) +
      theme_hc() + scale_colour_hc() +
      theme(legend.position = "none")
  }

  else {

    top_5 <- data %>%
      group_by({{ variable }}) %>%
      summarise(Total_funding = sum(FundingsSek)) %>%
      arrange(desc(Total_funding)) %>%
      top_n(5, Total_funding) %>%
      select({{ variable }})

    top_5 <- as.vector(top_5[[1]])

    df <- data %>%
      group_by(FundingYear, {{ variable }}) %>%
      summarise(Total_funding = sum(FundingsSek)) %>%
      arrange(desc(Total_funding)) %>%
      filter({{ variable }} %in% top_5)

    df %>% ggplot(aes(x = FundingYear, y = Total_funding, fill = {{ variable }}, col = {{ variable }})) +
      geom_line() +
      geom_point() +
      ggtitle("Sweden Research Funding by Year") +
      scale_x_continuous(name = "Year",
                         breaks = seq(from = min(df$FundingYear), to = max(df$FundingYear), by = 2)) +
      scale_y_continuous(name = "Total Funding (billion SEK)",
                         breaks =
                           if (max(df$Total_funding) < 5000000000) {
                             seq(from = 0, to = max(df$Total_funding)*1.1, by = 500000000)
                           }
                         else if (max(df$Total_funding) > 9000000000) {
                           seq(from = 0, to = max(df$Total_funding)*1.1, by = 2000000000)
                         }
                         else {
                           seq(from = 0, to = max(df$Total_funding)*1.1, by = 1000000000)
                         },
                         labels = function(x) paste0(x / 1000000000)) +
      theme_hc()+ scale_colour_hc() +
      theme(legend.position = "bottom",
            legend.title = element_blank())

  }
}
# 
# #example of implementation
# # projdata = all_university_projects
# 
# # plot_variable(projdata, CoordinatingOrganisationTypeOfOrganisationEn)
# # plot_variable(projdata, CoordinatingOrganisationNameEn)
# # plot_variable(projdata, FundingOrganisationNameEn)
# # plot_variable(projdata, TypeOfAwardDescrEn)
# # plot_variable(projdata, FundingYear)



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
years <- small_dataset %>% group_by(FundingYear) %>% summarise() %>% as.vector() %>% unlist() %>% unname()

# Book dataset
# The list of valid books
books <<- list("A Mid Summer Night's Dream" = "summer",
               "The Merchant of Venice" = "merchant",
               "Romeo and Juliet" = "romeo")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(book %in% books))
    stop("Unknown book")
  
  text <- readLines(sprintf("./Books/%s.txt", book),
                    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

# Mikael's wordcloud counts 2023-06-20 18:32
load("wordcloud_categories_count.RData")
book_library <- list(book_all_count,
                     book_1_count,
                     book_2_count,
                     book_3_count,
                     book_4_count,
                     book_5_count,
                     book_6_count,
                     book_9_count)


# scbs.codes <- read_csv2("scbs_codes_projID.csv") 
scbcat_df<- scbs.codes %>% 
  filter(scb_code<10) %>% 
  distinct(scb_code,.keep_all = T) %>% 
  select(-ProjectId) %>% 
  arrange(scb_code)

 book_selection <- as.vector(scbcat_df[2]) %>% unname() %>% unlist()
 # book_selection2 <- 


# People dataset
# parsed_people <- read_csv2("involved_people_projID.csv")

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


####### Definition of UI elements #######
# ui definition for FIRST module
# i.e., correlation of funds with various data
tab1 <- tabPanel("Correlation of variables",
                 sidebarPanel(
                   selectInput("select_input_x",
                               label = "Please select your x axis variable",
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

# ui definition for SECOND module
# i.e., mapping the funding onto Sweden
tab2 <- tabPanel("Geography",
                 headerPanel("Map of Sweden: "),
                 sliderInput("map_year",
                             label = "Please select year to filter by: ",
                             min = min(years),
                             max = max(years),
                             value = c(min(years), max(years))),
                 uiOutput("map_output", width="720px"))

# TESTINT TESTING ui definition for THIRD module TESTING TESTING
# i.e., mapping the funding onto Sweden
tab3 <- tabPanel("Analysis of abstracts",
                 titlePanel("Word Cloud"),
                 
                 sidebarLayout(
                   # Sidebar with a slider and selection inputs
                   sidebarPanel(
                     selectInput("selection", "Choose a book:",
                                 choices = books),
                     actionButton("update", "Change"),
                     hr(),
                     sliderInput("freq",
                                 "Minimum Frequency:",
                                 min = 1,  max = 50, value = 15),
                     sliderInput("max",
                                 "Maximum Number of Words:",
                                 min = 1,  max = 300,  value = 100)
                   ),
                   
                   # Show Word Cloud
                   mainPanel(
                     plotOutput("wordcloud_plot_output",
                                height = "500px")
                   )
                 ))

# ui definition for THIRD module
# i.e., mapping the funding onto Sweden
tab4 <- tabPanel("Analysis of abstracts",
                 titlePanel("Word Cloud Mikael"),
                 
                 sidebarLayout(
                   # Sidebar with a slider and selection inputs
                   sidebarPanel(
                     selectInput("selection_abstract_wordcount",
                                 label = "Choose a SCBS code: ",
                                 choices = book_selection),
                     selectInput("selection_mikael", "Choose a book:",
                                 choices = books),
                     actionButton("update_mikael", "Change"),
                     hr(),
                     sliderInput("freq_mikael",
                                 "Minimum Frequency:",
                                 min = 1,  max = 50, value = 15),
                     sliderInput("max_mikael",
                                 "Maximum Number of Words:",
                                 min = 1,  max = 300,  value = 100)
                   ),
                   
                   # Show Word Cloud
                   mainPanel(
                     plotOutput("wordcloud_plot_output_mikael",
                                height = "500px")
                   )
                 ))

####### END #######


###### HERE IS THE ACTUAL APP: ######
shinyApp(
  ui=navbarPage("Welcome to the FUNding application",#theme = bslib::bs_theme(primary = "#78C2AD"),
                tab1,
                tab2,
                tab3,
                tab4,
                tabPanel("Grant applicants")
    ),
  
  
  server=function(input,output,session) {
    ####### MODULE 1 [correlation]: ######
    output$text_selection_output <- renderText(paste0("You selected: ",
                                                      deparse(substitute(input$select_input_x)),
                                                      " and ",
                                                      input$select_input_y))

    output$plot_output <- renderPlot({
      plot_variable(small_dataset, !!sym(input$select_input_x)) + theme_bw()
      })
    ###### END ######
    
    ###### MODULE 2 [map]: ######
    output$map_output <- renderUI(m)
    ###### END ######
    
    ####### MODULE 3 [wordcloud]: ######
    # Define a reactive expression for the document term matrix
    terms <- reactive({
      # Change when the "update" button is pressed...
      input$update
      # ...but not for anything else
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          getTermMatrix(input$selection)
        })
      })
    })
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$wordcloud_plot_output <- renderPlot({
      v <- terms()
      wordcloud_rep(names(v), v, scale=c(4,0.5),
                    min.freq = input$freq, max.words=input$max,
                    colors=brewer.pal(8, "Dark2"))
    })
    ###### END ######
    
    ####### MODULE 4 [wordcloud MIKAEL]: ######
    # Define a reactive expression for the document term matrix
    terms2 <- reactive({
      # Change when the "update" button is pressed...
      input$update_mikael
      # ...but not for anything else
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          getTermMatrix(input$selection_mikael)
        })
      })
    })
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$wordcloud_plot_output_mikael <- renderPlot({
      v2 <- terms2()
      wordcloud_rep(names(v2), v2, scale=c(4,0.5),
                    min.freq = input$freq_mikael, max.words=input$max_mikael,
                    colors=brewer.pal(8, "Dark2"))
    })
    ###### END ######
    
  })
###### END ######