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
# library(scales)

###### Load in the necessary raw data: ######

###===
### TO LOAD RAW DATA FROM FILES, UNCOMMENT THE FOLLOWING CODE:
###===

### Main dataset
# all_university_projects <- read_csv("all_university_projects.csv")

### SCBS codes dataset
# scbs.codes <- read_csv2("scbs_codes_projID.csv") 

### Mikael's abstract word counts
# load("wordcloud_categories_count.RData")

### People dataset
# parsed_people <- read_csv2("involved_people_projID.csv")

###### END ######


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


# Function used to couple user input into the word cloud call
plot_selector <- function(selection) {
  variable_name <- paste0("book_", selection, "_count")
  variable_name
}

####### END #######


####### Calculations for datasets #######


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
book_library <- list(book_all_count,
                     book_1_count,
                     book_2_count,
                     book_3_count,
                     book_4_count,
                     book_5_count,
                     book_6_count,
                     book_9_count)


scbcat_df<- scbs.codes %>% 
  filter(scb_code<10) %>% 
  distinct(scb_code,.keep_all = T) %>% 
  select(-ProjectId) %>% 
  arrange(scb_code)

book_selection <- as.vector(scbcat_df[2]) %>% unname() %>% unlist()
book_selection <- c("All fields", book_selection)
list_of_fields <- c("all","1","2","3","4","5","6","9")
names(list_of_fields) <- book_selection


# Generation of funding by gender dataset, From Evelyn 2023-06-21 09:26
all_university_projects_people <- merge(all_university_projects, parsed_people, by="ProjectId")
all_university_projects_people <- all_university_projects_people[all_university_projects_people$FundingYear %in% c("2020","2021","2022","2023"), ]
all_university_projects_people_mean <- aggregate(FundingsSek ~ FundingYear + gender, data = all_university_projects_people, FUN = mean)
min_funding <- min(all_university_projects_people_mean$FundingsSek)
max_funding <- max(all_university_projects_people_mean$FundingsSek)
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

# ui definition for THIRD module
# i.e., mapping the funding onto Sweden
tab3 <- tabPanel("Analysis of abstracts",
                 titlePanel("Most frequent words used in English abstracts"),
                 
                 sidebarLayout(
                   # Sidebar with a slider and selection inputs
                   sidebarPanel(
                     selectInput("selection_abstract_wordcount",
                                 label = "Choose a research field: ",
                                 choices = list_of_fields),
                     actionButton("update_mikael", "Change"),
                     hr(),
                     # sliderInput("freq_mikael",
                     #             "Minimum Frequency:",
                     #             min = 1,  max = 50, value = 15),
                     sliderInput("max_mikael",
                                 "Maximum Number of Words:",
                                 min = 1,  max = 300,  value = 100)
                   ),
                   
                   # Show Word Cloud
                   mainPanel(
                     plotOutput("wordcloud_plot_test",
                                height = "500px"),
                     helpText(div("Wordcloud adapted from: ", tags$a("https://shiny.posit.co/r/gallery", href = "https://shiny.posit.co/r/gallery/start-simple/word-cloud/")))
                   )
                   # helpText("Footer")
                 ))

# ui definition for FIFTH module
# i.e., funding by gender
tab5 <- tabPanel("Grant applicants",
                 tabsetPanel(
                   tabPanel("Option 1",
                            sidebarPanel(
                              helpText("This is some text.")
                            ),
                            mainPanel(
                              plotOutput("gender_plot_year")
                            )),
                   tabPanel("Option2")
                   )
                 )

####### END #######


###### HERE IS THE ACTUAL APP: ######
shinyApp(
  ui=navbarPage("Welcome to the FUNding application",#theme = bslib::bs_theme(primary = "#78C2AD"),
                tab1,
                tab2,
                tab3,
                tab5
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
    
    
    ####### MODULE 4 [wordcloud MIKAEL]: ######
    # Define a reactive expression for the document term matrix
    terms3 <- reactive({
      # Change when the "update" button is pressed...
      input$update_mikael
      # ...but not for anything else
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          plot_selector(input$selection_abstract_wordcount)
        })
      })
    })
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    # Render the plot
    output$wordcloud_plot_test <- renderPlot({
      
      v3 <- terms3()
      
      eval(as.name(v3)) %>% 
        {wordcloud_rep(.$word, 
                       .$nr_words, 
                       scale = c(4,0.5),
                       # min.freq = input$freq_mikael,
                       max.words = input$max_mikael,
                       colors=brewer.pal(8, "Dark2"))}
    })
                    
    
    ###### END ######
    
    ###### MODULE 5 [gender ratio] #####
    output$gender_plot_year <- renderPlot({
      ggplot(all_university_projects_people, aes(x = factor(FundingYear), y = FundingsSek, fill = gender)) +
        geom_boxplot() +
        labs(x = "Year", y = "Funding (SEK)", fill = "Gender") +
        scale_y_continuous(limits = c(min_funding, max_funding), labels = scales::comma) +
        facet_grid(. ~ FundingYear, scales = "free_x", space = "free", switch = "y") +
        theme_classic() +
        theme(strip.text = element_blank()) +
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
    })
    ###### END ######
    
    
  })
###### END ######

list_of_fields

selection <- list_of_fields[2]



if(selection == "1") {
  print("yes")
}

selection <- plot_selector(list_of_fields[1])

eval(as.name(selection)) %>% {wordcloud(.$word, .$nr_words, max.words = 50)}


