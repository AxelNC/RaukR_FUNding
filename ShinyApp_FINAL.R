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
# library(leaflet.extras) # remotes::install_github("bhaskarvk/leaflet.extras")
# library(sf)
# library(htmlwidgets)



###### Load in the necessary raw data: ######

###===
### TO LOAD RAW DATA FROM FILES, UNCOMMENT THE FOLLOWING CODE:
###===

### Main dataset
### SCBS codes dataset
### Mikael's abstract word counts
### People dataset

all_university_projects <- read_csv("all_university_projects.csv")
scbs.codes <- read_csv2("scbs_codes_projID.csv")
load("wordcloud_categories_count.RData")
parsed_people <- read_csv2("involved_people_projID.csv")

###### END ######



###### Pre-calculate this ######
### Generation of frequencies of people being part of projects
# Added 2023-06-21 16:15
# # Generation of funding by gender dataset, From Evelyn 2023-06-21 09:26
# all_university_projects_people <- merge(all_university_projects, parsed_people, by="ProjectId")
# all_university_projects_people <- all_university_projects_people[all_university_projects_people$FundingYear %in% c("2020","2021","2022","2023"), ]
# all_university_projects_people_mean <- aggregate(FundingsSek ~ FundingYear + gender, data = all_university_projects_people, FUN = mean)
# min_funding <- min(all_university_projects_people_mean$FundingsSek)
# max_funding <- max(all_university_projects_people_mean$FundingsSek)
# #frequency of projects by orcidId
# data_filtered_orcid <- all_university_projects_people  %>% filter(!is.na(orcId))
# project_counts_orcid <- sapply(unique(data_filtered_orcid$orcId), function(id) sum(data_filtered_orcid$orcId == id, na.rm = T))
# freq_orcid <- table(project_counts_orcid)
# freq_orcid <- data.frame(Projects = as.numeric(names(freq_orcid)), Frequency = as.numeric(freq_orcid))
# 
# #top 10 scientists by orcid id
# project_counts_orcid_df <- as.data.frame(project_counts_orcid)
# project_counts_orcid_df_sort <- project_counts_orcid_df %>% arrange(desc(project_counts_orcid))
# project_counts_orcid_df_sort$fullName <- data_filtered_orcid$fullName[match(rownames(project_counts_orcid_df_sort), data_filtered_orcid$orcId)]
# 
# top_10_orcid <- head(project_counts_orcid_df_sort, 10)
# top_10_orcid$fullName <- factor(top_10_orcid$fullName, levels = top_10_orcid$fullName)
# 
# #frequency of projects by name
# data_filtered_name <- all_university_projects_people  %>% filter(!is.na(fullName))
# project_counts_name <- sapply(unique(data_filtered_name$fullName), function(id) sum(data_filtered_name$fullName == id, na.rm = T))
# freq_name <- table(project_counts_name)
# freq_name <- data.frame(Projects = as.numeric(names(freq_name)), Frequency = as.numeric(freq_name))
# 
# #top 10 scientists by name
# project_counts_name_df <- as.data.frame(project_counts_name)
# project_counts_name_df_sort  <- project_counts_name_df %>% arrange(desc(project_counts_name))
# project_counts_name_df_sort$fullName <- data_filtered_name$fullName[match(rownames(project_counts_name_df_sort), data_filtered_name$fullName)]
# 
# top_10_name <- head(project_counts_name_df_sort, 10)
# top_10_name$fullName <- factor(top_10_name$fullName, levels = top_10_name$fullName)
###### END ######



######## Functions ######
# Function retrieved from Mauricio at 2023-06-21 14:36
plot_variable <- function(data, variable) {
  variable_name <- data %>% select({{ variable }}) %>% colnames()
  
  if (variable_name == "CoordinatingOrganisationNameEn") {
    legend_name <- "Top 5 Universities"
  }
  else if (variable_name == "CoordinatingOrganisationTypeOfOrganisationEn") {
    legend_name <- "Type of Organization"
  }
  else if (variable_name == "FundingOrganisationNameEn") {
    legend_name <- "Funding Organization"
  }
  else if (variable_name == "TypeOfAwardDescrEn") {
    legend_name <- "Type of Award"
  }
  
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
      theme_bw() + scale_colour_hc() +
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
      labs(title ="Sweden Research Funding by Year",
           subtitle = legend_name) +
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
      theme_bw()+ scale_colour_hc() +
      theme(legend.position = "bottom",
            legend.title = element_blank())
    
  }
}



# Function used to couple user input into the word cloud call
plot_selector <- function(selection) {
  variable_name <- paste0("book_", selection, "_count")
  variable_name
}

####### END #######



####### Global variables and datasets #######
### Generation of dataset for MODULE 1 [funding plots]
small_dataset <- all_university_projects %>% 
  select(FundingYear,
         FundingsSek,
         CoordinatingOrganisationNameEn,
         CoordinatingOrganisationTypeOfOrganisationEn,
         FundingOrganisationNameEn,
         TypeOfAwardDescrEn)
years <- small_dataset %>% group_by(FundingYear) %>% summarise() %>% as.vector() %>% unlist() %>% unname()

### Global variables for MODULE 1 [funding plots]
x_variable_names <- list("Funding year",
                         "Funding [SEK]",
                         "Name of coordinating organization",
                         "Type of coordinating organization",
                         "Name of funding organization",
                         "Type of award")
x_variables <- colnames(small_dataset)
names(x_variables) <- x_variable_names
x_variables <- x_variables[-2]

### Generation of datasets for MODULE 3 [word clouds]
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

### Generation of dataset and variables for MODULE 4 [gender]

# Selection of plots
gender_plot_list <- c("plot1", "plot2", "plot3", "plot4", "plot5", "plot6")
names(gender_plot_list) <- c("Average funding per gender", 
                             "Average funding per gender over time",
                             "Number of projects per personal name",
                             "Personal names associated with the most projects",
                             "Number of projects per ORCID iD",
                             "ORCID iDs associated with the most projects")

####### END #######



####### Global variables and datasets for map 2023-06-21 13:30 #######
grouped.scbs.codes <- group_by(scbs.codes, ProjectId)

#### list containing the universities per county and county code

lan_data <- list(
  `01 Stockholms län` = c(
    "Stockholms universitet",
    "KTH, Kungliga tekniska högskolan",
    "Karolinska Institutet",
    "Handelshögskolan i Stockholm",
    "Södertörns högskola"
  ),
  `03 Uppsala län` = c(
    "Uppsala universitet",
    "Sveriges lantbruksuniversitet"
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
    "Linnéuniversitetet",
    "Högskolan i Kalmar"
  ),
  `09 Gotlands län` = c(
    "Uppsala universitet"
  ),
  `10 Blekinge län` = c(
    "Blekinge Tekniska Högskola"
  ),
  `12 Skåne län` = c(
    "Lunds universitet",
    "Malmö universitet"
  ),
  `13 Hallands län` = c(
    "Högskolan i Halmstad"
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
    "Mälardalens Högskola"
  ),
  `20 Dalarnas län` = c(
    "Högskolan Dalarna"
  ),
  `21 Gävleborgs län` = c(
    "Högskolan i Gävle"
  ),
  `22 Västernorrlands län` = c(
    "Mittuniversitetet"
  ),
  `23 Jämtlands län` = c(
    "Mittuniversitetet"
  ),
  `24 Västerbottens län` = c(
    "Umeå universitet"
  ),
  `25 Norrbottens län` = c(
    "Luleå Tekniska Universitet",
    "Umeå universitet"
  )
)

#### get/summarize yearly funding per university and keep county code
#### Get the listed data to a tibble with a row per university, maintaining the county and county code.

tibble_data <- enframe(lan_data, name = "County", value = "University")

print(tibble_data)

tibble_data <- tibble_data %>%
  mutate(Code = substr(County, 1, 2))

unnested_tibble_data <- tibble_data %>%
  rowwise() %>%
  do(data.frame(Code = .$Code,
                County = paste(str_split(.$County, ' ')[[1]][2:length(str_split(.$County, ' ')[[1]])], collapse = " "),
                University = unlist(.$University))) 

#### Fetch yearly funding information from master database and merge with our tibble that contains the universities
#### we are interested into

all_university_funding_yearly <- all_university_projects %>%
  group_by(CoordinatingOrganisationNameSv, FundingYear) %>%
  summarize(
    yearly_funding_sek = sum(FundingsSek)
  ) %>% rename(University = CoordinatingOrganisationNameSv)

university_yearly_sek_data <- left_join(unnested_tibble_data,
                                        all_university_funding_yearly,
                                        by = "University",
                                        relationship = "many-to-many")


#### get/summarize yearly funding per university and scbs code and keep county code

### merge the data frame with the unlsited scbs codes with the master dataframe to have funding info and stuff

university_sek_data_scbs <- left_join(grouped.scbs.codes, all_university_projects, by = "ProjectId") %>%
  select(ProjectId, CoordinatingOrganisationNameSv, scb_code, scb_sv_en, FundingYear, FundingsSek) %>%
  distinct(scb_code, .keep_all = T) # filter out repeated rows per projectID (some has 6 multiple timpes e.g)

### get yearly funding per scbs code and get info to a df with the universities we are interested into

university_yearly_sek_data_scbs <- university_sek_data_scbs %>% 
  group_by(CoordinatingOrganisationNameSv, FundingYear, scb_sv_en, scb_code) %>%
  summarize(
    yearly_funding_sek = sum(FundingsSek)
  ) %>% rename(University = CoordinatingOrganisationNameSv)

university_yearly_sek_data_scbs <- left_join(unnested_tibble_data,
                                             university_yearly_sek_data_scbs,
                                             by = "University",
                                             relationship = "many-to-many")

### From "untitled_3.R" ###
carto_vectors_sweden <- st_read("sweden-counties_1680.geojson")

# Convert the spatial object to an sf object
sf_vectors <- st_as_sf(carto_vectors_sweden) %>%
  rename(County = NAME_1)

# Create a Leaflet map
map <- leaflet(height = "600px") %>%
  addTiles()

#/

# Another color palette
pal <- colorNumeric("viridis", NULL)

#join the colour information into the sf_vectors df
## need to change the names first
# change County names in sf_vectors
v_sf <- c("Östergötlands län", "Blekinge län", "Dalarnas län", "Gävleborgs län", "Gotlands län", "Hallands län", "Jämtlands län", "Jönköpings län",
          "Kalmar län", "Kronobergs län", "Norrbottens län", "Örebro län", "Södermanlands län", "Skåne län", "Stockholms län", "Uppsala län",
          "Värmlands län", "Västerbottens län", "Västernorrlands län", "Västmanlands län", "Västra Götalands län")

sf_vectors$County <- v_sf

####### END #######



####### Definition of UI elements #######
# ui definition for FIRST module
# i.e., correlation of funds with various data
tab1 <- tabPanel("Data exploration",
                 sidebarPanel(
                   selectInput("select_input_x",
                               label = "Please select your x axis variable",
                               choices = x_variables,
                               selected = x_variables[1]
                               ),
                 ),
                 mainPanel(
                   plotOutput("plot_output")
                 )
                 )

# ui definition for SECOND module
# i.e., mapping the funding onto Sweden
tab4 <- tabPanel("Geography",
                 tabsetPanel(
                   tabPanel("Map",
                            headerPanel("Map of Sweden: "),
                            sliderInput("map_year_B",
                                        label = "Please select year to filter by: ",
                                        min = min(years),
                                        max = max(years),
                                        value = c(min(years), max(years)),
                                        sep = ""),
                            actionButton("update_map", "Update map"),
                            uiOutput("map_output_B", width="720px", height="1800px"),
                            helpText(div(h5("Legend: "), "Swecris data is shown for Stockholms universitet, KTH, Kungliga tekniska högskolan, Karolinska Institutet, Handelshögskolan i Stockholm, Södertörns högskola, Uppsala universitet, Sveriges lantbruksuniversitet, Mälardalens högskola, Linköpings universitet, Högskolan i Jönköping, Linnéuniversitetet, Linnéuniversitetet, Högskolan i Kalmar, Uppsala universitet, Blekinge Tekniska Högskola, Lunds universitet, Malmö universitet, Högskolan i Halmstad, Göteborgs universitet, Chalmers tekniska högskola, Karlstads universitet, Örebro universitet, Mälardalens Högskola, Högskolan Dalarna, Högskolan i Gävle, Mittuniversitetet, Mittuniversitetet, Umeå universitet, Luleå Tekniska Universitet, and Umeå universitet.")),
                            helpText(div(
                              "The interactive map is adapted from ",
                              tags$a("cartographyvectors.com/", href = "https://cartographyvectors.com/map/1680-sweden-counties")
                            ))
                   ),
                   tabPanel("Table",
                            helpText(div(h5("Universities and högskolor included in the map per Swedish län (i.e., administrative region)."))),
                            tableOutput("table_output"))
                 )
)

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
                     actionButton("update", "Change research field"),
                     hr(),
                     sliderInput("max_number_of_words_input",
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

# ui definition for FOURTH module
# i.e., funding by gender
tab2 <- tabPanel("Grant applicants",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("gender_input_selection",
                                 label = "Select plot type: ",
                                 choices = gender_plot_list)
                   ),
                   mainPanel(
                     uiOutput("ui")
                     )
                   )
                 )

####### END #######



###### HERE IS THE ACTUAL APP: ######
shinyApp(
  ui=navbarPage("Welcome to the FUNding application", #theme = bslib::bs_theme(primary = "#78C2AD"),
                tab1,
                tab2,
                tab3,
                tab4
    ),
  
  
  server=function(input,output,session) {
    ####### MODULE 1 [correlation]: ######
    output$plot_output <- renderPlot({
      plot_variable(small_dataset, !!sym(input$select_input_x)) + theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        title = element_text(size = 18)
      )
      })
    ###### END ######
    
    ###### MODULE 2 [map]: ######
    
    output$table_output <- renderTable(unnested_tibble_data)
    
    # Define a reactive expression for updating the year range
    update_map_range <- reactive({
      # Change when the "update" button is pressed... 
      input$update_map
      # ...but not for anything else
      isolate({
        withProgress({
          setProgress(message = "Updating map ...")
          input$map_year_B
        })
      })
    })
    
    output$map_output_B <- renderUI({
      get_years <- update_map_range()
      
      # years <- c(2012, 2013, 2014)
      yearly_2008_county <- university_yearly_sek_data %>%
        filter(FundingYear %in% get_years) %>%
        group_by(Code, County) %>%
        summarise(
          yearly_funding_sek_county = sum(yearly_funding_sek)
        ) %>% arrange(yearly_funding_sek_county)
      
      #now join
      sf_vectors <- left_join(sf_vectors, 
                              data.frame(County = yearly_2008_county$County,
                                         funding = yearly_2008_county$yearly_funding_sek_county),
                              by = 'County')  
      
      # Add the polygons to the Leaflet map
      map <- map %>%
        addPolygons(data = sf_vectors,
                    fillColor = ~pal(funding),
                    fillOpacity = 0.5,
                    color = "black",
                    weight = 1,
                    layerId = ~ID_1) 
      
      # Extract the centroids of polygons, be careful, they assumer same geometry for each polygon
      centroids <- st_centroid(sf_vectors)
      
      # Convert the centroids to a data frame
      centroids_df <- st_coordinates(centroids) %>%
        as.data.frame() %>% add_column(label = paste0("County: ", sf_vectors$County, "<br>",
                                                      "Funding: ", format(sf_vectors$funding, scientific = FALSE, big.mark = ","), " sek"))
      
      
      
      # Add labels to the polygons
      map <- map %>%
        addLabelOnlyMarkers(data = centroids_df,
                            lat = ~Y,  # column containing the latitude coordinates
                            lng = ~X,  # column containing the longitude coordinates
                            label = ~lapply(label, HTML),  # column containing the names
                            labelOptions = labelOptions(
                              noHide = TRUE,  # show labels by default
                              direction = "auto",
                              opacity = 0.8,
                              textOnly = FALSE),
                            clusterOptions = markerClusterOptions(
                              spiderfyOnMaxZoom = FALSE,
                              disableClusteringAtZoom = 25  # show labels at zoom level >= 25
                              )
                            )
      
      # Add legend
      map <- map %>% addLegend(data = sf_vectors, pal = pal, values = ~funding,
                               opacity = 0.7,
                               title = 'Funding per County',
                               position = "bottomleft") 
      
      map
      })
    
    ###### END ######
    
    ####### MODULE 3 [wordcloud]: ######
    # Define a reactive expression for updating the research field word library
    terms <- reactive({
      # Change when the "update" button is pressed...
      input$update
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
      
      get_terms <- terms()
      
      eval(as.name(get_terms)) %>% 
        {wordcloud_rep(.$word, 
                       .$nr_words, 
                       scale = c(4,0.5),
                       max.words = input$max_number_of_words_input,
                       colors=brewer.pal(8, "Dark2"))}
    })
    ###### END ######
    
    ###### MODULE 4 [gender ratio] #####
    
    # Check the input selection and render the appropriate plot
    output$ui <-  renderUI({
      if(input$gender_input_selection=="plot1") {
          output$gender_plot <- renderPlot({
            ggplot(all_university_projects_people, aes(x = gender, y = FundingsSek, fill = gender)) +
              geom_boxplot(na.rm=T) +
              labs(x = "Gender", y = "Funding (SEK)", fill = "Gender") +
              scale_y_continuous(limits = c(min_funding, max_funding), labels = scales::comma) +
              theme_classic() +
              theme(strip.text = element_blank()) +
              scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
              theme(
                axis.title = element_text(size=18),
                axis.text = element_text(size = 14),
                legend.text = element_text(size = 18)
              )
        })
      }
      else if(input$gender_input_selection=="plot2") {
        output$gender_plot <- renderPlot({
          ggplot(all_university_projects_people, aes(x = factor(FundingYear), y = FundingsSek, fill = gender)) +
            geom_boxplot(na.rm=T) +
            labs(x = "Year", y = "Funding (SEK)", fill = "Gender") +
            scale_y_continuous(limits = c(min_funding, max_funding), labels = scales::comma) +
            facet_grid(. ~ FundingYear, scales = "free_x", space = "free", switch = "y") +
            theme_classic() +
            theme(strip.text = element_blank()) +
            scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
            theme(
              axis.title = element_text(size=18),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 18)
            )
        })
      }
      else if(input$gender_input_selection=="plot3") {
        output$gender_plot <- renderPlot({
          ggplot(freq_name, aes(x = Projects, y = Frequency)) +
            geom_bar(stat = "identity", fill = "grey") +
            labs(x = "Number of projects for a given name", y = "Count") +
            theme_classic() +
            theme(axis.title.x = element_text(size = 12),
                  axis.title.y = element_text(size = 12)) +
            scale_x_continuous(breaks = seq(0, max(freq_name$Projects), by = 20)) +
            scale_y_sqrt(breaks = pretty(range(freq_name$Frequency), n = 5), expand = c(0,0)) +
            theme(
              axis.title = element_text(size=18),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 18)
            )
        })
      }
      else if(input$gender_input_selection=="plot4") {
        output$gender_plot <- renderPlot({
          ggplot(top_10_name, aes(x = fullName, y = project_counts_name)) +
            geom_bar(stat = "identity", fill = "grey") +
            labs(x = "Name of top 10 scientists", y = "number of projects") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            scale_y_continuous(expand = c(0, 0)) +
            theme(
              axis.title = element_text(size=18),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 18)
            )
        })
      }
      else if(input$gender_input_selection=="plot5") {
        output$gender_plot <- renderPlot({
          ggplot(freq_orcid, aes(x = Projects, y = Frequency)) +
            geom_bar(stat = "identity", fill = "grey") +
            labs(x = "Number of Projects for a given ORCID iD", y = "Count") +
            theme_classic() +
            theme(axis.title.x = element_text(size = 12),
                  axis.title.y = element_text(size = 12)) +
            scale_x_continuous(breaks = seq(0, 120, by = 20), limits = c(0, 80)) +
            scale_y_sqrt(breaks = c(10, 100, 1000, 10000), expand = c(0, 0), limits = c(0, 10000)) +
            theme(
              axis.title = element_text(size=18),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 18)
            )
        })
      }
      else if(input$gender_input_selection=="plot6") {
        output$gender_plot <- renderPlot({
          ggplot(top_10_orcid, aes(x = fullName, y = project_counts_orcid)) +
            geom_bar(stat = "identity", fill = "grey") +
            labs(x = "Name of top 10 scientists", y = "number of projects") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            scale_y_continuous(expand = c(0, 0)) +
            theme(
              axis.title = element_text(size=18),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 18)
            )
        })
      }
    })
    ###### END ######
    
    
  })
###### END ######