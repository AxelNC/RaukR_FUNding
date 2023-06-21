library(tidyverse)
library(ggthemes)

projdata <- readxl::read_excel("C://Users/MauricioRoza/Desktop/all_university_projects.xlsx")

projdata <- projdata %>%
  mutate_at(c('FundingYear', 'FundingsSek'), as.integer)

str(projdata$CoordinatingOrganisationNameEn)

projdata <- projdata %>%
  mutate_at(c('CoordinatingOrganisationNameEn', 'CoordinatingOrganisationTypeOfOrganisationEn'), as.factor)

#total by year
projdata %>% group_by(FundingYear) %>% 
  summarise(Total_funding = sum(FundingYear)) %>%
  ggplot(aes(x=FundingYear, y=Total_funding)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=FundingYear), vjust=-0.3, size=3.5)+
  theme_minimal()


#year
per_year <- projdata %>% group_by(FundingYear) %>% 
  summarise(Total_funding = sum(FundingsSek))
  
per_year %>% ggplot(aes(x = FundingYear, y = Total_funding)) +
  geom_line() +
  geom_point() +
  ggtitle("Sweden Research Funding by Year") +
  scale_x_continuous(name="Year",
                     breaks = seq(from = min(per_year$FundingYear), to= max(per_year$FundingYear), by = 2)) +
  scale_y_continuous(name = "Total Funding (billion SEK)",
                     breaks = seq(from = 0, to = max(per_year$Total_funding), by = 1000000000),
                     labels = function(x) paste0(x / 1000000000))

#University

per_univ <- projdata %>% group_by(CoordinatingOrganisationNameEn) %>% 
  summarise(Total_funding = sum(FundingsSek)) %>%
  arrange(desc(Total_funding)) %>%
  slice(1:5)


per_univ %>% ggplot(aes(x = reorder(CoordinatingOrganisationNameEn, +Total_funding), y = Total_funding)) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("Top 5 Sweden Institutions by total funding") +
  scale_y_continuous(name = "Total Funding (billion SEK)",
                     breaks = seq(from = 0, to = max(per_univ$Total_funding), by = 5000000000),
                     labels = function(x) paste0(x / 1000000000)) +
  theme(axis.text.y = element_text(angle = 30, hjust = 1)) +
  coord_flip()

#Organization type

per_orgtype <- projdata %>% group_by(CoordinatingOrganisationTypeOfOrganisationEn) %>% 
  summarise(Total_funding = sum(FundingsSek)) %>%
  arrange(desc(Total_funding))

per_orgtype %>%
  ggplot(aes(x = reorder(CoordinatingOrganisationTypeOfOrganisationEn, +Total_funding), y = Total_funding)) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("Total funding by type of organization") +
  scale_y_continuous(name = "Total Funding (billion SEK)",
                     breaks = seq(from = 0, to = max(per_orgtype$Total_funding), by = 20000000000),
                     labels = function(x) paste0(x / 1000000000)) +
  theme(axis.text.y = element_text(angle = 30, hjust = 1)) +
  coord_flip()

per_funder <- projdata %>% group_by(FundingOrganisationNameEn) %>% 
  summarise(Total_funding = sum(FundingsSek)) %>%
  arrange(desc(Total_funding)) %>%
  slice(1:5)

per_funder %>%
  ggplot(aes(x = reorder(FundingOrganisationNameEn, +Total_funding), y = Total_funding)) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("Total funding by funding organization") +
  scale_y_continuous(name = "Total Funding (billion SEK)",
                     breaks = seq(from = 0, to = max(per_funder$Total_funding), by = 20000000000),
                     labels = function(x) paste0(x / 1000000000)) +
  theme(axis.text.y = element_text(angle = 30, hjust = 1)) +
  coord_flip()

########################function to implement

############## chatgpt solution

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

# Usage example:
plot_variable(projdata, CoordinatingOrganisationTypeOfOrganisationEn)

plot_variable(projdata, CoordinatingOrganisationNameEn)

plot_variable(projdata, FundingOrganisationNameEn)

plot_variable(projdata, TypeOfAwardDescrEn)

plot_variable(projdata, FundingYear)


names(projdata)


###################implement function to work with funding year

plot_variable <- function(data, variable) {
  variable_name <- deparse(substitute(variable))
  variable_vector <- c(variable_name)
  
  if (variable_vector == "FundingYear") {
    per_year <- data %>%
      group_by({{ variable }}) %>%
      summarise(Total_funding = sum(FundingsSek))
  
    per_year %>% ggplot(aes(x = {{ variable }}, y = Total_funding)) +
        geom_line(aes(col = "blue")) +
        geom_point() +
        ggtitle("Sweden Research Funding by Year") +
        scale_x_continuous(name = "Year",
                           breaks = seq(from = min(per_year$FundingYear), to = max(per_year$FundingYear), by = 2)) +
        scale_y_continuous(name = "Total Funding (billion SEK)",
                           breaks = seq(from = 0, to = max(per_year$Total_funding), by = 1000000000),
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
  
  per_univ <- data %>%
    group_by(FundingYear, {{ variable }}) %>%
    summarise(Total_funding = sum(FundingsSek)) %>%
    arrange(desc(Total_funding)) %>%
    filter({{ variable }} %in% top_5)
  
  per_univ %>% ggplot(aes(x = FundingYear, y = Total_funding, fill = {{ variable }}, col = {{ variable }})) +
    geom_line() +
    geom_point() +
    ggtitle("Sweden Research Funding by Year") +
    scale_x_continuous(name = "Year",
                       breaks = seq(from = min(per_univ$FundingYear), to = max(per_univ$FundingYear), by = 2)) +
    scale_y_continuous(name = "Total Funding (billion SEK)",
                       breaks = seq(from = 0, to = max(per_univ$Total_funding), by = 1000000000),
                       labels = function(x) paste0(x / 1000000000)) +
    theme_hc()+ scale_colour_hc() +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
  }
}

plot_variable(projdata, CoordinatingOrganisationTypeOfOrganisationEn)
plot_variable(projdata, CoordinatingOrganisationNameEn)
plot_variable(projdata, FundingOrganisationNameEn)
plot_variable(projdata, TypeOfAwardDescrEn)
plot_variable(projdata, FundingYear)

names(projdata)


#######updated

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

plot_variable(projdata, CoordinatingOrganisationTypeOfOrganisationEn)
plot_variable(projdata, CoordinatingOrganisationNameEn)
plot_variable(projdata, FundingOrganisationNameEn)
plot_variable(projdata, TypeOfAwardDescrEn)
plot_variable(projdata, FundingYear)

names(projdata)

#######test
