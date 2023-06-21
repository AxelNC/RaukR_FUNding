library(tidyverse)
library(ggthemes)

projdata <- readxl::read_excel("C://Users/MauricioRoza/Desktop/all_university_projects.xlsx")
all_university_projects <- projdata

projdata <- projdata %>%
  mutate_at(c('FundingYear', 'FundingsSek'), as.integer)

projdata <- projdata %>%
  mutate_at(c('CoordinatingOrganisationNameEn', 'CoordinatingOrganisationTypeOfOrganisationEn'), as.factor)

#######fix legends

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

plot_variable(projdata, CoordinatingOrganisationTypeOfOrganisationEn)
plot_variable(projdata, CoordinatingOrganisationNameEn)
plot_variable(projdata, FundingOrganisationNameEn)
plot_variable(projdata, TypeOfAwardDescrEn)
plot_variable(projdata, FundingYear)

names(projdata)

# dput(projdata[c(sample(1:nrow(projdata), 5)), c("ProjectId", "FundingsSek", "Scbs")])
# 
# involved_people <- read.csv("involved_people_projID.csv", sep = ";")
# 
# scbs.codes <- read.csv("scbs_codes_projID.csv", sep = ";")
# scbs.codes$scb_sv_en %>% unique %>% length
# names(scbs.codes)
# 
# df <- projdata
# 
# df_merged <- df %>%
#   left_join(scbs.codes.mini, by = c("ProjectId" = "ProjectId"))
# #only numbers
# df_merged$scb_sv_en %>% unique
# 
# top_5 <- df_merged %>%
#   group_by(scb_sv_en) %>%
#   summarise(Total_funding = sum(FundingsSek)) %>%
#   arrange(desc(Total_funding)) %>% na.omit %>%
#   top_n(5, Total_funding) %>%
#   select(scb_sv_en)
# 
# top_5 <- as.vector(top_5[[1]])
# 
# df <- df_merged %>%
#   group_by(FundingYear, scb_sv_en) %>%
#   summarise(Total_funding = sum(FundingsSek)) %>%
#   arrange(desc(Total_funding)) %>%
#   filter(scb_sv_en %in% top_5)
# 
# df %>% ggplot(aes(x = FundingYear, y = Total_funding, fill = scb_sv_en, col = scb_sv_en)) +
#   geom_line() +
#   geom_point() +
#   ggtitle("Sweden Research Funding by Year") +
#   scale_x_continuous(name = "Year",
#                      breaks = seq(from = min(df$FundingYear), to = max(df$FundingYear), by = 2)) +
#   scale_y_continuous(name = "Total Funding (billion SEK)",
#                      breaks = 
#                        if (max(df$Total_funding) < 5000000000) {
#                          seq(from = 0, to = max(df$Total_funding)*1.1, by = 500000000)
#                        }
#                      else if (max(df$Total_funding) > 9000000000) {
#                        seq(from = 0, to = max(df$Total_funding)*1.1, by = 2000000000)
#                      }
#                      else {
#                        seq(from = 0, to = max(df$Total_funding)*1.1, by = 1000000000)
#                      },
#                      labels = function(x) paste0(x / 1000000000)) +
#   theme_hc()+ scale_colour_hc() +
#   theme(legend.position = "bottom",
#         legend.title = element_blank())
