library(tidyverse)
library(ggplot2)

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


####function

plot_function <- function(x ,var, var_name, scale_number, scale_label) {
  df <- x %>% group_by(var) %>% 
    summarise(Total_funding = sum(FundingsSek)) %>%
    arrange(desc(Total_funding)) %>%
    slice(1:5)
  
  df %>%
    ggplot(aes(x = reorder(var, +Total_funding), y = Total_funding)) +
    geom_bar(stat="identity", fill="steelblue") +
    xlab(paste0("Total funding by ", var_name)) +
    scale_y_continuous(name = "Total Funding (billion SEK)",
                       breaks = seq(from = 0, to = max(df$Total_funding), by = scale_number),
                       labels = function(x) paste0(x / scale_label)) +
    theme(axis.text.y = element_text(angle = 30, hjust = 1)) +
    coord_flip()
}

plot_function(x = projdata ,var = "FundingOrganisationNameEn", "funding organization", 20000000000, 1000000000)

###autoplot

projdata %>% group_by(FundingYear) %>% 
  summarise(Total_funding = sum(FundingYear)) %>%
  data.frame %>%
  autoplot()
