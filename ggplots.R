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
projdata %>% group_by(FundingYear) %>% 
  summarise(Total_funding = sum(FundingsSek)) %>%
  ggplot(aes(x = FundingYear, y = Total_funding)) +
  geom_line() +
  geom_point() +
  ggtitle("Sweden Research Funding by Year") +
  scale_x_continuous(name="Year",
                     breaks = seq(from = 2008, to= 2024, by = 2)) +
  scale_y_continuous(name = "Total Funding (billion SEK)",
                     breaks = seq(from = 0, to = 14000000000, by = 1000000000),
                     labels = function(x) paste0(x / 1000000000))

#University

projdata %>% group_by(CoordinatingOrganisationNameEn) %>% 
  summarise(Total_funding = sum(FundingsSek)) %>%
  arrange(desc(Total_funding)) %>%
  slice(1:5) %>%
  ggplot(aes(x = reorder(CoordinatingOrganisationNameEn, +Total_funding), y = Total_funding)) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("Top 5 Sweden Institutions by total funding") +
  scale_y_continuous(name = "Total Funding (billion SEK)",
                     breaks = seq(from = 0, to = 20755721767, by = 5000000000),
                     labels = function(x) paste0(x / 5000000000)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  coord_flip()




###autoplot

projdata %>% group_by(FundingYear) %>% 
  summarise(Total_funding = sum(FundingYear)) %>%
  data.frame %>%
  autoplot()
