library(tidyverse)
library(dummytools)

projdata <- readxl::read_excel("C://Users/MauricioRoza/Desktop/all_university_projects.xlsx")
all_university_projects <- projdata

projdata <- projdata %>%
  mutate_at(c('FundingYear', 'FundingsSek'), as.integer)

projdata <- projdata %>%
  mutate_at(c('CoordinatingOrganisationNameEn', 'CoordinatingOrganisationTypeOfOrganisationEn'), as.factor)

scbs.codes <- read.csv("scbs_codes_projID.csv", sep = ";")
scbs.codes.mini <- scbs.codes %>% filter(scb_code <10)

df <- projdata

# Split the Scbs column into separate categories
df$Scbs2 <- strsplit(df$Scbs, "¤¤¤ ")

# Remove the empty string at the beginning of each list
df$Scbs2 <- lapply(df$Scbs2, function(x) x[x != ""])

# Extract only the first category after a single number followed by a colon
df$Scbs2 <- lapply(df$Scbs2, function(x) sub("\\d+:\\s*(.*?),.*", "\\1", x))

df$Scbs2 <- lapply(df$Scbs2, unique)

dput(df[1:10, c("ProjectId","Scbs2")])

#dummy variables

df2 <- df %>%
  mutate(Scbs2 = map(Scbs2, as.character)) %>%
  unnest(Scbs2) %>%
  mutate(Scbs2 = as.factor(Scbs2)) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = Scbs2, values_from = value, values_fill = 0)

# Remove the prefix from variable names
names(df2)[-1] <- gsub("Scbs2_", "", names(df)[-1])
