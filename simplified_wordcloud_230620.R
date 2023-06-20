library(tidytext)
library(tidyverse)
library(wordcloud)
library(tidyr)
library(magrittr)
library(devtools)
library(httr)
library(jsonlite)
library(tidyverse)
library(swecris)

#make a function to download data from swecris
swecris_all_university_projects <- function (searchstring = "", 
                                             token) 
{
  if (missing(token)) 
    token <- "RWNDZ3FDRVVSMmNUNlZkMkN3"
  httr::GET("https://swecris-api.vr.se/v1/scp/export", query = list(`organizationType[]` = "Universitet", 
                                                                    sortOrder = "desc", sortColumn = "FundingStartDate", 
                                                                    searchText = URLencode(searchstring), token = token)) %>% 
    httr::content(as = "text", encoding = "UTF-8") %>% readr::read_delim(delim = ";", 
                                                                         quote = "\"", show_col_types = FALSE)
}
#get the data to an object
all_university_projects <- swecris_all_university_projects()
#load the other files
involved.people <- read.csv2("involved_people_projID.csv")
scbs.codes <- read.csv2("scbs_codes_projID.csv")

#add filtered column for the second column
scbs.codes.mini <- scbs.codes %>% filter(scb_code <10)
#nest by people
proj_nest1<- nest_join(x = all_university_projects, y = involved.people, by = "ProjectId")
#add nest by scbcodes (reduced to codes <10)
proj_nest2 <- nest_join(x = proj_nest1, y = scbs.codes.mini, by = "ProjectId")
#view data
proj_nest2 %>% glimpse()
#make a tibble with fewer cols
proj_nest2.small<- proj_nest2 %>% select(ProjectId,
                                         ProjectAbstractEn,
                                         CoordinatingOrganisationNameEn, 
                                         FundingsSek,
                                         FundingYear,
                                         CoordinatingOrganisationTypeOfOrganisationEn,
                                         FundingOrganisationTypeOfOrganisationEn,
                                         involved.people, 
                                         scbs.codes.mini)


#make a row of each word for each abstract
tokenized <- proj_nest2.small %>% unnest_tokens(word, 
                                                input = ProjectAbstractEn, 
                                                to_lower = T)
#delete so called stop-words
token_wo_stopwords <- tokenized %>% anti_join(stop_words)
#
#make a df of scb categories to filter on
scbcat_df<- scbs.codes %>% 
  filter(scb_code<10) %>% 
  distinct(scb_code,.keep_all = T) %>% 
  select(-ProjectId) %>% 
  arrange(scb_code)

#vector of numbers
scbcat_filter_vec <- scbcat_df$scb_code
#
rm(all_university_projects, involved.people, proj_nest1, 
   proj_nest2, proj_nest2.small, scbs.codes, tokenized, swecris_all_university_projects)


