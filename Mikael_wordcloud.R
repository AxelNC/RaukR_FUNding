library(devtools)
#install_github("KTH-Library/swecris", dependencies = T, force=T)
New
10:56
#install.packages(c("httr", "jsonlite"))
library(httr) #easy crafting of API calls
library(jsonlite) #easy handling of JSON (JavaScript Object Notation) data
library(tidyverse)

#install_github("KTH-Library/swecris", dependencies = T, force=T)
l
library(swecris)
allfunding<-swecris::swecris_fundings()


#I modified the following function from swecris, and removed the search string that specified KTH
#kth_projects <- swecris_funding()

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
all_university_projects %>% head
all_university_projects$ProjectId
all_university_projects$InvolvedPeople
all_university_projects$InvolvedPeople

swecris::parse_involved_people(all_university_projects$InvolvedPeople[4])
swecris::parse_scb_codes(all_university_projects$Scbs[4])
swecris::
all_university_projects$ProjectAbstractEn[1000]
unnest_tokens
###################################################################

library(tidytext)
#maake small scbcode df
scbs.codes.mini <- scbs.codes %>% filter(scb_code < 10) 
#join with scbcodes
all_university_projects_scb <- left_join(all_university_projects, scbs.codes.mini, by = "ProjectId")

small.df <- all_university_projects_scb[1:20000,]
small.df %>% colnames
text.tibble <- small.df %>%  tibble()
all_university_projects_scb %>% select(scb_code)
#make a row of each word for each abstract
tokenized <- text.tibble %>% unnest_tokens(word, ProjectAbstractEn)
#delete so called stop-words
token_wo_stopwords <- tokenized %>% anti_join(stop_words)
token_wo_stopwords
#what are the most common words?
token_wo_stopwords %>% 
  group_by(scb_code) %>% 
  count(word, sort = TRUE) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ggplot(aes(x = n, y = fct_reorder(word, n), fill = word)) + 
    geom_col(show.legend = FALSE) + 
  scale_colour_brewer(type = "qual", 
                      palette = "RdYlBu") + 
  theme_light()

token_wo_stopwords %>% filter()
token_wo_stopwords %>% glimpse

token_wo_stopwords_withscb <- left_join(x = token_wo_stopwords, 
          y = scbs.codes, 
          by = "ProjectId",
          relationship = "many-to-many")

token_wo_stopwords_withscb %>% select(ProjectId, word, scb_code) %>% head()
token_wo_stopwords_withscb 
#make wordclouds
library("ggwordcloud")
data("love_words_small")
token_wo_stopwords %>% 
  count(word, sort = TRUE) %>% 
  slice_max(order_by = n, n = 40) %>% 
  ggplot(aes(label = word, size = n)) +
  geom_text_wordcloud()

#########################################################################
all_university_projects
involved.people
scbs.codes
#add filtered column for the second column
scbs.codes.mini <- scbs.codes %>% filter(scb_code <10)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(tidyr)
library(magrittr)
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
token_wo_stopwords %>% glimpse()
#make a setup for wordcloud in rshiny
  #make a vector with the different fields
token_wo_stopwords %>% unnest(scbs.codes.mini) %$% unique(scb_code)
token_wo_stopwords %>% select(ProjectId, word)

token_wo_stopwords %>% glimpse()

####################################################################
token_wo_stopwords %>%
  group_by(scbs.codes.mini[1], 
           CoordinatingOrganisationTypeOfOrganisationEn) %>%  
  count(word, sort = TRUE) %>% 
  unnest("scbs.codes.mini[1]")


token_wo_stopwords %>% str

token_wo_stopwords %>% filter()
