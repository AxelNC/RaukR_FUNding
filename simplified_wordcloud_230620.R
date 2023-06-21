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
#save the word freq of all words
book_all_count  <-  token_wo_stopwords %>% 
  count(word, sort = TRUE, name = "nr_words") %>%  
  slice_max(order_by = nr_words, n = 300)
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
   proj_nest2, proj_nest2.small, scbs.codes, tokenized, 
   swecris_all_university_projects, scbs.codes.mini)
#DO NOT SAVE# LOAD after this####
##########################################################################
#save books in separate files
library(tictoc)
tic("book_1")
book_1 <- token_wo_stopwords %>% filter(grepl("1", scbs.codes.mini))
toc() #book_1: 165.9 sec elapsed

#write.csv2(file = "book_1.csv", x = book_1);rm(book_1)
#2
tic("book_2")
book_2 <- token_wo_stopwords %>% filter(grepl("2", scbs.codes.mini))
toc() #book_2: 162.78 sec elapsed
#write.csv2("book_2.csv", book_2);rm(book_2)
#3
tic("book_3")
book_3 <- token_wo_stopwords %>% filter(grepl("3", scbs.codes.mini))
toc()
#write.csv2("book_3.csv", book_3);rm(book_3)
#4
tic("book_4")
book_4 <- token_wo_stopwords %>% filter(grepl("4", scbs.codes.mini))
toc()
#write.csv2("book_4.csv", book_4);rm(book_4)
#5
tic("book_5")
book_5 <- token_wo_stopwords %>% filter(grepl("5", scbs.codes.mini))
toc()
#write.csv2("book_5.csv", book_5);rm(book_5)
#6
tic("book_6")
book_6 <- token_wo_stopwords %>% filter(grepl("6", scbs.codes.mini))
toc()
#write.csv2("book_6.csv", book_6);rm(book_6)
#9
tic("book_9")
book_9 <- token_wo_stopwords %>% filter(grepl("9", scbs.codes.mini))
toc()
#write.csv2("book_9.csv", book_9);rm(book_9)
#save as RDS
save(book_1, book_2, book_3, book_4, 
     book_5, book_6, book_9,  file = "wordcloud_categories.RData")
#remove books
rm(book_1, book_2, book_3, book_4, book_5, book_6, book_9)
#####################################################################
#Load Books#######
load("wordcloud_categories.RData")
#####################################################################
book_1_count  <-  book_1 %>% 
  count(word, sort = TRUE, name = "nr_words") %>%  
  slice_max(order_by = nr_words, n = 300)

book_2_count  <-  book_2 %>% 
  count(word, sort = TRUE, name = "nr_words") %>%  
  slice_max(order_by = nr_words, n = 300)

book_3_count  <-  book_3 %>% 
  count(word, sort = TRUE, name = "nr_words") %>%  
  slice_max(order_by = nr_words, n = 300)

book_4_count  <-  book_4 %>% 
  count(word, sort = TRUE, name = "nr_words") %>%  
  slice_max(order_by = nr_words, n = 300)

book_5_count  <-  book_5 %>% 
  count(word, sort = TRUE, name = "nr_words") %>%  
  slice_max(order_by = nr_words, n = 300)

book_6_count  <-  book_6 %>% 
  count(word, sort = TRUE, name = "nr_words") %>%  
  slice_max(order_by = nr_words, n = 300)

book_9_count  <-  book_9 %>% 
  count(word, sort = TRUE, name = "nr_words") %>%  
  slice_max(order_by = nr_words, n = 300)
rm(book_1, book_2, book_3, book_4, book_5, book_6, book_9)
#####################################################################
#load("wordcloud_categories.RData")
####################################################################
save(book_all_count, book_1_count, book_2_count, book_3_count, book_4_count, 
     book_5_count, book_6_count, book_9_count, 
     file = "wordcloud_categories_count.RData")
#####################################################################
#load
load("wordcloud_categories_count.RData")
#####################################################################
library(wordcloud)
library(tm)
#make wordcloud from data
# category 1
book_1_count %>% {wordcloud(.$word, .$nr_words, max.words = 50)}
