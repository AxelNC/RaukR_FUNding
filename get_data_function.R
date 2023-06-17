library(devtools)
install_github("KTH-Library/swecris", dependencies = TRUE)
library(swecris)
library(dplyr)

#I modified the following function from swecris, and 
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

all_university_projects <- swecris_all_university_projects()