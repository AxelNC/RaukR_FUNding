library(devtools)
library(tidyverse)
library(future)
install_github("KTH-Library/swecris", dependencies = TRUE)
library(swecris)

################################################################################

#### I modified the following function from swecris, and removed the search string that specified KTH
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

#### get the data to an objectc()
all_university_projects <- swecris_all_university_projects()

#### get new dataframe only with the involved people and proj id
all_university_projects_people <- all_university_projects %>%
  select(ProjectId, InvolvedPeople)

#### check number of cores
unname(availableCores())

#### parallel pipeline using 6 cores
t1 = proc.time()
plan(multisession)

a1 %<-% {
  n_times = 1
  inv.people <- tibble()
  for (i in all_university_projects_people$InvolvedPeople[1:10000]) {
    temp.inv.people <- tibble(swecris::parse_involved_people(i)) %>%
      add_column(.data = data.frame(ProjectId = rep(all_university_projects_people$ProjectId[n_times],
                                                    times = nrow(swecris::parse_involved_people(i)))))
    inv.people <- bind_rows(inv.people, temp.inv.people)
    n_times = n_times + 1
  }
  inv.people
} %seed% 42 # Use the %seed% to specify the same seed for all of them, for random number gen

a2 %<-% {
  n_times = 1
  inv.people <- tibble()
  for (i in all_university_projects_people$InvolvedPeople[10001:20000]) {
    temp.inv.people <- tibble(swecris::parse_involved_people(i)) %>%
      add_column(.data = data.frame(ProjectId = rep(all_university_projects_people$ProjectId[n_times],
                                                    times = nrow(swecris::parse_involved_people(i)))))
    inv.people <- bind_rows(inv.people, temp.inv.people)
    n_times = n_times + 1
  }
  inv.people
} %seed% 42 # Use the %seed% to specify the same seed for all of them, for random number gen

a3 %<-% {
  n_times = 1
  inv.people <- tibble()
  for (i in all_university_projects_people$InvolvedPeople[20001:30000]) {
    temp.inv.people <- tibble(swecris::parse_involved_people(i)) %>%
      add_column(.data = data.frame(ProjectId = rep(all_university_projects_people$ProjectId[n_times],
                                                    times = nrow(swecris::parse_involved_people(i)))))
    inv.people <- bind_rows(inv.people, temp.inv.people)
    n_times = n_times + 1
  }
  inv.people
} %seed% 42 # Use the %seed% to specify the same seed for all of them, for random number gen

a4 %<-% {
  n_times = 1
  inv.people <- tibble()
  for (i in all_university_projects_people$InvolvedPeople[30001:40000]) {
    temp.inv.people <- tibble(swecris::parse_involved_people(i)) %>%
      add_column(.data = data.frame(ProjectId = rep(all_university_projects_people$ProjectId[n_times],
                                                    times = nrow(swecris::parse_involved_people(i)))))
    inv.people <- bind_rows(inv.people, temp.inv.people)
    n_times = n_times + 1
  }
  inv.people
} %seed% 42 # Use the %seed% to specify the same seed for all of them, for random number gen

a5 %<-% {
  n_times = 1
  inv.people <- tibble()
  for (i in all_university_projects_people$InvolvedPeople[40001:50000]) {
    temp.inv.people <- tibble(swecris::parse_involved_people(i)) %>%
      add_column(.data = data.frame(ProjectId = rep(all_university_projects_people$ProjectId[n_times],
                                                    times = nrow(swecris::parse_involved_people(i)))))
    inv.people <- bind_rows(inv.people, temp.inv.people)
    n_times = n_times + 1
  }
  inv.people
} %seed% 42 # Use the %seed% to specify the same seed for all of them, for random number gen

a6 %<-% {
  n_times = 1
  inv.people <- tibble()
  for (i in all_university_projects_people$InvolvedPeople[50001:55318]) {
    temp.inv.people <- tibble(swecris::parse_involved_people(i)) %>%
      add_column(.data = data.frame(ProjectId = rep(all_university_projects_people$ProjectId[n_times],
                                                    times = nrow(swecris::parse_involved_people(i)))))
    inv.people <- bind_rows(inv.people, temp.inv.people)
    n_times = n_times + 1
  }
  inv.people
} %seed% 42 # Use the %seed% to specify the same seed for all of them, for random number gen

involved.people <- bind_rows(a1, a2, a3, a4, a5, a6)

t2 = proc.time()
t2 - t1

write_csv2(involved.people, "involved_people_projID.csv")

################################################################################
# get new dataframe only with the Scbs and proj id
all_university_projects_scbs <- all_university_projects %>%
  select(ProjectId, Scbs)

#### parallel pipeline using 6 cores
t1 = proc.time()
plan(multisession)

a1 %<-% {
  n_times = 1
  scbs.codes <- tibble()
  for (i in all_university_projects_scbs$Scbs[1:10000]) {
    temp.scbs.codes <- tibble(swecris::parse_scb_codes(i)) %>%
      add_column(.data = data.frame(ProjectId = rep(all_university_projects_scbs$ProjectId[n_times],
                                                    times = nrow(swecris::parse_scb_codes(i)))))
    scbs.codes <- bind_rows(scbs.codes, temp.scbs.codes)
    n_times = n_times + 1
  }
  scbs.codes
} %seed% 42 # Use the %seed% to specify the same seed for all of them, for random number gen

a2 %<-% {
  n_times = 1
  scbs.codes <- tibble()
  for (i in all_university_projects_scbs$Scbs[10001:20000]) {
    temp.scbs.codes <- tibble(swecris::parse_scb_codes(i)) %>%
      add_column(.data = data.frame(ProjectId = rep(all_university_projects_scbs$ProjectId[n_times],
                                                    times = nrow(swecris::parse_scb_codes(i)))))
    scbs.codes <- bind_rows(scbs.codes, temp.scbs.codes)
    n_times = n_times + 1
  }
  scbs.codes
} %seed% 42 # Use the %seed% to specify the same seed for all of them, for random number gen

a3 %<-% {
  n_times = 1
  scbs.codes <- tibble()
  for (i in all_university_projects_scbs$Scbs[20001:30000]) {
    temp.scbs.codes <- tibble(swecris::parse_scb_codes(i)) %>%
      add_column(.data = data.frame(ProjectId = rep(all_university_projects_scbs$ProjectId[n_times],
                                                    times = nrow(swecris::parse_scb_codes(i)))))
    scbs.codes <- bind_rows(scbs.codes, temp.scbs.codes)
    n_times = n_times + 1
  }
  scbs.codes
} %seed% 42 # Use the %seed% to specify the same seed for all of them, for random number gen

a4 %<-% {
  n_times = 1
  scbs.codes <- tibble()
  for (i in all_university_projects_scbs$Scbs[30001:40000]) {
    temp.scbs.codes <- tibble(swecris::parse_scb_codes(i)) %>%
      add_column(.data = data.frame(ProjectId = rep(all_university_projects_scbs$ProjectId[n_times],
                                                    times = nrow(swecris::parse_scb_codes(i)))))
    scbs.codes <- bind_rows(scbs.codes, temp.scbs.codes)
    n_times = n_times + 1
  }
  scbs.codes
} %seed% 42 # Use the %seed% to specify the same seed for all of them, for random number gen

a5 %<-% {
  n_times = 1
  scbs.codes <- tibble()
  for (i in all_university_projects_scbs$Scbs[40001:50000]) {
    temp.scbs.codes <- tibble(swecris::parse_scb_codes(i)) %>%
      add_column(.data = data.frame(ProjectId = rep(all_university_projects_scbs$ProjectId[n_times],
                                                    times = nrow(swecris::parse_scb_codes(i)))))
    scbs.codes <- bind_rows(scbs.codes, temp.scbs.codes)
    n_times = n_times + 1
  }
  scbs.codes
} %seed% 42 # Use the %seed% to specify the same seed for all of them, for random number gen

a6 %<-% {
  n_times = 1
  scbs.codes <- tibble()
  for (i in all_university_projects_scbs$Scbs[50001:55318]) {
    temp.scbs.codes <- tibble(swecris::parse_scb_codes(i)) %>%
      add_column(.data = data.frame(ProjectId = rep(all_university_projects_scbs$ProjectId[n_times],
                                                    times = nrow(swecris::parse_scb_codes(i)))))
    scbs.codes <- bind_rows(scbs.codes, temp.scbs.codes)
    n_times = n_times + 1
  }
  scbs.codes
} %seed% 42 # Use the %seed% to specify the same seed for all of them, for random number gen

scbs.codes <- bind_rows(a1, a2, a3, a4, a5, a6)

t2 = proc.time()
t2 - t1

write_csv2(scbs.codes, "scbs_codes_projID.csv")


grouped.scbs.codes <- group_by(scbs.codes, ProjectId)

################################################################################
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

library(tidyverse)

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


################################################################################
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





