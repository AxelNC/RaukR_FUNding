library(devtools)
library(tidyverse)
library(future)
library(swecris)

################################################################################
# get new dataframe only with the involved people and proj id
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
























