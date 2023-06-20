

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

tibble_data <- enframe(lan_data, name = "County", value = "University")

print(tibble_data)


library(dplyr)
library(tibble)

tibble_data <- tibble_data %>%
  mutate(Code = substr(County, 1, 2))

print(tibble_data)

library(tidyverse)

AA_all_swecris_data  %>% filter(CoordinatingOrganisationTypeOfOrganisationSv) %>% summarise()
  
  
  filter(CoordinatingOrganisationTypeOfOrganisationEn == “University”) %>%
  select("fundingOrganisationNameSv") %>%
  group_by(fundingOrganisationNameSv) %>% summarise()

####Glöm inte att göra en liten asterix som anger att finansieringskartan baseras på finansiering till listade universitet och högskolor