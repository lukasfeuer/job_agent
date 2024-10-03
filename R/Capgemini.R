# -------------------------------------------------------------------------
#
#             Capgemini - Jobportal
#
# -------------------------------------------------------------------------

library(tibble)
library(readr)
library(dplyr)
library(stringr)
library(httr)
library(rvest)
library(jsonlite)


cap_open <-  GET("https://www.capgemini.com/de-de/jobs/?search_term=data&filter_contract_type=unbefristet&filter_location=hannover", 
            verbose())

x <- GET("https://www.capgemini.com/de-de/jobs/?search_term=Data&filter_contract_type=unbefristet&filter_location=hannover")
re <- content(x)
fromJSON(re)

cap_open <- cap_open %>% 
  content(as = "parsed") 

cap_open %>% write_lines("test.html")

cap_open %>% 
  html_elements("h3.card_default__title")


cap_open <- cap_open %>% 
  tibble() %>% 
  select(ID, everything(), -c(PositionLocation, JobCategory)) %>% 
  filter(!str_detect(PositionTitle, c("Werkstud|Praktik|praktiku|Internship"))) %>% 
  rename(ID = 1, Title = 2, URI = 3)

## Compare open jobs with known jobs
cap_hist <- read_csv2("data/cap_temp.csv")

cap_new <- cap_open %>% 
  anti_join(cap_hist) %>% 
  mutate(Erfasst = today())

cap_complete <- cap_hist %>% 
  bind_rows(cap_new)

# TODO Spalte mit Zeitpunt des ersten Abrufs
write_csv2(cap_complete, "data/cap_temp.csv")