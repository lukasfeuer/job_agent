# -------------------------------------------------------------------------
#
#             HDI - Jobportal
#
# -------------------------------------------------------------------------
# 
# Filter: Hannover & Berufseinsteiger

library(tibble)
library(readr)
library(dplyr)
library(httr)
library(jsonlite)

hdi_open <- GET("https://careers.hdi.group/api/jobs/v1/de/jobs?page=1&pageSize=100&location=Hannover&level=Berufseinsteiger%3Ainnen&company=HDI%20AG",
                verbose())

hdi_open <- hdi_open %>%
  content(as = "text") %>%
  fromJSON()

hdi_open <- hdi_open$jobs

hdi_open <- hdi_open %>% 
  tibble() %>% 
  select(id, jobTitle, httpLink) %>% 
  rename(ID = 1, Title = 2, URI = 3)

## Compare open jobs with known jobs
hdi_hist <- read_csv2("data/hdi_temp.csv")

hdi_new <- hdi_open %>% 
  anti_join(hdi_hist) %>% 
  mutate(Erfasst = lubridate::today())

if (nrow(hdi_new) > 0) {
  hdi_complete <- hdi_hist %>% 
    bind_rows(hdi_new)
  
  # TODO Spalte mit Zeitpunt des ersten Abrufs
  write_csv2(hdi_complete, "data/hdi_temp.csv")
}



