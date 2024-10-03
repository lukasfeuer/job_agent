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

hdi_response <- list()

tryCatch(
  hdi_response <- GET("https://careers.hdi.group/api/jobs/v1/de/jobs?page=1&pageSize=100&location=Hannover&level=Berufseinsteiger%3Ainnen&company=HDI%20AG"
                  #, verbose()
                  )
  , error = function(e) {message("API call to careers.hdi.group failed!")}()
  , finally = message("Finished GET")
)

if (!is_empty(hdi_response) & hdi_response$status_code == 200) {
  message("Response not empty, continue processing")
} else {
  stop("Empty response for careers.hdi.group")
}

hdi_open <- hdi_response |> 
  content(as = "text") |> 
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
  message("New jobs for HDI found")
  write_csv2(hdi_complete, "data/hdi_temp.csv")
  message("New entries saved to HDI file")
} else {
  message("No new jobs for HDI found")
}