# -------------------------------------------------------------------------
#
#             Hannover Rück - Jobportal
#
# -------------------------------------------------------------------------
#
# hann_response <- GET("https://jobs.hannover-re.com/search/?createNewAlert=false&q=&optionsFacetsDD_customfield4=&optionsFacetsDD_customfield1=&optionsFacetsDD_country=&optionsFacetsDD_customfield3=") 
# hann_response |> 
#   content() |> 
#   readBin("character") 
# 
# hann_response |> 
#   content() |> 
#   readBin("character") |>
#   read_html() |> 
#   html_elements(".jobTitle-link") |> 
#   html_attr("href") |> unique()

library(tibble)
library(readr)
library(dplyr)
library(stringr)
library(httr)
library(rvest)

startrow <- 0
full_hann_response <- vector("character")

tryCatch({
  
  hann_response_raw <- GET(paste0("https://jobs.hannover-re.com/tile-search-results/?q=&sortColumn=referencedate&sortDirection=desc&startrow=", startrow)) |> 
    content() |> 
    readBin("character") |>
    read_html() 
  
  while (length(html_children(hann_response_raw)) > 0) {
    hann_response <- hann_response_raw |> 
      html_elements(".jobTitle-link") |> 
      html_attr("href") |> 
      unique()
    full_hann_response <- c(full_hann_response, hann_response)
    startrow <- startrow + length(hann_response)
    
    hann_response_raw <- GET(paste0("https://jobs.hannover-re.com/tile-search-results/?q=&sortColumn=referencedate&sortDirection=desc&startrow=", startrow)) |> 
      content() |> 
      readBin("character") |>
      read_html() 
  }
}
  , error = function(e) {message("API call to jobs.hannover-re.com failed!")}()
  , finally = message("Finished GET")
)

if (is_empty(full_hann_response)) {
  stop("Empty response for jobs.hannover-re.com")
} else {
  message("Response not empty, continue processing")
}

hann_open <- full_hann_response |> 
  tibble() |> 
  filter(str_detect(full_hann_response, "^/job/Hannover")) |> 
  mutate(
    ID = str_extract(full_hann_response, "/\\d+/$")
    , ID = factor(str_remove_all(ID, "/"))
    , Title = str_remove_all(full_hann_response, "^/job/Hannover-")
    , Title = str_remove_all(Title, "/\\d+/$")
    , Title = str_replace_all(Title, "-", " ")
    , URI = paste0("https://jobs.hannover-re.com", full_hann_response)
    , full_hann_response = NULL
  ) 

hann_hist <- read_csv2("data/hann_temp.csv"
                       , col_types = cols(
                         ID = col_factor()))

hann_new <- hann_open |> 
  anti_join(hann_hist) |> 
  mutate(Erfasst = lubridate::today())

if (nrow(hann_new) > 0) {
  hann_complete <- hann_hist |>
    bind_rows(hann_new)
  message("New jobs for HannoverRe found")
  write_csv2(hann_complete, "data/hann_temp.csv")
  message("New entries saved to HannoverRe file")
} else {
  message("No new jobs for HannoverRe found")
}
