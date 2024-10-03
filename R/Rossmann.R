# -------------------------------------------------------------------------
#
#             ROSSMANN - Jobportal
#
# -------------------------------------------------------------------------

library(tibble)
library(readr)
library(dplyr)
library(stringr)
library(httr)
library(rvest)

ross_response <- list()

tryCatch(
  ross_response <- GET("https://jobs.rossmann.de/?type=1201&tx_oycimport_vacancy[controller]=Vacancy&tx_oycimport_vacancy[action]=search"
                       #, verbose()
                       )
  , error = function(e) {message("API call to jobs.rossmann.de failed!")}()
  , finally = message("Finished GET")
)

if (!is_empty(ross_response) & ross_response$status_code == 200) {
  message("Response not empty, continue processing")
} else {
  stop("Empty response for jobs.rossmann.de")
}

# ross_open |> 
#   content() |> 
#   html_element("body") |> 
#   html_elements("div") 
# 
# ross_open |> 
#   content() |> 
#   html_element("body") |> 
#   html_elements(".job-list__item list-item") 

ross_titles <- ross_response |> 
  content() |> 
  html_elements(".job-list__item-title") |> 
  html_text()

ross_time <- ross_response |> 
  content() |> 
  html_elements(".job-list__item-time") |> 
  html_text()

ross_link <- ross_response |> 
  content() |> 
  html_elements(".job-list__item-link") |> 
  html_attr("href") |> 
  (\(.) paste0("https://jobs.rossmann.de", .))()

ross_open <- tibble(
  Title = ross_titles
  , time = ross_time
  , URI = ross_link
) |> 
  filter(
    time == "Vollzeit"
    , !str_detect(Title, "Teilzeit")
    , !str_detect(Title, "Verkäufer")
    , !str_detect(Title, "Regalplanung")
    , !str_detect(Title, "Filialleit|Führungs")
    , !str_detect(Title, "Trainee")
    , !str_detect(Title, "(L|l)eitung")
    , !str_detect(Title, "(L|l)eiter")
    , !str_detect(Title, "Kommissionier")
    , !str_detect(Title, "Praktikant")
    , !str_detect(Title, "Studium")
    , !str_detect(Title, "Ausbildung")
    , !str_detect(Title, "Mitarbeiter")
    , !str_detect(Title, "(F|f)ahrer")
    , !str_detect(Title, "Recht|Java|Controll|(T|t)est|Support|Cloud|Projektman|(A|a)ssisten")
    , !str_detect(Title, "Product Owner")
    
  ) |> 
  mutate(
    time = NULL
    , ID = str_extract(URI, "\\d+(?=\\.html)")
  ) |> 
  relocate(ID, .before = Title)

ross_hist <- read_csv2("data/ross_temp.csv"
                       , col_types = cols(
                         ID = col_factor()))

ross_new <- ross_open |> 
  anti_join(ross_hist) |> 
  mutate(Erfasst = today())

if (nrow(ross_new) > 0) {
  ross_complete <- ross_hist |>
    bind_rows(ross_new)
  message("New jobs for Rossmann found")
  write_csv2(ross_complete, "data/ross_temp.csv")
  message("New entries saved to Rossmann file")
} else {
  message("No new jobs for Rossmann found")
}