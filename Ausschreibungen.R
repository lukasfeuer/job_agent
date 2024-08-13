#==============================================================================:
# Ausschreibungen | WebScraping ----
#==============================================================================:

library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(blastula)
library(lubridate)
library(glue)

keyWords <- c("Analyst", "Reporting", "Mathematik", "Statistik", "Aktuar"
              , "Risiko", "Risk", "Daten", "Data", "Scientist")

# 1 | Continental AG -----------------------------------------------------------

## Get open jobs
# Filter: Germany, Hanover, Human Relations
# TODO filter
#  data+consultant+analyst+analytics+statistic+automation+analyst+recruiting+hr+diagnostic
#conti_open <- GET("https://api.continental-jobs.com/search/?data=%7B%22LanguageCode%22%3A%22EN%22%2C%22SearchParameters%22%3A%7B%22FirstItem%22%3A1%2C%22CountItem%22%3A30000%2C%22Sort%22%3A%5B%7B%22Criterion%22%3A%22PositionTitle%22%2C%22Direction%22%3A%22DESC%22%7D%5D%2C%22MatchedObjectDescriptor%22%3A%5B%22ID%22%2C%22PositionTitle%22%2C%22PositionURI%22%2C%22PositionLocation.CountryName%22%2C%22PositionLocation.CityName%22%2C%22JobCategory.Name%22%2C%22PositionLocation.Longitude%22%2C%22PositionLocation.Latitude%22%5D%7D%2C%22SearchCriteria%22%3A%5B%7B%22CriterionName%22%3A%22JobCategory.Code%22%2C%22CriterionValue%22%3A%5B%2211%22%5D%7D%2C%7B%22CriterionName%22%3A%22PositionLocation.Country%22%2C%22CriterionValue%22%3A%5B%2217%22%5D%7D%2C%7B%22CriterionName%22%3A%22PositionLocation.City%22%2C%22CriterionValue%22%3A%5B%22191%22%5D%7D%2C%7B%22CriterionName%22%3A%22PublicationLanguage.Code%22%2C%22CriterionValue%22%3A%5B%22EN%22%5D%7D%2C%7B%22CriterionName%22%3A%22PublicationChannel.Code%22%2C%22CriterionValue%22%3A%5B%2212%22%5D%7D%5D%7D", 
#                  verbose())

conti_open <- GET(paste0("https://api.continental-jobs.com/search/?data=%7B%22LanguageCode%22%3A%22EN%22%2C%22SearchParameters%22%3A%7B%22FirstItem%22%3A1%2C%22CountItem%22%3A100%2C%22Sort%22%3A%5B%7B%22Criterion%22%3A%22PublicationStartDate%22%2C%22Direction%22%3A%22DESC%22%7D%5D%2C%22MatchedObjectDescriptor%22%3A%5B%22ID%22%2C%22PositionID%22%2C%22PositionTitle%22%2C%22PositionURI%22%2C%22PositionLocation.CountryName%22%2C%22PositionLocation.CityName%22%2C%22PositionLocation.Longitude%22%2C%22PositionLocation.Latitude%22%2C%22PositionIndustry.Name%22%2C%22JobCategory.Name%22%2C%22PublicationStartDate%22%2C%22VacancyDivision%22%2C%22JobFlexibility.Code%22%2C%22JobFlexibility.Name%22%2C%22JobFlexibility.Description%22%5D%7D%2C%22SearchCriteria%22%3A%5B%7B%22CriterionName%22%3A%22PositionFormattedDescription.Content%22%2C%22CriterionValue%22%3A%5B%22", 
                  "data+consultant+analyst+analytics+statistic+automation+analyst+recruiting+hr+diagnostic",
                  "%22%5D%7D%2C%7B%22CriterionName%22%3A%22LeadershipLevel.Code%22%2C%22CriterionValue%22%3A%5B%221%22%5D%7D%2C%7B%22CriterionName%22%3A%22PositionLocation.City%22%2C%22CriterionValue%22%3A%5B%22191%22%5D%7D%2C%7B%22CriterionName%22%3A%22PublicationLanguage.Code%22%2C%22CriterionValue%22%3A%5B%22EN%22%5D%7D%2C%7B%22CriterionName%22%3A%22PublicationChannel.Code%22%2C%22CriterionValue%22%3A%5B%2212%22%5D%7D%5D%7D"),
                  verbose())

conti_open <- conti_open %>% 
  content(as = "text") %>% 
  fromJSON()

conti_open <- conti_open$SearchResult$SearchResultItems

conti_open <- conti_open$MatchedObjectDescriptor

conti_open <- conti_open %>% 
  tibble() %>% 
  select(PositionID, PositionTitle, PositionURI) %>% 
  filter(!str_detect(PositionTitle, 
                     c("Werkstud|Werksstudent|Praktik|praktiku|Internship|Student"))) %>% 
  rename(ID = 1, Title = 2, URI = 3)

## Compare open jobs with known jobs
conti_hist <- read_csv2("data/conti_temp.csv")

conti_new <- conti_open %>% 
  anti_join(conti_hist) %>% 
  mutate(Erfasst = today())

conti_complete <- conti_hist %>% 
  bind_rows(conti_new)

# TODO Spalte mit Zeitpunt des ersten Abrufs
write_csv2(conti_complete, "data/conti_temp.csv")


# 2 | HDI ----------------------------------------------------------------------

# Filter: Hannover & Berufseinsteiger

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
  mutate(Erfasst = today())

hdi_complete <- hdi_hist %>% 
  bind_rows(hdi_new)

# TODO Spalte mit Zeitpunt des ersten Abrufs
write_csv2(hdi_complete, "data/hdi_temp.csv")





# 3 | KKH -----------------------------------------------------------------

# TODO: pagination

kkh_response <- GET("https://www.kkh.de/apps/csa/proxy/search?spr=kkh23-searchresults-st&so=lastmodified%3Adesc&ex=false&q=&fi=path%3A%2Fcontent%2Fkkhweb%2F*&fi=jobPostCodes%3A30938%7C31191%7C31157%7C31555%7C30982%7C31275%7C30989%7C31319%7C30625%7C30669%7C30823%7C30900%7C30826%7C30627%7C31832%7C31515%7C31559%7C30926%7C30171%7C31180%7C30453%7C30177%7C30455%7C30851%7C30179%7C30173%7C30451%7C30175%7C30890%7C30659%7C30419%7C30539%7C30457%7C30655%7C30853%7C30974%7C31303%7C30459%7C30657%7C30855%7C30916%7C30519%7C31171%7C30161%7C30167%7C30169%7C30521%7C30163%7C30165%7C30880%7C30449%7C30966%7C30629%7C30827%7C30559%7C30952%7C30159")

kkh_title <- kkh_response |> 
  content() |> 
  html_elements("h3") |> 
  html_element("a") |>  
  html_text()

kkh_href <- kkh_response |> 
  content() |> 
  html_elements("h3") |> 
  html_element("a") |>  
  html_attr("href")

kkh_id <- kkh_href %>% 
  map_chr(., ~str_extract(., "\\d{5}$"))

kkh_jobs <- tibble(
  "ID" = kkh_id
  , "Title" = str_squish(kkh_title)
  , "URI" = kkh_href
)






# 3 | Deutsche Bahn ------------------------------------------------------------

# xml2::html_structure(x)
# 
# ul#resultItems .result-items.d-block

# Filter: Hannover/Niedersachsen & "Daten" & 50km Suchradius
bahn_open1 <- GET("https://karriere.deutschebahn.com/service/search/karriere-de/2653760?query=Daten&location=Hannover&state=Niedersachsen&radius=50&qli=true&sort=pubExternalDate_tdt&country=&view=asSearchResult&isMapTabDefault=false",
    verbose())

# Filter: Hannover/Niedersachsen & "Analyst" & 50km Suchradius
bahn_open2 <- GET("https://karriere.deutschebahn.com/service/search/karriere-de/2653760?query=analyst&location=Hannover&state=Niedersachsen&radius=50&qli=true&sort=pubExternalDate_tdt&country=&view=asSearchResult&isMapTabDefault=false",
                  verbose())

# attr(*, ".class")= chr "result-count"
# attr(*, "data-count")= chr "2"
# bahn_open2 %>% 
#   content(as = "parsed") %>% 
#   html_element("body") %>% 
#   xml2::as_list() %>% str()

bahn_open1 <- bahn_open1 %>% 
  content(as = "parsed")

bahn_open2 <- bahn_open2 %>% 
  content(as = "parsed") 
  
bahn_id1 <- bahn_open1 %>% 
  html_elements("div.add-to-bookmark-container") %>% 
  html_attr("data-jobid")

bahn_titles1 <- bahn_open1 %>% 
  html_element("body") %>% 
  html_elements(c(".title")) %>% 
  html_text()

bahn_hrefs1 <- bahn_open1 %>% 
  html_elements(".mt-lg-0") %>% 
  html_attr("href")

bahn_id2 <- bahn_open2 %>% 
  html_elements("div.add-to-bookmark-container") %>% 
  html_attr("data-jobid")

bahn_titles2 <- bahn_open2 %>% 
  html_element("body") %>% 
  html_elements(c(".title")) %>% 
  html_text()

bahn_hrefs2 <- bahn_open2 %>% 
  html_elements(".mt-lg-0") %>% 
  html_attr("href")


if (bahn_id1 > 0) {
  bahn_open_x1 <- bahn_id1 %>% 
    tibble() %>% 
    bind_cols(bahn_titles1, bahn_hrefs1) %>% 
    rename(ID = 1, Title = 2, URI = 3) %>% 
    mutate(URI = paste0("https://karriere.deutschebahn.com", URI))
}

if (bahn_id2 > 0) {
  bahn_open_x2 <- bahn_id2 %>% 
    tibble() %>% 
    bind_cols(bahn_titles2, bahn_hrefs2) %>% 
    rename(ID = 1, Title = 2, URI = 3) %>% 
    mutate(URI = paste0("https://karriere.deutschebahn.com", URI))
}

# TODO was passiert, wenn ein query leer ist? 
bahn_open <- bahn_open_x1 %>% 
  bind_rows(bahn_open_x2) %>% 
  type_convert() %>% 
  distinct() %>% 
  rename(ID = 1, Title = 2, URI = 3)

## Compare open jobs with known jobs
bahn_hist <- read_csv2("data/bahn_temp.csv")

bahn_new <- bahn_open %>% 
  anti_join(bahn_hist) %>% 
  mutate(Erfasst = today())

bahn_complete <- bahn_hist %>% 
  bind_rows(bahn_new)

# TODO Spalte mit Zeitpunt des ersten Abrufs
write_csv2(bahn_complete, "data/bahn_temp.csv")




# 4 | Capgemini ----------------------------------------------------------------

' 
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
'


# 5 | TUEV NORD ----------------------------------------------------------------

'
# Region Hannover, (IT-Bereich)

tuv_open <- GET("https://www.tuev-nord-group.com/de/karriere/stellenangebote/",
                verbose())

POST("https://jobs.tuev-nord.de/api/rest/v2/frontend/jobboards/5564_extern/jobs",
     )


GET("https://maps.google.com/maps-api-v3/api/js/47/1/intl/de_ALL/util.js")
GET("https://www.google.com/recaptcha/api2/bframe?hl=de&v=_7Co1fh8iT2hcjvquYJ_3zSP&k=6LfS60EUAAAAAPCCALpOPHrZyWl6MirRcFrm3GZr")

tuv_open %>% 
  content(as = "parsed") %>% 
  write_lines("test.html")
  

tuv_open <- tuv_open$jobs

tuv_open <- tuv_open %>% 
  tibble() %>% 
  select(id, jobTitle, httpLink) %>% 
  rename(ID = 1, Title = 2, URI = 3)

## Compare open jobs with known jobs
tuv_hist <- read_csv2("data/tuv_temp.csv")

tuv_new <- tuv_open %>% 
  anti_join(tuv_hist) %>% 
  mutate(Erfasst = today())

tuv_complete <- tuv_hist %>% 
  bind_rows(tuv_new)

# TODO Spalte mit Zeitpunt des ersten Abrufs
write_csv2(tuv_complete, "data/tuv_temp.csv")

' 

# Max-Planck Institute ---------------------------------------------------------

'
# https://www.mpg.de/stellenboerse?region%5B%5D=HH&region%5B%5D=NI

# GET("https://www.mpg.de/stellenboerse?job_type%5B%5D=admin&region%5B%5D=BY", 
# add_headers(Accept = ""))

GET("https://www.mpg.de/stellenboerse?job_type%5B%5D=admin&region%5B%5D=BY",
    add_headers(Accept = "application/json"))

GET("https://www.mpg.de/stellenboerse",
    query = list(region = "BY"),
    verbose())

application/json
application/xml

'


# Send email for new jobs ------------------------------------------------------

#   Gmail: ds.mailservice.germany
#   PW: Sys.getenv("mailservice_PW")
# 
# create_smtp_creds_file(
#   file = "gmail_creds",
#   user = "ds.mailservice.germany@gmail.com",
#   host = "smtp.gmail.com",
#   port = 465,
#   use_ssl = TRUE
# )

send_job_mail <- function() {
  # library(glue)
  # Create Markdown report
  # TODO vignette("simple_composition")
  
  t1 <- conti_new %>% 
    knitr::kable(format = "html") %>% 
    md() 
  
  t2 <- hdi_new %>% 
    knitr::kable(format = "html") %>% 
    md() 
  
  t3 <- bahn_new %>% 
    knitr::kable(format = "html") %>% 
    md() 
  
  job_message <- glue(t1, t2, t3) %>% 
    md() %>% 
    compose_email()
  
  #job_message <- conti_new %>% 
  #  knitr::kable(format = "html") %>% 
  #  md() %>% 
  #  compose_email()
  
  # Send report with gmail
  tryCatch(
    job_message %>%
      smtp_send(
        from = "ds.mailservice.germany@gmail.com",
        to = "lukas.feuer@gmail.com",
        subject = "Neue Ausschreibung",
        credentials = creds_file(file = "gmail_creds")
      )
    ,
    warning = function(w) {print("Warning")},
    error = function(e) {print("Error");
      write_lines(paste0("Fehler beim senden am ", today()), paste0("log/log_", today()));
      job_message %>%
        smtp_send(
          from = "ds.mailservice.germany@gmail.com",
          to = "lukas.feuer@gmail.com",
          subject = "Neue Ausschreibung",
          credentials = creds_file(file = "gmail_creds")
        )
      })
  
  # TODO 
  #   wiederholen wenn Fehler:
  #   Fehler in curl_fetch_memory(smtp_server, handle = h) : 
  #   LibreSSL SSL_read: SSL_ERROR_SYSCALL, errno 54
  # 
  # --> Error handling in r 
  # 
  # bzw. min. x-mal wiederholen bis:
  #   The email message was sent successfully.
  #   --> "message" hier immer name des objektes, das verwendet wird?
  
}

# Send if at least one new entry
ifelse((nrow(conti_new) > 0)|
         (nrow(hdi_new) > 0)|
         (nrow(bahn_new) > 0), 
       send_job_mail(),
       print("Heute keine neuen Stellen gefunden"))


# TODO
#   z.B. einmal in der Woche zusammenfassung aller derzeit offenen stellen