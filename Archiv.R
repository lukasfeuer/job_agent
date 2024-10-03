
'
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

'


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

ifelse(nrow(conti_new) < 0, mailing_list$conti <- conti_new, FALSE)