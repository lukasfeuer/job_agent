# -------------------------------------------------------------------------
#
#             KKH - Jobportal
#
# -------------------------------------------------------------------------

library(tibble)
library(readr)
library(dplyr)
library(stringr)
library(httr)
library(rvest)

# TODO: pagination 

kkh_response <- GET("https://www.kkh.de/apps/csa/proxy/search?spr=kkh23-searchresults-st&so=lastmodified%3Adesc&ex=false&q=&fi=path%3A%2Fcontent%2Fkkhweb%2F*&fi=jobPostCodes%3A30938%7C31191%7C31157%7C31555%7C30982%7C31275%7C30989%7C31319%7C30625%7C30669%7C30823%7C30900%7C30826%7C30627%7C31832%7C31515%7C31559%7C30926%7C30171%7C31180%7C30453%7C30177%7C30455%7C30851%7C30179%7C30173%7C30451%7C30175%7C30890%7C30659%7C30419%7C30539%7C30457%7C30655%7C30853%7C30974%7C31303%7C30459%7C30657%7C30855%7C30916%7C30519%7C31171%7C30161%7C30167%7C30169%7C30521%7C30163%7C30165%7C30880%7C30449%7C30966%7C30629%7C30827%7C30559%7C30952%7C30159"
                    , verbose())

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


ifelse(nrow(kkh_new) < 0, mailing_list$kkh <- kkh_new, FALSE)


