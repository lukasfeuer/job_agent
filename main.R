# -------------------------------------------------------------------------
# 
#                 Ausschreibungen | WebScraping
#
# -------------------------------------------------------------------------


keyWords <- c("Analyst", "Reporting", "Mathematik", "Statistik", "Aktuar"
              , "Risiko", "Risk", "Daten", "Data", "Scientist")

mailing_list <- list()


# 1 | HDI -----------------------------------------------------------------

source("R/HDI.R")

ifelse(nrow(hdi_new) > 0, mailing_list$hdi <- hdi_new, FALSE)


# 2 | Hannover Re ---------------------------------------------------------

source("R/HannoverRe.R")

ifelse(nrow(hann_new) > 0, mailing_list$hann <- hann_new, FALSE)


# 3 | KKH -----------------------------------------------------------------

# source("R/KKH.R")
# 
# ifelse(nrow(kkh_new) > 0, mailing_list$kkh <- kkh_new, FALSE)


# 4 | ROSSMANN ------------------------------------------------------------

source("R/Rossmann.R")

ifelse(nrow(ross_new) > 0, mailing_list$ross <- ross_new, FALSE)


# 5 | Capgemini -----------------------------------------------------------

#source("R/Capgemini.R")

#ifelse(nrow(cap_new) > 0, mailing_list$cap <- cap_new, FALSE)


# Send email for new jobs ------------------------------------------------------

#   Gmail: ds.mailservice.germany
#   PW: Sys.getenv("mailservice_PW") # Note: replaced by app password 2024-10-02
# 
# create_smtp_creds_file(
#   file = "gmail_creds",
#   user = "ds.mailservice.germany@gmail.com",
#   host = "smtp.gmail.com",
#   port = 465,
#   use_ssl = TRUE
# )

source("R/send_job_mail.R")

ifelse(length(mailing_list) > 0
       , send_job_mail(),
       print("Heute keine neuen Stellen gefunden"))
