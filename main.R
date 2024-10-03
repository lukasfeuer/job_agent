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
hdi_new <- rename(hdi_new, "HDI" = Title)
ifelse(nrow(hdi_new) > 0, mailing_list$hdi <- hdi_new, FALSE)


# 2 | Hannover Re ---------------------------------------------------------

source("R/HannoverRe.R")
hann_new <- rename(hann_new, "Hannover Re" = Title)
ifelse(nrow(hann_new) > 0, mailing_list$hann <- hann_new, FALSE)


# 3 | KKH -----------------------------------------------------------------

# source("R/KKH.R")
# 
# ifelse(nrow(kkh_new) > 0, mailing_list$kkh <- kkh_new, FALSE)


# 4 | ROSSMANN ------------------------------------------------------------

source("R/Rossmann.R")
ross_new <- rename(ross_new, "Rossmann" = Title)
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

if (length(mailing_list) > 0) {
  mailing_list <- map(mailing_list, \(.)select(., -c(ID, Erfasst)))
  source("R/send_job_mail.R")
  send_job_mail()
  message("\nDONE")
} else {
  message("No new Jobs found")
}