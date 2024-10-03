# -------------------------------------------------------------------------
#
#             Send Job Mail - Function 
#
# -------------------------------------------------------------------------
# 
# 

send_job_mail <- function() {
  # library(glue)
  # Create Markdown report
  # vignette("simple_composition")
  
  require(blastula)
  require(glue)
  
  if (length(mailing_list) > 0) {
    job_message <- mailing_list %>%
      map(., ~knitr::kable(., format = "html")) %>%
      map(md) |> 
      reduce(glue) |> 
      md() |> 
      compose_email()
  } else {
    job_message <- md("Heute *keine* neuen Stellen gefunden.") |> 
      compose_email()
  }
  
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
      write_lines(paste0("Fehler beim senden am ", lubridate::today()), paste0("log/log_", lubridate::today()));
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
  
}