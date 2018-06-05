install.packages("gmailr")
install.packages("dplyr")
install.packages("purrr")
suppressPackageStartupMessages(library(gmailr))
suppressPackageStartupMessages(library(gmailr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(readr))
install.packages("readr")

library(readr)
test_email <- mime(
  To = "christian.vogel7@web.de",
  From = "christian.vogel@faranto.de",
  Subject = "this is just a gmailr test",
  body = "Can you hear me now?")

send_message(test_email)
##########################################################################

eMailList <- read.csv("~/****.csv", header = T)

this_hw <- "The Fellowship Of The Ring"
email_sender <- 'Anonymous1 <****>'
optional_bcc <- 'Anonymous2 <****>'
body <- "Hi, %s.
Your Buddy is Christian
"
edat <- eMailList %>%
  mutate(
    To = sprintf('%s <%s>', name, email),
    Bcc = optional_bcc,
    From = email_sender,
    Subject = sprintf('Buddy for %s', this_hw),
    body = sprintf(body, name, this_hw, mark)) %>%
  select(To, Bcc, From, Subject, body)
write_csv(edat, "composed-emails.csv")

emails <- edat %>%
  pmap(mime)
str(emails, max.level=2, list.len=2)

safe_send_message <- safely(send_message)
sent_mail <- emails %>% 
  map(safe_send_message)

saveRDS(sent_mail,
        paste(gsub("\\s+", "_", this_hw), "sent-emails.rds", sep = "_"))

######################
eMailList <- read.csv("****", header = T)


email_sender <- 'Faranto <****>'
body <- "Hi, %s.
Your Buddy is Christian
"
edat <- eMailList %>%
  mutate(
    To = sprintf('%s <%s>', name, email),
    From = email_sender,
    Subject = sprintf('Subject123!'),
    body = sprintf(body, name, this_hw, mark)) %>%
  select(To, From, Subject, body)
write_csv(edat, "composed-emails.csv")

emails <- edat %>%
  pmap(mime)
str(emails, max.level=2, list.len=2)

safe_send_message <- safely(send_message)
sent_mail <- emails %>% 
  map(safe_send_message)

saveRDS(sent_mail,
        paste(gsub("\\s+", "_", this_hw), "sent-emails.rds", sep = "_"))

