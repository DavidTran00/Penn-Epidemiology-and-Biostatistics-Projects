############################################
############################################
# Antibiotic Prescribing Performance
# for UPHS Antibiotic Stewardship
#
# Takes monthly UPHS prescribing data for
# individual providers, analyzes data,
# sends email report to providers.
############################################
############################################

#######################################
# CONFIGURATION
#######################################

# set to TRUE to send out ACTUAL monthly reports to ALL physicians by email; set to FALSE otherwise
send_final_emails <- TRUE

# set to TRUE to send reports to the specified email address for TESTING, set to FALSE if you do not wish to test emails
send_test_emails <- FALSE
test_recipient_address <- "keith_hamilton_MD-UPENN_ABX_Stewardship@uphs.upenn.edu"

# antibiotic prescription dataset, file must be present in the same directory as this script
data_file <- "analysis_dataset_thr-jun18.csv"

# set add_testing_statistics to TRUE if you want to add quantiles to reports and plots for manual checks
add_testing_statistics <- FALSE

# email account to send reports from, *must be valid UPHS email*
# send a copy (bcc) of each email to bcc address for confirmation
sender_address <- "Keith Hamilton <keith_hamilton_MD-UPENN_ABX_Stewardship@uphs.upenn.edu>"
sender_password <- "-------"
bcc_address <- c("kathleen.degnan3@uphs.upenn.edu")
 
# files to be attached to all emails, files must be present in the same directory as this script
email_attachment1 <- "Calculation of Metrics for Antibiotic Use in Respiratory Infections.pdf"
email_attachment2 <- "Card for Physicians.pdf"

# time_window: number of recent months to include in analysis. Suggested value: 18
# specific_time_window: number of recent months to consider for specific diseases. Suggested value: 4
time_window <- 18
specific_time_window <- 4

#######################################
# load libraries
#######################################
library(mailR)
library(readxl)
library(reshape2)
library(rmarkdown)
library(stringr)
library(sqldf)
library(tidyverse)

main_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
email_attachment1 <- paste(main_dir, email_attachment1, sep = "/")
email_attachment2 <- paste(main_dir, email_attachment2, sep = "/")
current_date <- Sys.Date()

#######################################
# read UPHS data
#######################################
setwd(main_dir)
if (substr(data_file, nchar(data_file) - 2, nchar(data_file)) == "csv") {
  abx <- read_csv(file = data_file)
} else {
  abx <- read_excel(path = data_file)
}

verify_send <- function() {
  if (send_final_emails) {
    send_test_emails <- FALSE
    print("This script is about to send out the ACTUAL monthly antibiotic prescriptions reports to ALL participating physicians by email. 
            If you have checked the output for correctness and want to proceed, please type 'send' to confirm.")
    input <- readline()
    if (input != "send") {
      send_final_emails <- FALSE
      print("You did not confirm by typing 'send', not sending emails.")
    } else {
      print("Sending emails...")
    }
  }
  if (send_test_emails) {
    send_final_emails <- FALSE
    msg <- paste("This script is about to send TEST emails to ", test_recipient_address, ". Please type 'test' to confirm.", sep = "")
    print(msg)
    input <- readline()
    if (input != "test") {
      send_final_emails <- FALSE
      print("You did not confirm by typing 'test', not sending emails.")
    } else {
      print("Sending emails...")
    }
  }
}

send_email_report <- function(recipient_address, subject, body, attachments) {
  print(paste("Sending email to ", recipient_address, sep = ""))
  send.mail(from = sender_address,
            to = recipient_address,
            bcc = bcc_address,
            subject = subject,
            body = body,
            attach.files = attachments,
            html = TRUE,
            inline = TRUE,
            smtp = list(host.name = "uphssmtp.uphs.upenn.edu",
                        port = 25,
                        user.name = sender_address,
                        passwd = sender_password,
                        tls = TRUE),
            authenticate = TRUE,
            send = TRUE)
}


preprocess_data <- function(df) {
  ##############################################################################
  # recode providers with Charlson number and remove non-Charlson providers
  ##############################################################################
  
  # import provider_xwalk
  prov_xwalk <- read_csv("provider_xwalk_email.csv")
  
  # join abx data to provider_xwalk
  prov_abx <- abx %>% inner_join(prov_xwalk, by = "visit_prov_name")
  
  ########################################################
  # Recode enc_dx1 as disease based on disease crosswalk
  ########################################################
  # import disease_xwalk
  dis_xwalk <- read_csv("disease_xwalk.csv")
  
  # join prov_abx data to disease_xwalk
  prov_abx <- prov_abx %>% left_join(dis_xwalk, by = "enc_dx1")
  
  # recode diseases
  # disease (1=bronchitis (except pertussis), 2=sinusitis, 3=pharyngitis, 4=pneumonia, 5=otitis media, 6=pertussis, 7=other)
  
  ########################################################
  # Recode enc_dx1 as tier based on tier crosswalk
  ########################################################
  # not all dx in table are in a tier, so need to left join
  # import tier_xwalk
  tier_xwalk <- read_csv("tier_xwalk.csv")
  
  # join prov_abx data to tier_xwalk
  prov_abx <- prov_abx %>% left_join(tier_xwalk, by = "enc_dx1")
    
  ########################################################
  # create var for whether pt received abx
  ########################################################
  prov_abx$enc_med1 <- as.character(prov_abx$enc_med1)
  prov_abx$enc_med1[prov_abx$enc_med1 == ""] <- "NULL"
  prov_abx$presc_abx <- ifelse(prov_abx$enc_med1 == "NULL", 0, 1)
  
  ############################
  # create date intervals
  ############################
  
  # save as date
  prov_abx$enc_contact <- as.Date(prov_abx$enc_contact, format = "%m/%d/%Y")
  
  # create month name variable
  prov_abx$month <- months(prov_abx$enc_contact)
  prov_abx$month <- ifelse(prov_abx$month == "January", "Jan", ifelse(prov_abx$month == "February","Feb", 
                    ifelse(prov_abx$month == "March","Mar", ifelse(prov_abx$month == "April","Apr", 
                    ifelse(prov_abx$month == "May","May", ifelse(prov_abx$month == "June","Jun", 
                    ifelse(prov_abx$month == "July","Jul", ifelse(prov_abx$month == "August","Aug", 
                    ifelse(prov_abx$month == "September","Sep", ifelse(prov_abx$month == "October","Oct", 
                    ifelse(prov_abx$month == "November","Nov", ifelse(prov_abx$month == "December","Dec", "none")))))))))                                                                           )))
  
  # extract year
  prov_abx$year <- substring(prov_abx$enc_contact, 1, 4)
  
  # concat mon and year
  prov_abx$monyr <- paste(prov_abx$month, prov_abx$year, sep = " ")
  
  return(prov_abx)
}

most_recent_data <- function(data, n_months) {
  recent_months <- data %>% distinct(monyr) %>% tail(n_months) %>% pull
  filter(data, monyr %in% recent_months)
}

compute_quantiles <- function(data) {
  # calculates quartile statistic of prescription rates for ALL providers for the most recent month
  doctors <- data %>% distinct(VISIT_PROV_NAME) %>% pull
  month <- most_recent_months(data, 1)
  presc_rates <- sapply(doctors, monthly_rate, data = data, month = month)
  quantile(presc_rates, na.rm = TRUE)
}

which_quantile <- function(presc_rate, quartiles) {
  # returns the quartile that a prescription rate falls into
  ifelse(is.nan(presc_rate), NA, min(which(presc_rate <= quartiles[-1])))
}

get_presc_rate <- function(data, disease, prescriber, TIER3 = F) {
  data <- ifelse(!TIER3, data, data[!is.na(data$TIER3), ])
  if (!(disease == "all")) {
    diseases <- c(1, 2, 3, 7)
    names(diseases) <- c("bronchitis", "sinusitis", "pharyngitis", "other")
    if (!is.numeric(disease)) {
      disease <- diseases[disease]
    }
    data <- data[!is.na(presc_data$disease) & presc_data$disease == disease, ]
  }
  if (prescriber == "best") {
    best_rate <- 1
    names <- unique(presc_data$VISIT_PROV_NAME)
    for (name in names) {
      doc_data <- data[data$VISIT_PROV_NAME == name, ]
      n_presc <- dim(doc_data[doc_data$presc_abx == 1, ])[1]
      n_visits <- dim(doc_data)[1]
      presc_rate <- n_presc / n_visits
      if (n_visits >= 20 & presc_rate < best_rate) {
        best_rate <- presc_rate
      }
    }
    return (best_rate)
  }
  if (prescriber != "all") {
    data <- data[data$VISIT_PROV_NAME == prov_name, ]
  }
  presc_rate <- dim(data[data$presc_abx == 1, ])[1] / dim(data)[1]
  return (presc_rate)
}

most_recent_months <- function(data, n_months) {
  tail(unique(data$monyr), n_months)
}

time_series <- function(data, prescriber) {
  months <- unique(data$monyr)
  sapply(months, FUN = monthly_rate, data = data, prescriber = prescriber)
}

generate_reports <- function(data) {
  monyrs <- most_recent_months(data, time_window)
  last_month <- most_recent_months(data, 1)
  data <- filter(data, monyr %in% most_recent_months(data, time_window))
  data_tier3 <- filter(data, TIER == 3)
  prescribers <- unique(data$VISIT_PROV_NAME)
  
  cohorts <- 1:4
  best_performers <- sapply(cohorts, FUN = best_prescription_rate_new, data = data, diagnosis = NA, min_obs = 20, n_months = 4, return_name = T)
  best_performers_tier3 <- sapply(cohorts, FUN = best_prescription_rate_new, data = filter(data, TIER == 3), diagnosis = NA, min_obs = 20, n_months = 4, return_name = T)
  print(best_performers)
  print(best_performers_tier3)
  all_series <- time_series(data, "all")
  all_series_tier3 <- time_series(data_tier3, "all")
  quartiles <- compute_quantiles(data)
  quartiles_tier3 <- compute_quantiles(data_tier3)
  cohort_series <- sapply(cohorts, FUN = time_series, data = data)
  cohort_series_tier3 <- sapply(cohorts, FUN = time_series, data = filter(data, TIER == 3))
  
  for (doc in prescribers) {
    # calculate absolute number of prescriptions and visits during last month for report
    doc_data <- filter(data, VISIT_PROV_NAME == doc)
    data_last_month <-  filter(data, VISIT_PROV_NAME == doc, monyr %in% last_month)
    data_tier3_last_month <-  filter(data_tier3, VISIT_PROV_NAME == doc, monyr %in% last_month)
    n_presc_resp <- dim(filter(data_last_month, presc_abx == 1))[1]
    n_visit_resp <- dim(data_last_month)[1]
    n_presc_tier3 <- dim(filter(data_tier3_last_month, presc_abx == 1))[1]
    n_visit_tier3 <- dim(data_tier3_last_month)[1]
    
    # create data frames with time series of prescription rates
    cohort <- tail(data[data$VISIT_PROV_NAME == doc, ]$char_quar, 1)
    df <- data.frame("monyrs" = monyrs, 
                     "You" = time_series(data, doc), 
                     "Mean" = time_series(data, "all"), 
                     "Cohort" = time_series(data, cohort), 
                     "Best" = time_series(data, best_performers[cohort]))
    df_tier3 <- data.frame("monyrs" = monyrs, 
                           "You" = time_series(data_tier3, doc), 
                           "Mean" = time_series(data_tier3, "all"), 
                           "Cohort" = time_series(data_tier3, cohort), 
                           "Best" = time_series(data_tier3, best_performers_tier3[cohort]))
    df_diag <- diagnosis_table(data, doc, min_obs = 20, n_months = 4)
    
    quantile <- which_quantile(tail(df$You, 1), quartiles)
    quantile_tier3 <- which_quantile(tail(df_tier3$You, 1), quartiles_tier3)
    if (is.na(quantile)) quantile <- 5
    if (is.na(quantile_tier3)) quantile_tier3 <- 5
    
    doc_original <- doc
    doc <- gsub("'", "", doc) # remove apostrophes from names because they cause problems with HTML
    
    output_dir <- paste(main_dir, "/reports", '_', current_date, sep = '')
    params <- list(doc_name = doc, doc_num = 0, last_month = last_month, df1 = df, df2 = df_tier3, 
              df3 = df_diag, quantile = quantile, quantile_tier3 = quantile_tier3, testing = add_testing_statistics, 
              all_quantiles = quartiles, all_quantiles_tier3 = quartiles_tier3, n_presc_resp = n_presc_resp, 
              n_visit_resp = n_visit_resp, n_presc_tier3 = n_presc_tier3, n_visit_tier3 = n_visit_tier3, 
              output_dir = output_dir)
    
    rmarkdown::render(paste(main_dir, "/abx_report_docx.Rmd", sep = ''),
                      output_file = paste("report_", doc, '_', current_date, ".docx", sep = ''), 
                      output_dir = output_dir, 
                      params = params,
                      envir = new.env())
    rmarkdown::render(paste(main_dir, "/abx_report_html.Rmd", sep = ''),
                      output_file = paste("report_", doc, '_', current_date, ".html", sep = ''), 
                      output_dir = output_dir, 
                      params = params,
                      envir = new.env())
    
    template_file <- paste(main_dir, "/html_report_template.html", sep ='')
    html_template <- readChar(template_file, file.info(template_file)$size)
    
    plot1filename <- paste(output_dir, '/plot1_', doc, current_date, '.png', sep = '')
    plot2filename <- paste(output_dir, '/plot2_', doc, current_date, '.png', sep = '')
    html_template <- gsub("plot1filename", plot1filename, html_template)
    html_template <- gsub("plot2filename", plot2filename, html_template)
    
    html_template <- gsub("EXAMPLE DOC", doc_original, html_template)
    html_template <- gsub("replace_month", last_month, html_template)
    html_template <- gsub("replace_n_visit_resp", n_visit_resp, html_template)
    html_template <- gsub("replace_n_presc_resp", n_presc_resp, html_template)
    html_template <- gsub("replace_n_visit_tier3", n_visit_tier3, html_template)
    html_template <- gsub("replace_n_presc_tier3", n_presc_tier3, html_template)
    
    df_diag[is.nan(df_diag)] <- NA
    doctab <- format(round(100 * df_diag, 2), nsmall = 2)
    doctab[, 1] <- df_diag[, 1]
    df_diag <- doctab
    html_template <- gsub("replace_11", df_diag[1, 1], html_template)
    html_template <- gsub("replace_12", df_diag[1, 2], html_template)
    html_template <- gsub("replace_13", df_diag[1, 3], html_template)
    html_template <- gsub("replace_14", df_diag[1, 4], html_template)
    html_template <- gsub("replace_21", df_diag[2, 1], html_template)
    html_template <- gsub("replace_22", df_diag[2, 2], html_template)
    html_template <- gsub("replace_23", df_diag[2, 3], html_template)
    html_template <- gsub("replace_24", df_diag[2, 4], html_template)
    html_template <- gsub("replace_31", df_diag[3, 1], html_template)
    html_template <- gsub("replace_32", df_diag[3, 2], html_template)
    html_template <- gsub("replace_33", df_diag[3, 3], html_template)
    html_template <- gsub("replace_34", df_diag[3, 4], html_template)
    html_template <- gsub("replace_41", df_diag[4, 1], html_template)
    html_template <- gsub("replace_42", df_diag[4, 2], html_template)
    html_template <- gsub("replace_43", df_diag[4, 3], html_template)
    html_template <- gsub("replace_44", df_diag[4, 4], html_template)
    
    qtext <- switch(quantile,
                    "You are in the <font color = \"green\"><u><strong>best performing</strong></u></font> quartile of all prescribers for this metric", 
                    "You are in the <font color = \"orange\"><u><strong>2nd best performing</strong></u></font> quartile of all prescribers for this metric",
                    "You are in the <font color = \"red\"><u><strong>2nd lowest performing (3rd)</strong></u></font> quartile of all prescribers for this metric",
                    "You are in the <font color = \"red\"><u><strong>lowest performing (4th)</strong></u></font> quartile of all prescribers for this metric",
                    "You have no prescription data")
    q3text <- switch(quantile_tier3,
                    "You are in the <font color = \"green\"><u><strong>best performing</strong></u></font> quartile of all prescribers for this metric", 
                    "You are in the <font color = \"orange\"><u><strong>2nd best performing</strong></u></font> quartile of all prescribers for this metric",
                    "You are in the <font color = \"red\"><u><strong>2nd lowest performing (3rd)</strong></u></font> quartile of all prescribers for this metric",
                    "You are in the <font color = \"red\"><u><strong>lowest performing (4th)</strong></u></font> quartile of all prescribers for this metric",
                    "You have no prescription data")
    html_template <- gsub("replace_quartile_resp", qtext, html_template)
    html_template <- gsub("replace_quartile_tier3", q3text, html_template)
    
    outfile <- paste(output_dir, "/email_", doc, "_", current_date, ".html", sep ='')
    sink(outfile)
    cat(html_template)
    sink()
    
    docx_report <- paste(output_dir, "/report_", doc, '_', current_date, ".docx", sep = '')
    attachments <- c(email_attachment1, email_attachment2, docx_report)
    subject <- paste("Antibiotic Prescribing Feedback for ", last_month, sep = "")
    
    if (send_test_emails) {
      recipient_address <- test_recipient_address
      send_email_report(recipient_address = recipient_address, subject = subject, body = html_template, attachments = attachments)
    }
    if (send_final_emails) {
      recipient_address <- toString((doc_data %>% select(email))[1, 1])
      if (is.na(recipient_address)) {
        print(paste("Error: Could not find email address for ", doc, sep = ""))
      } else {
        send_email_report(recipient_address = recipient_address, subject = subject, body = html_template, attachments = attachments)
      }
    }
  }
}

monthly_rate <- function(data, prescriber, month) {
  if (is.numeric(prescriber)) {
    data <- data %>% filter(char_quar == prescriber)
  } else if (prescriber != "all") {
    data <- data %>% filter(VISIT_PROV_NAME == prescriber)
  }
  data <- data[data$monyr == month, ]
  n_observ <- dim(data)[1]
  n_prescr <- dim(data %>% filter(presc_abx == 1))[1]
  n_prescr / n_observ
  #ifelse(n_observ == 0, NA, n_prescr / n_observ)
}

rate_with_min <- function(data, prescriber = NA, diagnosis = NA, min_obs = 0) {
  if (!is.na(prescriber)) {
    data <- filter(data, VISIT_PROV_NAME == prescriber)
  }
  if (!is.na(diagnosis)) {
    data <- filter(data, disease == diagnosis)
  }
  n_observ <- nrow(data)
  n_prescr <- nrow(data %>% filter(presc_abx == 1))
  ifelse(n_observ < min_obs, NA, n_prescr / n_observ)
}

best_prescription_rate <- function(data, diagnosis = NA, min_obs = 20, n_months = 4, return_name = FALSE, cohort = NA) {
  if (!is.na(cohort)) {
    data <- filter(data, char_quar == cohort)
  }
  data <- filter(data, monyr %in% most_recent_months(data, n_months))
  rates <- sapply(unique(data$VISIT_PROV_NAME), 
                  FUN = rate_with_min, 
                  data = data, 
                  diagnosis = diagnosis, 
                  min_obs = min_obs)
  ifelse(return_name, names(which.min(rates)), min(rates, na.rm = TRUE))
}

prescription_rate_min_obs <- function(data, provider, min_obs) {
  # calculates average monthly prescription rate of provider if EVERY MONTH had AT LEAST n_min observations, returns NA otherwise
  data <- filter(data, VISIT_PROV_NAME == provider)
  months <- distinct(data, monyr) %>% pull
  n_visits_total <- nrow(data)
  n_prescriptions_total <- nrow(filter(data, presc_abx == 1))
  n_visits_monthly <- sapply(months, FUN = function(x) nrow(filter(data, monyr == x)))
  n_prescriptions_monthly <- sapply(months, FUN = function(x) nrow(filter(data, monyr == x, presc_abx == 1)))
  #print(n_visits_monthly)
  #print(n_prescriptions_monthly)
  #print(n_visits_total)
  #print(n_prescriptions_total)
  ifelse(all(n_visits_monthly >= min_obs), n_prescriptions_total / n_visits_total, NA)
}

best_prescription_rate_new <- function(data, diagnosis = NA, min_obs = 20, n_months = 4, return_name = FALSE, cohort = NA) {
  # new function: require at least 20 prescriptions PER MONTH for the last 4 months
  if (!is.na(cohort)) {
    data <- filter(data, char_quar == cohort)
  }
  data <- most_recent_data(data, n_months)
  providers <- distinct(data, VISIT_PROV_NAME) %>% pull
  prescription_rates <- sapply(providers,
                               FUN = prescription_rate_min_obs,
                               data = data,
                               min_obs = min_obs)
  #print(prescription_rates)
  #print(min(prescription_rates, na.rm = TRUE))
  #print(names(which.min(prescription_rates)))
  ifelse(return_name, names(which.min(prescription_rates)), min(prescription_rates, na.rm = TRUE))
}

diagnosis_table <- function(data, prescriber, min_obs = 20, n_months = 4) {
  data <- most_recent_data(data, n_months = n_months)
  diagnosis_codes <- c(1, 2, 3, 7)
  n_visits <- sapply(diagnosis_codes, FUN = function(x) nrow(filter(data, VISIT_PROV_NAME == prescriber, disease == x)))
  pres_rates <- sapply(diagnosis_codes,
                       FUN = rate_with_min, 
                       data = data,
                       prescriber = prescriber,
                       min_obs = 0)
  mean_rates <- sapply(diagnosis_codes, 
                       FUN = rate_with_min, 
                       data = data,
                       prescriber = NA, 
                       min_obs = 0)
  best_rates <- sapply(diagnosis_codes, 
                       FUN = best_prescription_rate, 
                       data = data, 
                       min_obs = min_obs)
  cbind(n_visits, pres_rates, mean_rates, best_rates)
}

main <- function() {
  verify_send()
  prov_abx <- preprocess_data(abx)
  generate_reports(prov_abx)
}

main()
