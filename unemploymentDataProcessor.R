# Helper app to download and process data from the DOL website
# THe downloads can be found here: http://oui.doleta.gov/unemploy/DataDownloads.asp
# For each download below, there is a data definition pdf that explains what each field is that is being sought

library(RCurl)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(zoo)
library(fredr)
fredr_set_key(Sys.getenv("FRED_KEY"))

#library(data.table)
#library(dplyr)
#require(bit64)

downloadUCData <- function (URL) {
  
  # first try to find the file on the filesystem.  If we can't find it
  # on the file system, then download it
  csvFile <- file.path("data", basename(URL))
  if (file.exists(csvFile))
      mydata <- read_csv(csvFile)
  else
      mydata <- read_csv(URL)
  
  # convert dates to a date type
  mydata %>% 
    mutate(rptdate = as.Date(rptdate, "%m/%d/%Y"))
} 

# sets the names of specific columns in the 5130 dataset
setBenefitAppealNames <- function(df) {
  df %>% 
    rename(`lower_filed` = c9, `lower_disposed` = c13, `higher_filed` = c10, `higher_disposed` = c14)
}


# return the maximum year (as a string) from the df's Date column
getMaxYear <- function (df) {
  return(as.integer(format(max(unique(df$rptdate)), "%Y")))
}

# return the minimum year (as a string) from the df's Date column
getMinYear <- function (df) {
  return(as.integer(format(min(unique(df$rptdate)), "%Y")))
}  


# get data on overpayments and process them
getOverpayments <- function() {

  ucOverpaymentsRegular <- downloadUCData("https://oui.doleta.gov/unemploy/csv/ar227.csv") #227 report
  ucOverpaymentsEUC91 <- downloadUCData("https://oui.doleta.gov/unemploy/csv/ac227.csv") #227 report
  ucOverpaymentsTEUC02 <- downloadUCData("https://oui.doleta.gov/unemploy/csv/at227.csv") #227 report
  ucOverpaymentsEUC08 <- downloadUCData("https://oui.doleta.gov/unemploy/csv/au227.csv") #227 report

  # cols that we want to keep
  overpayment_cols <- c("st","rptdate","regular_fraud_num","federal_fraud_num","regular_fraud_dol","federal_fraud_dol","regular_nonfraud_num","federal_nonfraud_num","regular_nonfraud_dol","federal_nonfraud_dol","state_tax_recovery","federal_tax_recovery", "outstanding", "recovered")
  all_cols <- c("st","rptdate")
  detection_cols <- c("federal_fraud_num","federal_fraud_dol","federal_nonfraud_num","federal_nonfraud_dol", "outstanding", "recovered")
  recovery_cols <- c("state_tax_recovery","federal_tax_recovery")
  
  
  # just pull out the columns that we care about
  ucOverpaymentsRegular <- ucOverpaymentsRegular %>%
    mutate(
      regular_fraud_num = c1 + c2,
      federal_fraud_num = c234,
      regular_fraud_dol = c3 + c4,
      federal_fraud_dol = c235,
      regular_nonfraud_num = c27 + c28,
      federal_nonfraud_num = c250,
      regular_nonfraud_dol = c29 + c30,
      federal_nonfraud_dol = c251,
      state_tax_recovery = c210 + c211 + c212 + c213 + c284 + c285,
      federal_tax_recovery = c286 + c287 + c289 + c290+ c288 + c291,
      outstanding = c35 + c36 + c276 + c37 + c38 + c277,
      recovered = c206 + c207 + c278 + c208 + c209 + c279
    ) %>% 
    select(one_of(overpayment_cols)) %>%
    pivot_longer(cols = -c(st, rptdate), names_to = "metric", values_to = "value")
    
  ucOverpaymentsEUC91 <- ucOverpaymentsEUC91 %>% 
    mutate( 
      federal_fraud_num = c1 + c2,
      federal_fraud_dol = c3 + c4,
      federal_nonfraud_num = c5 + c6,
      federal_nonfraud_dol = c7 + c8,
      outstanding = c9 + c10 + c11 + c12,
      recovered = c13 + c14 + c15 + c16
    ) %>% 
    select(one_of(all_cols, detection_cols)) %>% 
    pivot_longer(cols = -c(st, rptdate), names_to = "metric", values_to = "valueEUC91")
  
  
  ucOverpaymentsTEUC02 <- ucOverpaymentsTEUC02 %>% 
    mutate(
      federal_fraud_num = c1 + c2,
      federal_fraud_dol = c3 + c4,
      federal_nonfraud_num = c27 + c28,
      federal_nonfraud_dol = c29 + c30,
      state_tax_recovery = c210 + c211 + c212 + c213,
      federal_tax_recovery = c214 + c215 + c216 + c217,
      outstanding = c35 + c36 + c37 + c38,
      recovered = c206 + c207 + c208 + c209,
    ) %>% 
    select(one_of(all_cols, detection_cols, recovery_cols)) %>% 
    pivot_longer(cols = -c(st, rptdate), names_to = "metric", values_to = "valueTEUC02")
  
  
  ucOverpaymentsEUC08 <- ucOverpaymentsEUC08 %>% 
    mutate(
      federal_fraud_num = c1 + c2,
      federal_fraud_dol = c3 + c4,
      federal_nonfraud_num = c27 + c28,
      federal_nonfraud_dol = c29 + c30,
      state_tax_recovery = c210 + c211 + c212 + c213,
      federal_tax_recovery = c214 + c215 + c216 + c217,
      outstanding = c35 + c36 + c37 + c38,
      recovered = c206 + c207 + c208 + c209,
    ) %>% 
    select(one_of(all_cols, detection_cols, recovery_cols)) %>% 
    pivot_longer(cols = -c(st, rptdate), names_to = "metric", values_to = "valueEUC08")
  
  # combine the data sets
  ucOverpayments <- ucOverpaymentsRegular %>% 
    left_join(ucOverpaymentsEUC91, by = c("st", "rptdate", "metric")) %>% 
    left_join(ucOverpaymentsTEUC02, by = c("st", "rptdate", "metric")) %>% 
    left_join(ucOverpaymentsEUC08, by = c("st", "rptdate", "metric")) %>%
    rowwise() %>% 
    # add up all of the like metrics per month per st (e.g. outstanding = outstandingregular + outstanting EUC02, etc....)
    mutate(total = sum(value, valueEUC91, valueEUC08, valueTEUC02, na.rm = T)) %>% 
    # remove all of the intermediate columns that we don't need anymore
    select(-value, -valueEUC91, -valueEUC08, -valueTEUC02) %>% 
    # and make a wide table again that has everything we want - total outstanding, etc...
    pivot_wider(names_from = "metric", values_from = "total")
  
  
  # compute US Averages and add them into the df
  usAvg <- ucOverpayments %>% 
    group_by(rptdate) %>% 
    summarize(across(where(is.numeric), ~round(mean(., 1))))
  
  ucOverpayments <- ucOverpayments %>%
    bind_rows(usAvg %>% mutate(st = "US")) %>%
    mutate(
      # then calculate the fraud percent
      fraud_num_percent = round((regular_fraud_num + federal_fraud_num) / 
                                  (regular_fraud_num + federal_fraud_num + 
                                     regular_nonfraud_num + federal_nonfraud_num), 
                                3))
  
  return(ucOverpayments)
}

# gets the selected series from FRED, adds in a state name, renames a few columns, and returns the DF
# can be used in a map function to get all 50 states; will sleep if sleep is set to true
# to avoid api limitations
get_fred_series_with_state_id <- function(series, metric_name, sleep = FALSE, start_date = as.Date("1976-01-01")) {
  
  # get the data from fredr; use a try catch b/c some series don't exist and we 
  # need to catch those and move on
  df <- tryCatch({
    fredr(series_id = series,
          observation_start = start_date)
  }, warning = function(c) {
    print(glue::glue("{series} does not exist.  Moving on. {c}"))
    return(NULL)
  }, message = function(c) {
    print(glue::glue("{series} does not exist.  Moving on.  {c}"))
    return(NULL)
  }, error = function(c) {
    print(glue::glue("{series} does not exist.  Moving on. {c}"))
    return(NULL)
  })
  
  # if we got an error, then end the function here.
  if(is.null(df)) return(df)
  
  # get the state abbreviation
  state = get_state_from_series_id(series)
  
  # do a few mutations to make the data match the other data we have
  df <- df %>% 
    mutate(st = state,
           metric = metric_name,
           # I want the data on unemployment to match with the 
           # other data we are collecting.  All other reports have a date
           # of the last day of the month in question; the bls data has a date
           # of the first day of the month; this normalizes everything
           rptdate = ceiling_date(date, "month") - 1) %>% 
    select(rptdate, st, metric, value)
  
  # sleep to avoid a rate limitation, if need be
  if(sleep) Sys.sleep(1)
  
  return(df)
}


# gets the state abbr from a fred series.  The one special case is the US, which we have
# to rename to US from USA
get_state_from_series_id <- function(series) {
  state <- fredr_series_tags(series) %>% 
    filter(group_id == "geo") %>% 
    slice(n()) %>% 
    pull(name) %>% 
    toupper()
  
  if(state == "USA") state = "US"
  
  return(state)
  
}



# combine bls unemployed info with general unemployment continuing claims numbers to get a recipiency rate
# generally speaking, recipiency rate is total continued claims in an average week / total unemployed over that week
# the function below returns a df with a recipiency rate that is smoothed with 12 month moving averages.
# it also calculates state, fed, and total recipiency rates separately so that you can see how much of the recipiency
# rate is from federal programs like EB and EUC.
# accepts as a parameter bls_unemployment data
getRecipiency <- function (bls_unemployed)
{
  
  
  # take the bls unemployment data and just extract the data that we need, which includes getting a 
  # 12 month moving averages of the unemployed number
  bls_unemployed <- bls_unemployed %>% filter(endsWith(metric, "nsa")) %>% 
    pivot_wider(names_from = metric, values_from = value) %>% 
    arrange(st, rptdate) %>% 
    group_by(st) %>% 
    mutate(unemployed_avg = round(rollmean(total_unemployed_nsa, k=12, align="right", na.pad=T), 0)) %>% 
    ungroup()
  
  
  ucClaimsPaymentsRegular <- downloadUCData("https://oui.doleta.gov/unemploy/csv/ar5159.csv") #5159 report
  ucClaimsPaymentsExtended <- downloadUCData("https://oui.doleta.gov/unemploy/csv/ae5159.csv") #5159 report
  ucClaimsPaymentsEUC91 <- downloadUCData("https://oui.doleta.gov/unemploy/csv/ac5159.csv") #5159 report
  ucClaimsPaymentsTEUC02 <- downloadUCData("https://oui.doleta.gov/unemploy/csv/at5159.csv") #5159 report
  ucClaimsPaymentsEUC08 <- downloadUCData("https://oui.doleta.gov/unemploy/csv/au5159.csv") #5159 report
  
  # EUC data from the 80s isn't available on the DOL website, but DOL provided a copy of those claims
  ucClaimsPaymentsEUC80s <- read.csv("EUC-1982-1987-USDOLData.csv")
  ucClaimsPaymentsEUC80s <- ucClaimsPaymentsEUC80s %>% 
    mutate(rptdate =  as.Date(rptdate))
  
  # name the columns that we care about for later code readability
  all_cols <- c("st","rptdate")
  reg_cols <- c("state_intrastate", "state_liable", "ucfe_instrastate", "ufce_liable", "ucx_intrastate", "ucx_liable", "state_compensated", "ucfe_ucx_compensated")
  ext_cols <- c("ext_state_intrastate", "ext_state_liable", "ext_ucfe_instrastate", "ext_ufce_liable", "ext_ucx_intrastate", "ext_ucx_liable", "ext_state_compensated", "ext_ucfe_ucx_compensated")
  euc91_cols <- c("euc91_state_intrastate", "euc91_state_liable", "euc91_ucfe_instrastate", "euc91_ufce_liable", "euc91_ucx_intrastate", "euc91_ucx_liable", "euc91_state_compensated", "euc91_ucfe_ucx_compensated")
  euc08_cols <- c("euc08_state_intrastate", "euc08_state_liable", "euc08_ucfe_instrastate", "euc08_ufce_liable", "euc08_ucx_intrastate", "euc08_ucx_liable", "euc08_state_compensated", "euc08_ucfe_ucx_compensated")
  teuc_cols <-  c("teuc02_state_intrastate", "teuc02_state_liable", "teuc02_ucfe_instrastate", "teuc02_ufce_liable", "teuc02_ucx_intrastate", "teuc02_ucx_liable", "teuc02_state_compensated")
  
  ucClaimsPaymentsRegular <- ucClaimsPaymentsRegular %>% 
    rename_at(vars(c("c21", "c24", "c27", "c30", "c33", "c36", "c45","c48")), ~ reg_cols)
  ucClaimsPaymentsExtended <- ucClaimsPaymentsExtended %>% 
    rename_at(vars(c("c12", "c15", "c18", "c21", "c24", "c27","c35", "c37")), ~ext_cols)
  
  ucClaimsPaymentsEUC91 <- ucClaimsPaymentsEUC91 %>% 
    rename_at(vars(c("c19", "c22", "c23", "c26", "c27", "c30", "c38","c42")), ~euc91_cols)
  ucClaimsPaymentsTEUC02 <- ucClaimsPaymentsTEUC02 %>% 
    rename_at(vars(c("c12", "c15", "c18", "c21", "c24", "c27","c35")),~teuc_cols)
  ucClaimsPaymentsEUC08 <- ucClaimsPaymentsEUC08 %>% 
    rename_at(vars(c("c12", "c15", "c18", "c21", "c24", "c27","c35", "c37")), ~euc08_cols)
  
  # merge the different datasets together and backfill with 0 if there is no data for a month
  ucRecipiency <- ucClaimsPaymentsRegular %>% select(all_of(c(all_cols, reg_cols))) %>% 
    left_join(ucClaimsPaymentsExtended %>% select(all_of(c(all_cols, ext_cols))), by = all_cols) %>% 
    left_join(ucClaimsPaymentsEUC91 %>% select(all_of(c(all_cols, euc91_cols))), by = all_cols) %>% 
    left_join(ucClaimsPaymentsTEUC02 %>% select(all_of(c(all_cols, teuc_cols))), by = all_cols) %>% 
    left_join(ucClaimsPaymentsEUC08 %>% select(all_of(c(all_cols, euc08_cols))), by = all_cols) %>% 
    left_join(ucClaimsPaymentsEUC80s %>% select(one_of(c(all_cols, "euc80s"))), by = all_cols) %>% 
    replace(is.na(.), 0)
    
  
  # sum the various numbers that we care about and then divide by the number of weeks in the month to get a weekly number rather than a monthly number
  ucRecipiency <- ucRecipiency %>% 
    mutate(reg_total = state_intrastate + state_liable + ucfe_instrastate + ufce_liable + ucx_intrastate + ucx_liable,
           fed_total = ext_state_intrastate + ext_state_liable + ext_ucfe_instrastate + ext_ufce_liable + ext_ucx_intrastate + 
             ext_ucx_liable + euc91_state_intrastate + euc91_state_liable + euc91_ucfe_instrastate + euc91_ufce_liable + 
             euc91_ucx_intrastate + euc91_ucx_liable + euc08_state_intrastate + euc08_state_liable + euc08_ucfe_instrastate + 
             euc08_ufce_liable + euc08_ucx_intrastate + euc08_ucx_liable + teuc02_state_intrastate + teuc02_state_liable + 
             teuc02_ucfe_instrastate + teuc02_ufce_liable + teuc02_ucx_intrastate + teuc02_ucx_liable + euc80s,
           total = reg_total + fed_total, 
           reg_total_week = reg_total / (as.numeric(days_in_month(rptdate)) / 7),
           fed_total_week = fed_total / (as.numeric(days_in_month(rptdate)) / 7),
           total_week = total / (as.numeric(days_in_month(rptdate)) / 7),
           
           # this is a measure of the total money paid out per month in all of the various programs.  Note that we are missing EUC80s....
           total_compensated = state_compensated + ucfe_ucx_compensated + euc91_state_compensated + euc91_ucfe_ucx_compensated + 
             teuc02_state_compensated + euc08_state_compensated + euc08_ucfe_ucx_compensated + ext_state_compensated + ext_ucfe_ucx_compensated,
           total_state_compensated = state_compensated + ucfe_ucx_compensated,
           total_federal_compensated = total_compensated - total_state_compensated
    ) %>% 
    arrange(rptdate) %>% 
    # 12 month moving averages of the above
    group_by(st) %>% 
    mutate(
      reg_total_week_mov_avg = rollmean(reg_total_week, k = 12, align = "right", na.pad = T),
      fed_total_week_mov_avg = rollmean(fed_total_week, k = 12, align = "right", na.pad = T),
      total_week_mov_avg = rollmean(total_week, k = 12, align = "right", na.pad = T),
      total_compensated_mov_avg = rollmean(total_compensated, k = 12, align = "right", na.pad = T),
      total_state_compensated_mov_avg = rollmean(total_state_compensated, k = 12, align = "right", na.pad = T),
      total_federal_compensated_mov_avg = rollmean(total_federal_compensated, k = 12, align = "right", na.pad = T)
    ) %>% 
    ungroup() #%>% 
    # # try to get rid of NAs that show up as a crazy integer in integer64 cols
    # mutate(total_compensated_mov_avg = if_else(total_compensated_mov_avg==9218868437227407266, NA_integer64_, total_compensated_mov_avg),
    #        total_state_compensated_mov_avg = if_else(total_state_compensated_mov_avg==9218868437227407266, NA_integer64_, total_state_compensated_mov_avg),
    #        total_federal_compensated_mov_avg = if_else(total_federal_compensated_mov_avg==9218868437227407266, NA_integer64_, total_federal_compensated_mov_avg))

  # now combine with the BLS unemployed data
  ucRecipiency <- ucRecipiency %>% 
    left_join(bls_unemployed %>% select(c("st", "rptdate", "total_unemployed_nsa", "unemployed_avg")), 
              by = c("st", "rptdate"))
  

  
  # compute US Averages and add them into the df
  usAvg <- ucRecipiency %>% 
    group_by(rptdate) %>% 
    summarize(across(where(is.numeric), mean, na.rm = T))
  
  ucRecipiency <- ucRecipiency %>% 
    bind_rows(usAvg %>% mutate(st = "US")) %>%
    mutate(
      # get recipiency rates
      recipiency_annual_reg = round(reg_total_week_mov_avg / unemployed_avg,3),
      recipiency_annual_total = round(total_week_mov_avg / unemployed_avg, 3)) %>% 
    replace(is.na(.), 0) %>% 
    # select just the cols that we need
    select(st,rptdate,total_week_mov_avg, unemployed_avg, recipiency_annual_reg, recipiency_annual_total, total_state_compensated_mov_avg,
           total_week_mov_avg, total_compensated, total_federal_compensated_mov_avg, total_state_compensated, total_federal_compensated,
           total_compensated_mov_avg)

  return(ucRecipiency)
}


# sort through the non monetary determinations to get information about separation and non-separation issues
getNonMonetaryDeterminations <- function()
{
  ucNonMonetaryRegular <- downloadUCData("https://oui.doleta.gov/unemploy/csv/ar207.csv") #207 report
  ucNonMonetaryExtended <- downloadUCData("https://oui.doleta.gov/unemploy/csv/ae207.csv") #207 report
  ucNonMonetaryEUC91 <- downloadUCData("https://oui.doleta.gov/unemploy/csv/ac207.csv") #207 report
  ucNonMonetaryTEUC02 <- downloadUCData("https://oui.doleta.gov/unemploy/csv/at207.csv") #207 report
  ucNonMonetaryEUC08 <- downloadUCData("https://oui.doleta.gov/unemploy/csv/au207.csv") #207 report
  
  # name the columns that we care about for later code readability
  # EUC08 and TEUC appear to have the same structure as extended benefits, not euc91
  all_cols <- c("st","rptdate")
  reg_cols <- c("state_total_determ", "ufce_total_determ", "ucx_total_determ", "state_determ_sep_total", "state_determ_sep_vol", "state_determ_sep_misconduct", "state_determ_sep_other", "state_denial_sep_total", "state_denial_sep_vol", "state_denial_sep_misconduct", "state_denial_sep_other", "ufce_determ_sep_total", "ufce_determ_sep_vol", "ufce_determ_sep_misconduct", "ufce_determ_sep_other", "ufce_denial_sep_total", "ufce_denial_sep_vol", "ufce_denial_sep_misconduct", "ufce_denial_sep_other", "state_determ_non_total", "state_determ_non_aa", "state_determ_non_income","state_determ_non_refusework","state_determ_non_reporting","state_determ_non_referrals","state_determ_non_other",  "state_denial_non_total", "state_denial_non_aa", "state_denial_non_income","state_denial_non_refusework","state_denial_non_reporting","state_denial_non_referrals","state_denial_non_other") 
  ext_cols <- c("ext_state_total_determ", "ext_ufce_total_determ", "ext_ucx_total_determ", "ext_state_determ_sep_total", "ext_state_determ_sep_vol", "ext_state_determ_sep_misconduct", "ext_state_determ_sep_other", "ext_state_denial_sep_total", "ext_state_denial_sep_vol", "ext_state_denial_sep_misconduct", "ext_state_denial_sep_other", "ext_state_determ_non_total", "ext_state_determ_non_aa", "ext_state_determ_non_refusework","ext_state_determ_non_other",  "ext_state_denial_non_total", "ext_state_denial_non_aa","ext_state_denial_non_refusework","ext_state_denial_non_other")
  euc91_cols <- c("euc91_state_total_determ", "euc91_ufce_total_determ", "euc91_ucx_total_determ", "euc91_state_determ_sep_total", "euc91_state_determ_sep_vol", "euc91_state_determ_sep_misconduct", "euc91_state_determ_sep_other", "euc91_state_denial_sep_total", "euc91_state_denial_sep_vol", "euc91_state_denial_sep_misconduct", "euc91_state_denial_sep_other", "euc91_state_determ_non_total", "euc91_state_determ_non_aa", "euc91_state_determ_non_income","euc91_state_determ_non_refusework", "euc91_state_determ_non_reporting","euc91_state_determ_non_other",  "euc91_state_denial_non_total", "euc91_state_denial_non_aa", "euc91_state_denial_non_income","euc91_state_denial_non_refusework","euc91_state_denial_non_reporting","euc91_state_denial_non_other")  
  euc08_cols <- c("euc08_state_total_determ", "euc08_ufce_total_determ", "euc08_ucx_total_determ", "euc08_state_determ_sep_total", "euc08_state_determ_sep_vol", "euc08_state_determ_sep_misconduct", "euc08_state_determ_sep_other", "euc08_state_denial_sep_total", "euc08_state_denial_sep_vol", "euc08_state_denial_sep_misconduct", "euc08_state_denial_sep_other", "euc08_state_determ_non_total", "euc08_state_determ_non_aa", "euc08_state_determ_non_refusework","euc08_state_determ_non_other",  "euc08_state_denial_non_total", "euc08_state_denial_non_aa","euc08_state_denial_non_refusework","euc08_state_denial_non_other")
  teuc_cols <- c("teuc_state_total_determ", "teuc_ufce_total_determ", "teuc_ucx_total_determ", "teuc_state_determ_sep_total", "teuc_state_determ_sep_vol", "teuc_state_determ_sep_misconduct", "teuc_state_determ_sep_other", "teuc_state_denial_sep_total", "teuc_state_denial_sep_vol", "teuc_state_denial_sep_misconduct", "teuc_state_denial_sep_other", "teuc_state_determ_non_total", "teuc_state_determ_non_aa", "teuc_state_determ_non_refusework","teuc_state_determ_non_other",  "teuc_state_denial_non_total", "teuc_state_denial_non_aa","teuc_state_denial_non_refusework","teuc_state_denial_non_other")

  ucNonMonetaryRegular <- ucNonMonetaryRegular %>% 
    rename_at(vars(c("c1", "c13", "c15", paste0("c", 17:37), "c45", paste0("c", 38:43), "c46", "c44")), ~reg_cols)
  ucNonMonetaryExtended <- ucNonMonetaryExtended %>% 
    rename_at(vars(c("c1", "c3", "c5", paste0("c", 7:22))), ~ext_cols)
  ucNonMonetaryEUC91 <- ucNonMonetaryEUC91 %>% 
    rename_at(vars(c("c1", "c3", "c5", paste0("c", 7:26))), ~euc91_cols)
  ucNonMonetaryTEUC02 <- ucNonMonetaryTEUC02 %>% 
    rename_at(vars(c("c1", "c3", "c5", paste0("c", 7:22))), ~teuc_cols)
  ucNonMonetaryEUC08 <- ucNonMonetaryEUC08 %>% 
    rename_at(vars(c("c1", "c3", "c5", paste0("c", 7:22))), ~euc08_cols)
  
  # merge the different datasets together and backfill with 0 if there is no data for a month
  ucNonMonetary <- ucNonMonetaryRegular %>% 
    select(all_of(c(all_cols,reg_cols))) %>% 
    left_join(ucNonMonetaryExtended %>% select(all_of(c(all_cols,ext_cols))), by = all_cols) %>% 
    left_join(ucNonMonetaryEUC91 %>% select(all_of(c(all_cols,euc91_cols))), by = all_cols) %>% 
    left_join(ucNonMonetaryTEUC02 %>% select(all_of(c(all_cols,teuc_cols))), by = all_cols) %>% 
    left_join(ucNonMonetaryEUC08 %>% select(all_of(c(all_cols,euc08_cols))), by = all_cols) %>% 
    replace(is.na(.), 0)
  
  # do some math to get some interesting information for graphing purposes
  # so we want to find out the percentage of determinations that are non-sep denials and sep denials
  # we also want to find out the percentage of each denial type under sep and non-sep as a % of total denials
  # this involves adding together categories first and then doing lots of division.  Fun!
  # first just get our aggregate totals with all federal programs integrated with state numbers
  ucNonMonetary <- ucNonMonetary %>% 
    mutate(
      determ_total = state_total_determ + ufce_total_determ + ucx_total_determ + ext_state_total_determ + 
        ext_ufce_total_determ + ext_ucx_total_determ + euc08_state_total_determ + euc08_ufce_total_determ + 
        euc08_ucx_total_determ + teuc_state_total_determ + teuc_ucx_total_determ + teuc_ufce_total_determ + 
        euc91_state_total_determ + euc91_ufce_total_determ + euc91_ucx_total_determ,
      determ_sep_vol = state_determ_sep_vol + ufce_determ_sep_vol + ext_state_determ_sep_vol + 
        euc08_state_determ_sep_vol + euc91_state_determ_sep_vol + teuc_state_determ_sep_vol,
      determ_sep_misconduct = state_determ_sep_misconduct + ufce_determ_sep_misconduct + 
        ext_state_determ_sep_misconduct + euc08_state_determ_sep_misconduct + euc91_state_determ_sep_misconduct + 
        teuc_state_determ_sep_misconduct,
      determ_sep_other = state_determ_sep_other + ufce_determ_sep_other + ext_state_determ_sep_other + 
        euc08_state_determ_sep_other + euc91_state_determ_sep_other + teuc_state_determ_sep_other,
      determ_non_aa = state_determ_non_aa + ext_state_determ_non_aa + euc08_state_determ_non_aa + 
        euc91_state_determ_non_aa + teuc_state_determ_non_aa,
      determ_non_income = state_determ_non_income + euc91_state_determ_non_income,
      determ_non_refusework = state_determ_non_refusework + ext_state_determ_non_refusework + 
        euc08_state_determ_non_refusework + euc91_state_determ_non_refusework + 
        teuc_state_determ_non_refusework,
      determ_non_reporting = state_determ_non_reporting + euc91_state_determ_non_reporting,
      determ_non_referrals = state_determ_non_referrals,
      determ_non_other = state_determ_non_other + ext_state_determ_non_other + euc08_state_determ_non_other + 
        euc91_state_determ_non_other + teuc_state_determ_non_other,
      denial_sep_total = state_denial_sep_total + ufce_denial_sep_total + ext_state_denial_sep_total + 
        euc08_state_denial_sep_total + euc91_state_denial_sep_total + teuc_state_denial_sep_total,
      denial_non_total = state_denial_non_total + ext_state_denial_non_total + euc08_state_denial_non_total + 
        euc91_state_denial_non_total + teuc_state_denial_non_total,
      denial_total = denial_sep_total + denial_non_total,
      denial_sep_misconduct = state_denial_sep_misconduct + ufce_denial_sep_misconduct + 
        ext_state_denial_sep_misconduct + euc08_state_denial_sep_misconduct + 
        euc91_state_denial_sep_misconduct + teuc_state_denial_sep_misconduct,
      denial_sep_vol = state_denial_sep_vol + ufce_denial_sep_vol + ext_state_denial_sep_vol + 
        euc08_state_denial_sep_vol + euc91_state_denial_sep_vol + teuc_state_denial_sep_vol,
      denial_sep_other = state_denial_sep_other + ufce_denial_sep_other + ext_state_denial_sep_other + 
        euc08_state_denial_sep_other + euc91_state_denial_sep_other + teuc_state_denial_sep_other,
      denial_non_aa = state_denial_non_aa + ext_state_denial_non_aa + euc08_state_denial_non_aa + 
        euc91_state_denial_non_aa + teuc_state_denial_non_aa,
      denial_non_income = state_denial_non_income + euc91_state_denial_non_income,
      denial_non_refusework = state_denial_non_refusework + ext_state_denial_non_refusework + 
        euc08_state_denial_non_refusework + euc91_state_denial_non_refusework + 
        teuc_state_denial_non_refusework,
      denial_non_reporting = state_denial_non_reporting + euc91_state_denial_non_reporting,
      denial_non_referrals = state_denial_non_referrals,
      denial_non_other = state_denial_non_other + ext_state_denial_non_other + euc08_state_denial_non_other + 
        euc91_state_denial_non_other + teuc_state_denial_non_other,
      
      # now calculate our actual statistics that we care about
      denial_rate_overall = round(denial_total / determ_total, 3),
      denial_sep_percent = round(denial_sep_total / determ_total,3),
      denial_sep_rate = round(denial_sep_total / (determ_sep_misconduct + determ_sep_vol + 
                                                    determ_sep_other),3),
      denial_non_percent = round(denial_non_total / determ_total,3),
      denial_non_rate = round(denial_non_total / 
                                (determ_non_aa + determ_non_income + determ_non_refusework + 
                                   determ_non_reporting + determ_non_referrals + determ_non_other),3),
      denial_sep_misconduct_percent = round(denial_sep_misconduct / denial_sep_total,3),
      denial_sep_misconduct_rate = round(denial_sep_misconduct / determ_sep_misconduct, 3),
      denial_sep_vol_percent = round(denial_sep_vol / denial_sep_total,3),
      denial_sep_vol_rate = round(denial_sep_vol / determ_sep_vol,3),
      denial_sep_other_percent = round(denial_sep_other / denial_sep_total,3),
      denial_sep_other_rate = round(denial_sep_other / determ_sep_other, 3),
      denial_non_aa_percent = round(denial_non_aa / denial_non_total, 3),
      denial_non_income_percent = round(denial_non_income / denial_non_total, 3),
      denial_non_refusework_percent = round(denial_non_refusework / denial_non_total, 3),
      denial_non_reporting_percent = round(denial_non_reporting / denial_non_total, 3),
      denial_non_referrals_percent = round(denial_non_referrals / denial_non_total, 3),
      denial_non_other_percent = round(denial_non_other / denial_non_total, 3),
      denial_non_aa_rate = round(denial_non_aa / determ_non_aa, 3),
      denial_non_income_rate = round(denial_non_income / determ_non_income, 3),
      denial_non_refusework_rate = round(denial_non_refusework / determ_non_refusework, 3),
      denial_non_reporting_rate = round(denial_non_reporting / determ_non_reporting, 3),
      denial_non_referrals_rate = round(denial_non_referrals / determ_non_referrals, 3),
      denial_non_other_rate = round(denial_non_other / determ_non_other, 3)) %>% 
    select(all_of(all_cols), starts_with(c("denial_", "determ_"))) %>% 
    # there seems to be some bad data in the dataset--every once in a while, a state will misreport total denials by a factor of 10
    # This makes the proportions > 1, which obviously doesn't makes much sense.  
    # for now, I am going to just set any proportion > 1 to 1.  But in the future, may make more sense
    # to capture 'total denials' and 'total determinations' by adding the subsets together.
    # this function looks at every "*percent" column and sets the max value to 1.
    mutate_at(vars(ends_with(c("percent", "rate"))), ~if_else(. > 1, 1, .)) %>% 
    mutate_at(vars(ends_with(c("percent", "rate"))), ~if_else(is.na(.), 0, .))
  
  
  # compute US Averages and add them into the df
  usAvg <- ucNonMonetary %>% 
    group_by(rptdate) %>% 
    summarize(across(where(is.numeric), mean))
  
  ucNonMonetary <- ucNonMonetary %>% 
    bind_rows(usAvg %>% mutate(st = "US"))
  
  
  return(ucNonMonetary)  
}

getUCFirstTimePaymentLapse <- function() {
  
  # download the data
  ucFirstTimePaymentLapse <- downloadUCData("https://oui.doleta.gov/unemploy/csv/ar9050.csv") # 9050 report

  #set some names
  ucFirstTimePaymentLapse <- ucFirstTimePaymentLapse %>% 
    rename_at(vars(c("c1", "c9", "c17", "c25", "c33", "c41", "c49", "c57", "c65","c73","c81","c89")), 
           ~c("first_time_payment_total", "x0x7", "x8x14", "x15x21", "x22x28", "x29x35", "x36x42","x43x49","x50x56","x57x63","x64x70","xOver70" ))
  
  # calculate some values
  ucFirstTimePaymentLapse <- ucFirstTimePaymentLapse %>% 
    mutate(
      first_time_payment_Within15Days = round((x0x7 + x8x14) / first_time_payment_total, 3),
      first_time_payment_Within35Days = round((x0x7 + x8x14 + x15x21 + x22x28 + x29x35) / first_time_payment_total, 3),
      first_time_payment_Within49Days = round((x0x7 + x8x14 + x15x21 + x22x28 + x29x35 + x36x42 + x43x49) / first_time_payment_total, 3),
      first_time_payment_Within70Days = round((x0x7 + x8x14 + x15x21 + x22x28 + x29x35 + x36x42+ x43x49 + 
                              x50x56 + x57x63 + x64x70) / first_time_payment_total,3),
      first_time_payment_Over70Days = round((x0x7 + x8x14 + x15x21 + x22x28 + x29x35 + x36x42 + x43x49 + x50x56 + 
                            x57x63 + x64x70 + xOver70) / first_time_payment_total, 3)) %>% 
  
    # we only need to choose certain columns, so this isn't strictly necessary, but is a convenience
    select(all_of(c("st","rptdate","first_time_payment_Within15Days","first_time_payment_Within35Days", "first_time_payment_Within49Days", "first_time_payment_Within70Days", "first_time_payment_total"))) 
  
  #compute US Averages
  # compute US Averages and add them into the df
  usAvg <- ucFirstTimePaymentLapse %>% 
    group_by(rptdate) %>% 
    summarize(across(where(is.numeric), function(x) round(mean(x), 3))) %>% 
    mutate(first_time_payment_total = NA) # this is ported from earlier code; I'm not sure why I did this back then
  
  ucFirstTimePaymentLapse <- ucFirstTimePaymentLapse %>% 
    bind_rows(usAvg %>% mutate(st = "US")) %>% 
    mutate(st = as.factor(st))
  
  
  ## mgh note to later self - this is doing somethign weird.  it is computing a US average AND it is 
  # both making a row out of it and extra columns on each row.  Seems like a waste.  In redoing
  # this, I'm going to skip that.
  #paymentAvg <- aggregate(cbind(Within15Days,Within35Days, Within49Days, Within70Days) ~ rptdate, paymentTimeliness, FUN=function(x) round(mean(x),3))
  #setnames(paymentAvg, c("rptdate","Avg15Day", "Avg35Day", "Avg49Day","Avg70Day"))
  #paymentTimeliness <- merge(paymentTimeliness, paymentAvg, by="rptdate")
  
  return(ucFirstTimePaymentLapse)
}


getUCAppealsTimeLapseLower <- function(ucBenefitAppealsRegular) {

  # get the data
  ucAppealsTimeLapseLower <- downloadUCData("https://oui.doleta.gov/unemploy/csv/ar9054l.csv") # 9054 report
  ucAppealsCaseAgingLower <- downloadUCData("https://oui.doleta.gov/unemploy/csv/ar9055l.csv") # 9055 report
  
  # set some names
  ucAppealsTimeLapseLower <- ucAppealsTimeLapseLower %>% 
    rename_at(vars(c("c1", "c4", "c7")), 
           ~ c("Total", "x0x30", "x31x45"))
  ucAppealsCaseAgingLower <- ucAppealsCaseAgingLower %>% rename(lower_total = c1)
  
  # calculate some values
  ucAppealsTimeLapseLower <- ucAppealsTimeLapseLower %>% 
    mutate(
      lower_Within30Days = round(x0x30 / Total, 3),
      lower_Within45Days = round((x0x30 + x31x45) / Total, 3)) %>% 
    select(all_of(c("st","rptdate","lower_Within30Days","lower_Within45Days")))
  
  # need to add EUC and EB into this, but not now
  ucAppealsTimeLapseLower <- ucAppealsTimeLapseLower %>% 
    full_join(ucAppealsCaseAgingLower %>% 
                select(all_of(c("st","rptdate","lower_total"))), by=c("st", "rptdate")) %>% 
    full_join(ucBenefitAppealsRegular %>% 
                select(all_of(c("st", "rptdate", "lower_filed","lower_disposed"))), by=c("st", "rptdate"))

  # compute US Averages  
  usAvg <- ucAppealsTimeLapseLower %>% 
    group_by(rptdate) %>% 
    summarize(across(where(is.numeric), function(x) round(mean(x), 3))) %>% 
    mutate(lower_total = NA) # this is ported from earlier code; I'm not sure why I did this back then
  
  ucAppealsTimeLapseLower <- ucAppealsTimeLapseLower %>% 
    bind_rows(usAvg %>% mutate(st = "US")) %>% 
    mutate(st = as.factor(st))

  # mgh: note to self - same.  We are making averges and then adding them to each row
  # setnames(refereeAvg, c("rptdate","shortAvg", "longAvg"))
  # refereeTimeliness <- merge(refereeTimeliness, refereeAvg, by="rptdate")
  # refereeTimeliness <- refereeTimeliness[,c("st", "rptdate","Within30Days","Within45Days", "lower_filed","lower_disposed","Total", "shortAvg","longAvg")]
  # 
  # then merge in the same data, but as a separate "state" for US Avg
  return(ucAppealsTimeLapseLower)
}

getucAppealsTimeLapseHigher <- function() {
  
  # download data
  ucAppealsTimeLapseHigher <- downloadUCData("https://oui.doleta.gov/unemploy/csv/ar9054h.csv") # 9054 report
  ucAppealsCaseAgingHigher <- downloadUCData("https://oui.doleta.gov/unemploy/csv/ar9055h.csv") # 9055 report
  
  # set some names
  ucAppealsTimeLapseHigher <- ucAppealsTimeLapseHigher %>% 
    rename_at(vars(c("c1", "c4", "c7", "c10")), ~c("Total", "x0x45", "x46x60", "x61x75"))
  ucAppealsCaseAgingHigher <- ucAppealsCaseAgingHigher %>% 
    rename(higher_total = c1)
  
  #calculate soome values
  ucAppealsTimeLapseHigher <- ucAppealsTimeLapseHigher %>% 
    mutate(
      higher_Within45Days = round(x0x45 / Total,3),
      higher_Within75Days = round((x0x45 + x46x60 + x61x75) / Total,3)) %>% 
    select(one_of(c("st","rptdate","higher_Within45Days","higher_Within75Days"))) %>% 
    # merge with UCCaseAging Data
    full_join(ucAppealsCaseAgingHigher %>% 
                select(all_of(c("st","rptdate","higher_total"))), 
              by=c("st", "rptdate")) %>% 
    # merge with ucbenefitappeal data, but not EUC stuff yet
    full_join(ucBenefitAppealsRegular %>% 
                select(all_of(c("st", "rptdate", "higher_filed","higher_disposed"))),
              by=c("st", "rptdate"))
  
  #compute US Averages
  usAvg <- ucAppealsTimeLapseHigher %>% 
    group_by(rptdate) %>% 
    summarize(across(where(is.numeric), function(x) round(mean(x), 3))) %>% 
    mutate(higher_total = NA) # this is ported from earlier code; I'm not sure why I did this back then
  
  ucAppealsTimeLapseHigher <- ucAppealsTimeLapseHigher %>% 
    bind_rows(usAvg %>% mutate(st = "US")) %>% 
    mutate(st = as.factor(st))
  
  # mgh: note again that we are adding in the average to each row and then again at the bottom
  # ucbrAvg <- aggregate(cbind(Within45Days, Within75Days) ~ rptdate, ucbrTimeliness, FUN=function(x) round(mean(x),3))
  # setnames(ucbrAvg, c("rptdate","shortAvg", "longAvg"))
  # 
  # # first merge in the average to each row
  # ucbrTimeliness <- merge(ucbrTimeliness, ucbrAvg, by="rptdate")
  # ucbrTimeliness <- ucbrTimeliness[, c("st", "rptdate","Within45Days","Within75Days", "higher_filed","higher_disposed", "Total", "shortAvg","longAvg")]
  # 
  # #then merge in the same data, but as a separate "state"
  # ucbrAvg$st <- "US"
  # ucbrAvg$Within45Days <- ucbrAvg$shortAvg
  # ucbrAvg$Within75Days <- ucbrAvg$longAvg
  # ucbrAvg[c("higher_filed","higher_disposed", "Total")] <- NA
  # ucbrTimeliness <- rbind(ucbrTimeliness, ucbrAvg)
  # 
  
  return(ucAppealsTimeLapseHigher)
}

getUCBenefitAppeals <- function(url) {
  df <- downloadUCData(url) %>%  # 5130 report
    setBenefitAppealNames()
  return(df)
}


# gets the unemployment rate and total unemployed for all 50 states + DC + the US;
# uses a sleep within each request (1sec) so it takes on the order of 5 minutes to retrieve all of the data that we want
# without hitting a rate limit
labor_force_info <- bind_rows(
  map_dfr(c("CLF16OV", "DCLF", paste0(state.abb, "LF")), get_fred_series_with_state_id, "labor_force_sa", sleep = TRUE),
  map_dfr(c("CIVPART", paste0("LBSSA", str_pad(1:56,width = 2, side = "left", pad = "0"))), get_fred_series_with_state_id, "labor_force_participation_rate_sa", sleep = TRUE)
) %>% 
  pivot_wider(names_from = metric, values_from = value) %>% 
  mutate(civilian_non_insitutionalized_population_sa = 100 * labor_force_sa / labor_force_participation_rate_sa) %>% 
  pivot_longer(cols = 3:5, names_to = "metric")

bls_unemployed <- bind_rows(
  map_dfr(c("UNRATE", "DCUR", paste0(state.abb, "UR")), get_fred_series_with_state_id, "unemployment_rate_sa", sleep = TRUE),
  map_dfr(c("UNRATENSA", "DCURN", paste0(state.abb, "URN")), get_fred_series_with_state_id, "unemployment_rate_nsa", sleep = TRUE),
  map_dfr(c("UNEMPLOY", paste0("LASST", str_pad(1:56,width = 2, side = "left", pad = "0"), "0000000000004")), get_fred_series_with_state_id, "total_unemployed_sa", sleep = TRUE),
  map_dfr(c("LNU03000000", paste0("LAUST", str_pad(1:56,width = 2, side = "left", pad = "0"), "0000000000004")), get_fred_series_with_state_id, "total_unemployed_nsa", sleep = TRUE),
  labor_force_info)


ucFirstTimePaymentLapse <- getUCFirstTimePaymentLapse()

# get all of the appeals information
ucBenefitAppealsRegular <- getUCBenefitAppeals("https://oui.doleta.gov/unemploy/csv/ar5130.csv") 
ucBenefitAppealsExtended <- getUCBenefitAppeals("https://oui.doleta.gov/unemploy/csv/ae5130.csv") 
ucBenefitAppealsEUC91x94 <- getUCBenefitAppeals("https://oui.doleta.gov/unemploy/csv/ac5130.csv") 
ucBenefitAppealsEUC02x04 <- getUCBenefitAppeals("https://oui.doleta.gov/unemploy/csv/at5130.csv") 
ucBenefitAppealsEUC08x13 <- getUCBenefitAppeals("https://oui.doleta.gov/unemploy/csv/au5130.csv") 

ucAppealsTimeLapseLower <- getUCAppealsTimeLapseLower(ucBenefitAppealsRegular)
ucAppealsTimeLapseHigher <- getucAppealsTimeLapseHigher()


# get UC recipiency and overpayments
ucRecipiency <- getRecipiency(bls_unemployed)
ucOverpayments <- getOverpayments()

# add in the uc payments by month into the ucOverpayments data to get overpayments as a percent of annual costs
### mgh: look into this section
ucOverpayments <- ucOverpayments %>% 
  left_join(ucRecipiency %>% 
              select(all_of(
                c("st","rptdate","total_state_compensated", "total_compensated", 
                  "total_federal_compensated", "total_federal_compensated_mov_avg", 
                  "total_state_compensated_mov_avg", "total_compensated_mov_avg"))), 
            by=c("st", "rptdate"))
ucOverpayments$total_paid_annual_mov_avg <- ucOverpayments$total_compensated_mov_avg*12
ucRecipiency$total_paid_annual_mov_avg <- ucRecipiency$total_compensated_mov_avg*12
ucOverpayments$outstanding_proportion <- round(ucOverpayments$outstanding / ucOverpayments$total_paid_annual_mov_avg,4)

# get determination data
ucNonMonetary <- getNonMonetaryDeterminations()


# make long-uberdf
unemployment_df <- 
  map_dfr(list(ucNonMonetary, ucOverpayments, ucRecipiency, ucFirstTimePaymentLapse, 
             ucAppealsTimeLapseLower, ucAppealsTimeLapseHigher), 
        function(x) { 
          x %>% 
            pivot_longer(cols = !one_of(c("rptdate", "st")), names_to = "metric", values_to = "value")}) %>% 
  bind_rows(bls_unemployed) %>% 
  # there are a few repeat metrics that get thrown in there by accident; get rid of them:
  distinct()

arrow::write_feather(unemployment_df, "~/unemployment_data.feather")




