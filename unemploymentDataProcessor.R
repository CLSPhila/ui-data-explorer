# Helper app to download and process data from the DOL website
# THe downloads can be found here: http://ows.doleta.gov/unemploy/DataDownloads.asp
# For each download below, there is a data definition pdf that explains what each field is that is being sought

library(RCurl)
library(ggplot2)
library(data.table)
library(lubridate)
library(dplyr)
library(zoo)
library(rgdal)
library(leaflet)
require(bit64)


downloadUCData <- function (URL) {
  
  # first try to find the file on the filesystem.  If we can't find it
  # on the file system, then download it
  csvFile <- file.path("data", basename(URL))
  if (file.exists(csvFile))
      mydata <- read.csv(csvFile)
  else
      mydata <- fread(URL)
  
  # convert dates to a date type
  mydata %>% 
    mutate(rptdate = as.Date(rptdate, "%m/%d/%Y"))
} 

# sets the names of specific columns in the 5130 dataset
setBenefitAppealNames <- function(df) {
  setnames(df, c("c9", "c13", "c10", "c14"), c("lower-filed", "lower-disposed", "higher-filed", "higher-disposed"))
}

# return a list of all of the states
getStates <- function (df) {
  return(unique(df$st))
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

  ucOverpaymentsRegular <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar227.csv") #227 report
  ucOverpaymentsEUC91 <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ac227.csv") #227 report
  ucOverpaymentsTEUC02 <- downloadUCData("https://ows.doleta.gov/unemploy/csv/at227.csv") #227 report
  ucOverpaymentsEUC08 <- downloadUCData("https://ows.doleta.gov/unemploy/csv/au227.csv") #227 report

  # cols that we want to keep
  overpayment_cols <- c("st","rptdate","regular_fraud_num","federal_fraud_num","regular_fraud_dol","federal_fraud_dol","regular_nonfraud_num","federal_nonfraud_num","regular_nonfraud_dol","federal_nonfraud_dol","state_tax_recovery","federal_tax_recovery", "outstanding", "recovered")
  all_cols <- c("st","rptdate")
  detection_cols <- c("yfederal_fraud_num","yfederal_fraud_dol","yfederal_nonfraud_num","yfederal_nonfraud_dol", "youtstanding", "yrecovered")
  recovery_cols <- c("ystate_tax_recovery","yfederal_tax_recovery")
  
  
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
    select(one_of(overpayment_cols))
    
  ucOverpaymentsEUC91 <- ucOverpaymentsEUC91 %>% 
    mutate( 
      yfederal_fraud_num = c1 + c2,
      yfederal_fraud_dol = c3 + c4,
      yfederal_nonfraud_num = c5 + c6,
      yfederal_nonfraud_dol = c7 + c8,
      youtstanding = c9 + c10 + c11 + c12,
      yrecovered = c13 + c14 + c15 + c16
    ) %>% 
    select(one_of(all_cols, detection_cols))
  
  ucOverpaymentsTEUC02 <- ucOverpaymentsTEUC02 %>% 
    mutate(
      yfederal_fraud_num = c1 + c2,
      yfederal_fraud_dol = c3 + c4,
      yfederal_nonfraud_num = c27 + c28,
      yfederal_nonfraud_dol = c29 + c30,
      ystate_tax_recovery = c210 + c211 + c212 + c213,
      yfederal_tax_recovery = c214 + c215 + c216 + c217,
      youtstanding = c35 + c36 + c37 + c38,
      yrecovered = c206 + c207 + c208 + c209,
    ) %>% 
    select(one_of(all_cols, detection_cols, recovery_cols))
  
  ucOverpaymentsEUC08 <- ucOverpaymentsEUC08 %>% 
    mutate(
      yfederal_fraud_num = c1 + c2,
      yfederal_fraud_dol = c3 + c4,
      yfederal_nonfraud_num = c27 + c28,
      yfederal_nonfraud_dol = c29 + c30,
      ystate_tax_recovery = c210 + c211 + c212 + c213,
      yfederal_tax_recovery = c214 + c215 + c216 + c217,
      youtstanding = c35 + c36 + c37 + c38,
      yrecovered = c206 + c207 + c208 + c209,
    ) %>% 
    select(one_of(all_cols, detection_cols, recovery_cols))
  
  # join the four dfs together and do some calculaations
  
  combine_ucOverpaymentDFs <- function(overpayment_df, extension_df, 
                                       all_cols, overpayment_cols) {
    df <- overpayment_df %>% 
      left_join(extension_df, by = all_cols) %>% 
      rowwise() %>% 
      mutate(
        federal_fraud_num = sum(federal_fraud_num, yfederal_fraud_num, na.rm = T),
        federal_fraud_dol = sum(federal_fraud_dol, yfederal_fraud_dol, na.rm = T),
        federal_nonfraud_num = sum(federal_nonfraud_num, yfederal_nonfraud_num, na.rm = T),
        federal_nonfraud_dol = sum(federal_nonfraud_dol,yfederal_nonfraud_dol, na.rm = T),
        outstanding = sum(outstanding, youtstanding, na.rm = T),
        recovered = sum(recovered, yrecovered, na.rm = T)
      ) %>% 
      ungroup() 

    # for some of the extensions, we track state_tax_recovery as well
    if("ystate_tax_recovery" %in% colnames(extension_df)) {
      df <- df %>% 
        rowwise() %>% 
        mutate(
          state_tax_recovery = sum(state_tax_recovery, ystate_tax_recovery, na.rm = T),
          federal_tax_recovery = sum(federal_tax_recovery, yfederal_tax_recovery, na.rm = T)
        ) %>% 
        ungroup()
    }
    
    # keep only some columns
    df %>% 
      select(one_of(overpayment_cols))
  }
  
  ucOverpayments <- ucOverpaymentsRegular %>% 
    combine_ucOverpaymentDFs(ucOverpaymentsEUC91, all_cols, overpayment_cols) %>% 
    combine_ucOverpaymentDFs(ucOverpaymentsTEUC02, all_cols, overpayment_cols) %>% 
    combine_ucOverpaymentDFs(ucOverpaymentsEUC08, all_cols, overpayment_cols)
  
  
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


# get unemployment information from the BLS website and convert to a datatable.  This function takes a long
# time to run.  Param nsa refers to the not-seasonally-adjusdted figures vs seasonally adjusted
get_bls_employment_data <- function(nsa=TRUE) {
  if (nsa)
    blsurl <- "https://www.bls.gov/web/laus/ststdnsadata.txt"
  else
    blsurl <- "https://www.bls.gov/web/laus/ststdsadata.txt"
  tf <- tempfile()
  download.file(blsurl, tf)
  #ststdnsadata <- curl::curl(blsurl) %>% readLines()
  #fp <- file(tf)
  #write(ststdnsadata, fp)
  #close(fp)
  doc <- readLines(tf)
  doc <- sub('^ +', '', doc, perl=TRUE)
  doc <- gsub(',', '', doc, perl=TRUE)
  doc <- gsub(' [.][.]+', '', doc, perl=TRUE)
  cols <- strsplit(doc, '  +', perl=TRUE)
  
  month.name <- format(ISOdate(1900, 1:12, 1), "%B")
  results <- NULL
  curdate <- NULL
  n <- length(cols)
  i <- 1L
  for (d in cols){
    if (length(d) == 1){
      e = strsplit(d[1], ' ')[[1]]
      if (e[1] %in% month.name){
        curdate <- as.Date(paste(e[1], '1', e[2], sep=' '), "%B %d %Y")
      }
    }
    else if (d[1] %in% state.name){
      if (is.null(results)){
        results <- data.frame(state=rep(d[1], n),
                              pop=rep(as.integer(d[2]), n),
                              total=rep(as.integer(d[3]), n),
                              perc_pop=rep(as.numeric(d[4]), n),
                              total_employed=rep(as.integer(d[5]), n),
                              perc_employed=rep(as.numeric(d[6]), n),
                              total_unemployed=rep(as.integer(d[7]), n),
                              perc_unemployed=rep(as.numeric(d[8]), n),
                              month=rep(curdate, n))
      }
      else{
        i <- i + 1L
        set(results, i, 1L, d[1])
        set(results, i, 2L, as.integer(d[2]))
        set(results, i, 3L, as.integer(d[3]))
        set(results, i, 4L, as.numeric(d[4]))
        set(results, i, 5L, as.integer(d[5]))
        set(results, i, 6L, as.numeric(d[6]))
        set(results, i, 7L, as.integer(d[7]))
        set(results, i, 8L, as.numeric(d[8]))
        set(results, i, 9L, curdate)
      }
    }
  }
  results <- results[-seq(i + 1L, n), ]
}


# combine bls unemployed info with general unemployment continuing claims numbers to get a recipiency rate
# generally speaking, recipiency rate is total continued claims in an average week / total unemployed over that week
# the function below returns a df with a recipiency rate that is smoothed with 12 month moving averages.
# it also calculates state, fed, and total recipiency rates separately so that you can see how much of the recipiency
# rate is from federal programs like EB and EUC
getRecipiency <- function ()
{
  
  # start by downloading the bls employment data to get unemployed per month
  bls_unemployed <- get_bls_employment_data(nsa=TRUE)
  
  # add in the state abbreviations and make a new column with the last date of the month to match with DOL data
  bls_unemployed <- bls_unemployed %>% 
    mutate(
      st = state.abb[match(state, state.name)],
      rptdate = ceiling_date(month, "month") - 1
    )
  
  
  #arrange the BLS data by state then month and then do 12 month moving averages of the unemployed number
  bls_unemployed <- bls_unemployed %>%
    arrange(st, rptdate) %>% 
    group_by(st) %>% 
    mutate(unemployed_avg = round(rollmean(total_unemployed, k=12, align="right", na.pad=T), 0)) %>% 
    ungroup()
  
  
  ucClaimsPaymentsRegular <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar5159.csv") #5159 report
  ucClaimsPaymentsExtended <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ae5159.csv") #5159 report
  ucClaimsPaymentsEUC91 <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ac5159.csv") #5159 report
  ucClaimsPaymentsTEUC02 <- downloadUCData("https://ows.doleta.gov/unemploy/csv/at5159.csv") #5159 report
  ucClaimsPaymentsEUC08 <- downloadUCData("https://ows.doleta.gov/unemploy/csv/au5159.csv") #5159 report
  
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
  setnames(ucClaimsPaymentsRegular, c("c21", "c24", "c27", "c30", "c33", "c36", "c45","c48"), reg_cols)
  setnames(ucClaimsPaymentsExtended, c("c12", "c15", "c18", "c21", "c24", "c27","c35", "c37"), ext_cols)
  setnames(ucClaimsPaymentsEUC91, c("c19", "c22", "c23", "c26", "c27", "c30", "c38","c42"), euc91_cols)
  setnames(ucClaimsPaymentsTEUC02, c("c12", "c15", "c18", "c21", "c24", "c27","c35"),teuc_cols)
  setnames(ucClaimsPaymentsEUC08, c("c12", "c15", "c18", "c21", "c24", "c27","c35", "c37"), euc08_cols)
  
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
      total_federal_compensated_mov_avg = rollmean(total_federal_compensated, k = 12, align = "right", na.pad = T),
    ) %>% 
    ungroup() %>% 
    # try to get rid of NAs that show up as a crazy integer in integer64 cols
    mutate(total_compensated_mov_avg = if_else(total_compensated_mov_avg==9218868437227407266, NA_integer64_, total_compensated_mov_avg),
           total_state_compensated_mov_avg = if_else(total_state_compensated_mov_avg==9218868437227407266, NA_integer64_, total_state_compensated_mov_avg),
           total_federal_compensated_mov_avg = if_else(total_federal_compensated_mov_avg==9218868437227407266, NA_integer64_, total_federal_compensated_mov_avg))
  

  # now combine with the BLS unemployed data
  ucRecipiency <- ucRecipiency %>% 
    left_join(bls_unemployed %>% select(c("st", "rptdate", "total_unemployed", "unemployed_avg")), 
              by = c("st", "rptdate"))
  
  
  # compute US Averages and add them into the df
  usAvg <- ucRecipiency %>% 
    group_by(rptdate) %>% 
    summarize(across(where(is.numeric), mean))
  
  ucRecipiency <- ucRecipiency %>% 
    bind_rows(usAvg %>% mutate(st = "US")) %>%
    mutate(
      # get recipiency rates
      recipiency_annual_reg = round(reg_total_week_mov_avg / unemployed_avg,3),
      recipiency_annual_total = round(total_week_mov_avg / unemployed_avg, 3)) %>% 
    replace(is.na(.), 0)

  return(ucRecipiency)
}


# sort through the non monetary determinations to get information about separation and non-separation issues
getNonMonetaryDeterminations <- function()
{
  ucNonMonetaryRegular <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar207.csv") #207 report
  ucNonMonetaryExtended <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ae207.csv") #207 report
  ucNonMonetaryEUC91 <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ac207.csv") #207 report
  ucNonMonetaryTEUC02 <- downloadUCData("https://ows.doleta.gov/unemploy/csv/at207.csv") #207 report
  ucNonMonetaryEUC08 <- downloadUCData("https://ows.doleta.gov/unemploy/csv/au207.csv") #207 report
  
  # name the columns that we care about for later code readability
  # EUC08 and TEUC appear to have the same structure as extended benefits, not euc91
  all_cols <- c("st","rptdate")
  reg_cols <- c("state_total_determ", "ufce_total_determ", "ucx_total_determ", "state_determ_sep_total", "state_determ_sep_vol", "state_determ_sep_misconduct", "state_determ_sep_other", "state_denial_sep_total", "state_denial_sep_vol", "state_denial_sep_misconduct", "state_denial_sep_other", "ufce_determ_sep_total", "ufce_determ_sep_vol", "ufce_determ_sep_misconduct", "ufce_determ_sep_other", "ufce_denial_sep_total", "ufce_denial_sep_vol", "ufce_denial_sep_misconduct", "ufce_denial_sep_other", "state_determ_non_total", "state_determ_non_aa", "state_determ_non_income","state_determ_non_refusework","state_determ_non_reporting","state_determ_non_referrals","state_determ_non_other",  "state_denial_non_total", "state_denial_non_aa", "state_denial_non_income","state_denial_non_refusework","state_denial_non_reporting","state_denial_non_referrals","state_denial_non_other") 
  ext_cols <- c("ext_state_total_determ", "ext_ufce_total_determ", "ext_ucx_total_determ", "ext_state_determ_sep_total", "ext_state_determ_sep_vol", "ext_state_determ_sep_misconduct", "ext_state_determ_sep_other", "ext_state_denial_sep_total", "ext_state_denial_sep_vol", "ext_state_denial_sep_misconduct", "ext_state_denial_sep_other", "ext_state_determ_non_total", "ext_state_determ_non_aa", "ext_state_determ_non_refusework","ext_state_determ_non_other",  "ext_state_denial_non_total", "ext_state_denial_non_aa","ext_state_denial_non_refusework","ext_state_denial_non_other")
  euc91_cols <- c("euc91_state_total_determ", "euc91_ufce_total_determ", "euc91_ucx_total_determ", "euc91_state_determ_sep_total", "euc91_state_determ_sep_vol", "euc91_state_determ_sep_misconduct", "euc91_state_determ_sep_other", "euc91_state_denial_sep_total", "euc91_state_denial_sep_vol", "euc91_state_denial_sep_misconduct", "euc91_state_denial_sep_other", "euc91_state_determ_non_total", "euc91_state_determ_non_aa", "euc91_state_determ_non_income","euc91_state_determ_non_refusework", "euc91_state_determ_non_reporting","euc91_state_determ_non_other",  "euc91_state_denial_non_total", "euc91_state_denial_non_aa", "euc91_state_denial_non_income","euc91_state_denial_non_refusework","euc91_state_denial_non_reporting","euc91_state_denial_non_other")  
  euc08_cols <- c("euc08_state_total_determ", "euc08_ufce_total_determ", "euc08_ucx_total_determ", "euc08_state_determ_sep_total", "euc08_state_determ_sep_vol", "euc08_state_determ_sep_misconduct", "euc08_state_determ_sep_other", "euc08_state_denial_sep_total", "euc08_state_denial_sep_vol", "euc08_state_denial_sep_misconduct", "euc08_state_denial_sep_other", "euc08_state_determ_non_total", "euc08_state_determ_non_aa", "euc08_state_determ_non_refusework","euc08_state_determ_non_other",  "euc08_state_denial_non_total", "euc08_state_denial_non_aa","euc08_state_denial_non_refusework","euc08_state_denial_non_other")
  teuc_cols <- c("teuc_state_total_determ", "teuc_ufce_total_determ", "teuc_ucx_total_determ", "teuc_state_determ_sep_total", "teuc_state_determ_sep_vol", "teuc_state_determ_sep_misconduct", "teuc_state_determ_sep_other", "teuc_state_denial_sep_total", "teuc_state_denial_sep_vol", "teuc_state_denial_sep_misconduct", "teuc_state_denial_sep_other", "teuc_state_determ_non_total", "teuc_state_determ_non_aa", "teuc_state_determ_non_refusework","teuc_state_determ_non_other",  "teuc_state_denial_non_total", "teuc_state_denial_non_aa","teuc_state_denial_non_refusework","teuc_state_denial_non_other")

  setnames(ucNonMonetaryRegular, c("c1", "c13", "c15", paste0("c", 17:37), "c45", paste0("c", 38:43), "c46", "c44"), reg_cols)
  setnames(ucNonMonetaryExtended, c("c1", "c3", "c5", paste0("c", 7:22)), ext_cols)
  setnames(ucNonMonetaryEUC91, c("c1", "c3", "c5", paste0("c", 7:26)), euc91_cols)
  setnames(ucNonMonetaryTEUC02, c("c1", "c3", "c5", paste0("c", 7:22)),teuc_cols)
  setnames(ucNonMonetaryEUC08, c("c1", "c3", "c5", paste0("c", 7:22)), euc08_cols)
  
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
    mutate_if(ends_with(c("percent", "rate")), ~if_else(. > 1, 1, .))
  
  
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
  ucFirstTimePaymentLapse <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar9050.csv") # 9050 report

  #set some names
  setnames(ucFirstTimePaymentLapse, 
           c("c1", "c9", "c17", "c25", "c33", "c41", "c49", "c57", "c65","c73","c81","c89"), 
           c("Total", "x0x7", "x8x14", "x15x21", "x22x28", "x29x35", "x36x42","x43x49","x50x56","x57x63","x64x70","xOver70" ))
  
  # calculate some values
  ucFirstTimePaymentLapse <- ucFirstTimePaymentLapse %>% 
    mutate(
      Within15Days = round((x0x7 + x8x14) / Total, 3),
      Within35Days = round((x0x7 + x8x14 + x15x21 + x22x28 + x29x35) / Total, 3),
      Within49Days = round((x0x7 + x8x14 + x15x21 + x22x28 + x29x35 + x36x42 + x43x49) / Total, 3),
      Within70Days = round((x0x7 + x8x14 + x15x21 + x22x28 + x29x35 + x36x42+ x43x49 + 
                              x50x56 + x57x63 + x64x70) / Total,3),
      Over70Days = round((x0x7 + x8x14 + x15x21 + x22x28 + x29x35 + x36x42 + x43x49 + x50x56 + 
                            x57x63 + x64x70 + xOver70) / Total,3)) %>% 
  
    # we only need to choose certain columns, so this isn't strictly necessary, but is a convenience
    select(all_of(c("st","rptdate","Within15Days","Within35Days", "Within49Days", "Within70Days", "Total"))) 
  
  #compute US Averages
  # compute US Averages and add them into the df
  usAvg <- ucFirstTimePaymentLapse %>% 
    group_by(rptdate) %>% 
    summarize(across(where(is.numeric), function(x) round(mean(x), 3))) %>% 
    mutate(Total = NA) # this is ported from earlier code; I'm not sure why I did this back then
  
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


getUCFirstTimePaymentLapse <- function(ucBenefitAppealsRegular) {

  # get the data
  ucAppealsTimeLapseLower <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar9054l.csv") # 9054 report
  ucAppealsCaseAgingLower <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar9055l.csv") # 9055 report
  
  # set some names
  setnames(ucAppealsTimeLapseLower, 
           c("c1", "c4", "c7"), 
           c("Total", "x0x30", "x31x45"))
  setnames(ucAppealsCaseAgingLower, "c1", "Total")
  
  # calculate some values
  ucAppealsTimeLapseLower <- ucAppealsTimeLapseLower %>% 
    mutate(
      Within30Days = round(x0x30 / Total, 3),
      Within45Days = round((x0x30 + x31x45) / Total, 3)) %>% 
    select(all_of(c("st","rptdate","Within30Days","Within45Days")))
  
  # need to add EUC and EB into this, but not now
  ucAppealsTimeLapseLower <- ucAppealsTimeLapseLower %>% 
    full_join(ucAppealsCaseAgingLower %>% 
                select(all_of(c("st","rptdate","Total"))), by=c("st", "rptdate")) %>% 
    full_join(ucBenefitAppealsRegular %>% 
                select(all_of(c("st", "rptdate", "lower-filed","lower-disposed"))), by=c("st", "rptdate"))

  # compute US Averages  
  usAvg <- ucAppealsTimeLapseLower %>% 
    group_by(rptdate) %>% 
    summarize(across(where(is.numeric), function(x) round(mean(x), 3))) %>% 
    mutate(Total = NA) # this is ported from earlier code; I'm not sure why I did this back then
  
  ucAppealsTimeLapseLower <- ucAppealsTimeLapseLower %>% 
    bind_rows(usAvg %>% mutate(st = "US")) %>% 
    mutate(st = as.factor(st))

  # mgh: note to self - same.  We are making averges and then adding them to each row
  # setnames(refereeAvg, c("rptdate","shortAvg", "longAvg"))
  # refereeTimeliness <- merge(refereeTimeliness, refereeAvg, by="rptdate")
  # refereeTimeliness <- refereeTimeliness[,c("st", "rptdate","Within30Days","Within45Days", "lower-filed","lower-disposed","Total", "shortAvg","longAvg")]
  # 
  # then merge in the same data, but as a separate "state" for US Avg
  return(ucAppealsTimeLapseLower)
}

getucAppealsTimeLapseHigher <- function() {
  
  # download data
  ucAppealsTimeLapseHigher <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar9054h.csv") # 9054 report
  ucAppealsCaseAgingHigher <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar9055h.csv") # 9055 report
  
  # set some names
  setnames(ucAppealsTimeLapseHigher, c("c1", "c4", "c7", "c10"), c("Total", "x0x45", "x46x60", "x61x75"))
  setnames(ucAppealsCaseAgingHigher, "c1", "Total")
  
  #calculate soome values
  ucAppealsTimeLapseHigher <- ucAppealsTimeLapseHigher %>% 
    mutate(
      Within45Days = round(x0x45 / Total,3),
      Within75Days = round((x0x45 + x46x60 + x61x75) / Total,3)) %>% 
    select(one_of(c("st","rptdate","Within45Days","Within75Days"))) %>% 
    # merge with UCCaseAging Data
    full_join(ucAppealsCaseAgingHigher %>% 
                select(all_of(c("st","rptdate","Total"))), 
              by=c("st", "rptdate")) %>% 
    # merge with ucbenefitappeal data, but not EUC stuff yet
    full_join(ucBenefitAppealsRegular %>% 
                select(all_of(c("st", "rptdate", "higher-filed","higher-disposed"))),
              by=c("st", "rptdate"))
  
  #compute US Averages
  usAvg <- ucAppealsTimeLapseHigher %>% 
    group_by(rptdate) %>% 
    summarize(across(where(is.numeric), function(x) round(mean(x), 3))) %>% 
    mutate(Total = NA) # this is ported from earlier code; I'm not sure why I did this back then
  
  ucAppealsTimeLapseHigher <- ucAppealsTimeLapseHigher %>% 
    bind_rows(usAvg %>% mutate(st = "US")) %>% 
    mutate(st = as.factor(st))
  
  # mgh: note again that we are adding in the average to each row and then again at the bottom
  # ucbrAvg <- aggregate(cbind(Within45Days, Within75Days) ~ rptdate, ucbrTimeliness, FUN=function(x) round(mean(x),3))
  # setnames(ucbrAvg, c("rptdate","shortAvg", "longAvg"))
  # 
  # # first merge in the average to each row
  # ucbrTimeliness <- merge(ucbrTimeliness, ucbrAvg, by="rptdate")
  # ucbrTimeliness <- ucbrTimeliness[, c("st", "rptdate","Within45Days","Within75Days", "higher-filed","higher-disposed", "Total", "shortAvg","longAvg")]
  # 
  # #then merge in the same data, but as a separate "state"
  # ucbrAvg$st <- "US"
  # ucbrAvg$Within45Days <- ucbrAvg$shortAvg
  # ucbrAvg$Within75Days <- ucbrAvg$longAvg
  # ucbrAvg[c("higher-filed","higher-disposed", "Total")] <- NA
  # ucbrTimeliness <- rbind(ucbrTimeliness, ucbrAvg)
  # 
  
  return(ucAppealsTimeLapseHigher)
}

getUCBenefitAppeals <- function(url) {
  df <- downloadUCData(url) %>%  # 5130 report
    setBenefitAppealNames()
  return(df)
}


ucFirstTimePaymentLapse <- getUCFirstTimePaymentLapse()

# get all of the appeals information
ucBenefitAppealsRegular <- getUCBenefitAppeals("https://ows.doleta.gov/unemploy/csv/ar5130.csv") 
ucBenefitAppealsExtended <- getUCBenefitAppeals("https://ows.doleta.gov/unemploy/csv/ae5130.csv") 
ucBenefitAppealsEUC91x94 <- getUCBenefitAppeals("https://ows.doleta.gov/unemploy/csv/ac5130.csv") 
ucBenefitAppealsEUC02x04 <- getUCBenefitAppeals("https://ows.doleta.gov/unemploy/csv/at5130.csv") 
ucBenefitAppealsEUC08x13 <- getUCBenefitAppeals("https://ows.doleta.gov/unemploy/csv/au5130.csv") 

ucAppealsTimeLapseLower <- getUCAppealsTimeLapseLower(ucBenefitAppealsRegular)
ucAppealsTimeLapseHigher <- getucAppealsTimeLapseHigher()




# get seasonally adjusted unemployment data and change state names to abbrs
bls_unemployed_sa <- get_bls_employment_data(nsa=FALSE)
bls_unemployed_sa$state <- state.abb[match(bls_unemployed_sa$state,state.name)]

# calculate the US numbers
bls_us <- aggregate(cbind(pop, total, total_employed, total_unemployed) ~ month, bls_unemployed_sa, FUN=function(x) round(sum(x),3))
bls_us$state <- "US"
bls_us$perc_unemployed <- round(bls_us$total_unemployed/bls_us$total * 100,1)
bls_us$perc_employed <- round(bls_us$total_employed/bls_us$pop * 100,1)
bls_us$perc_pop <- round(bls_us$total/bls_us$pop * 100,1)
bls_us <- bls_us[c("state","pop","total", "perc_pop","total_employed","perc_employed","total_unemployed","perc_unemployed","month")]

#now merge in us data, as a separate "state" and rename a few columns
bls_unemployed_sa <- rbind(bls_unemployed_sa, bls_us)
names(bls_unemployed_sa)[names(bls_unemployed_sa)=='state'] <- "st"
names(bls_unemployed_sa)[names(bls_unemployed_sa)=='month'] <- "rptdate"


# recession data; not implemented in the charting yet
recessions.df = read.table(textConnection(
  "Peak, Trough
  1969-12-01, 1970-11-01
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)


# get the UC recipiency table
ucRecipiency <- getRecipiency()

ucOverpayments <- getOverpayments()

# add in the uc payments by month into the ucOverpayments data to get overpayments as a percent of annual costs
ucOverpayments <- merge(ucOverpayments, ucRecipiency[,c("st","rptdate","total_state_compensated", "total_compensated", "total_federal_compensated", "total_federal_compensated_mov_avg", "total_state_compensated_mov_avg", "total_compensated_mov_avg")], by=c("st", "rptdate"), all.x=TRUE)
ucOverpayments$total_paid_annual_mov_avg <- ucOverpayments$total_compensated_mov_avg*12
ucRecipiency$total_paid_annual_mov_avg <- ucRecipiency$total_compensated_mov_avg*12
ucOverpayments$outstanding_proportion <- round(ucOverpayments$outstanding / ucOverpayments$total_paid_annual_mov_avg,4)

# get determination data
ucNonMonetary <- getNonMonetaryDeterminations()

#get the max dollars; use for scale of the graph
maxOverpaymentDollars <- max(c(ucOverpayments$state_tax_recovery,ucOverpayments$federal_tax_recovery))
maxOutstandingOverpayment <- max(ucOverpayments$outstanding)
maxUIPayments <- max(ucRecipiency$total_compensated_mov_avg, na.rm=TRUE)
maxOutstandingProportion <- max(ucOverpayments$outstanding_proportion, na.rm = TRUE)
maxUnemployedRecipients <- max(ucRecipiency$total_week_mov_avg, ucRecipiency$unemployed_avg, na.rm = TRUE)
maxUnemploymentRate <- max(bls_unemployed_sa$perc_unemployed)
maxDenials <- max(ucNonMonetary$denial_rate_overall, na.rm = TRUE)
maxSepDenials <- max(ucNonMonetary$denial_sep_misconduct_percent, ucNonMonetary$denial_sep_vol_percent, ucNonMonetary$denial_sep_other_percent, na.rm = TRUE)
maxNonSepDenials <- max(ucNonMonetary$denial_non_aa_percent,ucNonMonetary$denial_non_income_percent, ucNonMonetary$denial_non_refusework_percent, ucNonMonetary$denial_non_reporting_percent, ucNonMonetary$denial_non_referrals_percent, ucNonMonetary$denial_non_other_percent, na.rm=TRUE)
maxSepDenialRate <- max(ucNonMonetary$denial_sep_misconduct_rate, ucNonMonetary$denial_sep_vol_rate, ucNonMonetary$denial_sep_other_rate, na.rm=TRUE)
maxNonSepDenialRate <- max(ucNonMonetary$denial_non_aa_rate,ucNonMonetary$denial_non_income_rate, ucNonMonetary$denial_non_refusework_rate, ucNonMonetary$denial_non_reporting_rate, ucNonMonetary$denial_non_referrals_rate, ucNonMonetary$denial_non_other_rate, na.rm=TRUE)

tmp <- tempdir()
unzip("cb_2015_us_state_20m.zip", exdir = tmp)
 
usa <- readOGR(dsn=tmp, layer="cb_2015_us_state_20m")
# 
# #add data to the usa dataframe
# maxDate = max(ucRecipiency$rptdate)
# # use a left join b/c there is no data for DC and PR in most cases
# usa@data = usa@data %>%
#   left_join(ucRecipiency[ucRecipiency$rptdate==maxDate,c("st","rptdate","recipiency_annual_total")], by=c("STUSPS"="st"))
# 
# # this creates the color mapping
#pal <- colorBin("Reds", NULL, bins = 7)
pal <- colorNumeric("Reds",NULL)

getUIMap <- function(usa,df,uiDate,dfColumn, stateText, reverseLevels) 
{
  # the dates that come in can be anywhere in the month b/c of the way the date slider works, so we have to 
  # get the end of the month
  uiDate <- ceiling_date(uiDate,"month") - days(1)
  # this is b/c the stats are on different release cycles--some release earlier and later in the month, some release quarterly
  # so this allows us to pick a date that doesn't exist, but fall back to something that does
  if(!(uiDate %in% df$rptdate))
    uiDate <- max(df$rptdate)
  
  df <- subset(df, rptdate==uiDate, select=c("st","rptdate",dfColumn))
  
  # flop the measure col if reverse is true so that we can do the shading correctly when high is good
  if (reverseLevels)
    df$measureCol <- 1/df[[dfColumn]]
  else
    df$measureCol <- df[[dfColumn]]
  
  # use a left join b/c there is no data for DC and PR in most cases
  usa@data = usa@data %>%
    left_join(df, by=c("STUSPS"="st"), copy=TRUE)

  
  state_popup <- paste0(stateText,
                        "<br><strong>", 
                        usa$NAME, 
                        "</strong>: ",
                        usa[[dfColumn]])
  

  # print out the map.  The problem here is that the palettes are white->red as you go from bad->good, which
  # means that we often will want to resort everything 
  uiMap <- leaflet(data = usa) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~pal(measureCol), 
                fillOpacity = 0.8, 
                color = "#BDBDC3", 
                weight = 1, 
                popup = state_popup) %>%
    setView(lng = -93.85, lat = 37.45, zoom = 4)
  

  return(uiMap)
}

# a function to generate small multiple plots of 50-state data, compared against the US average for a given measure
# measure - the measure to graph
getSMPlot <- function(dfData, startDate, endDate, measure, yLabel, plotTitle)
{ 
  
  # small multiple plot
  smPlot  <- ggplot(subset(dfData, rptdate > as.Date(startDate) & rptdate < as.Date(endDate) & !(st %in% c("US","PR","VI","DC")), select=c("rptdate","st", measure)), aes_string(x="rptdate", y=measure)) +
     geom_line(size=1.1, color="gray29") +
     facet_wrap(~ st, ncol=5) +
     geom_line(data=subset(dfData, rptdate > as.Date(startDate) & rptdate < as.Date(endDate) & st == "US", select=c("rptdate",measure)), aes_string(x="rptdate", y=measure), color="tomato3", linetype="dashed") +
     theme_minimal() +
     theme(plot.title = element_text(face="bold", hjust=.5, size=20),
           legend.position="top",
           legend.title = element_blank(),
           axis.title = element_text(size=10, face="bold"),
           axis.text = element_text(size=10),
           strip.text.x = element_text(face="bold")
     ) +
    labs(x="Date", y=yLabel) + 
    ggtitle(plotTitle) 
  return(smPlot) 
}


# a function to generate a single state vs 50-state overview on one graphs
# measure - the measure to graph
get50StateComparisonPlot <- function(dfData, startDate, endDate, measure, highlightState, yLabel, plotTitle)
{ 
  plot <- ggplot(dfData[dfData$rptdate > as.Date(startDate) & dfData$rptdate < as.Date(endDate) & !(dfData$st %in% c("US","PR","VI","DC")), c("rptdate","st", measure)], aes_string(x="rptdate", y=measure, group="st")) +
    geom_line(color="grey") +
    geom_line(data=dfData[dfData$rptdate > as.Date(startDate) & dfData$rptdate < as.Date(endDate) & dfData$st==highlightState, c("rptdate", "st", measure)], color="tomato3", size=2) +
    theme_minimal() +
    theme(plot.title = element_text(face="bold", hjust=.5, size=20),
          legend.position="top",
          legend.title = element_blank(),
          axis.title = element_text(size=17, face="bold"),
          axis.text = element_text(size=17)
    ) +
    ggtitle(plotTitle) +
    labs(x="Date", y=yLabel)
  return(plot)
}


# # make an uber DF that has all of the measures that we want and then melt/do a facet_grid
# uberDF <- ucRecipiency[,c("rptdate","st", "recipiency_annual_total")]
# 
# bls_unemployed_sa$rptdate <- bls_unemployed_sa$rptdate + months(1)-days(1)
# uberDF <- merge(uberDF, bls_unemployed_sa[,c("rptdate", "st", "perc_unemployed")], by=c("st", "rptdate"), all=TRUE)
# uberDF <- merge(uberDF, ucNonMonetary[,c("rptdate","st","denial_sep_percent", "denial_non_percent", "denial_rate_overall", "denial_sep_rate", "denial_non_rate")], by=c("st", "rptdate"), all=TRUE)
# 
# uberMelt <- melt(uberDF[,c("rptdate","st","perc_unemployed", "recipiency_annual_total", "denial_rate_overall", "denial_sep_rate", "denial_non_rate")],id.vars=c("st", "rptdate"))
# uberMelt$variable <- factor(uberMelt$variable, levels=c("perc_unemployed", "recipiency_annual_total", "denial_rate_overall", "denial_sep_rate", "denial_non_rate"), labels=c("Unemp Rate", "Recipiency Rate", "Non-Mon Denial Rate", "Sep Denial Rate", "Non-Sep Denial Rate"))
# 
# #smPlot  <- ggplot(uberMelt[!(uberMelt$st %in% c("US","PR","VI","DC")) & !is.na(uberMelt$value),], aes_string(x="rptdate", y="value")) +
# smPlot  <- ggplot(uberMelt[uberMelt$st %in% c("AK","PA") & !is.na(uberMelt$value),], aes_string(x="rptdate", y="value")) +
#   geom_line(size=1.1, color="gray29") +
#   facet_wrap(st ~ variable, ncol=5, scales="free_y", labeller=labeller(.cols=label_value, .multi_line=FALSE)) +
#   geom_line(data=subset(dfData, rptdate > as.Date(startDate) & rptdate < as.Date(endDate) & st == "US", select=c("rptdate",measure)), aes_string(x="rptdate", y=measure), color="tomato3", linetype="dashed") +
# #  geom_line(data=uberMelt[uberMelt$st=="US" & !is.na(uberMelt$value),c("rptdate", "variable", "value")], aes_string(x="rptdate", y="value"), color="tomato3", linetype="dashed") +
#   theme_minimal() +
#   theme(plot.title = element_text(face="bold", hjust=.5, size=20),
#         legend.position="top",
#         legend.title = element_blank(),
#         axis.title = element_text(size=10, face="bold"),
#         axis.text = element_text(size=10),
#         strip.text.x = element_text(face="bold")
#   ) +
#   ggtitle("A 50-State Look at the Health of Unemployment Systems") 
# 
# ggsave("out2.png", plot=smPlot, device="png", width=15, height=50, units="in", limitsize=FALSE)


