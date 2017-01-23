# Helper app to download and process data from the DOL website
# THe downloads can be found here: http://ows.doleta.gov/unemploy/DataDownloads.asp
# For each download below, there is a data definition pdf that explains what each field is that is being sought

#library(data.table)
library(ggplot2)
library(data.table)
#library(curl)
#library(RCurl)
#library(httr)
#library(openssl)

downloadUCData <- function (URL) {
  #mydata = read.csv(text=getURL(URL, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE))
  
  #bar = RCurl::getURLContent(URL)
  #foo = textConnection(bar)
  #mydata = read.csv(foo)
  #close(foo)

  #doc <- content(GET(URL), as="text")
  #mydata <- read.csv(text=doc)
  
  # convert dates to a date type
  mydata <- fread(URL)
  mydata$rptdate <- as.Date(mydata$rptdate,("%m/%d/%Y"))
  return(mydata)
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

ucFirstTimePaymentLapse <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar9050.csv") # 9050 report

ucAppealsTimeLapseLower <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar9054l.csv") # 9054 report
ucAppealsTimeLapseHigher <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar9054h.csv") # 9054 report

ucAppealsCaseAgingLower <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar9055l.csv") # 9055 report
ucAppealsCaseAgingHigher <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar9055h.csv") # 9055 report

ucBenefitAppealsRegular <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ar5130.csv") # 5130 report
ucBenefitAppealsExtended <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ae5130.csv") # 5130 report
ucBenefitAppealsEUC91x94 <- downloadUCData("https://ows.doleta.gov/unemploy/csv/ac5130.csv") # 5130 report
ucBenefitAppealsEUC02x04 <- downloadUCData("https://ows.doleta.gov/unemploy/csv/at5130.csv") # 5130 report
ucBenefitAppealsEUC08x13 <- downloadUCData("https://ows.doleta.gov/unemploy/csv/au5130.csv") # 5130 report

# set the column names for the data that we're interested in
setBenefitAppealNames(ucBenefitAppealsRegular)
setBenefitAppealNames(ucBenefitAppealsExtended)
setBenefitAppealNames(ucBenefitAppealsEUC08x13)
setBenefitAppealNames(ucBenefitAppealsEUC02x04)
setBenefitAppealNames(ucBenefitAppealsEUC91x94)

#9050 report
setnames(ucFirstTimePaymentLapse, c("c1", "c9", "c17", "c25", "c33", "c41", "c49"), c("Total", "x0x7", "x8x14", "x15x21", "x22x28", "x29x35", "x36x42"))
#9054 reports
setnames(ucAppealsTimeLapseLower, c("c1", "c4", "c7"), c("Total", "x0x30", "x31x45"))
setnames(ucAppealsTimeLapseHigher, c("c1", "c4", "c7", "c10"), c("Total", "x0x45", "x46x60", "x61x75"))

#9055 reports
setnames(ucAppealsCaseAgingLower, "c1", "Total")
setnames(ucAppealsCaseAgingHigher, "c1", "Total")

#calculated values
ucAppealsTimeLapseLower$Within30Days <- round(ucAppealsTimeLapseLower$x0x30 / ucAppealsTimeLapseLower$Total, 3)
ucAppealsTimeLapseLower$Within45Days <- round((ucAppealsTimeLapseLower$x0x30 + ucAppealsTimeLapseLower$x31x45) / ucAppealsTimeLapseLower$Total, 3)
ucAppealsTimeLapseHigher$Within45Days <- round(ucAppealsTimeLapseHigher$x0x45 / ucAppealsTimeLapseHigher$Total,3)
ucAppealsTimeLapseHigher$Within75Days <- round((ucAppealsTimeLapseHigher$x0x45 + ucAppealsTimeLapseHigher$x46x60 + ucAppealsTimeLapseHigher$x61x75) / ucAppealsTimeLapseHigher$Total,3)

ucFirstTimePaymentLapse$Within15Days <- round((ucFirstTimePaymentLapse$x0x7+ucFirstTimePaymentLapse$x8x14) / ucFirstTimePaymentLapse$Total,3)
ucFirstTimePaymentLapse$Within35Days <- round((ucFirstTimePaymentLapse$x0x7+ucFirstTimePaymentLapse$x8x14 + ucFirstTimePaymentLapse$x15x21 + ucFirstTimePaymentLapse$x22x28 + ucFirstTimePaymentLapse$x29x35) / ucFirstTimePaymentLapse$Total,3)



# merge the data sets to get DFs that have only the columns that we need/want for display

# need to add EUC and EB into this, but not now
refereeTimeliness <- merge(ucAppealsTimeLapseLower[,c("st","rptdate","Within30Days","Within45Days")], ucAppealsCaseAgingLower[,c("st","rptdate","Total")], by=c("st", "rptdate"), all=TRUE)
refereeTimeliness <- merge(refereeTimeliness, ucBenefitAppealsRegular[,c("st", "rptdate", "lower-filed","lower-disposed")], by=c("st", "rptdate"), all=TRUE)
# compute US Averages
refereeAvg <- aggregate(cbind(Within30Days, Within45Days) ~ rptdate, refereeTimeliness, FUN= function(x) round(mean(x),3))
setnames(refereeAvg, c("rptdate","shortAvg", "longAvg"))
refereeTimeliness <- merge(refereeTimeliness, refereeAvg, by="rptdate")
refereeTimeliness <- refereeTimeliness[,c("st", "rptdate","Within30Days","Within45Days", "lower-filed","lower-disposed","Total", "shortAvg","longAvg")]

# then merge in the same data, but as a separate "state" for US Avg
refereeAvg$st <- "US"
refereeAvg$Within30Days <- refereeAvg$shortAvg
refereeAvg$Within45Days <- refereeAvg$longAvg
refereeAvg[c("lower-filed","lower-disposed","Total")] <- NA
refereeTimeliness <- rbind(refereeTimeliness,refereeAvg)

ucbrTimeliness <- merge(ucAppealsTimeLapseHigher[,c("st","rptdate","Within45Days","Within75Days")], ucAppealsCaseAgingHigher[,c("st","rptdate","Total")], by=c("st", "rptdate"), all=TRUE)
ucbrTimeliness <- merge(ucbrTimeliness, ucBenefitAppealsRegular[,c("st", "rptdate", "higher-filed","higher-disposed")], by=c("st", "rptdate"), all=TRUE)
#compute US Averages
ucbrAvg <- aggregate(cbind(Within45Days, Within75Days) ~ rptdate, ucbrTimeliness, FUN=function(x) round(mean(x),3))
setnames(ucbrAvg, c("rptdate","shortAvg", "longAvg"))

# first merge in the average to each row
ucbrTimeliness <- merge(ucbrTimeliness, ucbrAvg, by="rptdate")
ucbrTimeliness <- ucbrTimeliness[, c("st", "rptdate","Within45Days","Within75Days", "higher-filed","higher-disposed", "Total", "shortAvg","longAvg")]

#then merge in the same data, but as a separate "state"
ucbrAvg$st <- "US"
ucbrAvg$Within45Days <- ucbrAvg$shortAvg
ucbrAvg$Within75Days <- ucbrAvg$longAvg
ucbrAvg[c("higher-filed","higher-disposed", "Total")] <- NA
ucbrTimeliness <- rbind(ucbrTimeliness, ucbrAvg)


# we only need to choose certain columns, so this isn't strictly necessary, but is a convenience
paymentTimeliness <- ucFirstTimePaymentLapse[,c("st","rptdate","Within15Days","Within35Days", "Total")]
#compute US Averages
paymentAvg <- aggregate(cbind(Within15Days, Within35Days) ~ rptdate, paymentTimeliness, FUN=function(x) round(mean(x),3))
setnames(paymentAvg, c("rptdate","shortAvg", "longAvg"))
paymentTimeliness <- merge(paymentTimeliness, paymentAvg, by="rptdate")

#then merge in payment Avg as separate "state" for US averages
paymentAvg$st <- "US"
paymentAvg$Within15Days <- paymentAvg$shortAvg
paymentAvg$Within35Days <- paymentAvg$longAvg
paymentAvg$Total <- NA
paymentTimeliness <- rbind(paymentTimeliness,paymentAvg)

#uiTable <- melt(subset(refereeTimeliness, st=="PA", select=c("rptdate", "Within30Days", "Within45Days")) ,id.vars="rptdate")


# uPlot <- ggplot(uiTable, aes(rptdate, value, col=variable)) +
#   geom_point() +
#   stat_smooth() + 
#   labs(x="Date") + 
#   ggtitle("hi")
# 
# uPlot +
#   geom_hline(aes(yintercept=.9), linetype="dashed") +
#   geom_text(aes(x=as.Date("2000-01-01"), y=.9,label = "the line", vjust = -1), color="black")
# 
# ?geom_hline
