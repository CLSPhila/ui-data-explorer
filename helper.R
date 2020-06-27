#' helper.R
#' A series of functions to help with the UI data explorer shiny app

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


# a df of the recessions; used for graphing
recession_df <- data.frame(start = c("1969-12-01", "1973-11-01", "1980-01-01",  
                                     "1981-07-01", "1990-07-01", "2001-03-01", "2007-12-01", "2020-02-01"),
                           end = c("1970-11-01", "1975-03-01", "1980-07-01",
                                   "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01", NA))


# graphing.... get the max dollars; use for scale of the graph
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


