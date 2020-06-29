#' helper.R
#' A series of functions to help with the UI data explorer shiny app

library(rgdal)
library(leaflet)
library(ggplot2)
library(lubridate)

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
recession_df <- data.frame(start = as.Date(c("1969-12-01", "1973-11-01", "1980-01-01",  
                                     "1981-07-01", "1990-07-01", "2001-03-01", "2007-12-01", "2020-02-01")),
                           end = as.Date(c("1970-11-01", "1975-03-01", "1980-07-01",
                                   "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01", NA))) %>% 
  replace(is.na(.), today())


# graphing.... get the max dollars; use for scale of the graph
# maxOverpaymentDollars <- max(c(ucOverpayments$state_tax_recovery,ucOverpayments$federal_tax_recovery))
# maxOutstandingOverpayment <- max(ucOverpayments$outstanding)
# maxUIPayments <- max(ucRecipiency$total_compensated_mov_avg, na.rm=TRUE)
# maxOutstandingProportion <- max(ucOverpayments$outstanding_proportion, na.rm = TRUE)
# maxUnemployedRecipients <- max(ucRecipiency$total_week_mov_avg, ucRecipiency$unemployed_avg, na.rm = TRUE)
# maxUnemploymentRate <- max(bls_unemployed_sa$perc_unemployed)
# maxDenials <- max(ucNonMonetary$denial_rate_overall, na.rm = TRUE)
# maxSepDenials <- max(ucNonMonetary$denial_sep_misconduct_percent, ucNonMonetary$denial_sep_vol_percent, ucNonMonetary$denial_sep_other_percent, na.rm = TRUE)
# maxNonSepDenials <- max(ucNonMonetary$denial_non_aa_percent,ucNonMonetary$denial_non_income_percent, ucNonMonetary$denial_non_refusework_percent, ucNonMonetary$denial_non_reporting_percent, ucNonMonetary$denial_non_referrals_percent, ucNonMonetary$denial_non_other_percent, na.rm=TRUE)
# maxSepDenialRate <- max(ucNonMonetary$denial_sep_misconduct_rate, ucNonMonetary$denial_sep_vol_rate, ucNonMonetary$denial_sep_other_rate, na.rm=TRUE)
# maxNonSepDenialRate <- max(ucNonMonetary$denial_non_aa_rate,ucNonMonetary$denial_non_income_rate, ucNonMonetary$denial_non_refusework_rate, ucNonMonetary$denial_non_reporting_rate, ucNonMonetary$denial_non_referrals_rate, ucNonMonetary$denial_non_other_rate, na.rm=TRUE)

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

reportTheme <- theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5, size=20),
        legend.position="top",
        legend.title = element_blank(),
        axis.title = element_text(size=17, face="bold"),
        axis.text = element_text(size=17))


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

# a plot with ribbons rather than lines to represent values (like a stacked bar, but a stacked line)
getRibbonPlot <- function(df, xlab = "Date", ylab, caption, title, ...) {
  
  df %>% 
    ggplot(aes(x = rptdate, y = value, ymin = 0, ymax = value, fill = metric, group = desc(metric))) +
    geom_rect(inherit.aes = FALSE, data=recession_df, aes(xmin=start, xmax=end, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.3) +
    geom_ribbon(alpha=.9) +
    geom_line() +
    #scale_color_discrete(guide = FALSE) +
    #scale_fill_discrete(...)
    scale_fill_brewer(palette="Set1", ...) +
    reportTheme +
    labs(x = xlab,
        y = ylab,
        caption = caption,
        title = title)
}

# line plot with lines representing each metric passed in
getLinePlot <- function(df, xlab = "Date", ylab, caption, title, ...) {
  df %>% 
    ggplot(aes(x = rptdate, y = value, color = metric)) +
    geom_rect(inherit.aes = FALSE, data=recession_df, aes(xmin=start, xmax=end, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.3) +
    geom_line(size = 2) + 
    reportTheme +
    labs(x = xlab, y = ylab,
         caption = caption, 
         title= title) + 
    scale_y_continuous(labels=comma) +
    scale_color_brewer(palette="Set1", ...)
}

# point plot with smoothing
getPointPlot <- function(df, xlab = "Date", ylab, caption, title, ...) {
  df %>% 
    ggplot(aes(x = rptdate, y = value, color = metric)) +
    geom_rect(inherit.aes = FALSE, data=recession_df, aes(xmin=start, xmax=end, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.3) +
    geom_point() +
    stat_smooth(span=.3, se = F) + 
    reportTheme + 
    labs(x=xlab, y = ylab,
         caption = caption,
         title= title) + 
    scale_fill_brewer(palette="Set1", ...)
}


add_line_with_label <- function(plot, x, y, label) {
  plot + 
    geom_hline(yintercept = y, linetype = "dashed") +
    annotate("text", y = y, x = x, label = label, vjust = -.8, hjust=0)
    
}

# gets a wide table for printing, with the columns in a specific order
get_wide_UI_table <- function(df, col_list) {
  df %>% 
    filter(metric %in% col_list) %>% 
    pivot_wider(names_from = metric, values_from = value) %>% 
    select(st, rptdate, one_of(col_list))
}

# gets a DT::datatable for printing
get_UI_DT_datable <- function(df, col_list, names_list, class = "stripe") { 
  DT::datatable(get_wide_UI_table(df, col_list), 
                options = list(
                  pageLength = 12,
                  lengthMenu = list(c(12, 24, 48, -1),c("12", "24", "48", 'All')),
                  order = list(1,'desc'),
                  searching = FALSE
                ), 
                colnames= names_list,
                class = class,
                rownames = FALSE)
}


append_rowcallback <- function(dt, lim_a, lim_b) {
  
  dt$x$options <- append(uiDT$x$options, 
                           list(rowCallback = 
                                  DT::JS(paste('function (row,data) { if(parseFloat(data[2]) < ', 
                                               lim_a, 
                                               ') { $("td:eq(2)",row).css("color","red").css("font-weight", "bold"); } if(parseFloat(data[3]) <', 
                                               lim_b,  
                                               ') { $("td:eq(3)",row).css("color","red").css("font-weight", "bold"); }  }'))))
  
  return(dt)
}
                         
