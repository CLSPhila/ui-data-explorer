#' helper.R
#' A series of functions to help with the UI data explorer shiny app

library(sf)
library(leaflet)
library(ggplot2)
library(gghighlight)
library(lubridate)
library(geojsonio)

tmp <- tempdir()
unzip("cb_2015_us_state_20m.zip", exdir = tmp)

usa <- sf::st_read(tmp) %>% sf::st_transform(crs = 4326)
# a df of the recessions; used for graphing
recession_df <- data.frame(start = as.Date(c("1969-12-01", "1973-11-01", "1980-01-01",  
                                     "1981-07-01", "1990-07-01", "2001-03-01", "2007-12-01", "2020-02-01")),
                           end = as.Date(c("1970-11-01", "1975-03-01", "1980-07-01",
                                   "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01", NA))) %>% 
  replace(is.na(.), today())


# creates a choropleth map of ui data
getUIMap <- function(df, uiDate, metric_filter, stateText, reverseLevels, prefix = "", suffix = "", scale = 1, round_digits = 2) 
{
  # first filter the DF to just the metric we need
  df <- df %>% 
    filter(metric == metric_filter)
  
  #then try and figure out what month we are seaching for; the slider allows
  # values throughout the month, but our reports must be based on the last
  # day in the month
  uiDate <- ceiling_date(uiDate,"month") - days(1)
  
  # if the uiDate isn't in our rptdate vector, find the next highest value that is
  if(!(uiDate %in% df$rptdate)) {
    dates <- unique(df$rptdate)
    uiDate <- dates[which.min(abs(dates - uiDate))]
  }

  df <- df %>% 
    filter(rptdate == uiDate,
           metric == metric) %>% 
           select(st, rptdate, value)
  
  # flop the measure col if reverse is true so that we can do the shading correctly when high is good
  if(reverseLevels) { df$measure = 1/df$value 
  } else { df$measure = df$value }

  
  # join the shapefile and the state
  map <- usa %>%
    left_join(df, by=c("STUSPS"="st")) 
  
  labels = paste0(stateText,"<br><strong>", map$NAME, "</strong>: ", prefix, round(map$value * scale, round_digits), suffix) %>% 
    lapply(htmltools::HTML)
  
  # this creates the color mapping
  #pal <- colorBin("Reds", NULL, bins = 7)
  pal <- colorNumeric("Reds", domain = (map %>% filter(!is.infinite(measure)) %>% pull(measure)))
  

  
  # print out the map.  The problem here is that the palettes are white->red as you go from bad->good, which
  # means that we often will want to resort everything 
  uiMap <- leaflet(map) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~pal(measure), 
                fillOpacity = 0.8, 
                color = "#BDBDC3", 
                weight = 1, 
                label = labels) %>%
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
getSMPlot <- function(df, startDate, endDate, measure, yLabel, plotTitle, free_y, ...)
{ 
  
  scales = "fixed"
  if(free_y) scales = "free_y"
  
  # start by filtering the df to the right time period and to the right metric
  df <- df %>% 
    filter(rptdate > startDate, 
           rptdate < endDate, 
           metric == measure)
  
  # small multiple plot
  smPlot  <- df %>% 
    filter(!st %in% ("US")) %>% 
    ggplot(aes(x = rptdate, y = value)) +
    geom_line(size = 1.1, color = "gray29") +
    facet_wrap(vars(st), ncol = 5, scales = scales) +
    geom_line(data = df %>% 
                filter(st == "US") %>% 
                select(-st), 
              aes(x = rptdate, y = value), color="tomato3", linetype="dashed") +
    theme_minimal() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3), 
                       labels = label_comma(...)) +
    scale_x_date(breaks = scales::pretty_breaks(n = 4, min.n = 3)) +
    theme(plot.title = element_text(face="bold", hjust=.5, size=20),
          legend.position="top",
          legend.title = element_blank(),
          axis.title = element_text(size=15, face="bold"),
          axis.text = element_text(size=12),
          strip.text.x = element_text(size = 12, face="bold")
    ) +
    labs(x="Date", 
         y=yLabel, 
         title = plotTitle) 
  return(smPlot) 
}


# a function to generate a single state vs 50-state overview on one graphs
# measure - the measure to graph
get50StateComparisonPlot <- function(df, startDate, endDate, measure, highlightState, yLabel, plotTitle, ...)
{ 
  
  df <- df %>% 
    filter(rptdate > startDate, 
           rptdate < endDate, 
           metric == measure)
  
  plot <- df %>% 
    ggplot(aes(x = rptdate, y = value, color = st)) +
    geom_line(size = 2) +
    gghighlight(st == highlightState,
                unhighlighted_params = list(size = 1)) + 
    theme_minimal() +
    scale_y_continuous(labels = label_comma(...)) +
    #scale_x_date(breaks = scales::pretty_breaks) +
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
    ggplot(aes(x = rptdate, y = value, ymin = 0, ymax = value, fill = metric)) +
    geom_rect(inherit.aes = FALSE, data=recession_df, aes(xmin=start, xmax=end, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.3) +
    geom_col(alpha=.9, position = "stack", width = 31) +
    geom_line(position = "stack") +
    #scale_color_discrete(guide = FALSE) +
    #scale_fill_discrete(...)
    scale_fill_brewer(palette="Set1", ...) +
    #scale_color_brewer(palette="Set1", ...) +
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
    scale_fill_brewer(palette="Set1", ...) + 
    scale_color_brewer(palette="Set1", ...)
  
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
    select(st, rptdate, one_of(col_list)) %>%
    # remove rows that don't have complete data; this happens most
    # frequently for tables with data that is quarterly since some data in the DT
    # may also be produced monthly.  we only want the quarterly info
    na.omit()
}

# gets a DT::datatable for printing
get_UI_DT_datable <- function(df, col_list, names_list, class = "stripe", lim_a = NULL, lim_b = NULL) {
 
  # deal with callbacks when we want one to highlight rows that don't comply with a federal standard
  rowCallback <- NA
  if (!is.null(lim_a) & !is.null(lim_b)) {
    rowCallback <- DT::JS(paste('function (row,data) { if(parseFloat(data[2]) < ', 
                                lim_a, 
                                ') { $("td:eq(2)",row).css("color","red").css("font-weight", "bold"); } if(parseFloat(data[3]) <', 
                                lim_b,  
                                ') { $("td:eq(3)",row).css("color","red").css("font-weight", "bold"); }  }'))
  }
  
  DT::datatable(get_wide_UI_table(df, col_list), 
                options = list(
                  pageLength = 12,
                  lengthMenu = list(c(12, 24, 48, -1),c("12", "24", "48", 'All')),
                  order = list(1,'desc'),
                  searching = FALSE, 
                  rowCallback = rowCallback
                ), 
                colnames= names_list,
                class = class,
                rownames = FALSE)
}
