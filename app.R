#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# An explorer of UI data, which can be downlaoded from here: https://oui.doleta.gov/unemploy
# This data concerns the administration of the UI program in each state. 
# Specifically, this focuses on data regarding payment and decision timelapses, to ensure that
# states are holding up their end of the bargain in promptly paying claimants or at least adjudicating their 
# cases promptly
# Made by Michael Hollander of Community Legal Services, 1/2017

# shinyapps.io uses calls to 'library' to know what the environment for the
# app should be.
library(config)
library(V8)
library(jqr)
library(shiny)
library(lubridate)
library(DT)
library(ggplot2)
library(scales)
library(shinycssloaders)
library(tidyverse)
library(geojson)
library(gghighlight)
library(ggrepel)
#library(protolight)
library(geojsonio)
library(leaflet)
library(arrow)
source("helper.R")


# for testing
# input <- list(range = as.Date(c("2008-01-01", "2020-05-30")), state = "PA")

# read in the df of data that we need
stored_data_location <- file.path(get("DATA_DIR"), "unemployment_data.parquet")
unemployed_df <- arrow::read_parquet(stored_data_location)


maxDate <- max(unemployed_df$rptdate)
minDate <- min(unemployed_df$rptdate)
states <- sort(unique(unemployed_df$st))
pua_earliest <- ymd("2020-01-31")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Unemployment Insurance Data Explorer"),
  
  # Sidebar with a file input 
  sidebarLayout(
    sidebarPanel(
      # the input that allows a state to be chosen
      selectInput("state", 
                  label = "Choose a state",
                  choices = states,
                  selected = "PA"),
    
      shinyWidgets::sliderTextInput(
        inputId    = "range",
        label      = "Years to View:",
        choices    = zoo::as.yearmon(seq.Date(floor_date(minDate, "month"), floor_date(maxDate, "month"), by = "month")),
        selected   = zoo::as.yearmon(c(maxDate - years(10), maxDate)),
        hide_min_max = TRUE,
        #grid       = TRUE,
        width      = "100%"
      ),
    
      # the list of data to view; shoudl rethink how to easily define new data sets rather than
      # having to handcode each dataset here and in 10 other places
      selectInput("viewData",
                  label = 'Select Data to View',
                  size=33, selectize=FALSE,
                  choices = c("Basic UI Data" = "basicUI_claims",
                              "--Weeks Claims/Compensated" = "basicUI_compensated",
                              "--Weekly Initial and Continued Claims" = "basicWeeklyClaims",
                              "--Monthly UI Payments" = "monthlyUI", 
                              "--Init. Payments / Claims" = "basicUI_payment_rate",
                              "--Workshare Initial Claims" = "basicUI_workshare_initial",
                              "--Workshare Continued Claims" = "basicUI_workshare_continued",
                              "--Unemployment Rate (SA)" = "uirate",
                              "Demographics: Race" = "demographics_race",
                              "--Demographics: Ethnicity" = "demographics_ethnicity",
                              "--Demographics: Sex" = "demographics_sex",
                              "--Demographics: Age" = "demographics_age",
                              "Recipiency Rate" = "recipRate",
                              "--Recipiency Rate Breakdown" = "recipBreakdown",
                              "Pandemic Unemployment Assistance" = "puaData",
                              "--PUA Basic Claims Data" = "puaClaims", 
                              "--PUC $600 Payments" = "pucClaims",
                              "Timeliness of First Payments" = "firstPay", 
                              "Timeliness of Lower Authority Decisions" = "lowerAuthority", 
                              "Timeliness of Higher Authority Decisions" = "higherAuthority", 
                              "Overpayment vs Recovery" = "overvRecovery",
                              "--Overpayment Balance/Annual UI Payments" = "overvPayments",
                              "--Fraud vs Non Fraud Overpayents" = "fraudvNon", 
                              "--Tax Program Overpayment Recoveries" = "TOPS",
                              "Non-Monetary Denials" = "nonMonDen",
                              "--Separation Denial Breakdown" = "nonMonSep",
                              "--Separation Denial Rates" = "nonMonSepRate",
                              "--Non-Separation Denial Breakdown" = "nonMonNonSep",
                              "--Non-Separation Denial Rates" = "nonMonNonSepRate",
                              "Monetary Determinations" = "monetaryDeterminations",
                              "--Percent Receiving Max Weekly Benefit" = "monetaryDeterminations_max_weekly",
                              "--Average Weeks Duration" = "monetaryDeterminations_average_weeks"),

                  # the default selected is the recipiency Rate, but this coudl be anythign
                  selected = "basicUI_claims"),
      
      # allows for a constant y axis on the main charts, which allows state to state
      # comparisons to be made.  
      checkboxInput("constant_y_axis", 
                    label="Constant y axis? (makes comparisons easier)",
                    value=FALSE),
      
      tags$img(src="https://clsphila.org/wp-content/uploads/2019/02/CLS-Logo_120.png")
      
      # the width of the sidebar panel
      , width=3),
    
    # the main panel
    mainPanel(

      # specify the different panels and layouts
      tabsetPanel(
        # the main panel; has a plot, data, and a download data button
        tabPanel("Data",
          withSpinner(plotOutput("uiplot")),
          downloadButton('downloader', 'Download Data'),
          dataTableOutput("uidata")
        ),
        # the 50-state on one plot tab
        tabPanel("50-State Overlay (one plot)", withSpinner(plotOutput("fiftyStatePlot"))),
        
        # the 50-state on different plots tab
        tabPanel("50-State Comparison (many plots)", withSpinner(plotOutput("smplot", height="900px"))),
        
        # chloropleth map
        tabPanel("Map",withSpinner(leafletOutput("uimap"))),
        
        # the about page, which may need to be rewritten
        tabPanel("About/Terms", br(), 
                 p("This page was created by ", a(href="https://www.clsphila.org" ,"Community Legal Services"), " to visualize the unemployment data made avaialble by the US Department of Labor and Bureau of Labor Statistics."),
                 p("The DOL Data can be found here: https://oui.doleta.gov/unemploy/DataDownloads.asp and the BLS data can be found ", a(href="https://www.bls.gov/web/laus/ststdsadata.txt", "here"), "and ", a(href="https://www.bls.gov/web/laus/ststdnsadata.txt", "here.")),
                 p("If you have any suggestions for any further measures to put on the page, please email ", a(href="mailto:mhollander@clsphila.org", "Michael Hollander"), "(mhollander@clsphila.org, the creator and maintainer of this page."),
                 p("You can find the code for this page on github here: ", a(href='https://github.com/CLSPhila/ui-data-explorer', target="_blank", "https://github.com/CLSPhila/ui-data-explorer"), "."),
                 p("This product uses the FRED® API but is not endorsed or certified by the Federal Reserve Bank of St. Louis."),
                 p("By using this application, you are also bound by FRED® API ", a(href="https://research.stlouisfed.org/docs/api/terms_of_use.html", target="_blank", "terms of use."))
        )
      )
    )
  ),
  
  # a row at the bottom with links, etc...
  fluidRow(
    hr(),
    HTML("This page is maintained by <a href=mailto:hollander@gmail.com>Michael Hollander</a>, formerly of <a href='clsphila.org' target=_blank>Community Legal Services</a>.  You can find the code for this page on github here:<a href='https://github.com/CLSPhila/ui-data-explorer' target=_blank>Github</a>.  All of the data for this website comes from <a href=https://oui.doleta.gov/unemploy/DataDownloads.asp target=_blank>the US Department of Labor</a> and U.S. Bureau of Labor Statistics, through <a href='https://fred.stlouisfed.org/'>FRED/the Federal Reserve Bank of St. Louis.</a>")
  )
)



# Define server logic required to draw page
server <- function(input, output) {

  
  makeReactiveBinding("date_filter_start")
  makeReactiveBinding("date_filter_end")
  
  
  toListen <- reactive({list(input$range, input$viewData)})
  
  observeEvent(toListen(), { 
    date_filter_start <<- get_last_day_of_month_from_range(input$range[1])
    date_filter_end <<- get_last_day_of_month_from_range(input$range[2])
    
    # set the start and end date to no earlier than the 1/31/2020 if we selected PUA
    if(input$viewData %in% c("puaData", "puaClaims", "pucClaims")) {
      if(date_filter_start < pua_earliest) date_filter_start <<- pua_earliest
      if(date_filter_end < pua_earliest) date_filter_end <<- date_filter_start + months(4)
    } 
    
  })

  # render the plot
  output$uiplot <- renderPlot({

    # get start and end date filters
    df <- unemployed_df %>%
      filter(st == input$state,
             rptdate >= date_filter_start,
             rptdate <= date_filter_end)
    
    if (input$viewData == "monthlyUI")
    {
      
      metric_filter = c("total_federal_compensated_mov_avg", "total_state_compensated_mov_avg")
      df <- df %>% 
        filter(metric %in% metric_filter) %>% 
        mutate(metric = factor(metric, labels = metric_filter, ordered = TRUE))
      
      
      uPlot <- getRibbonPlot(df, xlab = "Date", ylab = "Total Paid",
                      caption = "12-month moving average of UI paid per month in both regular and federal UI programs.\nNote that 'regular UI' includes state UI, UFCE, and UCX.  Federal programs include EB, and the various EUC programs that have been enacted.",  
                      title = glue::glue("{input$state} Monthly UI Payments from {format.Date(date_filter_start, 'm-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                      breaks=c("total_state_compensated_mov_avg","total_federal_compensated_mov_avg"),
                      labels=c("Regular Programs","Federal Programs")) +
        scale_y_continuous(labels = label_number(scale = 1/1000000, prefix = "$", suffix = "M"))
      
      # adjustment for finding the max height of the graph
      metric_filter = c("total_compensated_mov_avg")
      
      
    }

    else if (input$viewData == "basicWeeklyClaims")
    {
      
      metric_filter = c("weekly_initial_claims", "weekly_continued_claims")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 539, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Weekly Initial and Continued Claims Claims from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks=  metric_filter,
                            labels=c("Initial Claims", "Continued Claims")) + 
        scale_y_continuous(labels = comma)
      
      
      # adjustment for finding the max height of the graph
      metric_filter = c("weekly_continued_claims")
    }
    
    else if (input$viewData == "basicUI_claims")
    {
      
      metric_filter = c("monthly_initial_claims", "monthly_first_payments", "monthly_exhaustion" )
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 5129, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Monthly State Claims, Payments and Exhaustion Eligibility from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks=  c("monthly_initial_claims", "monthly_first_payments", "monthly_exhaustion" ),
                            labels=c("Initial Claims", "First Payments", "Exhaustion")) + 
        scale_y_continuous(labels = comma)
      
      
      # adjustment for finding the max height of the graph
      metric_filter = c("monthly_initial_claims")
    }
    
    else if (input$viewData == "basicUI_compensated")
    {
      
      metric_filter = c("monthly_weeks_compensated", "monthly_partial_weeks_compensated")
      df <- df %>% 
        filter(metric %in% metric_filter)  
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 5129, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Monthly Compensation (state), Weeks Compensated from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks= metric_filter,
                            labels=c("Weeks Compensated", "Partial Weeks Compensated")) + 
        scale_y_continuous(labels = comma)
      
      
      # adjustment for finding the max height of the graph
      metric_filter = c("monthly_weeks_compensated")
    }
    
    else if (input$viewData == "basicUI_payment_rate")
    {
      
      metric_filter = c("monthly_first_payments_as_prop_claims")
      df <- df %>% 
        filter(metric %in% metric_filter)  
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 5129, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} First Payments as a Proportion of Initial Claims from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks= metric_filter,
                            labels=c("Monthly Payments as a Proportion of Initial Claims")) + 
        scale_y_continuous(labels = scales::percent)
      
      
      # adjustment for finding the max height of the graph
      metric_filter = c("monthly_first_payments_as_prop_claims")
    }
    
    
    else if (input$viewData == "basicUI_workshare_initial")
    {
      
      metric_filter = c("workshare_initial_claims")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA aw5159, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Monthly Workshare Initial Claims from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks=  metric_filter,
                            labels=c("Initial Claims")) + 
        scale_y_continuous(labels = comma)
      
    }
    
    else if (input$viewData == "basicUI_workshare_continued")
    {
      
      metric_filter = c("workshare_continued_claims")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA aw5159, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Monthly Workshare Continued Claims from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks=  metric_filter,
                            labels=c("Continued Claims")) + 
        scale_y_continuous(labels = comma)
      
    }
    
    else if (input$viewData == "demographics_race")
    {
      
      metric_filter = c("demographic_race_prop_native_american_alaskan", "demographic_race_prop_asian_pacific_islander", "demographic_race_prop_unk", 
                        "demographic_race_prop_black", "demographic_race_prop_white")
      df <- df %>% 
        filter(metric %in% metric_filter) %>% 
        mutate(metric = factor(metric, levels = metric_filter))
      
      
      uPlot <- get_area_chart(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 203, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Racial Composition of UI Recipients from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks= metric_filter,
                            labels=c("% Native American/Native Alaskan", "% Asian/Pacific Islander", "% Unknown", "% Black", "% White")) + 
        scale_y_continuous(labels = scales::percent,limits = c(NA, 1))
      
    }

    else if (input$viewData == "demographics_sex")
    {
      
      metric_filter = c("demographic_sex_prop_men", "demographic_sex_prop_women")
      df <- df %>% 
        filter(metric %in% metric_filter) %>% 
        mutate(metric = factor(metric, levels = metric_filter))
      
      
      uPlot <- get_area_chart(df, xlab = "Date", ylab = "",
                              caption = "Data courtesy of the USDOL.  Report used is ETA 203, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                              title = glue::glue("{input$state} Sex Composition of UI Recipients from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                              breaks= metric_filter,
                              labels=c("% Women", "% Men")) + 
        scale_y_continuous(labels = scales::percent,limits = c(NA, 1))
      
      
  
    }
    
    else if (input$viewData == "demographics_ethnicity")
    {
      
      metric_filter = c("demographic_eth_prop_latinx", "demographic_eth_prop_non_latinx")
      df <- df %>% 
        filter(metric %in% metric_filter) %>% 
        mutate(metric = factor(metric, levels = metric_filter))
      
      
      uPlot <- get_area_chart(df, xlab = "Date", ylab = "",
                              caption = "Data courtesy of the USDOL.  Report used is ETA 203, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                              title = glue::glue("{input$state} Latinx Composition of UI Recipients from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                              breaks= metric_filter,
                              labels=c("% Latinx", "% Non-Latinx")) + 
        scale_y_continuous(labels = scales::percent,limits = c(NA, 1))
      
      
    }
    
    else if (input$viewData == "demographics_age")
    {
      
      metric_filter = c("demographic_age_prop_unk", "demographic_age_prop_55_and_older", "demographic_age_prop_45_54", "demographic_age_prop_35_44", 
                        "demographic_age_prop_25_34", "demographic_age_prop_under25")
      df <- df %>% 
        filter(metric %in% metric_filter) %>% 
        mutate(metric = factor(metric, levels = metric_filter))
      
      
      uPlot <- get_area_chart(df, xlab = "Date", ylab = "",
                              caption = "Data courtesy of the USDOL.  Report used is ETA 203, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                              title = glue::glue("{input$state} Age Composition of UI Recipients from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                              breaks= metric_filter,
                              labels=c("% Unknown", "% 55+", "% 45-54", "% 35-44", "% 25-34", "% Under 25")) + 
        scale_y_continuous(labels = scales::percent,limits = c(NA, 1))
      
    }
    
    else if (input$viewData == "puaData")
    {
      
      metric_filter = c("pua_percent_eligible", "pua_percent_eligible_self_employed", "pua_percent_applicants_self_employed" )
      df <- df %>% 
        filter(metric %in% metric_filter)  
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 902p, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} PUA Eligibility: General and Self Employed from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks= metric_filter,
                            labels=c("Overall Eligibility Rate", "Self-Employed Eligibility Rate", "% Self-Employed Applicants")) + 
        scale_y_continuous(labels = scales::percent)
      
      # adjustment for finding the max height of the graph
      metric_filter = c("pua_percent_eligible")
    }

    else if (input$viewData == "pucClaims")
    {
      
      metric_filter = c("puc_weeks")
      df <- df %>% 
        filter(metric %in% metric_filter) 
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "Weeks Compensated (Thousands)",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 2112, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} PUC Payments: Weeks and Total Amount Compensated from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks= metric_filter,
                            labels=c("PUC $600 Payment")) + 
        scale_y_continuous(labels = label_number(scale = 1/1000, prefix = "", suffix = "K", big.mark = ","),
                           sec.axis = sec_axis(trans = ~.*600,
                                               labels = label_number(scale = 1/1000000, prefix = "$", suffix = "M", big.mark = ","),
                                               name = "Total Disbursed (Millions)"))

      # adjustment for finding the max height of the graph
      metric_filter = c("pua_percent_eligible")
    } 
    
    else if (input$viewData == "puaClaims")
    {
      
      metric_filter = c("pua_initial_applications_total", "pua_eligible_total", "pua_first_payments", "pua_weeks_compensated")
      df <- df %>% 
        filter(metric %in% metric_filter) %>% 
        mutate(metric = factor(metric, labels = metric_filter, ordered = TRUE))
      
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 902p, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} PUA Claims and Payments from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks= metric_filter,
                            labels=c("Initial Applications", "Total Eligible", "First Payments", "Weeks Compensated")) +
        scale_y_continuous(labels = comma)
      
    }
    
    else if (input$viewData == "recipBreakdown")
    {
      metric_filter = c("total_week_mov_avg","unemployed_avg") 
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getLinePlot(df, xlab = "Date", ylab = "", 
                           caption = "Weekly continued claims and Total Unemployed by month.\nBoth numbers are smoothed over 12 month periods.  These are the two components of recipiency rate.",
                           title = glue::glue("{input$state} Recipiency Rate Breakdown from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                           breaks=c("total_week_mov_avg","unemployed_avg"),
                           labels=c("Weekly Continued Claims","Monthy Unemployed (BLS)"))
      
    }
    
    else if (input$viewData == "uirate")
    {  
      
      metric_filter = c("unemployment_rate_sa")
      df <- df %>% 
        filter(metric %in% metric_filter)
      df_us <- unemployed_df %>% 
        filter(st == "US",
               rptdate >= date_filter_start,
               rptdate <= date_filter_end,
               metric %in% metric_filter)

        uPlot <- getLinePlot(df, xlab = "Date", ylab = "",
                             caption = "Seasonally adjusted unemployed rate, based on BLS monthly report found here: https://www.bls.gov/web/laus/ststdsadata.txt.",
                             title = glue::glue("{input$state} Unemployment Rate (SA) from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                             breaks=c("perc_unemployed"),
                             labels=c("Seasonally adjusted unemployment rate")) +
          # add the us average line and label
          geom_line(data = df_us, size = 1, linetype = "dashed", color = "black") +
          annotate("text", x = min(df_us$rptdate), y = df_us %>% filter(rptdate == min(rptdate)) %>% pull(value), 
                   label = "US Avg", vjust = -1, hjust = 0, fontface = "bold")
    }
    
    else if (input$viewData == "overvPayments")
    {
      
      metric_filter = c("outstanding_proportion")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                     caption = "Outstanding overpayment balance divided by the total benefits paid in all federal and state programs over the last 12 months.\n Data courtesy of the USDOL.  Reports used are ETA 227 and 5159, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                     title = glue::glue("{input$state} Overpayment Balance vs Montly UI Payments from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                     breaks=c("outstanding_proportion"),
                     labels=c("Overpayment Balance / Annual UI Payments"))
    }

    else if (input$viewData == "fraudvNon")
    {
      
      metric_filter = c("fraud_num_percent")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 227, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Fraud Overpayments as a Percent of All Overpayments from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks=c("fraud_num_percent"),
                            labels=c("% Fraud Overpayments")) + 
        scale_y_continuous(labels = scales::percent)
      
    }
    
    else if (input$viewData == "TOPS")
    {
      
      metric_filter = c("state_tax_recovery", "federal_tax_recovery")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "Total Overpayment Recovery",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 227, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Tax Program Overpayment Recovery from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks=c("state_tax_recovery", "federal_tax_recovery"),
                            labels=c("State Tax Recover", "Federal Tax Recovery")) + 
        scale_y_continuous(labels = label_number(scale = 1/1000000, prefix = "$", suffix = "M"))
        
      
    }
    
    
    ## mgh: I can't figure out how to reverse the orders of geoms here.  shit.
    else if (input$viewData == "recipRate")
    {
      metric_filter = c("recipiency_annual_fed", "recipiency_annual_reg")
      df <- df %>% 
        filter(metric %in% metric_filter) %>% 
        mutate(metric = factor(metric, labels = metric_filter, ordered = TRUE))
      metric_filter = c("recipiency_annual_total")
      
      uPlot <- getRibbonPlot(df, xlab = "Date", ylab = "Recipiency Rate",
                             caption = "Recipiency rate calculated by dividing 12 month moving average of unemployment continuing claims divided by 12 month moving average of total unemployed.\nData not seasonally adjusted.  \nSource: Continuing claims can be found in ETA report 5159, found here: https://oui.doleta.gov/unemploy/DataDownloads.asp.\nUnemployed numbers courtesy the BLS: https://www.bls.gov/web/laus/ststdnsadata.txt.  \nNote that 'regular UI' includes state UI, UFCE, and UCX.  Federal programs include EB, and the various EUC programs that have been enacted.",  
                             title = glue::glue("{input$state} Recipiency Rate from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                             breaks=c("recipiency_annual_reg","recipiency_annual_fed"),
                             labels=c("Regular Programs", "Federal Programs"))
       
      
    }
    
    else if (input$viewData == "overvRecovery")
    {
      metric_filter = c("outstanding", "recovered")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 227, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Outstanding State Overpayments vs State $ Recovered from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks=c("outstanding", "recovered"),
                            labels=c("Outstanding Overpayments", "Overpayments Recovered")) + 
        scale_y_continuous(labels = label_number(scale = 1/1000000, prefix = "$", suffix = "M"))
      
      
    }
    
    # Non-Monetary denials.  Show a graph of separation denial % and non-separation denial %
    else if (input$viewData == "nonMonDen")
    {
      
      metric_filter = c("denial_sep_percent", "denial_non_percent", "denial_rate_overall")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "Proportion of non-monetary determinations",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 207, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Proportion of Denials for Separation and Non-Separation Reasons\n in Non-Monetary Decisions {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks=c("denial_sep_percent", "denial_non_percent", "denial_rate_overall"),
                            labels=c("Separation Denials", "Non-Separation Denials", "Total Denial Rate"))
  
    }

    else if (input$viewData == "nonMonSep")
    {
      
      metric_filter = c("denial_sep_misconduct_percent","denial_sep_vol_percent", "denial_sep_other_percent")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "Proportion of separation denials",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 207, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Proportion of Non-Monetary Separation Denials from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks=c("denial_sep_misconduct_percent","denial_sep_vol_percent", "denial_sep_other_percent"),
                            labels=c("Misconduct", "Voluntary Quit", "Other"))
    
    }

    else if (input$viewData == "nonMonSepRate")
    {
      
      metric_filter = c("denial_sep_misconduct_rate","denial_sep_vol_rate", "denial_sep_other_rate")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "Proportion of separation denials",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 207, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Non-Monetary Separation Denial Rate from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks=c("denial_sep_misconduct_rate","denial_sep_vol_rate", "denial_sep_other_rate"),
                            labels=c("Misconduct", "Voluntary Quit", "Other")) + 
        scale_y_continuous(labels = scales::percent)
    
    }
    
    else if (input$viewData == "nonMonNonSep")
    {
      
      metric_filter = c("denial_non_aa_percent","denial_non_income_percent", "denial_non_refusework_percent", "denial_non_reporting_percent", "denial_non_referrals_percent", "denial_non_other_percent")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "Proportion of non-separation denials",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 207, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Proportion of Non-Monetary Non-Separation Denials from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks=c("denial_non_aa_percent","denial_non_income_percent", "denial_non_refusework_percent", "denial_non_reporting_percent", "denial_non_referrals_percent", "denial_non_other_percent"),
                            labels=c("Able and Available", "Disqualifying Income", "Refusal of Suitable Work", "Reporting/Call Ins/Etc...", "Refusal of Referral", "Other"))
    
    }

    else if (input$viewData == "nonMonNonSepRate")
    {
      
      metric_filter = c("denial_non_aa_rate","denial_non_income_rate", "denial_non_refusework_rate", "denial_non_reporting_rate", "denial_non_referrals_rate", "denial_non_other_rate")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "Proportion of non-separation denials",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 207, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Denial Rates for Non-Monetary Non-Separation Denials from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks=c("denial_non_aa_rate","denial_non_income_rate", "denial_non_refusework_rate", "denial_non_reporting_rate", "denial_non_referrals_rate", "denial_non_other_rate"),
                            labels=c("Able and Available", "Disqualifying Income", "Refusal of Suitable Work", "Reporting/Call Ins/Etc...", "Refusal of Referral", "Other")) + 
        scale_y_continuous(labels = scales::percent)
      
    
    }
    
    else if (input$viewData == "monetaryDeterminations")
    {
      
      metric_filter = c("monetaryDet_mon_eligible_prop")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA ar218, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Monetary Eligibility Rate from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks= metric_filter,
                            labels= c("Proportion Monetarily Eligible")) +
        scale_y_continuous(labels = scales::percent)
    }
    
    else if (input$viewData == "monetaryDeterminations_max_weekly")
    {
      
      metric_filter = c("monetaryDet_max_benefit_prop")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA ar218, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Maximum Weekly Benefit Eligibility Rate from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks= metric_filter,
                            labels= c("Proportion Eligible for Max Weekly Benefit")) +
        scale_y_continuous(labels = scales::percent)
    
      
    } else if (input$viewData == "monetaryDeterminations_average_weeks") {

    
      metric_filter = c("monetaryDet_avg_weeks_duration")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "Weeks",
                            caption = "Data courtesy of the USDOL.  Report used is ETA ar218, found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Average Weeks Duration of Benefit Received from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}"),
                            breaks= metric_filter,
                            labels= c("Average Weeks Duration"))
  
    
    } else if (input$viewData == "lowerAuthority") {
      
      metric_filter = c("lower_Within30Days", "lower_Within45Days")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Reports used are ETA 5130, 9050, 9054, and 9055.  \nAll can be found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Lower Authority Decision Timeliness from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}")) %>% 
        add_line_with_label(x = min(df$rptdate), y = .6, label = "30-day threshold") %>% 
        add_line_with_label(x = min(df$rptdate), y = .8, label = "45-day threshold") 

      
    } else if(input$viewData == "firstPay") {
      metric_filter = c("first_time_payment_Within15Days", "first_time_payment_Within35Days")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Reports used are ETA 5130, 9050, 9054, and 9055.  \nAll can be found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} First Payment Timeliness from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}")) %>% 
        add_line_with_label(x = min(df$rptdate), y = .87, label = "15-day threshold") %>% 
        add_line_with_label(x = min(df$rptdate), y = .93, label = "35-day threshold") 
      
      
    } else if(input$viewData == "higherAuthority") {
      
      metric_filter = c("higher_Within45Days", "higher_Within75Days")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Reports used are ETA 5130, 9050, 9054, and 9055.  \nAll can be found at https://oui.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Higher Authority Decision Timeliness from {format.Date(date_filter_start, '%m-%Y')} to {format.Date(date_filter_end, '%m-%Y')}")) %>% 
        add_line_with_label(x = min(df$rptdate), y = .4, label = "45-day threshold") %>% 
        add_line_with_label(x = min(df$rptdate), y = .8, label = "75-day threshold") 
      
    }


    
    # constant scaling on the y axis for easier state comparison; scaling on x axis so we can scale to ranges with no recession
    if (input$constant_y_axis)
    {
      
      # this represents the highest y value of all of the states for this metric
      ymax = max(unemployed_df %>% 
        filter(metric %in% metric_filter,
               rptdate > (date_filter_start-10),
               rptdate < (date_filter_end+10)) %>% 
        select(value))
      
      uPlot <- uPlot + coord_cartesian(ylim=c(0, ymax),
                                       xlim=c(as.Date(date_filter_start), as.Date(date_filter_end)))
    }
    else
    {
      uPlot <- uPlot + coord_cartesian(xlim=c(as.Date(date_filter_start),as.Date(date_filter_end)))
    }
    
    return(uPlot)
  })
  
  # render the data table
  output$uidata <- renderDataTable({
    
    df <- unemployed_df %>% 
      filter(st == input$state,
             rptdate >= date_filter_start,
             rptdate <= date_filter_end)
    
    
    if (input$viewData == "monthlyUI")
    {
      
      col_list = c("total_state_compensated_mov_avg", "total_federal_compensated_mov_avg", "total_state_compensated", "total_federal_compensated", "total_paid_annual_mov_avg")
      names_list <- c("State","Report Date", "State UI Payments (Monthly, mov avg)", "Federal UI Payments (Monthly, mov avg)", "State UI Payments (Monthly)", "Federal UI Payments (Monthly)", "Annual UI Payments (mov avg)")
      uiDT <- get_UI_DT_datable(df, col_list, names_list) %>% 
        formatCurrency(3:7, '$')
      
    }

    if (input$viewData %in% c("basicUI_claims", "basicUI_compensated", "basicUI_payment_rate", "basicUI_workshare_continued", "basicUI_workshare_initial"))
    {
      
      col_list = c("monthly_initial_claims", "monthly_first_payments", "monthly_weeks_compensated", "monthly_partial_weeks_compensated", "monthly_weeks_claimed", 
                   "monthly_exhaustion", "monthly_first_payments_as_prop_claims", "workshare_initial_claims", "workshare_continued_claims", "workshare_weeks_compensated", "workshare_first_payments") 
      names_list <- c("State","Report Date", "Initial Claims", "First Payments", "Weeks Compensated", "Partial Weeks Compensated", "Weeks Claimed",
                      "Number Exhausted", "Initial Payments / Initial Claims", "Workshare Initial Claims", "Workshare Continued Claims", "Workshare Weeks Compensated", "Workshare First Payments")
      uiDT <- get_UI_DT_datable(df, col_list, names_list) %>% 
        formatRound(columns = c(3:8, 10:13), digits = 0) %>% 
        formatRound(columns=c(9), digits=2)
    }
    
    if (input$viewData == "basicWeeklyClaims")
    {
      col_list = c("weekly_initial_claims", "weekly_continued_claims") 
      names_list <- c("State","Report Date", "Initial Claims (Weekly)", "Continued Claims (Weekly)")
      uiDT <- get_UI_DT_datable(df, col_list, names_list) %>% 
        formatRound(columns = c(3:4), digits = 0)
    }

    if (input$viewData == "demographics_race")
    {
      
      col_list = c("demographic_race_black", "demographic_race_white", "demographic_race_asian_pacific_islander", "demographic_race_native_american_alaskan", "demographic_race_unk",
                   "demographic_race_prop_black", "demographic_race_prop_white", "demographic_race_prop_asian_pacific_islander", "demographic_race_prop_native_american_alaskan", "demographic_race_prop_unk") 
      names_list <- c("State","Report Date", "Black", "White", "Asian/Pacific Islander", "Native American / Native Alaskan", "Unknown", 
                      "% Black", "% White", "% Asian/Pacific Islander", "%Native American / Native Alaskan", "% Unknown")
      uiDT <- get_UI_DT_datable(df, col_list, names_list) %>% 
        formatRound(columns = c(3:7), digits = 0) %>% 
        formatPercentage(columns = c(8:12))
    }
    
    
    if (input$viewData == "demographics_ethnicity")
    {
      
      col_list = c("demographic_eth_latinx", "demographic_eth_non_latinx", "demographic_eth_prop_latinx", "demographic_eth_prop_non_latinx") 
      names_list <- c("State","Report Date", "Latinx", "Non Latinx", "% Latinx", "% Non-Latinx")
      uiDT <- get_UI_DT_datable(df, col_list, names_list) %>% 
        formatRound(columns = c(3:4), digits = 0) %>% 
        formatPercentage(columns = c(5:6))
    }
    
    
    if (input$viewData == "demographics_sex")
    {
      
      col_list = c("demographic_sex_men", "demographic_sex_women", "demographic_sex_prop_men", "demographic_sex_prop_women") 
      names_list <- c("State","Report Date", "Men", "Women", "% Men", "% Women")
      uiDT <- get_UI_DT_datable(df, col_list, names_list) %>% 
        formatRound(columns = c(3:4), digits = 0) %>% 
        formatPercentage(columns = c(5:6))
    }
    
    
    if (input$viewData == "demographics_age")
    {
      
      col_list = c("demographic_age_under25", "demographic_age_25_34", "demographic_age_35_44", "demographic_age_45_54", "demographic_age_55_and_older", "demographic_age_unk",
                   "demographic_age_prop_under25", "demographic_age_prop_25_34", "demographic_age_prop_35_44", "demographic_age_prop_45_54", "demographic_age_prop_55_and_older", "demographic_age_prop_unk") 
      names_list <- c("State","Report Date", "Under 25", "25-34", "35-44", "45-54", "55+", "Unknown", "% Under 25", "% 25-34", "% 35-44", "% 45-54", "% 55+", "% Unknown")
      uiDT <- get_UI_DT_datable(df, col_list, names_list) %>% 
        formatRound(columns = c(3:8), digits = 0) %>% 
        formatPercentage(columns = c(9:14))
    }
    
    if (input$viewData == "puaData")
    {
      
      col_list = c("pua_initial_applications_total", "pua_initial_applications_self", "pua_eligible_total",  "pua_eligible_self" , "pua_percent_eligible", "pua_percent_eligible_self_employed", "pua_percent_applicants_self_employed")
      names_list <- c("State","Report Date", "PUA Initial Applications", "PUA Initial Applications (Self-Employed)", "PUA Eligible", "PUA Eligible (Self-Employed)", "PUA Eligibility Rate (Overall)", "PUA Eligibility Rate (Self-Employed)", "PUA Percent Self-Employed Applicants")
      uiDT <- get_UI_DT_datable(df, col_list, names_list) %>% 
        formatRound(columns=c(7:9), digits=2)
    }

    if (input$viewData == "puaClaims")
    {
      
      col_list = c("pua_initial_applications_total", "pua_eligible_total", "pua_first_payments", "pua_weeks_compensated")
      names_list <- c("State","Report Date", "PUA Initial Applications", "PUA Eligible", "PUA First Payments", "PUA Weeks Compensated")
      uiDT <- get_UI_DT_datable(df, col_list, names_list)
    }
    
    if (input$viewData == "pucClaims")
    {
      
      col_list = c("puc_payments_total", "puc_weeks")
      names_list <- c("State","Report Date", "PUC Total Disbursed", "PUC Weeks Compensated")
      uiDT <- get_UI_DT_datable(df, col_list, names_list) %>% 
        formatCurrency(3, '$') %>% 
        formatRound(columns=4, digits = 0)
      
    }
    
    else if (input$viewData == "overvPayments")
    {
      
      col_list <- c("outstanding_proportion", "outstanding", "total_paid_annual_mov_avg")
      names_list <- c("State","Report Date", "Outstanding balance / Annual UI Payments", "Outstanding Overpayment Balance", "Annual UI Payments")
      uiDT <- get_UI_DT_datable(df, col_list, names_list) %>% 
        formatCurrency(4:5, '$')
    }
    
    else if (input$viewData == "fraudvNon")
    {
      
      col_list <- c("fraud_num_percent", "regular_fraud_num", "federal_fraud_num", "regular_nonfraud_num", "federal_nonfraud_num")
      names_list <- c("State","Report Date", "Fraud as % of Total Overpayments", "Regular UI Fraud", "Federal UI Fraud", "Regular UI Non-Fraud", "Federal UI Non-Fraud")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class = "nowrap stripe") %>% 
        formatRound(columns = c(4:7), digits = 0)
      
      
      
    }

    else if (input$viewData == "TOPS")
    {
      col_list <- c("state_tax_recovery", "federal_tax_recovery")
      names_list <- c("State","Report Date", "State Tax Recovery", "Federal Tax Recovery")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class = "nowrap stripe") %>% 
        formatCurrency(3:4, '$')
    }
    
    else if (input$viewData == "overvRecovery")
    {
      col_list <- c("outstanding", "recovered", "outstanding_fed_programs", "recovered_fed_programs")
      names_list <- c("State","Report Date", "Outstanding Owed", "Recovered", "Outstanding Owed (Fed Programs)", "Recovered (Fed Programs)")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class = "nowrap stripe") %>% 
        formatCurrency(3:6, '$')
      
    }

    else if (input$viewData == "nonMonDen")
    { 
      col_list <- c("determ_total", "denial_sep_total", "denial_non_total", "denial_sep_percent", "denial_non_percent", "denial_rate_overall")
      names_list <- c("State","Report Date", "Total Non-Mon Determinations", "Separation Denials", "Non-Separation Denials", "Separation Denial Rate", "Non-Separation Denial Rate", "Overall Denial Rate")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class = "nowrap stripe") %>% 
        formatRound(columns = c(3:5), digits = 0)
      
    }
    
    else if (input$viewData == "nonMonSep")
    {
      col_list <- c("determ_total", "denial_sep_total", "denial_sep_misconduct_percent","denial_sep_vol_percent", "denial_sep_other_percent")
      names_list <- c("State","Report Date", "Total Non-Mon Determinations", "Separation Denials", "Misconduct %", "Voluntary Quit %", "Other %")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class = "nowrap stripe") %>% 
        formatRound(columns = c(3:4), digits = 0)
      
    }

    else if (input$viewData == "nonMonSepRate")
    {
      
      col_list <- c("determ_total", "denial_sep_total", "denial_sep_misconduct_rate","denial_sep_vol_rate", "denial_sep_other_rate")
      names_list <- c("State","Report Date", "Total Non-Mon Determinations", "Separation Denials", "Misconduct Denial Rate", "Voluntary Quit Denial Rate", "Other Denial Rate")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class = "nowrap stripe") %>% 
        formatRound(columns = c(3:4), digits = 0)
    }
    
    else if (input$viewData == "nonMonNonSep")
    {
      col_list <- c("determ_total", "denial_non_total", "denial_non_aa_percent","denial_non_income_percent", "denial_non_refusework_percent", "denial_non_reporting_percent", "denial_non_referrals_percent", "denial_non_other_percent")
      names_list <- c("State","Report Date", "Total Non-Mon Determinations", "Non-Separation Denials", "A&A %", "Disqualifying Income %", "Refusal of Suitable Work %", "Reporting/call In/etc..", "Refuse Referral", "Other")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class = "nowrap stripe")
    }

    else if (input$viewData == "nonMonNonSepRate")
    {
      ## mgh: two problems: 1 we are missing lots of data b/c Refuse Referral is NA not 0
      # 2: we are missing a loess line
      col_list <- c("denial_non_aa_rate","denial_non_income_rate", "denial_non_refusework_rate", "denial_non_reporting_rate", "denial_non_referrals_rate", "denial_non_other_rate")
      names_list <- c("State","Report Date", "A&A %", "Disqualifying Income %", "Refusal of Suitable Work %", "Reporting/call In/etc..", "Refuse Referral", "Other")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class = "nowrap stripe")
    }
    
    else if (startsWith(input$viewData, "monetaryDeterminations"))
    {
      col_list <- c("monetaryDet_total", "monetaryDet_not_eligible", "monetaryDet_num_establish_duration", "monetaryDet_num_max_benefit", "monetaryDet_avg_weeks_duration", "monetaryDet_mon_eligible_prop", "monetaryDet_max_benefit_prop") 
      names_list <- c("State","Report Date", "Total Monetary Determinations", "Not Monetarily Eligible", "Total Establishing Benefit Years", "Num Max Weekly Benefit", "Average Weeks Duration (after final payment)", "Proportion Monetarily Eligible", "Proportion Eligible for the Maximum Weekly Benefit")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class = "nowrap stripe") %>% 
        formatRound(columns = c(3:6), digits = 0) %>% 
        formatRound(columns = c(7), digits = 2) %>% 
        formatPercentage(columns = c(8:9))
      
    }
    
    
    else if (input$viewData == "recipBreakdown")
    {
      col_list <- c("total_week_mov_avg", "unemployed_avg", "recipiency_annual_total")
      names_list <- c("State","Report Date", "Weekly Continuing Claims (12-mo moving avg)","Total Unemployed (12-mo moving avg)", "Recipiency (State + Fed)")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class = "stripe") %>%
        formatCurrency(columns=c(3:4), currency='', digits=0)
      
      
    }

    else if (input$viewData == "uirate")
    {
      ## mgh: missing a few values - civilian non-inst population; labor force; can get from BLS?
      col_list <- c("civilian_non_insitutionalized_population_sa", "labor_force_sa", "total_unemployed_sa", "unemployment_rate_sa")
      names_list <- c("State","Month", "Civilian Non-Inst. Pop","Labor Force", "Unemployed (SA)", "% Unemployed (SA)")
      uiDT <- get_UI_DT_datable(df, col_list, names_list) %>% 
        formatRound(columns=c(3:5), digits=0)
      
    }
    
    else if (input$viewData == "recipRate")
    {
      col_list <- c("recipiency_annual_reg","recipiency_annual_total")
      names_list <- c("State","Report Date", "Regular Programs", "Regular + Federal Programs")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class = "nowrap stripe")
    }
    
    else if (input$viewData == "lowerAuthority") {
      
      # mgh: is lower total correct?  and also us averages
      col_list <- c("total_lower_appeals", "lower_Within30Days", "lower_Within45Days", "lower_filed", "lower_appeals_total_disposed", "lower_appeals_total_outstanding")
      names_list <- c("State", "Date", "Total Lower Authority Appeals", "Within 30 Days", "Within 45 Days", "Number Filed", "Number Decided", "Number Pending")#, "US 30 Day Avg" ,"US 45 Day Avg")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class="nowrap stripe", lim_a = .6, lim_b = .8) %>% 
        formatRound(columns = c(5:7), digits = 0)
      
      
    } else if (input$viewData == "firstPay") {
      # mgh: need to think about how to add in US averages
      col_list <- c("first_time_payment_total", "first_time_payment_Within15Days", "first_time_payment_Within35Days")
      names_list <- c("State", "Date", "First Time Payments", "Within 15 Days", "Within 35 Days") #, "US 15 Day Avg", "US 35 Day Avg")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class="nowrap stripe", lim_a = .87, lim_b = .93) %>% 
        formatRound(columns = c(5), digits = 0)
      
      
    } else if (input$viewData == "higherAuthority") {
      # mgh: same: us averages and is higher_total correct?
      col_list <- c("total_higher_appeals", "higher_Within45Days", "higher_Within75Days", "higher_filed", "higher_appeals_total_disposed", "higher_appeals_total_outstanding")
      names_list <- c("State", "Date", "Total Higher Authority Appeals", "Within 45 Days", "Within 75 Days", "Number Filed", "Number Decided", "Number Pending") #, "US 45 Day Avg", "US 75 Day Avg")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class="nowrap stripe", lim_a = .4, lim_b = .8) %>% 
        formatRound(columns = c(5:7), digits = 0)
      
    }
      
    return(uiDT)
  })

  output$downloader = downloadHandler(
    filename = function() {
      # get an appropriate filename
      filename <- switch(input$viewData,
                         "monthlyUI" = "Monthly_UI_Payments",
                         "basicUI_claims" = "Basic_UI_Payments_Claims",
                         "basicWeeklyClaims" = "Weekly_UI_Claims",
                         "basicUI_compensated" = "Basic_UI_Payments_Claims",
                         "basicUI_payment_rate" = "Basic_UI_Payments_Claims",
                         "basicUI_workshare_initial" = "Basic_UI_Payments_Claims",
                         "basicUI_workshare_continued" = "Basic_UI_Payments_Claims",
                         "demographics_race" = "UI_Demographics",
                         "demographics_ethnicity" = "UI_Demographics",
                         "demographics_sex" = "UI_Demographics",
                         "demographics_age" = "UI_Demographics",
                         "puaData" = "PUA_Data",
                         "puaClaims" = "PUA_Claims",
                         "pucClaims" = "PUC_Claims",
                         "overvPayments" = "Monthly_UI_Overpayments_v_UI_Payments",
                         "fraudvNon" = "Monthly_UI_Fraud_and_Non_Fraud_Overpayments",
                         "TOPS" = "UI_Tax_Offset_Program_Data",
                         "overvRecovery" = "Monthly_UI_Overpayments_v_Overpayment_Recovery",
                         "nonMonDen" = "Monthly_UI_Non_Monetary_Denials",
                         "nonMonSep" = "Monthly_UI_Non_Monetary_Separation_Denials",
                         "nonMonSepRate" = "Monthly_UI_Non_Monetary_Separation_Rates",
                         "nonMonNonSep" = "Monthly_Non_Monetary_Non_Separation_Denials",
                         "nonMonNonSepRate" = "Monthly_Non_Monetary_Non_Separation_Rates",
                         "monetaryDeterminations" = "Monthly_Monetary_Determinations",
                         "monetaryDeterminations_max_weekly" = "Monthly_Monetary_Determinations",
                         "monetaryDeterminations_average_weeks" = "Monthly_Monetary_Determinations",
                         "uirate" = "Monthly_UI_Rate",
                         "recipRate" = "Monthly_UI_Recipiency_Rate",
                         "recipBreakdown" = "Monthly_UI_Recipiency_Rate_Breakdown",
                         "lowerAuthority" = "Monthly_Lower_Authority_Appeal_Decision_Timeliness",
                         "firstPay" = "Monthly_First_Time_Payment_Timeliness",
                         "higherAuthority" = "Monthly_Higher_Authority_Appeal_Decision_Timeliness")
      
      # add in the state name and dates to the filename
      filename = glue::glue("{input$state}_{filename}_{format(date_filter_start, '%Y-%m')}_to_{format(date_filter_end, '%Y-%m')}.csv")
      return(filename)
    },

    content = function(file) { 
      
    # set vars to use for the datatable and download
      
      demographics_list = c("demographic_sex_men", "demographic_sex_women", 
                            "demographic_eth_latinx", "demographic_eth_non_latinx",
                            "demographic_race_black", "demographic_race_white",
                            "demographic_race_native_american_alaskan",  "demographic_race_asian_pacific_islander", "demographic_race_unk",
                            "demographic_age_under25", "demographic_age_25_34", "demographic_age_35_44","demographic_age_45_54", "demographic_age_55_and_older", "demographic_age_unk", 
                            "demographic_all_insured",
                            "demographic_sex_prop_men", "demographic_sex_prop_women", 
                            "demographic_eth_prop_latinx", "demographic_eth_prop_non_latinx", 
                            "demographic_race_prop_black", "demographic_race_prop_white", "demographic_race_prop_native_american_alaskan", "demographic_race_prop_asian_pacific_islander","demographic_race_prop_unk", 
                            "demographic_age_prop_under25", "demographic_age_prop_25_34", "demographic_age_prop_35_44", "demographic_age_prop_45_54", "demographic_age_prop_55_and_older", "demographic_age_prop_unk")

      basicUI_list <- c("monthly_initial_claims", "monthly_first_payments", "monthly_weeks_compensated", "monthly_partial_weeks_compensated", "monthly_weeks_claimed", 
                        "monthly_exhaustion", "monthly_first_payments_as_prop_claims", "workshare_initial_claims", "workshare_continued_claims", "workshare_weeks_compensated", "workshare_first_payments")
      
      monetary_determinations_list <- c("monetaryDet_total", "monetaryDet_not_eligible", "monetaryDet_num_establish_duration", "monetaryDet_num_max_benefit", "monetaryDet_avg_weeks_duration", "monetaryDet_mon_eligible_prop", "monetaryDet_max_benefit_prop") 
      
      col_list <- switch(input$viewData,
                       "monthlyUI" = c("total_state_compensated_mov_avg", "total_federal_compensated_mov_avg", "total_state_compensated", "total_federal_compensated", "total_paid_annual_mov_avg"),
                       "basicUI_claims" = basicUI_list,
                       "basicWeeklyClaims" = c("weekly_initial_claims", "weekly_continued_claims"),
                       "basicUI_compensated" = basicUI_list,
                       "basicUI_payment_rate" = basicUI_list,
                       "basicUI_workshare_initial" = basicUI_list,
                       "basicUI_workshare_continued" = basicUI_list,
                       "demographics_race" = demographics_list,
                       "demographics_ethnicity" = demographics_list,
                       "demographics_age" = demographics_list,
                       "demographics_sex" = demographics_list,
                       "puaData" = c("pua_initial_applications_total", "pua_initial_applications_self", "pua_eligible_total",  "pua_eligible_self" , "pua_percent_eligible", "pua_percent_eligible_self_employed", "pua_percent_applicants_self_employed"),
                       "puaClaims" = c("pua_initial_applications_total", "pua_eligible_total", "pua_first_payments", "pua_weeks_compensated"),
                       "pucClaims" = c("puc_payments_total", "puc_weeks_compensated"),
                       "overvPayments" = c("outstanding_proportion", "outstanding", "total_paid_annual_mov_avg"),
                       "fraudvNon" = c("fraud_num_percent", "regular_fraud_num", "federal_fraud_num","regular_nonfraud_num","federal_nonfraud_num"),
                       "TOPS" = c("state_tax_recovery", "federal_tax_recovery"),
                       "overvRecovery" = c("outstanding", "recovered", "outstanding_fed_programs", "recovered_fed_programs"),
                       "nonMonDen" = c("determ_total", "determ_sep_vol", "determ_sep_misconduct", "determ_sep_other", "determ_non_aa", "determ_non_income", "determ_non_refusework", "determ_non_reporting", "determ_non_referrals", "determ_non_other",  "denial_sep_total", "denial_non_total", "denial_sep_misconduct", "denial_sep_vol", "denial_sep_other", "denial_non_aa", "denial_non_income", "denial_non_refusework", "denial_non_reporting","denial_non_referrals", "denial_non_other"),
                       "nonMonSep" = c("determ_total", "determ_sep_vol", "determ_sep_misconduct", "determ_sep_other", "determ_non_aa", "determ_non_income", "determ_non_refusework", "determ_non_reporting", "determ_non_referrals", "determ_non_other",  "denial_sep_total", "denial_non_total", "denial_sep_misconduct", "denial_sep_vol", "denial_sep_other", "denial_non_aa", "denial_non_income", "denial_non_refusework", "denial_non_reporting","denial_non_referrals", "denial_non_other"),
                       "nonMonSepRate" = c("determ_total", "determ_sep_vol", "determ_sep_misconduct", "determ_sep_other", "determ_non_aa", "determ_non_income", "determ_non_refusework", "determ_non_reporting", "determ_non_referrals", "determ_non_other",  "denial_sep_total", "denial_non_total", "denial_sep_misconduct", "denial_sep_vol", "denial_sep_other", "denial_non_aa", "denial_non_income", "denial_non_refusework", "denial_non_reporting","denial_non_referrals", "denial_non_other"),
                       "nonMonNonSep" = c("determ_total", "determ_sep_vol", "determ_sep_misconduct", "determ_sep_other", "determ_non_aa", "determ_non_income", "determ_non_refusework", "determ_non_reporting", "determ_non_referrals", "determ_non_other",  "denial_sep_total", "denial_non_total", "denial_sep_misconduct", "denial_sep_vol", "denial_sep_other", "denial_non_aa", "denial_non_income", "denial_non_refusework", "denial_non_reporting","denial_non_referrals", "denial_non_other"),
                       "nonMonNonSepRate" = c("determ_total", "determ_sep_vol", "determ_sep_misconduct", "determ_sep_other", "determ_non_aa", "determ_non_income", "determ_non_refusework", "determ_non_reporting", "determ_non_referrals", "determ_non_other",  "denial_sep_total", "denial_non_total", "denial_sep_misconduct", "denial_sep_vol", "denial_sep_other", "denial_non_aa", "denial_non_income", "denial_non_refusework", "denial_non_reporting","denial_non_referrals", "denial_non_other"),
                       "monetaryDeterminations" = monetary_determinations_list,
                       "monetaryDeterminations_max_weekly" = monetary_determinations_list,
                       "monetaryDeterminations_average_weeks" = monetary_determinations_list,
                       "uirate" = c("civilian_non_insitutionalized_population_sa", "labor_force_sa", "total_unemployed_sa", "unemployment_rate_sa"),
                       "recipRate" = c("recipiency_annual_reg","recipiency_annual_total"),
                       "recipBreakdown" = c("total_week_mov_avg","unemployed_avg","recipiency_annual_total"),
                       "lowerAuthority" = c("total_lower_appeals", "lower_Within30Days", "lower_Within45Days", "lower_filed", "lower_appeals_total_disposed", "lower_appeals_total_outstanding"),
                       "firstPay" = c("first_time_payment_total", "first_time_payment_Within15Days", "first_time_payment_Within35Days", "first_time_payment_total"),
                       "higherAuthority" = c("total_higher_appeals", "higher_Within45Days", "higher_Within75Days", "higher_filed", "higher_appeals_total_disposed", "higher_appeals_total_outstanding"), 
    )
      
    col_list <- c("st", "rptdate", col_list)
    
    
    basicUI_names_list <- c("Initial Claims", "First Payments", "Weeks Compensated", "Partial Weeks Compensated", "Weeks Claimed",
                                                    "Number Exhausted", "Initial Payments / Initial Claims", "Workshare Initial Claims", 
                                                    "Workshare Continued Claims", "Workshare Weeks Compensated", "Workshare First Time Payments")
    
    monetary_determinations_names_list <- c("Total Monetary Determinations", "Not Monetarily Eligible", "Total Establishing Benefit Years", "Num Max Weekly Benefit", "Average Weeks Duration (after final payment)", "Proportion Monetaryily Eligible", "Proportion Eligible for the Maximum Weekly Benefit")
    
    names_list <- switch(input$viewData,
                         "monthlyUI" = c("State UI Payments (Monthly, mov avg)", "Federal UI Payments (Monthly, mov avg)", "State UI Payments (Monthly)", "Federal UI Payments (Monthly)", "Annual UI Payments (mov avg)"),
                         "basicUI_claims" = basicUI_names_list,
                         "basicWeeklyClaims" = c("Initial Claims", "Continued Claims"),
                         "basicUI_compensated" = basicUI_names_list,
                         "basicUI_payment_rate" = basicUI_names_list,
                         "demographics_race" = demographics_list,
                         "demographics_ethnicity" = demographics_list,
                         "demographics_age" = demographics_list,
                         "demographics_sex" = demographics_list,
                         "puaData" = c("PUA Initial Applications", "PUA Initial Applications (Self-Employed)", "PUA Eligible", "PUA Eligible (Self-Employed)", "PUA % Eligible (Overall)", "PUA % Eligible (Self-Employed)", "PUA Percent Self-Employed Applicants"),
                         "puaClaims" = c("PUA Initial Applications", "PUA Eligible", "PUA First Payments", "PUA Weeks Compensated"),
                         "puaClaims" = c("PUC Total Payments", "PUC Weeks Compensated"),
                         "overvPayments" = c("Outstanding balance / Annual UI Payments", "Outstanding Overpayment Balance", "Annual UI Payments"),
                         "fraudvNon" = c("Fraud as % of Total Overpayments", "Regular UI Fraud", "Federal UI Fraud", "Regular UI Non-Fraud", "Federal UI Non-Fraud"),
                         "TOPS" = c("State Tax Recovery", "Federal Tax Recovery"),
                         "overvRecovery" = c("Outstanding Owed", "Recovered", "Outstanding Owed (Fed Programs)", "Recovered (Fed Programs)"),
                         "nonMonDen" = c("Total Determinations", "Separation-Voluntary Quit Determinations", "Separation-Misconduct Determinations", "Separation-Other Determinations", 
                                         "Non-monetary Able & Available Determations", "Non-monetary Disqualifying Income Determations", "Non-monetary Refusal of Suitable Work Determations", "Non-monetary Reporting/Call-ins/Etc Determations", "Non-monetary Refusal of Referral Determations", "Non-monetary Other Determations", 
                                         "Total Separation Denials", "Total Non-Separation Denials", "Separation-Misconduct Denials", "Separation-Voluntaray Quit Denials", "Separation-Other Denials", 
                                         "Non-monetary Able & Available Denials", "Non-monetary Disqualifying Income Denials", "Non-monetary Refusal of Suitable Work Denials", "Non-monetary Reporting/Call-ins/Etc Denials", "Non-monetary Refusal of Referral Denials", "Non-monetary Other Denials"),
                         "nonMonSep" = c("Total Determinations", "Separation-Voluntary Quit Determinations", "Separation-Misconduct Determinations", "Separation-Other Determinations", 
                                         "Non-monetary Able & Available Determations", "Non-monetary Disqualifying Income Determations", "Non-monetary Refusal of Suitable Work Determations", "Non-monetary Reporting/Call-ins/Etc Determations", "Non-monetary Refusal of Referral Determations", "Non-monetary Other Determations", 
                                         "Total Separation Denials", "Total Non-Separation Denials", "Separation-Misconduct Denials", "Separation-Voluntaray Quit Denials", "Separation-Other Denials", 
                                         "Non-monetary Able & Available Denials", "Non-monetary Disqualifying Income Denials", "Non-monetary Refusal of Suitable Work Denials", "Non-monetary Reporting/Call-ins/Etc Denials", "Non-monetary Refusal of Referral Denials", "Non-monetary Other Denials"),
                         "nonMonSepRate" = c("Total Determinations", "Separation-Voluntary Quit Determinations", "Separation-Misconduct Determinations", "Separation-Other Determinations", 
                                             "Non-monetary Able & Available Determations", "Non-monetary Disqualifying Income Determations", "Non-monetary Refusal of Suitable Work Determations", "Non-monetary Reporting/Call-ins/Etc Determations", "Non-monetary Refusal of Referral Determations", "Non-monetary Other Determations", 
                                             "Total Separation Denials", "Total Non-Separation Denials", "Separation-Misconduct Denials", "Separation-Voluntaray Quit Denials", "Separation-Other Denials", 
                                             "Non-monetary Able & Available Denials", "Non-monetary Disqualifying Income Denials", "Non-monetary Refusal of Suitable Work Denials", "Non-monetary Reporting/Call-ins/Etc Denials", "Non-monetary Refusal of Referral Denials", "Non-monetary Other Denials"),
                         "nonMonNonSep" = c("Total Determinations", "Separation-Voluntary Quit Determinations", "Separation-Misconduct Determinations", "Separation-Other Determinations", 
                                            "Non-monetary Able & Available Determations", "Non-monetary Disqualifying Income Determations", "Non-monetary Refusal of Suitable Work Determations", "Non-monetary Reporting/Call-ins/Etc Determations", "Non-monetary Refusal of Referral Determations", "Non-monetary Other Determations", 
                                            "Total Separation Denials", "Total Non-Separation Denials", "Separation-Misconduct Denials", "Separation-Voluntaray Quit Denials", "Separation-Other Denials", 
                                            "Non-monetary Able & Available Denials", "Non-monetary Disqualifying Income Denials", "Non-monetary Refusal of Suitable Work Denials", "Non-monetary Reporting/Call-ins/Etc Denials", "Non-monetary Refusal of Referral Denials", "Non-monetary Other Denials"),
                         "nonMonNonSepRate" = c("Total Determinations", "Separation-Voluntary Quit Determinations", "Separation-Misconduct Determinations", "Separation-Other Determinations", 
                                                "Non-monetary Able & Available Determations", "Non-monetary Disqualifying Income Determations", "Non-monetary Refusal of Suitable Work Determations", "Non-monetary Reporting/Call-ins/Etc Determations", "Non-monetary Refusal of Referral Determations", "Non-monetary Other Determations", 
                                                "Total Separation Denials", "Total Non-Separation Denials", "Separation-Misconduct Denials", "Separation-Voluntaray Quit Denials", "Separation-Other Denials", 
                                                "Non-monetary Able & Available Denials", "Non-monetary Disqualifying Income Denials", "Non-monetary Refusal of Suitable Work Denials", "Non-monetary Reporting/Call-ins/Etc Denials", "Non-monetary Refusal of Referral Denials", "Non-monetary Other Denials"),
                         "monetaryDeterminations" = monetary_determinations_names_list,
                         "monetaryDeterminations_max_weekly" = monetary_determinations_names_list,
                         "monetaryDeterminations_average_weeks" = monetary_determinations_names_list,
                         "uirate" = c("Civilian Non-Institutionalized Population", "Labor Force", "Unemployed", "Unemployment Rate"),
                         "recipRate" = c("Annual Recipiency Rate (Regular UI)","Annual Recipiency Rate (Regular + Federal)"),
                         "recipBreakdown" = c("Weekly Continuing Claims (12-mo moving avg)", "Total Unemployed (12-mo moving avg)", "Recipiency Rate (state + federal programs)"),
                         "lowerAuthority" = c("Total Lower Authority Appeals", "Within 30 Days", "Within 45 Days", "Number Filed", "Number Decided", "Number Pending"),
                         "firstPay" = c("Total First Time Payments", "Within 15 Days", "Within 35 Days", "Total Paid"),
                         "higherAuthority" = c("Total Higher Authority Appeals", "Within 45 Days", "Within 75 Days", "Number Filed", "Number Decided", "Number Pending"))
    names_list <- c("State", "Report Date", names_list)
    
    df <- unemployed_df %>% 
      filter(st == input$state,
             rptdate >= date_filter_start,
             rptdate <= date_filter_end,
             metric %in% col_list) %>% 
      pivot_wider(names_from = metric, values_from = value) %>% 
      rename_at(vars(all_of(col_list)), ~c(names_list)) %>% 
      arrange(desc(`Report Date`)) %>% 
      # remove rows that don't have complete data; this happens most
      # frequently for tables with data that is quarterly since some data in the DT
      # may also be produced monthly.  we only want the quarterly info
      na.omit()
    
    
    write.csv(df, file, row.names=FALSE)
  })
    
  # render the proper leaflet map
  output$uimap <- renderLeaflet({
    uiMap <- switch(input$viewData,
                    "monthlyUI" = getUIMap(unemployed_df, date_filter_end,"total_compensated_mov_avg", paste("12-mo moving average of total UI Payments disbursed in ", date_filter_end), FALSE, suffix = "M", scale = 1/1000000, round_digits = 0),
                    "basicUI_claims" = getUIMap(unemployed_df, date_filter_end,"monthly_initial_claims", paste("Initial UI Claims in ", date_filter_end), FALSE),
                    "basicWeeklyClaims" = getUIMap(unemployed_df, date_filter_end,"weekly_initial_claims", paste("Weekly Initial UI Claims in ", date_filter_end), FALSE),
                    "basicUI_compensated" = getUIMap(unemployed_df, date_filter_end,"monthly_weeks_compensated", paste("Weeks Compensated in ", date_filter_end), FALSE),
                    "basicUI_payment_rate" = getUIMap(unemployed_df, date_filter_end,"monthly_first_payments_as_prop_claims", paste("First Payments as a Proportion of Initial Claims in ", date_filter_end), FALSE),
                    "basicUI_workshare_initial" = getUIMap(unemployed_df, date_filter_end,"workshare_initial_claims", paste("Workshare Initial Claims in ", date_filter_end), FALSE),
                    "basicUI_workshare_continued" = getUIMap(unemployed_df, date_filter_end,"workshare_continued_claims", paste("Workshare Continued Claims in ", date_filter_end), FALSE),
                    "demographics_race" = getUIMap(unemployed_df, date_filter_end,"demographic_race_prop_black", paste("Percent Black Claimants in ", date_filter_end), FALSE),
                    "demographics_age" = getUIMap(unemployed_df, date_filter_end,"demographic_age_prop_under25", paste("Percent Claimants Under 25yo in ", date_filter_end), FALSE),
                    "demographics_ethnicity" = getUIMap(unemployed_df, date_filter_end,"demographic_eth_prop_latinx", paste("Percent Latinx Claimants in ", date_filter_end), FALSE),
                    "demographics_sex" = getUIMap(unemployed_df, date_filter_end,"demographic_sex_prop_women", paste("Percent Female Claimants in ", date_filter_end), FALSE),
                    "puaData" = getUIMap(unemployed_df, date_filter_end,"pua_percent_eligible", paste("Pecent of Eligible PUA Applicants in ", date_filter_end), FALSE),
                    "puaClaims" = getUIMap(unemployed_df, date_filter_end,"pua_weeks_compensated", paste("PUA Total Weeks Compensated in ", date_filter_end), FALSE),
                    "pucClaims" = getUIMap(unemployed_df, date_filter_end,"puc_weeks", paste("PUC Total Weeks Compensated in ", date_filter_end), FALSE),
                    "overvPayments" = getUIMap(unemployed_df, date_filter_end, "outstanding_proportion", paste("Outstanding Overpayment Balance as a Proportion of Total UI Paid Annually in ", date_filter_end), FALSE),
                    "fraudvNon" = getUIMap(unemployed_df, date_filter_end,"fraud_num_percent", paste("Fraud vs Non-Fraud Overpayments in ",date_filter_end),FALSE),
                    "overvRecovery" = getUIMap(unemployed_df, date_filter_end,"outstanding", paste("Outstanding Overpayments Balance in ",date_filter_end), FALSE, scale = 1/1000000, prefix = "$", suffix = "M", round_digits = 0),
                    "nonMonDen" = getUIMap(unemployed_df, date_filter_end,"denial_rate_overall", paste("Non-Monetary Denial Rate in ",date_filter_end),FALSE),
                    "nonMonSep" = getUIMap(unemployed_df, date_filter_end,"denial_sep_percent", paste("Proportion of Non-Monetary Denials that are Separation Related in ",date_filter_end),FALSE),
                    "nonMonSepRate" = getUIMap(unemployed_df, date_filter_end,"denial_sep_rate", paste("Non-Monetary Denial Rate in ",date_filter_end),FALSE),
                    "nonMonNonSep" = getUIMap(unemployed_df, date_filter_end,"denial_non_percent", paste("Proportion of Non-Monetary Denials that are Non-Separation Related in ",date_filter_end),FALSE),
                    "nonMonNonSepRate" = getUIMap(unemployed_df, date_filter_end,"denial_non_rate", paste("Non-Monetary Non-Separation Denial Rate in ",date_filter_end),FALSE),
                    "monetaryDeterminations" = getUIMap(unemployed_df, date_filter_end,"monetaryDet_mon_eligible_prop", paste("Proportion Monetarily Eligible in ",date_filter_end),FALSE),
                    "monetaryDeterminations_max_weekly" = getUIMap(unemployed_df, date_filter_end,"monetaryDet_max_benefit_prop", paste("Proportion Eligible for Max Weekly Benefit in ",date_filter_end),FALSE),
                    "monetaryDeterminations_average_weeks" = getUIMap(unemployed_df, date_filter_end,"monetaryDet_avg_weeks_duration", paste("Average Weeks Duration in ",date_filter_end),FALSE),
                    "TOPS" = getUIMap(unemployed_df, date_filter_end,"federal_tax_recovery", paste("Federal Tax Intercepts in Quarter ending ",date_filter_end), FALSE, scale = 1/1000000, prefix = "$", suffix = "M", round_digits = 0),
                    "recipRate" = getUIMap(unemployed_df, date_filter_end,"recipiency_annual_total", paste("Recipiency Rate (State+Federal) in ",date_filter_end), TRUE),
                    "recipBreakdown" = getUIMap(unemployed_df, date_filter_end,"recipiency_annual_total", paste("Recipiency Rate (State+Federal) in ",date_filter_end), TRUE),
                    "uirate" = getUIMap(unemployed_df, date_filter_end,"unemployment_rate_sa", paste("Seasonally Adjusted Unemployed Rate in ",date_filter_end), FALSE),
                    "lowerAuthority" = getUIMap(unemployed_df, date_filter_end,"lower_Within45Days", paste("Proportion of First Level Appeal Decisions within 45 days, ",date_filter_end), TRUE),
                    "firstPay" = getUIMap(unemployed_df, date_filter_end,"first_time_payment_Within35Days", paste("Proportion of First Payments within 35 days, ",date_filter_end), TRUE),
                    "higherAuthority" = getUIMap(unemployed_df, date_filter_end,"higher_Within75Days", paste("Proportion of Second Level Appeal Decisions within 75 days, ",date_filter_end), TRUE))
      
    return(uiMap)
  })
  
  
  # recip breakdown - should have multiple lines? mgh
  output$smplot <- renderPlot({ 
    
    # should the scales be the same for all facets or free?
    free_y = !input$constant_y_axis
      
    smPlot <- switch(input$viewData,
                    "monthlyUI" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "total_compensated_mov_avg", "Montly UI Payments","50-state Comparison of Total Monthly UI Payments", free_y, scale = 1/1000000, prefix = "", suffix = "M"),
                    "basicUI_claims" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "monthly_initial_claims", "Initial Claims","50-state Comparison of Initial UI Claims", free_y),
                    "basicWeeklyClaims" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "weekly_initial_claims", "Initial Claims","50-state Comparison of Weekly Initial UI Claims", free_y),
                    "basicUI_compensated" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "monthly_weeks_compensated", "Weeks Compensated","50-state Comparison of Weeks of UI Compensated", free_y),
                    "basicUI_payment_rate" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "monthly_first_payments_as_prop_claims", "Proportion","50-state Comparison of Initial Payments / Initial Claims", free_y),
                    "basicUI_workshare_initial" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "workshare_initial_claims", "Initial Claims","50-state Comparison of Workshare Initial Claims", free_y),
                    "basicUI_workshare_continued" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "workshare_continued_claims", "Continued Claims","50-state Comparison of Workshare Continued Claims", free_y),
                    "demographics_race" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "demographic_race_prop_black", "Proportion","50-state Comparison of the Proportion of Black Claiamnts", free_y),
                    "demographics_ethnicity" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "demographic_eth_prop_latinx", "Proportion","50-state Comparison of the Proportion of Latinx Claiamnts", free_y),
                    "demographics_age" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "demographic_age_prop_under25", "Proportion","50-state Comparison of the Proportion of Claiamnts Under 25yo", free_y),
                    "demographics_sex" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "demographic_sex_prop_women", "Proportion","50-state Comparison of the Proportion of Female Claiamnts", free_y),
                    "puaData" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "pua_percent_eligible", "PUA Eligibility Rate","50-state Comparison of PUA Eligibility Rate", free_y),
                    "puaClaims" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "pua_weeks_compensated", "Weeks Compesnated","50-state Comparison of Weeks of PUA Compensated", free_y),
                    "pucClaims" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "puc_weeks", "Weeks Compesnated","50-state Comparison of Weeks of PUC Compensated", free_y),
                    "overvPayments" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "outstanding_proportion", "Overpayment Balance/Annual UI Payments","50-state Comparison of Outstanding Overpayment Balance as a Proportion of Total UI Paid Annually", free_y),
                    "fraudvNon" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "fraud_num_percent", "Fraud/Non-Fraud","50-state Comparison of Fraud vs Non-Fraud UI Overpayemnts", free_y),
                    "overvRecovery" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "outstanding", "Overpayment Balance","50-state Comparison of Outstanding State UI Overpayment Balance", free_y, scale = 1/1000000, prefix = "$", suffix = "M"),
                    "nonMonDen" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "denial_rate_overall", "Non-Monetary Denial Rate","50-state Comparison of Denial Rates for Non-Monetary Reasons", free_y),
                    "nonMonSep" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "denial_sep_percent", "Proportion of all Non-Monetary Determinations","50-state Comparison of Denials for Separation Reasons", free_y),
                    "nonMonSepRate" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "denial_sep_rate", "Non-Monetary Separation Denial Rate","50-state Comparison of Denial Rate for Separation Reasons", free_y),
                    "nonMonNonSep" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "denial_non_percent", "Proportion of all Non-Monetary Determinations","50-state Comparison of Denials for Non-Separation Reasons", free_y),
                    "nonMonNonSepRate" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "denial_non_rate", "Non-Monetary Non-Separation Denial Rate","50-state Comparison of Denial Rate for Non-Separation Reasons", free_y),
                    "monetaryDeterminations" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "monetaryDet_mon_eligible_prop", "Proportion Monetarily Eligible","50-state Comparison of Monetary Eligibility Rates", free_y),
                    "monetaryDeterminations_max_weekly" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "monetaryDet_max_benefit_prop", "Proportion Receiving Max Weekly Benefit","50-state Comparison of Max Weekly Benefit Rates", free_y),
                    "monetaryDeterminations_average_weeks" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "monetaryDet_avg_weeks_duration", "Average Weeks Duration","50-state Comparison of the Average Weeks Duration of Final Payments", free_y),
                    "TOPS" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "federal_tax_recovery", "Fed Tax Intercept $","50-state Comparison of Fed Tax Intercepts (Quarterly)", free_y, scale = 1/1000000, prefix = "$", suffix = "M"),
                    "recipRate" = getSMPlot(unemployed_df, date_filter_start, date_filter_end, "recipiency_annual_total", "Recipiency Rate", "50-state Comparison of UI Recipiency Rate", free_y),
                    "recipBreakdown" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "recipiency_annual_total", "Recipiency Rate", "50-state Comparison of UI Recipiency Rates", free_y),
                    "uirate" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "unemployment_rate_sa", "Unemployment Rate (Seasonally Adjusted)","50-state Comparison of SA Unemployment Rates", free_y),
                    "lowerAuthority" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "lower_Within45Days", "Proportion of Decisions Within 45 Days","50-state Comparison of First Level Appeal Decisions within 45 Days", free_y),
                    "firstPay" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "first_time_payment_Within35Days","Proportion of Payments Within 35 Days", "50-state Comparison of First Payments within 35 Days", free_y),
                    "higherAuthority" = getSMPlot(unemployed_df,date_filter_start, date_filter_end, "higher_Within75Days", "Proportion of Decisions Within 75 Days", "50-state Comparison of Second Level Appeal Decisions within 75 Days", free_y))
    
    return(smPlot)
  })

  # render the small multiple plot
  output$fiftyStatePlot <- renderPlot({
    fiftyStatePlot <- switch(input$viewData,
                     "monthlyUI" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "total_compensated_mov_avg", input$state, "Monthly UI Payments",paste(input$state, "vs. US: Total Monthly UI Payments"), scale = 1/1000000, prefix = "", suffix = "M"),
                     "basicUI_claims" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "monthly_initial_claims", input$state, "Initial Claims",paste(input$state, "vs. US: Monthly Initial Claims")),
                     "basicWeeklyClaims" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "weekly_initial_claims", input$state, "Initial Claims",paste(input$state, "vs. US: Weekly Initial Claims")),
                     "basicUI_compensated" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "monthly_weeks_compensated", input$state, "Weeks Compensated",paste(input$state, "vs. US: Monthly Weeks Compensated")),
                     "basicUI_payment_rate" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "monthly_first_payments_as_prop_claims", input$state, "Proportion",paste(input$state, "vs. US: First Payments / Initial Claims")),
                     "basicUI_workshare_initial" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "workshare_initial_claims", input$state, "Initial Claims",paste(input$state, "vs. US: Workshare Initial Claims")),
                     "basicUI_workshare_continued" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "workshare_continued_claims", input$state, "Continued Claims",paste(input$state, "vs. US: Workshare Continued Claims")),
                     "demographics_race" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "demographic_race_prop_black", input$state, "Proportion",paste(input$state, "vs. US: % Black Claimants")),
                     "demographics_ethnicity" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "demographic_eth_prop_latinx", input$state, "Proportion",paste(input$state, "vs. US: % Latinx Claimants")),
                     "demographics_sex" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "demographic_sex_prop_women", input$state, "Proportion",paste(input$state, "vs. US: % Female Claimants")),
                     "demographics_age" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "demographic_age_prop_under25", input$state, "Proportion",paste(input$state, "vs. US: % Claimants Under 25yo")),
                     "puaData" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "pua_percent_eligible", input$state, "PUA Eligibility Rate",paste(input$state, "vs. US: PUA Eligibility Rate")),
                     "puaClaims" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "pua_weeks_compensated", input$state, "Weeks Compensated",paste(input$state, "vs. US: Weeks of PUA Compensated")),
                     "pucClaims" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "puc_weeks", input$state, "Weeks Compensated",paste(input$state, "vs. US: Weeks of PUC Compensated")),
                     "overvPayments" = get50StateComparisonPlot(unemployed_df, date_filter_start, date_filter_end, "outstanding_proportion", input$state, "Overpayment Balance/Annual UI Payments",paste(input$state, "vs. US: Outstanding Overpayment Balance as a Proportion of Total UI Paid Annually")),
                     "fraudvNon" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "fraud_num_percent", input$state, "Fraud/Non-Fraud",paste(input$state, "vs. US: Fraud / Non-Fraud UI Overpayemnts")),
                     "overvRecovery" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "outstanding", input$state, "Overpayment Balance",paste(input$state, "vs. US: Outstanding UI Overpayment Balance"), scale = 1/1000000, prefix = "$", suffix = "M"),
                     "nonMonDen" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "denial_rate_overall", input$state, "Non-Monetary Denial Rate",paste(input$state, "vs. US: Denial Rates for Non-Monetary Reasons")),
                     "nonMonSep" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "denial_sep_percent", input$state, "Proportion of all Non-Monetary Determinations",paste(input$state, "vs. US: Denials for Separation Reasons")),
                     "nonMonSepRate" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "denial_sep_rate", input$state, "Non-Monetary Separation Denial Rate",paste(input$state, "vs. US: Denial Rate for Separation Reasons")),
                     "nonMonNonSep" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "denial_non_percent", input$state, "Proportion of all Non-Monetary Determinations",paste(input$state, "vs. US: Denials for Non-Separation Reasons")),
                     "nonMonNonSepRate" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "denial_non_rate", input$state, "Non-Monetary Non-Separation Denial Rate",paste(input$state, "vs. US: Denial Rate for Non-Separation Reasons")),
                     "monetaryDeterminations" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "monetaryDet_mon_eligible_prop", input$state, "Monetary Eligibility Rate",paste(input$state, "vs. US: Monetary Eligibility Rate")),
                     "monetaryDeterminations_max_weekly" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "monetaryDet_max_benefit_prop", input$state, "Max Weekly Benefit Eligibility Rate",paste(input$state, "vs. US: Max Weekly Benefit Eligibility Rate")),
                     "monetaryDeterminations_average_weeks" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "monetaryDet_avg_weeks_duration", input$state, "Average Weeks Duration",paste(input$state, "vs. US: Average Weeks Duration")),
                     "TOPS" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "federal_tax_recovery", input$state, "Fed Tax Intercept $",paste(input$state, "vs. US: Fed Tax Intercepts (Quarterly)"), scale = 1/1000000, prefix = "$", suffix = "M"),
                     "recipRate" = get50StateComparisonPlot(unemployed_df, date_filter_start, date_filter_end, "recipiency_annual_total", input$state, "Recipiency Rate", paste(input$state, "vs. US: UI Recipiency Rate")),
                     "recipBreakdown" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "recipiency_annual_total", input$state, "Recipiency Rate", paste(input$state, "vs. US: UI Recipiency Rates")),
                     "uirate" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "unemployment_rate_sa", input$state, "Unemployment Rate",paste(input$state, "vs. US: Seasonally Adjusted Unemployment Rates")),
                     "lowerAuthority" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "lower_Within45Days", input$state, "Proportion of Decisions Within 45 Days", paste(input$state, "vs. US: First Level Appeal Decisions within 45 Days")),
                     "firstPay" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "first_time_payment_Within35Days", input$state, "Proportion of Payments Within 35 Days", paste(input$state, "vs. US: First Payments within 35 Days")),
                     "higherAuthority" = get50StateComparisonPlot(unemployed_df,date_filter_start, date_filter_end, "higher_Within75Days", input$state, "Proportion of Decisions Within 75 Days", paste(input$state, "vs. US: Second Level Appeal Decisions within 75 Days")))
    return(fiftyStatePlot)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

