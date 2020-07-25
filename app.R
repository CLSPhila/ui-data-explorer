#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# An explorer of UI data, which can be downlaoded from here: https://ows.doleta.gov/unemploy
# This data concerns the administration of the UI program in each state. 
# Specifically, this focuses on data regarding payment and decision timelapses, to ensure that
# states are holding up their end of the bargain in promptly paying claimants or at least adjudicating their 
# cases promptly
# Made by Michael Hollander of Community Legal Services, 1/2017

library(shiny)
library(DT)
library(ggplot2)
library(scales)
library(shinycssloaders)
library(tidyverse)
source("helper.R")

# for testing
# input <- list(range = as.Date(c("2008-01-01", "2020-05-30")), state = "PA")

# read in the df of data that we need
stored_data_location <- "~/unemployment_data.feather"
unemployed_df <- arrow::read_feather(stored_data_location)

maxDate <- max(unemployed_df$rptdate)
minDate <- min(unemployed_df$rptdate)
states <- sort(unique(unemployed_df$st))


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
    
      # the slider that allows for a date range
      sliderInput("range", 
                  label = "Years to View:",
                  # the default range is 10 years prior to the most recent date
                  min = minDate, max = maxDate, value = c(maxDate - (10 * 365), maxDate), timeFormat="%m/%Y"),
      
      # the list of data to view; shoudl rethink how to easily define new data sets rather than
      # having to handcode each dataset here and in 10 other places
      selectInput("viewData",
                  label = 'Select Data to View',
                  size=17, selectize=FALSE,
                  choices = c("Recipiency Rate"="recipRate",
                              "--Recipiency Rate Breakdown"="recipBreakdown",
                              "Monthly UI Payments"="monthlyUI", 
                              "Timeliness of First Payments"="firstPay", 
                              "Timeliness of Lower Authority Decisions"="lowerAuthority", 
                              "Timeliness of Higher Authority Decisions"="higherAuthority", 
                              "Overpayment vs Recovery"="overvRecovery",
                              "--Overpayment Balance/Annual UI Payments"="overvPayments",
                              "--Fraud vs Non Fraud Overpayents"="fraudvNon", 
                              "--Tax Program Overpayment Recoveries"="TOPS",
                              "Non-Monetary Denials"="nonMonDen",
                              "--Separation Denial Breakdown"="nonMonSep",
                              "--Separation Denial Rates"="nonMonSepRate",
                              "--Non-Separation Denial Breakdown"="nonMonNonSep",
                              "--Non-Separation Denial Rates"="nonMonNonSepRate",
                              "Unemployment Rate (SA)"="uirate"),
                  # the default selected is the recipiency Rate, but this coudl be anythign
                  selected = "recipRate"),
      
      # allows for a constant y axis on the main charts, which allows state to state
      # comparisons to be made.  
      checkboxInput("constant_y_axis", 
                    label="Constant y axis? (makes comparisons easier)",
                    value=TRUE),
      
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
        tabPanel("About", br(), 
                 p("This page was created by ", a(href="https://www.clsphila.org" ,"Community Legal Services"), " to visualize the unemployment data made avaialble by the US Department of Labor and Bureau of Labor Statistics."),
                 p("The DOL Data can be found here: https://ows.doleta.gov/unemploy/DataDownloads.asp and the BLS data can be found ", a(href="https://www.bls.gov/web/laus/ststdsadata.txt", "here"), "and ", a(href="https://www.bls.gov/web/laus/ststdnsadata.txt", "here.")),
                 p("If you have any suggestions for any further measures to put on the page, please email ", a(href="mailto:mhollander@clsphila.org", "Michael Hollander"), "(mhollander@clsphila.org, the creator and maintainer of this page."),
                 p("You can find the code for this page on github here: ", a(href='https://github.com/CLSPhila/ui-data-explorer', target="_blank", "https://github.com/CLSPhila/ui-data-explorer"), ".")
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

  
  
  # render the plot
  output$uiplot <- renderPlot({

    df <- unemployed_df %>%
      filter(st == input$state,
             rptdate > (input$range[1]-10),
             rptdate < (input$range[2]+10))

    if (input$viewData == "monthlyUI")
    {
      
      metric_filter = c("total_federal_compensated_mov_avg", "total_state_compensated_mov_avg")
      df <- df %>% 
        filter(metric %in% metric_filter) %>% 
        mutate(metric = factor(metric, labels = metric_filter, ordered = TRUE))
      
      
      uPlot <- getRibbonPlot(df, xlab = "Date", ylab = "Total Paid",
                      caption = "12-month moving average of UI paid per month in both regular and federal UI programs.\nNote that 'regular UI' includes state UI, UFCE, and UCX.  Federal programs include EB, and the various EUC programs that have been enacted.",  
                      title = glue::glue("{input$state} Monthly UI Payments from {format.Date(input$range[1], 'm-%Y')} to {format.Date(input$range[2], '%m-%Y')}"),
                      breaks=c("total_state_compensated_mov_avg","total_federal_compensated_mov_avg"),
                      labels=c("Regular Programs","Federal Programs")) +
        scale_y_continuous(labels = label_number(scale = 1/1000000, prefix = "$", suffix = "M"))
      
      # adjustment for finding the max height of the graph
      metric_filter = c("total_compensated_mov_avg")
      
      
    }
    
    else if (input$viewData == "recipBreakdown")
    {
      metric_filter = c("total_week_mov_avg","unemployed_avg") 
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getLinePlot(df, xlab = "Date", ylab = "", 
                           caption = "Weekly continued claims and Total Unemployed by month.\nBoth numbers are smoothed over 12 month periods.  These are the two components of recipiency rate.",
                           title = glue::glue("{input$state} Recipiency Rate Breakdown from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}"),
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
               rptdate > (input$range[1]-10),
               rptdate < (input$range[2]+10),
               metric %in% metric_filter)

        uPlot <- getLinePlot(df, xlab = "Date", ylab = "",
                             caption = "Seasonally adjusted unemployed rate, based on BLS monthly report found here: https://www.bls.gov/web/laus/ststdsadata.txt.",
                             title = glue::glue("{input$state} Unemployment Rate (SA) from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}"),
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
                     caption = "Outstanding overpayment balance divided by the total benefits paid in all federal and state programs over the last 12 months.\n Data courtesy of the USDOL.  Reports used are ETA 227 and 5159, found at https://ows.doleta.gov/unemploy/DataDownloads.asp.",
                     title = glue::glue("{input$state} Overpayment Balance vs Montly UI Payments from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}"),
                     breaks=c("outstanding_proportion"),
                     labels=c("Overpayment Balance / Annual UI Payments"))
    }

    else if (input$viewData == "fraudvNon")
    {
      
      metric_filter = c("fraud_num_percent")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 227, found at https://ows.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Fraud Overpayments as a Percent of All Overpayments from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}"),
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
                            caption = "Data courtesy of the USDOL.  Report used is ETA 227, found at https://ows.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Tax Program Overpayment Recovery from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}"),
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
                             caption = "Recipiency rate calculated by dividing 12 month moving average of unemployment continuing claims divided by 12 month moving average of total unemployed.\nData not seasonally adjusted.  \nSource: Continuing claims can be found in ETA report 5159, found here: https://ows.doleta.gov/unemploy/DataDownloads.asp.\nUnemployed numbers courtesy the BLS: https://www.bls.gov/web/laus/ststdnsadata.txt.  \nNote that 'regular UI' includes state UI, UFCE, and UCX.  Federal programs include EB, and the various EUC programs that have been enacted.",  
                             title = glue::glue("{input$state} Recipiency Rate from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}"),
                             breaks=c("recipiency_annual_reg","recipiency_annual_fed"),
                             labels=c("Regular Programs", "Federal Programs"))
       
      
    }
    
    else if (input$viewData == "overvRecovery")
    {
      metric_filter = c("outstanding", "recovered")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 227, found at https://ows.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Outstanding State Overpayments vs State $ Recovered from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}"),
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
                            caption = "Data courtesy of the USDOL.  Report used is ETA 207, found at https://ows.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Proportion of Denials for Separation and Non-Separation Reasons\n in Non-Monetary Decisions {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}"),
                            breaks=c("denial_sep_percent", "denial_non_percent", "denial_rate_overall"),
                            labels=c("Separation Denials", "Non-Separation Denials", "Total Denial Rate"))
  
    }

    else if (input$viewData == "nonMonSep")
    {
      
      metric_filter = c("denial_sep_misconduct_percent","denial_sep_vol_percent", "denial_sep_other_percent")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "Proportion of separation denials",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 207, found at https://ows.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Proportion of Non-Monetary Separation Denials from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}"),
                            breaks=c("denial_sep_misconduct_percent","denial_sep_vol_percent", "denial_sep_other_percent"),
                            labels=c("Misconduct", "Voluntary Quit", "Other"))
    
    }

    else if (input$viewData == "nonMonSepRate")
    {
      
      metric_filter = c("denial_sep_misconduct_rate","denial_sep_vol_rate", "denial_sep_other_rate")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "Proportion of separation denials",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 207, found at https://ows.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Non-Monetary Separation Denial Rate from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}"),
                            breaks=c("denial_sep_misconduct_rate","denial_sep_vol_rate", "denial_sep_other_rate"),
                            labels=c("Misconduct", "Voluntary Quit", "Other"))
    
    }
    
    else if (input$viewData == "nonMonNonSep")
    {
      
      metric_filter = c("denial_non_aa_percent","denial_non_income_percent", "denial_non_refusework_percent", "denial_non_reporting_percent", "denial_non_referrals_percent", "denial_non_other_percent")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "Proportion of non-separation denials",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 207, found at https://ows.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Proportion of Non-Monetary Non-Separation Denials from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}"),
                            breaks=c("denial_non_aa_percent","denial_non_income_percent", "denial_non_refusework_percent", "denial_non_reporting_percent", "denial_non_referrals_percent", "denial_non_other_percent"),
                            labels=c("Able and Available", "Disqualifying Income", "Refusal of Suitable Work", "Reporting/Call Ins/Etc...", "Refusal of Referral", "Other"))
    
    }

    else if (input$viewData == "nonMonNonSepRate")
    {
      
      metric_filter = c("denial_non_aa_rate","denial_non_income_rate", "denial_non_refusework_rate", "denial_non_reporting_rate", "denial_non_referrals_rate", "denial_non_other_rate")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "Proportion of non-separation denials",
                            caption = "Data courtesy of the USDOL.  Report used is ETA 207, found at https://ows.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Denial Rates for Non-Monetary Non-Separation Denials from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}"),
                            breaks=c("denial_non_aa_rate","denial_non_income_rate", "denial_non_refusework_rate", "denial_non_reporting_rate", "denial_non_referrals_rate", "denial_non_other_rate"),
                            labels=c("Able and Available", "Disqualifying Income", "Refusal of Suitable Work", "Reporting/Call Ins/Etc...", "Refusal of Referral", "Other"))
      
    
    }
    
    else if (input$viewData == "lowerAuthority") {
      
      metric_filter = c("lower_Within30Days", "lower_Within45Days")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Reports used are ETA 5130, 9050, 9054, and 9055.  \nAll can be found at https://ows.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Lower Authority Decision Timeliness from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}")) %>% 
        add_line_with_label(x = min(df$rptdate), y = .6, label = "30-day threshold") %>% 
        add_line_with_label(x = min(df$rptdate), y = .8, label = "45-day threshold") 

      
    } else if(input$viewData == "firstPay") {
      metric_filter = c("first_time_payment_Within15Days", "first_time_payment_Within35Days")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Reports used are ETA 5130, 9050, 9054, and 9055.  \nAll can be found at https://ows.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} First Payment Timeliness from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}")) %>% 
        add_line_with_label(x = min(df$rptdate), y = .87, label = "15-day threshold") %>% 
        add_line_with_label(x = min(df$rptdate), y = .93, label = "35-day threshold") 
      
      
    } else if(input$viewData == "higherAuthority") {
      
      metric_filter = c("higher_Within45Days", "higher_Within75Days")
      df <- df %>% 
        filter(metric %in% metric_filter)
      
      uPlot <- getPointPlot(df, xlab = "Date", ylab = "",
                            caption = "Data courtesy of the USDOL.  Reports used are ETA 5130, 9050, 9054, and 9055.  \nAll can be found at https://ows.doleta.gov/unemploy/DataDownloads.asp.",
                            title = glue::glue("{input$state} Higher Authority Decision Timeliness from {format.Date(input$range[1], '%m-%Y')} to {format.Date(input$range[2], '%m-%Y')}")) %>% 
        add_line_with_label(x = min(df$rptdate), y = .4, label = "45-day threshold") %>% 
        add_line_with_label(x = min(df$rptdate), y = .8, label = "75-day threshold") 
      
    }


    
    # constant scaling on the y axis for easier state comparison; scaling on x axis so we can scale to ranges with no recession
    if (input$constant_y_axis)
    {
      
      # this represents the highest y value of all of the states for this metric
      ymax = max(unemployed_df %>% 
        filter(metric %in% metric_filter,
               rptdate > (input$range[1]-10),
               rptdate < (input$range[2]+10)) %>% 
        select(value))
      
      uPlot <- uPlot + coord_cartesian(ylim=c(0, ymax),
                                       xlim=c(as.Date(input$range[1]), as.Date(input$range[2])))
    }
    else
    {
      uPlot <- uPlot + coord_cartesian(xlim=c(as.Date(input$range[1]),as.Date(input$range[2])))
    }
    
    return(uPlot)
  })
  
  # render the data table
  output$uidata <- renderDataTable({
    
    df <- unemployed_df %>% 
      filter(st == input$state,
             rptdate > (input$range[1]-10),
             rptdate < (input$range[2]+10))
    
    
    if (input$viewData == "monthlyUI")
    {
      
      col_list = c("total_state_compensated_mov_avg", "total_federal_compensated_mov_avg", "total_state_compensated", "total_federal_compensated", "total_paid_annual_mov_avg")
      names_list <- c("State","Report Date", "State UI Payments (Monthly, mov avg)", "Federal UI Payments (Monthly, mov avg)", "State UI Payments (Monthly)", "Federal UI Payments (Monthly)", "Annual UI Payments (mov avg)")
      uiDT <- get_UI_DT_datable(df, col_list, names_list) %>% 
        formatCurrency(3:7, '$')
      
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
      col_list <- c("lower_Within30Days", "lower_Within45Days", "lower_filed", "lower_disposed", "lower_total")
      names_list <- c("State", "Date", "Within 30 Days", "Within 45 Days", "Number Filed", "Number Decided", "Number Pending")#, "US 30 Day Avg" ,"US 45 Day Avg")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class="nowrap stripe", lim_a = .6, lim_b = .8) %>% 
        formatRound(columns = c(5:7), digits = 0)
      
      
    } else if (input$viewData == "firstPay") {
      # mgh: need to think about how to add in US averages
      col_list <- c("first_time_payment_Within15Days", "first_time_payment_Within35Days", "first_time_payment_total")
      names_list <- c("State", "Date", "Within 15 Days", "Within 35 Days", "Total Paid") #, "US 15 Day Avg", "US 35 Day Avg")
      uiDT <- get_UI_DT_datable(df, col_list, names_list, class="nowrap stripe", lim_a = .87, lim_b = .93) %>% 
        formatRound(columns = c(5), digits = 0)
      
      
    } else if (input$viewData == "higherAuthority") {
      # mgh: same: us averages and is higher_total correct?
      col_list <- c("higher_Within45Days", "higher_Within75Days", "higher_filed", "higher_disposed", "higher_total")
      names_list <- c("State", "Date", "Within 45 Days", "Within 75 Days", "Number Filed", "Number Decided", "Number Pending") #, "US 45 Day Avg", "US 75 Day Avg")
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
                         "overvPayments" = "Monthly_UI_Overpayments_v_UI_Payments",
                         "fraudvNon" = "Monthly_UI_Fraud_and_Non_Fraud_Overpayments",
                         "TOPS" = "UI_Tax_Offset_Program_Data",
                         "overvRecovery" = "Monthly_UI_Overpayments_v_Overpayment_Recovery",
                         "nonMonDen" = "Monthly_UI_Non_Monetary_Denials",
                         "nonMonSep" = "Monthly_UI_Non_Monetary_Separation_Denials",
                         "nonMonSepRate" = "Monthly_UI_Non_Monetary_Separation_Rates",
                         "nonMonNonSep" = "Monthly_Non_Monetary_Non_Separation_Denials",
                         "nonMonNonSepRate" = "Monthly_Non_Monetary_Non_Separation_Rates",
                         "uirate" = "Monthly_UI_Rate",
                         "recipRate" = "Monthly_UI_Recipiency_Rate",
                         "recipBreakdown" = "Monthly_UI_Recipiency_Rate_Breakdown",
                         "lowerAuthority" = "Monthly_Lower_Authority_Appeal_Decision_Timeliness",
                         "firstPay" = "Monthly_First_Time_Payment_Timeliness",
                         "higherAuthority" = "Monthly_Higher_Authority_Appeal_Decision_Timeliness")
      
      # add in the state name and dates to the filename
      filename = glue::glue("{input$state}_{filename}_{format(input$range[1], '%Y-%m')}_to_{format(input$range[2], '%Y-%m')}.csv")
      return(filename)
    },
    
    content = function(file) { 
    
    # set vars to use for the datatable and download
    col_list <- switch(input$viewData,
                       "monthlyUI" = c("total_state_compensated_mov_avg", "total_federal_compensated_mov_avg", "total_state_compensated", "total_federal_compensated", "total_paid_annual_mov_avg"),
                       "overvPayments" = c("outstanding_proportion", "outstanding", "total_paid_annual_mov_avg"),
                       "fraudvNon" = c("fraud_num_percent", "regular_fraud_num", "federal_fraud_num","regular_nonfraud_num","federal_nonfraud_num"),
                       "TOPS" = c("state_tax_recovery", "federal_tax_recovery"),
                       "overvRecovery" = c("outstanding", "recovered", "outstanding_fed_programs", "recovered_fed_programs"),
                       "nonMonDen" = c("determ_total", "determ_sep_vol", "determ_sep_misconduct", "determ_sep_other", "determ_non_aa", "determ_non_income", "determ_non_refusework", "determ_non_reporting", "determ_non_referrals", "determ_non_other",  "denial_sep_total", "denial_non_total", "denial_sep_misconduct", "denial_sep_vol", "denial_sep_other", "denial_non_aa", "denial_non_income", "denial_non_refusework", "denial_non_reporting","denial_non_referrals", "denial_non_other"),
                       "nonMonSep" = c("determ_total", "determ_sep_vol", "determ_sep_misconduct", "determ_sep_other", "determ_non_aa", "determ_non_income", "determ_non_refusework", "determ_non_reporting", "determ_non_referrals", "determ_non_other",  "denial_sep_total", "denial_non_total", "denial_sep_misconduct", "denial_sep_vol", "denial_sep_other", "denial_non_aa", "denial_non_income", "denial_non_refusework", "denial_non_reporting","denial_non_referrals", "denial_non_other"),
                       "nonMonSepRate" = c("determ_total", "determ_sep_vol", "determ_sep_misconduct", "determ_sep_other", "determ_non_aa", "determ_non_income", "determ_non_refusework", "determ_non_reporting", "determ_non_referrals", "determ_non_other",  "denial_sep_total", "denial_non_total", "denial_sep_misconduct", "denial_sep_vol", "denial_sep_other", "denial_non_aa", "denial_non_income", "denial_non_refusework", "denial_non_reporting","denial_non_referrals", "denial_non_other"),
                       "nonMonNonSep" = c("determ_total", "determ_sep_vol", "determ_sep_misconduct", "determ_sep_other", "determ_non_aa", "determ_non_income", "determ_non_refusework", "determ_non_reporting", "determ_non_referrals", "determ_non_other",  "denial_sep_total", "denial_non_total", "denial_sep_misconduct", "denial_sep_vol", "denial_sep_other", "denial_non_aa", "denial_non_income", "denial_non_refusework", "denial_non_reporting","denial_non_referrals", "denial_non_other"),
                       "nonMonNonSepRate" = c("determ_total", "determ_sep_vol", "determ_sep_misconduct", "determ_sep_other", "determ_non_aa", "determ_non_income", "determ_non_refusework", "determ_non_reporting", "determ_non_referrals", "determ_non_other",  "denial_sep_total", "denial_non_total", "denial_sep_misconduct", "denial_sep_vol", "denial_sep_other", "denial_non_aa", "denial_non_income", "denial_non_refusework", "denial_non_reporting","denial_non_referrals", "denial_non_other"),
                       "uirate" = c("civilian_non_insitutionalized_population_sa", "labor_force_sa", "total_unemployed_sa", "unemployment_rate_sa"),
                       "recipRate" = c("recipiency_annual_reg","recipiency_annual_total"),
                       "recipBreakdown" = c("total_week_mov_avg","unemployed_avg","recipiency_annual_total"),
                       "lowerAuthority" = c("lower_Within30Days", "lower_Within45Days", "lower_filed", "lower_disposed", "lower_total"),
                       "firstPay" = c("first_time_payment_Within15Days", "first_time_payment_Within35Days", "first_time_payment_total"),
                       "higherAuthority" = c("higher_Within45Days", "higher_Within75Days", "higher_filed", "higher_disposed", "higher_total"), 
    )
    col_list <- c("st", "rptdate", col_list)
    
    
    names_list <- switch(input$viewData,
                         "monthlyUI" = c("State UI Payments (Monthly, mov avg)", "Federal UI Payments (Monthly, mov avg)", "State UI Payments (Monthly)", "Federal UI Payments (Monthly)", "Annual UI Payments (mov avg)"),
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
                         "uirate" = c("Civilian Non-Institutionalized Population", "Labor Force", "Unemployed", "Unemployment Rate"),
                         "recipRate" = c("Annual Recipiency Rate (Regular UI)","Annual Recipiency Rate (Regular + Federal)"),
                         "recipBreakdown" = c("Weekly Continuing Claims (12-mo moving avg)", "Total Unemployed (12-mo moving avg)", "Recipiency Rate (state + federal programs)"),
                         "lowerAuthority" = c("Within 30 Days", "Within 45 Days", "Number Filed", "Number Decided", "Number Pending"),
                         "firstPay" = c("Within 15 Days", "Within 35 Days", "Total Paid"),
                         "higherAuthority" = c("Within 45 Days", "Within 75 Days", "Number Filed", "Number Decided", "Number Pending"))
    names_list <- c("State", "Report Date", names_list)
    
    df <- unemployed_df %>% 
      filter(st == input$state,
             rptdate > (input$range[1]-10),
             rptdate < (input$range[2]+10),
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
                    "monthlyUI" = getUIMap(unemployed_df, input$range[2],"total_compensated_mov_avg", paste("12-mo moving average of total UI Payments disbursed in ", input$range[2]), FALSE, suffix = "M", scale = 1/1000000, round_digits = 0),
                    "overvPayments" = getUIMap(unemployed_df, input$range[2], "outstanding_proportion", paste("Outstanding Overpayment Balance as a Proportion of Total UI Paid Annually in ", input$range[2]), FALSE),
                    "fraudvNon" = getUIMap(unemployed_df, input$range[2],"fraud_num_percent", paste("Fraud vs Non-Fraud Overpayments in ",input$range[2]),FALSE),
                    "overvRecovery" = getUIMap(unemployed_df, input$range[2],"outstanding", paste("Outstanding Overpayments Balance in ",input$range[2]), FALSE, scale = 1/1000000, prefix = "$", suffix = "M", round_digits = 0),
                    "nonMonDen" = getUIMap(unemployed_df, input$range[2],"denial_rate_overall", paste("Non-Monetary Denial Rate in ",input$range[2]),FALSE),
                    "nonMonSep" = getUIMap(unemployed_df, input$range[2],"denial_sep_percent", paste("Proportion of Non-Monetary Denials that are Separation Related in ",input$range[2]),FALSE),
                    "nonMonSepRate" = getUIMap(unemployed_df, input$range[2],"denial_sep_rate", paste("Non-Monetary Denial Rate in ",input$range[2]),FALSE),
                    "nonMonNonSep" = getUIMap(unemployed_df, input$range[2],"denial_non_percent", paste("Proportion of Non-Monetary Denials that are Non-Separation Related in ",input$range[2]),FALSE),
                    "nonMonNonSepRate" = getUIMap(unemployed_df, input$range[2],"denial_non_rate", paste("Non-Monetary Non-Separation Denial Rate in ",input$range[2]),FALSE),
                    "TOPS" = getUIMap(unemployed_df, input$range[2],"federal_tax_recovery", paste("Federal Tax Intercepts in Quarter ending ",input$range[2]), FALSE, scale = 1/1000000, prefix = "$", suffix = "M", round_digits = 0),
                    "recipRate" = getUIMap(unemployed_df, input$range[2],"recipiency_annual_total", paste("Recipiency Rate (State+Federal) in ",input$range[2]), TRUE),
                    "recipBreakdown" = getUIMap(unemployed_df, input$range[2],"recipiency_annual_total", paste("Recipiency Rate (State+Federal) in ",input$range[2]), TRUE),
                    "uirate" = getUIMap(unemployed_df, input$range[2],"unemployment_rate_sa", paste("Seasonally Adjusted Unemployed Rate in ",input$range[2]), FALSE),
                    "lowerAuthority" = getUIMap(unemployed_df, input$range[2],"lower_Within45Days", paste("Proportion of First Level Appeal Decisions within 45 days, ",input$range[2]), TRUE),
                    "firstPay" = getUIMap(unemployed_df, input$range[2],"first_time_payment_Within35Days", paste("Proportion of First Payments within 35 days, ",input$range[2]), TRUE),
                    "higherAuthority" = getUIMap(unemployed_df, input$range[2],"higher_Within75Days", paste("Proportion of Second Level Appeal Decisions within 75 days, ",input$range[2]), TRUE))
      
    return(uiMap)
  })
  
  
  # recip breakdown - should have multiple lines? mgh
  output$smplot <- renderPlot({ 
    
    # should the scales be the same for all facets or free?
    free_y = !input$constant_y_axis
    
    smPlot <- switch(input$viewData,
                    "monthlyUI" = getSMPlot(unemployed_df, input$range[1], input$range[2], "total_compensated_mov_avg", "Montly UI Payments","50-state Comparison of Total Monthly UI Payments", free_y, scale = 1/1000000, prefix = "", suffix = "M"),
                    "overvPayments" = getSMPlot(unemployed_df, input$range[1], input$range[2], "outstanding_proportion", "Overpayment Balance/Annual UI Payments","50-state Comparison of Outstanding Overpayment Balance as a Proportion of Total UI Paid Annually", free_y),
                    "fraudvNon" = getSMPlot(unemployed_df,input$range[1], input$range[2], "fraud_num_percent", "Fraud/Non-Fraud","50-state Comparison of Fraud vs Non-Fraud UI Overpayemnts", free_y),
                    "overvRecovery" = getSMPlot(unemployed_df,input$range[1], input$range[2], "outstanding", "Overpayment Balance","50-state Comparison of Outstanding State UI Overpayment Balance", free_y, scale = 1/1000000, prefix = "$", suffix = "M"),
                    "nonMonDen" = getSMPlot(unemployed_df,input$range[1], input$range[2], "denial_rate_overall", "Non-Monetary Denial Rate","50-state Comparison of Denial Rates for Non-Monetary Reasons", free_y),
                    "nonMonSep" = getSMPlot(unemployed_df,input$range[1], input$range[2], "denial_sep_percent", "Proportion of all Non-Monetary Determinations","50-state Comparison of Denials for Separation Reasons", free_y),
                    "nonMonSepRate" = getSMPlot(unemployed_df,input$range[1], input$range[2], "denial_sep_rate", "Non-Monetary Separation Denial Rate","50-state Comparison of Denial Rate for Separation Reasons", free_y),
                    "nonMonNonSep" = getSMPlot(unemployed_df,input$range[1], input$range[2], "denial_non_percent", "Proportion of all Non-Monetary Determinations","50-state Comparison of Denials for Non-Separation Reasons", free_y),
                    "nonMonNonSepRate" = getSMPlot(unemployed_df,input$range[1], input$range[2], "denial_non_rate", "Non-Monetary Non-Separation Denial Rate","50-state Comparison of Denial Rate for Non-Separation Reasons", free_y),
                    "TOPS" = getSMPlot(unemployed_df,input$range[1], input$range[2], "federal_tax_recovery", "Fed Tax Intercept $","50-state Comparison of Fed Tax Intercepts (Quarterly)", free_y, scale = 1/1000000, prefix = "$", suffix = "M"),
                    "recipRate" = getSMPlot(unemployed_df, input$range[1], input$range[2], "recipiency_annual_total", "Recipiency Rate", "50-state Comparison of UI Recipiency Rate", free_y),
                    "recipBreakdown" = getSMPlot(unemployed_df,input$range[1], input$range[2], "recipiency_annual_total", "Recipiency Rate", "50-state Comparison of UI Recipiency Rates", free_y),
                    "uirate" = getSMPlot(unemployed_df,input$range[1], input$range[2], "unemployment_rate_sa", "Unemployment Rate (Seasonally Adjusted)","50-state Comparison of SA Unemployment Rates", free_y),
                    "lowerAuthority" = getSMPlot(unemployed_df,input$range[1], input$range[2], "lower_Within45Days", "Proportion of Decisions Within 45 Days","50-state Comparison of First Level Appeal Decisions within 45 Days", free_y),
                    "firstPay" = getSMPlot(unemployed_df,input$range[1], input$range[2], "first_time_payment_Within35Days","Proportion of Payments Within 35 Days", "50-state Comparison of First Payments within 35 Days", free_y),
                    "higherAuthority" = getSMPlot(unemployed_df,input$range[1], input$range[2], "higher_Within75Days", "Proportion of Decisions Within 75 Days", "50-state Comparison of Second Level Appeal Decisions within 75 Days", free_y))
    
    return(smPlot)
  })

  # render the small multiple plot
  output$fiftyStatePlot <- renderPlot({
    fiftyStatePlot <- switch(input$viewData,
                     "monthlyUI" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "total_compensated_mov_avg", input$state, "Montly UI Payments",paste(input$state, "vs. US: Total Monthly UI Payments"), scale = 1/1000000, prefix = "", suffix = "M"),
                     "overvPayments" = get50StateComparisonPlot(unemployed_df, input$range[1], input$range[2], "outstanding_proportion", input$state, "Overpayment Balance/Annual UI Payments",paste(input$state, "vs. US: Outstanding Overpayment Balance as a Proportion of Total UI Paid Annually")),
                     "fraudvNon" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "fraud_num_percent", input$state, "Fraud/Non-Fraud",paste(input$state, "vs. US: Fraud / Non-Fraud UI Overpayemnts")),
                     "overvRecovery" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "outstanding", input$state, "Overpayment Balance",paste(input$state, "vs. US: Outstanding UI Overpayment Balance"), scale = 1/1000000, prefix = "$", suffix = "M"),
                     "nonMonDen" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "denial_rate_overall", input$state, "Non-Monetary Denial Rate",paste(input$state, "vs. US: Denial Rates for Non-Monetary Reasons")),
                     "nonMonSep" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "denial_sep_percent", input$state, "Proportion of all Non-Monetary Determinations",paste(input$state, "vs. US: Denials for Separation Reasons")),
                     "nonMonSepRate" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "denial_sep_rate", input$state, "Non-Monetary Separation Denial Rate",paste(input$state, "vs. US: Denial Rate for Separation Reasons")),
                     "nonMonNonSep" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "denial_non_percent", input$state, "Proportion of all Non-Monetary Determinations",paste(input$state, "vs. US: Denials for Non-Separation Reasons")),
                     "nonMonNonSepRate" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "denial_non_rate", input$state, "Non-Monetary Non-Separation Denial Rate",paste(input$state, "vs. US: Denial Rate for Non-Separation Reasons")),
                     "TOPS" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "federal_tax_recovery", input$state, "Fed Tax Intercept $",paste(input$state, "vs. US: Fed Tax Intercepts (Quarterly)"), scale = 1/1000000, prefix = "$", suffix = "M"),
                     "recipRate" = get50StateComparisonPlot(unemployed_df, input$range[1], input$range[2], "recipiency_annual_total", input$state, "Recipiency Rate", paste(input$state, "vs. US: UI Recipiency Rate")),
                     "recipBreakdown" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "recipiency_annual_total", input$state, "Recipiency Rate", paste(input$state, "vs. US: UI Recipiency Rates")),
                     "uirate" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "unemployment_rate_sa", input$state, "Unemployment Rate",paste(input$state, "vs. US: Seasonally Adjusted Unemployment Rates")),
                     "lowerAuthority" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "lower_Within45Days", input$state, "Proportion of Decisions Within 45 Days", paste(input$state, "vs. US: First Level Appeal Decisions within 45 Days")),
                     "firstPay" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "first_time_payment_Within35Days", input$state, "Proportion of Payments Within 35 Days", paste(input$state, "vs. US: First Payments within 35 Days")),
                     "higherAuthority" = get50StateComparisonPlot(unemployed_df,input$range[1], input$range[2], "higher_Within75Days", input$state, "Proportion of Decisions Within 75 Days", paste(input$state, "vs. US: Second Level Appeal Decisions within 75 Days")))
    return(fiftyStatePlot)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

