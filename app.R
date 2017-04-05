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
library(reshape2)
source("unemploymentDataProcessor.R")

maxDate <- max(ucFirstTimePaymentLapse$rptdate)
minDate <- min(ucRecipiency$rptdate)
states <- getStates(ucbrTimeliness)

reportTheme <- theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5, size=20),
        legend.position="top",
        legend.title = element_blank(),
        axis.title = element_text(size=17, face="bold"),
        axis.text = element_text(size=17))



# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Unemployment Insurance Data Explorer"),
  
  # Sidebar with a file input 
  sidebarLayout(
    sidebarPanel(
      selectInput("state", 
                  label = "Choose a state",
                  choices = states,
                  selected = "PA"),
    
      sliderInput("range", 
                  label = "Years to View:",
                  min = minDate, max = maxDate, value = c(maxDate - 3650, maxDate), timeFormat="%Y-%m"),
      
      selectInput("viewData",
                  label = 'Select Data to View',
                  size=10, selectize=FALSE,
                  choices = c("Monthly UI Payments", "Timeliness of Referee Decisions", "Timeliness of UCSC Payments", "Timeliness of UCBR Decisions", "Recipiency Rates", "Overpayment vs Recovery", "Overpayment Balance/Annual UI Payments", "Fraud vs Non Fraud Overpayents", "Tax Program Overpayment Recoveries"),
                  selected = "Monthly UI Payments"),
      
      checkboxInput("constant_y_axis", 
                    label="Constant y axis? (makes comparisons easier)",
                    value=TRUE),
      
      tags$img(src="https://clsphila.org/sites/all/themes/clsphila_base/images/logo-50.svg")
      
    , width=3),
    # Show something
    mainPanel(
      tabsetPanel(
        tabPanel("Data",
          plotOutput("uiplot"),
          downloadButton('data.csv', 'Download Data'),
          dataTableOutput("uidata")
        ),
        tabPanel("Map",leafletOutput("uimap"))
      )
    )
  ),
  fluidRow(
    hr(),
    HTML("This page is maintained by <a href=mailto:mhollander@clsphila.org>Michael Hollander</a> of <a href='clsphila.org' target=_blank>Community Legal Services</a>.  You can find the code for this page on github here:<a href='https://github.com/CLSPhila/ui-data-explorer' target=_blank>Github</a>.  All of the data for this website comes from <a href=https://ows.doleta.gov/unemploy/DataDownloads.asp target=_blank>the US Department of Labor</a>.")
    
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # render the plot
  output$uiplot <- renderPlot({

    if (input$viewData == "Monthly UI Payments")
    {
      
      ucMelt <- melt(subset(ucRecipiency, st==input$state & rptdate > input$range[1]-10 & rptdate < input$range[2]+10, select=c("rptdate","total_compensated_mov_avg", "total_state_compensated_mov_avg")) ,id.vars="rptdate")
                                                                                                                                  
      
      uPlot = ggplot(ucMelt) +
        geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.3) +
        geom_ribbon(data=ucMelt[ucMelt$variable=="total_compensated_mov_avg",],aes(x=rptdate, ymin=0, ymax=value/1000000, fill=variable), alpha=.9)+
        geom_ribbon(data=ucMelt[ucMelt$variable=="total_state_compensated_mov_avg",], aes(x=rptdate, ymin=0, ymax=value/1000000, fill=variable), alpha=.9)+
        geom_line(aes(rptdate, value/1000000, col=variable)) +
        scale_color_discrete(guide=FALSE) +
        reportTheme + 
        labs(x="Date", y="Total Paid (Millions of $)",
             caption="12-month moving average of UI paid per month in both regular and federal UI programs.  \nNote that 'regular UI' includes state UI, UFCE, and UCX.  Federal programs include EB, and the various EUC programs that have been enacted.") + 
        ggtitle(paste(input$state, input$viewData, "from", format.Date(input$range[1],"%Y-%m"), "to", format.Date(input$range[2],"%Y-%m"))) +
        scale_fill_brewer(palette="Set1",
                          breaks=c("total_state_compensated_mov_avg","total_compensated_mov_avg"),
                          labels=c("Regular Programs","Federal Programs"))
      
      maxPlot <- maxUIPayments/1000000
      
    }
    
    else if (input$viewData == "Overpayment Balance/Annual UI Payments")
    {
      ucMelt <- melt(subset(ucOverpayments, st==input$state & rptdate > input$range[1]-10 & rptdate < input$range[2]+10, select=c("rptdate","outstanding_proportion")) ,id.vars="rptdate")
      
      
      uPlot = ggplot(ucMelt) +
        geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.3) +
        geom_point(aes(rptdate, value, col=variable), alpha=.9) +
        reportTheme + 
        stat_smooth(span=.3, aes(rptdate, value, col=variable)) + 
        labs(x="Date", 
             caption="Outstanding overpayment balance divided by the total benefits paid in all federal and state programs over the last 12 months.\n Data courtesy of the USDOL.  Reports used are ETA 227 and 5159, found at https://ows.doleta.gov/unemploy/DataDownloads.asp.") + 
        ggtitle(paste(input$viewData, "from", format.Date(input$range[1],"%Y-%m"), "to", format.Date(input$range[2],"%Y-%m"))) + 
        scale_fill_brewer(palette="Set1")
      
      maxPlot <- maxOutstandingProportion
      
    }

    else if (input$viewData == "Fraud vs Non Fraud Overpayents")
    {
      ucMelt <- melt(subset(ucOverpayments, st==input$state & rptdate > input$range[1]-10 & rptdate < input$range[2]+10, select=c("rptdate","fraud_num_percent")) ,id.vars="rptdate")
      
      
      
      uPlot = ggplot(ucMelt) +
        geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.3) +
        geom_point(aes(rptdate, value, col=variable), alpha=.9) +
        reportTheme + 
        stat_smooth(span=.3, aes(rptdate, value, col=variable)) + 
        labs(x="Date", 
             caption="Data courtesy of the USDOL.  Report used is ETA 227, found at https://ows.doleta.gov/unemploy/DataDownloads.asp.") + 
        ggtitle(paste(input$viewData, "from", format.Date(input$range[1],"%Y-%m"), "to", format.Date(input$range[2],"%Y-%m"))) + 
        scale_fill_brewer(palette="Set1")
      
      maxPlot <- 1
    }
    
    else if (input$viewData == "Tax Program Overpayment Recoveries")
    {
      ucMelt <- melt(subset(ucOverpayments, st==input$state & rptdate > input$range[1]-10 & rptdate < input$range[2]+10, select=c("rptdate","state_tax_recovery", "federal_tax_recovery")) ,id.vars="rptdate")
      
        uPlot = ggplot(ucMelt) +
        geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.3) +
        geom_point(aes(rptdate, value/1000000, col=variable), alpha=.9) +
        reportTheme + 
        stat_smooth(span=.3, aes(rptdate, value/1000000, col=variable)) + 
        labs(x="Date", 
             y="Total Overpayment Recovery (millions of $)",
             caption="Data courtesy of the USDOL.  Report used is ETA 227, found at https://ows.doleta.gov/unemploy/DataDownloads.asp.") + 
        scale_y_continuous(labels=scales::dollar) + 
        ggtitle(paste(input$viewData, "from", format.Date(input$range[1],"%Y-%m"), "to", format.Date(input$range[2],"%Y-%m"))) + 
        scale_fill_brewer(palette="Set1")
      
      maxPlot <- maxOverpaymentDollars/1000000
      
    }
    else if (input$viewData == "Recipiency Rates")
    {
     
     
      ucMelt <- melt(subset(ucRecipiency, st==input$state & rptdate > input$range[1]-10 & rptdate < input$range[2]+10, select=c("rptdate","recipiency_annual_reg","recipiency_annual_total")) ,id.vars="rptdate")
       
       
       
      uPlot = ggplot(ucMelt) +
        geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.3) +
        geom_ribbon(data=ucMelt[ucMelt$variable=="recipiency_annual_total",],aes(x=rptdate, ymin=0, ymax=value, fill=variable), alpha=.9)+
         geom_ribbon(data=ucMelt[ucMelt$variable=="recipiency_annual_reg",], aes(x=rptdate, ymin=0, ymax=value, fill=variable), alpha=.9)+
         geom_line(aes(rptdate, value, col=variable)) +
         scale_color_discrete(guide=FALSE) +
         reportTheme + 
         labs(x="Date", y="Recipiency Rate",
              caption="Recipiency rate calculated by dividing 12 month moving average of unemployment continuing claims divided by 12 month moving average of total unemployed.\nData not seasonally adjusted.  \nSource: Continuing claims can be found in ETA report 5159, found here: https://ows.doleta.gov/unemploy/DataDownloads.asp.\nUnemployed numbers courtesy the BLS: https://www.bls.gov/web/laus/ststdnsadata.txt.  \nNote that 'regular UI' includes state UI, UFCE, and UCX.  Federal programs include EB, and the various EUC programs that have been enacted.") + 
         ggtitle(paste(input$state, input$viewData, "from", format.Date(input$range[1],"%Y-%m"), "to", format.Date(input$range[2],"%Y-%m"))) +
         scale_fill_brewer(palette="Set1",
                           breaks=c("recipiency_annual_reg","recipiency_annual_total"),
                           labels=c("Regular Programs","Federal Programs"))
      
      maxPlot <- 1
    }
    
    else if (input$viewData == "Overpayment vs Recovery")
    {
      
      ucMelt <- melt(subset(ucOverpayments, st==input$state & rptdate > input$range[1]-10 & rptdate < input$range[2]+10, select=c("rptdate","outstanding", "recovered")) ,id.vars="rptdate")
      
      uPlot = ggplot(ucMelt) +
        geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.3) +
        geom_point(aes(rptdate, value/1000000, col=variable), alpha=.9) +
        reportTheme + 
        stat_smooth(span=.3, aes(rptdate, value/1000000, col=variable)) + 
        labs(x="Date", 
             y="Millions of $",
             caption="Data courtesy of the USDOL.  Report used is ETA 227, found at https://ows.doleta.gov/unemploy/DataDownloads.asp.") + 
        ggtitle(paste("Outstanding Overpayments vs $ Recovered from", format.Date(input$range[1],"%Y-%m"), "to", format.Date(input$range[2],"%Y-%m"))) + 
        scale_fill_brewer(palette="Set1")
      
      maxPlot <- maxOutstandingOverpayment/1000000
      
    }
    else
    {
      uiTable <- switch(input$viewData,
                        "Timeliness of Referee Decisions" = refereeTimeliness,
                        "Timeliness of UCSC Payments" = paymentTimeliness,
                        "Timeliness of UCBR Decisions" = ucbrTimeliness)
  
      plotCols <- switch(input$viewData,
                        "Timeliness of Referee Decisions" = c("rptdate", "Within30Days", "Within45Days"),
                        "Timeliness of UCSC Payments" = c("rptdate","Within15Days","Within35Days"),
                        "Timeliness of UCBR Decisions" = c("rptdate","Within45Days","Within75Days"))
  
      line1 <- switch(input$viewData,
                         "Timeliness of Referee Decisions" = c(.6,"30-day threshold"),
                         "Timeliness of UCSC Payments" = c(.87,"15-day threshold"),
                         "Timeliness of UCBR Decisions" = c(.4,"45-day threshold"))
      
      
      
      line2 <- switch(input$viewData,
                      "Timeliness of Referee Decisions" = c(.8,"45-day threshold"),
                      "Timeliness of UCSC Payments" = c(.93,"35-day threshold"),
                      "Timeliness of UCBR Decisions" = c(.8,"75-day threshold"))
      
      uiTable <- melt(subset(uiTable, st==input$state & rptdate > input$range[1]-10 & rptdate < input$range[2]+10, select=plotCols) ,id.vars="rptdate")
  
      # loess smoothing; .3 chosen arbitrarily to make the line a bit more responsive to data points.  
      uPlot <- ggplot(uiTable) +
        geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.3) +
        geom_point(aes(rptdate, value, col=variable), alpha=.9) +
        reportTheme + 
        stat_smooth(span=.3, aes(rptdate, value, col=variable)) + 
        labs(x="Date", 
             caption="Data courtesy of the USDOL.  Reports used are ETA 5130, 9050, 9054, and 9055.  \nAll can be found at https://ows.doleta.gov/unemploy/DataDownloads.asp.") + 
        geom_hline(aes(yintercept=as.numeric(line1[1])), linetype="dashed") +
        geom_text(aes(x=as.Date(input$range[1]),y=as.numeric(line1[1]),label = line1[2], vjust = -1, hjust=0), color="black") +
        geom_hline(aes(yintercept=as.numeric(line2[1])), linetype="dashed") +
        geom_text(aes(x=as.Date(input$range[1]),y=as.numeric(line2[1]),label = line2[2], vjust = -1, hjust=0), color="black") +
        ggtitle(paste(input$viewData, "from", format.Date(input$range[1],"%Y-%m"), "to", format.Date(input$range[2],"%Y-%m"))) + 
        scale_fill_brewer(palette="Set1")
      
      maxPlot <- 1

    }
    
    # constant scaling on the y axis for easier state comparison; scaling on x axis so we can scale to ranges with no recession
    if (input$constant_y_axis)
    {
      uPlot <- uPlot + coord_cartesian(ylim=c(0,maxPlot),
                                       xlim=c(as.Date(input$range[1]),as.Date(input$range[2])))
    }
    else
    {
      uPlot <- uPlot + coord_cartesian(xlim=c(as.Date(input$range[1]),as.Date(input$range[2])))
    }
    
    return(uPlot)
  })
  
  # render the data table
  output$uidata <- renderDataTable({
    
    
    if (input$viewData == "Monthly UI Payments")
    {
      uiDT <- DT::datatable(ucRecipiency[ucRecipiency$st==input$state & ucRecipiency$rptdate > input$range[1]-1 & ucRecipiency$rptdate < input$range[2]+1,
                                           c("st","rptdate", "total_state_compensated_mov_avg", "total_federal_compensated_mov_avg", "total_state_compensated", "total_federal_compensated", "total_paid_annual_mov_avg")], 
                            options=list(
                              pageLength = 12,
                              lengthMenu = list(c(12, 24, 48, -1),c("12", "24", "48", 'All')),
                              order = list(1,'desc'),
                              searching=FALSE
                            ), 
                            colnames=c("State","Report Date", "State UI Payments (Monthly, mov avg)", "Federal UI Payments (Monthly, mov avg)", "State UI Payments (Monthly)", "Federal UI Payments (Monthly)", "Annual UI Payments (mov avg)"),
                            class="stripe",
                            rownames=FALSE
      )%>% formatCurrency(3:7, '$')
      
    }
    
    else if (input$viewData == "Overpayment Balance/Annual UI Payments")
    {
      
      uiDT <- DT::datatable(ucOverpayments[ucOverpayments$st==input$state & ucOverpayments$rptdate > input$range[1]-1 & ucOverpayments$rptdate < input$range[2]+1,
                                         c("st","rptdate", "outstanding_proportion", "outstanding", "total_paid_annual_mov_avg")], 
                            options=list(
                              pageLength = 12,
                              lengthMenu = list(c(12, 24, 48, -1),c("12", "24", "48", 'All')),
                              order = list(1,'desc'),
                              searching=FALSE
                            ), 
                            colnames=c("State","Report Date", "Outstanding balance / Annual UI Payments", "Outstanding Overpayment Balance", "Annual UI Payments"),
                            class="stripe",
                            rownames=FALSE
      )%>% formatCurrency(4:5, '$')
    }
    
    else if (input$viewData == "Fraud vs Non Fraud Overpayents")
    {
      uiDT <- DT::datatable(ucOverpayments[ucOverpayments$st==input$state & ucOverpayments$rptdate > input$range[1]-1 & ucOverpayments$rptdate < input$range[2]+1,
                                         c("st","rptdate", "fraud_num_percent", "regular_fraud_num", "federal_fraud_num","regular_nonfraud_num","federal_nonfraud_num")],
                            options=list(
                              pageLength = 12,
                              lengthMenu = list(c(12, 24, 48, -1),c("12", "24", "48", 'All')),
                              order = list(1,'desc'),
                              searching=FALSE
                            ), 
                            colnames=c("State","Report Date", "Fraud as % of Total Overpayments", "Regular UI Fraud", "Federal UI Fraud", "Regular UI Non-Fraud", "Federal UI Non-Fraud"),
                            class="nowrap stripe",
                            rownames=FALSE
      )
      
    }

    else if (input$viewData == "Tax Program Overpayment Recoveries")
    {
      uiDT <- DT::datatable(ucOverpayments[ucOverpayments$st==input$state & ucOverpayments$rptdate > input$range[1]-1 & ucOverpayments$rptdate < input$range[2]+1,
                                           c("st","rptdate", "state_tax_recovery", "federal_tax_recovery")],
                            options=list(
                              pageLength = 12,
                              lengthMenu = list(c(12, 24, 48, -1),c("12", "24", "48", 'All')),
                              order = list(1,'desc'),
                              searching=FALSE
                            ), 
                            colnames=c("State","Report Date", "State Tax Recovery", "Federal Tax Recovery"),
                            class="nowrap stripe",
                            rownames=FALSE
      ) %>% formatCurrency(3:4, '$')
    }
    
    else if (input$viewData == "Overpayment vs Recovery")
    {
      
      uiDT <- DT::datatable(ucOverpayments[ucOverpayments$st==input$state & ucOverpayments$rptdate > input$range[1]-1 & ucOverpayments$rptdate < input$range[2]+1,
                                           c("st","rptdate", "outstanding", "recovered")],
                            options=list(
                              pageLength = 12,
                              lengthMenu = list(c(12, 24, 48, -1),c("12", "24", "48", 'All')),
                              order = list(1,'desc'),
                              searching=FALSE
                            ), 
                            colnames=c("State","Report Date", "Outstanding Owed", "Recovered"),
                            class="nowrap stripe",
                            rownames=FALSE
      ) %>% formatCurrency(3:4, '$')
      
    }
    else if (input$viewData == "Recipiency Rates")
    {
        uiDT <- DT::datatable(ucRecipiency[ucRecipiency$st==input$state & ucRecipiency$rptdate > input$range[1]-1 & ucRecipiency$rptdate < input$range[2]+1,
                                           c("st","rptdate","recipiency_annual_reg","recipiency_annual_total")],
                            options=list(
                              pageLength = 12,
                              lengthMenu = list(c(12, 24, 48, -1),c("12", "24", "48", 'All')),
                              order = list(1,'desc'),
                              searching=FALSE
                            ), 
                            colnames=c("State","Report Date", "Regular Programs", "Regular + Federal Programs"),
                            class="nowrap stripe",
                            rownames=FALSE
      )
      
    }
    else 
    {
      uiTable <- switch(input$viewData,
                        "Timeliness of Referee Decisions" = refereeTimeliness,
                        "Timeliness of UCSC Payments" = paymentTimeliness,
                        "Timeliness of UCBR Decisions" = ucbrTimeliness)
      
      uiCols <- switch(input$viewData,
                        "Timeliness of Referee Decisions" = c("State", "Date", "Within 30 Days", "Within 45 Days", "Number Filed", "Number Decided", "Number Pending", "US 30 Day Avg" ,"US 45 Day Avg"),
                        "Timeliness of UCSC Payments" = c("State", "Date", "Within 15 Days", "Within 35 Days", "Total Paid", "US 15 Day Avg", "US 35 Day Avg"),
                        "Timeliness of UCBR Decisions" = c("State", "Date", "Within 45 Days", "Within 75 Days", "Number Filed", "Number Decided", "Number Pending", "US 45 Day Avg", "US 75 Day Avg"))
      
      thresholds <- switch(input$viewData,
                           "Timeliness of Referee Decisions" = c(.6,.8),
                           "Timeliness of UCSC Payments" = c(.87,.93),
                           "Timeliness of UCBR Decisions" = c(.4,.8)) 
                           
      
      
                        
      # only return the data from the requested state and within the selected input range
      uiDT <- DT::datatable(uiTable[uiTable$st==input$state & uiTable$rptdate > input$range[1]-1 & uiTable$rptdate < input$range[2]+1,],
                            options=list(
                                          pageLength = 12,
                                          lengthMenu = list(c(12, 24, 48, -1),c("12", "24", "48", 'All')),
                                          order = list(1,'desc'),
                                          searching=FALSE,
                                          rowCallback = DT::JS(
                                            paste('function (row,data) { if(parseFloat(data[2]) < ', as.numeric(thresholds[1]), ') { $("td:eq(2)",row).css("color","red").css("font-weight", "bold"); } if(parseFloat(data[3]) <', as.numeric(thresholds[2]),  ') { $("td:eq(3)",row).css("color","red").css("font-weight", "bold"); }  }')
                          
                                          )
                            ), 
                            colnames=uiCols,
                            class="nowrap stripe",
                            rownames=FALSE
      )
    }
    return(uiDT)
  })

  output$data.csv = downloadHandler('data.csv', content = function(file) { 
        df <- switch(input$viewData,
                        "Monthly UI Payments" = ucRecipiency[ucRecipiency$st==input$state & ucRecipiency$rptdate > input$range[1]-1 & ucRecipiency$rptdate < input$range[2]+1,
                                                             c("st","rptdate", "total_state_compensated_mov_avg", "total_federal_compensated_mov_avg", "total_state_compensated", "total_federal_compensated", "total_paid_annual_mov_avg")],
                        "Overpayment Balance/Annual UI Payments" = ucOverpayments[ucOverpayments$st==input$state & ucOverpayments$rptdate > input$range[1]-1 & ucOverpayments$rptdate < input$range[2]+1,
                                                          c("st","rptdate", "outstanding_proportion", "outstanding", "total_paid_annual_mov_avg")],
                        "Timeliness of Referee Decisions" = refereeTimeliness[refereeTimeliness$st==input$state & refereeTimeliness$rptdate > input$range[1]-1 & refereeTimeliness$rptdate < input$range[2]+1,],
                        "Timeliness of UCSC Payments" = paymentTimeliness[paymentTimeliness$st==input$state & paymentTimeliness$rptdate > input$range[1]-1 & paymentTimeliness$rptdate < input$range[2]+1,],
                        "Timeliness of UCBR Decisions" = ucbrTimeliness[ucbrTimeliness$st==input$state & ucbrTimeliness$rptdate > input$range[1]-1 & ucbrTimeliness$rptdate < input$range[2]+1,], 
                        "Fraud vs Non Fraud Overpayents" = ucOverpayments[ucOverpayments$st==input$state & ucOverpayments$rptdate > input$range[1]-1 & ucOverpayments$rptdate < input$range[2]+1,
                                                                          c("st","rptdate", "fraud_num_percent", "regular_fraud_num", "federal_fraud_num","regular_nonfraud_num","federal_nonfraud_num")],
                        "Tax Program Overpayment Recoveries" = ucOverpayments[ucOverpayments$st==input$state & ucOverpayments$rptdate > input$range[1]-1 & ucOverpayments$rptdate < input$range[2]+1, c("st","rptdate", "state_tax_recovery", "federal_tax_recovery")],
                        "Overpayment vs Recovery" = ucOverpayments[ucOverpayments$st==input$state & ucOverpayments$rptdate > input$range[1]-1 & ucOverpayments$rptdate < input$range[2]+1, c("st","rptdate", "outstanding", "recovered")],
                        "Recipiency Rates" = ucRecipiency[ucRecipiency$st==input$state & ucRecipiency$rptdate > input$range[1]-1 & ucRecipiency$rptdate < input$range[2]+1, c("st","rptdate","recipiency_annual_reg","recipiency_annual_total")]
                    ) 
        

        write.csv(df[order(-rptdate),], file)
  })
    
  # render the proper leaflet map
  output$uimap <- renderLeaflet({
    
    uiMap <- switch(input$viewData,
                    "Monthly UI Payments" = getUIMap(usa,ucRecipiency,input$range[2],"total_compensated_mov_avg", paste("Total UI Payments disbursed in ", input$range[2]), FALSE),
                    "Overpayment Balance/Annual UI Payments" = getUIMap(usa,ucOverpayments, input$range[2], "outstanding_proportion", paste("Outstanding Overpayment Balance as a Proportion of Total UI Paid Annually in ", input$range[2]), FALSE),
                    "Fraud vs Non Fraud Overpayents" = getUIMap(usa,ucOverpayments,input$range[2],"fraud_num_percent", paste("Fraud vs Non-Fraud Overpayments in ",input$range[2]),FALSE),
                    "Overpayment vs Recovery" = getUIMap(usa,ucOverpayments,input$range[2],"outstanding", paste("Outstanding Overpayments Balance in ",input$range[2]),FALSE),
                    "Tax Program Overpayment Recoveries" = getUIMap(usa,ucOverpayments,input$range[2],"federal_tax_recovery", paste("Federal Tax Intercepts in Quarter ending ",input$range[2]), FALSE),
                    "Recipiency Rates" = getUIMap(usa,ucRecipiency,input$range[2],"recipiency_annual_total", paste("Recipiency Rate (State+Federal) in ",input$range[2]), TRUE),
                    "Timeliness of Referee Decisions" = getUIMap(usa,refereeTimeliness,input$range[2],"Within45Days", paste("Percent of Referee Decisions within 45 days, ",input$range[2]), TRUE),
                    "Timeliness of UCSC Payments" = getUIMap(usa,paymentTimeliness,input$range[2],"Within35Days", paste("Percent of First Payments within 35 days, ",input$range[2]), TRUE),
                    "Timeliness of UCBR Decisions" = getUIMap(usa,ucbrTimeliness,input$range[2],"Within75Days", paste("Percent of Board of Review Decisions within 75 days, ",input$range[2]), TRUE))
      
    return(uiMap)
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

