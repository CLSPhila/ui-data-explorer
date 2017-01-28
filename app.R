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

maxDate <- max(ucRecipiency$rptdate)
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
  titlePanel("Unemployment Insurance Administration Explorer"),
  
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
                  choices = c("Timeliness of Referee Decisions", "Timeliness of UCSC Payments", "Timeliness of UCBR Decisions", "Recipiency Rates"),
                  selected = "Timeliness of Referee Decisions")
      
    , width=3),
    # Show something
    mainPanel(
      plotOutput("uiplot"),
      dataTableOutput("uidata")
    )
  ),
  fluidRow(
    hr(),
    HTML("This page is maintained by <a href=mailto:mhollander@clsphila.org>Michael Hollander</a> of <a href='clsphila.org' target=_blank>Community Legal Services</a>.  You can find the code for this page on github here: .  All of the data for this website comes from <a href=https://ows.doleta.gov/unemploy/DataDownloads.asp target=_blank>the US Department of Labor</a>.")
    
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # render the plot
  output$uiplot <- renderPlot({
    if (input$viewData == "Recipiency Rates")
    {
     
     
      ucMelt <- melt(subset(ucRecipiency, st==input$state & rptdate > input$range[1]-10 & rptdate < input$range[2]+10, select=c("rptdate","recipiency_annual_reg","recipiency_annual_total")) ,id.vars="rptdate")
       
       
       
      uPlot = ggplot(ucMelt) +
         geom_ribbon(data=ucMelt[ucMelt$variable=="recipiency_annual_total",],aes(x=rptdate, ymin=0, ymax=value, fill=variable))+
         geom_ribbon(data=ucMelt[ucMelt$variable=="recipiency_annual_reg",], aes(x=rptdate, ymin=0, ymax=value, fill=variable))+
         geom_line(aes(rptdate, value, col=variable)) +
         scale_fill_discrete(breaks=c("recipiency_annual_reg","recipiency_annual_total"),
                              labels=c("Regular Programs","Federal Programs")) +
         scale_color_discrete(guide=FALSE) +
         reportTheme + 
         labs(x="Date", y="Recipiency Rate") + 
         geom_rect(data=recessions.df[recessions.df$Peak > as.Date(input$range[1]),], aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
         ggtitle(paste(input$state, input$viewData, "from", format.Date(input$range[1],"%Y-%m"), "to", format.Date(input$range[2],"%Y-%m")))
      
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
        geom_point(aes(rptdate, value, col=variable)) +
        reportTheme + 
        stat_smooth(span=.3, aes(rptdate, value, col=variable)) + 
        labs(x="Date") + 
        geom_hline(aes(yintercept=as.numeric(line1[1])), linetype="dashed") +
        geom_text(aes(x=as.Date(input$range[1]),y=as.numeric(line1[1]),label = line1[2], vjust = -1, hjust=0), color="black") +
        geom_hline(aes(yintercept=as.numeric(line2[1])), linetype="dashed") +
        geom_text(aes(x=as.Date(input$range[1]),y=as.numeric(line2[1]),label = line2[2], vjust = -1, hjust=0), color="black") +
        geom_rect(data=recessions.df[recessions.df$Peak > as.Date(input$range[1]),], aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
        ggtitle(paste(input$viewData, "from", format.Date(input$range[1],"%Y-%m"), "to", format.Date(input$range[2],"%Y-%m")))

    }
    
    return(uPlot)
  })
  
  # render the data table
  output$uidata <- renderDataTable({
    
    if (input$viewData == "Recipiency Rates")
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
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

