
#load("DataSources_16Aug.RData")
#webshot::install_phantomjs()
#webshot::install_phantomjs()

library(shinydashboard)

library(shiny)
library(devtools)
library(xts)
library(dplyr)
library(googleVis)
library(tidyverse)
library(data.table)
library(xlsx)
library(dplyr)

start <- as.Date("2019-01-01")
end <- as.Date(Sys.Date())+1 #as.Date("2019-08-13")
v_target =-4
Portfolio_Stocks <- data.frame(value=c("LABU","SOXL","BABA","ROKU","AYX","AMZN","MDB","DRIP","ERY","SPPI"))
                        

if (!require("DescTools")) {
  install.packages("DescTools")
  library(DescTools)
}

if (!require("formattable")) {
  install.packages("formattable")
  library(formattable)
}

# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

if (!require("DT")) {
  install.packages("DT")
  library(DT)
}


f_stock <- function(symbol, start, end)
{ 
  status <- tryCatch(getSymbols(symbol, src = "yahoo", from = start, to = end, auto.assign = FALSE , verbose = TRUE),error = identity)
  v_status <- unlist(status)

  if ( any(v_status  %like%  "%Error%")   ) {
    mt_st = data.frame()
    return(mt_st)
  }
  
  mt_st <- status
  df_st <- as.data.frame(mt_st)
  df_st$Stock<- symbol
  df_st$Date<- rownames(df_st)
  rownames(df_st)<-NULL
  names(df_st)[1:6] <- c("Open","High","Low","Close","Volume","Adjusted")
  
  return(df_st)
}


f_stock_processing <- function(start, end, v_target,stocks)
{ 

  for (i in 1:length(stocks))
  { 
    df_st = f_stock(stocks[i], start ,end)
    if (i==1) 
    {
      df_stocks = df_st
    }
    df_stocks = union(df_st,df_stocks)
  }

return(df_stocks)

}


f_priordays <- function(n, DFStocks)
{
  
  ETFStocks_Dn <- data.frame(numeric(n))
  colnames(ETFStocks_Dn) <- c("Close")
  d1=as.data.frame(ETFStocks_Dn$Close)
  d2=as.data.frame(DFStocks$Close)
  colnames(d1) <- c("Close")
  colnames(d2) <- c("Close")
  ETFStocks_Dn= rbind(d1,d2 )
  
  ETFStocks_Dn <- as.data.frame(ETFStocks_Dn[-(   (nrow(ETFStocks_Dn)- (n - 1)):(nrow(ETFStocks_Dn)) ),])  
  colnames(ETFStocks_Dn) <- c(paste("ClosePrior",n , "D" ,sep=""))
  
  return(ETFStocks_Dn)
}

f_nextdays <- function(n, DFStocks)
{
  
  ETFStocks_Dn <- data.frame(numeric(n))
  colnames(ETFStocks_Dn) <- c("Close")
  d1=as.data.frame(ETFStocks_Dn$Close)
  d2=as.data.frame(DFStocks$Close)
  colnames(d1) <- c("Close")
  colnames(d2) <- c("Close")
  ETFStocks_Dn= rbind(d2, d1 )
  
  ETFStocks_Dn <- as.data.frame(ETFStocks_Dn[-((1):(n)),])  
  colnames(ETFStocks_Dn) <- c(paste("CloseNext",n , "D" ,sep=""))
  
  return(ETFStocks_Dn)
}




#   +========================+=============================+
#   | SECTION 40 scrapping                                 |
#   +========================+=============================+

# setwd("C:/Users/sjvd2/Downloads/phantomjs-2.1.1-windows/bin") 
# 
# # Let phantomJS scrape techstars, output is written to techstars.html
# system("./phantomjs scrape.js")
# 
# batches <- html("techstars.html")
# 
# class(batches)
# 
# library(httr)
# library(rvest)
# 
# batch_xc <- batches %>%
#   html_nodes(".show-td") %>%
#   html_text()
# 
# 
# xc = as.data.frame(batch_xc)
# xc
# 
# df <- data.frame()
# 
# for (i in  seq(6, length(batch_xc), 5)   ) 
# { 
#   print(i)
#   #print(xc[i,1,drop=FALSE])
#   df[i,1] = batch_xc[i]
#   
# }
# 
# df <- na.omit(df)
# rownames(df) <- NULL
# colnames(df) <- c("Stock")
# class(df)

#   +========================+=============================+
#   | END SECTION 40                                       |
#   +========================+=============================+


skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "black"


sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Opportunities", icon = icon("shopping-cart"), tabName = "opportunities"),
    selectizeInput(
      "Portfolio"
      , "Enter a List of Stocks"
      , choices = Portfolio_Stocks$value
      , multiple = TRUE
      , options = list(create = TRUE, closeAfterSelect = TRUE)
      ,selected = Portfolio_Stocks$value 
    ),
    actionButton(inputId="btn", label="Refresh")
  )
  
  
)

body <- dashboardBody(
  

    tabItems(

      tabItem("opportunities",
  
             tabBox( title = tagList(shiny::icon("search-dollar"), "Market Opportunities"),width = 12,
               tabPanel( strong("Best Stocks"),   
                         fluidRow(
                                 box(title="From the 100 Highest 52 Week Stock Returns",status="primary",solidHeader = TRUE,
                                     DT::dataTableOutput("chart2_n"),width=12)
                                )
  
               ),
               tabPanel(strong("Best ETFs"),   
                        fluidRow(
                        box(title="From the 100 Highest 52 Week ETF Returns",status="primary",solidHeader = TRUE,
                            DT::dataTableOutput("chart2_m"),width=12)
                               )
               ),
               tabPanel(strong("Popular Stocks"),   
                        fluidRow(
                        box(title="From the 100 Popular Stocks",status="primary",solidHeader = TRUE,
                              DT::dataTableOutput("chart2_p"),width=12)
                        )
               )
               
             )
  
             ),
  
    
    tabItem("dashboard",
            
        
            
            fluidRow(
              
              box(title="Today's Performance",status="primary",solidHeader = TRUE,
                  DT::dataTableOutput("chart2b"),width=12)

              
            ),
            

            fluidRow(
              
              box(title="Today's Performance (Size of the Bubble = Today's Return)",status="primary",solidHeader = TRUE,
                  htmlOutput("chart2"),width=6,height=710),
              
           
              box(title="Today's Performance (Size of the Bubble = Last 5 Day's Return)",status="primary",solidHeader = TRUE,
                  htmlOutput("chart2a"),width=6,height=710)
              
              
            )
    )
    
  )
)








notifications <- dropdownMenu(type = "notifications", badgeStatus = "info",
                              notificationItem(
                                text = "GitHub",
                                icon("github-alt"),
                                href="https://github.com/saulventura"
                              ),
                              notificationItem(
                                text = "Linkedin",
                                icon("linkedin"),
                                href="https://www.linkedin.com/in/saul-ventura/"
                              )
                              
)


header <- dashboardHeader(
  title = "Personal Finance Dashboard",
  notifications
)





# 
# 
# 
# 
# header <- dashboardHeader(
#   title = "Personal Finance Dashboard"
# )



ui <- dashboardPage(header, sidebar, body, skin = skin)



server <- function(input, output ) {
  

  
  output$chart2<- renderGvis({
    
    stocks = as.character(input$Portfolio)
    df_stocks = f_stock_processing(start, end, v_target,stocks)
    
   # df_stocks_Sub<-select(subset(df_stocks, date>=max(ga3$date)-as.numeric(input$radio)),-date)
    
    df_stocks_Sub_prior = df_stocks%>% 
      group_by(Stock) %>% #group on Stock
      arrange(Stock,Date) %>% #arrange in ascending order by Stock,Date
      filter(row_number() == n()-1) #take the second last row
    
    
    df_stocks_Sub_start = df_stocks%>% 
      group_by(Stock) %>%
      filter(Date == min(Date)) %>% arrange(Stock)
    
    df_stocks_Sub = df_stocks%>% 
      group_by(Stock) %>%
      filter(Date == max(Date)) %>% arrange(Stock)
    
    df_stocks_Sub_start<- subset(df_stocks_Sub_start, select =-c(Date,High,Low, Adjusted)  )
    df_stocks_Sub<- subset(df_stocks_Sub, select =-c(Date,High,Low, Adjusted)  )
    
    
    
    #by_stock <- group_by(df_stocks_Sub, Stock)
    #df_stocks_Sub<-summarise(by_stock, m(Open), mean(Close),sum(Volume))
    df_stocks_Sub$TodayReturn <- 100*(df_stocks_Sub$Close - df_stocks_Sub$Open)/df_stocks_Sub$Open
    df_stocks_Sub$DayReturn <- 100*(df_stocks_Sub$Close - df_stocks_Sub_prior$Close)/df_stocks_Sub_prior$Close
    df_stocks_Sub$YearReturn <- 100*(df_stocks_Sub$Close - df_stocks_Sub_start$Open)/df_stocks_Sub_start$Open
    df_stocks_Sub$Volume <- df_stocks_Sub$Volume/1000000
   # names(df_stocks_Sub)<-c("Open","Close","Volume","Stock","YearReturn")
    Bubble <- gvisBubbleChart(df_stocks_Sub, idvar="Stock", 
                              xvar="DayReturn", yvar="TodayReturn",
                              colorvar="Stock", sizevar="TodayReturn",
                              options=list( 
                                #title="Daily Performance (Year's Return = Size of the Bubble)",
                                vAxis="{title: 'Today Return'}",
                                hAxis="{title: 'Day Return'}",
                                hAxis="{minValue:75,  maxValue:125}",
                                 height=600,
                                legend = T))
    Bubble
  })
  
  
  
  output$chart2a<- renderGvis({
    
    stocks = as.character(input$Portfolio)
    df_stocks = f_stock_processing(start, end, v_target,stocks)
    
    
    df_stocks_Sub_prior = df_stocks%>% 
      group_by(Stock) %>% #group on Stock
      arrange(Stock,Date) %>% #arrange in ascending order by Stock,Date
      filter(row_number() == n()-1) #take the second last row
    
    
    df_stocks_Sub_last_five = df_stocks%>% 
      group_by(Stock) %>% 
      arrange(Stock,Date) %>% 
      filter(row_number() == n()-4 ) 
    
    df_stocks_Sub_start = df_stocks%>% 
      group_by(Stock) %>%
      filter(Date == min(Date)) %>% arrange(Stock)
    
    df_stocks_Sub = df_stocks%>% 
      group_by(Stock) %>%
      filter(Date == max(Date)) %>% arrange(Stock)
    
    df_stocks_Sub_start<- subset(df_stocks_Sub_start, select =-c(Date,High,Low, Adjusted)  )
    df_stocks_Sub<- subset(df_stocks_Sub, select =-c(Date,High,Low, Adjusted)  )
    
    
    
    #by_stock <- group_by(df_stocks_Sub, Stock)
    #df_stocks_Sub<-summarise(by_stock, m(Open), mean(Close),sum(Volume))
    df_stocks_Sub$TodayReturn <- 100*(df_stocks_Sub$Close - df_stocks_Sub$Open)/df_stocks_Sub$Open
    df_stocks_Sub$OneDayReturn <- 100*(df_stocks_Sub$Close - df_stocks_Sub_prior$Close)/df_stocks_Sub_prior$Close
    df_stocks_Sub$YearReturn <- 100*(df_stocks_Sub$Close - df_stocks_Sub_start$Open)/df_stocks_Sub_start$Open
    df_stocks_Sub$FiveDayReturn <- 100*(df_stocks_Sub$Close - df_stocks_Sub_last_five$Open)/df_stocks_Sub_last_five$Open
    df_stocks_Sub$Volume <- df_stocks_Sub$Volume/1000000

    Bubble <- gvisBubbleChart(df_stocks_Sub, idvar="Stock", 
                              xvar="FiveDayReturn", yvar="OneDayReturn",
                              colorvar="Stock", sizevar="FiveDayReturn",
                              options=list( 
                                #title="Daily Performance (Year's Return = Size of the Bubble)",
                                vAxis="{title: 'Day Return'}",
                                hAxis="{title: '5 Day Return'}",
                                hAxis="{minValue:-15,  maxValue:125}",
                                colorAxis="{colors: ['red', 'blue']}",
                                height=600,
                                legend = T))
    Bubble
  })
  
  
  
  observeEvent(input$btn, {
  
  output$chart2b<- DT::renderDataTable({
    
      stocks = as.character(input$Portfolio)
      df_stocks = f_stock_processing(start, end, v_target,stocks)
      
    df_stocks_Sub_prior = df_stocks%>% 
      group_by(Stock) %>% #group on Stock
      arrange(Stock,Date) %>% #arrange in ascending order by Stock,Date
      filter(row_number() == n()-1) #take the second last row
    
    
    df_stocks_Sub_last_five = df_stocks%>% 
      group_by(Stock) %>% 
      arrange(Stock,Date) %>% 
      filter(row_number() == n()-4 ) 
    
    df_stocks_Sub_start = df_stocks%>% 
      group_by(Stock) %>%
      filter(Date == min(Date)) %>% arrange(Stock)
    
    df_stocks_Sub = df_stocks%>% 
      group_by(Stock) %>%
      filter(Date == max(Date)) %>% arrange(Stock)
    
    df_stocks_Sub_start<- subset(df_stocks_Sub_start, select =-c(Date,High,Low, Adjusted)  )
    df_stocks_Sub<- subset(df_stocks_Sub, select =-c(Date,High,Low, Adjusted)  )
    
    
    
    #by_stock <- group_by(df_stocks_Sub, Stock)
    #df_stocks_Sub<-summarise(by_stock, m(Open), mean(Close),sum(Volume))
    df_stocks_Sub$TodayReturn <- 100*(df_stocks_Sub$Close - df_stocks_Sub$Open)/df_stocks_Sub$Open
    df_stocks_Sub$OneDayReturn <- 100*(df_stocks_Sub$Close - df_stocks_Sub_prior$Close)/df_stocks_Sub_prior$Close
    df_stocks_Sub$FiveDayReturn <- 100*(df_stocks_Sub$Close - df_stocks_Sub_last_five$Open)/df_stocks_Sub_last_five$Open
    df_stocks_Sub$YearReturn <- 100*(df_stocks_Sub$Close - df_stocks_Sub_start$Open)/df_stocks_Sub_start$Open
    df_stocks_Sub$Volume <- df_stocks_Sub$Volume/1000000
    
    
    
    df_stocks_Sub <- rapply(object = df_stocks_Sub, f = round, classes = "numeric", how = "replace", digits = 2) 
    
    
    as.datatable(  formattable(df_stocks_Sub,  list(
      Volume = color_tile("white", "violet"),
      grade = formatter("span", style = x ~ ifelse(x == "A", 
                                                   style(color = "green", font.weight = "bold"), NA)),
      area(col = c(YearReturn)) ~ normalize_bar("lightblue", 0.2) ,
      FiveDayReturn = formatter("span",
                                style = x ~ style(color = ifelse(rank(-x) <= 5, "green", ifelse(rank(-x) <= 10, "orange", "red")  )),
                                x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
      OneDayReturn = formatter("span",
                               style = x ~ style(color = ifelse(x<0, "red", "green")),
                               x ~ icontext(ifelse(x <0, "arrow-down", "arrow-up"), percent(x / 100)  ) ),
      TodayReturn = formatter("span",
                              style = x ~ style(color = ifelse(x<0, "red", "green")),
                              x ~ icontext(ifelse(x <0, "arrow-down", "arrow-up"), percent(x / 100)   ))
      
    
     
    )
    )
    
    ,options = list(  pageLength = 10,scroller = TRUE,
                      scrollX = TRUE ,columnDefs = list(list(className = 'dt-right', targets = c(0,1,2,3,5,6,7,8)  )))
    
    
    )
    
  })
  
  
  }, ignoreNULL = FALSE)
  
  
  output$chart2_m<- DT::renderDataTable({
    
    stocks_new = c('SOXL','TECL','NAIL','TQQQ','FNGU','DFEN','USD','RUSL','HOML','ROM','FAS','SPXL','UPRO','EXIV','DUSL','FINU','UBOT','FNGO','WANT','QLD','UDOW','NEED','DRN','FBGX','NUGT','FLGE','FRLG','UMDD','MIDU','UTSL','INR','KOLD','CHAU','CWEB','UXI','SMH','EURL','URTY','TNA','LMLP','XSD','SPUU','SSO','SOXX','LABU','TAN','UYG','CURE','FTXL','PBW','DZK','TPOR','WTIU','CNRG','OILU','PSI','URE','UGE','UWT','JNUG','URR','XLK','DPST','CHIK','DDM','FTEC','VGT','BBC','UCC','DGAZ','QTEC','IXN','CHIS','ACES','MVV','IYW','SVXY','PTF','QTUM','UCO','SNSR','CHIQ','PALL','UGLD','EDC','FIEE','UWM','JPNL','SMLL','FEUL','CNXT','GREK','QCLN','DWAQ','ROKT','UBIO','UPV','FFEU','FIYY','LBDC')
    
    df = data.frame()
    df = data.frame(matrix(unlist(stocks_new), nrow=length(stocks_new), byrow=T),stringsAsFactors=FALSE)
    #"LLQD",
    
    #   +========================+=============================+
    #   | SECTION 50 added to filter Stocks before processing  |
    #   +========================+=============================+
    
    
    
    ETFStocks_total = data.frame()
    
    for (i in 1:length(stocks_new))
    {
      #assign("last.warning", NULL, envir = baseenv())
      ETFStocks = f_stock(df[i,1], end-5 ,end)
      
      if ( sum(is.na(ETFStocks$Close))==0  ) {
        if (nrow(ETFStocks_total) ==0 ) {
          ETFStocks_total = ETFStocks
        }
        else {
          ETFStocks_total =rbind(ETFStocks_total,ETFStocks)
        }
      } 
      
      
    }
    
    
    df_stocks_Sub_prior = ETFStocks_total%>% 
      select(Stock,Close,Date) %>% 
      group_by(Stock) %>% #group on Stock
      arrange(Stock,Date) %>% #arrange in ascending order by Stock,Date
      filter(row_number() == n()-1) #take the second last row
    
    df_stocks_Sub = ETFStocks_total%>% 
      select(Stock,Close,Date) %>% 
      group_by(Stock) %>%
      filter(Date == max(Date)) %>% arrange(Stock) 
    
    
    allData = merge(df_stocks_Sub, df_stocks_Sub_prior , by.x = "Stock", by.y = "Stock")
    
    allData_target = allData$Stock[(100*(allData$Close.x - allData$Close.y)/allData$Close.y)< v_target]
    
    df = data.frame(matrix(unlist(allData_target), nrow=length(allData_target), byrow=T),stringsAsFactors=FALSE)
    
    #   +========================+=============================+
    #   | END SECTION 50                                       |
    #   +========================+=============================+
    
    
    #   +========================+=============================+
    #   | SECTION 60 main process                              |
    #   +========================+=============================+
    
    if (length(allData_target)>0)  
    {
    
    for (i in 1:length(allData_target))
    {
      
      ETFStocks = f_stock(df[i,1], start ,end)
      
      
      for (j in 1:5) 
      { 
        ETFStocks= cbind(ETFStocks,f_priordays(j,ETFStocks))
      }
      
      for (j in 1:5) 
      { 
        ETFStocks = cbind(ETFStocks,f_nextdays(j,ETFStocks))
      }
      
      
      ETFStocks$VarPrior1D =  100*(ETFStocks$Close - ETFStocks$ClosePrior1D)/ETFStocks$ClosePrior1D
      ETFStocks$VarPrior2D =  100*(ETFStocks$ClosePrior1D - ETFStocks$ClosePrior2D)/ETFStocks$ClosePrior2D
      ETFStocks$VarPrior3D =  100*(ETFStocks$ClosePrior2D - ETFStocks$ClosePrior3D)/ETFStocks$ClosePrior3D
      ETFStocks$VarPrior4D =  100*(ETFStocks$ClosePrior3D - ETFStocks$ClosePrior4D)/ETFStocks$ClosePrior4D
      ETFStocks$VarPrior5D =  100*(ETFStocks$ClosePrior4D - ETFStocks$ClosePrior5D)/ETFStocks$ClosePrior5D
      
      
      ETFStocks$RtnLast3D =  100*(ETFStocks$Close - ETFStocks$ClosePrior2D)/ETFStocks$ClosePrior2D
      ETFStocks$RtnLast5D =  100*(ETFStocks$Close - ETFStocks$ClosePrior4D)/ETFStocks$ClosePrior4D
      
      
      
      ETFStocks$VarNext1D[ETFStocks$Date <= end-1-1] =  100*(ETFStocks$CloseNext1D[ETFStocks$Date <= end-1-1] - ETFStocks$Close[ETFStocks$Date <= end-1-1])/ETFStocks$Close[ETFStocks$Date <= end-1-1]
      ETFStocks$VarNext2D[ETFStocks$Date <= end-1-2] =  100*(ETFStocks$CloseNext2D[ETFStocks$Date <= end-1-2] - ETFStocks$CloseNext1D[ETFStocks$Date <= end-1-2])/ETFStocks$CloseNext1D[ETFStocks$Date <= end-1-2]
      ETFStocks$VarNext3D[ETFStocks$Date <= end-1-3] =  100*(ETFStocks$CloseNext3D[ETFStocks$Date <= end-1-3] - ETFStocks$CloseNext2D[ETFStocks$Date <= end-1-3])/ETFStocks$CloseNext2D[ETFStocks$Date <= end-1-3]
      ETFStocks$VarNext4D[ETFStocks$Date <= end-1-4] =  100*(ETFStocks$CloseNext4D[ETFStocks$Date <= end-1-4] - ETFStocks$CloseNext3D[ETFStocks$Date <= end-1-4])/ETFStocks$CloseNext3D[ETFStocks$Date <= end-1-4]
      ETFStocks$VarNext5D[ETFStocks$Date <= end-1-5] =  100*(ETFStocks$CloseNext5D[ETFStocks$Date <= end-1-5] - ETFStocks$CloseNext4D[ETFStocks$Date <= end-1-5])/ETFStocks$CloseNext4D[ETFStocks$Date <= end-1-5]
      
      ETFStocks$RtnNext1D[ETFStocks$Date <= end-1-1]  =  100*(ETFStocks$CloseNext1D[ETFStocks$Date <= end-1-1] - ETFStocks$Close[ETFStocks$Date <= end-1-1])/ETFStocks$Close[ETFStocks$Date <= end-1-1]
      ETFStocks$RtnNext2D[ETFStocks$Date <= end-1-2]  =  100*(ETFStocks$CloseNext2D[ETFStocks$Date <= end-1-2] - ETFStocks$Close[ETFStocks$Date <= end-1-2])/ETFStocks$Close[ETFStocks$Date <= end-1-2]
      ETFStocks$RtnNext3D[ETFStocks$Date <= end-1-3]  =  100*(ETFStocks$CloseNext3D[ETFStocks$Date <= end-1-3] - ETFStocks$Close[ETFStocks$Date <= end-1-3])/ETFStocks$Close[ETFStocks$Date <= end-1-3]
      ETFStocks$RtnNext5D[ETFStocks$Date <= end-1-5] =   100*(ETFStocks$CloseNext5D[ETFStocks$Date <= end-1-5] - ETFStocks$Close[ETFStocks$Date <= end-1-5])/ETFStocks$Close[ETFStocks$Date <= end-1-5]
      
      
      if (i==1) {
        ETFStocks_total = ETFStocks
      } else 
      {
        ETFStocks_total =rbind(ETFStocks_total,ETFStocks)
      }
      
      
      ETFStocks_losser_days = ETFStocks %>% slice(-n())
      
      ETFStocks_losser_days = subset(ETFStocks_losser_days, VarPrior1D< v_target )
      ETFStocks_bounce_days = subset(ETFStocks_losser_days, VarNext1D > 0 )
      
      if (nrow(ETFStocks_losser_days)>0) {
        success_rate = nrow(ETFStocks_bounce_days)/nrow(ETFStocks_losser_days)
        
        AvgRtnNext1D  = mean(ETFStocks_losser_days$RtnNext1D, na.rm = TRUE)
        AvgRtnNext2D  = mean(ETFStocks_losser_days$RtnNext2D, na.rm = TRUE)
        AvgRtnNext3D  = mean(ETFStocks_losser_days$RtnNext3D, na.rm = TRUE)
        AvgRtnNext5D  = mean(ETFStocks_losser_days$RtnNext5D, na.rm = TRUE)
        
      } else 
      {
        success_rate = 0
      }
      
      df[i,2] = success_rate
      df[i,3] = nrow(ETFStocks_losser_days)
      df[i,4] = nrow(ETFStocks_bounce_days)
      df[i,5] = AvgRtnNext1D
      df[i,6] = AvgRtnNext2D
      df[i,7] = AvgRtnNext3D
      df[i,8] = AvgRtnNext5D
    }
    
    
  } else {
    
    
    df <- data.frame(matrix(ncol = 8, nrow = 0))
    ETFStocks_total$VarPrior1D = 0
    
  }
  
  
  
  
    names(df)[1:8] <- c("Stock","SuccessRate","LosserDays","BounceDays","AvgRtnNext1D","AvgRtnNext2D","AvgRtnNext3D","AvgRtnNext5D")
    df = arrange(df,-SuccessRate)
    
    
    ETFStocks_total = ETFStocks_total%>% 
      group_by(Stock) %>%
      filter(Date == max(Date)) %>% arrange(Stock)
    
    
    ETFStocks_total = subset(ETFStocks_total, VarPrior1D< v_target )
    
    ETFStocks_total = merge(ETFStocks_total, df , by.x = "Stock", by.y = "Stock")
    
    #   +========================+=============================+
    #   | END SECTION 60                                       |
    #   +========================+=============================+
    
    
    
    ETFStocks_total <- rapply(object = ETFStocks_total, f = round, classes = "numeric", how = "replace", digits = 2) 
    
    cols_remove <- c("Date","Adjusted","CloseNext1D", "CloseNext2D","CloseNext3D", "CloseNext4D","CloseNext5D","VarNext1D","VarNext2D","VarNext3D","VarNext4D","VarNext5D","RtnNext1D","RtnNext2D","RtnNext3D","RtnNext5D")
    ETFStocks_total  = ETFStocks_total[, !(colnames(ETFStocks_total) %in% cols_remove)]
    
    
    
    
    datatable(  ETFStocks_total ,
                options = list(columnDefs = 
                                 list(list(className = 'dt-right', targets = c(2:24)  )),
                               
                               pageLength = 15,scroller = TRUE,
                               scrollX = TRUE  ) 
    )%>%
      formatStyle(c("VarPrior2D","VarPrior3D","VarPrior4D","VarPrior5D"), backgroundColor = "#d9fcf4",color = styleInterval(cuts = c(0, 3),  values = c("red", "green", "blue"))) %>%
      formatStyle("VarPrior1D", backgroundColor = "#dbfcd9",color = styleInterval(cuts = c(0, 3),  values = c("red", "green", "blue"))) %>%
      formatStyle(c("RtnLast3D","RtnLast5D"), backgroundColor = "#d9e9fc",color = styleInterval(cuts = c(0, 3),  values = c("red", "green", "blue"))) %>%
      formatStyle(c("LosserDays","BounceDays"), backgroundColor = "#ebd9fc") %>%
      formatStyle("SuccessRate", backgroundColor = "#ebd9fc",color = styleInterval(cuts = c(0.5, 0.7),  values = c("red", "green", "blue"))) %>%
      formatStyle(c("AvgRtnNext1D","AvgRtnNext2D","AvgRtnNext3D","AvgRtnNext5D"), backgroundColor = "#fcf4d9",color = styleInterval(cuts = c(0, 3),  values = c("red", "green", "blue")))
    
    
  })
  
  
  
  
  
  
  output$chart2_n<- DT::renderDataTable({
    
    
    stocks_new = c("CYRXW","AXSM",	"ENPH",	"EVER",	"REKR",	"CVRS",	"ENLV",	"ROKU",	"PAYS",	"ADVM",	"VFF",	"FLGT",	"INS",	"APPS",	"ZYXI",	"DYAI",	"SMSI",	"REFR",	"SE",	"EIDX",	"CPRX",	"CVM",	"HYGS",	"TLRA",	"NVCR",	"TNAV",	"GSB",	"EHTH",	"AVP",	"LSCC",	"AVLR",	"ARWR",	"PAGS",	"GH",	"RUBI",	"SHOP",	"IIPR",	"PERI",	"SEDG",	"ARCE",	"IOTS",	"NVTA",	"PRO",	"AXNX",	"OLED",	"SNDX",	"ATLC",	"ISSC",	"AYX",	"CYTK",	"PLAN",	"COUP",	"CYRX",	"JYNT",	"SHAK",	"COKE",	"BAND",	"RDVT",		"MODN",	"NEO",	"NSSC",	"IPHI",	"OKTA",	"ASFI",	"FTDR",	"SLP",	"DRD",	"HEI",	"BRID",	"CCC",	"SBGL",	"SILV",	"LUNA",	"MKTX",	"BLL",	"NG",	"ZYME",	"AG",	"KL",	"TTEC",	"ELP",	"BRFS",	"HMY",	"CHMA",	"QIWI",	"AU",	"RARX",	"REPH",	"SBS",	"EBR",	"CZZ",	"GFI",	"SBUX",		"AZUL",	"GOL",	"APYX")
    
    stocks_new = c('AXSM','KRMD','EVER','KOD','RLMD','CDLX','ADVM','MDCO','SAVA','HEBT','APLT','RCEL','ENPH','EIDX','STXS','SBGL','ARWR','SSI','ZYME','BASI','PRVB','VERU','TNK','AIRI','CNST','IDN','ROKU','AGRX','ATLC','AVP','APPS','BLDP','DRRX','CUE','PESI','ASPN','CVM','ARQL','EPZM','SVM','VIPS','LPG','SHOP','XPEL','DRD','SEAC','PLMR','NVRO','AMD','PFNX','TLRA','SEDG','MODN','SE','CCC.W','CZZ','CVNA','INFU','FPAY','ARDX','UCTT','LEU','JCS','MTEM','ARVN','FRTA','UROV','GSX','ATEC','CRUS','KPTI','PERI','ELLO','REPH','TPTX','SPAR','MTNB','ZEAL','FRO','BOOT','OESX','CGEN','RDVT','COUP','AUDC','IPHI','MAXR','GDS','NG','REKR','CMRE','LGL','INFN','SILV','SAFE','AUPH','STM','KRYS','XBIT','XOMA')
    
    #"CYRXW",
    # stocks_new = c("VFF")
    df = data.frame()
    df = data.frame(matrix(unlist(stocks_new), nrow=length(stocks_new), byrow=T),stringsAsFactors=FALSE)
    
    
    #   +========================+=============================+
    #   | SECTION 50 added to filter Stocks before processing  |
    #   +========================+=============================+
    
    
    
    ETFStocks_total = data.frame()
    
    for (i in 1:length(stocks_new))
    {
      #assign("last.warning", NULL, envir = baseenv())
      ETFStocks = f_stock(df[i,1], end-5 ,end)
      
      if ( sum(is.na(ETFStocks$Close))==0  ) {
        if (nrow(ETFStocks_total) ==0 ) {
          ETFStocks_total = ETFStocks
        }
        else {
          ETFStocks_total =rbind(ETFStocks_total,ETFStocks)
        }
      } 
      
      
    }
    
    
    df_stocks_Sub_prior = ETFStocks_total%>% 
      select(Stock,Close,Date) %>% 
      group_by(Stock) %>% #group on Stock
      arrange(Stock,Date) %>% #arrange in ascending order by Stock,Date
      filter(row_number() == n()-1) #take the second last row
    
    df_stocks_Sub = ETFStocks_total%>% 
      select(Stock,Close,Date) %>% 
      group_by(Stock) %>%
      filter(Date == max(Date)) %>% arrange(Stock) 
    
    
    allData = merge(df_stocks_Sub, df_stocks_Sub_prior , by.x = "Stock", by.y = "Stock")
    
    allData_target = allData$Stock[(100*(allData$Close.x - allData$Close.y)/allData$Close.y)< v_target]
    
    df = data.frame(matrix(unlist(allData_target), nrow=length(allData_target), byrow=T),stringsAsFactors=FALSE)
    
    #   +========================+=============================+
    #   | END SECTION 50                                       |
    #   +========================+=============================+
    
    
    #   +========================+=============================+
    #   | SECTION 60 main process                              |
    #   +========================+=============================+
    
    if (length(allData_target)>0)  
    {
    
    for (i in 1:length(allData_target))
    {
      
      ETFStocks = f_stock(df[i,1], start ,end)
      
      
      for (j in 1:5) 
      { 
        ETFStocks= cbind(ETFStocks,f_priordays(j,ETFStocks))
      }
      
      for (j in 1:5) 
      { 
        ETFStocks = cbind(ETFStocks,f_nextdays(j,ETFStocks))
      }
      
      
      ETFStocks$VarPrior1D =  100*(ETFStocks$Close - ETFStocks$ClosePrior1D)/ETFStocks$ClosePrior1D
      ETFStocks$VarPrior2D =  100*(ETFStocks$ClosePrior1D - ETFStocks$ClosePrior2D)/ETFStocks$ClosePrior2D
      ETFStocks$VarPrior3D =  100*(ETFStocks$ClosePrior2D - ETFStocks$ClosePrior3D)/ETFStocks$ClosePrior3D
      ETFStocks$VarPrior4D =  100*(ETFStocks$ClosePrior3D - ETFStocks$ClosePrior4D)/ETFStocks$ClosePrior4D
      ETFStocks$VarPrior5D =  100*(ETFStocks$ClosePrior4D - ETFStocks$ClosePrior5D)/ETFStocks$ClosePrior5D
      
      
      ETFStocks$RtnLast3D =  100*(ETFStocks$Close - ETFStocks$ClosePrior2D)/ETFStocks$ClosePrior2D
      ETFStocks$RtnLast5D =  100*(ETFStocks$Close - ETFStocks$ClosePrior4D)/ETFStocks$ClosePrior4D
      
      
      
      ETFStocks$VarNext1D[ETFStocks$Date <= end-1-1] =  100*(ETFStocks$CloseNext1D[ETFStocks$Date <= end-1-1] - ETFStocks$Close[ETFStocks$Date <= end-1-1])/ETFStocks$Close[ETFStocks$Date <= end-1-1]
      ETFStocks$VarNext2D[ETFStocks$Date <= end-1-2] =  100*(ETFStocks$CloseNext2D[ETFStocks$Date <= end-1-2] - ETFStocks$CloseNext1D[ETFStocks$Date <= end-1-2])/ETFStocks$CloseNext1D[ETFStocks$Date <= end-1-2]
      ETFStocks$VarNext3D[ETFStocks$Date <= end-1-3] =  100*(ETFStocks$CloseNext3D[ETFStocks$Date <= end-1-3] - ETFStocks$CloseNext2D[ETFStocks$Date <= end-1-3])/ETFStocks$CloseNext2D[ETFStocks$Date <= end-1-3]
      ETFStocks$VarNext4D[ETFStocks$Date <= end-1-4] =  100*(ETFStocks$CloseNext4D[ETFStocks$Date <= end-1-4] - ETFStocks$CloseNext3D[ETFStocks$Date <= end-1-4])/ETFStocks$CloseNext3D[ETFStocks$Date <= end-1-4]
      ETFStocks$VarNext5D[ETFStocks$Date <= end-1-5] =  100*(ETFStocks$CloseNext5D[ETFStocks$Date <= end-1-5] - ETFStocks$CloseNext4D[ETFStocks$Date <= end-1-5])/ETFStocks$CloseNext4D[ETFStocks$Date <= end-1-5]
      
      ETFStocks$RtnNext1D[ETFStocks$Date <= end-1-1]  =  100*(ETFStocks$CloseNext1D[ETFStocks$Date <= end-1-1] - ETFStocks$Close[ETFStocks$Date <= end-1-1])/ETFStocks$Close[ETFStocks$Date <= end-1-1]
      ETFStocks$RtnNext2D[ETFStocks$Date <= end-1-2]  =  100*(ETFStocks$CloseNext2D[ETFStocks$Date <= end-1-2] - ETFStocks$Close[ETFStocks$Date <= end-1-2])/ETFStocks$Close[ETFStocks$Date <= end-1-2]
      ETFStocks$RtnNext3D[ETFStocks$Date <= end-1-3]  =  100*(ETFStocks$CloseNext3D[ETFStocks$Date <= end-1-3] - ETFStocks$Close[ETFStocks$Date <= end-1-3])/ETFStocks$Close[ETFStocks$Date <= end-1-3]
      ETFStocks$RtnNext5D[ETFStocks$Date <= end-1-5] =   100*(ETFStocks$CloseNext5D[ETFStocks$Date <= end-1-5] - ETFStocks$Close[ETFStocks$Date <= end-1-5])/ETFStocks$Close[ETFStocks$Date <= end-1-5]
      
      
      if (i==1) {
        ETFStocks_total = ETFStocks
      } else 
      {
        ETFStocks_total =rbind(ETFStocks_total,ETFStocks)
      }
      
      
      ETFStocks_losser_days = ETFStocks %>% slice(-n())
      
      ETFStocks_losser_days = subset(ETFStocks_losser_days, VarPrior1D< v_target )
      ETFStocks_bounce_days = subset(ETFStocks_losser_days, VarNext1D > 0 )
      
      if (nrow(ETFStocks_losser_days)>0) {
        success_rate = nrow(ETFStocks_bounce_days)/nrow(ETFStocks_losser_days)
        
        AvgRtnNext1D  = mean(ETFStocks_losser_days$RtnNext1D, na.rm = TRUE)
        AvgRtnNext2D  = mean(ETFStocks_losser_days$RtnNext2D, na.rm = TRUE)
        AvgRtnNext3D  = mean(ETFStocks_losser_days$RtnNext3D, na.rm = TRUE)
        AvgRtnNext5D  = mean(ETFStocks_losser_days$RtnNext5D, na.rm = TRUE)
        
      } else 
      {
        success_rate = 0
      }
      
      df[i,2] = success_rate
      df[i,3] = nrow(ETFStocks_losser_days)
      df[i,4] = nrow(ETFStocks_bounce_days)
      df[i,5] = AvgRtnNext1D
      df[i,6] = AvgRtnNext2D
      df[i,7] = AvgRtnNext3D
      df[i,8] = AvgRtnNext5D
    }
    
    
  } else {
    
    
    df <- data.frame(matrix(ncol = 8, nrow = 0))
    ETFStocks_total$VarPrior1D = 0
    
  }
  
    names(df)[1:8] <- c("Stock","SuccessRate","LosserDays","BounceDays","AvgRtnNext1D","AvgRtnNext2D","AvgRtnNext3D","AvgRtnNext5D")
    
    
    df = arrange(df,-SuccessRate)
    
    
    ETFStocks_total = ETFStocks_total%>% 
      group_by(Stock) %>%
      filter(Date == max(Date)) %>% arrange(Stock)
    
    
    ETFStocks_total = subset(ETFStocks_total, VarPrior1D< v_target )
    
    ETFStocks_total = merge(ETFStocks_total, df , by.x = "Stock", by.y = "Stock")
    
    #   +========================+=============================+
    #   | END SECTION 60                                       |
    #   +========================+=============================+
    
    
    ETFStocks_total <- rapply(object = ETFStocks_total, f = round, classes = "numeric", how = "replace", digits = 2) 
    
    cols_remove <- c("Date","Adjusted","CloseNext1D", "CloseNext2D","CloseNext3D", "CloseNext4D","CloseNext5D","VarNext1D","VarNext2D","VarNext3D","VarNext4D","VarNext5D","RtnNext1D","RtnNext2D","RtnNext3D","RtnNext5D")
    ETFStocks_total  = ETFStocks_total[, !(colnames(ETFStocks_total) %in% cols_remove)]
    
    
    

    datatable(  ETFStocks_total ,
                options = list(columnDefs = 
                  list(list(className = 'dt-right', targets = c(2:24)  )),
                  
                  pageLength = 15,scroller = TRUE,
                                 scrollX = TRUE  ) 
                )%>%
      formatStyle(c("VarPrior2D","VarPrior3D","VarPrior4D","VarPrior5D"), backgroundColor = "#d9fcf4",color = styleInterval(cuts = c(0, 3),  values = c("red", "green", "blue"))) %>%
      formatStyle("VarPrior1D", backgroundColor = "#dbfcd9",color = styleInterval(cuts = c(0, 3),  values = c("red", "green", "blue"))) %>%
      formatStyle(c("RtnLast3D","RtnLast5D"), backgroundColor = "#d9e9fc",color = styleInterval(cuts = c(0, 3),  values = c("red", "green", "blue"))) %>%
      formatStyle(c("LosserDays","BounceDays"), backgroundColor = "#ebd9fc") %>%
      formatStyle("SuccessRate", backgroundColor = "#ebd9fc",color = styleInterval(cuts = c(0.5, 0.7),  values = c("red", "green", "blue"))) %>%
      formatStyle(c("AvgRtnNext1D","AvgRtnNext2D","AvgRtnNext3D","AvgRtnNext5D"), backgroundColor = "#fcf4d9",color = styleInterval(cuts = c(0, 3),  values = c("red", "green", "blue")))
    
    
  })
  
  
  
  
  ###############################################
  ###############################################
  ###############################################
  ###############################################
  # regular stocks
  ###############################################
  ###############################################
  ###############################################
  ###############################################
  
  
  output$chart2_p<- DT::renderDataTable({
    
    
    stocks_new = c("LABU",	"AMZN",	"SOXL",	"TECL",	"UBER",	"AMD",	"MRVL",	"FB",	"NFLX",	"DIS",	"BYND",	"CLDR",	"GOL",	"NVDA",	"AMAT",	"TLND",	"MU",	"TSLA",	"BABA",	"TTWO",	"PYPL",	"ABBV",	"INFI",	"MSFT",	"UDOW",	"FAS",	"ATVI",	"IRBT",	"AAPL",	"RCL",	"INTC",	"AAOI",	"AEIS",		"FDX",	"MCHP",	"GM",	"ON",	"LNTH",	"SPPI",	"ARKW",	"CRM",	"ORCL",	"NI",	"ROM",	"MTUM",	"CY",	"CVX",	"PEP",	"BAC",	"MDB",	"AVGO",	"F",	"GE",	"SKYY",	"QTEC",	"SNSR",	"ONEQ",	"QQQ",	"GAMR",	"XNTK",	"CSCO",	"ADSK",	"ROST",	"IGV",	"VDC",	"NEM",	"PSI",	"KO",		"GOOGL",	"UPS",	"WDC",	"BWA",	"MCD",	"PWR",	"PDP",	"IBM",	"UDR",	"VMW",	"GPRO",	"CBS",	"KIE",	"IAK",	"QCLN",	"V",	"HD",	"PG",	"AXP",	"TRV",	"WMT",	"CMG",	"BLL",	"TSN",	"SNPS",	"LDOS",		"KLAC",	"TGT",	"MELI",	"CDNS",	"CTAS",	"IDXX",	"LULU",	"LRCX",	"SBUX","DRIP","SPPI","SAGE","ERY","PTON")

      
    # stocks_new = c("VFF")
    df = data.frame()
    df = data.frame(matrix(unlist(stocks_new), nrow=length(stocks_new), byrow=T),stringsAsFactors=FALSE)
    
    
    
    #   +========================+=============================+
    #   | SECTION 50 added to filter Stocks before processing  |
    #   +========================+=============================+
    
    
    
    ETFStocks_total = data.frame()
    
    for (i in 1:length(stocks_new))
    {
      #assign("last.warning", NULL, envir = baseenv())
      ETFStocks = f_stock(df[i,1], end-5 ,end)
      
      if ( sum(is.na(ETFStocks$Close))==0  ) {
        if (nrow(ETFStocks_total) ==0 ) {
          ETFStocks_total = ETFStocks
        }
        else {
          ETFStocks_total =rbind(ETFStocks_total,ETFStocks)
        }
      } 
      
      
    }
    
    
    df_stocks_Sub_prior = ETFStocks_total%>% 
      select(Stock,Close,Date) %>% 
      group_by(Stock) %>% #group on Stock
      arrange(Stock,Date) %>% #arrange in ascending order by Stock,Date
      filter(row_number() == n()-1) #take the second last row
    
    df_stocks_Sub = ETFStocks_total%>% 
      select(Stock,Close,Date) %>% 
      group_by(Stock) %>%
      filter(Date == max(Date)) %>% arrange(Stock) 
    
    
    allData = merge(df_stocks_Sub, df_stocks_Sub_prior , by.x = "Stock", by.y = "Stock")
    
    allData_target = allData$Stock[(100*(allData$Close.x - allData$Close.y)/allData$Close.y)< v_target]
    
    df = data.frame(matrix(unlist(allData_target), nrow=length(allData_target), byrow=T),stringsAsFactors=FALSE)
    
    #   +========================+=============================+
    #   | END SECTION 50                                       |
    #   +========================+=============================+
    
    
    #   +========================+=============================+
    #   | SECTION 60 main process                              |
    #   +========================+=============================+
    
    if (length(allData_target)>0)  
    {
      
      for (i in 1:length(allData_target))
      {
        
        ETFStocks = f_stock(df[i,1], start ,end)
        
        
        for (j in 1:5) 
        { 
          ETFStocks= cbind(ETFStocks,f_priordays(j,ETFStocks))
        }
        
        for (j in 1:5) 
        { 
          ETFStocks = cbind(ETFStocks,f_nextdays(j,ETFStocks))
        }
        
        
        ETFStocks$VarPrior1D =  100*(ETFStocks$Close - ETFStocks$ClosePrior1D)/ETFStocks$ClosePrior1D
        ETFStocks$VarPrior2D =  100*(ETFStocks$ClosePrior1D - ETFStocks$ClosePrior2D)/ETFStocks$ClosePrior2D
        ETFStocks$VarPrior3D =  100*(ETFStocks$ClosePrior2D - ETFStocks$ClosePrior3D)/ETFStocks$ClosePrior3D
        ETFStocks$VarPrior4D =  100*(ETFStocks$ClosePrior3D - ETFStocks$ClosePrior4D)/ETFStocks$ClosePrior4D
        ETFStocks$VarPrior5D =  100*(ETFStocks$ClosePrior4D - ETFStocks$ClosePrior5D)/ETFStocks$ClosePrior5D
        
        
        ETFStocks$RtnLast3D =  100*(ETFStocks$Close - ETFStocks$ClosePrior2D)/ETFStocks$ClosePrior2D
        ETFStocks$RtnLast5D =  100*(ETFStocks$Close - ETFStocks$ClosePrior4D)/ETFStocks$ClosePrior4D
        
        
        
        ETFStocks$VarNext1D[ETFStocks$Date <= end-1-1] =  100*(ETFStocks$CloseNext1D[ETFStocks$Date <= end-1-1] - ETFStocks$Close[ETFStocks$Date <= end-1-1])/ETFStocks$Close[ETFStocks$Date <= end-1-1]
        ETFStocks$VarNext2D[ETFStocks$Date <= end-1-2] =  100*(ETFStocks$CloseNext2D[ETFStocks$Date <= end-1-2] - ETFStocks$CloseNext1D[ETFStocks$Date <= end-1-2])/ETFStocks$CloseNext1D[ETFStocks$Date <= end-1-2]
        ETFStocks$VarNext3D[ETFStocks$Date <= end-1-3] =  100*(ETFStocks$CloseNext3D[ETFStocks$Date <= end-1-3] - ETFStocks$CloseNext2D[ETFStocks$Date <= end-1-3])/ETFStocks$CloseNext2D[ETFStocks$Date <= end-1-3]
        ETFStocks$VarNext4D[ETFStocks$Date <= end-1-4] =  100*(ETFStocks$CloseNext4D[ETFStocks$Date <= end-1-4] - ETFStocks$CloseNext3D[ETFStocks$Date <= end-1-4])/ETFStocks$CloseNext3D[ETFStocks$Date <= end-1-4]
        ETFStocks$VarNext5D[ETFStocks$Date <= end-1-5] =  100*(ETFStocks$CloseNext5D[ETFStocks$Date <= end-1-5] - ETFStocks$CloseNext4D[ETFStocks$Date <= end-1-5])/ETFStocks$CloseNext4D[ETFStocks$Date <= end-1-5]
        
        ETFStocks$RtnNext1D[ETFStocks$Date <= end-1-1]  =  100*(ETFStocks$CloseNext1D[ETFStocks$Date <= end-1-1] - ETFStocks$Close[ETFStocks$Date <= end-1-1])/ETFStocks$Close[ETFStocks$Date <= end-1-1]
        ETFStocks$RtnNext2D[ETFStocks$Date <= end-1-2]  =  100*(ETFStocks$CloseNext2D[ETFStocks$Date <= end-1-2] - ETFStocks$Close[ETFStocks$Date <= end-1-2])/ETFStocks$Close[ETFStocks$Date <= end-1-2]
        ETFStocks$RtnNext3D[ETFStocks$Date <= end-1-3]  =  100*(ETFStocks$CloseNext3D[ETFStocks$Date <= end-1-3] - ETFStocks$Close[ETFStocks$Date <= end-1-3])/ETFStocks$Close[ETFStocks$Date <= end-1-3]
        ETFStocks$RtnNext5D[ETFStocks$Date <= end-1-5] =   100*(ETFStocks$CloseNext5D[ETFStocks$Date <= end-1-5] - ETFStocks$Close[ETFStocks$Date <= end-1-5])/ETFStocks$Close[ETFStocks$Date <= end-1-5]
        
        
        if (i==1) {
          ETFStocks_total = ETFStocks
        } else 
        {
          ETFStocks_total =rbind(ETFStocks_total,ETFStocks)
        }
        
        
        ETFStocks_losser_days = ETFStocks %>% slice(-n())
        
        ETFStocks_losser_days = subset(ETFStocks_losser_days, VarPrior1D< v_target )
        ETFStocks_bounce_days = subset(ETFStocks_losser_days, VarNext1D > 0 )
        
        if (nrow(ETFStocks_losser_days)>0) {
          success_rate = nrow(ETFStocks_bounce_days)/nrow(ETFStocks_losser_days)
          
          AvgRtnNext1D  = mean(ETFStocks_losser_days$RtnNext1D, na.rm = TRUE)
          AvgRtnNext2D  = mean(ETFStocks_losser_days$RtnNext2D, na.rm = TRUE)
          AvgRtnNext3D  = mean(ETFStocks_losser_days$RtnNext3D, na.rm = TRUE)
          AvgRtnNext5D  = mean(ETFStocks_losser_days$RtnNext5D, na.rm = TRUE)
          
        } else 
        {
          success_rate = 0
        }
        
        df[i,2] = success_rate
        df[i,3] = nrow(ETFStocks_losser_days)
        df[i,4] = nrow(ETFStocks_bounce_days)
        df[i,5] = AvgRtnNext1D
        df[i,6] = AvgRtnNext2D
        df[i,7] = AvgRtnNext3D
        df[i,8] = AvgRtnNext5D
      }
      
      
    } else {
      
      
      df <- data.frame(matrix(ncol = 8, nrow = 0))
      ETFStocks_total$VarPrior1D = 0
      
    }
    
    
    
    
    
    names(df)[1:8] <- c("Stock","SuccessRate","LosserDays","BounceDays","AvgRtnNext1D","AvgRtnNext2D","AvgRtnNext3D","AvgRtnNext5D")
    
    
    df = arrange(df,-SuccessRate)
    
    
    ETFStocks_total = ETFStocks_total%>% 
      group_by(Stock) %>%
      filter(Date == max(Date)) %>% arrange(Stock)
    
    
    ETFStocks_total = subset(ETFStocks_total, VarPrior1D< v_target )
    
    ETFStocks_total = merge(ETFStocks_total, df , by.x = "Stock", by.y = "Stock")
    
    #   +========================+=============================+
    #   | END SECTION 60                                       |
    #   +========================+=============================+
    
    
    ETFStocks_total <- rapply(object = ETFStocks_total, f = round, classes = "numeric", how = "replace", digits = 2) 
    
    cols_remove <- c("Date","Adjusted","CloseNext1D", "CloseNext2D","CloseNext3D", "CloseNext4D","CloseNext5D","VarNext1D","VarNext2D","VarNext3D","VarNext4D","VarNext5D","RtnNext1D","RtnNext2D","RtnNext3D","RtnNext5D")
    ETFStocks_total  = ETFStocks_total[, !(colnames(ETFStocks_total) %in% cols_remove)]
    
    
    
    
    datatable(  ETFStocks_total ,
                options = list(columnDefs = 
                                 list(list(className = 'dt-right', targets = c(2:24)  )),
                               
                               pageLength = 15,scroller = TRUE,
                               scrollX = TRUE  ) 
    )%>%
      formatStyle(c("VarPrior2D","VarPrior3D","VarPrior4D","VarPrior5D"), backgroundColor = "#d9fcf4",color = styleInterval(cuts = c(0, 3),  values = c("red", "green", "blue"))) %>%
      formatStyle("VarPrior1D", backgroundColor = "#dbfcd9",color = styleInterval(cuts = c(0, 3),  values = c("red", "green", "blue"))) %>%
      formatStyle(c("RtnLast3D","RtnLast5D"), backgroundColor = "#d9e9fc",color = styleInterval(cuts = c(0, 3),  values = c("red", "green", "blue"))) %>%
      formatStyle(c("LosserDays","BounceDays"), backgroundColor = "#ebd9fc") %>%
      formatStyle("SuccessRate", backgroundColor = "#ebd9fc",color = styleInterval(cuts = c(0.5, 0.7),  values = c("red", "green", "blue"))) %>%
      formatStyle(c("AvgRtnNext1D","AvgRtnNext2D","AvgRtnNext3D","AvgRtnNext5D"), backgroundColor = "#fcf4d9",color = styleInterval(cuts = c(0, 3),  values = c("red", "green", "blue")))
    
    
  })
  
  
  
}

shinyApp(ui, server)
