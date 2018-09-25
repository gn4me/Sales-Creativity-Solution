library(plotly)
output$analyticsTab <- renderUI(tabsetPanel(id="analyticsTabset"
                                            ,tabPanel("Quartely budgets",value = "quartelyBudgetTabPanel",fluidRow(column(12,div(
                                              fluidRow(column(1,h5("Year ",style="font-weight: 700;")),column(2,selectInput("quartelyBudgetYearInput",NULL,yearsLOV,selected = "quartelyBudgetYearInput"))
                                                       ,column(1,h5("Category: ",style="font-weight: 700;")),column(2,selectInput("quartelyBudgetReportCategory",NULL,dataLoadCategoriesLOV,selected = "quartelyBudgetReportCategory"))
                                                       ,column(1,h5("Operator: ",style="font-weight: 700;")),column(2,selectInput("quartelyBudgetOperatorName",NULL,dataLoadOperatorsLOV,selected=operatorNameLOV))
                                                       ,column(1,actionButton("showquartelyBudgetsBtn","Show")))
                                              ,class="well"))),
                                              fluidRow(uiOutput("headerUi")),
                                              fluidRow(column(6,tableOutput("quartelyBudgetTable"),uiOutput("afterShowUi"),uiOutput("forecastHeader"),tableOutput("forecastTable")),column(4,plotlyOutput("quartelyBudgetLinePlot"),plotlyOutput("quartelyBudgetPlot")))                                            )))
get_quarterly_budget_data <- function(year,operator_id,cat_id)
{
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  months <- dbSendQuery(jdbcConnection,paste0("SELECT SALES_YEAR, SALES_MONTH,MONTHLY_GN4ME_REVENUE AS REVENUE 
                                              FROM BI_SALES_MONTHLY_REVENUES where sales_year = '",year,"' 
                                              AND OPERATOR_ID = ",operator_id," AND MAIN_CATEGORY_ID=",cat_id," order by sales_month"))
  months <- dbFetch(months)
  if(nrow(months) > 0)
  {
    if(nrow(months) %% 3 != 0)
    {
      months <- months[1:(nrow(months)-(nrow(months) %% 3)),]
    }
    lookup <- data.frame("quarter"=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"),month=c("01","02","03","04","05","06","07","08","09","10","11","12"))
    months$Quarter <- lookup$quarter[match(months$SALES_MONTH, lookup$month)]
    months <- aggregate(months[c("REVENUE")], by=months[c("Quarter")], sum)
    quarters <- dbSendQuery(jdbcConnection,paste0("SELECT SALES_QUARTER,GN4ME_BUDGET FROM BI_SALES_Q_BUDGETS WHERE SALES_YEAR ='",year,"' AND MAIN_CATEGORY_ID =",cat_id," AND OPERATOR_ID = ",operator_id))
    quarters <- dbFetch(quarters)
    months$GN4ME_BUDGET <- quarters$GN4ME_BUDGET[match(months$Quarter,quarters$SALES_QUARTER)]
    months[is.na(months)] <- 0
    colnames(months) <- c("Quarter","Revenue","Budget")
    
  }
  else{
    months <- data.frame(Quarter=character(0),Revenue=numeric(0),Budget=numeric(0))
  }
  dbDisconnect(jdbcConnection)
  months
}
predictData <- data.frame()
observeEvent(input$showquartelyBudgetsBtn,{
  if(input$quartelyBudgetYearInput == "" || input$quartelyBudgetReportCategory == "" || input$quartelyBudgetOperatorName == "")
  {
    showNotification("Please fill in the fields.")
    return()
  }
  data <- get_quarterly_budget_data(input$quartelyBudgetYearInput,dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$quartelyBudgetOperatorName],dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$quartelyBudgetReportCategory])
  predictData <<- data[,1:2]
  output$headerUi <- renderUI(h4("Quarterly budget comparison ",style="font-weight: 700;text-align:center;color:white;"))
  p <- plot_ly(data,x=data$Quarter,y=~Revenue,type = 'bar',name='Revenue')%>% add_trace(y=~Budget,name='Budget') %>% layout(xaxis=list(title="Quarter"),yaxis=list(barmode="group",title="Total")) 
  q <- plotly_build(p)
  q$elementId <- NULL
  output$quartelyBudgetPlot <- renderPlotly(q)
  p2 <- plot_ly(data,x=data$Quarter,y=~Revenue,type = 'scatter', mode = 'lines',name='Revenue')%>% add_trace(y=~Budget,name='Budget') %>% layout(xaxis=list(title="Quarter"),yaxis=list(barmode="group",title="Total")) 
  q2 <- plotly_build(p2)
  q2$elementId <- NULL
  output$quartelyBudgetLinePlot <- renderPlotly(q2)
  #data <- DT::datatable(data,options = list(lengthMenu=sort(c(10,15,nrow(data)))))
  
  if(nrow(data) > 0)
  {
    data[,2]<-round(data[,2],digits=0)
    data$`ABS Error` <- NA
    data[,4] <- round(abs(data$Revenue - data$Budget),digits =0)
    data$`MAPS` <- NA
    data[,5] <- round(data[,4]/data[,3],digits=2)
    if(data[,5] == Inf)
    {
      drops <- c("MAPS")
      data <- data[ , !(names(data) %in% drops)]
      sums <- cbind("Total",as.data.frame(t(colSums(data[sapply(data, is.numeric)], na.rm = TRUE))))
      names(sums)<- names(data)
      data <- rbind(data,sums)
      data[,4] <- prettyNum(data[,4], big.mark=",")
      data[,2] <- prettyNum(data[,2], big.mark=",")
      data[,3] <- prettyNum(data[,3], big.mark=",")
    }
    else{
      quartersCount <- nrow(data)
      sums <- cbind("Total",as.data.frame(t(colSums(data[sapply(data, is.numeric)], na.rm = TRUE))))
      names(sums)<- names(data)
      data <- rbind(data,sums)
      data$Quarter <- as.character(data$Quarter)
      data <- rbind(data,c("Mean absolute","percent error"," "," ",round(data[quartersCount+1,5]/quartersCount,2)))
      
      data[,5] <- prettyNum(data[,5], big.mark=",")
      data[,4] <- prettyNum(data[,4], big.mark=",")
      data[,2] <- prettyNum(data[,2], big.mark=",")
      data[,3] <- prettyNum(data[,3], big.mark=",")
    }
  }
  output$forecastHeader <- renderUI(fluidRow())
  output$forecastTable <- renderUI(fluidRow())
  output$quartelyBudgetTable <- renderTable(data)
  output$afterShowUi <- renderUI(div(actionButton("predictDataButton","Predict")))
},ignoreNULL = TRUE,ignoreInit = TRUE)
observeEvent(input$predictDataButton,{
  if(nrow(predictData) == 2)
  {
    predictData$Cyclic <- c(" "," ")
    predictData$Revenue <- round(predictData$Revenue,0)
    predictData <- rbind(predictData,c("Q3"," ",round((predictData[1,2]+predictData[2,2])/2,digits = 0)))
    
    for(i in 2:ncol(predictData))
    {
      predictData[,i]<- prettyNum(predictData[,i],big.mark = ",")
    }
  }
  else if(nrow(predictData) == 3)
  {
    predictData$weights <- c(0.1,0.3,0.6)
    predictData$Cyclic <- c(" "," "," ")
    predictData$Random <- c(" "," "," ")
    predictData$Seasonal <- c(" "," "," ")
    predictData$Revenue <- round(predictData$Revenue,0)
    predictData <- rbind(predictData,c("Q4"," "," ",round((predictData[2,2]+predictData[3,2])/2,digits = 0),round(sum(predictData$Revenue)/3,digits = 0),round(sum(predictData$Revenue*predictData$weights),digits = 0)))
    drops <- c("weights")
    predictData <- predictData[ , !(names(predictData) %in% drops)]
    
    for(i in 2:ncol(predictData))
    {
      predictData[,i]<- prettyNum(predictData[,i],big.mark = ",")
    }
  }
  else{
    showNotification(paste("Prediction can't be done with only",nrow(predictData),"quarter."))
    return()
  }
  output$forecastHeader <- renderUI(div(style="margin-top:10px;",h5("Forecast Models",style="color:white;font-weight:700;")))
  output$forecastTable <- renderTable(predictData)
},ignoreNULL = TRUE,ignoreInit = TRUE)