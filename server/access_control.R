accessControlLOV <- data.frame()
dataAccessCategories <- data.frame()
dataAccessOperators <- data.frame()

output$accessControlTab2 <- renderUI(tabsetPanel(id="ACTabsetPanel"
       ,tabPanel("Per Operator",value = "ACPerOperatorTabPanel",
                 fluidRow(column(11,div(fluidRow(column(1,h5("Category: ",style="font-weight: 700;")),column(3,selectInput("AccessControlCategory",NULL,dataAccessCategories,choices = dataAccessCategories$Categories))
                                                 ,column(1,h5("Operator: ",style="font-weight: 700;")),column(3,selectInput("AccessControlOperator",NULL,dataAccessOperators,selected=dataAccessOperators[1,1]))
                                                 ,column(1,h5("Year: ",style="font-weight: 700;")),column(2,selectInput("AccessControlYear",NULL,dataLoadYearsLOV,selected=dataLoadYearsLOV[1,1])))
                                        ,fluidRow(column(7,radioButtons("accessControlRadioBtns",NULL,choices = c("Summary per main service","Detail per service"),selected ="Summary per main service",inline = T)),column(2,actionButton("showACReview","Show")))
                                        ,uiOutput("providerNameOp")
                                        ,class="well")
                 )),fluidRow(dataTableOutput("userOutput")))
       ,tabPanel("All Operators",value = "ACAllOperatorTabPanel",
                 fluidRow(column(11,div(class="well"
                                        ,fluidRow(column(1,h5("Year: ",style="font-weight: 700;")),column(2,selectInput("AccessControlYear2",NULL,dataLoadYearsLOV,selected="AccessControlYear2"))
                                                  ,column(6,radioButtons("accessControlRadioBtns2",NULL,choices = c("Summary per main service","Monthly revenues per service"),selected ="Summary per main service",inline = T)),column(2,actionButton("accessControlBtn2","Show"))))))
                 ,fluidRow(column(8,dataTableOutput("userOutput2"))))
))

observeEvent(input$showACReview,{
  if(input$accessControlRadioBtns == "" || input$AccessControlCategory == "" || input$AccessControlOperator == "" || input$AccessControlYear == "")
  {
    showNotification("Please fill the fields!")
    return()
  }
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  statusCheck <- dbSendQuery(jdbcConnection,paste0("select * from BI_SALES_PROVIDERS_STATUS where PROVIDER_ID = ",userCheck$PROVIDER_ID," AND PROVIDER_STATUS=1 AND SALES_YEAR='",input$AccessControlYear,"' order by SALES_MONTH DESC,SALES_YEAR DESC"))
  statusCheck <- dbFetch(statusCheck)
  if(nrow(statusCheck) == 0)
  {
    showNotification("Data not available yet.")
    return()
  }
  else{
    if(input$accessControlRadioBtns == "Summary per main service"){
      if(statusCheck$PROVIDER_STATUS == 0)
      {
        showNotification("Data not available yet.")
      }
      else{
        ProviderName <- dbSendQuery(jdbcConnection,paste0("SELECT PROVIDER_NAME,PROVIDER_SHARE FROM BI_SALES_PROVIDERS WHERE PROVIDER_ID=",userCheck$PROVIDER_ID))
        ProviderName <- dbFetch(ProviderName)
        output$providerNameOp <- renderUI(div(style="font-weight: 700;color:red;",renderText(paste("Provider:",ProviderName[1,1],", Share:",(ProviderName[1,2]*100),"%"))))
        getServiceSummaryData(userCheck$PROVIDER_ID,unique(accessControlLOV$CATEGORY_ID[accessControlLOV$CATEGORY_NAME == input$AccessControlCategory]),unique(accessControlLOV$OPERATOR_ID[accessControlLOV$OPERATOR_NAME == input$AccessControlOperator]),input$AccessControlYear)
      }
    }
    else if(input$accessControlRadioBtns == "Detail per service"){
      if(statusCheck$PROVIDER_STATUS == 0)
      {
        showNotification("Data not available yet.")
      }
      else{
        ProviderName <- dbSendQuery(jdbcConnection,paste0("SELECT PROVIDER_NAME,PROVIDER_SHARE FROM BI_SALES_PROVIDERS WHERE PROVIDER_ID=",userCheck$PROVIDER_ID))
        ProviderName <- dbFetch(ProviderName)
        output$providerNameOp <- renderUI(div(style="font-weight: 700;color:red;",renderText(paste("Provider:",ProviderName[1,1],", Share:",(ProviderName[1,2]*100),"%"))))
        getServiceDetailData(userCheck$PROVIDER_ID,accessControlLOV$CATEGORY_ID[accessControlLOV$CATEGORY_NAME == input$AccessControlCategory],accessControlLOV$OPERATOR_ID[accessControlLOV$OPERATOR_NAME == input$AccessControlOperator],input$AccessControlYear)
      }
    }
    else if(input$accessControlRadioBtns == "Detail per day"){
      if(nrow(statusCheck) == 1)
      {
        if(statusCheck$PROVIDER_STATUS == 0)
        {
          showNotification("Data not available yet.")
        }
        else{
          ProviderName <- dbSendQuery(jdbcConnection,paste0("SELECT PROVIDER_NAME,PROVIDER_SHARE FROM BI_SALES_PROVIDERS WHERE PROVIDER_ID=",userCheck$PROVIDER_ID))
          ProviderName <- dbFetch(ProviderName)
          output$providerNameOp <- renderUI(div(style="font-weight: 700;color:red;",renderText(paste("Provider:",ProviderName[1,1],", Share:",(ProviderName[1,2]*100),"%"))))
          output$userOutput <- renderDataTable(getAccessData(statusCheck$SALES_MONTH,statusCheck$SALES_YEAR,userCheck$PROVIDER_ID,accessControlLOV$OPERATOR_ID[accessControlLOV$OPERATOR_NAME == input$AccessControlOperator]))
        }
      }
      else if(nrow(statusCheck) > 1)
      {
        jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
        ProviderName <- dbSendQuery(jdbcConnection,paste0("SELECT PROVIDER_NAME,PROVIDER_SHARE FROM BI_SALES_PROVIDERS WHERE PROVIDER_ID=",userCheck$PROVIDER_ID))
        ProviderName <- dbFetch(ProviderName)
        dbDisconnect(jdbcConnection)
        output$userOutput <- renderDataTable(getAccessData(statusCheck[1,4],statusCheck[1,3],userCheck$PROVIDER_ID,accessControlLOV$OPERATOR_ID[accessControlLOV$OPERATOR_NAME == input$AccessControlOperator]))
        
        providerMonthsACLOV <<- as.data.frame(statusCheck[,4],stringsAsFactors=FALSE)
        colnames(providerMonthsACLOV) <- c("Months")
        output$providerNameOp <- renderUI(fluidRow(column(4,h5(style="font-weight: 700;color:red;",paste("Provider:",ProviderName[1,1],", Share:",(ProviderName[1,2]*100),"%"))),
                                                   column(1,h5("Months: ",style="font-weight: 700;")),column(2,selectInput("providerMonthsSelector",NULL,providerMonthsACLOV,selected = "providerMonthsSelector"))
                                                   ,fluidRow(actionButton("showMonthDataBtn","Show Data"))))
      }
    }
  }
},ignoreInit = TRUE,ignoreNULL = TRUE)

observeEvent(input$accessControlBtn2,{
  if(input$AccessControlYear2 == "" || input$accessControlRadioBtns2 == "")
  {
    showNotification("Please fill the fields!")
    return()
  }
  if(input$accessControlRadioBtns2 == "Summary per main service")
  {
    operatorsServicesSummary(userCheck$PROVIDER_ID,input$AccessControlYear2)
  }
  else if(input$accessControlRadioBtns2 == "Monthly revenues per service")
  {
    operatorsMonthlyRevenues(userCheck$PROVIDER_ID,input$AccessControlYear2)
  }
},ignoreInit = TRUE,ignoreNULL = TRUE)