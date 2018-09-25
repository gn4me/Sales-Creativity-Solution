#Detail per day
getAccessData <- function(month,year,provider_id,operator_id){
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  # INNER JOIN BI_SALES_PROVIDERS PVS ON PVS.PROVIDER_ID = PV.PROVIDER_ID
  providerData <- dbSendQuery(jdbcConnection,paste0("SELECT PV.SERVICE_DATE,PV.PRODUCT_ID,PRO.PRODUCT_NAME,PRO.CHARGE_TYPE,
                                                    S.SERVICE_NAME,SSB.SUBCATEGORY_NAME,  MS.GN4ME_REVENUE AS ",shQuote("Gn4me Revenue"),",   PV.PROVIDER_REVENUE AS ",shQuote("Provider Revenue")," 
                                                    FROM BI_SALES_MYPROVIDER_D PV 
                                                    INNER JOIN BI_SALES_MYSERVICES MS ON( PV.SERVICE_ID = MS.SERVICE_ID AND PV.PRODUCT_ID=MS.PRODUCT_ID AND PV.SERVICE_DATE = MS.SERVICE_DATE AND PV.OPERATOR_ID = MS.OPERATOR_ID) 
                                                    INNER JOIN BI_SALES_PRODUCT PRO ON MS.PRODUCT_ID = PRO.PRODUCT_ID 
                                                    INNER JOIN BI_SALES_SERVICE S ON MS.SERVICE_ID = S.SERVICE_ID 
                                                    INNER JOIN BI_SALES_SUBCATEGORY SSB ON  (SSB.SUBCATEGORY_ID = S.SERVICE_SUBCATEGORY AND SSB.CATEGORY_ID = S.SERVICE_CATEGORY) 
INNER JOIN bI_Sales_Providers_Status sss on (sss.PROVIDER_ID = PV.PROVIDER_ID and sss.operator_id=S.OPERATOR_ID and sss.category_id=S.SERVICE_CATEGORY and sss.sales_year = to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') and sss.sales_month =  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM'))
WHERE to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",month,"' AND  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') = '",year,"' 
AND PV.PROVIDER_ID = ",provider_id," and sss.provider_status=1 AND PV.OPERATOR_ID=",operator_id))
  providerData <- dbFetch(providerData)
  dbDisconnect(jdbcConnection)
  providerData <- providerData[with(providerData,order(SERVICE_DATE,PRODUCT_ID)),]
  providerData$`Gn4me Revenue` <- round(providerData$`Gn4me Revenue`,digits = 2)
  providerData$`Provider Revenue` <- round(providerData$`Provider Revenue`,digits = 2)
  providerData$PRODUCT_ID <- as.character(providerData$PRODUCT_ID)
  sums <-as.data.frame(t(colSums(providerData[sapply(providerData, is.numeric)], na.rm = TRUE)))
  
  sums[,1] <- prettyNum(sums[,1], big.mark=",")
  sums[,2] <- prettyNum(sums[,2], big.mark = ",")
  
  sums1 <- cbind(" "=" ","Service Date" = " ","Product ID"=" ","Product Name"=" ","Charge Type"=" ","Service Name"=" ","Subcategory Name"=" ",sums)
  sums <- cbind("Total" = "Total"," "=" "," "="  "," "="  "," "="  "," "="  "," "=" ", sums)
  names(sums)<- as.character(apply(sums[1, ], 1, paste))
  sketch = htmltools::withTags(table(list(tableHeader(sums1),tableFooter(sums))))
  rownames(providerData) <- NULL
  providerData <- DT::datatable(providerData,container = sketch,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),lengthMenu=c(10,15,nrow(providerData))))
}
#Summary per main service
getServiceSummaryData<-function(provider_id,category_id,operator_id,year){
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  data <- dbSendQuery(jdbcConnection,paste0("SELECT C.CATEGORY_NAME AS ",shQuote("Main Service"),", to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') AS ",shQuote("Month"),",
        SUM(PV.PROVIDER_REVENUE)  AS ",shQuote("Provider Revenue"),"
        FROM BI_SALES_MYPROVIDER_D PV INNER JOIN BI_SALES_MYSERVICES MS
        ON( PV.SERVICE_ID = MS.SERVICE_ID AND PV.PRODUCT_ID=MS.PRODUCT_ID AND PV.SERVICE_DATE = MS.SERVICE_DATE AND PV.OPERATOR_ID = MS.OPERATOR_ID)
        INNER JOIN BI_SALES_SERVICE S ON MS.SERVICE_ID = S.SERVICE_ID
        INNER JOIN BI_SALES_PROVIDER_SERVICES PS ON ( PV.SERVICE_ID = PS.SERVICE_ID AND PS.PROVIDER_ID = PV.PROVIDER_ID)
        INNER JOIN BI_SALES_PROVIDERS P ON P.PROVIDER_ID = PS.PROVIDER_ID
        INNER JOIN BI_SALES_CATEGORY C ON C.CATEGORY_ID =  S.SERVICE_CATEGORY
        INNER JOIN bI_Sales_Providers_Status sss on (sss.PROVIDER_ID = PS.PROVIDER_ID and sss.operator_id=PV.OPERATOR_ID and sss.category_id=S.SERVICE_CATEGORY and sss.sales_year = to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') and sss.sales_month =  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM'))
        WHERE P.PROVIDER_ID = ",provider_id," AND PV.OPERATOR_ID = ",operator_id," AND S.SERVICE_CATEGORY = ",category_id," 
        AND  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",year,"'
        and sss.provider_status=1
        group by  C.CATEGORY_NAME, to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM')  
        order by  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM'),C.CATEGORY_NAME"))
  data <- dbFetch(data)
  if(nrow(data) > 0)
  {
    data <- reshape(data,idvar=c("Main Service"),timevar="Month",direction = "wide")
    names(data) <- gsub("Provider Revenue.", "", names(data))
    data[is.na(data)] <- 0
    data <- cbind(data,Total=rowSums(data[sapply(data, is.numeric)], na.rm = TRUE))
    sums <- as.data.frame(t(colSums(data[sapply(data, is.numeric)], na.rm = TRUE)))
    for(i in 1:length(names(sums)))
    {
      sums[,i]<-round(sums[,i],digits = 2)
      sums[,i]<-prettyNum(sums[,i],big.mark = ",")
    }
    sums1 <- cbind(" "=" ","Main Service"=" ",sums)
    sums <- cbind("Total" = "Total"," "="  ", sums)
    names(sums)<- as.character(apply(sums[1, ], 1, paste))
    sketch = htmltools::withTags(table(list(tableHeader(sums1),tableFooter(sums))))
    for(i in 2:length(names(data)))
    {
      data[,i] <- round(data[,i],digits = 2)
      #data[,i] <- prettyNum(data[,i], big.mark=",")
    }
    rownames(data) <- NULL
    data <-DT::datatable(data,container=sketch,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),bFilter=FALSE,lengthMenu=sort(c(10,15,nrow(data)))))
    output$userOutput <- renderDataTable(data)
  }
  else{
    data <- data.frame(Result="No data available")
    data <-DT::datatable(data,options = list(bFilter=FALSE))
    output$userOutput <- renderDataTable(data)
  }
  dbDisconnect(jdbcConnection)
  
}
#Detail per service
getServiceDetailData <- function(provider_id,category_id,operator_id,year){
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  data <- dbSendQuery(jdbcConnection,paste0("SELECT S.SERVICE_NAME AS ",shQuote("Service Name"),", to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') AS ",shQuote("Month"),",
              SUM(  PV.PROVIDER_REVENUE)  AS ",shQuote("Provider Revenue"),"
              FROM BI_SALES_MYPROVIDER_D PV INNER JOIN BI_SALES_MYSERVICES MS
              ON( PV.SERVICE_ID = MS.SERVICE_ID AND PV.PRODUCT_ID=MS.PRODUCT_ID AND PV.SERVICE_DATE = MS.SERVICE_DATE AND PV.OPERATOR_ID = MS.OPERATOR_ID)
              INNER JOIN BI_SALES_SERVICE S ON MS.SERVICE_ID = S.SERVICE_ID
              INNER JOIN BI_SALES_PROVIDER_SERVICES PS ON ( PV.SERVICE_ID = PS.SERVICE_ID AND PS.PROVIDER_ID = PV.PROVIDER_ID)
              INNER JOIN BI_SALES_PROVIDERS P ON P.PROVIDER_ID = PS.PROVIDER_ID
              INNER JOIN bI_Sales_Providers_Status sss on (sss.PROVIDER_ID = PS.PROVIDER_ID and sss.operator_id=PV.OPERATOR_ID and sss.category_id=S.SERVICE_CATEGORY and sss.sales_year = to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') and sss.sales_month =  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM'))
              WHERE P.PROVIDER_ID = ",provider_id," AND PV.OPERATOR_ID = ",operator_id," AND S.SERVICE_CATEGORY = ",category_id," 
              AND  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",year,"'
              and sss.provider_status =1                                                  
              group by  S.SERVICE_NAME, to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM'),  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY')  
              order by  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM'),S.SERVICE_NAME"))
  data <- dbFetch(data)
  if(nrow(data) > 0)
  {
    data <- reshape(data,idvar=c("Service Name"),timevar="Month",direction = "wide")
    names(data) <- gsub("Provider Revenue.", "", names(data))
    data[is.na(data)] <- 0
    data <- cbind(data,Total=rowSums(data[sapply(data, is.numeric)], na.rm = TRUE))
    sums <- as.data.frame(t(colSums(data[sapply(data, is.numeric)], na.rm = TRUE)))
    for(i in 1:length(names(sums)))
    {
      sums[,i]<-round(sums[,i],digits = 2)
      sums[,i]<-prettyNum(sums[,i],big.mark = ",")
    }
    sums1 <- cbind(" "=" ","Service Name"=" ",sums)
    sums <- cbind("Total" = "Total"," "="  ", sums)
    names(sums)<- as.character(apply(sums[1, ], 1, paste))
    sketch = htmltools::withTags(table(list(tableHeader(sums1),tableFooter(sums))))
    for(i in 2:length(names(data)))
    {
      data[,i] <- round(data[,i],digits = 2)
      #data[,i] <- prettyNum(data[,i], big.mark=",")
    }
    rownames(data) <- NULL
    data <-DT::datatable(data,container=sketch,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),bFilter=FALSE,lengthMenu=sort(c(10,15,nrow(data)))))
    output$userOutput <- renderDataTable(data)
  }
  else{
    data <- data.frame(Result="No data available")
    data <-DT::datatable(data,options = list(bFilter=FALSE))
    output$userOutput <- renderDataTable(data)
  }
  dbDisconnect(jdbcConnection)
}
userCheck <- data.frame()
providerMonthsACLOV <- data.frame()
providerYearsACLOV <- data.frame()

observeEvent(input$showMonthDataBtn,{
  if(is.null(input$providerMonthsSelector))
  {
    showNotification("Please select month and year!")
    return()
  }
  output$userOutput <- renderDataTable(getAccessData(input$providerMonthsSelector,input$AccessControlYear,userCheck$PROVIDER_ID,dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$AccessControlOperator]))
},ignoreInit = TRUE,ignoreNULL = TRUE)
output$accessControlTab <- renderUI(tabsetPanel(id="ACTabsetPanel"
       ,tabPanel("Per Operator",value = "ACPerOperatorTabPanel",
                 fluidRow(column(11,div(fluidRow(column(1,h5("Category: ",style="font-weight: 700;")),column(3,selectInput("AccessControlCategory",NULL,dataLoadCategoriesLOV,selected=dataLoadCategoriesLOV[1,1]))
                                                 ,column(1,h5("Operator: ",style="font-weight: 700;")),column(3,selectInput("AccessControlOperator",NULL,dataLoadOperatorsLOV,selected=dataLoadOperatorsLOV[1,1]))
                                                 ,column(1,h5("Year: ",style="font-weight: 700;")),column(2,selectInput("AccessControlYear",NULL,dataLoadYearsLOV,selected=dataLoadYearsLOV[1,1])))
                                        ,fluidRow(column(1,h5("Provider: ",style="font-weight:700;")),column(2,selectizeInput("AccessControlProviders",NULL, multiple = FALSE,providerNameLOV2,"AccessControlProviders")),column(7,radioButtons("accessControlRadioBtns",NULL,choices = c("Summary per main service","Detail per service"),selected ="Summary per main service",inline = T)),column(2,actionButton("showACReview2","Show")))
                                        ,uiOutput("providerNameOp")
                                        ,class="well")
                 )),fluidRow(dataTableOutput("userOutput")))
       ,tabPanel("All Operators",value = "ACAllOperatorTabPanel",
                 fluidRow(column(11,div(class="well"
                                 ,fluidRow(column(1,h5("Year: ",style="font-weight: 700;")),column(1,selectInput("AccessControlYear2",NULL,dataLoadYearsLOV,selected="AccessControlYear2"))
                                           ,column(1,h5("Provider: ",style="font-weight:700;")),column(2,selectInput("AccessControlProviders2",NULL,providerNameLOV3,"AccessControlProviders2"))
                                           ,column(6,radioButtons("accessControlRadioBtns2",NULL,choices = c("Summary per main service","Monthly revenues per service"),selected ="Summary per main service",inline = T)),column(1,actionButton("accessControlBtn1","Show"))))))
                 ,fluidRow(column(8,dataTableOutput("userOutput2"))))
))
providerNameLOV2 <-data.frame()

observeEvent(c(input$AccessControlCategory,input$AccessControlOperator),{
  providerNameLOV2 <<- as.data.frame(unique(operatorsLOV$PROVIDER_NAME[(operatorsLOV$OPERATOR_NAME == input$AccessControlOperator & operatorsLOV$CATEGORY_NAME == input$AccessControlCategory)]),stringsAsFactors = FALSE)
  colnames(providerNameLOV2)<- c("Providers Names")
  updateSelectizeInput(session,"AccessControlProviders",NULL,choices = providerNameLOV2,selected = "AccessControlProviders")
  
})

reactiveRadioBtnsAC <- reactive(input$accessControlRadioBtns)

observeEvent(input$showACReview2,{
  if(input$accessControlRadioBtns == "" || input$AccessControlCategory == "" || input$AccessControlProviders == "" || input$AccessControlOperator == "" || input$AccessControlYear == "")
  {
    showNotification("Please fill the fields!")
    return()
  }
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  statusCheck <- dbSendQuery(jdbcConnection,paste0("select * from BI_SALES_PROVIDERS_STATUS where PROVIDER_ID = ",operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$AccessControlProviders]," AND PROVIDER_STATUS=1 AND SALES_YEAR='",input$AccessControlYear,"' order by SALES_MONTH DESC,SALES_YEAR DESC"))
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
        ProviderName <- dbSendQuery(jdbcConnection,paste0("SELECT PROVIDER_NAME,PROVIDER_SHARE FROM BI_SALES_PROVIDERS WHERE PROVIDER_ID=",operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$AccessControlProviders]))
        ProviderName <- dbFetch(ProviderName)
        output$providerNameOp <- renderUI(div(style="font-weight: 700;color:red;",renderText(paste("Provider:",ProviderName[1,1],", Share:",(ProviderName[1,2]*100),"%"))))
        getServiceSummaryData(operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$AccessControlProviders],dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$AccessControlCategory],dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$AccessControlOperator],input$AccessControlYear)
      }
    }
    else if(input$accessControlRadioBtns == "Detail per service"){
      if(statusCheck$PROVIDER_STATUS == 0)
      {
        showNotification("Data not available yet.")
      }
      else{
        ProviderName <- dbSendQuery(jdbcConnection,paste0("SELECT PROVIDER_NAME,PROVIDER_SHARE FROM BI_SALES_PROVIDERS WHERE PROVIDER_ID=",operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$AccessControlProviders]))
        ProviderName <- dbFetch(ProviderName)
        output$providerNameOp <- renderUI(div(style="font-weight: 700;color:red;",renderText(paste("Provider:",ProviderName[1,1],", Share:",(ProviderName[1,2]*100),"%"))))
        getServiceDetailData(operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$AccessControlProviders],dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$AccessControlCategory],dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$AccessControlOperator],input$AccessControlYear)
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
          ProviderName <- dbSendQuery(jdbcConnection,paste0("SELECT PROVIDER_NAME,PROVIDER_SHARE FROM BI_SALES_PROVIDERS WHERE PROVIDER_ID=",operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$AccessControlProviders]))
          ProviderName <- dbFetch(ProviderName)
          output$providerNameOp <- renderUI(div(style="font-weight: 700;color:red;",renderText(paste("Provider:",ProviderName[1,1],", Share:",(ProviderName[1,2]*100),"%"))))
          output$userOutput <- renderDataTable(getAccessData(statusCheck$SALES_MONTH,statusCheck$SALES_YEAR,operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$AccessControlProviders]))
        }
      }
      else if(nrow(statusCheck) > 1)
      {
        jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
        ProviderName <- dbSendQuery(jdbcConnection,paste0("SELECT PROVIDER_NAME,PROVIDER_SHARE FROM BI_SALES_PROVIDERS WHERE PROVIDER_ID=",operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$AccessControlProviders]))
        ProviderName <- dbFetch(ProviderName)
        dbDisconnect(jdbcConnection)
        output$userOutput <- renderDataTable(getAccessData(statusCheck[1,4],statusCheck[1,3],userCheck$PROVIDER_ID))
        
        providerMonthsACLOV <<- as.data.frame(statusCheck[,4],stringsAsFactors=FALSE)
        colnames(providerMonthsACLOV) <- c("Months")
        # providerYearsACLOV <<- as.data.frame(statusCheck[,3],stringsAsFactors=FALSE)
        # colnames(providerYearsACLOV) <-c("Years")
        output$providerNameOp <- renderUI(fluidRow(column(4,h5(style="font-weight: 700;color:red;",paste("Provider:",ProviderName[1,1],", Share:",(ProviderName[1,2]*100),"%"))),
                                                   column(1,h5("Months: ",style="font-weight: 700;")),column(2,selectInput("providerMonthsSelector",NULL,providerMonthsACLOV,selected = "providerMonthsSelector"))
                                                   #,column(1,h5("Years: ",style="font-weight: 700;")),column(2,selectInput("providerYearsSelector",NULL,providerYearsACLOV,selected = "providerYearsSelector"))
                                                   ,fluidRow(actionButton("showMonthDataBtn","Show Data"))))
      }
    }
  }
},ignoreInit = TRUE,ignoreNULL = TRUE)
#For grouped operators

operatorsServicesSummary <- function(provider_id,year){
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  data <- dbSendQuery(jdbcConnection,paste0("SELECT C.CATEGORY_NAME,OMP.OPERATOR_NAME, to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') AS ",shQuote("Month"),",
            SUM(  PV.PROVIDER_REVENUE)  AS ",shQuote("Provider Revenue")," FROM BI_SALES_MYPROVIDER_D PV INNER JOIN BI_SALES_MYSERVICES MS
            ON( PV.SERVICE_ID = MS.SERVICE_ID AND PV.PRODUCT_ID=MS.PRODUCT_ID AND PV.SERVICE_DATE = MS.SERVICE_DATE AND PV.OPERATOR_ID = MS.OPERATOR_ID)
            INNER JOIN BI_SALES_SERVICE S ON MS.SERVICE_ID = S.SERVICE_ID
            INNER JOIN BI_SALES_PROVIDER_SERVICES PS ON ( PV.SERVICE_ID = PS.SERVICE_ID AND PS.PROVIDER_ID = PV.PROVIDER_ID)
            INNER JOIN BI_SALES_PROVIDERS P ON P.PROVIDER_ID = PS.PROVIDER_ID
            INNER JOIN BI_SALES_CATEGORY C ON C.CATEGORY_ID =  S.SERVICE_CATEGORY
            INNER JOIN BI_SALES_MYOPERATOR OMP ON OMP.OPERATOR_ID = PV.OPERATOR_ID
            INNER JOIN bI_Sales_Providers_Status sss on (sss.PROVIDER_ID = PS.PROVIDER_ID and sss.operator_id=S.OPERATOR_ID and sss.category_id=S.SERVICE_CATEGORY and sss.sales_year = to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') and sss.sales_month =  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM'))
            WHERE P.PROVIDER_ID=",provider_id," AND  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",year,"'
            and sss.provider_status =1                                              
            group by C.CATEGORY_NAME,OMP.OPERATOR_NAME, to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM')
            order by  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM'),C.CATEGORY_NAME"))
  data <- dbFetch(data)
  if(nrow(data) > 0)
  {
    data <- reshape(data,idvar=c("CATEGORY_NAME","Month"),timevar="OPERATOR_NAME",direction = "wide")
    names(data) <- gsub("Provider Revenue.", "", names(data))
    data[is.na(data)] <- 0
    data <- cbind(data,Total=rowSums(data[sapply(data, is.numeric)], na.rm = TRUE))
    sums <- as.data.frame(t(colSums(data[sapply(data, is.numeric)], na.rm = TRUE)))
    for(i in 1:length(names(sums)))
    {
      sums[,i]<-round(sums[,i],digits = 2)
      sums[,i]<-prettyNum(sums[,i],big.mark = ",")
    }
    sums1 <- cbind(" "=" ","Category Name"=" ","Month"=" ",sums)
    sums <- cbind("Total" = "Total"," "="  "," "=" ", sums)
    names(sums)<- as.character(apply(sums[1, ], 1, paste))
    sketch = htmltools::withTags(table(list(tableHeader(sums1),tableFooter(sums))))
    for(i in 3:length(names(data)))
    {
      data[,i] <- round(data[,i],digits = 2)
      #data[,i] <- prettyNum(data[,i], big.mark=",")
    }
    rownames(data) <- NULL
    data <-DT::datatable(data,container=sketch,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),bFilter=FALSE,lengthMenu=sort(c(10,15,nrow(data)))))
    output$userOutput2 <- renderDataTable(data)
  }
  else{
    data <- data.frame(Result="No data available")
    data <-DT::datatable(data,options = list(bFilter=FALSE))
    output$userOutput2 <- renderDataTable(data)
  }
}

operatorsMonthlyRevenues <- function(provider_id,year){
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  data <- dbSendQuery(jdbcConnection,paste0("SELECT S.SERVICE_NAME,OMP.OPERATOR_NAME, to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') AS ",shQuote("Month"),",
              SUM(  PV.PROVIDER_REVENUE)  AS ",shQuote("Provider Revenue")," FROM BI_SALES_MYPROVIDER_D PV INNER JOIN BI_SALES_MYSERVICES MS
              ON( PV.SERVICE_ID = MS.SERVICE_ID AND PV.PRODUCT_ID=MS.PRODUCT_ID AND PV.SERVICE_DATE = MS.SERVICE_DATE AND PV.OPERATOR_ID = MS.OPERATOR_ID)
              INNER JOIN BI_SALES_SERVICE S ON MS.SERVICE_ID = S.SERVICE_ID
              INNER JOIN BI_SALES_PROVIDER_SERVICES PS ON ( PV.SERVICE_ID = PS.SERVICE_ID AND PS.PROVIDER_ID = PV.PROVIDER_ID)
              INNER JOIN BI_SALES_PROVIDERS P ON P.PROVIDER_ID = PS.PROVIDER_ID
              INNER JOIN BI_SALES_CATEGORY C ON C.CATEGORY_ID =  S.SERVICE_CATEGORY
              INNER JOIN BI_SALES_MYOPERATOR OMP ON OMP.OPERATOR_ID = PV.OPERATOR_ID
              INNER JOIN bI_Sales_Providers_Status sss on (sss.PROVIDER_ID = PS.PROVIDER_ID and sss.operator_id=S.OPERATOR_ID and sss.category_id=S.SERVICE_CATEGORY and sss.sales_year = to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') and sss.sales_month =  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM'))
              WHERE P.PROVIDER_ID=",provider_id," AND  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",year,"'
              and sss.provider_status=1                                            
              group by S.SERVICE_NAME,OMP.OPERATOR_NAME, to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM')
              order by  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM'),S.SERVICE_NAME"))
  data <- dbFetch(data)
  if(nrow(data) > 0)
  {
    data <- reshape(data,idvar=c("SERVICE_NAME","Month"),timevar="OPERATOR_NAME",direction = "wide")
    names(data) <- gsub("Provider Revenue.", "", names(data))
    data[is.na(data)] <- 0
    #data <- data[,c(2,1,3)]
    data <- cbind(data,Total=rowSums(data[sapply(data, is.numeric)], na.rm = TRUE))
    sums <- as.data.frame(t(colSums(data[sapply(data, is.numeric)], na.rm = TRUE)))
    for(i in 1:length(names(sums)))
    {
      sums[,i]<-round(sums[,i],digits = 2)
      sums[,i]<-prettyNum(sums[,i],big.mark = ",")
    }
    sums1 <- cbind(" "=" ","Service Name"=" ","Month"=" ",sums)
    sums <- cbind("Total" = "Total"," "="  "," "=" ", sums)
    names(sums)<- as.character(apply(sums[1, ], 1, paste))
    sketch = htmltools::withTags(table(list(tableHeader(sums1),tableFooter(sums))))
    for(i in 3:length(names(data)))
    {
      data[,i] <- round(data[,i],digits = 2)
      #data[,i] <- prettyNum(data[,i], big.mark=",")
    }
    rownames(data) <- NULL
    data <-DT::datatable(data,container=sketch,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),bFilter=FALSE,lengthMenu=sort(c(10,15,nrow(data)))))
    output$userOutput2 <- renderDataTable(data)
  }
  else{
    data <- data.frame(Result="No data available")
    data <-DT::datatable(data,options = list(bFilter=FALSE))
    output$userOutput2 <- renderDataTable(data)
  }
}

providerNameLOV3 <- as.data.frame(unique(operatorsLOV$PROVIDER_NAME),stringsAsFactors = FALSE)
colnames(providerNameLOV3) <- c("Providers")
observeEvent(input$accessControlBtn1,{
  if(input$AccessControlYear2 == "" || input$accessControlRadioBtns2 == "")
  {
    showNotification("Please fill the fields!")
    return()
  }
  if(input$accessControlRadioBtns2 == "Summary per main service")
  {
    operatorsServicesSummary(operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$AccessControlProviders2],input$AccessControlYear2)
  }
  else if(input$accessControlRadioBtns2 == "Monthly revenues per service")
  {
    operatorsMonthlyRevenues(operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$AccessControlProviders2],input$AccessControlYear2)
  }
},ignoreInit = TRUE,ignoreNULL = TRUE)
