output$reviewTab <- renderUI(tabsetPanel(id="reviewApproveTabset",tabPanel("Approve per provider",value = "appPerProviderTabPanel",fluidRow(column(12,div(
  fluidRow(column(1,h5("Year ",style="font-weight: 700;")),column(1,selectInput("yearInput",NULL,yearsLOV,selected = "yearInput")),column(1,h5("Month",style="font-weight: 700;")),column(1,selectInput("monthInput",NULL,monthsLOV,selected = "monthInput"))
  ,column(1,h5("Category: ",style="font-weight: 700;")),column(3,selectInput("reportCategory",NULL,categoriesNamesLOV,selected = "reportCategory"))
  ,column(1,h5("Operator: ",style="font-weight: 700;")),column(2,selectInput("operatorName",NULL,operatorNameLOV,selected=operatorNameLOV[1,1])))
  ,fluidRow(column(1,h5("Provider: ",style="font-weight: 700;")),column(3,selectizeInput("providerName",NULL,providerNameLOV, multiple = FALSE,selected="providerName"))
            ,column(1,actionButton("calculateAlertReportBtn","Review")),column(2,actionButton("makeAvailableBtn","Make Available")),column(1,actionButton("unmakeBtn","Unmake Available")))
  ,class="well"))),fluidRow(dataTableOutput("alertTableOutput"))
),
tabPanel("Approve all providers",value = "appAllProvidersTabPanel",
         fluidRow(column(12,div(fluidRow(column(1,h5("Year ",style="font-weight: 700;")),column(1,selectInput("dataLoadYearInput2",NULL,dataLoadYearsLOV,selected = "dataLoadYearInput2")),column(1,h5("Month",style="font-weight: 700;")),column(1,selectInput("dataLoadMonthInput2",NULL,dataLoadMonthsLOV,selected = "dataLoadMonthInput2"))
                               ,column(1,h5("Category: ",style="font-weight: 700;")),column(3,selectInput("dataLoadCategory2",NULL,dataLoadCategoriesLOV,selected = "dataLoadCategory2"))
                               ,column(1,h5("Operator: ",style="font-weight: 700;")),column(3,selectInput("dataLoadOperatorName2",NULL,dataLoadOperatorsLOV,selected="dataLoadOperatorName2")))
                               ,fluidRow(column(2,radioButtons("reviewRadioBtns",NULL,inline=T,choices = c("Summary","Detail"),selected ="Summary"))
                                         ,column(1,style="text-align:right;",actionButton("groupedDataBtn","Review")),column(2,actionButton("monthClosingBtn2","Month Closing")),column(2,actionButton("makeAllAvailableBtn","Make All Available")),column(2,actionButton("unmakeAllBtn","Unmake Available")),column(2,actionButton("monthClosingDeletionButton","Month Closing Unlock")))
                               ,class="well")))
         ,fluidRow(column(8,uiOutput("tableheaderop")))
         ,fluidRow(column(8,offset=1,uiOutput("grandTotal")))
)))

## Month and year lookups
jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
datesLOV <- dbSendQuery(jdbcConnection,"SELECT SALES_YEAR, SALES_MONTH FROM RTEST.BI_SALES_MONTHS")
datesLOV <- dbFetch(datesLOV)

yearsLOV <- as.data.frame(unique(datesLOV$SALES_YEAR),stringAsFactors=FALSE)
colnames(yearsLOV)<- c("Years")
#yearsLOV <- rbind(r,yearsLOV)
monthsLOV <- as.data.frame(unique(datesLOV$SALES_MONTH),stringAsFactors=FALSE)
colnames(monthsLOV)<- c("Months")
#monthsLOV <- rbind(r,monthsLOV)

dynamicMonth <- reactive(input$yearInput)

observeEvent(dynamicMonth(),{
  SelectedYear <- input$yearInput
  monthsLOV <<- as.data.frame(datesLOV$SALES_MONTH[datesLOV$SALES_YEAR == SelectedYear])
  colnames(monthsLOV)<- c("Months")
  updateSelectInput(session,"monthInput",NULL,choices = monthsLOV)
})
## Review lookups
operatorsLOV <- dbSendQuery(jdbcConnection,"SELECT DISTINCT C.CATEGORY_NAME,C.CATEGORY_ID,V.PROVIDER_ID,PV.PROVIDER_NAME, O.OPERATOR_ID, O.OPERATOR_NAME 
FROM BI_SALES_MYOPERATOR O INNER JOIN BI_SALES_OPERATOR_SERVICES S ON O.OPERATOR_ID = S.OPERATOR_ID
                            INNER JOIN BI_SALES_SERVICE SS ON SS.SERVICE_ID = S.SERVICE_ID
                            INNER JOIN BI_SALES_PROVIDER_SERVICES V ON V.SERVICE_ID = S.SERVICE_ID 
                            INNER JOIN BI_SALES_PROVIDERS PV ON PV.PROVIDER_ID = V.PROVIDER_ID  
                            INNER JOIN BI_SALES_CATEGORY C ON C.CATEGORY_ID = SS.SERVICE_CATEGORY ORDER BY V.PROVIDER_ID")
operatorsLOV <- dbFetch(operatorsLOV)
dbDisconnect(jdbcConnection)
operatorsLOV$OPERATOR_NAME <- as.character(operatorsLOV$OPERATOR_NAME)

operatorsLOV$CATEGORY_NAME <- as.character(operatorsLOV$CATEGORY_NAME)
categoriesNamesLOV <- as.data.frame(unique(operatorsLOV$CATEGORY_NAME),stringsAsFactors = FALSE)
colnames(categoriesNamesLOV)<- c("Categories")

operatorNameLOV <- as.data.frame(unique(operatorsLOV$OPERATOR_NAME),stringsAsFactors = FALSE)
colnames(operatorNameLOV)<- c("Operators Names")
operatorNameLOV <- rbind(r,operatorNameLOV)

observeEvent(c(input$reportCategory,input$operatorName),{
  providerNameLOV <<- as.data.frame(operatorsLOV$PROVIDER_NAME[(operatorsLOV$OPERATOR_NAME == input$operatorName & operatorsLOV$CATEGORY_NAME == input$reportCategory)],stringsAsFactors = FALSE)
  colnames(providerNameLOV)<- c("Providers Names")
  updateSelectInput(session,"providerName",NULL,choices = providerNameLOV,selected = providerNameLOV )

})

providerNameLOV <- as.data.frame(unique(operatorsLOV$PROVIDER_NAME),stringsAsFactors = FALSE)
colnames(providerNameLOV)<- c("Providers Names")
#providerNameLOV <- rbind(r,providerNameLOV)
observeEvent(input$calculateAlertReportBtn,{
  if(input$yearInput == "" || input$monthInput =="" || input$operatorName == "" || input$providerName == "")
  {
    showNotification("Please fill in the missing fields!")
    return()
  }
  else{
    showNotification("Reviewing.. Please wait!")
    jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
    tableData <- dbSendQuery(jdbcConnection,paste0("SELECT PV.SERVICE_DATE,PV.PRODUCT_ID,PRO.PRODUCT_NAME,PRO.CHARGE_TYPE,S.SERVICE_NAME,SSB.SUBCATEGORY_NAME,MS.TOTAL_REVENUE AS ",shQuote("Total Revenue"),",  MS.GN4ME_REVENUE AS ",shQuote("Gn4me Revenue"),",   PV.PROVIDER_REVENUE AS ",shQuote("Provider Revenue")," 
                                                   FROM BI_SALES_MYPROVIDER_D PV INNER JOIN BI_SALES_MYSERVICES MS ON( PV.SERVICE_ID = MS.SERVICE_ID AND PV.PRODUCT_ID=MS.PRODUCT_ID AND PV.SERVICE_DATE = MS.SERVICE_DATE AND PV.OPERATOR_ID=MS.OPERATOR_ID) INNER JOIN BI_SALES_PRODUCT PRO ON MS.PRODUCT_ID = PRO.PRODUCT_ID INNER JOIN BI_SALES_SERVICE S 
                                                   ON MS.SERVICE_ID = S.SERVICE_ID INNER JOIN BI_SALES_SUBCATEGORY SSB ON  (SSB.SUBCATEGORY_ID = S.SERVICE_SUBCATEGORY AND SSB.CATEGORY_ID = S.SERVICE_CATEGORY) WHERE to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",input$monthInput,"' 
                                                   AND  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') = '",input$yearInput,"' AND PV.PROVIDER_ID = ",operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$providerName],"AND PV.OPERATOR_ID=",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$operatorName]))
    tableData <- dbFetch(tableData)
    tableData$`Total Revenue` <- round(tableData$`Total Revenue`,digits = 2)
    tableData$`Provider Revenue` <- round(tableData$`Provider Revenue`,digits = 2)
    tableData$`Gn4me Revenue` <- round(tableData$`Gn4me Revenue`,digits = 2)
    dbDisconnect(jdbcConnection)
    #Order by Date, product_id
    tableData <- tableData[with(tableData,order(SERVICE_DATE,PRODUCT_ID)),]
    if(nrow(tableData) > 0)
    {
      jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
      share <- dbSendQuery(jdbcConnection,paste0("SELECT PROVIDER_SHARE FROM BI_SALES_PROVIDERS WHERE PROVIDER_ID = ",operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$providerName]))
      share <- dbFetch(share)
      dbDisconnect(jdbcConnection)
      tableData$PRODUCT_ID <- as.character(tableData$PRODUCT_ID)
      
      sums <-as.data.frame(t(colSums(tableData[sapply(tableData, is.numeric)], na.rm = TRUE)))
      sums[,1] <- prettyNum(sums[,1], big.mark=",")
      sums[,2] <- prettyNum(sums[,2], big.mark = ",")
      sums[,3] <- prettyNum(sums[,3], big.mark = ",")
      sums1 <- cbind(" "=" ","Service Date" = " ","Product ID"=" ","Product Name"=" ","Charge Type"=" ","Service Name"=" ","Subcategory Name"=" ",sums)
      sums <- cbind("Total" = "Total"," "=" "," "="  "," "="  "," "="  "," "="  "," "="  ", sums)
      names(sums)<- as.character(apply(sums[1, ], 1, paste))
      sketch = htmltools::withTags(table(list(tableHeader(sums1),tableFooter(sums))))
      rownames(data) <- NULL
      data <- DT::datatable(tableData,caption = paste("Provider Share=",share*100,"%"),container = sketch,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),lengthMenu=sort(c(10,15,nrow(tableData)))))
      output$alertTableOutput <- renderDataTable(data)  
    }
    else{
      output$alertTableOutput <- renderDataTable(DT::datatable(data.frame(Result="No data to show!"),options = list(rownames = FALSE)))
    }
  }
},ignoreInit = TRUE,ignoreNULL = TRUE)
observeEvent(input$makeAvailableBtn,{
  if(input$reportCategory == "" || input$yearInput == "" || input$monthInput =="" || input$operatorName == "" || input$providerName == "")
  {
    showNotification("Please fill in the fields!")
    return()
  }
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  rowCheck <- dbSendQuery(jdbcConnection,paste0("SELECT * FROM BI_SALES_PROVIDERS_STATUS WHERE PROVIDER_ID =",operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$providerName],"AND OPERATOR_ID=",operatorsLOV$OPERATOR_ID[operatorsLOV$OPERATOR_NAME == input$operatorName],"AND SALES_MONTH ='",input$monthInput,"' AND SALES_YEAR=",input$yearInput," AND CATEGORY_ID=",operatorsLOV$CATEGORY_ID[match(operatorsLOV$CATEGORY_NAME,input$reportCategory)]))
  rowCheck <- dbFetch(rowCheck)
  if(nrow(rowCheck) == 0)
  {
    dbSendUpdate(jdbcConnection,paste0("INSERT INTO BI_SALES_PROVIDERS_STATUS(  PROVIDER_ID, CATEGORY_ID, SALES_YEAR, SALES_MONTH, PROVIDER_STATUS, OPERATOR_ID) VALUES(",operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$providerName],",",operatorsLOV$CATEGORY_ID[match(operatorsLOV$CATEGORY_NAME,input$reportCategory)],",",input$yearInput,",'",input$monthInput,"',1 ,",operatorsLOV$OPERATOR_ID[operatorsLOV$OPERATOR_NAME == input$operatorName],")"))
    dbDisconnect(jdbcConnection)
    showNotification("Data now available for provider!")
  }
  else{
    if(rowCheck$PROVIDER_STATUS == 1)
    {
      dbDisconnect(jdbcConnection)
      showNotification("Data is already available for provider!")
    }
  }
})

observeEvent(input$unmakeBtn,{
  if(input$reportCategory == "" || input$yearInput == "" || input$monthInput =="" || input$operatorName == "" || input$providerName == "")
  {
    showNotification("Please fill in the fields!")
    return()
  }
  showModal(modalDialog(title = "Attention",easyClose = TRUE,size='m',renderUI(fluidRow(column(8,h5("Are you sure you want to unmake them?",style="font-weight:700;")),column(4,div(style="text-align:right;",actionButton("unmakeConfirmedBtn","Yes")))))))
},ignoreInit = TRUE,ignoreNULL = TRUE)
observeEvent(input$unmakeConfirmedBtn,{
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  dbSendUpdate(jdbcConnection,paste0("DELETE FROM BI_SALES_PROVIDERS_STATUS WHERE OPERATOR_ID =",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$operatorName]," 
                                     AND CATEGORY_ID =",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$reportCategory]," AND SALES_YEAR='",input$yearInput,"' AND SALES_MONTH='",input$monthInput,"' 
                                     AND PROVIDER_ID = ",operatorsLOV$PROVIDER_ID[operatorsLOV$PROVIDER_NAME == input$providerName]))
  dbDisconnect(jdbcConnection)
  showNotification("Deletion done successfully!")
},ignoreInit = TRUE,ignoreNULL = TRUE)


providers <- data.frame()
grouped_data <- data.frame()
observeEvent(input$groupedDataBtn,{
  if(input$dataLoadYearInput2 == "" || is.null(input$reviewRadioBtns) || input$dataLoadMonthInput2 == "" || input$dataLoadOperatorName2 == "" || input$dataLoadCategory2 =="")
  {
    showNotification("Please fill in the fields!")
    return()
  }
  if(input$reviewRadioBtns == "Summary")
  {
    jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
    grouped_data <- dbSendQuery(jdbcConnection,paste0("SELECT P.PROVIDER_ID, P.PROVIDER_NAME AS ",shQuote("Provider Name"),",P.PROVIDER_SHARE AS ",shQuote("Provider Share(%)"),",SUM(W.GN4ME_REVENUE) AS ",shQuote("Total Revenue"),",SUM(D.PROVIDER_REVENUE) AS ",shQuote("Provider Revenue"),"
                                                      FROM BI_SALES_MYPROVIDER_D D  INNER JOIN BI_SALES_MYSERVICES W
                                                      ON(D.SERVICE_ID = W.SERVICE_ID AND D.PRODUCT_ID = W.PRODUCT_ID AND D.SERVICE_DATE = W.SERVICE_DATE  AND D.OPERATOR_ID = W.OPERATOR_ID)
                                                      INNER JOIN BI_SALES_PROVIDERS P ON D.PROVIDER_ID= P.PROVIDER_ID INNER JOIN BI_SALES_SERVICE S
                                                      ON S.SERVICE_ID = D.SERVICE_ID 
                                                      where to_char(TO_DATE(D.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",input$dataLoadMonthInput2,"' AND
                                                      to_char(TO_DATE(D.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",input$dataLoadYearInput2,"' 
                                                      AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory2]," 
                                                      AND D.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName2],"
                                                      GROUP BY P.PROVIDER_ID, P.PROVIDER_NAME,P.PROVIDER_SHARE
                                                      ORDER BY P.PROVIDER_NAME"))
    grouped_data <- dbFetch(grouped_data)
    dbDisconnect(jdbcConnection)
    grouped_data <- grouped_data[2:5]
    grouped_data[,2]<-grouped_data[,2]*100
    sums <-as.data.frame(t(colSums(grouped_data[sapply(grouped_data, is.numeric)], na.rm = TRUE)))
    grouped_data$`Share Check` <- round((grouped_data$`Provider Revenue`/grouped_data$`Total Revenue`)*100,0)
    grouped_data$`Total Revenue` <- round(grouped_data$`Total Revenue`,digits = 2)
    grouped_data$`Provider Revenue` <- round(grouped_data$`Provider Revenue`,digits = 2)
    
    grouped_data <- DT::datatable(grouped_data,extensions = 'Buttons',options = list(bFilter=FALSE,dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),lengthMenu=sort(c(10,15,nrow(grouped_data)))))
    
    output$tableheaderop <- renderUI({
      output$x <- DT::renderDataTable(grouped_data)
      dataTableOutput("x")})
    output$grandTotal <- renderUI(h4(paste("Grand Total Revnue =",prettyNum(sums$`Total Revenue`, big.mark=",")," ,Total Provider Revenue=",prettyNum(sums$`Provider Revenue`, big.mark=",")),style="font-weight: 700;color:white;"))
    
  }
  else if( input$reviewRadioBtns == "Detail")
  {
    jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
    grouped_data <- dbSendQuery(jdbcConnection,paste0("SELECT D.PROVIDER_ID,P.PROVIDER_NAME,P.PROVIDER_SHARE, S.SERVICE_NAME,SUM(W.GN4ME_REVENUE) AS ",shQuote("Total Revenue"),",SUM(D.PROVIDER_REVENUE) AS ",shQuote("Provider Revenue"),"
                                                      FROM BI_SALES_MYPROVIDER_D D  INNER JOIN BI_SALES_MYSERVICES W
                                                      ON(D.SERVICE_ID = W.SERVICE_ID AND D.PRODUCT_ID = W.PRODUCT_ID AND D.SERVICE_DATE = W.SERVICE_DATE AND D.OPERATOR_ID = W.OPERATOR_ID)
                                                      INNER JOIN BI_SALES_PROVIDERS P ON D.PROVIDER_ID= P.PROVIDER_ID INNER JOIN BI_SALES_SERVICE S
                                                      ON S.SERVICE_ID = D.SERVICE_ID  
                                                      where to_char(TO_DATE(D.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",input$dataLoadMonthInput2,"' AND
                                                      to_char(TO_DATE(D.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",input$dataLoadYearInput2,"' 
                                                      AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory2]," 
                                                      AND D.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName2],"
                                                      GROUP BY  D.PROVIDER_ID,P.PROVIDER_NAME,P.PROVIDER_SHARE,  S.SERVICE_ID,S.SERVICE_NAME
                                                      ORDER BY P.PROVIDER_NAME"))
    grouped_data <- dbFetch(grouped_data)
    dbDisconnect(jdbcConnection)
    if(nrow(grouped_data) > 0)
    {
      grouped_data$PROVIDER_ID <- as.character(grouped_data$PROVIDER_ID)
      
      grouped_data$`Total Revenue` <- round(grouped_data$`Total Revenue`,digits = 2)
      grouped_data$`Provider Revenue` <- round(grouped_data$`Provider Revenue`,digits = 2)
      
      providers<-as.data.frame(unique(grouped_data$PROVIDER_ID),stringsAsFactors=FALSE)
      output$tableheaderop <- renderUI({
        lapply(1:nrow(providers), function(i) {
          
          DT::dataTableOutput(paste0('T', i))
          temp<-grouped_data[grouped_data$PROVIDER_ID==providers[i,1],]
          tempH <- paste0("Provider Name: ",temp[1,2],", Provider Share: ",(temp[1,3]*100),"%")
          temp <- temp[,4:6]
          sums <-as.data.frame(t(colSums(temp[sapply(temp, is.numeric)], na.rm = TRUE)))
          sums[,1] <- prettyNum(sums[,1], big.mark=",")
          sums[,2] <- prettyNum(sums[,2], big.mark=",")
          sums1 <- cbind(" "=" ","Service Name"=" ",sums)
          sums <- cbind("Total" = "Total"," "="  ",sums)
          names(sums)<- as.character(apply(sums[1, ], 1, paste))
          sketch = htmltools::withTags(table(list(tableHeader(sums1),tableFooter(sums))))
          rownames(temp) <- NULL
          temp <- DT::datatable(temp,caption = tempH,container = sketch,options = list(bFilter=FALSE,lengthMenu=sort(c(10,15,nrow(temp)))))
          name<-paste0('T', i)
          output[[name]] <- DT::renderDataTable({temp})
        })
      })
      sums <-as.data.frame(t(colSums(grouped_data[sapply(grouped_data, is.numeric)], na.rm = TRUE)))
      output$grandTotal <- renderUI(h4(paste("Grand Total Revnue =",prettyNum(sums$`Total Revenue`, big.mark=",")," ,Total Provider Revenue=",prettyNum(sums$`Provider Revenue`, big.mark=",")),style="font-weight: 700;color:white;"))
      #output$AfterReportUI2 <- renderUI(div(style="text-align:center;",actionButton("makeAllAvailableBtn","Make All Available")))
      #output$downloadButton <- renderUI(downloadLink("dataExportButton","Download",class="btn btn-default"))
    }
    else{
      grouped_data <- data.frame(Result = "There's no Data available.")
      grouped_data <- DT::datatable(grouped_data)
      output$tableheaderop <- renderUI({
        output$x <- DT::renderDataTable(grouped_data)
        dataTableOutput("x")})
    }
  }
  
},ignoreInit = TRUE,ignoreNULL = TRUE)

observeEvent(input$makeAllAvailableBtn,{
  if(is.null(input$dataLoadCategory2) || is.null(input$dataLoadYearInput2) || is.null(input$dataLoadMonthInput2)|| is.null(input$dataLoadOperatorName2))
  {
    showNotification("Please fill the fields!")
    return()
  }
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  operatorId <- dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName2]
  categoryId <- dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory2]
  providers <- dbSendQuery(jdbcConnection,paste0("SELECT P.PROVIDER_ID FROM BI_SALES_MYPROVIDER_D D  INNER JOIN BI_SALES_MYSERVICES W
     ON(D.SERVICE_ID = W.SERVICE_ID AND D.PRODUCT_ID = W.PRODUCT_ID AND D.SERVICE_DATE = W.SERVICE_DATE AND D.OPERATOR_ID = W.OPERATOR_ID)
     INNER JOIN BI_SALES_PROVIDERS P ON D.PROVIDER_ID= P.PROVIDER_ID INNER JOIN BI_SALES_SERVICE S
     ON S.SERVICE_ID = D.SERVICE_ID 
     where to_char(TO_DATE(D.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",input$dataLoadMonthInput2,"' AND
     to_char(TO_DATE(D.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",input$dataLoadYearInput2,"' 
     AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory2]," 
     AND D.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName2],"
     GROUP BY P.PROVIDER_ID"))
  providers <- dbFetch(providers)
  if(nrow(providers) > 0)
  {
    providers <- cbind(providers,categoryId,input$dataLoadYearInput2,input$dataLoadMonthInput2,PROVIDER_STATUS=1,operatorId)
    rowCheck <- dbSendQuery(jdbcConnection,paste0("SELECT * FROM BI_SALES_PROVIDERS_STATUS WHERE OPERATOR_ID=",operatorId,"AND SALES_MONTH ='",input$dataLoadMonthInput2,"' AND SALES_YEAR=",input$dataLoadYearInput2," AND CATEGORY_ID=",categoryId))
    rowCheck <- dbFetch(rowCheck)
    providers <- providers[!(providers[,1] %in% rowCheck$PROVIDER_ID),]
    colnames(providers)<-c("PROVIDER_ID", "CATEGORY_ID", "SALES_YEAR", 
                           "SALES_MONTH", "PROVIDER_STATUS", "OPERATOR_ID")
    done <- tryCatch(dbWriteTable(jdbcConnection, "BI_SALES_PROVIDERS_STATUS",providers,append = TRUE, row.names = FALSE, overwrite = FALSE),error = function(e) {return(FALSE)})
    dbDisconnect(jdbcConnection)
    showNotification("Data now available for all providers!")
  }
  else{
    showNotification("Data is already available for providers!")
  }
},ignoreInit = TRUE,ignoreNULL = TRUE)
observeEvent(input$unmakeAllBtn,{
  if(is.null(input$dataLoadCategory2) || is.null(input$dataLoadYearInput2) || is.null(input$dataLoadMonthInput2)|| is.null(input$dataLoadOperatorName2))
  {
    showNotification("Please fill the fields!")
    return()
  }
  showModal(modalDialog(title = "Attention",easyClose = TRUE,size='m',renderUI(fluidRow(column(8,h5("Are you sure you want to delete?",style="font-weight:700;")),column(4,div(style="text-align:right;",actionButton("unmakeAllConfirmedBtn","Yes")))))))
},ignoreInit = TRUE,ignoreNULL = TRUE)
observeEvent(input$unmakeAllConfirmedBtn,{
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  dbSendUpdate(jdbcConnection,paste0("DELETE FROM BI_SALES_PROVIDERS_STATUS WHERE OPERATOR_ID =",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName2]," 
                                                  AND CATEGORY_ID =",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory2]," AND SALES_YEAR='",input$dataLoadYearInput2,"' AND SALES_MONTH='",input$dataLoadMonthInput2,"'"))
  dbSendUpdate(jdbcConnection,paste0("DELETE FROM BI_SALES_MONTHLY_REVENUES WHERE OPERATOR_ID =",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName2]," 
                                                  AND MAIN_CATEGORY_ID =",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory2]," AND SALES_YEAR='",input$dataLoadYearInput2,"' AND SALES_MONTH='",input$dataLoadMonthInput2,"'"))
  showNotification("Deletion done successfully!")
  dbDisconnect(jdbcConnection)
},ignoreInit = TRUE,ignoreNULL = TRUE)
observeEvent(input$monthClosingBtn2,{
  if(is.null(input$dataLoadCategory2) || is.null(input$dataLoadYearInput2) || is.null(input$dataLoadMonthInput2)|| is.null(input$dataLoadOperatorName2))
  {
    showNotification("Please fill the fields!")
    return()
  }
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  check <- dbSendQuery(jdbcConnection,paste0("SELECT count(*) FROM BI_SALES_MONTHLY_REVENUES WHERE SALES_MONTH = '",input$dataLoadMonthInput2,"' AND SALES_YEAR='",input$dataLoadYearInput2,"' 
           AND MAIN_CATEGORY_ID =",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory2]," AND OPERATOR_ID=",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName2]))
  check <- dbFetch(check)
  if(check[1,1] == 0)
  {
    data <- dbSendQuery(jdbcConnection,paste0("SELECT  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') AS MONTH,SUM(  MS.GN4ME_REVENUE)  AS GN4ME_REV,SUM(  PV.PROVIDER_REVENUE)  AS PROV_REV
    FROM BI_SALES_MYPROVIDER_D PV INNER JOIN BI_SALES_MYSERVICES MS
    ON( PV.SERVICE_ID = MS.SERVICE_ID AND PV.PRODUCT_ID=MS.PRODUCT_ID AND PV.SERVICE_DATE = MS.SERVICE_DATE AND PV.OPERATOR_ID = MS.OPERATOR_ID)
    INNER JOIN BI_SALES_SERVICE S ON MS.SERVICE_ID = S.SERVICE_ID
    INNER JOIN BI_SALES_PROVIDER_SERVICES PS ON ( PV.SERVICE_ID = PS.SERVICE_ID AND PS.PROVIDER_ID = PV.PROVIDER_ID)
    INNER JOIN BI_SALES_PROVIDERS P ON P.PROVIDER_ID = PS.PROVIDER_ID
    INNER JOIN BI_SALES_CATEGORY C ON C.CATEGORY_ID =  S.SERVICE_CATEGORY
    WHERE PV.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName2]," AND to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') ='",input$dataLoadMonthInput2,"'
    AND S.SERVICE_CATEGORY=",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory2]," AND to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",input$dataLoadYearInput2,"'
    group by  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM')
    order by  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM')"))
    data <- dbFetch(data)
    if(nrow(data) > 0)
    {
      data <- cbind(input$dataLoadYearInput2,data)
      data <- cbind(data,dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory2])
      data <- cbind(data,dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName2])
      colnames(data) <- c("SALES_YEAR","SALES_MONTH", "MONTHLY_GN4ME_REVENUE", "MONTHLY_GN4ME_COST", "MAIN_CATEGORY_ID", "OPERATOR_ID")

      done <- tryCatch(dbWriteTable(jdbcConnection, "BI_SALES_MONTHLY_REVENUES",data,append = TRUE, row.names = FALSE, overwrite = FALSE),error = function(e) {return(FALSE)})
      dbDisconnect(jdbcConnection)
      if(as.character(done) == "FALSE")
      {
        showNotification("Month Closing data wasn't added!")
      }
      else{
        showNotification("Month Closing data was added successfully!")
      }
    }
    else{
      showNotification("No data to add!")
    }
  }
  else{
    dbDisconnect(jdbcConnection)
    showNotification("Data already exists!")
  }
    
},ignoreInit = TRUE,ignoreNULL = TRUE)
observeEvent(input$monthClosingDeletionButton,{
  if(is.null(input$dataLoadCategory2) || is.null(input$dataLoadYearInput2) || is.null(input$dataLoadMonthInput2)|| is.null(input$dataLoadOperatorName2))
  {
    showNotification("Please fill the fields!")
    return()
  }
  showModal(modalDialog(title = "Attention",easyClose = TRUE,size='m',renderUI(fluidRow(column(8,h5("Are you sure you want to Unlock?",style="font-weight:700;")),column(4,div(style="text-align:right;",actionButton("monthClosingDeletionConfirmedButton","Yes")))))))
  
},ignoreInit = TRUE,ignoreNULL = T)
observeEvent(input$monthClosingDeletionConfirmedButton,{
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  done <- tryCatch(dbSendUpdate(jdbcConnection,paste0("DELETE FROM BI_SALES_MONTHLY_REVENUES WHERE OPERATOR_ID =",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName2],"
                                                      AND MAIN_CATEGORY_ID =",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory2]," AND SALES_MONTH ='",input$dataLoadMonthInput2,"' AND SALES_YEAR ='",input$dataLoadYearInput2,"'")),error = function(e) {return(FALSE)})
  if(!is.null(done))
  {
    showNotification("Deletion process failed!")
    return()
  }
  showNotification("Unlocked successfully!")
  dbDisconnect(jdbcConnection)
},ignoreInit = TRUE,ignoreNULL = TRUE)