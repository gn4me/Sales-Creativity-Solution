dynamicRdCategory <- reactive(input$lookUpCatgory)
observeEvent(dynamicRdCategory(),{
  if(input$lookUpCatgory == "")
  {
    return()
  }
  else if(input$lookUpCatgory == "ALERTS")
  {
    rdDf <- c("List of providers","List of services","List of products","List of providers' services")
  }
  else if(input$lookUpCatgory == "INTERACTIVE-EGYPT LIKES")
  {
    rdDf <- c("List of short codes","List of services","List of products","List of providers' services")
  }
  else{
    return()
  }
  output$rdOp <- renderUI(radioButtons("rdLOV",NULL,choices = rdDf,selected =rdDf[1]))
})
observeEvent(input$lookUpBtn,{
  if(is.null(input$lookUpCatgory) || is.null(input$lookUpOperators) || is.null(input$rdLOV))
  {
    showNotification("Please fill the fields!")
    return()
  }
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  lookupDf <- data.frame()
  withProgress(message = "Loading ..",value = 0,{
    incProgress(1/4)
    if(input$lookUpCatgory == "ALERTS" && input$rdLOV == "List of providers")
    {
      lookupDf <- dbSendQuery(jdbcConnection,paste0("SELECT DISTINCT P.PROVIDER_NAME ",shQuote("Provider Name"),",(P.PROVIDER_SHARE)*100 AS ",shQuote("Provider Share (%)")," FROM BI_SALES_PROVIDERS P INNER JOIN BI_SALES_PROVIDER_SERVICES
                                                    PS ON P.PROVIDER_ID=PS.PROVIDER_ID INNER JOIN BI_SALES_SERVICE S 
                                                    ON PS.SERVICE_ID = S.SERVICE_ID 
                                                    INNER JOIN BI_SALES_OPERATOR_SERVICES BOS on BOS.SERVICE_ID = S.SERVICE_ID
                                                    WHERE S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$lookUpCatgory]," 
                                                    AND BOS.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$lookUpOperators]," ORDER BY P.PROVIDER_NAME"))
      incProgress(1/4)
      lookupDf <- dbFetch(lookupDf)
      output$lookupHeader <- renderUI(h5(paste("Data for Category:",input$lookUpCatgory,"& Operator:",input$lookUpOperators),style="font-weight: 700;color:white;"))
      lookupDf <- DT::datatable(lookupDf,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),lengthMenu=sort(c(10,15,nrow(lookupDf)))))
      incProgress(1/4)
      output$lookupListTables <- renderUI({
        output$x <- DT::renderDataTable(lookupDf)
        dataTableOutput("x")})
    }
    else if(input$lookUpCatgory == "INTERACTIVE-EGYPT LIKES" && input$rdLOV == "List of short codes")
    {
      incProgress(1/4)
      lookupDf <- dbSendQuery(jdbcConnection,paste0("SELECT DISTINCT S.SHORT_CODE, S.CHANNEL,S.SC_PRICE 
                                                    FROM BI_SALES_SHORT_CODE S INNER JOIN BI_SALES_SERVICE SS 
                                                    ON S.SC_CATEGORY = SS.SERVICE_CATEGORY
                                                    INNER JOIN BI_SALES_OPERATOR_SERVICES BOS on BOS.SERVICE_ID = SS.SERVICE_ID
                                                    WHERE SS.SERVICE_CATEGORY =",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$lookUpCatgory]," 
                                                    AND BOS.OPERATOR_ID =",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$lookUpOperators]))
      lookupDf <- dbFetch(lookupDf)
      output$lookupHeader <- renderUI(h5(paste("Data for Category:",input$lookUpCatgory,"& Operator:",input$lookUpOperators),style="font-weight: 700;color:white;"))
      lookupDf <- DT::datatable(lookupDf,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),lengthMenu=sort(c(10,15,nrow(lookupDf)))))
      incProgress(1/4)
      output$lookupListTables <- renderUI({
        output$x <- DT::renderDataTable(lookupDf)
        dataTableOutput("x")})
    }
    else if(input$rdLOV == "List of services")
    {
      lookupDf <- dbSendQuery(jdbcConnection,paste0("SELECT DISTINCT S.SERVICE_NAME AS ",shQuote("Service Name"),",S.SERVICE_FORMAT AS ",shQuote("Service Format")," FROM BI_SALES_SERVICE S 
                INNER JOIN BI_SALES_OPERATOR_SERVICES BOS on BOS.SERVICE_ID = S.SERVICE_ID                                                    
                WHERE S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$lookUpCatgory]," 
                AND BOS.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$lookUpOperators]," ORDER BY S.SERVICE_NAME"))
      incProgress(1/4)
      lookupDf <- dbFetch(lookupDf)
      output$lookupHeader <- renderUI(h5(paste("Data for Category:",input$lookUpCatgory,"& Operator:",input$lookUpOperators),style="font-weight: 700;color:white;"))
      lookupDf <- DT::datatable(lookupDf,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),lengthMenu=sort(c(10,15,nrow(lookupDf)))))
      incProgress(1/4)
      output$lookupListTables <- renderUI({
        output$x <- DT::renderDataTable(lookupDf)
        dataTableOutput("x")})
    }
    else if(input$rdLOV == "List of products")
    {
      lookupDf <- dbSendQuery(jdbcConnection,paste0("SELECT DISTINCT S.SERVICE_NAME,P.PRODUCT_ID AS ",shQuote("Product ID"),",P.PRODUCT_NAME AS ",shQuote("Product Name"),",P.CHARGE_TYPE AS ",shQuote("Charge Type")," from BI_SALES_SERVICE S INNER JOIN BI_SALES_PRODUCT P ON P.SERVICE_ID=S.SERVICE_ID 
INNER JOIN BI_SALES_OPERATOR_SERVICES BOS on BOS.SERVICE_ID = S.SERVICE_ID                                                     
WHERE S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$lookUpCatgory]," 
                                                    AND BOS.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$lookUpOperators]," ORDER BY S.SERVICE_NAME"))
      incProgress(1/4)
      lookupDf <- dbFetch(lookupDf)
      if(nrow(lookupDf) > 0)
      {
        services<-as.data.frame(unique(lookupDf$SERVICE_NAME),stringsAsFactors=FALSE)
        output$lookupListTables <- renderUI({
          lapply(1:nrow(services), function(i) {
            DT::dataTableOutput(paste0('L', i))
            temp<-lookupDf[lookupDf$SERVICE_NAME==services[i,1],]
            tempH <- paste0("Service Name: ",temp[1,1])
            temp <- temp[,2:4]
            rownames(temp) <- NULL
            temp <- DT::datatable(temp,caption = tempH,options = list(bFilter=FALSE,lengthMenu=sort(c(10,15,nrow(temp)))))
            name<-paste0('L', i)
            output[[name]] <- DT::renderDataTable({temp})
          })
        })
      }
      incProgress(1/4)
    }
    else if(input$rdLOV == "List of providers' services")
    {
      lookupDf <- dbSendQuery(jdbcConnection,paste0("SELECT DISTINCT P.PROVIDER_NAME,S.SERVICE_NAME AS ",shQuote("Service Name"),",S.SERVICE_FORMAT AS ",shQuote("Service Format")," 
                                                    FROM BI_SALES_PROVIDERS P INNER JOIN BI_SALES_PROVIDER_SERVICES PS 
                                                    ON P.PROVIDER_ID=PS.PROVIDER_ID INNER JOIN BI_SALES_SERVICE S ON PS.SERVICE_ID = S.SERVICE_ID 
INNER JOIN BI_SALES_OPERATOR_SERVICES BOS on BOS.SERVICE_ID = S.SERVICE_ID                                                    
WHERE S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$lookUpCatgory]," 
                                                    AND BOS.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$lookUpOperators]," ORDER BY P.PROVIDER_NAME"))
      incProgress(1/4)
      lookupDf <- dbFetch(lookupDf)
      if(nrow(lookupDf) > 0)
      {
        providers<-as.data.frame(unique(lookupDf$PROVIDER_NAME),stringsAsFactors=FALSE)
        output$lookupListTables <- renderUI({
          lapply(1:nrow(providers), function(i) {
            DT::dataTableOutput(paste0('L', i))
            temp<-lookupDf[lookupDf$PROVIDER_NAME==providers[i,1],]
            tempH <- paste0("Provider Name: ",temp[1,1])
            temp <- temp[,2:3]
            rownames(temp) <- NULL
            temp <- DT::datatable(temp,caption = tempH,options = list(bFilter=FALSE,lengthMenu=sort(c(10,15,nrow(temp)))))
            name<-paste0('L', i)
            output[[name]] <- DT::renderDataTable({temp})
          })
        })
      }
      incProgress(1/4)
    }
    else{
      lookupDf <- data.frame(Result = "There's no Data available.")
      lookupDf <- DT::datatable(lookupDf)
      output$lookupListTables <- renderUI({
        output$x <- DT::renderDataTable(lookupDf)
        dataTableOutput("x")})
    }
  })
  dbDisconnect(jdbcConnection)
},ignoreInit = TRUE,ignoreNULL = TRUE)

observeEvent(input$transactionBtn,{
  if(is.null(input$transactionCatgory) || is.null(input$transactionYear) ||is.null(input$transactionOperators) || is.null(input$dataShowRadioBtns))
  {
    showNotification("Please fill the fields!")
    return()
  }
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  if(input$dataShowRadioBtns == "Monthly transactions")
  {
    monthlyTransData <- dbSendQuery(jdbcConnection,paste0("SELECT to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') AS ",shQuote("Month"),",
 SUM( MS.GN4ME_REVENUE)  AS ",shQuote("GN4ME Revenue")," FROM BI_SALES_MYPROVIDER_D PV INNER JOIN BI_SALES_MYSERVICES MS
 ON( PV.SERVICE_ID = MS.SERVICE_ID AND PV.PRODUCT_ID=MS.PRODUCT_ID AND PV.SERVICE_DATE = MS.SERVICE_DATE AND PV.OPERATOR_ID = MS.OPERATOR_ID)
 INNER JOIN BI_SALES_SERVICE S ON MS.SERVICE_ID = S.SERVICE_ID
 WHERE PV.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$transactionOperators]," AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$transactionCatgory],"
 AND to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') = '",input$transactionYear,"'
  group by to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM'),  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') 
 order by  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM')"))
    monthlyTransData <- dbFetch(monthlyTransData)
    if(nrow(monthlyTransData) > 0)
    {
      monthlyTransData$`GN4ME Revenue` <- round(monthlyTransData$`GN4ME Revenue`,digits = 2)
      #monthlyTransData$`GN4ME Revenue`<- prettyNum(monthlyTransData$`GN4ME Revenue`, big.mark=",")
      sums <- as.data.frame(t(colSums(monthlyTransData[sapply(monthlyTransData, is.numeric)], na.rm = TRUE)))
      sums[,1]<-round(sums[,1],digits = 2)
      sums[,1]<-prettyNum(sums[,1],big.mark = ",")
      sums1 <- cbind(" "=" ","Month"=" ",sums)
      sums <- cbind("Total" = "Total"," "=" ",sums)
      names(sums)<- as.character(apply(sums[1, ], 1, paste))
      sketch = htmltools::withTags(table(list(tableHeader(sums1),tableFooter(sums))))
      rownames(monthlyTransData) <- NULL
      monthlyTransData <-DT::datatable(monthlyTransData,container = sketch,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),bFilter=FALSE,lengthMenu=sort(c(10,15,nrow(monthlyTransData)))))
      
      output$transactionTables <- renderUI({
        output$y <- DT::renderDataTable(monthlyTransData)
        dataTableOutput("y")})
    }
    else{
      monthlyTransData <- data.frame(Result="No data available")
      monthlyTransData <-DT::datatable(monthlyTransData,options = list(bFilter=FALSE))
      output$transactionTables <- renderUI({
        output$y <- DT::renderDataTable(monthlyTransData)
        dataTableOutput("y")})
    }
    dbDisconnect(jdbcConnection)
  }
  else if(input$dataShowRadioBtns == "Monthly revenues per service")
  {
    monthlyTransData1 <- dbSendQuery(jdbcConnection,paste0("SELECT S.SERVICE_NAME AS ",shQuote("Service Name"),",P.PROVIDER_NAME AS ",shQuote("Provider Name"),", to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') AS ",shQuote("Month"),",
    SUM(  MS.GN4ME_REVENUE)  AS ",shQuote("GN4ME Revenue"),"
    FROM BI_SALES_MYPROVIDER_D PV INNER JOIN BI_SALES_MYSERVICES MS
    ON( PV.SERVICE_ID = MS.SERVICE_ID AND PV.PRODUCT_ID=MS.PRODUCT_ID AND PV.SERVICE_DATE = MS.SERVICE_DATE AND PV.OPERATOR_ID = MS.OPERATOR_ID)
    INNER JOIN BI_SALES_SERVICE S ON MS.SERVICE_ID = S.SERVICE_ID
    INNER JOIN BI_SALES_PROVIDER_SERVICES PS ON ( PV.SERVICE_ID = PS.SERVICE_ID AND PS.PROVIDER_ID = PV.PROVIDER_ID)
    INNER JOIN BI_SALES_PROVIDERS P ON P.PROVIDER_ID = PS.PROVIDER_ID
    WHERE PV.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$transactionOperators]," AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$transactionCatgory]," 
    AND  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",input$transactionYear,"'
    group by  P.PROVIDER_NAME,S.SERVICE_NAME, to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM'),  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY')  
    order by  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM')"))
    monthlyTransData1 <- dbFetch(monthlyTransData1)
    
    monthlyTransData2 <- dbSendQuery(jdbcConnection,paste0("SELECT P.PROVIDER_NAME AS ",shQuote("Provider Name"),", to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') AS ",shQuote("Month"),",
    SUM(  PV.PROVIDER_REVENUE)  AS ",shQuote("Provider Revenue"),"
    FROM BI_SALES_MYPROVIDER_D PV INNER JOIN BI_SALES_MYSERVICES MS
    ON( PV.SERVICE_ID = MS.SERVICE_ID AND PV.PRODUCT_ID=MS.PRODUCT_ID AND PV.SERVICE_DATE = MS.SERVICE_DATE AND PV.OPERATOR_ID = MS.OPERATOR_ID)
    INNER JOIN BI_SALES_SERVICE S ON MS.SERVICE_ID = S.SERVICE_ID
    INNER JOIN BI_SALES_PROVIDER_SERVICES PS ON ( PV.SERVICE_ID = PS.SERVICE_ID AND PS.PROVIDER_ID = PV.PROVIDER_ID)
    INNER JOIN BI_SALES_PROVIDERS P ON P.PROVIDER_ID = PS.PROVIDER_ID
    WHERE PV.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$transactionOperators]," AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$transactionCatgory]," 
    AND  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",input$transactionYear,"'
    group by  P.PROVIDER_NAME, to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM'),  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY')  
    order by  to_char(TO_DATE(PV.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM')"))
    monthlyTransData2 <- dbFetch(monthlyTransData2)
    
    monthlyTransData3 <- dbSendQuery(jdbcConnection,paste0("select SALES_MONTH as ",shQuote("Month"),", round((MONTHLY_GN4ME_REVENUE-MONTHLY_GN4ME_COST),2) AS ",shQuote("Net Profit"),",round(((MONTHLY_GN4ME_REVENUE-MONTHLY_GN4ME_COST)/ MONTHLY_GN4ME_REVENUE)*100,2) as ",shQuote("Profit (%)"),",round(MONTHLY_GN4ME_COST,2) as ",shQuote("Cost"),",round((MONTHLY_GN4ME_COST/MONTHLY_GN4ME_REVENUE)*100,2) AS ",shQuote("Cost %")," from BI_SALES_MONTHLY_REVENUES
   WHERE MAIN_CATEGORY_ID = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$transactionCatgory]," AND OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$transactionOperators]," AND SALES_YEAR ='",input$transactionYear,"'"))
    monthlyTransData3 <- dbFetch(monthlyTransData3)
    
    if(nrow(monthlyTransData1) > 0 && nrow(monthlyTransData2) > 0)
    {
      monthlyTransData1 <- reshape(monthlyTransData1,idvar=c("Service Name","Provider Name"),timevar="Month",direction = "wide")
      names(monthlyTransData1) <- gsub("GN4ME Revenue.", "", names(monthlyTransData1))
      monthlyTransData1[is.na(monthlyTransData1)] <- 0
      monthlyTransData1 <- cbind(monthlyTransData1,Total=rowSums(monthlyTransData1[sapply(monthlyTransData1, is.numeric)], na.rm = TRUE))
      sums <- as.data.frame(t(colSums(monthlyTransData1[sapply(monthlyTransData1, is.numeric)], na.rm = TRUE)))
      for(i in 1:length(names(sums)))
      {
        sums[,i]<-prettyNum(sums[,i],big.mark = ",")
      }
      sums1 <- cbind(" "=" ","Service Name"=" ","Provider Name"=" ",sums)
      sums <- cbind("Total" = "Total"," "="  "," "="  ", sums)
      names(sums)<- as.character(apply(sums[1, ], 1, paste))
      sketch = htmltools::withTags(table(list(tableHeader(sums1),tableFooter(sums))))
      for(i in 3:length(names(monthlyTransData1)))
      {
        monthlyTransData1[,i] <- round(monthlyTransData1[,i],digits = 2)
        #monthlyTransData1[,i] <- prettyNum(monthlyTransData1[,i], big.mark=",")
      }
      rownames(monthlyTransData1) <- NULL
      monthlyTransData1 <-DT::datatable(monthlyTransData1,caption = "Monthly revenues per service.",container=sketch,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),lengthMenu=sort(c(10,15,nrow(monthlyTransData1)))))
      
      monthlyTransData2 <- reshape(monthlyTransData2,idvar=c("Provider Name"),timevar="Month",direction = "wide")
      names(monthlyTransData2) <- gsub("Provider Revenue.", "", names(monthlyTransData2))
      monthlyTransData2[is.na(monthlyTransData2)] <- 0
      monthlyTransData2 <- cbind(monthlyTransData2,Total=rowSums(monthlyTransData2[sapply(monthlyTransData2, is.numeric)], na.rm = TRUE))
      sums <- as.data.frame(t(colSums(monthlyTransData2[sapply(monthlyTransData2, is.numeric)], na.rm = TRUE)))
      for(i in 1:length(names(sums)))
      {
        sums[,i]<-prettyNum(sums[,i],big.mark = ",")
      }
      sums1 <- cbind(" "=" ","Provider Name"=" ",sums)
      sums <- cbind("Total" = "Total"," "="  ", sums)
      names(sums)<- as.character(apply(sums[1, ], 1, paste))
      sketch2 = htmltools::withTags(table(list(tableHeader(sums1),tableFooter(sums))))
      for(i in 2:length(names(monthlyTransData2)))
      {
        monthlyTransData2[,i] <- round(monthlyTransData2[,i],digits = 2)
        #monthlyTransData2[,i] <- prettyNum(monthlyTransData2[,i], big.mark=",")
      }
      rownames(monthlyTransData2) <- NULL
      monthlyTransData2 <-DT::datatable(monthlyTransData2,caption = "Monthly cost per service.",container=sketch2,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),lengthMenu=sort(c(10,15,nrow(monthlyTransData2)))))
      
      sums <- as.data.frame(t(colSums(monthlyTransData3[sapply(monthlyTransData3, is.numeric)], na.rm = TRUE)))
      for(i in 1:length(names(sums)))
      {
        sums[,i]<-prettyNum(sums[,i],big.mark = ",")
      }
      sums[1,2]<- 0
      sums[1,4]<-0
      sums1 <- cbind(" "=" ","Month"=" ",sums)
      sums <- cbind("Total" = "Total"," "="  ", sums)
      names(sums)<- as.character(apply(sums[1, ], 1, paste))
      sketch2 = htmltools::withTags(table(list(tableHeader(sums1),tableFooter(sums))))
      # for(i in 2:length(names(monthlyTransData3)))
      # {
      #   monthlyTransData3[,i] <- prettyNum(monthlyTransData3[,i], big.mark=",")
      # }
      rownames(monthlyTransData3) <- NULL
      monthlyTransData3 <-DT::datatable(monthlyTransData3,caption = "Monthly net profit",container=sketch2,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),bFilter=FALSE,lengthMenu=sort(c(10,15,nrow(monthlyTransData3)))))
      
      output$std1 <- DT::renderDataTable({monthlyTransData1})
      output$std2 <- DT::renderDataTable({monthlyTransData2})
      output$std3 <- DT::renderDataTable({monthlyTransData3})
      
      output$transactionTables <- renderUI({
        lapply(1:3,function(i){
          DT::dataTableOutput(paste0("std",i))
        })
      })
    }
    else{
      monthlyTransData <- data.frame(Result="No data available")
      monthlyTransData <-DT::datatable(monthlyTransData,options = list(bFilter=FALSE))
      output$transactionTables <- renderUI({
        output$y <- DT::renderDataTable(monthlyTransData)
        dataTableOutput("y")})
    }
    dbDisconnect(jdbcConnection)
  }
},ignoreInit = TRUE,ignoreNULL = TRUE)