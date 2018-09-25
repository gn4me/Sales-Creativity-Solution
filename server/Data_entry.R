
#categoryReact <- reactive(input$dataEntryCategory)
productsLOV<-data.frame()
dataEntryProductsNamesLOV <- data.frame()
observeEvent(c(input$dataEntryCategory,input$dataEntryOperatorName),{
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  productsLOV <- dbSendQuery(jdbcConnection,paste0(" SELECT P.* FROM BI_SALES_PRODUCT P 
                                                   INNER JOIN BI_SALES_OPERATOR_SERVICES S ON P.SERVICE_ID = S.SERVICE_ID 
                                                  INNER JOIN BI_SALES_SERVICE SS ON SS.SERVICE_ID = S.SERVICE_ID
                                                   WHERE SS.SERVICE_CATEGORY =",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataEntryCategory]
                                                   ," AND S.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataEntryOperatorName],"ORDER BY P.PRODUCT_NAME"))
  productsLOV <-dbFetch(productsLOV)
  productsLOV <<-productsLOV
  productsLOV$PRODUCT_NAME <- as.character(productsLOV$PRODUCT_NAME)
  dataEntryProductsNamesLOV <<- as.data.frame(unique(productsLOV$PRODUCT_NAME),stringsAsFactors = FALSE)
  colnames(dataEntryProductsNamesLOV) <<- c("Products")
  dbDisconnect(jdbcConnection)
  updateSelectizeInput(session,"dataEntryProductNames","Product:",dataEntryProductsNamesLOV,selected = dataEntryProductsNamesLOV[1,1])
})

reflectiveDataEntry <- data.frame()
reflectiveDataEntryD <- data.frame()
observeEvent(input$displayDataEntryButton,{
  if(input$dataEntryYearInput == "" && input$dataEntryMonthInput == "" && input$dataEntryOperatorName == "" && input$dataEntryCategory =="" || input$dataEntryProductNames == "" || input$chargeTypeLOV == "" || input$totalRevInput == "" || input$postPaidInput == "" || input$prePaidInput =="" || input$gn4meRevInput == "")
  {
    showNotification("Please fill in the fields!")
    return()
  }
  withProgress(message = "Adding ..",value = 0,{
    incProgress(1/4)
    jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
    my_share <- dbSendQuery(jdbcConnection,paste0("SELECT MY_SHARE FROM BI_SALES_MYSHARES WHERE CURRENT_FLAG=1 AND OPERATOR_ID=",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataEntryOperatorName]," AND CATEGORY_ID=",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataEntryCategory]))
    my_share <- dbFetch(my_share)
    my_share <- my_share[1,1]
    incProgress(1/4)
    if(identical(productsLOV$PRODUCT_ID[productsLOV$PRODUCT_NAME == input$dataEntryProductNames & productsLOV$CHARGE_TYPE == input$chargeTypeLOV],numeric(0)))
    {
      showNotification("This product doesn't exist.")
      return()
    }
    temp <- data.frame(SERVICE_ID=productsLOV$SERVICE_ID[(productsLOV$PRODUCT_NAME == input$dataEntryProductNames & productsLOV$CHARGE_TYPE == input$chargeTypeLOV)]
                       ,PRODUCT_ID=productsLOV$PRODUCT_ID[productsLOV$PRODUCT_NAME == input$dataEntryProductNames & productsLOV$CHARGE_TYPE == input$chargeTypeLOV]
                       ,SERVICE_DATE=paste0("01-",input$dataEntryMonthInput,"-",input$dataEntryYearInput)
                        ,TOTAL_REVENUE=as.numeric(input$totalRevInput), POST_PAID=as.numeric(input$postPaidInput),PRE_PAID=as.numeric(input$prePaidInput),
                        GN4ME_REVENUE=as.numeric(input$gn4meRevInput),SC_TOT_SMS=NA,SC_MONTHLY_COST=NA,RBT_FLEX_ACT_COUNT=NA,RBT_FLEX_ACT_REV=NA,
                        RBT_FLEX_REN_COUNT=NA, RBT_FLEX_REN_REV=NA, RBT_NFLEX_ACT_COUNT=NA, RBT_NFLEX_ACT_REV=NA,
                        RBT_NFLEX_REN_COUNT=NA, RBT_NFLEX_REN_REV=NA, CALCULATED_GN4ME_REVENUE=((as.numeric(input$totalRevInput)*my_share)/100)
                        ,OPERATOR_ID=dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataEntryOperatorName])
  if(nrow(reflectiveDataEntry) > 0)
  {
    library(plyr)
    check <- nrow(match_df(reflectiveDataEntry, temp,on=c("SERVICE_ID","PRODUCT_ID","SERVICE_DATE","OPERATOR_ID")))
    if(check == 0)
    {
      reflectiveDataEntry <<- rbind(reflectiveDataEntry,temp)
    }
    else{
      showNotification("You already entered this record!")
      dbDisconnect(jdbcConnection)
      return()
    }
  }
  else{
    reflectiveDataEntry <<- rbind(reflectiveDataEntry,temp)
  }
  temp<-NULL
  provider_lookup <- dbSendQuery(jdbcConnection,"SELECT  * FROM BI_SALES_PROVIDER_SERVICES")
  provider_lookup <- dbFetch(provider_lookup)
  
  temp <- data.frame(PROVIDER_ID=provider_lookup$PROVIDER_ID[match(productsLOV$SERVICE_ID[(productsLOV$PRODUCT_NAME == input$dataEntryProductNames & productsLOV$CHARGE_TYPE == input$chargeTypeLOV)], provider_lookup$SERVICE_ID)],
                     SERVICE_ID=productsLOV$SERVICE_ID[(productsLOV$PRODUCT_NAME == input$dataEntryProductNames & productsLOV$CHARGE_TYPE == input$chargeTypeLOV)],
                     PRODUCT_ID=productsLOV$PRODUCT_ID[productsLOV$PRODUCT_NAME == input$dataEntryProductNames & productsLOV$CHARGE_TYPE == input$chargeTypeLOV],
                     SERVICE_DATE=paste0("01-",input$dataEntryMonthInput,"-",input$dataEntryYearInput),
                     PROVIDER_REVENUE=as.numeric(input$gn4meRevInput), MONTHLY_COST=NA,OPERATOR_ID=dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataEntryOperatorName])
  if(nrow(reflectiveDataEntryD) > 0)
  {
    library(plyr)
    check <- nrow(match_df(reflectiveDataEntryD, temp,on=c("SERVICE_ID","PRODUCT_ID","SERVICE_DATE","OPERATOR_ID")))
    if(check == 0)
    {
      reflectiveDataEntryD <<- rbind(reflectiveDataEntryD,temp)
    }
    else{
      showNotification("You already entered this record!")
      provider_lookup <- NULL
      dbDisconnect(jdbcConnection)
      return()
    }
  }
  else{
    reflectiveDataEntryD <<- rbind(reflectiveDataEntryD,temp)
  }
  temp<-NULL
  provider_lookup <- NULL
  dbDisconnect(jdbcConnection)
  data <- DT::datatable(reflectiveDataEntry,options = list(scrollX = TRUE,scrollCollapse = TRUE,lengthMenu=sort(c(10,15,nrow(reflectiveDataEntry)))))
  output$dataEntryDisplayTable <- DT::renderDataTable(data)
  })
},ignoreInit = TRUE,ignoreNULL = TRUE)
observeEvent(input$dataEntryBtn,{
  if(input$dataEntryYearInput == "" && input$dataEntryMonthInput == "" && input$dataEntryOperatorName == "" && input$dataEntryCategory =="" || input$dataEntryProductNames == "" || input$chargeTypeLOV == "" || input$totalRevInput == "" || input$postPaidInput == "" || input$prePaidInput =="" || input$gn4meRevInput == "")
  {
    showNotification("Please fill in the fields!")
    return()
  }
  if(nrow(reflectiveDataEntry) == 0)
  {
    showNotification("Please enter the data first!")
    return()
  }
  withProgress(message = "Adding ..",value = 0,{
    incProgress(1/4)
    jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
    done <- tryCatch(dbWriteTable(jdbcConnection,"BI_SALES_MYSERVICES",reflectiveDataEntry,append = TRUE, row.names = FALSE, overwrite = FALSE),error = function(e) {return(FALSE)})
    if(as.character(done) == "FALSE")
    {
      dbDisconnect(jdbcConnection)
      showNotification("Database wasn't updated")
      return()
    }
    incProgress(1/4)
    done <- tryCatch(dbWriteTable(jdbcConnection,"BI_SALES_MYPROVIDER_D",reflectiveDataEntryD,append = TRUE, row.names = FALSE, overwrite = FALSE),error = function(e) {return(FALSE)})
    
    if(as.character(done) == "FALSE")
    {
      dbDisconnect(jdbcConnection)
      showNotification("Database wasn't updatde")
      return()
    }
    # serviceName <- dbSendQuery(jdbcConnection,paste0("SELECT S.SERVICE_NAME,C.SUBCATEGORY_NAME 
    #                                                  FROM BI_SALES_SERVICE S INNER JOIN BI_SALES_SUBCATEGORY C ON (S.SERVICE_CATEGORY = C.CATEGORY_ID AND S.SERVICE_SUBCATEGORY = C.SUBCATEGORY_ID)
    #                                                  WHERE S.SERVICE_ID = ",productsLOV$SERVICE_ID[(productsLOV$PRODUCT_NAME == input$dataEntryProductNames & productsLOV$CHARGE_TYPE == input$chargeTypeLOV)]))
    # serviceName <- dbFetch(serviceName)
    # dbDisconnect(jdbcConnection)
    # dataf <- data.frame("Service Date"=paste0("01-",input$dataEntryMonthInput,"-",input$dataEntryYearInput),"Product ID"=productsLOV$PRODUCT_ID[productsLOV$PRODUCT_NAME == input$dataEntryProductNames & productsLOV$CHARGE_TYPE == input$chargeTypeLOV]
    #                     ,"Product Name"=input$dataEntryProductNames,"Charge Type"=input$chargeTypeLOV,"Service Name"=serviceName[1,1],"Subcategory Name"=serviceName[1,2],"Total Revenue"=as.numeric(input$totalRevInput),"Post Paid"=as.numeric(input$postPaidInput),"Pre Paid"=as.numeric(input$prePaidInput),"GN4ME Revenue"=as.numeric(input$gn4meRevInput),"Calculated GN4ME Revenue"=((as.numeric(input$totalRevInput)*my_share)/100))
    # dataf <- DT::datatable(dataf,options = list(scrollX = TRUE,scrollCollapse = TRUE,lengthMenu=sort(c(10,15,nrow(dataf)))))
    incProgress(1/4)
    if(is.null(done)){
      showNotification("Database updated successfully!")
      #output$dataEntryDisplayTable <- DT::renderDataTable(dataf)
    }
  })
  
},ignoreInit = TRUE,ignoreNULL = TRUE)

observeEvent(input$deleteDataEntryBtn,{
  if(input$dataEntryYearInput == "" && input$dataEntryMonthInput == "" && input$dataEntryOperatorName == "" && input$dataEntryategory =="" || input$dataEntryProductNames == "" || input$chargeTypeLOV == "" )
  {
    showNotification("Please fill in the fields!")
    return()
  }
  showModal(modalDialog(title = "Attention",easyClose = TRUE,size='m',renderUI(fluidRow(column(8,h5("Are you sure you want to delete?",style="font-weight:700;")),column(4,div(style="text-align:right;",actionButton("deleteDataEntryCOnfirmedBtn","Yes")))))))
  
},ignoreInit = TRUE,ignoreNULL = TRUE)

observeEvent(input$deleteDataEntryCOnfirmedBtn,{
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  check <- dbSendQuery(jdbcConnection,paste0("SELECT COUNT(*)  FROM BI_SALES_MYPROVIDER_D D INNER JOIN BI_SALES_MYSERVICES W ON( D.SERVICE_ID = W.SERVICE_ID AND D.SERVICE_DATE = W.SERVICE_DATE AND D.PRODUCT_ID = W.PRODUCT_ID AND D.OPERATOR_ID = W.OPERATOR_ID) INNER JOIN BI_SALES_SERVICE S ON S.SERVICE_ID = W.SERVICE_ID where to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",input$dataEntryMonthInput,"' AND  to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",input$dataEntryYearInput,"' AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataEntryCategory]," AND D.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataEntryOperatorName]))
  check <- dbFetch(check)
  if(check[1,1] == 0)
  {
    showNotification("There is no data to delete!")
  }
  else{
    done <- tryCatch(dbSendUpdate(jdbcConnection,paste0("DELETE FROM BI_SALES_MYSERVICES WHERE OPERATOR_ID =",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataEntryOperatorName]," AND PRODUCT_ID=",productsLOV$PRODUCT_ID[productsLOV$PRODUCT_NAME == input$dataEntryProductNames & productsLOV$CHARGE_TYPE == input$chargeTypeLOV]," AND to_char(TO_DATE(SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM')='",input$dataEntryMonthInput,"' AND to_char(TO_DATE(SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY')='",input$dataEntryYearInput,"' AND SERVICE_ID=",productsLOV$SERVICE_ID[(productsLOV$PRODUCT_NAME == input$dataEntryProductNames & productsLOV$CHARGE_TYPE == input$chargeTypeLOV)])),error = function(e) {return(FALSE)})
    if(!is.null(done))
    {
      showNotification("Deletion Process wasn't done!")
      return()
    }
    done <- tryCatch(dbSendUpdate(jdbcConnection,paste0("DELETE FROM BI_SALES_MYPROVIDER_D WHERE OPERATOR_ID =",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataEntryOperatorName]," AND PRODUCT_ID=",productsLOV$PRODUCT_ID[productsLOV$PRODUCT_NAME == input$dataEntryProductNames & productsLOV$CHARGE_TYPE == input$chargeTypeLOV]," AND to_char(TO_DATE(SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM')='",input$dataEntryMonthInput,"' AND to_char(TO_DATE(SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY')='",input$dataEntryYearInput,"' AND SERVICE_ID=",productsLOV$SERVICE_ID[(productsLOV$PRODUCT_NAME == input$dataEntryProductNames & productsLOV$CHARGE_TYPE == input$chargeTypeLOV)])),error = function(e) {return(FALSE)})
    if(!is.null(done))
    {
      showNotification("Deletion Process wasn't done!")
      return()
    }
    showNotification("Deletion Process done successfully!")
  }
  dbDisconnect(jdbcConnection)
  
},ignoreInit = TRUE,ignoreNULL = TRUE)

