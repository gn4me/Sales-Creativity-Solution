output$insertLookUp <- renderUI(tabsetPanel(id="DataLoadTabset",
                                            tabPanel("Data Load",value = "dataLoadTabPanel",
                                                     fluidRow(column(12,div(fluidRow(column(1,h5("Year ",style="font-weight: 700;")),column(1,selectInput("dataLoadYearInput",NULL,dataLoadYearsLOV,selected = "dataLoadYearInput")),column(1,h5("Month",style="font-weight: 700;")),column(1,selectInput("dataLoadMonthInput",NULL,dataLoadMonthsLOV,selected = "dataLoadMonthInput"))
                                                                                     ,column(1,h5("Category: ",style="font-weight: 700;")),column(3,selectInput("dataLoadCategory",NULL,dataLoadCategoriesLOV,selected = "dataLoadCategory"))
                                                                                     ,column(1,h5("Operator: ",style="font-weight: 700;")),column(3,selectInput("dataLoadOperatorName",NULL,dataLoadOperatorsLOV,selected="dataLoadOperatorName")))
                                                                            ,fluidRow(column(2,h5("Choose Data File",style="font-weight: 700;")),column(4,fileInput("file1", NULL,multiple = FALSE,accept = c('.xlsx'))),column(2,actionButton("dataLoadBtn","Calculate Shares")),column(2,actionButton("displayBtn","Display")),column(2,actionButton("deleteBtn","Delete File")))
                                                                            ,uiOutput("newProducts")
                                                                            ,uiOutput("differentShareOp")
                                                                            ,class="well")))
                                                     ,fluidRow(dataTableOutput("dataLoadDisplayTable"))),
                                            tabPanel("Data Entry",value = "dataEntryTabPanel",
                                                     fluidRow(column(12,div(fluidRow(column(1,h5("Year ",style="font-weight: 700;")),column(1,selectInput("dataEntryYearInput",NULL,dataLoadYearsLOV,selected = "dataLoadYearInput")),column(1,h5("Month",style="font-weight: 700;")),column(1,selectInput("dataEntryMonthInput",NULL,dataLoadMonthsLOV,selected = "dataLoadMonthInput"))
                                                                                     ,column(1,h5("Category: ",style="font-weight: 700;")),column(3,selectInput("dataEntryCategory",NULL,dataLoadCategoriesLOV,selected = "dataLoadCategory"))
                                                                                     ,column(1,h5("Operator: ",style="font-weight: 700;")),column(3,selectInput("dataEntryOperatorName",NULL,dataLoadOperatorsLOV,selected="dataLoadOperatorName")))
                                                                            ,div(fluidRow(column(2,selectizeInput("dataEntryProductNames","Product:",isolate(dataEntryProductsNamesLOV),selected = dataEntryProductsNamesLOV[1,1], multiple = FALSE))
                                                                                          ,column(2,radioButtons("chargeTypeLOV","Charge Type:",c("Daily","Monthly"),selected = "Daily",inline = T))
                                                                                          ,column(2,textInput("totalRevInput","Total Revenue:"))
                                                                                          ,column(2,textInput("postPaidInput","Post Paid:"))
                                                                                          ,column(2,textInput("prePaidInput","Pre Paid"))
                                                                                          ,column(2,textInput("gn4meRevInput","GN4ME Revenue:")))
                                                                                 ,fluidRow(column(1,offset=7,actionButton("displayDataEntryButton","Add")),column(2,actionButton("dataEntryBtn","Calculate Shares")),column(2,actionButton("deleteDataEntryBtn","Delete"))))         
                                                                            ,class="well")))
                                                     ,fluidRow(dataTableOutput("dataEntryDisplayTable"))),
                                            tabPanel("Lookup Data Show",value = "lookupDataShowTabPanel",
                                                     fluidRow(column(8,div(class="well",
                                                                           fluidRow(column(2,h5("Category",style="font-weight: 700;")),column(3,selectInput("lookUpCatgory",NULL,dataLoadCategoriesLOV,selected = "lookUpCatgory")),
                                                                                    column(2,h5("Operator",style="font-weight: 700;")),column(3,selectInput("lookUpOperators",NULL,dataLoadOperatorsLOV,selected = "lookUpOperators"))
                                                                                    ,column(2,div(style="text-align:right;",actionButton("lookUpBtn","Show"))))
                                                                           ,fluidRow(column(4,uiOutput("rdOp")))
                                                     )))
                                                     ,fluidRow(uiOutput("lookupHeader"))
                                                     ,fluidRow(column(8,uiOutput("lookupListTables")))
                                            ),
                                            tabPanel("Transaction Data Show",value = "showTransactionDataTabPanel",
                                                     fluidRow(class="col-sm-11",div(class="well",fluidRow(column(1,h5("Category",style="font-weight: 700;")),column(3,selectInput("transactionCatgory",NULL,dataLoadCategoriesLOV,selected = "transactionCatgory")),
                                                                                                          column(1,h5("Operator",style="font-weight: 700;")),column(2,selectInput("transactionOperators",NULL,dataLoadOperatorsLOV,selected = "transactionOperators")),
                                                                                                          column(1,h5("Year",style="font-weight: 700;")),column(2,selectInput("transactionYear",NULL,dataLoadYearsLOV,selected = "transactionYear"))
                                                                                                          ,column(1,div(style="text-align:right;",actionButton("transactionBtn","Show"))))
                                                                                    ,fluidRow(radioButtons("dataShowRadioBtns",NULL,choices = c("Monthly transactions","Monthly revenues per service"),selected ="Monthly transactions",inline = T)
                                                                                    )))
                                                     ,fluidRow(column(8,uiOutput("transactionTables")))
                                            )
))

jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
r <- c("")
## Month and year lookups
dataLoadDatesLOV <- dbSendQuery(jdbcConnection,"SELECT SALES_YEAR, SALES_MONTH FROM RTEST.BI_SALES_MONTHS")
dataLoadDatesLOV <- dbFetch(dataLoadDatesLOV)

dataLoadYearsLOV <- as.data.frame(unique(dataLoadDatesLOV$SALES_YEAR),stringsAsFactors=FALSE)
colnames(dataLoadYearsLOV)<- c("Years")

dataLoadMonthsLOV <- as.data.frame(unique(dataLoadDatesLOV$SALES_MONTH),stringsAsFactors=FALSE)
colnames(dataLoadMonthsLOV)<- c("Months")

dynamicDataLoadMonth <- reactive(input$dataLoadYearInput)

observeEvent(dynamicDataLoadMonth(),{
  dataLoadMonthsLOV <<- as.data.frame(dataLoadDatesLOV$SALES_MONTH[dataLoadDatesLOV$SALES_YEAR == input$dataLoadYearInput],stringsAsFactors=FALSE)
  colnames(dataLoadMonthsLOV)<- c("Months")
  updateSelectInput(session,"dataLoadMonthInput",NULL,choices = dataLoadMonthsLOV)
})

dynamicDataLoadMonth2 <- reactive(input$dataLoadYearInput2)

observeEvent(dynamicDataLoadMonth2(),{
  dataLoadMonthsLOV <<- as.data.frame(dataLoadDatesLOV$SALES_MONTH[dataLoadDatesLOV$SALES_YEAR == input$dataLoadYearInput2],stringsAsFactors=FALSE)
  colnames(dataLoadMonthsLOV)<- c("Months")
  updateSelectInput(session,"dataLoadMonthInput2",NULL,choices = dataLoadMonthsLOV)
})
# DATA LOAD LOV
dataLoadCategories <- dbSendQuery(jdbcConnection,"SELECT DISTINCT C.CATEGORY_NAME,C.CATEGORY_ID FROM BI_SALES_CATEGORY C ")
dataLoadCategories <- dbFetch(dataLoadCategories)
dataLoadCategories$CATEGORY_NAME <- as.character(dataLoadCategories$CATEGORY_NAME)

dataLoadCategoriesLOV <- as.data.frame(unique(dataLoadCategories$CATEGORY_NAME),stringsAsFactors = FALSE)
colnames(dataLoadCategoriesLOV) <- c("Categories")

dataLoadOperators <- dbSendQuery(jdbcConnection,"SELECT O.OPERATOR_ID, O.OPERATOR_NAME FROM BI_SALES_MYOPERATOR O ")
dataLoadOperators <- dbFetch(dataLoadOperators)


dataLoadOperators$OPERATOR_NAME <- as.character(dataLoadOperators$OPERATOR_NAME)
dbDisconnect(jdbcConnection)
dataLoadOperatorsLOV <- as.data.frame(unique(dataLoadOperators$OPERATOR_NAME),stringsAsFactors = FALSE)
colnames(dataLoadOperatorsLOV) <- c("Operators")


dataClean <- function(){
  dataf$Service.Name <- gsub("_Gn4me", "", dataf$Service.Name)
  dataf$charge_type <- ifelse(grepl("daily", dataf$charge_type),"daily", dataf$charge_type)
  dataf$charge_type <- ifelse(grepl("monthly", dataf$charge_type),"monthly", dataf$charge_type)
  product_lookups <- dbSendQuery(jdbcConnection,paste0("SELECT B.PRODUCT_ID, B.PRODUCT_NAME, B.CHARGE_TYPE, B.SERVICE_ID,S.SERVICE_NAME FROM RTEST.BI_SALES_PRODUCT B INNER JOIN BI_SALES_SERVICE  S ON B.SERVICE_ID = S.SERVICE_ID INNER JOIN BI_SALES_CATEGORY C ON C.CATEGORY_ID = S.SERVICE_CATEGORY WHERE C.CATEGORY_ID = 1 AND S.OPERATOR_ID = 1"))
  product_lookups<-dbFetch(product_lookups)
  dataf$Service.Name<-tolower(dataf$Service.Name)
  product_lookups$SERVICE_NAME<-tolower(product_lookups$SERVICE_NAME)
  dataf$Service_id <- product_lookups$SERVICE_ID[match(dataf$Service.Name, product_lookups$SERVICE_NAME)] 
}

observeEvent(input$dataLoadBtn,{
  if(input$dataLoadCategory == "ALL")
  {
    showNotification("You can't load all the data at the same time.")
    return()
  }
  if(input$dataLoadOperatorName == "Vodafone" || input$dataLoadOperatorName == "Orange" || input$dataLoadOperatorName == "Etisalat")
  {
    inFile <- input$file1
    if (is.null(inFile))
    {
      showNotification("Please add the file.")
      return(NULL)
    }
    if(input$dataLoadYearInput == "" && input$dataLoadMonthInput == "" && input$dataLoadOperatorName == "" && input$dataLoadCategory =="")
    {
      showNotification("Please fill in the fields!")
      return()
    }
    
    jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
    withProgress(message = "Adding ..",value = 0,{
      incProgress(1/4)
      dbTableName <- "BI_SALES_MYSERVICES"
      dbTableName2 <- "BI_SALES_MYPROVIDER_D"
      library(openxlsx)
      dataf <- data.frame(stringsAsFactors = FALSE)
      dataf <- tryCatch(read.xlsx(input$file1[[1,'datapath']], sheet = 1, startRow = 1, colNames = TRUE,detectDates = TRUE,skipEmptyRows = TRUE,skipEmptyCols = TRUE)
                        ,error=function(e){
                          showNotification("There's a problem with your file.")
                          return(data.frame())})
      if(nrow(dataf) == 0)
      {
        return()
      }
      dataf <- dataf[,1:9]
      dataf<-dataf[!is.na(dataf$Date),]
      #Column names check
      actual_names <- c("Date","Product.ID","Service.Name","ChargeType","Revenue","Postpaid","Prepaid","Vendor.Share","Vendor.Rev.Share")
      if(!all(names(dataf) == actual_names))
      {
        showNotification("Columns order is Wrong.")
        temp <- which(names(dataf) != actual_names)
        tempMsg <- as.data.frame(paste("Column",names(dataf)[temp[1]],"is supposed to be:",actual_names[temp[1]]),stringsAsFactors = FALSE)
        for(i in 2:length(temp))
        {
          m<-paste("Column",names(dataf)[temp[i]],"is supposed to be:",actual_names[temp[i]])
          tempMsg <- rbind(tempMsg,m)
        }
        colnames(tempMsg) <- c("Result")
        showModal(modalDialog(title = "Attention",easyClose = TRUE,size='m',renderTable(tempMsg)))
        return()
      }
      # Getting Lookup data.
      product_lookups <- dbSendQuery(jdbcConnection,paste0("SELECT B.PRODUCT_ID, B.PRODUCT_NAME, B.CHARGE_TYPE, B.SERVICE_ID FROM RTEST.BI_SALES_PRODUCT B  INNER JOIN BI_SALES_OPERATOR_SERVICES  S ON B.SERVICE_ID = S.SERVICE_ID  inner join bi_sales_service SS on S.service_id = SS.service_id INNER JOIN BI_SALES_CATEGORY C ON C.CATEGORY_ID = SS.SERVICE_CATEGORY WHERE C.CATEGORY_ID = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory]," AND S.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName]))
      product_lookups <- dbFetch(product_lookups)
      dataf$Service_id <- NA
      # Getting service id
      dataf$Service_id <- product_lookups$SERVICE_ID[match(dataf$Product.ID, product_lookups$PRODUCT_ID)]    
      dataf <- dataf[,c(10,2,1,5,6,7,9)]
      dataf[is.na(dataf)] <- 0
      if(is.numeric(dataf$Date))
      {
        #Detected different date formats and must be converted from number to date
        library(janitor)
        dataf$Date <- excel_numeric_to_date(dataf$Date)
        dataf$Date<-ifelse(substr(dataf$Date,(nchar(as.character(dataf$Date))+1)-2,nchar(as.character(dataf$Date)))==input$dataLoadMonthInput,format(as.Date(dataf$Date, '%Y-%d-%m'), "%m-%d-%Y"),format(as.Date(dataf$Date, '%Y-%m-%d'), "%d-%m-%Y"))      
        if(!grepl(input$dataLoadYearInput,dataf$Date))  
        {
          showNotification("Year in Date isn't correct!")
          return()
        }
      }
      else if (class(dataf$Date) == "Date"){
        #Detected date in correct format.
        dataf$Date <- as.character(format(dataf$Date,"%d-%m-%Y"))
        if(!grepl(input$dataLoadYearInput,dataf$Date))  
        {
          showNotification("Year in Date isn't correct!")
          return()
        }
      }
      else if(class(dataf$Date) == "character"){
        library(janitor)
        dataf$Date <- lapply(1:nrow(dataf), function(i) {
          if(substr(dataf[i,3],0,2) == input$dataLoadMonthInput && nchar(dataf[i,3]) == 10)
          {
            dataf[i,3] <- format(as.Date(dataf[i,3], '%m/%d/%Y'), "%d-%m-%Y")
          }
          else if(nchar(dataf[i,3]) == 5)
          {
            temp <- excel_numeric_to_date(as.numeric(dataf[i,3]))
            dataf[i,3] <- ifelse(substr(temp,(nchar(as.character(temp))+1)-2,nchar(as.character(temp)))==input$dataLoadMonthInput,format(as.Date(temp, '%Y-%d-%m'), "%m-%d-%Y"),format(as.Date(temp, '%Y-%m-%d'), "%d-%m-%Y"))      
          }
          else{
            dataf[i,3] <- format(as.Date(dataf[i,3],'%d/%m/%Y'),"%d-%m-%Y")
          }
        })
        if(!grepl(input$dataLoadYearInput,dataf$Date))  
        {
          showNotification("Year in Date isn't correct!")
          return()
        }
        #dataf$Date<-ifelse(substr(dataf$Date,0,2) == input$dataLoadMonthInput,format(as.Date(dataf$Date, '%m/%d/%Y'), "%d-%m-%Y"),format(as.Date(dataf$Date,'%d/%m/%Y'),"%d-%m-%Y"))
      }
      else{
        showNotification("Unambiguous Date Format")
        return()
      }
      
      dataf$GN4ME_REVENUE <- as.numeric(dataf$Vendor.Rev.Share)
      vendor_share <- dataf$Vendor.Share[1]
      drops <- c("Vendor.Share","Vendor.Rev.Share")
      dataf <- dataf[ , !(names(dataf) %in% drops)]
      colnames(dataf) <- c("SERVICE_ID", "PRODUCT_ID", "SERVICE_DATE", "TOTAL_REVENUE", "POST_PAID", "PRE_PAID", "GN4ME_REVENUE")
      dataf$TOTAL_REVENUE <- as.numeric(dataf$TOTAL_REVENUE)
      dataf$POST_PAID <- as.numeric(dataf$POST_PAID)
      dataf$PRE_PAID <- as.numeric(dataf$PRE_PAID)
      if(nrow(unique(dataf[c("SERVICE_ID","PRODUCT_ID","SERVICE_DATE")])) < nrow(dataf))
      {
        showNotification("There are duplicate rows.")
        dataf <- as.data.frame(lapply(dataf, unlist))
        dataf <- aggregate(dataf[c("TOTAL_REVENUE","POST_PAID","PRE_PAID","GN4ME_REVENUE")], by=dataf[c("SERVICE_ID","PRODUCT_ID","SERVICE_DATE")], sum,simplify=FALSE)
      }
      if(input$dataLoadCategory == "ALERTS")
      {
        dataf$SC_TOT_SMS <- NA
        dataf$SC_MONTHLY_COST <- NA
        dataf$RBT_FLEX_ACT_COUNT<- NA
        dataf$RBT_FLEX_ACT_REV <- NA
        dataf$RBT_FLEX_REN_COUNT <- NA
        dataf$RBT_FLEX_REN_REV <- NA
        dataf$RBT_NFLEX_ACT_COUNT <- NA
        dataf$RBT_NFLEX_ACT_REV <- NA
        dataf$RBT_NFLEX_REN_COUNT <- NA
        dataf$RBT_NFLEX_REN_REV <- NA
      }
      else if(input$dataLoadCategory == "RBT")
      {
        dataf$RBT_FLEX_ACT_COUNT<- NA
        dataf$RBT_FLEX_ACT_REV <- NA
        dataf$RBT_FLEX_REN_COUNT <- NA
        dataf$RBT_FLEX_REN_REV <- NA
        dataf$RBT_NFLEX_ACT_COUNT <- NA
        dataf$RBT_NFLEX_ACT_REV <- NA
        dataf$RBT_NFLEX_REN_COUNT <- NA
        dataf$RBT_NFLEX_REN_REV <- NA
      }
      my_share <- dbSendQuery(jdbcConnection,paste0("SELECT MY_SHARE FROM BI_SALES_MYSHARES WHERE CURRENT_FLAG=1 
                                                    AND OPERATOR_ID=",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName]," 
                                                    AND CATEGORY_ID=",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory]))
      my_share <- dbFetch(my_share)
      my_share <- my_share[1,1]
      dataf$CALCULATED_GN4ME_REVENUE <- NA
      dataf$CALCULATED_GN4ME_REVENUE <- ((as.numeric(dataf$TOTAL_REVENUE)*my_share)/100)
      
      dataf <- as.data.frame(lapply(dataf, unlist))
      dataf <- dataf[order(dataf$SERVICE_DATE),]
      
      newProductsCount <- dataf[(dataf$SERVICE_ID == 0),]
      newProductsCount <- length(unique(newProductsCount$PRODUCT_ID))
      dataf$OPERATOR_ID <- NA
      dataf$OPERATOR_ID <- dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName]
      dataf[,4] <- round(dataf[,4],10)
      dataf[,5] <- round(dataf[,5],10)
      dataf[,18] <- round(dataf[,18],10)
      
      # library(ROracle)
      # con <- dbConnect(dbDriver("Oracle"),"obie12",username="rtest",password="rtest")
      # done <- tryCatch(dbWriteTable(con, dbTableName,dataf,append = TRUE, row.names = FALSE, overwrite = FALSE),error = function(e) {return(FALSE)})
      done <- tryCatch(dbWriteTable(jdbcConnection, dbTableName,dataf,append = TRUE, row.names = FALSE, overwrite = FALSE),error = function(e) {return(FALSE)})
      if(as.character(done) == "FALSE")
      {
        incProgress(1/4)
        jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
        addedCheck <- dbSendQuery(jdbcConnection,paste0("select count(*) from BI_SALES_MYSERVICES W INNER JOIN BI_SALES_SERVICE S ON S.SERVICE_ID = W.SERVICE_ID 
                                                        where to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",input$dataLoadMonthInput,"' 
                                                        AND to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') = '",input$dataLoadYearInput,"'
                                                        AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory]," 
                                                        AND S.OPERATOR_ID =",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName]))
        addedCheck <- dbFetch(addedCheck)
        if(addedCheck[1,1] > 0)
        {
          showNotification("Data already in the database")
          dbDisconnect(jdbcConnection)
        }
        else if(addedCheck[1,1] == 0)
        {
          showNotification("There's a problem, Please contact the developer.")
          dbDisconnect(jdbcConnection)
        }
        else{
          jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
          dbSendUpdate(jdbcConnection,paste0("DELETE (select W.* from BI_SALES_MYSERVICES W INNER JOIN BI_SALES_SERVICE S ON S.SERVICE_ID = W.SERVICE_ID 
                                             where to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",input$dataLoadMonthInput,"' 
                                             AND to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') = '",input$dataLoadYearInput,"'
                                             AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory]," 
                                             AND S.OPERATOR_ID =",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName],")"))
          showNotification("There is an error within the file.")
          dbDisconnect(jdbcConnection)
        }
        return()
      }
      #Getting provider ids from BI_SALES_PROVIDER_SERVICES
      data_detail_f <- dataf[,1:3]
      data_detail_f <- cbind(data_detail_f,dataf$GN4ME_REVENUE)
      provider_lookup <- dbSendQuery(jdbcConnection,"SELECT  * FROM BI_SALES_PROVIDER_SERVICES")
      provider_lookup <- dbFetch(provider_lookup)
      data_detail_f$PROVIDER_ID <- NA
      data_detail_f$PROVIDER_ID <- provider_lookup$PROVIDER_ID[match(data_detail_f$SERVICE_ID, provider_lookup$SERVICE_ID)]
      
      ## Calculating provider revenue
      provider_lookup2 <- dbSendQuery(jdbcConnection,"SELECT PROVIDER_ID,PROVIDER_SHARE FROM BI_SALES_PROVIDERS")
      provider_lookup2 <- dbFetch(provider_lookup2)
      data_detail_f$PROVIDER_SHARE <- NA
      data_detail_f$PROVIDER_SHARE <- provider_lookup2$PROVIDER_SHARE[match(data_detail_f$PROVIDER_ID, provider_lookup2$PROVIDER_ID)]
      
      #Delete when providers problem is solved
      data_detail_f[is.na(data_detail_f)] <- 0
      
      data_detail_f$PROVIDER_REVENUE <- NA
      data_detail_f$PROVIDER_REVENUE <- as.numeric(data_detail_f$`dataf$GN4ME_REVENUE`*data_detail_f$PROVIDER_SHARE)
      data_detail_f <- data_detail_f[,c(5,1,2,3,7)]
      data_detail_f$MONTHLY_COST <- NA
      ## Adding the data in table BI_SALES_MYPROVIDER_D (PROVIDER_ID, SERVICE_ID, PRODUCT_ID, SERVICE_DATE, PROVIDER_REVENUE, MONTHLY_COST)
      data_detail_f$OPERATOR_ID <- NA
      data_detail_f$OPERATOR_ID <- dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName]
      # con <- dbConnect(dbDriver("Oracle"),"obie12",username="rtest",password="rtest")
      # done <- tryCatch(dbWriteTable(con,dbTableName2,data_detail_f,append = TRUE, row.names = FALSE, overwrite = FALSE),error = function(e) {return(FALSE)})
      
      done <- tryCatch(dbWriteTable(jdbcConnection, dbTableName2,data_detail_f,append = TRUE, row.names = FALSE, overwrite = FALSE),error = function(e) {return(FALSE)})
      if(as.character(done) == "FALSE")
      {
        incProgress(1/4)
        jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
        addedCheck <- dbSendQuery(jdbcConnection,paste0("select count(*) from BI_SALES_MYPROVIDER_D W INNER JOIN BI_SALES_SERVICE S ON S.SERVICE_ID = W.SERVICE_ID 
                                                        where to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",input$dataLoadMonthInput,"' 
                                                        AND to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') = '",input$dataLoadYearInput,"'
                                                        AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory]," 
                                                        AND W.OPERATOR_ID =",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName]))
        addedCheck <- dbFetch(addedCheck)
        if(addedCheck[1,1] > 0)
        {
          showNotification("Data already in the database")
          dbDisconnect(jdbcConnection)
        }
        else if(addedCheck[1,1] == 0)
        {
          showNotification("There's a problem, Please contact the developer.")
          dbDisconnect(jdbcConnection)
        }
        else{
          jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
          done <- tryCatch(dbSendUpdate(jdbcConnection,paste0("DELETE (select W.* from BI_SALES_MYPROVIDER_D W INNER JOIN BI_SALES_SERVICE S ON S.SERVICE_ID = W.SERVICE_ID 
                                                              where to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",input$dataLoadMonthInput,"' 
                                                              AND to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') = '",input$dataLoadYearInput,"'
                                                              AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory]," 
                                                              AND S.OPERATOR_ID =",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName],")")),error = function(e) {return(FALSE)})
          if(!is.null(done))
          {
            showNotification("You must contact the developer!")
          }
          showNotification("There is an error within the file.")
          dbDisconnect(jdbcConnection)
        }
        return()
      }      
      data_detail_f <- NULL
      #Appending product Name.
      dataf$PRODUCT_NAME <- NA
      dataf$PRODUCT_NAME <- product_lookups$PRODUCT_NAME[match(dataf$PRODUCT_ID,product_lookups$PRODUCT_ID)]
      #Appending charge type
      dataf$CHARGE_TYPE <- NA
      dataf$CHARGE_TYPE <- product_lookups$CHARGE_TYPE[match(dataf$PRODUCT_ID,product_lookups$PRODUCT_ID)]    
      ## Services lookup dataframe. from Subcategory and service tables
      service_lookup <- dbSendQuery(jdbcConnection,"SELECT  B.SERVICE_ID, B.SERVICE_NAME, X.SUBCATEGORY_NAME FROM RTEST.BI_SALES_SERVICE B INNER JOIN  RTEST.BI_SALES_SUBCATEGORY X ON (X.subcategory_id =  B.SERVICE_SUBCATEGORY AND X.CATEGORY_ID = B.SERVICE_CATEGORY)")
      service_lookup <- dbFetch(service_lookup)
      dbDisconnect(jdbcConnection)
      #Appending service name
      dataf$`Service Name` <- NA
      dataf$`Service Name` <- service_lookup$SERVICE_NAME[match(dataf$SERVICE_ID,service_lookup$SERVICE_ID)]
      #Appending Subcategory name
      dataf$`Subcategory Name` <- NA
      dataf$`Subcategory Name` <- service_lookup$SUBCATEGORY_NAME[match(dataf$SERVICE_ID,service_lookup$SERVICE_ID)]
      #Arrange the columns and sort with date ASC.
      dataf <- dataf[,c(3,2,19,20,21,22,4,5,6,7,18)]
      
      product_lookups <- NULL
      service_lookup <- NULL
      provider_lookup <- NULL
      provider_lookup2 <- NULL
      rownames(dataf) <- NULL
      dataf$GN4ME_REVENUE <- round(dataf$GN4ME_REVENUE,digits = 2)
      dataf$CALCULATED_GN4ME_REVENUE <- round(dataf$CALCULATED_GN4ME_REVENUE,digits=2)
      dataf$TOTAL_REVENUE <- round(dataf$TOTAL_REVENUE,digits = 2)
      dataf$POST_PAID <- round(dataf$POST_PAID,digits = 2)
      dataf$PRE_PAID <- round(dataf$PRE_PAID,digits = 2)
      
      dataf$PRODUCT_ID <- as.character(dataf$PRODUCT_ID)
      dataf$OPERATOR_ID <- as.character(dataf$OPERATOR_ID)
      colnames(dataf)<-c("Service Date","Product ID","Product Name","Charge Type","Service Name","Subcategory Name","Total Revenue","Post Paid","Pre Paid","GN4ME Revenue","Calculated GN4ME Revenue")
      #data_to_display <<- dataf
      
      sums <- as.data.frame(t(colSums(dataf[sapply(dataf, is.numeric)], na.rm = TRUE)))
      for(i in 1:length(names(sums)))
      {
        sums[,i]<-round(sums[,i],digits = 2)
        sums[,i]<-prettyNum(sums[,i],big.mark = ",")
      }
      sums1 <- cbind(" "=" ","Service Date"=" ","Product ID"=" ","Product Name"=" ","Charge Type"=" ","Service Name"=" ","Subcategory Name"=" ",sums)
      sums <- cbind("Total" = "Total"," "=" "," "=" "," "=" "," "=" "," "=" "," "=" ", sums)
      names(sums)<- as.character(apply(sums[1, ], 1, paste))
      sketch = htmltools::withTags(table(list(tableHeader(sums1),tableFooter(sums))))
      rownames(dataf) <- NULL
      
      dataf <- DT::datatable(dataf,container = sketch,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),scrollX = TRUE,scrollCollapse = TRUE,lengthMenu=sort(c(10,15,nrow(dataf)))))
      incProgress(1/4)
      if(as.character(done) == "TRUE"){
        temp <- paste0("Database updated successfully! with file: ",input$file1[[1,'datapath']])
        showNotification(temp)
        output$dataLoadDisplayTable <- DT::renderDataTable(dataf)
        if(newProductsCount > 0)
        {
          output$newProducts <- renderUI(fluidRow(h5(style="color:red;font-weight:700;",paste("You have",newProductsCount,"new products."))))
        }
        if(vendor_share != my_share)
        {
          output$differentShareOp <- renderUI(fluidRow(h5(style="color:red;font-weight:700;",paste("Database share is",(my_share*100),"%"))))
        }
      }
      else{
        temp1 <- paste0("Database wasn't updated with file: ",input$file1[[1,'datapath']])
        showNotification(temp1)
      }
  })
  }
  else{
    showNotification("Not working yet!")
    return()
  }
  },ignoreInit = TRUE,ignoreNULL = TRUE)

observeEvent(input$displayBtn,{
  if(input$dataLoadYearInput == "" && input$dataLoadMonthInput == "" && input$dataLoadOperatorName == "" && input$dataLoadCategory =="")
  {
    showNotification("Please fill in the fields!")
    return()
  }
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  data <- dbSendQuery(jdbcConnection,paste0("SELECT S.SERVICE_DATE,S.PRODUCT_ID,PRO.PRODUCT_NAME, PRO.CHARGE_TYPE,SS.SERVICE_NAME,SSB.SUBCATEGORY_NAME,S.TOTAL_REVENUE,S.POST_PAID,S.PRE_PAID,S.GN4ME_REVENUE,S.CALCULATED_GN4ME_REVENUE
                                            FROM BI_SALES_MYSERVICES S INNER JOIN BI_SALES_SERVICE SS ON S.SERVICE_ID=SS.SERVICE_ID
                                            INNER JOIN BI_SALES_SUBCATEGORY SSB ON  (SSB.SUBCATEGORY_ID = SS.SERVICE_SUBCATEGORY AND SSB.CATEGORY_ID = SS.SERVICE_CATEGORY)
                                            INNER JOIN BI_SALES_PRODUCT PRO ON S.PRODUCT_ID = PRO.PRODUCT_ID 
                                            WHERE to_char(TO_DATE(S.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",input$dataLoadMonthInput,"' AND  to_char(TO_DATE(S.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') = '",input$dataLoadYearInput,"' 
                                            AND S.OPERATOR_ID =",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName]," AND SS.SERVICE_CATEGORY =",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory]
                                            ," ORDER BY S.SERVICE_DATE,S.PRODUCT_ID"))
  data <- dbFetch(data)
  if(nrow(data) == 0)
  {
    data = data.frame(Result="There is no data available.")
    data <- DT::datatable(data,options = list(scrollX = TRUE,scrollCollapse = TRUE))
    output$dataLoadDisplayTable <- DT::renderDataTable(data)
    showNotification("Data is not available.")
    return()
  }
  data$GN4ME_REVENUE <- round(data$GN4ME_REVENUE,digits = 2)
  data$CALCULATED_GN4ME_REVENUE <- round(data$CALCULATED_GN4ME_REVENUE,digits=2)
  data$TOTAL_REVENUE <- round(data$TOTAL_REVENUE,digits = 2)
  data$POST_PAID <- round(data$POST_PAID,digits = 2)
  data$PRE_PAID <- round(data$PRE_PAID,digits = 2)
  data$PRODUCT_ID <- as.character(data$PRODUCT_ID)
  
  colnames(data)<-c("Service Date","Product ID","Product Name","Charge Type","Service Name","Subcategory Name","Total Revenue","Post Paid","Pre Paid","GN4ME Revenue","Calculated GN4ME Revenue")
  
  sums <- as.data.frame(t(colSums(data[sapply(data, is.numeric)], na.rm = TRUE)))
  for(i in 1:length(names(sums)))
  {
    sums[,i]<-round(sums[,i],digits = 2)
    sums[,i]<-prettyNum(sums[,i],big.mark = ",")
  }
  sums1 <- cbind(" "=" ","Service Date"=" ","Product ID"=" ","Product Name"=" ","Charge Type"=" ","Service Name"=" ","Subcategory Name"=" ",sums)
  sums <- cbind("Total" = "Total"," "=" "," "=" "," "=" "," "=" "," "=" "," "=" ", sums)
  names(sums)<- as.character(apply(sums[1, ], 1, paste))
  sketch = htmltools::withTags(table(list(tableHeader(sums1),tableFooter(sums))))
  rownames(data) <- NULL
  data <- DT::datatable(data,container=sketch,extensions = 'Buttons',options = list(dom = 'Blfrtip',buttons = c('csv', 'excel', 'pdf', 'print'),scrollX = TRUE,scrollCollapse = TRUE,lengthMenu=sort(c(10,15,nrow(data)))))
  output$dataLoadDisplayTable <- DT::renderDataTable(data)
},ignoreInit = TRUE,ignoreNULL = TRUE)

observeEvent(input$deleteBtn,{
  if(input$dataLoadOperatorName != "")
  {
    if(input$dataLoadYearInput == "" && input$dataLoadMonthInput == "" && input$dataLoadOperatorName == "" && input$dataLoadCategory =="")
    {
      showNotification("Please fill in the fields!")
      return()
    }
  }
  showModal(modalDialog(title = "Attention",easyClose = TRUE,size='m',renderUI(fluidRow(column(8,h5("Are you sure you want to delete?",style="font-weight:700;")),column(4,div(style="text-align:right;",actionButton("deleteConfimed","Yes")))))))
},ignoreInit = TRUE,ignoreNULL = TRUE)

observeEvent(input$deleteConfimed,{
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
  check <- dbSendQuery(jdbcConnection,paste0("SELECT COUNT(*)  FROM BI_SALES_MYPROVIDER_D D INNER JOIN BI_SALES_MYSERVICES W ON( D.SERVICE_ID = W.SERVICE_ID AND D.SERVICE_DATE = W.SERVICE_DATE AND D.PRODUCT_ID = W.PRODUCT_ID AND D.OPERATOR_ID = W.OPERATOR_ID) INNER JOIN BI_SALES_SERVICE S ON S.SERVICE_ID = W.SERVICE_ID where to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",input$dataLoadMonthInput,"' AND  to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",input$dataLoadYearInput,"' AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory]," AND D.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName]))
  check <- dbFetch(check)
  if(check[1,1] == 0)
  {
    showNotification("There is no data to delete!")
  }
  else{
    done <- tryCatch(dbSendUpdate(jdbcConnection,paste0("DELETE( SELECT D.*  FROM BI_SALES_MYPROVIDER_D D INNER JOIN BI_SALES_MYSERVICES W ON( D.SERVICE_ID = W.SERVICE_ID AND D.SERVICE_DATE = W.SERVICE_DATE AND D.PRODUCT_ID = W.PRODUCT_ID AND D.OPERATOR_ID = W.OPERATOR_ID) INNER JOIN BI_SALES_SERVICE S ON S.SERVICE_ID = W.SERVICE_ID where to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",input$dataLoadMonthInput,"' AND  to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",input$dataLoadYearInput,"' AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory]," AND D.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName],")")),error = function(e) {return(FALSE)})
    if(!is.null(done))
    {
      showNotification("Deletion Process wasn't done!")
      return()
    }
    done <- tryCatch(dbSendUpdate(jdbcConnection,paste0("DELETE( SELECT W.*  FROM BI_SALES_MYSERVICES W INNER JOIN BI_SALES_SERVICE S ON S.SERVICE_ID = W.SERVICE_ID where to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'MM') = '",input$dataLoadMonthInput,"' AND  to_char(TO_DATE(W.SERVICE_DATE,'dd/MM/yyyy HH:MI:ss PM'),'YYYY') ='",input$dataLoadYearInput,"' AND S.SERVICE_CATEGORY = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory]," AND W.OPERATOR_ID = ",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName],")")),error = function(e) {return(FALSE)})
    if(!is.null(done))
    {
      showNotification("Deletion Process wasn't done!")
      return()
    }
    done <- tryCatch(dbSendUpdate(jdbcConnection,paste0("DELETE FROM BI_SALES_PROVIDERS_STATUS WHERE CATEGORY_ID = ",dataLoadCategories$CATEGORY_ID[dataLoadCategories$CATEGORY_NAME == input$dataLoadCategory]," AND SALES_YEAR ='",input$dataLoadYearInput,"' AND SALES_MONTH='",input$dataLoadMonthInput,"' AND OPERATOR_ID=",dataLoadOperators$OPERATOR_ID[dataLoadOperators$OPERATOR_NAME == input$dataLoadOperatorName])),error = function(e) {return(FALSE)})
    if(!is.null(done))
    {
      showNotification("Deletion Process wasn't done!")
      return()
    }
    showNotification("Deletion Process done successfully!")
  }
  dbDisconnect(jdbcConnection)
},ignoreInit = TRUE,ignoreNULL = TRUE)