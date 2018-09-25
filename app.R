rm(list = ls())
library(shiny)
library(shinyBS)
library(DT)
library(data.table)
library(DBI)
library(RJDBC)
library(shinyWidgets)

options(shiny.maxRequestSize=30*1024^2,shiny.sanitize.errors = TRUE,warn = -1)

Logged = FALSE
userType = 1
loginGeneration = FALSE
#Log in UI
ui1 <- function() {
  tagList(
    div(
      id = "login",
      wellPanel(
        textInput("userName", "Username"),
        passwordInput("passwd", "Password"),
        br(),
        div(style="text-align:center;",actionButton("Login", "Log in"))
      )
    #,setBackgroundImage(src = "home.jpg")
    )
    ,tags$style(
      type = "text/css",
      ".label,#Login {
          font-weight: 700;
      }
      #login {font-size:10px; text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}"
    )
  )
}
sendMail <- function(recipient)
{
  library(mailR)
  sender <- "gn4me.emaill@gmail.com"
  send.mail(from = sender,
            to = recipient,
            subject = "Subject",
            body = "Hello",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = sender,            
                        passwd = "nmohamedgn4me", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  
}
#Gn4me user UI
ui3<-function(){
  navbarPage(id="MainPageNavBar",  windowTitle="Sales Creative Solution",title=div(class="col-sm-12",img(src="gn2.png",style="float:left;display:inline-block;"),h4("Sales Creative Solution",style="display:inline-block;padding:8px 15px;")),collapsible = TRUE,
             fluid = TRUE,
             tabPanel("Home",style="background-color:#f5f5f5;",uiOutput("homeTab"),icon=icon("home")),
             tabPanel("Load/Calculate Data",value="LookupTab", uiOutput("insertLookUp")),
             tabPanel("Review/Approve",value="reviewTab", uiOutput("reviewTab")),
             tabPanel("Access Control",value="accessControlTab",uiOutput("accessControlTab")),
             tabPanel("Analytics",value="analyticsTab",uiOutput("analyticsTab"))
,setBackgroundImage(src = NULL)             
,tags$head(tags$style(HTML("
  a {
    font-weight: 700;
  }
 #displayDataEntryButton, #dataLoadBtn,#dataEntryBtn,#deleteDataEntryBtn,#deleteDataEntryCOnfirmedBtn,#accessControlBtn1,#calculateAlertReportBtn,#transactionBtn,#unmakeConfirmedBtn,#unmakeBtn,
#unmakeAllConfirmedBtn,#unmakeAllBtn,#monthClosingBtn,#makeAllAvailableBtn,
#showMonthDataBtn,#lookUpBtn, #displayBtn,#loginBtn, #makeAvailableBtn,
#deleteBtn,#groupedDataBtn,#monthClosingBtn2,#showquartelyBudgetsBtn,#predictDataButton{
      font-weight: 700;
}
.navbar-brand{
   padding:0px;
 }
table, th, td {
  white-space: nowrap;
}
div#quartelyBudgetPlot {
    margin-top: 10px;
}
body{
  background-color: #337ab7;
}
  caption{
     color:white;
      font-weight: bold;
    }
  .datatables {
    margin: 3%;
  }
  .checkbox label, .radio label {
    font-weight: 700;
  }
#deleteBtn,#unmakeAllBtn,#unmakeBtn,#deleteDataEntryBtn,#monthClosingDeletionButton {
    font-weight: 700;
    background-color: red;
}
#dataLoadDisplayTable,#alertTableOutput,#userOutput,#dataEntryDisplayTable {
    margin: 0 !important;
}
a {
    background-color: white;
}
.dataTables_length > label , .dataTables_wrapper .dataTables_info{
                                color:white;
}
.dataTables_wrapper .dataTables_info {
  color:white;
}
th {
    font-weight: bold;
    color: white;
}
.modal-body .dataTables_length > label {
    color: black;
}
select {
color:black;
}
 .nav-tabs > li.active > a{color:red;}
#quartelyBudgetTable,#forecastTable {color: white;}  
.modebar{display: none !important;}
#quartelyBudgetTable > .table.shiny-table.table-.spacing-s
, #forecastTable > .table.shiny-table.table-.spacing-s {border: 1px solid white;}
#forecastTable{
  margin-top: 10px;
}
")))
             #,useShinyjs()
             ,footer =HTML('<p style="color:black;text-align: center;">GN4ME 2018 &#169; Data Management Services</p>'))
}
#Client UI
ui2 <- function() {
  navbarPage(id="MainPageNavBar",  windowTitle="Sales Creative Solution",title=div(class="col-sm-12",img(src="gn2.png",style="float:left;display:inline-block;"),h4("Sales Creative Solution",style="display:inline-block;padding:8px 15px;")),collapsible = TRUE,
             fluid = TRUE,
             tabPanel("Access Control",value="accessControlTab",uiOutput("accessControlTab2")),
             setBackgroundImage(src = NULL)              
            ,tags$head(tags$style(HTML("
              a {
              font-weight: 700;
              }
              #showACReview,#dataentry,#accessControlBtn1,#accessControlBtn2, #calculateAlertReportBtn,#showMonthDataBtn,#lookUpBtn, #displayBtn,#loginBtn, #makeAvailableBtn,#deleteBtn,#groupedDataBtn {
              font-weight: 700;
              }
              caption{
              color:white;
              font-weight: bold;
              }
              .datatables {
              margin: 3%;
              }
.navbar-brand{
                                 padding:0px;
                                 }
              .checkbox label, .radio label , label span{
              font-weight: 700;
              }
              #dataLoadDisplayStyle,#alertTableOutput,#userOutput {
              margin: 0 !important;
              }
body{
  background-color: #337ab7;
}
a {
    background-color: white;
}
.dataTables_length > label , .dataTables_wrapper .dataTables_info{
                                color:white;
}
.dataTables_wrapper .dataTables_info {
  color:white;
}
th {
    font-weight: bold;
    color: white;
}
.modal-body .dataTables_length > label {
    color: black;
}
select {
color:black;
}
 .nav-tabs > li.active > a{color:red;}
              ")))
             ,footer =HTML('<p style="color:black;text-align: center;">GN4ME 2018 &#169; Data Management Services</p>'))
  
}


ui = (htmlOutput("page"))

server = function(input, output, session) {
  USER <- reactiveValues(Logged = Logged)
  output$homeTab <- renderUI(fluidPage(fluidRow(img(src="home.jpg",class="img-responsive",style="margin-top:6%;"))))
  ####################################* Constants *###################################
  jdbcDriverPath <- "D:\\app\\product\\11.2.0\\db_1\\jdbc\\lib\\ojdbc6.jar"
  #jdbcDriverPath <- "D:\\oracle\\product\\11.2.0\\db_1\\jdbc\\lib\\ojdbc6.jar"
  jdbcDriver <- JDBC("oracle.jdbc.OracleDriver",classPath=jdbcDriverPath)
  
  #####################################################################
  # Data tables Insertion Tab #
  #####################################################################
  source(file.path("server", "Alert_Insertion.R"),  local = TRUE)$value 
  source(file.path("server","Data_entry.R"),local = TRUE)$value
  #####################################################################
  # Lookup tables show Tab #
  #####################################################################
  source(file.path("server", "lookup.R"), local = TRUE)$value
  #####################################################################
  # Review tab #
  #####################################################################
  source(file.path("server","review_approve.R"),local = TRUE)$value
  #####################################################################
  # Access Control Tab #
  #####################################################################
  source(file.path("server", "access_control2.R"), local = TRUE)$value
  source(file.path("server", "access_control.R"), local = TRUE)$value
  #####################################################################
  # Analytics Tab #
  #####################################################################
  source(file.path("server", "analytics.R"), local = TRUE)$value
  
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (length(input$Login) > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(Username == "" && Password == "")
          {
            return()
          }
          library(RJDBC)
          jdbcDriverPath <- "D:\\app\\product\\11.2.0\\db_1\\jdbc\\lib\\ojdbc6.jar"
          jdbcDriver <- JDBC("oracle.jdbc.OracleDriver",classPath=jdbcDriverPath)
          jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
          userCheck <- dbSendQuery(jdbcConnection,paste0("select * from BI_SALES_PROVIDERS_USERS where USER_NAME='",Username,"' AND USER_PASSWD = '",Password,"'"))
          userCheck <- dbFetch(userCheck)
          userCheck <<- userCheck
          dbDisconnect(jdbcConnection)
          if(nrow(userCheck) > 0)
          {
            USER$Logged <- TRUE
            if(userCheck$USER_TYPE == 0)
            {
              USER$userType <- 0
            }
            else{
              jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.220.220.219:1521/obie12", "rtest", "rtest")
              accessControlLOV <- dbSendQuery(jdbcConnection,paste0("SELECT DISTINCT  PV.PROVIDER_ID,PV.PROVIDER_NAME,OS.OPERATOR_ID,OP.OPERATOR_NAME,C.category_id,C.CATEGORY_NAME FROM BI_SALES_PROVIDERS PV
                                                                    INNER JOIN BI_SALES_PROVIDER_SERVICES PS ON PV.PROVIDER_ID = PS.PROVIDER_ID
                                                                    INNER JOIN BI_SALES_SERVICE S ON PS.SERVICE_ID = S.SERVICE_ID
                                                                    INNER JOIN BI_SALES_OPERATOR_SERVICES OS ON OS.SERVICE_ID = S.SERVICE_ID
                                                                    INNER JOIN BI_SALES_MYOPERATOR OP ON OP.OPERATOR_ID = OS.OPERATOR_ID
                                                                    INNER JOIN BI_SALES_SUBCATEGORY SS ON SS.SUBCATEGORY_ID = S.SERVICE_CATEGORY
                                                                    INNER JOIN BI_SALES_CATEGORY C ON S.SERVICE_CATEGORY = C.CATEGORY_ID
                                                                    WHERE PV.PROVIDER_ID = ",userCheck$PROVIDER_ID))
              accessControlLOV <- dbFetch(accessControlLOV)
              accessControlLOV <<- accessControlLOV
              dbDisconnect(jdbcConnection)
              dataAccessCategories <<- as.data.frame(unique(as.character(accessControlLOV$CATEGORY_NAME)),stringsAsFactors = FALSE)
              colnames(dataAccessCategories) <<- c("Categories")
              
              dataAccessOperators <<- as.data.frame(unique(accessControlLOV$OPERATOR_NAME),stringsAsFactors = FALSE)
              colnames(dataAccessOperators) <<- c("Operators")
              USER$userType <- 1
            }
          }
          else{
            showNotification("Wrong username or password!")
          }
        }
      }
    }
  })
  
  output$page <- renderUI({
    if (USER$Logged == FALSE) {
      do.call(bootstrapPage, c("", ui1()))
    } else {
      #bootstrapPage
      if(USER$userType == 0)
      {
        do.call(bootstrapPage, ui3())
      }
      else if(USER$userType == 1)
      {
        do.call(bootstrapPage, ui2())
      }
    }
  })
}

shinyApp(ui, server)