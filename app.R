library(shinyjs)
library(qdapRegex)
library(shinydashboard)
library(tableHTML)
library(tidyverse)
library(stringr)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)
library(shiny)
library(readxl)
library(DT)
require(IDPmisc)

folder_path = './app/data/'

#folder_path = '/Users/vignesh/redflagstagingshiny/app/data/'

## Get all the selection needed for inputs (operation)

## Can have only one operation table for each country


country_month_list<- list()
country_shop_list<- list()
file.ls.month <- list.files(path=folder_path,pattern="LY.csv|LW.csv")
country_list = unique(sub("\\_.*", "",file.ls.month))
for (i in country_list){country_month_list[i]=i}
for (country in (country_month_list)){
  file.ls2<-file.ls.month[grepl(paste0("^",country,".*\\.csv"), file.ls.month)]
  print(file.ls2)
  op_file = grep("operation_table", file.ls2, value =TRUE)
  df=read.csv(paste0(folder_path,op_file))
  if (country!="Singapore"){
    df = subset(df, select = -c(ShopNo) )
    names(df)[names(df) == "ShopName"] <- "ShopNo"
  }
  # PRE_LOADING
  temp_name=op_file
  print(temp_name)
  temp_name0 = substr(temp_name, 1, nchar(temp_name)-4)
  print(temp_name0)
  do.call("<<-",list(temp_name0, df))
  country_month_list[[gsub(' ','_',country, fixed=TRUE)]]=temp_name0
  df_y_w =df %>%  filter(!is.na(Sales.),!is.na(NOS.) ,!is.na(Incoming.)) %>% group_by(ShopNo, Year) %>% summarize(week=list(unique(Week)))
  country_shop_list[[gsub(' ','_',country, fixed=TRUE)]] = data.frame(df_y_w)
}

## Get all the selection needed for inputs (Target from inventory)

invent_target_list<- list()
invent_target_files<- list()
invent_target.ls.month <- list.files(path=folder_path,pattern='calc_target_table.xlsx')
# invent_target.ls.day <- list.files(path=folder_path,pattern='calc_target_table_daily.xlsx')
country_list = unique(gsub("_"," ",sub("\\_calc.*", "",invent_target.ls.month)))
for (i in country_list){
  invent_target_list[i]=i
  invent_target_files[i]=i}
for (country in (invent_target_list)){
  file.month <- invent_target.ls.month[grepl(paste0("^",gsub(' ','_',country, fixed=TRUE),".*\\.xlsx"), invent_target.ls.month)]
  # file.day <- invent_target.ls.day[grepl(paste0("^",gsub(' ','_',country, fixed=TRUE),".*\\.xlsx"), invent_target.ls.day)]
  if (length(file.month)==1){
    df0 = read_excel(paste0(folder_path,file.month))
    df_month = df0 %>%  filter(!is.na(KPI_diff)) %>% group_by(ShopNo, Year) %>% summarize(month=list(unique(Month)))
    # df1 = read_excel(paste0(folder_path,file.day))
    # df_day = df1 %>%  filter(!is.na(KPI_diff)) %>% group_by(ShopNo, Year, Month) %>% summarize(day=list(unique(Day)))
    invent_target_files[[gsub(' ','_',country, fixed=TRUE)]]=file.month
    invent_target_list[[gsub(' ','_',country, fixed=TRUE)]]=data.frame(df_month)
    #PRE_LOADING
    # print(file.month)
    # temp_name=file.month
    # print(temp_name)
    # temp_name0 = substr(temp_name, 1, nchar(temp_name)-5)
    # print(temp_name0)
    # do.call("<<-",list(temp_name0, df1))
    # invent_target_list[[gsub(' ','_',country, fixed=TRUE)]]["Day"]=data.frame(df_day)
  }
  else {invent_target_list[[gsub(' ','_',country, fixed=TRUE)]]="None"
  # invent_target_list[[gsub(' ','_',country, fixed=TRUE)]]["Day"]="None"
  }
}

## Get all the selection needed for inputs (Target from KPIs)

ml_outlier_list<- list()
outlier.ls <- list.files(path=folder_path,pattern='outlier.csv')
country_list = unique(gsub("_"," ",sub("\\_.*", "",outlier.ls)))
for (i in country_list){ml_outlier_list[i]=i}
for (country in (ml_outlier_list)){
  country_cat<-unique(str_match(outlier.ls, paste0(gsub(' ','_',country, fixed=TRUE),"_(.*?)(_shop_name_outlier)"))[,2])
  ml_outlier_list[[country]]=list(country_cat)
  
  file.name <- outlier.ls[grepl(paste0("^",gsub(' ','_',country, fixed=TRUE),".*\\.csv"), outlier.ls)]
  for (add in file.name){
    cat = str_match(add, "_(.*?)(_shop_name_outlier)")[,2]
    if (is.na(cat)!=TRUE){
      ml_outlier_list[[country]][cat]=substr(add, 1, nchar(add)-4)
      df=read.csv(paste0(folder_path,add))
      if (country!="Singapore"){
        df = subset(df, select = -c(ShopNo) )
        names(df)[names(df) == "ShopName"] <- "ShopNo"
      }
      #PRE_LOADING
      print(add)
      temp_name=add
      print(temp_name)
      temp_name0 = substr(temp_name, 1, nchar(temp_name)-4)
      print(temp_name0)
      do.call("<<-",list(temp_name0, df))
      df=df%>% group_by(ShopNo, Year) %>% summarize(month=list(unique(Month)))
      df=data.frame(df)
      ml_outlier_list[[country]][[1]][cat]=list(unique(df$ShopNo))
    }
  }
}

inv_check_list <- list()
inv.check.ls <- list.files(path=folder_path,pattern='inv_checker.xlsx')
country_list = unique(gsub("_"," ",sub("\\_.*", "",inv.check.ls)))
for (i in country_list){inv_check_list[i]=i}
for (country in (inv_check_list)){
  file.name <- inv.check.ls[grepl(paste0("^",gsub(' ','_',country, fixed=TRUE),".*\\.xlsx"), inv.check.ls)]
  for (add in file.name){
    cat<-unique(str_match(add, "_(.*?)(_inv_checker)")[,2])
    if (isTRUE(cat=='shop_level')){
      inv_check_list[[gsub(' ','_',country, fixed=TRUE)]][["Shop"]]=add
    }
    else
    {
      inv_check_list[[gsub(' ','_',country, fixed=TRUE)]][["country"]]=add
    }
  }
}



ui <- dashboardPage(
  dashboardHeader(
    title = "Red Flag System",
    tags$li(
      actionLink(
        'openModal', 
        label = "",
        icon = icon("home"),
        onclick ="location.href='https://planning-dashboard.e-charleskeith.com/dashboard_home/';"
      ),
      class = "dropdown"
    )
  ),
  dashboardSidebar(sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("th"),
             menuSubItem("Year on Year", tabName = "yoy"),
             menuSubItem("VS Model Forecast Target", tabName = "vstarget"),
             menuSubItem("VS CK Inventory Target", tabName = "vstarget_inv")
             #menuSubItem("VS Planning Target", tabName = "vstarget_plan")
    ),
    uiOutput('country_selector'),
    uiOutput('shop_selector'),
    uiOutput('year_selector'),
    uiOutput('cat_selector'),
    uiOutput("menu_creator")
    )),
  
  
  ## For YoY
  
  dashboardBody(
    tabItems(
      tabItem("yoy",
              fluidRow(
                # Dynamic valueBoxes
                valueBoxOutput("salesBox",width = 4),
                valueBoxOutput("rosBox",width = 4),
                valueBoxOutput("qBox",width = 4),
                div(infoBoxOutput("goodBox",width = 6), style = "font-size:130%"),
                div(infoBoxOutput("relookBox",width = 6), style = "font-size:130%"),
                tags$head(
                  tags$style(HTML("
                                  @import url('//fonts.googleapis.com/css?family=Merriweather:Bold|Cabin:400,700');
                                  
                                  h1 {
                                  font-family: 'Merriweather', cursive;
                                  font-weight: 500;
                                  line-height: 1.1;
                                  
                                  color: #C70039;
                                  }
                                  
                                  "))
                  ),
                h1(textOutput("sales_text")),
                br(),
                headerPanel("Operation"),
                br(),
                div(column(12,dataTableOutput('table1')), style = "font-size:110%"),
                br(),
                # div(column(3,tableOutput('table_event')), style = "font-size:110%"),
                br(),
                headerPanel("Inventory"),
                br(),
                valueBoxOutput("PDBox",width = 4),
                valueBoxOutput("ASPBox",width = 4),
                valueBoxOutput("MBox",width = 4),
                br(),
                div(column(12,dataTableOutput('table2'))), style = "font-size:110%")
                  ),
      tabItem("vstarget",

              fluidRow(
                h2(textOutput("var_selection")),
                br(),
                
                infoBoxOutput("goodpara",width = 6),
                infoBoxOutput("badpara",width = 6),
                box(
                  title = " Summary KPI movement", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 12,
                  plotOutput("plot3", height = 350)
                ),
                tabBox(
                  title = tagList(shiny::icon("gear"), "Movement list"),selected = "Positive",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", width=12,
                  tabPanel("Negative", "Down Movement ",dataTableOutput('table4')),
                  tabPanel("Positive", "Up movement",dataTableOutput('table3'))
                )
              )
      ),
      tabItem("vstarget_inv",

              fluidRow(
                h2(textOutput("var_selection_inv",container = tags$li)),
                br(),
                infoBoxOutput("goodstore_inv",width = 6),
                infoBoxOutput("badstore_inv",width = 6),
                tabBox(
                  title = tagList(shiny::icon("gear"), "Movement list"),selected = "Positive",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", width=12,
                  tabPanel("Negative", "Down Movement ",dataTableOutput('table4_inv')),
                  tabPanel("Positive", "Up movement",dataTableOutput('table3_inv')))
              )
      )
                )
              ), 
  skin="red")

server <- function(input, output, session) {

  output$country_selector <- renderUI({
    if (input$tabs %in% c("yoy")) {
    selectInput(inputId= "country_name", label= "Select country", 
                                                      choices= unique(sub("_", " ",names(country_month_list))), selected ="Singapore",
                                                      multiple=FALSE)}
    else if (input$tabs %in% c("vstarget_inv")){
      selectInput(inputId= "country_name", label= "Select country", 
                                                            choices= names(invent_target_list), selected ="Singapore",
                                                            multiple=FALSE)
    }
    else if (input$tabs %in% c("vstarget")){
      selectInput(inputId= "country_name", label= "Select country", 
                  choices= names(ml_outlier_list), selected ="Singapore",
                  multiple=FALSE)}
  })
  
  CKS_event_list_edited <- reactive({
    req(input$country_name)
    print(input$country_name)
    if (input$country_name=="Singapore")
      {event_list=read_excel(paste0(folder_path,"Singapore_event_list_edited.xlsx"))}
    else {event_list=data.frame(ShopNo=character(),Event=character())}
    event_list
  })
    
  
  output$shop_selector <- renderUI({
    req(input$country_name)

    if (input$tabs %in% c("yoy")) {
  selectInput(inputId= "shop_name", label= "Select shop", 
                                                     choices= unique(country_shop_list[[gsub(" ", "_", input$country_name)]]$ShopNo), selected ="Total",
                                                     multiple=FALSE)}
    else if (input$tabs %in% c("vstarget_inv")){
      selectInput(inputId= "shop_name", label= "Select shop", 
                  choices= unique(invent_target_list[[gsub(" ", "_", input$country_name)]]$ShopNo), selected ="Total",
                  multiple=FALSE)}
    else if (input$tabs %in% c("vstarget")){
      selectInput(inputId= "shop_name", label= "Select shop", 
                  choices= (unique(ml_outlier_list[[gsub(" ", "_", input$country_name)]][[1]][input$cat_name][1])[[1]]), selected ="Total",
                  multiple=FALSE)}
    
  })
  output$cat_selector <- renderUI({
    req(input$country_name)
    if (input$tabs %in% c("vstarget")) {
      cat_table<-names(ml_outlier_list[[gsub(" ", "_", input$country_name)]][[1]])
      selectInput(inputId= "cat_name", label= "Select category", 
                  choices= cat_table[cat_table!=""], selected ='bags',
                  multiple=FALSE)}})

  df_1 <-reactive({
      req(input$country_name)
      temp_name = country_month_list[[gsub(" ", "_", input$country_name)]]
      df_1<-eval(as.name(temp_name))
      # if("ShopName" %in% colnames(df_1))
      # {
      #   df_1 = subset(df_1, select = -c(ShopNo) )
      #   names(df_1)[names(df_1) == "ShopName"] <- "ShopNo"
      # }
      # else{}
    df_1
  })
  
  invent_target <-reactive({
    req(input$country_name)
    temp_name1 = invent_target_files[[gsub(" ", "_", input$country_name)]]
    invent_target<-read_excel(paste(folder_path,temp_name1, sep = ''))
    invent_target
  })
  
  
  output$year_selector <- renderUI({
    req(input$country_name)
    req(input$shop_name)
    if (input$tabs %in% c("yoy")) {
    year_table= country_shop_list[[gsub(" ", "_", input$country_name)]]
    yoy_year =year_table[(year_table$ShopNo==input$shop_name),]$Year
    selectInput(inputId = "year",
                label = "Choose a year:",
                choices = c('2018', '2019'),
                selected = 2019)}
    else if (input$tabs %in% c("vstarget_inv")) {
      month_table<-invent_target_list[[gsub(" ", "_", input$country_name)]]
      yoy_year_inv =month_table[(month_table$ShopNo==input$shop_name),]$Year
      selectInput(inputId = "year",
                  label = "Choose a year:",
                  choices =  yoy_year_inv,
                  selected = 2019)
    }
    else if (input$tabs %in% c("vstarget")) {
      selectInput(inputId = "year",
                  label = "Choose a year:",
                  choices = c('2018','2019'),
                  selected = 2019)
    }
    
  })
  
  outlier<-reactive({
    req(input$country_name)
    req(input$cat_name)
    temp_name3 = paste0(input$country_name,"_",input$cat_name,"_shop_name_outlier" )
    df<-eval(as.name(temp_name3))
    df})


  output$menu_creator <- renderUI({
    req(input$shop_name)
    req(input$year)
    req(input$country_name)

    # yoy_week = yoyweekInput()
    if (input$tabs %in% c("yoy")) {
      year_table<-country_shop_list[[gsub(" ", "_", input$country_name)]]
      week_list<-year_table[(year_table$ShopNo==input$shop_name & year_table$Year==input$year),]$week[[1]]
      dyn_ui <- list(selectInput(inputId = "week",
                      label = "Choose a week:",
                      choices = week_list,
                      selected =1)
      )
    }
    else if (input$tabs %in% c("vstarget_inv")) {
      month_table<-invent_target_list[[gsub(" ", "_", input$country_name)]]
      month_list<-month_table[(month_table$ShopNo==input$shop_name & month_table$Year==input$year),]$month[[1]]
      dyn_ui <- list(selectInput(inputId = "vstarget_inv_month",
                                 label = "Choose a month:",
                                 choices =month_list,
                                 selected =1)
      )
    }
    else if (input$tabs %in% c("vstarget")) {
      req(outlier())
      outlier=outlier()
      month_list1<-outlier[(outlier$ShopNo==input$shop_name & outlier$Year==input$year),]$Month
      month_list1=unique(month_list1)
      
      dyn_ui <-
        list(selectInput(inputId = "vstarget_month",
                                 label = "Choose a month:",
                                 choices =month_list1[sort.list(month_list1)],
                                 selected =1))

    }
    else if (input$tabs %in% c("vstarget_plan"))
      {dyn_ui <-NULL
    }
    return(dyn_ui)
  })
  
  
  q<-reactive({
    req(input$shop_name)
    req(input$year)
    req(input$week)
    req(df_1())
    df_1=df_1()
    
  # if (is.null(df_1[(df_1$Year == input$year & df_1$Week == input$week & df_1$ShopNo != input$shop_name), ])!=TRUE){
  if (input$shop_name=='Total')
  {df_1=data.frame(df_1[(df_1$Year == input$year & df_1$Week == input$week & df_1$ShopNo != input$shop_name), ])
  df_1=df_1[c('ShopNo', 'Action', 'Remark', 'Traffic_alert', 'Holiday')]
  df_1=merge(x = df_1, y = event(), by = "ShopNo", all = TRUE)
  }
  else
  {df_1=data.frame(df_1[(df_1$Year == input$year & df_1$Week == input$week & df_1$ShopNo == input$shop_name), ])
  df_1=df_1[c('ShopNo', 'Action', 'Remark', 'Traffic_alert', 'Holiday')]
  df_1=merge(x = df_1, y = event(), by = "ShopNo", all.x = TRUE)
  }
  
  row.names(df_1) <- NULL

  return(df_1%>% distinct())
    # }
  # else{return (NULL)}
  
  })

## For YoY
  
  output$salesBox <- renderValueBox({
    req(input$shop_name)
    req(input$year)
    req(input$week)
    req(df_1())
    if(is.null(df_1())!=TRUE){
    df_1 <-df_1()
    
    sales =(df_1[(df_1$Year == input$year & df_1$Week == input$week & df_1$ShopNo == input$shop_name),]$Sales.)
    if (sales>0){valueBox(
      paste0("+",sales, "%"), "Sales YoY", icon = NULL,
      color = "green"
    )}else if(sales<0){valueBox(
      paste0(sales, "%"), "Sales YoY", icon = NULL,
      color = "red"
    )}
    else if(sales==0){valueBox(
      paste0(sales, "%"), "Sales YoY", icon = NULL,
      color = "purple"
    )}}
    else{
      return (NULL)
      }

  })

  output$rosBox <- renderValueBox({
    req(input$shop_name)
    req(input$year)
    req(input$week)
    req(df_1())
    df_1 <-df_1()
    ROS = as.numeric(df_1[(df_1$Year == input$year & df_1$Week == input$week  & df_1$ShopNo == input$shop_name), ]$ROS.)
    if (ROS > 0){valueBox(
      paste0("+",ROS, "%"), "ROS YoY", icon = NULL,
      color = "green"
    )}
    else if(ROS < 0) {valueBox(
      paste0(ROS, "%"), "ROS YoY", icon = NULL,
      color = "red"
    )}
    else if(ROS == 0) {valueBox(
      paste0(ROS, "%"), "ROS YoY", icon = NULL,
      color = "purple"
    )}
  })
  output$qBox <- renderValueBox({
    req(input$shop_name)
    req(input$year)
    req(input$week)
    req(df_1())
    df_1 <-df_1()
    Quantity  = as.numeric(df_1[(df_1$Year == input$year & df_1$Week == input$week & df_1$ShopNo == input$shop_name), ]$SoldQty.)
    if (Quantity > 0){valueBox(
      paste0("+",Quantity, "%"), "Quantity YoY", "Progress", icon = NULL,
      color = "green"
    )}else if (Quantity < 0){valueBox(
      paste0(Quantity, "%"), "Quantity YoY", "Progress", icon = NULL,
      color = "red"
    )}else if (Quantity == 0){valueBox(
      paste0(Quantity, "%"), "Quantity YoY", "Progress", icon = NULL,
      color = "purple"
    )}
  })

  output$PDBox <- renderValueBox({
    req(input$shop_name)
    req(input$year)
    req(input$week)
    req(df_1())
    df_1 <-df_1()
    pd = as.numeric(df_1[(df_1$Year == input$year & df_1$Week == input$week  & df_1$ShopNo == input$shop_name),]$PD.)
    pd = NaRV.omit(pd)
    if (pd>0){valueBox(
      paste0("+",pd, "%"), "Profit Dilution YoY", icon = NULL,
      color = "red"
    )}else if(pd<0){valueBox(
      paste0(pd, "%"), "Profit Dilution YoY", icon = NULL,
      color = "green"
    )}
    else if(pd==0){valueBox(
      paste0(pd, "%"), "Profit Dilution YoY", icon = NULL,
      color = "purple"
    )}

  })

  output$ASPBox <- renderValueBox({
    req(input$shop_name)
    req(input$year)
    req(input$week)
    req(df_1())
    df_1 <-df_1()
    ASP = as.numeric(df_1[(df_1$Year == input$year & df_1$Week == input$week & df_1$ShopNo == input$shop_name), ]$ASP.)
    if (ASP > 0){valueBox(
      paste0("+",ASP, "%"), "ASP YoY", icon = NULL,
      color = "green"
    )}
    else if(ASP < 0) {valueBox(
      paste0(ASP, "%"), "ASP YoY", icon = NULL,
      color = "red"
    )}
    else if(ASP == 0) {valueBox(
      paste0(ASP, "%"), "ASP YoY", icon = NULL,
      color = "purple"
    )}
  })
  output$MBox <- renderValueBox({
    req(input$shop_name)
    req(input$year)
    req(input$week)
    req(df_1())
    df_1 <-df_1()
    M  = as.numeric(df_1[(df_1$Year == input$year & df_1$Week == input$week & df_1$ShopNo == input$shop_name), ]$M.)
    if (M > 0){valueBox(
      paste0("+",M, "%"), "Margin YoY", "Progress", icon = NULL,
      color = "green"
    )}else if (M < 0){valueBox(
      paste0(M, "%"), "Margin YoY", "Progress", icon =NULL,
      color = "red"
    )}else if (M == 0){valueBox(
      paste0(M, "%"), "Margin YoY", "Progress", icon =NULL,
      color = "purple"
    )}
  })

  
  df_3 <-reactive({
    req(input$shop_name)
    req(input$year)
    req(input$week)
    req(df_1())
    if (input$shop_name=="Total" & is.null(inv_check_list[[(gsub(' ','_',input$country_name, fixed=TRUE))]][["country"]])!=TRUE){
      add=inv_check_list[[(gsub(' ','_',input$country_name, fixed=TRUE))]][["country"]]
      df_3 =read_excel(paste0(folder_path,add))
    }
    else if (input$shop_name=="Total" & is.null(inv_check_list[[(gsub(' ','_',input$country_name, fixed=TRUE))]][["country"]])==TRUE){
      df_3 =NULL
    }
    else if (input$shop_name!="Total" & is.null(inv_check_list[[(gsub(' ','_',input$country_name, fixed=TRUE))]][["Shop"]])==TRUE){
      df_3 =NULL
    }
    else
    {
      add=inv_check_list[[(gsub(' ','_',input$country_name, fixed=TRUE))]][["Shop"]]
      df_3 =read_excel(paste0(folder_path,add))
    }
    if (is.null(df_3)){return (NULL)}
    else{return (df_3)}
    req(df_3)
    return(df_3)
  })

  r <- reactive ({
    req(input$shop_name)
    req(input$year)
    req(input$week)
    req(df_3())
  if(input$shop_name=="Total"){
    df_3 = df_3()
    table = df_3[(df_3$year == input$year & df_3$week == input$week ), ]
    print(table)
    }

  else {
    df_3 = df_3()
    table = df_3[(df_3$year == input$year & df_3$week == input$week & df_3$ShopNo == input$shop_name ), ]}
  Cat=rm_between(table$Category[1], "'", "'", extract=TRUE)[[1]]
  Alert=rm_between(table$Alert[1], "'", "'", extract=TRUE)[[1]]
  Grading=rm_between(table$Actual_Grading[1], "'", "'", extract=TRUE)[[1]]
  ART=strsplit(gsub("\\[|\\]", "", table$`ART%`[1]), "\\s+")[[1]]
  SALES=strsplit(gsub("\\[|\\]", "", table$`SALES%`[1]), "\\s+")[[1]]
  SOLD=strsplit(gsub("\\[|\\]", "", table$`SOLD%`[1]), "\\s+")[[1]]
  SOH=strsplit(gsub("\\[|\\]", "", table$`SOH%`[1]), "\\s+")[[1]]
  WT=strsplit(gsub("\\[|\\]", "", table$`WT%`[1]), "\\s+")[[1]]
  SALES[!SALES %in% ""]
  table3=data.frame("Cat"=Cat[!Cat %in% ""], 
                    "Alert"=Alert[!Alert %in% ""], 
                    "Grading"=Grading[!Grading %in% ""],
                    "ART%"=round(as.numeric(ART[!ART %in% ""])*100,1),
                    "SALES%"=round(as.numeric(SALES[!SALES %in% ""])*100,1), 
                    "SOLD%"=round(as.numeric(SOLD[!SOLD %in% ""])*100,1), 
                    "SOH%"=round(as.numeric(SOH[!SOH %in% ""])*100,1), 
                    "WT%"=round(as.numeric(WT[!WT %in% ""])*100,1))
  
  if (is.null(table3)){return (NULL)}
  else{return (table3)}
  
  })
  
  output$goodBox <- renderInfoBox({
    req(input$shop_name)
    req(input$year)
    req(input$week)
    req(df_1())
    table=q()
    table = table %>% dplyr::filter(Action == "Positive operation.")
    if (nrow(table)==0){shop=""}
    else{
      shop=(paste(as.character(table$Shop), collapse=", "))
      shop=gsub("\'","\n ", shop)}
    infoBox(
      "Good stores",tags$span(shop ,style="font-size:100%;"),  icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green",fill=FALSE
    )
  })
  
  output$relookBox <- renderInfoBox({
    req(input$shop_name)
    req(input$year)
    req(input$week)
    req(df_1())
    table=q()
    table = table %>% dplyr::filter(Action != "Positive operation.") %>% dplyr::filter(Action != "Relook holidays and events!")  %>% dplyr::filter(Remark != "new")
    shop=paste(as.character(table$Shop), collapse=", ")
    shop=gsub("\'","\n ", shop)
    infoBox(
      "Alert stores",tags$span(shop,style="font-size:100%;"),  icon = icon("exclamation", lib = "font-awesome"),
      color = "yellow",fill=FALSE
    )
  })
  event<- reactive({
    req(input$year)
    req(input$week)
    CKS_event_list_edited=CKS_event_list_edited()
    table_event=CKS_event_list_edited[(CKS_event_list_edited$year== input$year) & (CKS_event_list_edited$week== input$week),] %>% select(ShopNo, Event) %>% distinct()
  return (table_event)})
  output$table1 <-renderDataTable((
    q()
    
  ))
  # output$table_event <-renderTable({
  #   req(event())
  #   if (is.null(event())){ return(NULL)}
  #   else{return(event())}
  #   }
  # )
  output$table2 <-renderDataTable((
    r()
  ))
  
  ## For predicted target 
  
  output$var_selection <- renderText({paste(" You have selected year ", input$year, "for month",input$vstarget_month, " .")})
  
  
  
  s <- reactive ({
    req(input$year)
    req(input$vstarget_month)
    req(input$shop_name)
    outlier=outlier()
    if (input$shop_name =="Total"){
      table_s=data.frame("Shop"=outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$ShopNo,
                                                      "Type"=outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$Type,
                                                      "Movement"=(outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$Actual-outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$Predicted)/outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$Actual)}
    else{table_s=data.frame("Shop"=outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$ShopNo,
                                     "Type"=outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$Type,
                                     "Movement"=(outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$Actual-outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$Predicted)/outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$Actual)}
  table_s = table_s %>% dplyr::filter(Movement > 0)%>% mutate_at(vars(Movement), funs(round(., 1)*100))
  names(table_s)[names(table_s) == "Movement"] <- "Movement %"
  table_s$Type=gsub('.{0}$', '', table_s$Type)
  req(table_s)
  datatable(table_s) %>% 
    formatStyle(columns = "Movement %", target = "cell", backgroundColor = "#9EEA74")
  })
  t <- reactive ({
    req(input$year)
    req(input$vstarget_month)
    req(input$shop_name)
    outlier=outlier()
    if (input$shop_name =="Total"){
    table_t=data.frame("Shop"=outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$ShopNo,
                                     "Type"=outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$Type,
                                     "Movement"=(outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$Actual-outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$Predicted)/outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$Actual)}
    else{table_t=data.frame("Shop"=outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$ShopNo,
                            "Type"=outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$Type,
                            "Movement"=(outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$Actual-outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$Predicted)/outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$Actual)}
    
  table_t = table_t %>% dplyr::filter(Movement < 0)%>% mutate_at(vars(Movement), funs(round(., 1)*100))
  names(table_t)[names(table_t) == "Movement"] <- "Movement %"
  table_t$Type=gsub('.{0}$', '', table_t$Type)
  req(table_t)
  datatable(table_t) %>% formatStyle(columns = "Movement %", target = "cell", backgroundColor = "#F67C6B")
  
  })
  
  
  output$tabset1Selected <- renderDataTable((
    s()
  ))
  output$table3 <- renderDataTable((
    
    s()
  ))
  output$table4 <- renderDataTable((
    t()
  ))
  
  v <- reactive ({
    req(input$year)
    req(input$vstarget_month)
    req(input$shop_name)
    outlier=outlier()
    if (input$shop_name=="Total"){ table_v = data.frame("Shop"=outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$ShopNo,
                                                        "Type"=outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$Type,
                                                        "Movement"=(outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$Actual-outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$Predicted)/outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month), ]$Actual)
    }
    else{table_v = data.frame("Shop"=outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month) & (outlier$ShopNo == input$shop_name), ]$ShopNo,
                                       "Type"=outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$Type,
                                       "Movement"=(outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$Actual-outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$Predicted)/outlier[(outlier$Year == input$year) & (outlier$Month == input$vstarget_month)& (outlier$ShopNo == input$shop_name), ]$Actual)
    }
  req(table_v)
  return (table_v)})
  
  output$goodpara <- renderInfoBox({
    table_v = v()
    req(table_v)
    table_v = table_v %>% group_by(Type)  %>% summarise(avg = mean(Movement, na.rm = TRUE))
    table_v = table_v %>% dplyr::filter(avg > 0)
    table_v$Type=gsub('.{0}$', '', table_v$Type)
    
    shop=paste(as.character(table_v$Type), collapse=", ")
    
    infoBox(
      "Healthy parameter",tags$span(shop ,style="font-size:100%;"),  icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green",fill=FALSE
    )
  })
  
  output$badpara <- renderInfoBox({
    req(v())
    table_v = v()
    table_v = table_v %>% group_by(Type)  %>% summarise(avg = mean(Movement, na.rm = TRUE))
    table_v = table_v %>% dplyr::filter(avg < 0)
    table_v$Type=gsub('.{0}$', '', table_v$Type)
    shop=paste(as.character(table_v$Type), collapse=", ")
    
    infoBox(
      "Alert parameter",tags$span(shop ,style="font-size:100%;"),  icon = icon("exclamation", lib = "font-awesome"),
      color = "orange",fill=FALSE
    )
  })
  output$goodstore <- renderInfoBox({
    req(v())
    table_g = v()
    table_g = table_g %>% group_by(Shop)  %>% summarise(avg = mean(Movement, na.rm = TRUE)) %>% dplyr::filter(avg >0)
    shop2 = paste(as.character(table_g$Shop), collapse=", ")
    
    infoBox(
      "Overall good stores",tags$span(shop2 ,style="font-size:100%;"),  icon = icon("ok", lib = "glyphicon"),
      color = "green",fill=TRUE
    )
  })
  
  output$badstore <- renderInfoBox({
    req(v())
    table_b = v()
    table_b = table_b %>% group_by(Shop)  %>% summarise(avg = mean(Movement, na.rm = TRUE)) %>% dplyr::filter(avg <0)
    shop1=paste(as.character(table_b$Shop), collapse=", ")
    
    infoBox(
      "Overall alert stores",tags$span(shop1,style="font-size:100%;"),  icon = icon("flag", lib = "glyphicon"),
      color = "yellow",fill=TRUE
    )
  })
  
  output$plot1 <- renderPlot({
    req(s())
    group1 <- data.frame(s())
    s = group1 %>% group_by(Type)  %>% summarise(avg = mean(Movement, na.rm = TRUE))
    qplot(x = Type, y = avg, color = Type, data = s, geom = "point",size=1)
  })
  
  output$plot2 <- renderPlot({
    req(t())
    group1 <- data.frame(t())
    t = group1 %>% group_by(Type)  %>% summarise(avg = mean(Movement, na.rm = TRUE))
    qplot(x = Type, y = avg, color = Type, data = t, geom = "point",size=1)
  })
  
  output$plot3 <- renderPlot({
    req(v())
    v <- data.frame(v())
    v = v %>% group_by(Type)  %>% summarise(avg = mean(Movement, na.rm = TRUE))
    v$Type=gsub('.{5}$', '', v$Type)
    qplot(x = Type, y = avg, color = Type, data = v, geom = "point", size=1)+ geom_hline(yintercept=0, size=2)
  })
  
  ### For target by SAP
  
  # observe({
  #   calc_target_table_month=na.omit(calc_target_table_month)
  #   val <- input$yeartarget_inv
  #   updateSliderInput(session, "monthtarget_inv", value = 3,
  #                     min = 1, max = max(calc_target_table_month[(calc_target_table_month$Year == input$yeartarget_inv), ]$Month)  , step = 1)
  # })
  # 
  # observe({
  #   invent_target=na.omit(invent_target())
  #   val <- input$yeartarget_inv
  #   updateSliderInput(session, "day_month_target_inv", value = 3,
  #                     min = 1, max = max(invent_target[(invent_target$Year == input$yeartarget_inv), ]$Month)  , step = 1)
  # })
  # 
  # observe({
  #   invent_target=na.omit(invent_target())
  #   val <- input$yeartarget_inv
  #   updateSliderInput(session, "daytarget_inv", value = 3,
  #                     min = 1, max = max(invent_target[(invent_target$Year ==input$yeartarget_inv)&(invent_target$Month ==input$day_month_target_inv), ]$Day)  , step = 1)
  # })
  
  ### For Target Inventory
  
  output$var_selection_inv <- renderText(
    # if (isTruthy(input$day_month_target_inv)){paste(" You have selected year ", input$year, "for month",input$vstarget_inv_month," for day",input$daytarget_inv, " .")}
    #                                      else{
                                           paste(" You have selected year ", input$year, "for month",input$vstarget_inv_month, " sales performance.")
                                           # }
    )

  
  m <-reactive({
    req(input$shop_name)
    req(input$year)
    req(input$week)
    invent_target=invent_target()
    if (input$shop_name != 'Total') {
    table_m = data.frame(
        "Shop"=invent_target[(invent_target$Year == input$year) & (invent_target$Month == input$vstarget_inv_month)& (invent_target$ShopNo == input$shop_name), ]$ShopNo,
        "Actual"=invent_target[(invent_target$Year == input$year) & (invent_target$Month == input$vstarget_inv_month)& (invent_target$ShopNo == input$shop_name), ]$Actual,
        "Target"=invent_target[(invent_target$Year == input$year) & (invent_target$Month == input$vstarget_inv_month)& (invent_target$ShopNo == input$shop_name), ]$Predicted,
        "Movement"=invent_target[(invent_target$Year == input$year) & (invent_target$Month == input$vstarget_inv_month)& (invent_target$ShopNo == input$shop_name), ]$Movement,
        "KPI_percent"=invent_target[(invent_target$Year == input$year) & (invent_target$Month == input$vstarget_inv_month)& (invent_target$ShopNo == input$shop_name), ]$KPI_diff)}
    else{table_m = data.frame(
      "Shop"=invent_target[(invent_target$Year == input$year) & (invent_target$Month == input$vstarget_inv_month), ]$ShopNo,
      "Actual"=invent_target[(invent_target$Year == input$year) & (invent_target$Month == input$vstarget_inv_month), ]$Actual,
      "Target"=invent_target[(invent_target$Year == input$year) & (invent_target$Month == input$vstarget_inv_month), ]$Predicted,
      "Movement"=invent_target[(invent_target$Year == input$year) & (invent_target$Month == input$vstarget_inv_month), ]$Movement,
      "KPI_percent"=invent_target[(invent_target$Year == input$year) & (invent_target$Month == input$vstarget_inv_month), ]$KPI_diff)}
    req(table_m)
    return (table_m)
  })
  
  output$goodstore_inv <- renderInfoBox({
    req(m())
    table_g = m()
    table_g = table_g %>% group_by(Shop)  %>%  dplyr::filter(Movement == "Up")
    shop2 = paste(as.character(table_g$Shop), collapse=", ")
    
    infoBox(
      "Overall good stores",tags$span(shop2 ,style="font-size:100%;"),  icon = icon("ok", lib = "glyphicon"),
      color = "green",fill=TRUE
    )
  })
  
  output$badstore_inv <- renderInfoBox({
    req(m())
    table_b = m()
    table_b = table_b %>% group_by(Shop)  %>%  dplyr::filter(Movement == "Down")
    shop1=paste(as.character(table_b$Shop), collapse=", ")
    
    infoBox(
      "Overall alert stores",tags$span(shop1,style="font-size:100%;"),  icon = icon("flag", lib = "glyphicon"),
      color = "yellow",fill=TRUE
    )
  })
  output$table3_inv <- renderDataTable((
    m() %>% group_by(Shop) %>%   mutate_at(vars(KPI_percent),funs(as.numeric)) %>% mutate_at(vars(KPI_percent), funs(round(., 1)*100)) %>%  dplyr::filter(KPI_percent > 0 ) %>% datatable(m()) %>% formatStyle('KPI_percent', target = "cell", backgroundColor = "#9EEA74")
    
  )
  )
  output$table4_inv <- renderDataTable((
    m() %>% group_by(Shop) %>% mutate_at(vars(KPI_percent),funs(as.numeric)) %>%  mutate_at(vars(KPI_percent), funs(round(., 1)*100)) %>%  dplyr::filter(KPI_percent < 0 ) %>% datatable(m()) %>% formatStyle('KPI_percent', target = "cell", backgroundColor = "#F67C6B")
  ))
  

  
}

shinyApp(ui, server)

