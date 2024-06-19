library(leaflet)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(shinyalert)
library(shinycssloaders)
library(shiny)

ui <- 
  dashboardPage( skin = "blue",
  dashboardHeader(
    title = "Data Laka Jakarta",
    titleWidth = 300,
    tags$li(class = "dropdown",
            tags$li(class = "dropdown",

                    style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px; "),
            tags$li(class = "dropdown",
                    actionButton("about", "Tentang Aplikasi"),
                    style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px; "))
    
  ),
  dashboardSidebar(
    useShinyalert(),
    HTML("<br>"),
    HTML("<div class='titles' id='selection1'>Pilihan 1</div>"),
    pickerInput("district", "Area", sort(as.character(districtList)), options = list("actions-box" = TRUE), multiple = FALSE, selected="DITLANTAS POLDA METRO"),
    pickerInput("year", "Tahun", yearList, options = list("actions-box" = TRUE), selected=c(2015:2020), multiple = TRUE),
    pickerInput("day", "Hari", levels(dayList), options = list("actions-box" = TRUE), multiple = TRUE, selected = dayList),
    # add new picker input- severity !!!!!!!!!!!!!!!!!!!!!
    pickerInput("Severity", "Tingkat Kecelakaan", levels(severityList), options = list("actions-box" = TRUE), multiple = TRUE, selected = severityList),
 
    bsTooltip("selection1", 
              "Silakan pilih area, hari, tahun, dan tingkat kecelakaan", placement = "bottom", trigger = "hover",
              options = NULL)
    
  ),
  
  dashboardBody(
    
    fluidRow(
      valueBoxOutput("info1", width = 12)
    ),
    fluidRow(
      
      valueBoxOutput("fatal1", width = 4),
      valueBoxOutput("severe1", width = 4),
      valueBoxOutput("minor1", width = 4),

    ),
  
    fluidRow( 
      box(
        title = "Frekuensi laka berdasarkan jam"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = FALSE 
        ,plotOutput("overTime", height = "300px") %>% withSpinner(color="#3c8dbc")
      ), 
      bsTooltip("overTime", 
                "Kurva menggambarkan % kecelakaan berdasarkan jam", placement = "bottom", trigger = "hover",
                options = NULL),
      box(
        title = "Tipe jalan"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = FALSE 
        ,plotOutput("byRoadType", height = "300px") %>% withSpinner(color="#3c8dbc")
      ),
      bsTooltip("byRoadType", 
                "Data laka berdasarkan tipe jalan", placement = "bottom", trigger = "hover",
                options = NULL),
    
    ),
    
    fluidRow( 
      box(
        title = "Data laka berdasarkan bulan"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = FALSE 
        ,plotOutput("overMonth", height = "300px") %>% withSpinner(color="#3c8dbc")
      ),
      bsTooltip("overMonth", 
                "Tiap diagram batang menggambarkan % laka sepanjang tahun berdasarkan bulan", placement = "bottom", trigger = "hover",
                options = NULL),
      box(
        title = "Data laka berdasarkan hari"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = FALSE 
        ,plotOutput("byDay", height = "300px") %>% withSpinner(color="#3c8dbc")
      ),
      bsTooltip("byDay", 
                "Tiap diagram batang menggambarkan % laka sepanjang tahun berdasarkan hari", placement = "bottom", trigger = "hover",
                options = NULL)
    ),
    fluidRow(
      box(
        title = " Prediksi kecelakaan menggunakan persentase",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 12,
        plotOutput("riskEstimation", height = "300px") %>% withSpinner(color="#3c8dbc")
      ),
      bsTooltip("riskEstimation", 
                "Prediksi laka ini menggunakan metode Machine Learning dengan interval waktu per 6 jam", placement = "top", trigger = "hover",
                options = NULL)
    ),
    tags$head(tags$style(HTML('.titles {font-weight:bold; text-align:center;
                                                  background-image: linear-gradient(#3c8dbc, #222d32)}')),
              tags$style(HTML('#about {background-color: #3c8dbc; font-weight:bold; color:white;}')),
              tags$style(HTML('div[role=tooltip] {font-weight:bold; color:white;}'))
    ),
    fluidRow(
      box(
        title = textOutput("Design") ,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 12,
        theme = shinythemes::shinytheme('simplex'),
        leaflet::leafletOutput('map1'),
        bsTooltip("map1", 
                  "Titik lokasi kecelakaan berdasarkan penandaan petugas. Silahkan klik lingkaran di kiri atas untuk melihat lokasi anda", placement = "top", trigger = "hover",
                  options = NULL),
        tags$style(type = "text/css", 
                   "html, body {width:100%;height;100%}
  #controls{background-color:white;padding:20px;}")
      )
        )
    


      ))
  




