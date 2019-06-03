library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(dashboardthemes)
library(leaflet)
library(leaflet.extras)
library(data.table)
library(googlesheets)
library(formattable)

for_gs <- gs_title("CallbackRefusalForm")
for_gs_sheet <- gs_read(for_gs)
target <- 369000
ui <- dashboardPage(
  dashboardHeader(title = "CBMS Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main", tabName = "main", icon = icon("dashboard")),
      menuItem("Locator", tabName = "loc", icon = icon("globe")),
      menuItem("Members", tabName = "mems", icon = icon("user")),
      menuItem("Files", tabName = "file", icon = icon("tasks")),
      menuItem("Maps", tabName = "widgets", icon = icon("map-marker")),
      menuItem("Graph", tabName = "es", icon = icon("signal"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme = "Grey light"),
    tabItems(
      # First tab content
      tabItem(
        tabName = "main",
        titlePanel('Main Data'),
        div(style = 'overflow-x: scroll', DT::dataTableOutput('contents')),
        tags$br(),
        tags$br(),
        fluidRow(box(
          fileInput("csvFile", "Upload main.csv over here!"),
          downloadButton('downloadData', label = "Download csv for mapping")
        ))
      ),
      tabItem(
        tabName = 'loc',
        box(
          title = 'Locate a Household',
          status = 'primary',
          solidHeader = T,
          textInput("caption", "", ""),
          leafletOutput('map')
        ),
        box(div(style = 'overflow-x: scroll', DT::dataTableOutput('leaf')))
      ),
      tabItem(
        tabName = 'mems',
        titlePanel('Members Data'),
        div(style = 'overflow-x: scroll', DT::dataTableOutput('contents1')),
        tags$br(),
        tags$br(),
        fluidRow(box(
          fileInput("csvFile1", "Upload hpq_mem.csv over here!")
        ))
      ),
      tabItem(
        tabName = 'file',
        titlePanel('Data'),
        div(style = 'overflow-x: scroll', DT::dataTableOutput('contents2')),
        fluidRow(box(
          fileInput("csvFile2", "Upload other csv file here!")
        ))
      ),
      tabItem(tabName = "widgets",
              htmlOutput("inc")),
      tabItem(tabName = "es",
              fluidPage(
                box(
                  titlePanel('Main'),
                  valueBoxOutput("pop"),
                  valueBoxOutput("hh"),
                  valueBoxOutput('pr')
                ),
                box(
                  titlePanel('Google Form'),
                  valueBoxOutput("ref"),
                  valueBoxOutput("cb"),
                  valueBoxOutput("va")
                )
              ),
              fluidRow())
    )
  )
)
server <- function(input, output) {
  gs <- reactive(return(for_gs_sheet))
  main <- eventReactive(input$csvFile, {
    fread(input$csvFile$datapath)
  })
  modifiedDataDD <- reactive({
    return(
      main() %>%
        select(
          main.id,
          geopoint_hh.latitude,
          geopoint_hh.longitude
          ,
          hcn,
          brgy,
          hhhead,
          respondent,
          phsize,
          hnum,
          street
        )
    )
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(modifiedDataDD(), file, row.names = F)
    }
  )
  modifiedData <- reactive({
    return(
      main() %>%
        select(
          main.id,
          hcn,
          brgy,
          hhhead,
          phsize,
          hnum,
          street,
          lng = geopoint_hh.longitude,
          lat = geopoint_hh.latitude,
          acc = geopoint_hh.accuracy
        )
    )
  })
  leafdata <- reactive({
    x <- as.numeric(unlist(strsplit(input$caption, ",")))
    return(modifiedData() %>%
             filter(main.id %in% x))
  })
  output$leaf <- renderDataTable(leafdata())
  output$contents <- renderDataTable(
    modifiedData(),
    filter = "top",
    selection = "multiple",
    escape = FALSE,
    server = T
  )
  output$map <- renderLeaflet({
    leaflet(leafdata()) %>% addTiles() %>%
      setView(121.145586, 14.732364, zoom = 14) %>%
      addCircleMarkers( ~ lng, ~ lat)
  })
  hpq_mem <- eventReactive(input$csvFile1, {
    fread(input$csvFile1$datapath)
  })
  modifiedData1 <- reactive({
    return(
      hpq_mem() %>%
        select(
          main.id,
          msname,
          mfname,
          mmname,
          birth_date,
          age_yr,
          educind,
          ofw,
          indust,
          wagcshm
        )
    )
  })
  output$contents1 <- renderDataTable({
    datatable(
      modifiedData1(),
      filter = "top",
      rownames = F,
      selection = "multiple",
      escape = FALSE
    )
  })
  other <- eventReactive(input$csvFile2, {
    read.csv(input$csvFile2$datapath)
  })
  output$contents2 <- renderDataTable({
    datatable(
      other(),
      filter = "top",
      rownames = F,
      selection = "multiple",
      escape = FALSE
    )
  })
  getPage <- function() {
    return(
      tags$iframe(
        src = "https://kepler.gl/demo"
        ,
        style = "width:100%;",
        frameborder = "0"
        ,
        id = "iframe"
        ,
        height = "560px"
      )
    )
  }
  output$inc <- renderUI({
    x <- input$test
    getPage()
  })
  output$pop <- renderValueBox({
    valueBox(
      sum(modifiedData()$phsize),
      'Population',
      icon = icon("user"),
      color = "yellow"
    )
  })
  output$hh <- renderValueBox({
    valueBox(NROW(modifiedData()),
             'Households',
             icon = icon("home"),
             color = "aqua")
  })
  output$pr <- renderValueBox({
    valueBox(percent(1 - ((
      target - sum(modifiedData()$phsize)
    ) / target)),
    'Progress',
    icon = icon("flash"),
    color = "fuchsia")
  })
  output$ref <- renderValueBox({
    valueBox(
      gs() %>%
        filter(Status == 'Refusal') %>%
        nrow(),
      'Refusal',
      icon = icon("remove"),
      color = "red"
    )
  })
  output$cb <- renderValueBox({
    valueBox(
      gs() %>%
        filter(Status == 'Callback') %>%
        nrow(),
      'Callback',
      icon = icon("repeat"),
      color = "aqua"
    )
  })
  output$va <- renderValueBox({
    valueBox(
      gs() %>%
        filter(Status == 'Vacant') %>%
        nrow(),
      'Vacant',
      icon = icon("sign"),
      color = "teal"
    )
  })
}
options(shiny.maxRequestSize = 400 * 1024 ^ 2)
shinyApp(ui, server)