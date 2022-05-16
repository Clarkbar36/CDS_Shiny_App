suppressMessages(library(shiny))
suppressMessages(library(tidyverse))
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(shinydashboard))
suppressMessages(library(DT))
suppressMessages(library(dbplyr))
suppressMessages(library(DBI))
suppressMessages(library(RSQLite))
suppressMessages(library(SqlRender))
suppressMessages(library(rhandsontable))
suppressMessages(library(rsconnect))
suppressMessages(library(purrr))
suppressMessages(library(stringr))


source('scripts/sql_help/all_depts.R', local = TRUE)
source('scripts/sql_help/search_departments.R', local = TRUE)
source('scripts/sql_help/network_departments.R', local = TRUE)
source('scripts/parameters/departments.R', local = TRUE)
source('scripts/parameters/notes.R', local = TRUE)
source('scripts/parameters/orders.R', local = TRUE)
source('scripts/parameters/att_provs.R', local = TRUE)
source('scripts/cohort/base_cohort.R', local = TRUE)
source('scripts/cohort/all_providers.R', local = TRUE)


header <- dashboardHeader(title = "CDS SQL")

sidebar <- dashboardSidebar(collapsed = FALSE,
                            sidebarMenu(
                              menuItem(
                                "Instructions",
                                tabName = "instr",
                                icon = icon('file-alt')
                              ),
                              menuItem(
                                'Department Upload',
                                tabName = 'deptsTab',
                                icon = icon('upload')
                              ),
                              menuItem(
                                "Parameters",
                                tabName = "parTab",
                                icon = icon("layer-group"),
                                menuSubItem("SQL Code Creation",
                                            tabName = "parameterTab",
                                            icon = icon("code"))
                              ),
                              menuItem(
                                "Cohort",
                                tabName = "cohTab",
                                icon = icon("layer-group"),
                                menuSubItem("SQL Code Creation",
                                            tabName = "cohortTab",
                                            icon = icon("code")),
                                menuSubItem('Attending Provider Upload',
                                            tabName = 'provTab',
                                            icon = icon('upload'))
                              )
                              # ,
                              # menuItem(
                              #     "Usage SQL Code Creation",
                              #     tabName = "usageTab",
                              #     icon = icon("layer-group")
                              # )
                            ))

body <- dashboardBody(
  tags$head(
    tags$style(
      "body {overflow-y: hidden;}"
    )),
  tabItems(
    tabItem(tabName = "instr",
            fluidRow(
              column(
                align = "center",
                offset = 1,
                width = 6,
                h2("Instructions"))
            ),
            fluidRow(
                column(
                    align = "center",
                    offset = 1,
                    width = 6,
                    textOutput('inst'))
            )
    ),

    tabItem(tabName = 'deptsTab',
            fluidRow(
              column(
                align = "center",
                width = 3,
                offset = 4,
                h2('Departments Upload'))),
            fluidRow(
              column(h3("Departments", align = "center"),
                     width = 4,
                     offset = 1),
              column(h3("Helper Queries", align = "center"),
                     width = 4,
                     offset = 2)
            ),
            fluidRow(
              column(
                align = "center",
                width = 1,
                offset = 1,
                selectInput("inType", label = "Select Input Type", choices = c('Input', 'Upload'))),
              # br(),
              # column(width = 2,
              #        downloadButton("downloadData", "Save Table")),
              column(
                align = "center",
                width = 2,
                fileInput(
                  "csvFile",
                  "Upload Department File")),
              #br(),
              column(align = "center",
                     width = 1,
                     downloadButton("downloadData", "Save Table"),
                     style = "margin-top: 25px;"),

              column(
                align = "center",
                offset = 1,
                width = 6,
                #h3("Helper Queries"),
                style='border-left:2px solid;'
              ),

              column(
                align = "center",
                width = 2,
                offset = 2,
                selectInput(
                  "helpquery",
                  "Select Which Query",
                  c(Choose = '' , 'All Departments', 'Search Specific Departments', 'Network Departments'))

              ),
              column(
                align = "center",
                width = 2,
                textInput(
                  "helpdept",
                  "Input Departments (Comma Seperated)")
              ),
              column(align = "center",
                     width = 1,
                     downloadButton("downloadSQLHelp", "Save SQL"),
                     style = "margin-top: 25px;")

            ),

            fluidRow(
              # br(), br(),
              column(
                width = 4,
                offset = 1,
                rHandsontableOutput("rTable"),
                tags$head(tags$style('#rTable * { word-wrap: break-word; max-height: 600px; max-width: 650px;}'))
              ),
              column(
                align = "left",
                width = 5,
                offset = 2,
                verbatimTextOutput("sqlhelp"),
                tags$head(tags$style("#sqlhelp{color:black; font-size:12px;
overflow-y:scroll; max-height: 700px; background: ghostwhite;}"))
              ))
    ),

    tabItem(tabName = "parameterTab",
            fluidRow(
              column(
                align = "center",
                width = 3,
                offset = 4,
                h2('Parameters SQL Creation'))),
            br(),
            fluidRow(
              column(
                align = "center",
                width = 3,
                offset = 2,
                selectInput(
                  "sql",
                  "Select SQL Type",
                  c(Choose = '' , 'MSSQL', 'Oracle')
                )
              ),
              column(
                align = "center",
                width = 3,
                selectInput(
                  "query",
                  "Select Which Query",
                  c(Choose = '' , 'Departments', 'Notes', 'Orders', 'Providers')
                )
              ),br(),
              column(align = "center",
                     width = 1,
                     downloadButton("downloadSQL", "Save SQL"),
                     style = "margin-top: 5px;")
            ),
            fluidRow(
              column(
                align = "left",
                width = 9,
                offset = 1,
                verbatimTextOutput("contents"),
                tags$head(tags$style("#contents{color:black; font-size:12px;
overflow-y:scroll; max-height: 700px; background: ghostwhite;}"))

              )
            )),
    tabItem(tabName = 'cohortTab',
            fluidRow(
              column(
                align = "center",
                width = 3,
                offset = 4,
                h2('Cohort SQL Creation'))),
            br(),
            fluidRow(
              column(
                align = "center",
                width = 3,
                offset = 2,
                selectInput(
                  "sqlcoh",
                  "Select SQL Type",
                  c(Choose = '' , 'MSSQL', 'Oracle')
                )
              ),
              column(
                align = "center",
                width = 4,
                selectInput(
                  "querycoh",
                  "Select Which Query",
                  c(Choose = '' , 'Cohort', 'All Providers')
                )
              ),br(),
              column(align = "center",
                     width = 1,
                     downloadButton("downloadSQLcoh", "Save SQL"),
                     style = "margin-top: 5px;")
            ),
            fluidRow(
              column(
                align = "left",
                width = 9,
                offset = 1,
                verbatimTextOutput("contentscoh"),
                tags$head(tags$style("#contentscoh{color:black; font-size:12px;
overflow-y:scroll; max-height: 700px; background: ghostwhite;}"))

              )

            )),
    tabItem(tabName = 'provTab',
            fluidRow(
              column(
                align = "center",
                width = 3,
                offset = 4,
                h2('Attending Provider Upload'))),
            fluidRow(
              column(h3("Attending Providers", align = "center"),
                     width = 4,
                     offset = 1)),
            fluidRow(
              column(
                align = "center",
                width = 1,
                offset = 1,
                selectInput("inTypeprov", label = "Select Input Type", choices = c('Input', 'Upload'))),
              # br(),
              # column(width = 2,
              #        downloadButton("downloadData", "Save Table")),
              column(
                align = "center",
                width = 3,
                fileInput(
                  "csvFileprov",
                  "Upload Attending Provider File")),
              #br(),
              column(align = "center",
                     width = 1,
                     downloadButton("downloadDataprov", "Save Table"),
                     style = "margin-top: 25px;")),
            fluidRow(
              # br(), br(),
              column(
                width = 4,
                offset = 1,
                rHandsontableOutput("rTableprov"),
                tags$head(tags$style('#rTableprov * { word-wrap: break-word; max-height: 600px; max-width: 750px;}'))))
    ),


    tabItem(tabName = 'usageTab',
            fluidRow(
              column(
                align = "center",
                width = 5,
                textOutput('plchlderUse')
              )

            ))



  ))

ui <- dashboardPage(title = 'CDS Code Creation Tool', header, sidebar, body, skin = 'purple')


server <- function(input, output, session) {
  rTable_content <- reactive({

    if(input$inType == "Input"){

      DF <- data.frame(departmentGroup = as.character(NA_character_), departmentID = as.integer(NA_integer_), openDate = as.character(NA_character_), closeDate = as.character(NA_character_), displayName = as.character(NA_character_))
    } else {
      req(input$csvFile)

      DF <- fread(input$csvFile$datapath)
    }

  })

  sql_query_out <- reactive({
    query_string <- if (input$sql == '') {
      return('Choose SQL Type')
    } else if (input$query == '') {
      return("Choose a Query")
    } else if (input$query == 'Departments') {
      depts_sql(hot_to_r(input$rTable), input$sql)
    } else if (input$query == 'Notes') {
      notes_sql(hot_to_r(input$rTable), input$sql)
    } else if (input$query == 'Orders') {
      orders_sql(hot_to_r(input$rTable), input$sql)
    } else if (input$query == 'Providers') {
      att_provs_sql(hot_to_r(input$rTable), input$sql)
    }  else {
      return('Wrong Inputs')
    }

  })

  help_query_out <- reactive({
    query_string <- if (input$helpquery == '') {
      return("Choose a Query")
    } else if (input$helpquery == 'All Departments') {
      cat(all_depts_sql())
    } else if (input$helpquery == 'Network Departments') {
      network_depts_sql(input$helpdept)
    } else if (input$helpquery == 'Search Specific Departments') {
      search_depts_sql(input$helpdept)
    }
    else {
      return('Wrong Inputs')
    }

  })

  define_cohorts_query_out <- reactive({
    query_string <- if (input$sqlcoh == '') {
      return('Choose SQL Type')
    } else if (input$querycoh == '') {
      return("Choose a Query")
    } else if (input$querycoh == 'Cohort') {
      cat(cohort_sql(hot_to_r(input$rTable), input$sqlcoh))
    } else if (input$querycoh == 'All Providers') {
      cat(all_provs_sql(hot_to_r(input$rTable), input$sqlcoh))
    }
    # else if (input$querycoh == 'All Providers') {
    #   cat(provs_sql(hot_to_r(input$rTable), input$sqlcoh))
    # }
    else {
      return('Wrong Inputs')
    }

  })


  output$rTable <- renderRHandsontable({

    rhandsontable(
      data = rTable_content(),
      rowHeaders = NULL,
      contextMenu = TRUE,
      width = 900,
      height = 900) %>%
      hot_col(c("openDate","closeDate"),  dateFormat = "M/D/YYYY", type = "date") %>%
      hot_cols(colWidths = c(130, 110, 110, 110, 130),
               manualColumnMove = FALSE,
               manualColumnResize = TRUE,
               halign = 'htCenter',
               valign = 'htMiddle'
      )
  })

  output$contents <- renderPrint({
    cat(sql_query_out())
    # tryCatch({

    # }, error = function(cond) {
    #     return("Please upload the depts.csv file in the Department File upload tab.")
    # }, warning = function(cond) {
    #     return("Please upload the depts.csv file in the Department File upload tab.")
    # })
  })

  output$sqlhelp <- renderPrint({
    cat(help_query_out())
    # tryCatch({

    # }, error = function(cond) {
    #     return("Please upload the depts.csv file in the Department File upload tab.")
    # }, warning = function(cond) {
    #     return("Please upload the depts.csv file in the Department File upload tab.")
    # })
  })

  output$contentscoh <- renderPrint({
    cat(define_cohorts_query_out())
    # tryCatch({

    # }, error = function(cond) {
    #     return("Please upload the depts.csv file in the Department File upload tab.")
    # }, warning = function(cond) {
    #     return("Please upload the depts.csv file in the Department File upload tab.")
    # })
  })

  output$downloadSQL <- downloadHandler(
    filename = function() {
      paste0(input$query,"_Query_",input$sql,".sql")
    },
    content = function(file) {

      write(sql_query_out(), file)
    }
  )

  output$downloadSQLHelp <- downloadHandler(
    filename = function() {
      paste0(input$helpquery,".sql")
    },
    content = function(file) {

      write(help_query_out(), file)
    }
  )

  output$downloadData <- downloadHandler(
    filename = "depts.csv",
    content = function(file) {
      write.csv(rTable_content(), file, row.names = FALSE)
    }
  )

  output$downloadSQLcoh <- downloadHandler(
    filename = function() {
      paste0(input$querycoh,"_Query_",input$sqlcoh,".sql")
    },
    content = function(file) {

      write(help_query_out(), file)
    }
  )

  output$downloadDataprov <- downloadHandler(
    filename = "attending_providers.csv",
    content = function(file) {
      write.csv(rTable_content_prov(), file, row.names = FALSE)
    }
  )

  rTable_content_prov <- reactive({

    if(input$inTypeprov == "Input"){

      DF_prov <- data.frame(providerID = as.integer(NA_integer_), userID = as.integer(NA_integer_), providerType = as.character(NA_character_),
                            providerName = as.character(NA_character_), deaprtmentGroup = as.character(NA_character_), departmentName = (NA_character_))
    } else {
      req(input$csvFileprov)

      DF_prov <- fread(input$csvFileprov$datapath)
    }

  })

  output$rTableprov <- renderRHandsontable({

    rhandsontable(
      data = rTable_content_prov(),
      rowHeaders = NULL,
      contextMenu = TRUE,
      width = 800,
      height = 700) %>%
      hot_cols(colWidths = c(90, 90, 110, 150, 150, 150),
               manualColumnMove = FALSE,
               manualColumnResize = TRUE,
               halign = 'htCenter',
               valign = 'htMiddle'
      )
  })

}

shinyApp(ui, server)

