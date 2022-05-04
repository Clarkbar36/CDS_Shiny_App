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

# list.files(path = 'C:/Users/aclark5/Desktop/CDS_App/Scripts/') %>%
#     str_subset('\\.R') %>%
#     walk(~source(.x,  local = TRUE))

source('scripts/all_depts.R', local = TRUE)
source('scripts/cohorts.R', local = TRUE)
source('scripts/departments.R', local = TRUE)
source('scripts/network_departments.R', local = TRUE)
source('scripts/notes.R', local = TRUE)
source('scripts/orders.R', local = TRUE)
source('scripts/provs.R', local = TRUE)
source('scripts/search_departments.R', local = TRUE)



header <- dashboardHeader(title = "CDS SQL Code Creator")

sidebar <- dashboardSidebar(collapsed = FALSE,
                            sidebarMenu(
                                menuItem(
                                    "Instructions",
                                    tabName = "instr",
                                    icon = icon("chart-line")
                                ),
                                menuItem(
                                    "Table Creations",
                                    tabName = "tableTab",
                                    icon = icon("chart-line"),
                                    startExpanded = TRUE,
                                    menuSubItem('Department Creation',
                                                tabName = 'deptsTab',
                                                icon = icon('chart-line')),
                                    menuSubItem('Attending Provider Creation',
                                                tabName = 'provTab',
                                                icon = icon('chart-line'))
                                ),
                                menuItem(
                                    "Parameter SQL Code Creation",
                                    tabName = "parTab",
                                    icon = icon("chart-line")
                                ),
                                menuItem(
                                    "Cohort SQL Code Creation",
                                    tabName = "cohortTab",
                                    icon = icon("chart-line")
                                ),
                                menuItem(
                                    "Usage SQL Code Creation",
                                    tabName = "usageTab",
                                    icon = icon("chart-line")
                                )
                            ))

body <- dashboardBody(tabItems(
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
                    textOutput('inst')),
                img(src = "params.png", height = 500, width = 250)
            )),

    tabItem(tabName = 'deptsTab',
            fluidRow(
                column(h2("Departments", align = "center"),
                       width = 4,
                       offset = 1),
                column(h2("Helper Queries", align = "center"),
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
                tags$style('#rTable * { word-wrap: break-word;}')),
                column(
                    align = "left",
                    width = 5,
                    offset = 2,
                    verbatimTextOutput("sqlhelp"),
                    tags$head(tags$style("#contents{color:black; font-size:12px;
overflow-y:scroll; max-height: 800px; background: ghostwhite;}"))
            ))),

    tabItem(tabName = "parTab",
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
overflow-y:scroll; max-height: 800px; background: ghostwhite;}"))

                )
            )),
    tabItem(tabName = 'cohortTab',
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
                        c(Choose = '' , 'Cohort', 'Attending Providers', 'All Providers')
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
overflow-y:scroll; max-height: 800px; background: ghostwhite;}"))

                )

            )),

    tabItem(tabName = 'usageTab',
            fluidRow(
                column(
                    align = "center",
                    width = 5,
                    textOutput('plchlderUse')
                )

            ))



))

ui <- dashboardPage(title = 'CDS', header, sidebar, body, skin = 'purple')


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
            provs_sql(hot_to_r(input$rTable), input$sql)
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
        } else if (input$querycoh == 'Attending Providers') {
          cat(provs_sql(hot_to_r(input$rTable), input$sqlcoh))
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
            paste0(input$querycoh,".sql")
        },
        content = function(file) {

            write(help_query_out(), file)
        }
    )



    output$plchlderUse <- renderText({
        return(
            "This tab will mirror the parameters tab, ask the user for SQL type, and which query to create, and it will spit out the Usage code."
        )
    })
}


# Run the application
shinyApp(ui = ui, server = server)
