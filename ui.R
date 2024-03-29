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


header <- dashboardHeader(title = "CDS SQL",
                          dropdownMenu(type = "messages",
                                       notificationItem(
                                           uiOutput("help"),
                                           icon("inbox")
                                       ),
                                       notificationItem(
                                           uiOutput("git"),
                                           icon("github")
                                       )))

sidebar <- dashboardSidebar(collapsed = FALSE,
                            sidebarMenu(
                                menuItem(
                                    "Steps Overview",
                                    tabName = "sbs",
                                    icon = icon('file-alt')
                                ),
                                menuItem(
                                    'Establish Departments',
                                                tabName = 'estDept',
                                                icon = icon('file-alt'),
                                    menuSubItem("Overview",
                                                tabName = "dptOvr",
                                                icon = icon('file-alt')),
                                    menuSubItem('Departments File Creation',
                                                tabName = 'deptsTab',
                                                icon = icon('upload'))
                                ),
                                menuItem(
                                    "Parameters",
                                    tabName = "parTab",
                                    icon = icon("layer-group"),
                                    menuSubItem("Overview",
                                                tabName = "parOvr",
                                                icon = icon('file-alt')),
                                    menuSubItem("SQL Code Creation",
                                    tabName = "parameterTab",
                                    icon = icon("code"))
                                ),
                                menuItem(
                                    "Cohort",
                                    tabName = "cohTab",
                                    icon = icon("layer-group"),
                                    menuSubItem("Overview",
                                                tabName = "cohOvr",
                                                icon = icon('file-alt')),
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
    tabItem(tabName = "sbs",
            fluidRow(
                column(width = 6,
                       offset = 1,
                       uiOutput('all_steps_rmd'),
                )
                )
            ),
    tabItem(tabName = 'dptOvr',
            fluidRow(
                column(width = 6,
                       offset = 1,
                       uiOutput('dept_rmd'),
                )
            )
    ),

    tabItem(tabName = 'deptsTab',
            fluidRow(
                column(
                    align = "center",
                    width = 3,
                    offset = 4,
                    h2('Department Upload'))),
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
    tabItem(tabName = "parOvr",
            fluidRow(
                column(width = 6,
                       offset = 1,
                       uiOutput('parameters_rmd'),
                )
            )
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
    tabItem(tabName = "cohOvr",
            fluidRow(
                column(width = 6,
                       offset = 1,
                       uiOutput('cohort_rmd'),
                )
            )
    ),
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
                       width = 3,
                       offset = 4)),
            br(),
            fluidRow(
                column(
                    align = "center",
                    width = 1,
                    offset = 3,
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
                    width = 8,
                    offset = 3,
                    rHandsontableOutput("rTableprov"),
                    tags$head(tags$style('#rTableprov * { word-wrap: break-word; max-height: 670px; max-width: 950px;}'))))
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
