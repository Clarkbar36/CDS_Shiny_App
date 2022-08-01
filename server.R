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

server <- function(input, output, session) {
  
  url <- a("Questions or Issues", href="mailto:alex_clark@urmc.rochester.edu; Adam_Dziorny@URMC.Rochester.edu")
  output$help <- renderUI({
    tagList(url) })
  
  git_url <- a("Github Repository", href="https://github.com/Clarkbar36/CDS_Shiny_App", target="_blank")
  output$git <- renderUI({
    tagList(git_url) })
  
  output$all_steps_rmd <- renderUI({
    tags$iframe(src='All_Steps.html',width="170%",frameBorder="0",height="900px")
  })
  
  rTable_content <- reactive({
    
    if(input$inType == "Input"){
      
      DF <- data.frame(departmentGroup = as.character(NA_character_), departmentID = as.integer(NA_integer_), openDate = as.character(NA_character_), closeDate = as.character(NA_character_), displayName = as.character(NA_character_))
    } else {
      req(input$csvFile)
      
      DF <- fread(input$csvFile$datapath)
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
  
  output$parameters_rmd <- renderUI({
    tags$iframe(src='parameters_overview.html',width="170%",frameBorder="0",height="900px")
  })
  
  sql_query_out <- reactive({
    tryCatch(
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
    },
    
    error = function(e){
      return("Please create or upload a department file via the Establish Departments tab.")
    }
    
    )
    
  })
  
  output$cohort_rmd <- renderUI({
    tags$iframe(src='cohort_overview.html',width="170%",frameBorder="0",height="900px")
  })
  
  define_cohorts_query_out <- reactive({
    tryCatch(
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
    },
    
    error = function(e){
      return("Please create or upload a department file via the Establish Departments tab.")
    }
    
    )
    
  })
  
  output$dept_rmd <- renderUI({
    tags$iframe(src='department_overview.html',width="170%",frameBorder="0",height="900px")
  })
  
  output$rTable <- renderRHandsontable({

    rhandsontable(
      data = rTable_content(),
      rowHeaders = NULL,
      contextMenu = TRUE,
      width = 900,
      height = 900) %>%
      hot_col(c("openDate","closeDate"),  dateFormat = "M/D/YYYY", type = "date") %>%
      hot_col(c("departmentGroup","departmentID"), strict = FALSE) %>%
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
      
      DF_prov <- data.frame(providerID = as.integer(NA_integer_), 
                            userID = as.integer(NA_integer_), 
                            providerType = as.character(NA_character_),
                            providerName = as.character(NA_character_), 
                            deaprtmentGroup = as.character(NA_character_), 
                            departmentName = as.character(NA_character_),
                            stringsAsFactors = FALSE)
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