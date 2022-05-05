suppressMessages(library(tidyverse))
suppressMessages(library(dbplyr))
suppressMessages(library(DBI))
suppressMessages(library(RSQLite))
suppressMessages(library(SqlRender))

search_depts_sql <- function(dept){
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  
  t <- dept %>%
    str_split(", ") %>%
    map(~ str_c("'",.x, "'")) %>% 
    unlist %>% 
    str_flatten(collapse = ", ")
  
  dept_filter <- read.table(text = t, sep = ",", fill = TRUE, as.is = TRUE) %>%
    data.frame(depts=unlist(.,use.names=FALSE)) %>%
    mutate(query_string = ifelse(
      row_number() == 1,
      paste0(
        "AND dpt.DEPARTMENT_NAME LIKE '%",
        trimws(toupper(depts)),
        "%' \n"
      ),
      paste0(
        "OR dpt.DEPARTMENT_NAME LIKE '%",
        trimws(toupper(depts)),
        "%' \n"
      )
    )) %>%
    pull(query_string) %>%
    sql_vector(., parens = FALSE, con = conn)
  
 search_dept_query <- dbplyr::build_sql("SELECT dpt.DEPARTMENT_ID
	,dpt.DEPARTMENT_NAME
	,loc.LOC_NAME
	,dpt.*
FROM CLARITY_DEP dpt
INNER JOIN CLARITY_LOC loc ON loc.LOC_ID = dpt.REV_LOC_ID
WHERE (loc.RECORD_STATUS IS NULL OR loc.RECORD_STATUS = '1') \n", dept_filter, con = conn)
  
  return (search_dept_query)
  
}