suppressMessages(library(tidyverse))
suppressMessages(library(dbplyr))
suppressMessages(library(DBI))
suppressMessages(library(RSQLite))
suppressMessages(library(SqlRender))

all_depts_sql <- function(){
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  all_dpt_query <-
    paste(
      "SELECT dpt.DEPARTMENT_ID
	,dpt.DEPARTMENT_NAME
	,loc.LOC_NAME
	,dpt.*
FROM CLARITY_DEP dpt
INNER JOIN CLARITY_LOC loc ON loc.LOC_ID = dpt.REV_LOC_ID
WHERE (
		loc.RECORD_STATUS IS NULL
		OR loc.RECORD_STATUS = '1'
		)
ORDER BY REV_LOC_ID
	,dpt.DEPARTMENT_ID;"
    )
  
    return(dbplyr::build_sql(all_dpt_query, con = conn) %>% str_sub(.,2,-2))
    
  
}