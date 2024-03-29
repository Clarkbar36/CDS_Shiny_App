suppressMessages(library(tidyverse))
suppressMessages(library(dbplyr))
suppressMessages(library(DBI))
suppressMessages(library(RSQLite))
suppressMessages(library(SqlRender))

network_depts_sql <- function(dept){
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  
  
  dept_char <- dept %>%
    str_split(", ") %>%
    map(~ str_c("'",.x, "'")) %>% 
    unlist %>% 
    str_flatten(collapse = ", ") %>% 
    toupper() %>% 
    sql_vector(., con = conn)
  
  

  
  net_dept_query <- dbplyr::build_sql("SELECT DISTINCT dpt.DEPARTMENT_NAME
	,adt.DEPARTMENT_ID
	,MIN(adt.EFFECTIVE_TIME) AS FIRST_ADT_DATE
	,Max(adt.EFFECTIVE_TIME) AS LAST_ADT_DATE
	,loc.LOC_NAME
	,unt.NAME AS UNIT_TYPE
	,COALESCE(dpt.LICENSED_BEDS, 0) AS LICENSED_BEDS
	,COALESCE(dpt.IS_PERIOP_DEP_YN, 'N') AS IS_PERIOP_DEP_YN
	,COUNT(DISTINCT (adt.PAT_ENC_CSN_ID)) AS UNIQUE_PATIENTS
FROM CLARITY_ADT adt
LEFT JOIN ZC_EVENT_TYPE evt ON adt.EVENT_TYPE_C = evt.EVENT_TYPE_C
LEFT JOIN CLARITY_DEP dpt
INNER JOIN CLARITY_LOC loc ON loc.LOC_ID = dpt.REV_LOC_ID
LEFT JOIN ZC_ADT_UNIT_TYPE unt ON unt.ADT_UNIT_TYPE_C = dpt.ADT_UNIT_TYPE_C 
                               ON adt.DEPARTMENT_ID = dpt.DEPARTMENT_ID 
LEFT JOIN PATIENT pat ON adt.PAT_ID = pat.PAT_ID WHERE adt.PAT_ENC_CSN_ID IN (
		SELECT DISTINCT PAT_ENC_CSN_ID
		FROM CLARITY_ADT adt
		LEFT JOIN CLARITY_DEP dpt ON adt.DEPARTMENT_ID = dpt.DEPARTMENT_ID
		WHERE EVENT_TYPE_C IN (
				'1'
				,'2'
				,'3'
				,'4'
				,'6'
				)
			AND dpt.DEPARTMENT_NAME IN (",
				dept_char, ")
			AND PAT_ENC_CSN_ID IS NOT NULL
		)
GROUP BY dpt.DEPARTMENT_NAME
	,adt.DEPARTMENT_ID
	,loc.LOC_NAME
	,unt.NAME
	,dpt.IS_PERIOP_DEP_YN
	,dpt.LICENSED_BEDS
ORDER BY UNIQUE_PATIENTS DESC", con = conn)
  
  return (net_dept_query)
  
}