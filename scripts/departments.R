suppressMessages(library(tidyverse))
suppressMessages(library(dbplyr))
suppressMessages(library(DBI))
suppressMessages(library(RSQLite))
suppressMessages(library(SqlRender))

depts_sql <- function(depts, sql){
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  # inFile <- input$csvFile
  #
  # if (is.null(inFile))
  #     return(NULL)
  #
  # depts <-
  #     read.csv(inFile$datapath,
  #              stringsAsFactors = FALSE,
  #              fileEncoding = "UTF-8-BOM")
  
  
  icu_only <- depts %>%
    filter(departmentGroup != 'PERIOP') %>%
    pull(departmentID) %>%
    toString()
  
  icu_only <-
    paste0("adt.DEPARTMENT_ID IN (", icu_only, ")") %>% sql_vector(., con = conn)
  
  dpt_query <-
    paste(
      "-- Save to results folder as 2a_dept_adt.csv
SELECT adt.EVENT_ID
  ,adt.DEPARTMENT_ID
  ,dept.DEPARTMENT_NAME
  ,dept.SPECIALTY
  ,dept.INPATIENT_DEPT_YN
  ,zc_ut.NAME AS ADT_UNIT_TYPE
  ,loc.LOC_NAME
  ,zc_event_type.NAME AS EVENT_TYPE
  ,zc_evt_subtype.NAME AS EVENT_SUBTYPE
  ,zc_pat_class.NAME AS PAT_CLASS
  ,bed.BED_LABEL
  ,room.ROOM_NAME
  ,adt.EFFECTIVE_TIME
  ,adt.PAT_ENC_CSN_ID
  ,enc.HOSP_ADMSN_TIME
  ,enc.HOSP_DISCH_TIME
  ,DATEDIFF(DAY, pat.BIRTH_DATE, adt.EFFECTIVE_TIME)/365.25 AS AGE_YRS
FROM CLARITY_ADT adt
  LEFT JOIN CLARITY_DEP dept ON adt.DEPARTMENT_ID = dept.DEPARTMENT_ID
  LEFT JOIN ZC_EVENT_TYPE zc_event_type ON adt.EVENT_TYPE_C = zc_event_type.EVENT_TYPE_C
  LEFT JOIN ZC_EVENT_SUBTYPE zc_evt_subtype ON adt.EVENT_SUBTYPE_C = zc_evt_subtype.EVENT_SUBTYPE_C
  LEFT JOIN ZC_PAT_CLASS zc_pat_class ON adt.PAT_CLASS_C = zc_pat_class.ADT_PAT_CLASS_C
  LEFT JOIN CLARITY_BED bed ON bed.BED_CSN_ID = adt.BED_CSN_ID
  LEFT JOIN CLARITY_ROM room ON room.ROOM_CSN_ID = adt.ROOM_CSN_ID
  LEFT JOIN PAT_ENC_HSP enc ON adt.PAT_ENC_CSN_ID = enc.PAT_ENC_CSN_ID
  LEFT JOIN PATIENT pat ON enc.PAT_ID = pat.PAT_ID
  LEFT JOIN ZC_ADT_UNIT_TYPE zc_ut ON zc_ut.ADT_UNIT_TYPE_C = dept.ADT_UNIT_TYPE_C
  LEFT JOIN CLARITY_LOC loc ON dept.REV_LOC_ID = loc.LOC_ID
WHERE (",
      icu_only,
      ")
  -- Instead of selecting for 1 (Admit) and 3 (Transfer In), we ensure
  -- there are no others by specifically excluding the Epic-released
  -- values that are not applicable. Non-Epic-released values would remain.
  AND adt.EVENT_TYPE_C NOT IN (2, 4, 5, 6, 7, 8, 9, 10);"
    )
  
  if (grepl("Oracle", sql, ignore.case = T)) {
    return(dbplyr::build_sql(SqlRender::translate(dpt_query, targetDialect = "oracle"), con = conn) %>% str_sub(.,2,-2))

    
  } else if (grepl("MSSQL", sql, ignore.case = T)) {
    return(dbplyr::build_sql(dpt_query, con = conn) %>% str_sub(.,2,-2))
    
    
  } else {
    return('Please input valid SQL type')
  }
  
}