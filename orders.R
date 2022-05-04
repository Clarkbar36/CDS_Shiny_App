suppressMessages(library(tidyverse))
suppressMessages(library(dbplyr))
suppressMessages(library(DBI))
suppressMessages(library(RSQLite))
suppressMessages(library(SqlRender))

orders_sql <- function(depts, sql){
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  
  if (grepl("Oracle", sql, ignore.case = T)) {
    icu_departments_filter <- depts %>%
      filter(departmentGroup != 'PERIOP') %>%
      mutate(query_string = ifelse(
        row_number() != n(),
        paste0(
          "(adt.DEPARTMENT_ID = ",
          departmentID,
          " and adt.EFFECTIVE_TIME BETWEEN TO_DATE('",
          openDate,
          "', 'MM/DD/YYYY') AND TO_DATE('",
          closeDate,
          "', 'MM/DD/YYYY')) /*",
          displayName,
          "*/ OR \n"
        ),
        paste0(
          "(adt.DEPARTMENT_ID = ",
          departmentID,
          " and adt.EFFECTIVE_TIME BETWEEN TO_DATE('",
          openDate,
          "', 'MM/DD/YYYY') AND TO_DATE('",
          closeDate,
          "', 'MM/DD/YYYY')) /*",
          displayName,
          "*/"
        )
      )) %>%
      pull(query_string) %>%
      sql_vector(., con = conn)
    
  } else if (grepl("MSSQL", sql, ignore.case = T)) {
    icu_departments_filter <- depts %>%
      filter(departmentGroup != 'PERIOP') %>%
      mutate(query_string = ifelse(
        row_number() != n(),
        paste0(
          "(adt.DEPARTMENT_ID = ",
          departmentID,
          " and adt.EFFECTIVE_TIME BETWEEN '",
          openDate,
          "' AND '",
          closeDate,
          "') /*",
          displayName,
          "*/ OR \n"
        ),
        paste0(
          "(adt.DEPARTMENT_ID = ",
          departmentID,
          " and adt.EFFECTIVE_TIME BETWEEN '",
          openDate,
          "' AND '",
          closeDate,
          "') /*",
          displayName,
          "*/"
        )
      )) %>%
      pull(query_string) %>%
      sql_vector(., con = conn)
    
  } else {
    print('Please check notes_sql.R')
  }
  
  
  icu_base_cte <- dbplyr::build_sql("
WITH
all_encounters AS (
SELECT DISTINCT
  adt.PAT_ENC_CSN_ID
FROM CLARITY_ADT adt
WHERE (",
                                    icu_departments_filter,
                                    ")
  AND adt.EVENT_TYPE_C NOT IN (2, 4, 5, 6, 7, 8, 9, 10)
  AND adt.EVENT_SUBTYPE_C NOT IN (2)),",
                                    con = conn
  )
  
  start_date <- depts %>%
    mutate(openDate = as.POSIXct(openDate, format = '%m/%d/%Y')) %>%
    summarize(openDate = as.character(min(openDate))) %>%
    pull(openDate)
  
  end_date <- depts %>%
    mutate(closeDate = as.POSIXct(closeDate, format = '%m/%d/%Y')) %>%
    summarize(closeDate = as.character(max(closeDate))) %>%
    pull(closeDate)
  
  if (grepl("Oracle", sql, ignore.case = T)) {
    where_dates <- paste0(
      " BETWEEN TO_DATE('",
      start_date,
      "', 'YYYY-MM-DD') AND TO_DATE('",
      end_date,
      "', 'YYYY-MM-DD'))"
    ) %>%
      sql_vector(., con = conn)
    
  } else if (grepl("MSSQL", sql, ignore.case = T)) {
    where_dates <-
      paste0(" BETWEEN '", start_date, "' AND '", end_date, "')") %>%
      sql_vector(., con = conn)
    
  } else {
    print('Please check orders.R, starting at line 85')
    
  }
  
  orders_query <- dbplyr::build_sql("-- Save to results folder as 2c_order_types.csv",
                                    icu_base_cte," 
ords AS (
SELECT 
  om.ORDER_ID 
  ,om.ORDER_MODE
  ,om.DISCONTINUE_MODE
  ,zc_ord_src.NAME AS ORDER_SOURCE
  ,om.ORDER_DTTM
  ,om.ACKNOWLEDGE_DTTM
  ,om.ORDER_DESC
  ,om.DISPLAY_NAME
  ,zc_ord_stat.NAME AS ORDER_STATUS
  ,om.PAT_ENC_CSN_ID
  ,zc_act_ord.NAME AS ACTIVE_ORDER
  ,zc_ord_type.NAME AS ORDER_TYPE
FROM all_encounters all_enc
  LEFT JOIN ORDER_METRICS om ON om.PAT_ENC_CSN_ID = all_enc.PAT_ENC_CSN_ID
  LEFT JOIN ZC_ORDER_SOURCE zc_ord_src ON zc_ord_src.ORDER_SOURCE_C = om.ORDER_SOURCE_C
  LEFT JOIN ZC_ORDER_STATUS zc_ord_stat ON zc_ord_stat.ORDER_STATUS_C = om.ORDER_STATUS_C
  LEFT JOIN ZC_ACTIVE_ORDER zc_act_ord ON zc_act_ord.ACTIVE_ORDER_C = om.ACTIVE_ORDER_C
  LEFT JOIN ZC_ORDER_TYPE zc_ord_type ON zc_ord_type.ORDER_TYPE_C = om.ORDER_TYPE_C
  WHERE ORDER_DTTM", where_dates," 
  
SELECT * FROM ords;", con = conn) %>% as.character()
  
  return (orders_query)
  
}