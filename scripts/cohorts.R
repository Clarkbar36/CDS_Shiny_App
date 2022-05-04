suppressMessages(library(tidyverse))
suppressMessages(library(dbplyr))
suppressMessages(library(DBI))
suppressMessages(library(RSQLite))
suppressMessages(library(SqlRender))

cohort_sql <- function(depts, sql){
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
    print('Something went wrong')
  }
  
  
  case_when_grouping <- depts %>%
    mutate(departmentID = paste0("'", departmentID, "'")) %>%
    group_by(departmentGroup) %>%
    summarise(ids = paste0(departmentID, collapse = ", ")) %>%
    mutate(query_string = ifelse(
      row_number() == 1,
      paste0("CASE WHEN dep.DEPARTMENT_ID IN (", ids, ") THEN '", departmentGroup, "' \n"),
      paste0("WHEN dep.DEPARTMENT_ID IN (", ids, ") THEN '", departmentGroup, "'\n")
    )) %>%
    pull(query_string) %>%
    sql_vector(., parens = FALSE, con = conn)
  
  
  if (grepl("Oracle", sql, ignore.case = T)) {
    final_cohort_where <- depts %>%
      filter(departmentGroup != 'PERIOP') %>%
      mutate(query_string = ifelse(
        row_number() != n(),
        paste0(
          "(DEPARTMENT_ID = ",
          departmentID,
          " and VISIT_IN_DT >= TO_DATE('",
          openDate,
          "', 'MM/DD/YYYY') AND OUT_DT < TO_DATE('",
          closeDate,
          "', 'MM/DD/YYYY')) /*",
          displayName,
          "*/ OR \n"
        ),
        paste0(
          "(DEPARTMENT_ID = ",
          departmentID,
          " and VISIT_IN_DT >= TO_DATE('",
          openDate,
          "', 'MM/DD/YYYY') AND OUT_DT < TO_DATE('",
          closeDate,
          "', 'MM/DD/YYYY')) /*",
          displayName,
          "*/"
        )
      )) %>%
      pull(query_string) %>%
      sql_vector(., con = conn)
    
  } else if (grepl("MSSQL", sql, ignore.case = T)) {
    final_cohort_where <- depts %>%
      filter(departmentGroup != 'PERIOP') %>%
      mutate(query_string = ifelse(
        row_number() != n(),
        paste0(
          "(DEPARTMENT_ID = ",
          departmentID,
          " and VISIT_IN_DT >= '",
          openDate,
          "' AND OUT_DT < '",
          closeDate,
          "') /*",
          displayName,
          "*/ OR \n"
        ),
        paste0(
          "(DEPARTMENT_ID = ",
          departmentID,
          " and VISIT_IN_DT >= '",
          openDate,
          "' AND OUT_DT < '",
          closeDate,
          "') /*",
          displayName,
          "*/"
        )
      )) %>%
      pull(query_string) %>%
      sql_vector(., con = conn)
    
  } else {
    print('It\'s wrong fool')
  }
  
  
  if (grepl("Oracle", sql, ignore.case = T)) {
    los_column <- ',((OUT_DT - ICU_IN_DT) * 1440) / 60 as ICU_LOS_HRS' %>%
      sql_vector(., con = conn)
    
  } else if (grepl("MSSQL", sql, ignore.case = T)) {
    los_column <- ',DATEDIFF(HOUR, VISIT_IN_DT, OUT_DT) AS ICU_LOS_HRS' %>%
      sql_vector(., con = conn)
    
  } else {
    print('Check Yo Self')
  }
  
  
  if (grepl("Oracle", sql, ignore.case = T)) {
    los_where <- 'AND ((OUT_DT - ICU_IN_DT) * 1440) / 60 >= .5' %>%
      sql_vector(., con = conn)
    
  } else if (grepl("MSSQL", sql, ignore.case = T)) {
    los_where <- 'AND DATEDIFF(MINUTE, VISIT_IN_DT, OUT_DT) / (60.) >= .5' %>%
      sql_vector(., con = conn)
    
  } else {
    print('Error 9045476')
  }
  
  

  
  cohort_query <- dbplyr::build_sql("WITH 
/**
 First we identify all of the Admission and Transfer In ADT events
 which will form the basis of the cohort. However, we have to weed out
 some ADT transfers, such as those to Periop and between rooms.
 */
adt_entry AS (
SELECT
  adt.EVENT_ID
  ,adt.PAT_ENC_CSN_ID
  ,adt.PAT_ID
  ,adt.EFFECTIVE_TIME
  ,adt.EVENT_TIME
  ,adt.SEQ_NUM_IN_ENC
  ,zc_et.NAME AS EVENT_TYPE
  ,zc_est.NAME AS EVENT_SUB_TYPE
  ,dep.DEPARTMENT_NAME
  ,dep.DEPARTMENT_ID
  ,room.ROOM_NAME
  ,zc_pc.NAME AS PAT_CLASS
  ,adt.XFER_IN_EVENT_ID
  ,adt.NEXT_OUT_EVENT_ID
  ,adt.LAST_IN_EVENT_ID
  ,adt.PREV_EVENT_ID
FROM CLARITY_ADT adt
  LEFT JOIN ZC_EVENT_TYPE zc_et ON zc_et.EVENT_TYPE_C = adt.EVENT_TYPE_C
  LEFT JOIN ZC_EVENT_SUBTYPE zc_est ON zc_est.EVENT_SUBTYPE_C = adt.EVENT_SUBTYPE_C
  LEFT JOIN CLARITY_DEP dep ON dep.DEPARTMENT_ID = adt.DEPARTMENT_ID
  LEFT JOIN ED_ROOM_INFO room ON room.ROOM_ID = adt.ROOM_ID
  LEFT JOIN ZC_PAT_CLASS zc_pc ON zc_pc.ADT_PAT_CLASS_C = adt.PAT_CLASS_C
WHERE", icu_departments_filter,"
  AND zc_et.NAME IN ('Admission', 'Transfer In')
  AND zc_est.NAME NOT IN ('Canceled') -- Yes, it's spelled incorrectly
),
/**
 Get all of the ADT rows for these CSNs
 */
adt_all_rows AS (
SELECT DISTINCT
  adt.EVENT_ID
  ,adt.PAT_ENC_CSN_ID
  ,adt.PAT_ID
  ,pat.PAT_MRN_ID
  ,pat.BIRTH_DATE
  ,adt.EFFECTIVE_TIME
  ,adt.EVENT_TIME
  ,adt.SEQ_NUM_IN_ENC
  ,zc_et.NAME AS EVENT_TYPE
  ,CASE WHEN zc_et.NAME IN ('Admission', 'Transfer In') THEN 'In'
        WHEN zc_et.NAME IN ('Transfer Out', 'Discharge') THEN 'Out'
		END AS EVENT_DIR
  ,zc_est.NAME AS EVENT_SUB_TYPE
  ,dep.DEPARTMENT_NAME
  ,dep.DEPARTMENT_ID
  -- Add Department Groupers to the DEPT_GRP variable
  ,",case_when_grouping," ELSE 'OTHER'
	END AS DEPT_GRP
  ,room.ROOM_NAME
FROM adt_entry
  LEFT JOIN CLARITY_ADT adt ON adt_entry.PAT_ENC_CSN_ID = adt.PAT_ENC_CSN_ID
  LEFT JOIN PATIENT pat ON adt.PAT_ID = pat.PAT_ID
  LEFT JOIN ZC_EVENT_TYPE zc_et ON zc_et.EVENT_TYPE_C = adt.EVENT_TYPE_C
  LEFT JOIN ZC_EVENT_SUBTYPE zc_est ON zc_est.EVENT_SUBTYPE_C = adt.EVENT_SUBTYPE_C
  LEFT JOIN CLARITY_DEP dep ON dep.DEPARTMENT_ID = adt.DEPARTMENT_ID
  LEFT JOIN ED_ROOM_INFO room ON room.ROOM_ID = adt.ROOM_ID
WHERE
  zc_et.NAME IN ('Admission', 'Transfer In', 'Transfer Out', 'Discharge')
  AND zc_est.NAME NOT IN ('Canceled')
),
/**
 Remove the periop rows by looking for rows that have an exit from PERIOP
 */
adt_remove_periop AS (
SELECT
  *
  ,FIRST_VALUE(EFFECTIVE_TIME) OVER (PARTITION BY PAT_ENC_CSN_ID ORDER BY SEQ_NUM_IN_ENC) AS HOSP_ADMIT_DT
  ,FIRST_VALUE(EFFECTIVE_TIME) OVER (PARTITION BY PAT_ENC_CSN_ID ORDER BY SEQ_NUM_IN_ENC DESC) AS HOSP_DC_DT
  ,CASE WHEN (LAG(DEPT_GRP) OVER (PARTITION BY PAT_ENC_CSN_ID ORDER BY SEQ_NUM_IN_ENC, EVENT_DIR) IN ('PERIOP') AND
                DEPT_GRP IN ('PERIOP') AND
                LAG(EVENT_TYPE) OVER (PARTITION BY PAT_ENC_CSN_ID ORDER BY SEQ_NUM_IN_ENC, EVENT_DIR) IN ('Admission', 'Transfer In') AND
                EVENT_TYPE IN ('Discharge', 'Transfer Out')
			 ) OR
             (LEAD(DEPT_GRP) OVER (PARTITION BY PAT_ENC_CSN_ID ORDER BY SEQ_NUM_IN_ENC, EVENT_DIR) IN ('PERIOP') AND
                DEPT_GRP IN ('PERIOP') AND
                LEAD(EVENT_TYPE) OVER (PARTITION BY PAT_ENC_CSN_ID ORDER BY SEQ_NUM_IN_ENC, EVENT_DIR) IN ('Transfer Out', 'Discharge') AND
                EVENT_TYPE IN ('Admission', 'Transfer In')
			 ) THEN 'YES'
        END AS OUT_PERIOP
FROM adt_all_rows
),
/**
 Identify when the last ADT OUT is from the same DEPT_GRP as the current ADT IN
 */
adt_same_dept AS (
SELECT
  *
  ,CASE WHEN LAG(DEPT_GRP) OVER (PARTITION BY PAT_ENC_CSN_ID ORDER BY SEQ_NUM_IN_ENC, EVENT_DIR) IN (DEPT_GRP) THEN 'YES'
        END AS SAME_DEPT
FROM adt_remove_periop
WHERE OUT_PERIOP IS NULL
),
/**
 Find the OUT of this entry to get the end date of the ICU Epoch
 */
adt_visit_dt_set AS (
SELECT
  *
  ,CASE WHEN SAME_DEPT IS NULL THEN EFFECTIVE_TIME
        END AS VISIT_IN_DT
  ,CASE WHEN LEAD(SAME_DEPT) OVER (PARTITION BY PAT_ENC_CSN_ID ORDER BY SEQ_NUM_IN_ENC, EVENT_DIR) IS NULL THEN EFFECTIVE_TIME
        END AS VISIT_OUT_DT
FROM adt_same_dept
WHERE DEPT_GRP NOT IN ('PERIOP', 'OTHER')
),
/**
 Define the final cohort
 */
adt_final_cohort AS (
SELECT DISTINCT
  *
  ,CASE WHEN VISIT_OUT_DT IS NULL THEN LEAD(VISIT_OUT_DT) OVER (PARTITION BY PAT_ENC_CSN_ID ORDER BY SEQ_NUM_IN_ENC, EVENT_DIR)
        END AS OUT_DT
  ,YEAR(VISIT_IN_DT) AS IN_YEAR
FROM adt_visit_dt_set
WHERE VISIT_IN_DT IS NOT NULL OR VISIT_OUT_DT IS NOT NULL
),
final_cohort AS (
/**
 Now we select the final set of rows
 */
SELECT
  EVENT_ID
  ,PAT_ENC_CSN_ID
  ,PAT_ID
  ,PAT_MRN_ID
  ,BIRTH_DATE
  ,EFFECTIVE_TIME
  ,EVENT_TIME
  ,EVENT_TYPE
  ,DEPARTMENT_ID
  ,DEPARTMENT_NAME
  ,DEPT_GRP
  ,ROOM_NAME AS FIRST_ROOM_NAME
  ,HOSP_ADMIT_DT
  ,HOSP_DC_DT
  ,VISIT_IN_DT AS ICU_IN_DT
  ,OUT_DT AS ICU_OUT_DT
  ,IN_YEAR
  ",
  los_column,"
FROM adt_final_cohort
WHERE VISIT_IN_DT IS NOT NULL AND OUT_DT IS NOT NULL
",
los_where, "
AND ",
final_cohort_where,"
)
SELECT * 
FROM final_cohort
ORDER BY ICU_IN_DT, PAT_ENC_CSN_ID;", con = conn) %>% as.character()
  
return(cohort_query)
  
}