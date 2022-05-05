suppressMessages(library(tidyverse))
suppressMessages(library(dbplyr))
suppressMessages(library(DBI))
suppressMessages(library(RSQLite))
suppressMessages(library(SqlRender))


notes_sql <- function(depts, sql){
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
  
  notes_query <-
    dbplyr::build_sql("-- Save to results folder as 2b_note_types.csv",
                      icu_base_cte,
                      "
note_info AS (SELECT
  hno.NOTE_ID
  ,hno.PAT_ENC_CSN_ID
  ,zc_nt_ip.NAME AS IP_NOTE_TYPE
  ,hno.ENTRY_DATETIME
  ,hno.CREATE_INSTANT_DTTM
  ,v_note_char.COPIED_SUM
  ,v_note_char.AUTHOR_SUM
  ,v_note_char.TOTAL_NOTE_LENGTH
  ,hno_2.STARTING_SMARTLINK_ID
  ,hno_2.STARTING_SMARTPHRASE_ID
  ,hno_2.STARTING_SMARTTEXT_ID
  ,v_note_char.NOTE_FILE_DTTM
  ,zc_note_status.NAME AS NOTE_STATUS
  ,v_note_char.DATE_OF_SERVICE_DTTM
  ,v_note_char.CURRENT_AUTHOR_ID
  ,v_note_char.AUTHOR_LINKED_PROV_ID
  ,zc_prov_type.NAME AS AUTHOR_PROV_TYPE
  ,zc_spec.NAME AS AUTHOR_SPECIALTY
  ,primary_dep.DEPARTMENT_NAME
FROM all_encounters all_enc
  LEFT JOIN HNO_INFO hno ON hno.PAT_ENC_CSN_ID = all_enc.PAT_ENC_CSN_ID
  LEFT JOIN HNO_INFO_2 hno_2 ON hno_2.NOTE_ID = hno.NOTE_ID
  LEFT JOIN ZC_NOTE_TYPE_IP zc_nt_ip ON hno.IP_NOTE_TYPE_C = zc_nt_ip.TYPE_IP_C
  LEFT JOIN V_NOTE_CHARACTERISTICS v_note_char ON hno.NOTE_ID = v_note_char.NOTE_ID
  LEFT JOIN ZC_NOTE_STATUS zc_note_status ON zc_note_status.NOTE_STATUS_C = v_note_char.NOTE_STATUS_C
  LEFT JOIN ZC_PROV_TYPE zc_prov_type ON zc_prov_type.PROV_TYPE_C = v_note_char.AUTHOR_PROV_TYPE_C
  LEFT JOIN ZC_SPECIALTY zc_spec ON zc_spec.SPECIALTY_C = v_note_char.AUTHOR_SPECIALTY_C
  LEFT JOIN Clarity_SER_2 ser_2 ON v_note_char.AUTHOR_LINKED_PROV_ID = ser_2.PROV_ID
  LEFT JOIN CLARITY_DEP primary_dep ON primary_dep.DEPARTMENT_ID = ser_2.PRIMARY_departmentID
  WHERE CREATE_INSTANT_DTTM IS NOT NULL)

  SELECT * FROM note_info;",
                      con = conn
    ) %>% as.character()
  
  return(notes_query)
}