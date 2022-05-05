suppressMessages(library(tidyverse))
suppressMessages(library(dbplyr))
suppressMessages(library(DBI))
suppressMessages(library(RSQLite))
suppressMessages(library(SqlRender))

att_provs_sql <- function(depts, sql){
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
                                    con = conn)

  providers_query <- dbplyr::build_sql("-- Save to results folder as 2d_attnd_prov.csv",
                                       icu_base_cte,"provs AS (
	SELECT
		 hsp.PROV_ID
		,ser.PROV_TYPE
		,ser.PROV_NAME
		,ser.USER_ID
		,SUM(DATEDIFF(day, hsp.ATTEND_FROM_DATE, hsp.ATTEND_TO_DATE)) AS ATTEND_LOS_DAY_TOTAL
		,AVG(DATEDIFF(day, hsp.ATTEND_FROM_DATE, hsp.ATTEND_TO_DATE)) AS ATTEND_LOS_DAY_AVG
		,COUNT(hsp.PAT_ENC_CSN_ID) as TOTAL_ENCOUNTERS
		,COUNT(DISTINCT(hsp.PAT_ENC_CSN_ID)) as TOTAL_UNIQUE_ENCOUNTERS
		,MIN(zc_ser_6.NAME) AS RPT_GRP_SIX
		,MIN(zc_ser_7.NAME) AS RPT_GRP_SEVEN
		,MIN(zc_ser_8.NAME) AS RPT_GRP_EIGHT
		,MIN(zc_ser_9.NAME) AS RPT_GRP_NINE
		,MIN(zc_ser_10.NAME) AS RPT_GRP_TEN
		,MIN(zc_ser_11.NAME) AS RPT_GRP_ELEVEN
		,MIN(zc_ser_12.NAME) AS RPT_GRP_TWELVE
		,MIN(zc_ser_13.NAME) AS RPT_GRP_THIRTEEN
		,MIN(zc_ser_14.NAME) AS RPT_GRP_FOURTEEN
		,MIN(zc_ser_15.NAME) AS RPT_GRP_FIFTEEN
		,MIN(zc_ser_16.NAME) AS RPT_GRP_SIXTEEN
		,MIN(zc_ser_17.NAME) AS RPT_GRP_SEVENTEEN
		,MIN(zc_ser_18.NAME) AS RPT_GRP_EIGHTEEN
		,MIN(zc_ser_19.NAME) AS RPT_GRP_NINTEEN
		,MIN(zc_ser_20.NAME) AS RPT_GRP_TWENTY
		,MIN(primary_dep.DEPARTMENT_NAME) AS PRIMARY_DEPT_NAME
	FROM all_encounters all_enc
	LEFT JOIN HSP_ATND_PROV hsp ON hsp.PAT_ENC_CSN_ID = all_enc.PAT_ENC_CSN_ID
	LEFT JOIN Clarity_SER ser ON hsp.PROV_ID = ser.PROV_ID
	LEFT JOIN Clarity_SER_2 ser_2 ON hsp.PROV_ID = ser_2.PROV_ID
	LEFT JOIN ZC_SER_RPT_GRP_6 zc_ser_6 ON zc_ser_6.RPT_GRP_SIX = ser.RPT_GRP_SIX
	LEFT JOIN ZC_SER_RPT_GRP_7 zc_ser_7 ON zc_ser_7.RPT_GRP_SEVEN = ser.RPT_GRP_SEVEN
	LEFT JOIN ZC_SER_RPT_GRP_8 zc_ser_8 ON zc_ser_8.RPT_GRP_EIGHT = ser.RPT_GRP_EIGHT
	LEFT JOIN ZC_SER_RPT_GRP_9 zc_ser_9 ON zc_ser_9.RPT_GRP_NINE = ser.RPT_GRP_NINE
	LEFT JOIN ZC_SER_RPT_GRP_10 zc_ser_10 ON zc_ser_10.RPT_GRP_TEN = ser.RPT_GRP_TEN
	LEFT JOIN ZC_SER_RPT_GRP_11 zc_ser_11 ON zc_ser_11.RPT_GRP_ELEVEN_C = ser.RPT_GRP_ELEVEN_C
	LEFT JOIN ZC_SER_RPT_GRP_12 zc_ser_12 ON zc_ser_12.RPT_GRP_TWELVE_C = ser.RPT_GRP_TWELVE_C
	LEFT JOIN ZC_SER_RPT_GRP_13 zc_ser_13 ON zc_ser_13.RPT_GRP_THIRTEEN_C = ser.RPT_GRP_THIRTEEN_C
	LEFT JOIN ZC_SER_RPT_GRP_14 zc_ser_14 ON zc_ser_14.RPT_GRP_FOURTEEN_C = ser.RPT_GRP_FOURTEEN_C
	LEFT JOIN ZC_SER_RPT_GRP_15 zc_ser_15 ON zc_ser_15.RPT_GRP_FIFTEEN_C = ser.RPT_GRP_FIFTEEN_C
	LEFT JOIN ZC_SER_RPT_GRP_16 zc_ser_16 ON zc_ser_16.RPT_GRP_SIXTEEN_C = ser.RPT_GRP_SIXTEEN_C
	LEFT JOIN ZC_SER_RPT_GRP_17 zc_ser_17 ON zc_ser_17.RPT_GRP_SEVNTEEN_C = ser.RPT_GRP_SEVNTEEN_C
	LEFT JOIN ZC_SER_RPT_GRP_18 zc_ser_18 ON zc_ser_18.RPT_GRP_EIGHTEEN_C = ser.RPT_GRP_EIGHTEEN_C
	LEFT JOIN ZC_SER_RPT_GRP_19 zc_ser_19 ON zc_ser_19.RPT_GRP_NINETEEN_C = ser.RPT_GRP_NINETEEN_C
	LEFT JOIN ZC_SER_RPT_GRP_20 zc_ser_20 ON zc_ser_20.RPT_GRP_TWENTY_C = ser.RPT_GRP_TWENTY_C
	LEFT JOIN CLARITY_DEP primary_dep ON primary_dep.DEPARTMENT_ID = ser_2.PRIMARY_departmentID
	LEFT JOIN PAT_ENC_HSP enc ON enc.PAT_ENC_CSN_ID = hsp.PAT_ENC_CSN_ID
	WHERE hsp.ATTEND_FROM_DATE < enc.HOSP_DISCH_TIME
		and all_enc.ICU_IN_DT >= hsp.ATTEND_FROM_DATE
		and all_enc.ICU_OUT_DT <= hsp.ATTEND_TO_DATE
	GROUP BY hsp.PROV_ID
		,ser.PROV_TYPE
		,ser.PROV_NAME
		,ser.USER_ID
	)
SELECT *
FROM provs
ORDER BY ATTEND_LOS_DAY_TOTAL DESC;", con = conn)

  if (grepl("Oracle", sql, ignore.case = T)) {
    return(build_sql(translate(providers_query,targetDialect = "oracle"),con = conn) %>% str_sub(.,2,-2))

  } else if (grepl("MSSQL", sql, ignore.case = T)) {
   return(build_sql(providers_query, con = conn) %>% str_sub(.,2,-2))

  } else {
    print('Please check provs.R')
  }

}
