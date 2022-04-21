
################################################################# ---
##                 NEC 투개표 : 교육감 (2010)                  ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-04-21                       ##
################################################################# ---


# 1. [스크립트] 구시군 크롤링 -------------

library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)
library(rvest)

# electionName: 20140604 변수명만 다름

nec_url <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                       "electionId=0000000000",
                       "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0000000000%2Fvc%2Fvccp04.jsp",
                       "&topMenuId=VC",
                       "&secondMenuId=VCCP04",
                       "&menuId=VCCP04",
                       "&statementId=VCCP04_%231",
                       "&oldElectionType=1",
                       "&electionType=4",
                       "&electionName=20100602",
                       "&electionCode=11",
                       "&cityCode=1100",
                       "&sggCityCode=-1",
                       "&townCodeFromSgg=-1",
                       "&townCode=1101",
                       "&sggTownCode=-1",
                       "&x=96",
                       "&y=22")

nec_html <- rvest::read_html(nec_url)

Sys.setlocale("LC_ALL", "C")

nec_raw <- nec_html %>%
  html_nodes(".table01") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  .[[1]]

Sys.setlocale("LC_ALL", "Korean")

## * 변수명 ------------------------------

nec_hubo_varnames <- nec_raw %>%
  janitor::clean_names(ascii = FALSE) %>%
  select(contains("후보자별")) %>%
  slice(1) %>%
  as.character()

nec_no_hubo_varnames <- nec_raw %>%
  janitor::clean_names(ascii = FALSE) %>%
  select(!contains("후보자별")) %>%
  names(.)

nec_varnames <- c(nec_no_hubo_varnames, glue::glue("후보_{nec_hubo_varnames}"))

nec_varnames

## * 본체 ------------------------------

nec_body_raw <- nec_raw %>%
  janitor::clean_names(ascii = FALSE) %>%
  slice(2:n()) %>%
  relocate(contains("후보자별"), .after = 기권수)

## * 결합 ------------------------------

nec_tbl <- nec_body_raw %>%
  set_names(nec_varnames)

nec_tbl

# 2. [함수] 구시군 크롤링 -------------

get_eduction_raw_data <- function(cityCode = "1100", townCode="1101") {

  cat("\n---------------------------\n", cityCode, ":", townCode, "\n")


  nec_url <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                        "electionId=0000000000",
                        "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0000000000%2Fvc%2Fvccp04.jsp",
                        "&topMenuId=VC",
                        "&secondMenuId=VCCP04",
                        "&menuId=VCCP04",
                        "&statementId=VCCP04_%232",
                        "&oldElectionType=1",
                        "&electionType=4",
                        "&electionName=20100602",
                        "&electionCode=11",
                        "&cityCode={cityCode}",
                        "&sggCityCode=-1",
                        "&townCodeFromSgg=-1",
                        "&townCode={townCode}",
                        "&sggTownCode=-1",
                        "&x=96",
                        "&y=22")

  nec_html <- rvest::read_html(nec_url)

  Sys.setlocale("LC_ALL", "C")

  nec_raw <- nec_html %>%
    html_nodes(".table01") %>%
    html_table(fill = TRUE, header = TRUE) %>%
    .[[1]]

  Sys.setlocale("LC_ALL", "Korean")

  ## * 변수명 ------------------------------

  nec_hubo_varnames <- nec_raw %>%
    janitor::clean_names(ascii = FALSE) %>%
    select(contains("후보자별")) %>%
    slice(1) %>%
    as.character()

  nec_no_hubo_varnames <- nec_raw %>%
    janitor::clean_names(ascii = FALSE) %>%
    select(!contains("후보자별")) %>%
    names(.)

  nec_varnames <- c(nec_no_hubo_varnames, glue::glue("후보_{nec_hubo_varnames}"))

  ## * 본체 ------------------------------

  nec_body_raw <- nec_raw %>%
    janitor::clean_names(ascii = FALSE) %>%
    slice(2:n()) %>%
    relocate(contains("후보자별"), .after = 기권수)

  ## * 결합 ------------------------------

  nec_tbl <- nec_body_raw %>%
    set_names(nec_varnames)
    # mutate(구분 = ifelse(구분 == "", 읍면동명, 구분))

  return(nec_tbl)
}

get_eduction_raw_data("1100", "1102")

# 3. 마스터 데이터 크롤링 ----------------------

education_2010_raw <- krvote::nec_sgg_code %>%
  unnest(data) %>%
  mutate(data = map2(시도코드, 구시군코드, safely(get_eduction_raw_data, otherwise = "error")) )

# 4. 데이터 정제작업 ----------------------

education_2010_raw %>%
  mutate(result = map(data, "result")) %>%
  mutate(error = map_lgl(result, is.data.frame) ) %>%
  count(error)

education_2010 <- education_2010_raw %>%
  mutate(result = map(data, "result")) %>%
  select(시도명, 구시군, result) %>%
  mutate(data = map(result, ~ .x %>% pivot_longer( contains("후보"),
                                                   names_to = "후보",
                                                   values_to = "득표수"))) %>%
  select(-result) %>%
  unnest(data) %>%
  mutate(across(c(선거인수, 투표수, 무효투표수, 기권수, 득표수), parse_number))

education_2010 %>%
  filter(시도명 == "서울특별시", 읍면동명 == "합계") %>%
  group_by(후보) %>%
  summarise( 득표수 = sum(득표수) )


## 5. 내보내기 ---------------------

education_2010 <- krvote::clean_varnames(education_2010)

usethis::use_data(education_2010, overwrite = TRUE)

# sinew::makeOxygen(education_2010)


