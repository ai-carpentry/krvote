
################################################################# ---
##             엑셀파일 유권자 투개표 : 지방선거/교육감        ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-03-13                       ##
################################################################# ---


# 제7회 지방선거 - 교육감 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)


##---------------------------------------------------------------- --
##                    제7회 지방선거 교육감 (2018)               --
##---------------------------------------------------------------- --
# 출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=14979
# 1. 데이터 가져오기 ---------------------------
local_edu_2018_excel <- read_excel("../../docs/krvotes/data-raw/제7회_전국동시지방선거_2018/20180619-7지선-07-(교육감)_읍면동별개표자료.xlsx", sheet="7지선-교육감", skip=0,
                             col_types = c(rep("text", 18)))


local_edu_2018_raw <- local_edu_2018_excel %>%
  group_by(선거구명) %>%
  nest() %>%
  ungroup()


# 2. 시도별로 데이터 전처리 -----
## 2.1. 변수명 깔끔하게 하는 함수 -------
clean_colnames <- function(df) {

  # 티블 변수명 정리를 위한 임시 저장소
  local_colnames_tbl <- df %>%
    janitor::clean_names(ascii = FALSE)

  # 티블 변수명 전반부 (확정)
  local_colnames_first <- local_colnames_tbl %>%
    select(선거종류:투표수) %>%
    colnames() %>%
    dput()

  # 티블 변수명 후반부 (첫번째 행에서 가져옮)
  local_colnames_second <- local_colnames_tbl %>%
    slice(1) %>%
    select(후보자별_득표수:기권수) %>%
    unlist %>% as.vector(.) %>%
    dput()

  local_colnames_second[length(local_colnames_second)-1] <- "무효투표수"
  local_colnames_second[length(local_colnames_second)] <- "기권수"

  # 변수명 결합
  local_colnames_v <- c(local_colnames_first, local_colnames_second)

  clean_tbl <- local_colnames_tbl %>%
    set_names(local_colnames_v) %>%
    janitor::clean_names(ascii = FALSE) %>%
    slice(2:n()) %>%
    select(!starts_with("x"))

  return(clean_tbl)
}

## 2.2. 시도별 변수명 작업 수행 -------

clean_variable <- function(raw_data) {
  clean_tbl <- raw_data %>%
    select(-선거종류) %>%
    filter(!is.na(구시군명),
           읍면동명 != "계") %>%
    mutate(구분 = ifelse(is.na(구분), 읍면동명, 구분)) %>%
    mutate_at(vars(선거인수:기권수), as.numeric) %>%
    filter(구분 != "소계")

  return(clean_tbl)
}

local_edu_20180613 <- local_edu_2018_raw %>%
  mutate(data = map(data, clean_colnames) ) %>%
  mutate(data = map(data, clean_variable)) %>%
  rename(시도명 = 선거구명)

# 3. 단위테스트 검증 -------------

local_edu_20180613$data[[1]]

test_that("지방선거 2018 후보득표검증", {

  # 서울시장 후보 단위테스트 검정
  local_edu_20180613_check <- local_edu_20180613 %>%
    filter(`시도명` == "서울특별시") %>%
    pull(data) %>% .[[1]] %>%
    summarise(조희연 = sum(조희연),
              조영달 = sum(조영달),
              박선영 = sum(박선영))

  expect_that( local_edu_20180613_check$조희연, equals(parse_number("2,271,413")) )
  expect_that( local_edu_20180613_check$조영달, equals(parse_number("841,599")) )
  expect_that( local_edu_20180613_check$박선영, equals(parse_number("1,762,658")) )
})

# 4. 내보내기 -------------

local_edu_20180613 <- krvote::clean_varnames(local_edu_20180613)

local_edu_20180613 <- local_edu_20180613 %>%
  mutate(data = map(data, krvote::clean_varnames))

usethis::use_data(local_edu_20180613, overwrite = TRUE)


##---------------------------------------------------------------- --
##                    제6회 지방선거 교육감 (2014)               --
##---------------------------------------------------------------- --
# 출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=14979

# 제6회 지방선거 - 교육감 -------------

# 1. 서울교육감 ---------------------------

## 1.1.함수 ----------------------------

local_educ_excel_sheets <- readxl::excel_sheets("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(서울)/07_교육감/서울특별시.xls")


turn_excel_into_dataframe <- function(sheet_name = sheet_name) {

  cat("\n-----------------------------\n", sheet_name, "\n")

  local_edu_2014_excel <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(서울)/07_교육감/서울특별시.xls",
                                     sheet = sheet_name, skip = 3,
                                     col_types = c(rep("text", 11)))

  candidate_names <- local_edu_2014_excel %>%
    janitor::clean_names(ascii = FALSE) %>%
    select(!starts_with("x")) %>% names(.)

  local_edu_2014_raw <- local_edu_2014_excel %>%
    janitor::clean_names(ascii = FALSE) %>%
    set_names(c("읍면동명", "구분", "선거인수", "투표수", candidate_names, "무효_투표수", "기권수")) %>%
    mutate(구분 = ifelse(is.na(구분), 읍면동명, 구분)) %>%
    mutate(across(선거인수:기권수, parse_number) )

  local_edu_2014_raw
}

turn_excel_into_dataframe(local_educ_excel_sheets[1])

local_edu_2014_raw <- tibble(시군구명 = local_educ_excel_sheets) %>%
  mutate(data = map( 시군구명, turn_excel_into_dataframe))

local_edu_seoul_20140604 <- local_edu_2014_raw  %>%
  mutate(시군구명 = str_remove(시군구명, "[0-9]+"))

## 1.2. 내보내기 ---------------------

local_edu_seoul_20140604 <- krvote::clean_varnames(local_edu_seoul_20140604)

local_edu_seoul_20140604 <- local_edu_seoul_20140604 %>%
  mutate(data = map(data, krvote::clean_varnames))

usethis::use_data(local_edu_seoul_20140604, overwrite = TRUE)

# 2. 전체 : 경우의 수 너무 많음 X ---------------------------
## 2.1. 엑셀 파일 찾기 --------------
local_edu_dir_filename <- list.files("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014")

filepath_tbl <- tibble(dirname = local_edu_dir_name) %>%
  mutate(data = map(dirname, list.files, full.name = TRUE)) %>%
  select(data) %>%
  unnest(data) %>%
  filter(str_detect(data, "교육감.*\\.xlsx?")) %>%
  rename(filepath = data)

## 2.2. 함수 수정 ------------------
## 구시군 엑셀쉬트 정보 추출

turn_excel_sheet_into_dataframe <- function(excel_name, sheet_name = sheet_name) {

  cat("\n-----------------------------\n", excel_name, ":", sheet_name, "\n")

  local_edu_2014_excel <- read_excel(excel_name,
                                     sheet = sheet_name, skip = 3)

  candidate_names <- local_edu_2014_excel %>%
    janitor::clean_names(ascii = FALSE) %>%
    select(!starts_with("x")) %>% names(.)

  local_edu_2014_raw <- local_edu_2014_excel %>%
    janitor::clean_names(ascii = FALSE) %>%
    set_names(c("읍면동명", "구분", "선거인수", "투표수", candidate_names, "무효_투표수", "기권수")) %>%
    mutate(구분 = ifelse(is.na(구분), 읍면동명, 구분)) %>%
    mutate(across(선거인수:기권수, parse_number) )

  local_edu_2014_raw
}



local_edu_2014_raw <- filepath_tbl %>%
  # filter(str_detect(filepath, "서울")) %>%
  mutate(sheetname = map(filepath, readxl::excel_sheets)) %>%
  unnest(sheetname) %>%
  mutate(data = map2(filepath, sheetname, safely(turn_excel_sheet_into_dataframe, otherwise = "error")) )

local_edu_2014_ok <- local_edu_2014_raw %>%
  mutate(시도 = str_extract(filepath, "\\((.*?)\\)")) %>%
  select(시도, sheetname, data) %>%
  mutate(result = map(data, "result")) %>%
  mutate(error = map_lgl(result, is.data.frame)) %>%
  filter(error) %>%
  select(시도, sheetname, result)

local_edu_2014_ok %>%
  count(시도)

# 3. 경기 ---------------------------

gg_filepath <- filepath_tbl %>%
  filter(str_detect(filepath, "경기")) %>%
  pull()

local_edu_gg_20140604_raw <- read_excel(gg_filepath, skip = 3)

candiate_gg_colname <- local_edu_gg_20140604_raw %>%
  janitor::clean_names(ascii = FALSE) %>%
  select(!starts_with("x")) %>%
  names(.)

local_edu_gg_20140604 <- local_edu_gg_20140604_raw %>%
  set_names(c("위원회명", "읍면동명", "구분", "선거인수", "투표수", candiate_gg_colname, "무효_투표수", "기권수")) %>%
  mutate(구분 = ifelse(is.na(구분), 읍면동명, 구분)) %>%
  mutate(across(선거인수:기권수, parse_number))

## 4.1. 내보내기 -------------

local_edu_gg_20140604 <- krvote::clean_varnames(local_edu_gg_20140604)

usethis::use_data(local_edu_gg_20140604, overwrite = TRUE)

# 4. 중간 결과 --------------

local_edu_20140604 <- list(seoul = local_edu_seoul_20140604,
                           gg    = local_edu_gg_20140604)

usethis::use_data(local_edu_20140604, overwrite = TRUE)


