
################################################################# ---
##             엑셀파일 유권자 투개표 : 지방선거/시도지사      ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-12-25                       ##
################################################################# ---


# 제8회 지방선거 - 시도지사 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)

##---------------------------------------------------------------- --
##                    제8회 지방선거 시도지사 (2022)               --
##---------------------------------------------------------------- --
# 출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=187608
# 1. 데이터 가져오기 ---------------------------
local_sido_2022_excel <- read_excel("Z:/dataset/선거데이터/01_대선_총선_지선_데이터/03_지방선거/제8회_지선_2022/시도지사선거.xlsx", sheet="시·도지사선거", skip=0,
                             col_types = c(rep("text", 15)))


local_sido_2022_raw <- local_sido_2022_excel %>%
  group_by(선거구명) %>%
  nest() %>%
  ungroup() %>%
  filter(!is.na(선거구명))


# 2. 시도별로 데이터 전처리 -----
## 2.1. 변수명 깔끔하게 하는 함수 -------
clean_colnames <- function(df) {

  # 티블 변수명 정리를 위한 임시 저장소
  local_colnames_tbl <- df %>%
    janitor::clean_names(ascii = FALSE)

  # 티블 변수명 전반부 (확정)
  local_colnames_first <- local_colnames_tbl %>%
    select(구시군명:투표수) %>%
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
    filter(읍면동명 != "계") %>%
    mutate(구분 = ifelse(is.na(구분), 읍면동명, 구분)) %>%
    mutate(across(선거인수:기권수, parse_number)) %>%
    filter(구분  != "소계",
           읍면동명 != "합계")

  return(clean_tbl)
}

local_sido_20220601 <- local_sido_2022_raw %>%
  mutate(data = map(data, clean_colnames) ) %>%
  mutate(data = map(data, clean_variable))

# 3. 단위테스트 검증 -------------

test_that("지방선거 2022 후보득표검증", {

  # 서울시장 후보 단위테스트 검정
  local_2022_seoul_check <- local_sido_20220601 %>%
    filter(`선거구명` == "서울특별시") %>%
    pull(data) %>% .[[1]] %>%
    summarise(`더불어민주당_송영길` = sum(`더불어민주당_송영길`),
              `국민의힘_오세훈`     = sum(`국민의힘_오세훈`),
              `정의당_권수정`       = sum(`정의당_권수정`),
              `기본소득당_신지혜`   = sum(`기본소득당_신지혜`),
              `무소속_김광종`       = sum(`무소속_김광종`))

  expect_that( local_2022_seoul_check$`더불어민주당_송영길`, equals(parse_number("1,733,183")))
  expect_that( local_2022_seoul_check$`국민의힘_오세훈`,   equals(parse_number("2,608,277")))
  expect_that( local_2022_seoul_check$`정의당_권수정`,   equals(parse_number("53,840")))
  expect_that( local_2022_seoul_check$`기본소득당_신지혜`,       equals(parse_number("12,619")))
  expect_that( local_2022_seoul_check$`무소속_김광종`,       equals(parse_number("9,000")))


  local_2022_jeju_check <- local_sido_20220601 %>%
    filter(`선거구명` == "제주특별자치도") %>%
    pull(data) %>% .[[1]] %>%
    summarise(`더불어민주당_오영훈` = sum(`더불어민주당_오영훈`),
              `국민의힘_허향진`   = sum(`국민의힘_허향진`),
              `녹색당_부순정`       = sum(`녹색당_부순정`),
              `무소속_박찬식`       = sum(`무소속_박찬식`))

  expect_that( local_2022_jeju_check$`더불어민주당_오영훈`, equals(parse_number("163,116")))
  expect_that( local_2022_jeju_check$`국민의힘_허향진`,   equals(parse_number("116,786")))
  expect_that( local_2022_jeju_check$`녹색당_부순정`,   equals(parse_number("5,750")))
  expect_that( local_2022_jeju_check$`무소속_박찬식`,       equals(parse_number("10,138")))
})

# 4. 내보내기 -------------

local_sido_20220601 <- clean_varnames(local_sido_20220601)

local_sido_20220601 <- local_sido_20220601 %>%
  mutate(data = map(data, clean_varnames))

usethis::use_data(local_sido_20220601, overwrite = TRUE)




