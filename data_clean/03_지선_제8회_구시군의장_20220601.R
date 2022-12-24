
################################################################# ---
##             엑셀파일 유권자 투개표 : 지방선거/구시군의장    ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-12-25                       ##
################################################################# ---


# 제8회 지방선거 - 구시군의장 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)


##---------------------------------------------------------------- --
##                    제8회 지방선거 구시군의장 (2022)             --
##---------------------------------------------------------------- --
# 출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=187608
# 1. 데이터 가져오기 ---------------------------
local_sgg_excel <- read_excel("Z:/dataset/선거데이터/01_대선_총선_지선_데이터/03_지방선거/제8회_지선_2022/구시군장선거.xlsx", sheet="구·시·군의장선거", skip=0,
                             col_types = c(rep("text", 15)))


local_sgg_raw <- local_sgg_excel %>%
  group_by(시도명, 구시군명, `선거구(구시군)`) %>%
  nest() %>%
  ungroup() %>%
  filter(!is.na(시도명))

local_sgg_raw %>%
  count(시도명)

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

local_sgg_20180613 <- local_sgg_raw %>%
  mutate(data = map(data, clean_colnames) ) %>%
  mutate(data = map(data, clean_variable))


# 3. 단위테스트 검증 -------------

test_that("지방선거 구시군의장 2018 후보득표검증", {

  # 서울 종로구청장 후보 단위테스트 검정
  local_sgg_20180613_check <- local_sgg_20180613 %>%
    filter(시도 == "서울특별시" & 선거구명 == "종로구") %>%
    pull(data) %>% .[[1]] %>%
    summarise(`더불어민주당_김영종` = sum(`더불어민주당_김영종`),
              `자유한국당_이숙연`   = sum(`자유한국당_이숙연`),
              `바른미래당_김복동`   = sum(`바른미래당_김복동`))

  expect_that( local_sgg_20180613_check$`더불어민주당_김영종`, equals(51305))
  expect_that( local_sgg_20180613_check$`자유한국당_이숙연`,   equals(19628))
  expect_that( local_sgg_20180613_check$`바른미래당_김복동`,   equals(8765))
})

test_that("지방선거 구시군의장 2018 후보득표검증", {

  # 경상북도 영덕군수 후보 단위테스트 검정
  local_sgg_20180613_check <- local_sgg_20180613 %>%
    filter(시도 == "경상북도" & 선거구명 == "영덕군") %>%
    pull(data) %>% .[[1]] %>%
    summarise(`더불어민주당_장성욱` = sum(`더불어민주당_장성욱`),
              `자유한국당_이희진`   = sum(`자유한국당_이희진`),
              `무소속_박병일`   = sum(`무소속_박병일`))

  expect_that( local_sgg_20180613_check$`더불어민주당_장성욱`, equals(10724))
  expect_that( local_sgg_20180613_check$`자유한국당_이희진`,   equals(13845))
  expect_that( local_sgg_20180613_check$`무소속_박병일`,   equals(1010))

})



# 4. 내보내기 -------------

local_sgg_20180613 <- krvote::clean_varnames(local_sgg_20180613)

local_sgg_20180613 <- local_sgg_20180613 %>%
  mutate(data = map(data, krvote::clean_varnames))

usethis::use_data(local_sgg_20180613, overwrite = TRUE)


##---------------------------------------------------------------- --
##                    제6회 지방선거 시도지사 (2014)               --
##---------------------------------------------------------------- --
# 출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=14979
# ???



