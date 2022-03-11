
################################################################# ---
##              공공데이터포털 마스터 데이터                   ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-03-11                       ##
################################################################# ---

# 팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)

##---------------------------------------------------------------- ---
##                      선거코드                                --
##---------------------------------------------------------------- ---

# 1. 선거코드 -------------------------
## 1.1. GET 요청 -------------------------

data_portal_election_code_request <-
  glue::glue("http://apis.data.go.kr/9760000/CommonCodeService/getCommonSgCodeList",
             "?resultType=json",
             "&numOfRows=1000",
             "&serviceKey={Sys.getenv('DATA_APIKEY')}")


election_code_list <- GET(data_portal_election_code_request) %>%
  content(as = "text") %>%
  jsonlite::fromJSON()

## 1.2. 데이터 정제 -------------------------
election_code <- election_code_list %>%
  pluck('getCommonSgCodeList') %>%
  pluck('item') %>%
  as_tibble() %>%
  janitor::clean_names(ascii = FALSE) %>%
  select(선거코드 = sg_id, 선거명 = sg_name, 선거구분 = sg_typecode)

## 1.3. 단위테스트 검증 -------------

test_that("중앙선거관리위원회 마스터 코드 단위테스트", {

  election_code_unit_test <- election_code %>%
    filter(str_detect(선거명, "제18대 대통령선거"))

  ## 선거코드
  expect_that( election_code_unit_test %>% pull(선거코드), equals( "20121219") )

})


## 1.4. 데이터 내보내기 -------------------------
### 1.4.1. 인코딩 -------------------

election_code <- clean_varnames(election_code)

### 1.4.2. 내보내기 -------------------

usethis::use_data(election_code, overwrite = TRUE)

