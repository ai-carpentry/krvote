
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
code_election <- election_code_list %>%
  pluck('getCommonSgCodeList') %>%
  pluck('item') %>%
  as_tibble() %>%
  janitor::clean_names(ascii = FALSE) %>%
  select(선거코드 = sg_id, 선거명 = sg_name, 선거구분 = sg_typecode)

## 1.3. 단위테스트 검증 -------------

test_that("중앙선거관리위원회 마스터 코드 단위테스트", {

  code_election_unit_test <- code_election %>%
    filter(str_detect(선거명, "제18대 대통령선거"))

  ## 선거코드
  expect_that( code_election_unit_test %>% pull(선거코드), equals( "20121219") )

})


## 1.4. 데이터 내보내기 -------------------------
### 1.4.1. 인코딩 -------------------

code_election <- clean_varnames(code_election)

### 1.4.2. 내보내기 -------------------

usethis::use_data(code_election, overwrite = TRUE)

# 2. 정당코드 -------------------------
## 2.1. GET 요청 -------------------------

unique_code_election_v <- krvote::code_election %>%
  pull(선거코드) %>%
  unique()

get_party_code_from_data_portal <- function(election_code = '20200415') {

  cat("\n------------------------------\n", election_code, "\n")

  data_portal_party_code_request <-
    glue::glue("http://apis.data.go.kr/9760000/CommonCodeService/getCommonPartyCodeList",
               "?resultType=json",
               "&numOfRows=1000",
               "&sgId={election_code}",
               "&serviceKey={Sys.getenv('DATA_APIKEY')}")

  party_code_list <- GET(data_portal_party_code_request) %>%
    content(as = "text") %>%
    jsonlite::fromJSON()

  party_code <- party_code_list %>%
    pluck('getCommonPartyCodeList') %>%
    pluck('item') %>%
    as_tibble() %>%
    janitor::clean_names(ascii = FALSE) %>%
    select(선거코드 = sg_id, 정당명 = jd_name)

  party_code
}

## 2.2. 데이터 정제 -------------------------

code_party_raw <- tibble(선거코드 = unique_code_election_v) %>%
  mutate(data = map(선거코드, get_party_code_from_data_portal))

## 2.3. 단위테스트 검증 -------------

test_that("중앙선거관리위원회 정당 코드 단위테스트", {

  # code_party %>%
  #   select(data) %>%
  #   unnest(data)

})


## 2.3. 데이터 내보내기 -------------------------
### 2.3.1. 인코딩 -------------------

code_party <- code_party_raw %>%
  mutate(data = map(data, clean_varnames))

code_party <- clean_varnames(code_party)

### 2.3.2. 내보내기 -------------------

usethis::use_data(code_party, overwrite = TRUE)


# 3. 선거구 코드 -------------------------
## 3.1. GET 요청 -------------------------

get_precinct_from_data_portal <- function(electionCode = "20200415", sgTypecode =  "2") {

  data_portal_precinct_request <-
    glue::glue("http://apis.data.go.kr/9760000/CommonCodeService/getCommonSggCodeList",
               "?resultType=json",
               "&numOfRows=10000",
               "&sgId={electionCode}",
               "&sgTypecode={sgTypecode}",
               "&serviceKey={Sys.getenv('DATA_APIKEY')}")


  precinct_code_list <- content(GET(data_portal_precinct_request), as = "text") %>%
    jsonlite::fromJSON()

  precinct_code <- precinct_code_list %>%
    pluck('getCommonSggCodeList') %>%
    pluck('item') %>%
    as_tibble() %>%
    janitor::clean_names(ascii = FALSE) %>%
    select(선거코드 = sg_id, 선거구명 = sgg_name, 시도명 = sd_name, 구시군명 = wiw_name)

  precinct_code
}

## 3.2. 데이터 정제 -------------------------

code_precinct_raw <- krvote::code_election %>%
  mutate(data = map2(선거코드, 선거구분, safely(get_precinct_from_data_portal, otherwise = "error")))

code_precinct <- code_precinct_raw %>%
  mutate(error = map(data, "error")) %>%
  mutate(check = map_lgl(error, is.null)) %>%
  filter(check) %>%
  mutate(result = map(data, "result")) %>%
  select(선거코드, 선거명, 선거구분, data = result)



## 3.3. 단위테스트 검증 -------------

test_that("중앙선거관리위원회 정당 코드 단위테스트", {

  # code_party %>%
  #   select(data) %>%
  #   unnest(data)

})


## 3.4. 데이터 내보내기 -------------------------
### 3.4.1. 인코딩 -------------------

code_precinct <- code_precinct %>%
  mutate(data = map(data, clean_varnames))

code_precinct <- krvote::clean_varnames(code_precinct)

### 3.4.2. 내보내기 -------------------

usethis::use_data(code_precinct, overwrite = TRUE)


# 4. 구시군 코드 -------------------------
## 4.1. GET 요청 -------------------------

get_gusigun_from_data_portal <- function(electionCode = '20200415') {

  cat("\n------------------------------------\n", electionCode, "\n")

  data_portal_gusigun_code_request <-
    glue::glue("http://apis.data.go.kr/9760000/CommonCodeService/getCommonGusigunCodeList",
               "?resultType=json",
               "&sgId={electionCode}",
               "&numOfRows=10000",
               "&serviceKey={Sys.getenv('DATA_APIKEY')}")

  gusigun_code_list <- content(GET(data_portal_gusigun_code_request), as = "text") %>%
    jsonlite::fromJSON()

  gusigun_code <- gusigun_code_list %>%
    pluck('getCommonGusigunCodeList') %>%
    pluck('item') %>%
    as_tibble() %>%
    janitor::clean_names(ascii = FALSE) %>%
    filter(str_detect(sd_name, "[가-힣]{2,}")) %>%
    select(선거코드 = sg_id, 시도명 = sd_name, 구시군명 = wiw_name)

  gusigun_code
}

## 4.2. 데이터 정제 -------------------------

code_gusigun_raw <- krvote::code_election %>%
  group_by(선거코드) %>%
  summarise(선거코드수 = n()) %>%
  mutate(data = map(선거코드, safely(get_gusigun_from_data_portal, otherwise = "error")))

code_gusigun <- code_gusigun_raw %>%
  mutate(error = map(data, "error")) %>%
  mutate(check = map_lgl(error, is.null)) %>%
  filter(check) %>%
  mutate(result = map(data, "result")) %>%
  select(선거코드, data = result)

## 4.3. 단위테스트 검증 -------------

test_that("중앙선거관리위원회 구시군 코드 단위테스트", {

  # code_party %>%
  #   select(data) %>%
  #   unnest(data)

})


## 4.4. 데이터 내보내기 -------------------------
### 4.4.1. 인코딩 -------------------

code_gusigun <- code_gusigun %>%
  mutate(data = map(data, clean_varnames))

code_gusigun <- krvote::clean_varnames(code_gusigun)

### 4.4.2. 내보내기 -------------------

usethis::use_data(code_gusigun, overwrite = TRUE)




