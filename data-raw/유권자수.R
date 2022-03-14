
################################################################# ---
##                선거인수 유권자 데이터                       ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-03-12                       ##
################################################################# ---


# 팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)

##---------------------------------------------------------------- --
##                시도별 선거인수 (getCtpvElcntInfoInqire)      --
##---------------------------------------------------------------- --

# 1. 시도별 선거인수 정보 --------------
## 1.1. GET 요청 -------------------------

get_voter_sido_from_data_portal <- function(election_code = "20220309", election_type = "0") {

  cat("\n--------------------------\n", election_code, ":", election_type, "\n")

  data_portal_voter_sido_request <-
    voters_url <- glue::glue("http://apis.data.go.kr/9760000/ElcntInfoInqireService/getCtpvElcntInfoInqire?",
                             "&ServiceKey={Sys.getenv('DATA_APIKEY')}",
                             "&pageNo=1",
                             "&resultType=json",
                             "&sgId={URLencode(election_code)}",
                             "&sgTypecode={election_type}",
                             "&numOfRows=10000")


  voters_sido_list <- GET(data_portal_voter_sido_request) %>%
    content(as = "text") %>%
    jsonlite::fromJSON()

  voters_sido_tbl <- voters_sido_list$getCtpvElcntInfoInqire$item %>%
    as_tibble()

  voters_sido_tbl
}


get_voter_sido_from_data_portal(election_code = "20220309", election_type = "0")


## 1.2. 데이터 크롤링 -------------------------

voter_sido_raw <- krvote::code_election %>%
  mutate(data = map2(선거코드, 선거구분, safely(get_voter_sido_from_data_portal, otherwise = "error")) )


## 1.3. 데이터 정제 -------------------------

voter_sido_tbl <- voter_sido_raw %>%
  # 제8회 지방선거 NULL 제거
  mutate(error = map(data, "error") ) %>%
  mutate(error_out = map_lgl(error, is.null)) %>%
  filter(error_out) %>%
  # 정상 크로링 데이터
  mutate(result = map(data, "result")) %>%
  select(선거코드, 선거명, 선거구분, data = result)

voter_sido_varname <- readxl::read_excel("inst/extdata/공공데이터포털.xlsx", sheet = 'getCtpvElcntInfoInqire', skip = 2)

voter_sido <- voter_sido_tbl %>%
  unnest(data) %>%
  set_names(c("선거코드", "선거명", "선거구분", voter_sido_varname$한글변수명) )

## 1.4. 단위테스트 검증 -------------

test_that("선관위 선거, 시도별 유권자수 단위테스트", {

  # code_election_unit_test <- code_election %>%
  #   filter(str_detect(선거명, "제18대 대통령선거"))
  #
  # ## 선거코드
  # expect_that( code_election_unit_test %>% pull(선거코드), equals( "20121219") )
  #
})


## 1.5. 데이터 내보내기 -------------------------
### 1.5.1. 인코딩 -------------------

voter_sido <- clean_varnames(voter_sido)

### 1.5.2. 내보내기 -------------------

usethis::use_data(voter_sido, overwrite = TRUE)


##---------------------------------------------------------------- --
##                구시군별 선거인수 (getGsigElcntInfoInqire)      --
##---------------------------------------------------------------- --

# 2. 구시군별 선거인수 정보 --------------
## 2.1. GET 요청 -------------------------

get_voter_gusigun_from_data_portal <- function(election_code = "20220309", election_type = "0", sido_name = '서울특별시') {

  cat("\n--------------------------\n", election_code, ":", election_type, ":", sido_name, "\n")

  data_portal_voter_request <-
    voters_url <- glue::glue("http://apis.data.go.kr/9760000/ElcntInfoInqireService/getGsigElcntInfoInqire?",
                             "&ServiceKey={Sys.getenv('DATA_APIKEY')}",
                             "&pageNo=1",
                             "&resultType=json",
                             "&sgId={URLencode(election_code)}",
                             "&sgTypecode={election_type}",
                             "&sdName={URLencode(sido_name)}",
                             "&numOfRows=10000")


  voters_list <- GET(data_portal_voter_request) %>%
    content(as = "text") %>%
    jsonlite::fromJSON()

  voters_tbl <- voters_list$getGsigElcntInfoInqire$item %>%
    as_tibble()

  voters_tbl
}


get_voter_gusigun_from_data_portal(election_code = "20220309", election_type = "0", sido_name = '서울특별시')


## 2.2. 데이터 크롤링 -------------------------

sido_code_tbl <- krvote::code_gusigun %>%
  select(data) %>%
  unnest(data) %>%
  count(선거코드, 시도명) %>%
  select(-n)

vote_gusigun_raw <- krvote::code_election %>%
  left_join(sido_code_tbl) %>%
  mutate(data = pmap(list(선거코드, 선거구분, 시도명), safely(get_voter_gusigun_from_data_portal, otherwise = "error")) )

vote_gusigun_raw


## 2.3. 데이터 정제 -------------------------

voter_gusigun_tbl <- vote_gusigun_raw %>%
  # 제8회 지방선거 NULL 제거
  mutate(error = map(data, "error") ) %>%
  mutate(error_out = map_lgl(error, is.null)) %>%
  filter(error_out) %>%
  # 정상 크로링 데이터
  mutate(result = map(data, "result")) %>%
  select(선거코드, 선거명, 선거구분, data = result)

voter_gusigun_varname <- readxl::read_excel("inst/extdata/공공데이터포털.xlsx", sheet = 'getGsigElcntInfoInqire', skip = 2)

voter_gusigun <- voter_gusigun_tbl %>%
  unnest(data) %>%
  set_names(c("선거코드", "선거명", "선거구분", voter_gusigun_varname$한글변수명) )

## 1.4. 단위테스트 검증 -------------

test_that("선관위 선거, 구시군별 유권자수 단위테스트", {

  # code_election_unit_test <- code_election %>%
  #   filter(str_detect(선거명, "제18대 대통령선거"))
  #
  # ## 선거코드
  # expect_that( code_election_unit_test %>% pull(선거코드), equals( "20121219") )
  #
})


## 1.5. 데이터 내보내기 -------------------------
### 1.5.1. 인코딩 -------------------

voter_gusigun <- clean_varnames(voter_gusigun)

### 1.5.2. 내보내기 -------------------

usethis::use_data(voter_gusigun, overwrite = TRUE)


##---------------------------------------------------------------- --
##                투표구별 선거인수 (getVtdsElcntInfoInqire)      --
##---------------------------------------------------------------- --

# 3. 투표구별 선거인수 정보 --------------
## 3.1. GET 요청 -------------------------

get_voter_station_from_data_portal <- function(election_code = "20220309", sido_name = '서울특별시', gusigun_name = "종로구") {

  cat("\n--------------------------\n", election_code, ":", sido_name, ":", gusigun_name,  "\n")

  data_portal_voter_request <-
    voters_url <- glue::glue("http://apis.data.go.kr/9760000/ElcntInfoInqireService/getVtdsElcntInfoInqire?",
                             "&ServiceKey={Sys.getenv('DATA_APIKEY')}",
                             "&pageNo=1",
                             "&resultType=json",
                             "&sgId={URLencode(election_code)}",
                             "&sdName={URLencode(sido_name)}",
                             "&wiwName={URLencode(gusigun_name)}",
                             "&numOfRows=10000")


  voters_list <- GET(data_portal_voter_request) %>%
    content(as = "text") %>%
    jsonlite::fromJSON()

  voters_tbl <- voters_list$getVtdsElcntInfoInqire$item %>%
    as_tibble()

  voters_tbl
}


get_voter_station_from_data_portal(election_code = "20220309", sido_name = '서울특별시', gusigun_name = "종로구")


## 3.2. 데이터 크롤링 -------------------------

gusigun_code_tbl <- krvote::code_gusigun %>%
  select(data) %>%
  unnest(data) %>%
  count(선거코드, 시도명, 구시군명) %>%
  select(-n)

vote_station_raw <- gusigun_code_tbl %>%
  mutate(data = pmap(list(선거코드, 시도명, 구시군명), safely(get_voter_station_from_data_portal, otherwise = "error")) )

vote_station_raw

vote_station_raw %>%
  write_rds("inst/extdata/vote_station_raw.rds")


## 3.3. 데이터 정제 -------------------------

voter_station_tbl <- vote_station_raw %>%
  # 제8회 지방선거 NULL 제거
  mutate(error = map(data, "error") ) %>%
  mutate(error_out = map_lgl(error, is.null)) %>%
  filter(error_out) %>%
  # 정상 크로링 데이터
  mutate(result = map(data, "result")) %>%
  select(선거코드, 시도명, 구시군명, data = result)

voter_station_varname <- readxl::read_excel("inst/extdata/공공데이터포털.xlsx", sheet = 'getVtdsElcntInfoInqire', skip = 2)

voter_station <- voter_station_tbl %>%
  unnest(data) %>%
  set_names(c("선거코드", "시도명", "구시군명", voter_station_varname$한글변수명) )

## 3.4. 단위테스트 검증 -------------

test_that("선관위 선거, 구시군, 투표구별 유권자수 단위테스트", {

  # code_election_unit_test <- code_election %>%
  #   filter(str_detect(선거명, "제18대 대통령선거"))
  #
  # ## 선거코드
  # expect_that( code_election_unit_test %>% pull(선거코드), equals( "20121219") )
  #
})


## 3.5. 데이터 내보내기 -------------------------
### 3.5.1. 인코딩 -------------------

voter_station <- clean_varnames(voter_station)

### 3.5.2. 내보내기 -------------------

usethis::use_data(voter_station, overwrite = TRUE)


##---------------------------------------------------------------- --
##                읍면동별 선거인수 (getEmdElcntInfoInqire)      --
##---------------------------------------------------------------- --

# 4. 읍면동별 선거인수 정보 --------------
## 4.1. GET 요청 -------------------------

get_voter_emd_from_data_portal <- function(election_code = "20220309", sido_name = '서울특별시', gusigun_name = "종로구") {

  cat("\n--------------------------\n", election_code, ":", sido_name, ":", gusigun_name,  "\n")

  data_portal_voter_request <-
    voters_url <- glue::glue("http://apis.data.go.kr/9760000/ElcntInfoInqireService/getEmdElcntInfoInqire?",
                             "&ServiceKey={Sys.getenv('DATA_APIKEY')}",
                             "&pageNo=1",
                             "&resultType=json",
                             "&sgId={URLencode(election_code)}",
                             "&sdName={URLencode(sido_name)}",
                             "&wiwName={URLencode(gusigun_name)}",
                             "&numOfRows=10000")


  voters_list <- GET(data_portal_voter_request) %>%
    content(as = "text") %>%
    jsonlite::fromJSON()

  voters_tbl <- voters_list$getEmdElcntInfoInqire$item %>%
    as_tibble()

  voters_tbl
}


get_voter_emd_from_data_portal(election_code = "20220309", sido_name = '서울특별시', gusigun_name = "종로구")


## 4.2. 데이터 크롤링 -------------------------

gusigun_code_tbl <- krvote::code_gusigun %>%
  select(data) %>%
  unnest(data) %>%
  count(선거코드, 시도명, 구시군명) %>%
  select(-n)

vote_emd_raw <- gusigun_code_tbl %>%
  mutate(data = pmap(list(선거코드, 시도명, 구시군명), safely(get_voter_emd_from_data_portal, otherwise = "error")) )

vote_emd_raw

vote_emd_raw %>%
  write_rds("inst/extdata/vote_emd_raw.rds")


##4.3. 데이터 정제 -------------------------

voter_emd_tbl <- vote_emd_raw %>%
  # 제8회 지방선거 NULL 제거
  mutate(error = map(data, "error") ) %>%
  mutate(error_out = map_lgl(error, is.null)) %>%
  filter(error_out) %>%
  # 정상 크로링 데이터
  mutate(result = map(data, "result")) %>%
  select(선거코드, 시도명, 구시군명, data = result)

voter_emd_varname <- readxl::read_excel("inst/extdata/공공데이터포털.xlsx", sheet = 'getEmdElcntInfoInqire', skip = 2)

voter_emd <- voter_emd_tbl %>%
  unnest(data) %>%
  set_names(c("선거코드", "시도명", "구시군명", voter_emd_varname$한글변수명) )

## 4.4. 단위테스트 검증 -------------

test_that("선관위 선거, 구시군, 읍면동별 유권자수 단위테스트", {

  # code_election_unit_test <- code_election %>%
  #   filter(str_detect(선거명, "제18대 대통령선거"))
  #
  # ## 선거코드
  # expect_that( code_election_unit_test %>% pull(선거코드), equals( "20121219") )
  #
})


## 4.5. 데이터 내보내기 -------------------------
### 4.5.1. 인코딩 -------------------

voter_emd <- clean_varnames(voter_emd)

### 4.5.2. 내보내기 -------------------

usethis::use_data(voter_emd, overwrite = TRUE)




##---------------------------------------------------------------- --
##                선거구별 선거인수 (getElpcElcntInfoInqire)      --
##---------------------------------------------------------------- --

# 5. 읍면동별 선거인수 정보 --------------
## 5.1. GET 요청 -------------------------

get_voter_precinct_from_data_portal <- function(election_code = "20220309", election_type = "2", sido_name = '서울특별시', gusigun_name = "종로구") {

  cat("\n--------------------------\n", election_code, ":", election_type, ":", sido_name, ":", gusigun_name,  "\n")

  data_portal_voter_request <-
    voters_url <- glue::glue("http://apis.data.go.kr/9760000/ElcntInfoInqireService/getElpcElcntInfoInqire?",
                             "&ServiceKey={Sys.getenv('DATA_APIKEY')}",
                             "&pageNo=1",
                             "&resultType=json",
                             "&sgId={URLencode(election_code)}",
                             "&sgTypecode={URLencode(election_type)}",
                             "&sdName={URLencode(sido_name)}",
                             "&wiwName={URLencode(gusigun_name)}",
                             "&numOfRows=10000")


  voters_list <- GET(data_portal_voter_request) %>%
    content(as = "text") %>%
    jsonlite::fromJSON()

  voters_tbl <- voters_list$getElpcElcntInfoInqire$item %>%
    as_tibble()

  voters_tbl
}


get_voter_precinct_from_data_portal(election_code = "20200415", election_type = "2", sido_name = '서울특별시', gusigun_name = "종로구")


## 5.2. 데이터 크롤링 -------------------------


precinct_code_tbl <- krvote::code_gusigun %>%
  select(data) %>%
  unnest(data) %>%
  count(선거코드, 시도명, 구시군명) %>%
  select(-n) %>%
  left_join(krvote::code_election %>% filter(선거구분 %in% c(2, 3, 4, 5, 6, 10, 11)) ) %>%
  filter(!is.na(선거구분))

vote_precinct_raw <- precinct_code_tbl %>%
  mutate(data = pmap(list(선거코드, 선거구분, 시도명, 구시군명), safely(get_voter_precinct_from_data_portal, otherwise = "error")) )

vote_precinct_raw

vote_precinct_raw %>%
  write_rds("inst/extdata/vote_precinct_raw.rds")


## 5.3. 데이터 정제 -------------------------

voter_precinct_tbl <- vote_precinct_raw %>%
  # 제8회 지방선거 NULL 제거
  mutate(error = map(data, "error") ) %>%
  mutate(error_out = map_lgl(error, is.null)) %>%
  filter(error_out) %>%
  # 정상 크로링 데이터
  mutate(result = map(data, "result")) %>%
  select(선거코드, 선거구분, 선거명, 시도명, 구시군명, data = result)

voter_precinct_varname <- readxl::read_excel("inst/extdata/공공데이터포털.xlsx", sheet = 'getElpcElcntInfoInqire', skip = 2)

voter_precinct <- voter_precinct_tbl %>%
  unnest(data) %>%
  set_names(c("선거코드", "선거구분", "선거명", "시도명", "구시군명", voter_precinct_varname$한글변수명) )

## 5.4. 단위테스트 검증 -------------

test_that("선관위 선거, 구시군, 선거구별 유권자수 단위테스트", {

  # code_election_unit_test <- code_election %>%
  #   filter(str_detect(선거명, "제18대 대통령선거"))
  #
  # ## 선거코드
  # expect_that( code_election_unit_test %>% pull(선거코드), equals( "20121219") )
  #
})


## 5.5. 데이터 내보내기 -------------------------
### 5.5.1. 인코딩 -------------------

voter_precinct <- clean_varnames(voter_precinct)

### 5.5.2. 내보내기 -------------------

usethis::use_data(voter_precinct, overwrite = TRUE)

