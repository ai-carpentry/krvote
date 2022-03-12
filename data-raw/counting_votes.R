
################################################################# ---
##                    투개표 데이터                            ##
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
##                투표 데이터 (getVoteSttusInfoInqire)      --
##---------------------------------------------------------------- --
# 선거ID와 선거종류코드, 시도명, 구시군명을 입력하여 투표 결과를 조회할 수 있는 서비스
# 재·보궐 - 코드정보API에서 조회되는 모든 sgTypecode

# 1. 투표(Casting) 데이터 --------------
## 1.1. GET 요청 -------------------------

get_casting_votes_from_data_portal <- function(election_code = "20200415",
                                                election_type = "7",
                                                sido_name = '서울특별시',
                                                gusigun_name = "종로구") {

  cat("\n--------------------------\n", election_code, ":", election_type, ":", sido_name, ":", gusigun_name,  "\n")

  data_portal_casting_request <-
    voters_url <- glue::glue("http://apis.data.go.kr/9760000/VoteXmntckInfoInqireService2/getVoteSttusInfoInqire?",
                             "&ServiceKey={Sys.getenv('DATA_APIKEY')}",
                             "&pageNo=1",
                             "&resultType=json",
                             "&sgId={URLencode(election_code)}",
                             "&sgTypecode={URLencode(election_type)}",
                             "&sdName={URLencode(sido_name)}",
                             "&wiwName={URLencode(gusigun_name)}",
                             "&numOfRows=10000")


  casting_votes_list <- GET(data_portal_casting_request) %>%
    content(as = "text") %>%
    jsonlite::fromJSON()

  casting_votes_tbl <- casting_votes_list$getVoteSttusInfoInqire$item %>%
    as_tibble()

  casting_votes_tbl
}


get_casting_votes_from_data_portal()


## 1.2. 데이터 크롤링 -------------------------

casting_votes_code <- krvote::code_election %>%
  filter(선거구분 %in% c("1", "7", "3")) %>%
  left_join(krvote::code_gusigun) %>%
  select(-선거코드) %>%
  unnest(data) %>%
  relocate(선거코드, .before = 선거명)

casting_votes_raw <- casting_votes_code %>%
  mutate(data = pmap(list(선거코드, 선거구분, 시도명, 구시군명), safely(get_casting_votes_from_data_portal, otherwise = "error")) )

## 1.3. 데이터 정제 -------------------------

casting_votes_tbl <- casting_votes_raw %>%
  # 제8회 지방선거 NULL 제거
  mutate(error = map(data, "error") ) %>%
  mutate(error_out = map_lgl(error, is.null)) %>%
  filter(error_out) %>%
  # 정상 크로링 데이터
  mutate(result = map(data, "result")) %>%
  select(선거코드, 선거명, 선거구분, 시도명, 구시군명, data = result)

casting_votes_varname <- readxl::read_excel("inst/extdata/공공데이터포털_투개표.xlsx", sheet = 'getVoteSttusInfoInqire', skip = 2)

casting_votes <- casting_votes_tbl %>%
  unnest(data) %>%
  set_names(c("선거코드", "선거명", "선거구분", "시도명", "구시군명", casting_votes_varname$한글변수명) )

## 1.4. 단위테스트 검증 -------------

test_that("선관위 투표자수 단위테스트", {

  # code_election_unit_test <- code_election %>%
  #   filter(str_detect(선거명, "제18대 대통령선거"))
  #
  # ## 선거코드
  # expect_that( code_election_unit_test %>% pull(선거코드), equals( "20121219") )
  #
})


## 1.5. 데이터 내보내기 -------------------------
### 1.5.1. 인코딩 -------------------

casting_votes <- clean_varnames(casting_votes)

### 1.5.2. 내보내기 -------------------

usethis::use_data(casting_votes, overwrite = TRUE)


# 2. 득표(Winning) 데이터 --------------
## 2.1. GET 요청 -------------------------
# 주의사항 : 투표수 : 유효투표수 + 무효투표수, 선거인수 : 투표수 + 기권수

get_counting_votes_from_data_portal <- function(election_code = "20200415",
                                                election_type = "2",
                                                sido_name = '서울특별시',
                                                gusigun_name = "종로구",
                                                precinct_name = "종로구") {

  cat("\n--------------------------\n", election_code, ":", election_type, ":",
      sido_name, ":", precinct_name, ":", gusigun_name,  "\n")

  data_portal_casting_request <-
    voters_url <- glue::glue("http://apis.data.go.kr/9760000/VoteXmntckInfoInqireService2/getXmntckSttusInfoInqire?",
                             "&ServiceKey={Sys.getenv('DATA_APIKEY')}",
                             "&pageNo=1",
                             "&resultType=json",
                             "&sgId={URLencode(election_code)}",
                             "&sgTypecode={URLencode(election_type)}",
                             "&sdName={URLencode(sido_name)}",
                             "&sggName={URLencode(precinct_name)}",
                             "&wiwName={URLencode(gusigun_name)}",
                             "&numOfRows=10000")


  casting_votes_list <- GET(data_portal_casting_request) %>%
    content(as = "text") %>%
    jsonlite::fromJSON()

  casting_votes_tbl <- casting_votes_list$getXmntckSttusInfoInqire$item %>%
    as_tibble()

  casting_votes_tbl
}


get_counting_votes_from_data_portal()


## 2.2. 데이터 크롤링 -------------------------
counting_votes_code <- krvote::code_precinct %>%
  select(선거구분, data) %>%
  unnest(data, names_repair = "unique") %>%
  filter(구시군명 != "") %>%
  select(선거코드, 선거구분, 시도명, 구시군명, 선거구명)

counting_votes_raw <- counting_votes_code %>%
  mutate(data = pmap(list(선거코드, 선거구분, 시도명, 구시군명, 선거구명), safely(get_counting_votes_from_data_portal, otherwise = "error")) )

## 2.3. 데이터 정제 -------------------------

counting_votes_tbl <- counting_votes_raw %>%
  # 제8회 지방선거 NULL 제거
  mutate(error = map(data, "error") ) %>%
  mutate(error_out = map_lgl(error, is.null)) %>%
  filter(error_out) %>%
  # 정상 크로링 데이터
  mutate(result = map(data, "result")) %>%
  select(선거코드, 선거구분, 시도명, 구시군명, 선거구명, data = result)

counting_votes_varname <- readxl::read_excel("inst/extdata/공공데이터포털_투개표.xlsx", sheet = 'getXmntckSttusInfoInqire', skip = 2)

counting_votes <- counting_votes_tbl %>%
  unnest(data) %>%
  set_names(c("선거코드", "선거구분", "시도명", "구시군명", "선거구명", counting_votes_varname$한글변수명) )

## 2.4. 단위테스트 검증 -------------

test_that("선관위 득표수 단위테스트", {

  # code_election_unit_test <- code_election %>%
  #   filter(str_detect(선거명, "제18대 대통령선거"))
  #
  # ## 선거코드
  # expect_that( code_election_unit_test %>% pull(선거코드), equals( "20121219") )
  #
})


## 2.5. 데이터 내보내기 -------------------------
### 2.5.1. 인코딩 -------------------

counting_votes <- clean_varnames(counting_votes)

### 2.5.2. 내보내기 -------------------

usethis::use_data(counting_votes, overwrite = TRUE)


