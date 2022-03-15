
################################################################# ---
##                    사전 투표 데이터                         ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-03-15                       ##
################################################################# ---
## 데이터 출처: https://bit.ly/2QRqyGQ

# 대통령선거 투개표 데이터 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)
library(rvest)

##---------------------------------------------------------------- --
##                시도별 선거인수 (getCtpvElcntInfoInqire)      --
##---------------------------------------------------------------- --

# 1. 사전투표 함수 --------------

get_early_voter_from_data_portal <- function(election_code = "20220309", sido_name = "서울특별시",
                                             sgg_name = "종로구", day_code = "0") {

  cat("\n--------------------------\n", election_code, ":", sido_name, ":", sgg_name, "\n")

  data_portal_request <-
    voters_url <- glue::glue("http://apis.data.go.kr/9760000/ErVotingSttusInfoInqireService/getErVotingSttusInfoInqire?",
                             "&ServiceKey={Sys.getenv('DATA_APIKEY')}",
                             "&pageNo=1",
                             "&resultType=json",
                             "&sgId={URLencode(election_code)}",
                             "&erVotingDiv={day_code}",
                             "&sdName={URLencode(sido_name)}",
                             "&wiwName={URLencode(sgg_name)}",
                             "&numOfRows=10000")


  portal_list <- GET(data_portal_request) %>%
    content(as = "text") %>%
    jsonlite::fromJSON()

  portal_tbl <- portal_list$getErVotingSttusInfoInqire$item %>%
    as_tibble()

  portal_tbl
}


get_early_voter_from_data_portal(election_code = "20200415", sido_name = "서울특별시", sgg_name = "종로구", day_code = "0" )


# 2. 사전투표 데이터 가져오기 --------------
## 2.1. 사전투표 대상 선거 --------------

early_voting_election_code <- krvote::code_election %>%
  filter(선거코드 %in% c("20200415", "20160413", # 총선
                         "20170509",  "20220309", # 대선
                         "20140604", "20180613")) %>%
  filter(선거구분 == "0") %>%
  pull(선거코드)

early_master_table <- krvote::code_gusigun %>%
  filter(선거코드 %in% early_voting_election_code) %>%
  select(data) %>%
  unnest(data) %>%
  ## 사전투표구분
  mutate(투표일 = list(c("0", "1", "2"))) %>%
  unnest(투표일)

early_master_table


## 2.2. 데이터 크롤링 --------------

early_voting_raw <- early_master_table %>%
  filter(선거코드 != "20140604") %>%
  # slice(1:3) %>%
  mutate(data = pmap( list(선거코드, 시도명, 구시군명, 투표일), safely(get_early_voter_from_data_portal, otherwise = "error")))


# 3. 데이터 정제 --------------

early_voting_tbl <- early_voting_raw %>%
  mutate(result = map(data, "result")) %>%
  mutate(error = map_lgl(result, is.data.frame)) %>%
  filter(error) %>%
  select(선거코드, 시도명, 구시군명, 투표일, data = result)

early_voting_varname <- readxl::read_excel("inst/extdata/공공데이터포털_사전투표.xlsx", sheet = 'getErVotingSttusInfoInqire', skip = 2)

early_voting <- early_voting_tbl %>%
  unnest(data) %>%
  set_names(c("선거코드", "시도명", "구시군명", "투표일", early_voting_varname$한글변수명) )

# 4. 단위테스트 검증 -------------

test_that("선관위 사전투표 단위테스트", {

  # code_election_unit_test <- code_election %>%
  #   filter(str_detect(선거명, "제18대 대통령선거"))
  #
  # ## 선거코드
  # expect_that( code_election_unit_test %>% pull(선거코드), equals( "20121219") )
  #
})


# 5. 데이터 내보내기 -------------------------
## 5.1. 인코딩 -------------------

early_voting <- krvote::clean_varnames(early_voting)

## 5.2. 내보내기 -------------------

usethis::use_data(early_voting, overwrite = TRUE)

