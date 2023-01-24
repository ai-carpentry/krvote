
################################################################# ---
##              선관위 선거통계시스템: 구시군의장 당선인 명부  ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2023-01-25                       ##
################################################################# ---

# 팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(rvest)
library(here)
library(httr)

##---------------------------------------------------------------- ---
##                      선거구수 및 정수현황                       --
##---------------------------------------------------------------- ---
# 1. 구시군의장 ---------------------------------------------------
## 1.1. 한번 시도 ---------------------------------------------------

winner_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                             "electionId=0000000000",
                             "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0000000000%2Fep%2Fepei01.jsp",
                             "&topMenuId=EP",
                             "&secondMenuId=EPEI01",
                             "&menuId=EPEI01",
                             "&statementId=EPEI01_%231",
                             "&oldElectionType=1",
                             "&electionType=4",
                             "&electionName=20220601",
                             "&electionCode=4",
                             "&cityCode=1100",
                             "&proportionalRepresentationCode=-1",
                             "&townCode=-1")


winner_resp <- GET(winner_request) %>%
  content(as="text") %>%
  rvest::read_html()

winner_raw <- winner_resp %>%
  html_elements("#table01") %>%
  html_table() %>%
  .[[1]]

winner_raw

## 1.2. 함수 ----------------------------------------------------

get_winner <- function(election_code = "20220601", sido_code = "1100") {

  winner_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                               "electionId=0000000000",
                               "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0000000000%2Fep%2Fepei01.jsp",
                               "&topMenuId=EP",
                               "&secondMenuId=EPEI01",
                               "&menuId=EPEI01",
                               "&statementId=EPEI01_%231",
                               "&oldElectionType=1",
                               "&electionType=4",
                               "&electionName={election_code}",
                               "&electionCode=4",
                               "&cityCode={sido_code}",
                               "&proportionalRepresentationCode=-1",
                               "&townCode=-1")



  winner_resp <- GET(winner_request) %>%
    content(as="text") %>%
    rvest::read_html()

  winner_tbl <- winner_resp %>%
    html_elements("#table01") %>%
    html_table() %>%
    .[[1]]

  return(winner_tbl)
}

get_winner("20220601", "1100")
get_winner("20140604", "1100")

## 1.3. 가져오기 ----------------------------------------------------

sido_code <- tribble(~"sido_code", ~"sido_name",
                     "1100", "서울특별시",
                     "2600", "부산광역시",
                     "2700", "대구광역시",
                     "2800", "인천광역시",
                     "2900", "광주광역시",
                     "3000", "대전광역시",
                     "3100", "울산광역시",
                     "5100", "세종특별자치시",
                     "4100", "경기도",
                     "4200", "강원도",
                     "4300", "충청북도",
                     "4400", "충청남도",
                     "4500", "전라북도",
                     "4600", "전라남도",
                     "4700", "경상북도",
                     "4800", "경상남도",
                     "4900", "제주특별자치도")

local_code <- tribble(~"election_code", ~"election_name",
                       "20220601", "제8회",
                       "20180613", "제7회",
                       "20140604", "제6회",
                       "20100602", "제5회",
                       "20060531", "제4회",
                       "20020613", "제3회",
                       "19980604", "제2회",
                       "19950627", "제1회")


local_sgg_election_raw <- tidyr::crossing(local_code, sido_code) %>%
  mutate(election_code = parse_number(election_code)) %>%
  # filter(election_code >= 20100602) %>%
  mutate(data = map2(election_code, sido_code, get_winner))

local_sgg_winner <- local_sgg_election_raw %>%
  unnest(data) %>%
  separate(`득표수(득표율)`, into = c("득표수", "득표율"), sep = "\\(") %>%
  mutate(득표수 = parse_number(득표수),
         득표율 = parse_number(득표율)) %>%
  separate(`생년월일(연령)`, into = c("생년월일", "연령"), sep = "\\(") %>%
  mutate(연령     = parse_number(연령)) %>%
  filter(!is.na(연령))


# # A tibble: 8 × 2
# election_name     n
# <chr>         <int>
#   1 제1회           230
# 2 제2회           232
# 3 제3회           232
# 4 제4회           230
# 5 제5회           228
# 6 제6회           226
# 7 제7회           226
# 8 제8회           226

## 1.4. 내보내기 ----------------------------------------------------

source("R/util.R")

local_sgg_winner <- krvote::clean_varnames(local_sgg_winner)

local_sgg_winner <- make_dataframe_clean(local_sgg_winner)

usethis::use_data(local_sgg_winner, overwrite = TRUE)


