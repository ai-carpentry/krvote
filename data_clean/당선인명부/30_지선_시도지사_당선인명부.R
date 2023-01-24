
################################################################# ---
##              선관위 선거통계시스템: 시도지사 당선인 명부    ##
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
# 1. 국회의원 ---------------------------------------------------
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
                             "&electionCode=3",
                             "&cityCode=-1",
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

get_winner <- function(election_code = "20220601") {

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
                               "&electionCode=3",
                               "&cityCode=-1",
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

get_winner("20220601")
get_winner("20140604")

## 1.3. 가져오기 ----------------------------------------------------

local_code <- tribble(~"election_code", ~"election_name",
                       "20220601", "제8회",
                       "20180613", "제7회",
                       "20140604", "제6회",
                       "20100602", "제5회",
                       "20060531", "제4회",
                       "20020613", "제3회",
                       "19980604", "제2회",
                       "19950627", "제1회")

local_election_raw <- local_code %>%
  mutate(election_code = parse_number(election_code)) %>%
  # filter(election_code >= 20100602) %>%
  mutate(data = map(election_code, get_winner))

local_winner <- local_election_raw %>%
  unnest(data) %>%
  separate(`득표수(득표율)`, into = c("득표수", "득표율"), sep = "\\(") %>%
  mutate(득표수 = parse_number(득표수),
         득표율 = parse_number(득표율)) %>%
  separate(`생년월일(연령)`, into = c("생년월일", "연령"), sep = "\\(") %>%
  mutate(연령     = parse_number(연령)) %>%
  filter(!is.na(연령))


## 1.4. 내보내기 ----------------------------------------------------

source("R/util.R")

local_winner <- krvote::clean_varnames(local_winner)

local_winner <- make_dataframe_clean(local_winner)

usethis::use_data(local_winner, overwrite = TRUE)


