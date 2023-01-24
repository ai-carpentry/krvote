
################################################################# ---
##              선관위 선거통계시스템: 국회의원 당선인 명부    ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-12-25                       ##
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
                             "&electionType=2",
                             "&electionName=20200415",
                             "&electionCode=2",
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

get_winner <- function(election_code = "20200415", sido_code = "1100") {

  winner_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                               "electionId=0000000000",
                               "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0000000000%2Fep%2Fepei01.jsp",
                               "&topMenuId=EP",
                               "&secondMenuId=EPEI01",
                               "&menuId=EPEI01",
                               "&statementId=EPEI01_%231",
                               "&oldElectionType=1",
                               "&electionType=2",
                               "&electionName={election_code}",
                               "&electionCode=2",
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

get_winner("19880426", "1100")
get_winner("20200415", "1100")

## 1.3. 가져오기 ----------------------------------------------------

code_sido <- tribble(~"sido_code", ~"sido_name",
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

chongsun_code <- tribble(~"cs_code", ~"cs_name",
                         "20200415", "제21대",
                         "20160413", "제20대",
                         "20120411", "제19대",
                         "20080409", "제18대",
                         "20040415", "제17대",
                         "20000413", "제16대",
                         "19960411", "제15대",
                         "19920324", "제14대",
                         "19880426", "제13대",
                         "19850212", "제12대",
                         "19810325", "제11대",
                         "19781212", "제10대",
                         "19730227", "제9대",
                         "19710525", "제8대",
                         "19670608", "제7대",
                         "19631126", "제6대",
                         "19600729", "제5대",
                         "19580502", "제4대",
                         "19540520", "제3대",
                         "19500530", "제2대",
                         "19480510", "제1대")

chongsun_election_master <- tidyr::crossing(chongsun_code, code_sido) %>%
  group_by(cs_code, cs_name) %>%
  nest() %>%
  mutate(cs_code = parse_number(cs_code)) %>%
  filter(cs_code >= 19880426)

chongsun_election_raw <- chongsun_election_master %>%
  unnest(data) %>%
  mutate(data = map2(cs_code, sido_code, get_winner))

chongun_winner <- chongsun_election_raw %>%
  unnest(data) %>%
  separate(`득표수(득표율)`, into = c("득표수", "득표율"), sep = "\\(") %>%
  mutate(득표수 = parse_number(득표수),
         득표율 = parse_number(득표율)) %>%
  separate(`생년월일(연령)`, into = c("생년월일", "연령"), sep = "\\(") %>%
  mutate(연령     = parse_number(연령)) %>%
  filter(!is.na(연령))

# chongun_winner %>%
#   count(cs_code)

# cs_code cs_name     n
# <dbl> <chr>   <int>
#   1 19880426 제13대    224
# 2 19920324 제14대    237
# 3 19960411 제15대    253
# 4 20000413 제16대    227
# 5 20040415 제17대    243
# 6 20080409 제18대    245
# 7 20120411 제19대    246
# 8 20160413 제20대    253
# 9 20200415 제21대    253

## 1.4. 내보내기 ----------------------------------------------------

chongun_winner <- krvote::clean_varnames(chongun_winner)

chongun_winner <- make_dataframe_clean(chongun_winner)

usethis::use_data(chongun_winner, overwrite = TRUE)


