
################################################################# ---
##              선관위 선거통계시스템: 대통령 당선인 명부      ##
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
# 1. 대통령선거 ---------------------------------------------------
## 1.1. 한번 시도 ---------------------------------------------------

winner_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                             "electionId=0000000000",
                             "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0000000000%2Fep%2Fepei01.jsp",
                             "&topMenuId=EP",
                             "&secondMenuId=EPEI01",
                             "&menuId=EPEI01",
                             "&statementId=EPEI01_%231",
                             "&oldElectionType=1",
                             "&electionType=1",
                             "&electionName=20220309",
                             "&electionCode=1",
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


## 1.2. 함수 ----------------------------------------------------

# 노무현 대통령 이전 statementId 값이 각기 다름  : EPEI01_#91 EPEI01_#1

get_winner <- function(election_code = "20220309") {

  if( election_code == "19871216") { ### 제13대 노태우 --------
    winner_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                                 "electionId=0000000000",
                                 "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0000000000%2Fep%2Fepei01.jsp",
                                 "&topMenuId=EP",
                                 "&secondMenuId=EPEI01",
                                 "&menuId=EPEI01",
                                 "&statementId=EPEI01_%2391",
                                 "&oldElectionType=0",
                                 "&electionType=1",
                                 "&electionName={election_code}",
                                 "&electionCode=1",
                                 "&cityCode=-1",
                                 "&proportionalRepresentationCode=-1",
                                 "&townCode=-1")
  } else if( election_code == "19921218") { ### 제14대 김영삼 --------
    winner_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                                 "electionId=0000000000",
                                 "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0000000000%2Fep%2Fepei01.jsp",
                                 "&topMenuId=EP",
                                 "&secondMenuId=EPEI01",
                                 "&menuId=EPEI01",
                                 "&statementId=EPEI01_%2391",
                                 "&oldElectionType=0",
                                 "&electionType=1",
                                 "&electionName={election_code}",
                                 "&electionCode=1",
                                 "&cityCode=-1",
                                 "&proportionalRepresentationCode=-1",
                                 "&townCode=-1")
  } else if( election_code == "19971218") { ### 제15대 김대중 --------
    winner_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                                 "electionId=0000000000",
                                 "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0000000000%2Fep%2Fepei01.jsp",
                                 "&topMenuId=EP",
                                 "&secondMenuId=EPEI01",
                                 "&menuId=EPEI01",
                                 "&statementId=EPEI01_%2391",
                                 "&oldElectionType=0",
                                 "&electionType=1",
                                 "&electionName={election_code}",
                                 "&electionCode=1",
                                 "&cityCode=-1",
                                 "&proportionalRepresentationCode=-1",
                                 "&townCode=-1")
  } else if( election_code == "20021219") { ### 제16대 노무현 --------
    winner_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                                 "electionId=0000000000",
                                 "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0000000000%2Fep%2Fepei01.jsp",
                                 "&topMenuId=EP",
                                 "&secondMenuId=EPEI01",
                                 "&menuId=EPEI01",
                                 "&statementId=EPEI01_%2391",
                                 "&oldElectionType=0",
                                 "&electionType=1",
                                 "&electionName={election_code}",
                                 "&electionCode=1",
                                 "&cityCode=-1",
                                 "&proportionalRepresentationCode=-1",
                                 "&townCode=-1")
    } else {
    winner_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                                 "electionId=0000000000",
                                 "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0000000000%2Fep%2Fepei01.jsp",
                                 "&topMenuId=EP",
                                 "&secondMenuId=EPEI01",
                                 "&menuId=EPEI01",
                                 "&statementId=EPEI01_%231",
                                 "&oldElectionType=1",
                                 "&electionType=1",
                                 "&electionName={election_code}",
                                 "&electionCode=1",
                                 "&cityCode=-1",
                                 "&proportionalRepresentationCode=-1",
                                 "&townCode=-1")
  }


  winner_resp <- GET(winner_request) %>%
    content(as="text") %>%
    rvest::read_html()

  winner_tbl <- winner_resp %>%
    html_elements("#table01") %>%
    html_table() %>%
    .[[1]]

  return(winner_tbl)
}

get_winner("20220309")
get_winner("19871216")

## 1.3. 가져오기 ----------------------------------------------------

president_election_code <- c('20220309','20170509','20121219','20071219','20021219','19971218','19921218','19871216','19810225','19800827','19791206','19780706','19721223','19710427','19670503','19631015','19600315','19560515','19520805','19480720')

president_winner_raw <- president_election_code %>%
  enframe(value = "election_code") %>%
  mutate(data = map(election_code, get_winner))

president_winner <- president_winner_raw %>%
  mutate(election_code = parse_number(election_code)) %>%
  filter(election_code >= 19871216) %>%
  select(-name) %>%
  select(election_code, data) %>%
  unnest(data) %>%
  separate(`득표수(득표율)`, into = c("득표수", "득표율"), sep = "\\(") %>%
  mutate(득표수 = parse_number(득표수),
         득표율 = parse_number(득표율)) %>%
  separate(`생년월일(연령)`, into = c("생년월일", "연령"), sep = "\\(") %>%
  mutate(연령     = parse_number(연령))


## 1.4. 내보내기 ----------------------------------------------------

president_winner <- krvote::clean_varnames(president_winner)

president_winner <- make_dataframe_clean(president_winner)

usethis::use_data(president_winner, overwrite = TRUE)


