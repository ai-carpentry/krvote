
################################################################# ---
##              선관위 선거통계시스템: 지방선거                ##
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
# 1. 지방선거 ---------------------------------------------------
## 1.1. 한번 시도 ---------------------------------------------------

precinct_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                                "electionId=0000000000",
                                "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0000000000%2Fbi%2Fbigi01.jsp",
                                "&topMenuId=BI",
                                "&secondMenuId=BIGI01",
                                "&menuId=BIGI01",
                                "&statementId=BIGI01",
                                "&oldElectionType=",
                                "&electionType=4",
                                "&electionName=20220601",
                                "&electionCode=-1")


precinct_resp <- GET(precinct_request) %>%
  content(as="text") %>%
  rvest::read_html()

precinct_raw <- precinct_resp %>%
  html_elements("#table01") %>%
  html_table() %>%
  .[[1]]

## 시도지사 선거
local_sido_tbl <- precinct_raw %>%
  janitor::clean_names(ascii = FALSE) %>%
  select(contains( c("시도명", "시_도지사선거") )) %>%
  set_names(c("시도명", "선거구수", "정수")) %>%
  filter(!시도명 %in% c("시도명", "합계")) %>%
  mutate(across(선거구수:정수, parse_number))

## 구시군의 장 선거

local_sgg_tbl <- precinct_raw %>%
  janitor::clean_names(ascii = FALSE) %>%
  select(contains( c("시도명", "구_시_군의_장선거") )) %>%
  filter(!시도명 %in% c("시도명", "합계")) %>%
  set_names(c("시도명", "선거구수", "정수")) %>%
  mutate(across(선거구수:정수, parse_number)) %>%
  mutate(across(선거구수:정수, ~replace_na(.x, 0)))


## 시도의원
### 지역구
local_assmbly_01 <- precinct_raw %>%
  janitor::clean_names(ascii = FALSE) %>%
  select(contains( c("시도명", "시_도의회의원선거") )) %>%
  filter(!시도명 %in% c("시도명", "합계")) %>%
  select(contains( c("시도명", "시_도의회의원선거_3", "시_도의회의원선거_4") )) %>%
  set_names(c("시도명", "선거구수", "정수")) %>%
  mutate(across(선거구수:정수, parse_number))

### 비례
local_assmbly_02 <- precinct_raw %>%
  janitor::clean_names(ascii = FALSE) %>%
  select(contains( c("시도명", "시_도의회의원선거") )) %>%
  filter(!시도명 %in% c("시도명", "합계")) %>%
  select(contains( c("시도명", "시_도의회의원선거_5", "시_도의회의원선거_6") )) %>%
  set_names(c("시도명", "선거구수", "정수")) %>%
  mutate(across(선거구수:정수, parse_number))


## 교육감
local_edu_tbl <- precinct_raw %>%
  janitor::clean_names(ascii = FALSE) %>%
  select(contains( c("시도명", "교육감선거") )) %>%
  set_names(c("시도명", "선거구수", "정수")) %>%
  filter(!시도명 %in% c("시도명", "합계")) %>%
  mutate(across(선거구수:정수, parse_number))

# 결합

# local_8th_tbl <- tibble(
#   시도지사  = local_sido_tbl,
#   구시군장  = local_sgg_tbl,
#   시도의원1 = local_assmbly_01,
#   시도의원2 = local_assmbly_02,
#   교육감    = local_edu_tbl
# )
#
# local_8th_tbl %>%
#   select(시도지사)

category <- c("시도지사", "구시군장", "시도의원1", "시도의원2", "교육감")
data <- list(local_sido_tbl, local_sgg_tbl, local_assmbly_01, local_assmbly_02, local_edu_tbl)

local_8th_tbl <- tibble(
  type = category,
  data = data
)

## 1.2. 함수 ----------------------------------------------------

get_precinct <- function(election_code = "20220601") {
  precinct_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                                 "electionId=0000000000",
                                 "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0000000000%2Fbi%2Fbigi01.jsp",
                                 "&topMenuId=BI",
                                 "&secondMenuId=BIGI01",
                                 "&menuId=BIGI01",
                                 "&statementId=BIGI01",
                                 "&oldElectionType=",
                                 "&electionType=4",
                                 "&electionName={election_code}",
                                 "&electionCode=-1")


  precinct_resp <- GET(precinct_request) %>%
    content(as="text") %>%
    rvest::read_html()

  precinct_raw <- precinct_resp %>%
    html_elements("#table01") %>%
    html_table() %>%
    .[[1]]

  ## 시도지사 선거
  local_sido_tbl <- precinct_raw %>%
    janitor::clean_names(ascii = FALSE) %>%
    select(contains( c("시도명", "시_도지사선거") )) %>%
    set_names(c("시도명", "선거구수", "정수")) %>%
    filter(!시도명 %in% c("시도명", "합계")) %>%
    mutate(across(선거구수:정수, parse_number))

  ## 구시군의 장 선거

  local_sgg_tbl <- precinct_raw %>%
    janitor::clean_names(ascii = FALSE) %>%
    select(contains( c("시도명", "구_시_군의_장선거") )) %>%
    filter(!시도명 %in% c("시도명", "합계")) %>%
    set_names(c("시도명", "선거구수", "정수")) %>%
    mutate(across(선거구수:정수, parse_number)) %>%
    mutate(across(선거구수:정수, ~replace_na(.x, 0)))


  ## 시도의원
  ### 지역구
  local_assmbly_01 <- precinct_raw %>%
    janitor::clean_names(ascii = FALSE) %>%
    select(contains( c("시도명", "시_도의회의원선거") )) %>%
    filter(!시도명 %in% c("시도명", "합계")) %>%
    select(contains( c("시도명", "시_도의회의원선거_3", "시_도의회의원선거_4") )) %>%
    set_names(c("시도명", "선거구수", "정수")) %>%
    mutate(across(선거구수:정수, parse_number))

  ### 비례
  local_assmbly_02 <- precinct_raw %>%
    janitor::clean_names(ascii = FALSE) %>%
    select(contains( c("시도명", "시_도의회의원선거") )) %>%
    filter(!시도명 %in% c("시도명", "합계")) %>%
    select(contains( c("시도명", "시_도의회의원선거_5", "시_도의회의원선거_6") )) %>%
    set_names(c("시도명", "선거구수", "정수")) %>%
    mutate(across(선거구수:정수, parse_number))


  ## 교육감
  local_edu_tbl <- precinct_raw %>%
    janitor::clean_names(ascii = FALSE) %>%
    select(contains( c("시도명", "교육감선거") )) %>%
    set_names(c("시도명", "선거구수", "정수")) %>%
    filter(!시도명 %in% c("시도명", "합계")) %>%
    mutate(across(선거구수:정수, parse_number))

  # 결합

  category <- c("시도지사", "구시군장", "시도의원1", "시도의원2", "교육감")
  data <- list(local_sido_tbl, local_sgg_tbl, local_assmbly_01, local_assmbly_02, local_edu_tbl)

  local_precinct_tbl <- tibble(
    type = category,
    data = data
  )

  return(local_precinct_tbl)
}

get_precinct("20100602")

## 1.3. 가져오기 ----------------------------------------------------

local_election_code <- c("20220601", "20180613", "20140604", "20100602")

precinct_local_raw <- local_election_code %>%
  enframe(value = "election_code") %>%
  mutate(data = map(election_code, get_precinct))

precinct_local <- precinct_local_raw %>%
  select(-name) %>%
  mutate(name = c("제8회", "제7회", "제6회", "제5회") ) %>%
  select(name, election_code, data)

## 1.4. 내보내기 ----------------------------------------------------

precinct_local <- krvote::clean_varnames(precinct_local)

precinct_local <- precinct_local %>%
  mutate(data = map(data, krvote::clean_varnames))

usethis::use_data(precinct_local, overwrite = TRUE)


##---------------------------------------------------------------- ---
##            국회의원 HWP                       --
##---------------------------------------------------------------- ---
# 2. 국회의원 ---------------------------------------------------

library(pdftools)
library(tidyverse)
library(tabulizer)

precinct_PDF <- fs::dir_ls("inst/extdata/국회의원_선거구/", glob  = "*.pdf")

get_tables <- function(filename) {

  num_pages <- tabulizer::get_n_pages(filename)

  documents <- list()

  for(i in 1:num_pages) {
    cat("pages : ", i, "\n")
    precinct_page <- extract_tables(filename,
                                     pages = i,
                                     encoding = "UTF-8",
                                     output = "matrix",
                                     method = "lattice",
                                     guess = FALSE)
    documents[[i]] <- precinct_page
  }

  return(documents)
}

res <- get_tables(precinct_PDF[4])


res_df <- map_df(res, enframe)

res_df %>%
  unnest(value)

