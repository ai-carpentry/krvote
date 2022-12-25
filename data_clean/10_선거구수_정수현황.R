
################################################################# ---
##              선관위 선거통계시스템                          ##
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
# 1. 한번 시도 ---------------------------------------------------

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

# 2. 시군구 코드 ------------------------------------------------
## 2.1. 함수 ----------------------------------------------------

get_sgg_code <- function(sido_code = "1100") {
  presid_20_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                                  "electionId=0020220309",
                                  "&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0020220309%2Fvc%2Fvccp08.jsp",
                                  "&topMenuId=VC",
                                  "&secondMenuId=VCCP08",
                                  "&menuId=VCCP08",
                                  "&statementId=VCCP08_%231",
                                  "&electionCode=1",
                                  "&cityCode={sido_code}",
                                  "&sggCityCode=-1",
                                  "&townCodeFromSgg=-1",
                                  "&sggTownCode=-1",
                                  "&checkCityCode=-1&x=55&y=18")

  presid_20_resp <- GET(presid_20_request) %>%
    content(as="text") %>%
    rvest::read_html()

  sgg_text <- presid_20_resp %>%
    html_elements(css ="#townCode") %>%
    html_elements("option") %>%
    html_text()

  sgg_value <- presid_20_resp %>%
    html_elements(css ="#townCode") %>%
    html_elements("option") %>%
    html_attr("value")

  nec_sgg_code <- tibble(구시군 = sgg_text, 구시군코드 = sgg_value) %>%
    filter(구시군코드 != "-1")

  nec_sgg_code
}

get_sgg_code("2600")

## 2.2. 가져오기 ----------------------------------------------------

nec_sgg_code <- nec_sido_code %>%
  mutate(data = map(시도코드, get_sgg_code))

## 2.3. 내보내기 ----------------------------------------------------

nec_sgg_code <- krvote::clean_varnames(nec_sgg_code)

nec_sgg_code <- nec_sgg_code %>%
  mutate(data = map(data, krvote::clean_varnames))

usethis::use_data(nec_sgg_code, overwrite = TRUE)




