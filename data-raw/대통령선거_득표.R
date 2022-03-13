
################################################################# ---
##              대통령선거 투표 데이터                         ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-03-11                       ##
################################################################# ---
## 데이터 출처: https://bit.ly/2QRqyGQ

# 대통령선거 투개표 데이터 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)
library(rvest)

##---------------------------------------------------------------- ---
##                      제19대 대통령                           --
##---------------------------------------------------------------- ---
# 1. 제19대 대통령 -----------------------------------------------

## 1.1. 원본데이터 가져오기 -------------

election_20170509_xlsx <- read_excel("inst/extdata/president/제19대선-투표구별개표자료.xlsx", sheet="19대선", skip=1) %>%
  janitor::clean_names(ascii = FALSE)

## 1.2. 데이터 정제작업 -------------

election_20170509_tbl <- election_20170509_xlsx %>%
  set_names(c("x1", "x2", "x3", "x4", "x5", "x6", "더불어민주당_문재인",
              "자유한국당_홍준표", "국민의당_안철수", "바른정당_유승민",
              "정의당_심상정", "새누리당_조원진", "경제애국당_오영국",
              "국민대통합당_장성민", "늘푸른한국당_이재오", "민중연합당_김선동",
              "한국국민당_이경희", "홍익당_윤홍식", "무소속_김민찬",
              "계", "x7", "x8")) %>%
  rename(시도명=`x1`, 구시군명=`x2`, 읍면동명=`x3`, 투표구명=`x4`, 선거인수=`x5`, 투표수=`x6`,
            문재인=`더불어민주당_문재인`,
            홍준표=`자유한국당_홍준표`,
            안철수=`국민의당_안철수`,
            유승민=`바른정당_유승민`,
            심상정=`정의당_심상정`,
            조원진=`새누리당_조원진`,
            오영국=`경제애국당_오영국`,
            장성민=`국민대통합당_장성민`,
            이재오=`늘푸른한국당_이재오`,
            김선동=`민중연합당_김선동`,
            이경희=`한국국민당_이경희`,
            윤홍식=`홍익당_윤홍식`,
            김민찬=`무소속_김민찬`,
            무효투표수=`x7`,
            기권수=`x8`) %>%
  filter(`시도명` != "전국") %>%
  filter(`구시군명` != "합계") %>%
  filter(`읍면동명` != "합계") %>%
  mutate(`투표구명` = ifelse(is.na(`투표구명`), `읍면동명`, `투표구명`)) %>% # because of NA
  filter(`투표구명` != "합계")


### 2.1.1. 투표율 ------------------------------------
election_20170509_casting <- election_20170509_tbl %>%
  select(시도명, 구시군명, 읍면동명, 투표구명, 선거인수, 투표수, 무효투표수, 기권수)

### 2.1.2. 득표율 ------------------------------------

election_20170509_voting <- election_20170509_tbl %>%
  select(시도명, 구시군명, 읍면동명, 투표구명, 선거인수, 투표수:계)


## 1.3. 단위테스트 검증 -------------

test_that("대선 2017 후보득표검증", {

  election_20170509_casting_unit_test <- election_20170509_casting %>%
    summarise(선거인수 = sum(선거인수),
                  투표수   = sum(투표수),
                  무효투표수 = sum(무효투표수))


  election_20170509_voting_unit_test <- election_20170509_voting %>%
    summarise(문재인 = sum(문재인),
                 홍준표 = sum(홍준표),
                 안철수 = sum(안철수),
                 유승민 = sum(유승민),
                 심상정 = sum(심상정))

  ## 투표율
  expect_that( election_20170509_casting_unit_test %>% pull(선거인수), equals( parse_number("42,479,710")) )
  expect_that( election_20170509_casting_unit_test %>% pull(투표수), equals( parse_number("32,807,908")) )
  expect_that( election_20170509_casting_unit_test %>% pull(무효투표수), equals( parse_number("135,733")) )

  ## 득표율
  expect_that( election_20170509_voting_unit_test$문재인, equals(13423800))
  expect_that( election_20170509_voting_unit_test$홍준표, equals(7852849))
  expect_that( election_20170509_voting_unit_test$안철수, equals(6998342))
  expect_that( election_20170509_voting_unit_test$유승민, equals(2208771))
  expect_that( election_20170509_voting_unit_test$유승민, equals(2208771))
  expect_that( election_20170509_voting_unit_test$심상정, equals(2017458))
})


## 1.4. 데이터 내보내기 -------------
### 1.4.1. 인코딩 -------------------

clean_varnames <- function(raw_data) {

  varnames <- names( raw_data )

  varnames_unicode <- map_chr(varnames, stringi::stri_escape_unicode )

  varnames_unicode_to_korean <- map_chr(varnames_unicode, stringi::stri_unescape_unicode)

  unicode_data <- raw_data %>%
    set_names(varnames_unicode_to_korean)

}

election_20170509_casting <- clean_varnames(election_20170509_casting)
election_20170509_voting  <- clean_varnames(election_20170509_voting)

### 1.4.2. 내보내기 -------------------

election_20170509 <- list( meta = list(
  title =  stringi::stri_escape_unicode("제19대 대통령선거") %>% stringi::stri_unescape_unicode(.),
  data  = stringi::stri_escape_unicode("투표구별 투표, 투표구/후보별 득표") %>% stringi::stri_unescape_unicode(.) ),
  투표율 = election_20170509_casting,
  득표율 = election_20170509_voting )

list_names <- names(election_20170509) %>%
  stringi::stri_escape_unicode(.) %>%
  stringi::stri_unescape_unicode(.)

names(election_20170509) <- list_names

usethis::use_data(election_20170509, overwrite = TRUE)



##---------------------------------------------------------------- --
##                     제18대 대통령                            --
##---------------------------------------------------------------- --

# 2. 제18대 대통령 -----------------------------------------------

## 2.1. 원본데이터 가져오기 -------------

election_20121219_xlsx <- read_excel("inst/extdata/president/제18대선-투표구별개표자료.xlsx", sheet="대통령", skip=2) %>%
  janitor::clean_names(ascii = FALSE)

## 2.2. 데이터 정제작업 -------------

election_20121219_tbl <- election_20121219_xlsx %>%
  set_names(c("시도명", "구시군명", "읍면동명", "투표구명", "선거인수", "투표수",
              "박근혜", "문재인", "박종선", "김소연", "강지원", "김순자",
              "계", "무효투표수", "기권수")) %>%
  filter(! 시도명 %in% c("전국", "시도명") )  %>%
  filter(`구시군명` != "합계") %>%
  fill(읍면동명, .direction = "down") %>%
  filter(`읍면동명` != "소계") %>%
  mutate(`투표구명` = ifelse(is.na(투표구명), `읍면동명`, `투표구명`)) %>%
  filter(투표구명 != "소계") %>%
  ## 자료형 변환
  mutate(across(선거인수:기권수, as.numeric))


## 2.1. 투표율 ------------------------------------
election_20121219_casting <- election_20121219_tbl %>%
  select(시도명, 구시군명, 읍면동명, 투표구명, 선거인수, 투표수, 무효투표수, 기권수)

## 2.2. 득표율 ------------------------------------

election_20121219_voting <- election_20121219_tbl %>%
  select(시도명, 구시군명, 읍면동명, 투표구명, 선거인수, 투표수:계)


## 2.3. 단위테스트 검증 -------------

test_that("대선 2012 후보득표검증", {

  election_20121219_casting_unit_test <- election_20121219_casting %>%
    summarise(선거인수 = sum(선거인수),
                  투표수   = sum(투표수),
                  무효투표수 = sum(무효투표수))


  election_20121219_voting_unit_test <- election_20121219_voting %>%
    summarise(박근혜 = sum(박근혜),
              문재인 = sum(문재인))

  ## 투표율
  expect_that( election_20121219_casting_unit_test %>% pull(선거인수), equals( parse_number("40,507,842 ")) )
  expect_that( election_20121219_casting_unit_test %>% pull(투표수), equals( parse_number("30,721,459")) )
  expect_that( election_20121219_casting_unit_test %>% pull(무효투표수), equals( parse_number("126,838")) )

  ## 득표율
  expect_that( election_20121219_voting_unit_test$박근혜, equals( 15773128 ))
  expect_that( election_20121219_voting_unit_test$문재인, equals( 14692632 ))
})


## 2.4. 데이터 내보내기 -------------
### 2.4.1. 인코딩 -------------------


election_20121219_casting <- clean_varnames(election_20121219_casting)
election_20121219_voting  <- clean_varnames(election_20121219_voting)

### 2.4.2. 내보내기 -------------------

election_20121219 <- list( meta = list(
  title =  stringi::stri_escape_unicode("제18대 대통령선거") %>% stringi::stri_unescape_unicode(.),
  data  = stringi::stri_escape_unicode("투표구별 투표, 투표구/후보별 득표") %>% stringi::stri_unescape_unicode(.) ),
  투표율 = election_20121219_casting,
  득표율 = election_20121219_voting )

list_names_20121219 <- names(election_20121219) %>%
  stringi::stri_escape_unicode(.) %>%
  stringi::stri_unescape_unicode(.)

names(election_20121219) <- list_names_20121219

usethis::use_data(election_20121219, overwrite = TRUE)


##---------------------------------------------------------------- ---
##                      제20대 대통령                           --
##---------------------------------------------------------------- ---
# 3. 제20대 대통령 -----------------------------------------------
## 3.1. 스크립트 -------------------------------------------------
### 데이터 가져오기
presid_20_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
           "electionId=0020220309&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0020220309%2Fvc%2Fvccp08.jsp&topMenuId=VC&secondMenuId=VCCP08&menuId=VCCP08&statementId=VCCP08_%231&electionCode=1&cityCode=1100&sggCityCode=-1&townCodeFromSgg=-1&townCode=1101&sggTownCode=-1&checkCityCode=-1&x=55&y=18")

Sys.setlocale("LC_ALL", "C")
presid_20_resp <- GET(presid_20_request) %>%
  content(as="text") %>%
  rvest::read_html()

presid_20_raw <- presid_20_resp %>%
  html_elements(css ="#table01") %>%
  html_table(fill = TRUE) %>%
  .[[1]]
Sys.setlocale("LC_ALL", "Korean")

### 데이터 정제

presid_20_colnames <- presid_20_raw %>%
  janitor::clean_names(ascii = FALSE) %>%
  slice(1) %>%
  unlist() %>%
  as.character()


presid_20_tbl <- presid_20_raw %>%
  janitor::clean_names(ascii = FALSE) %>%
  set_names(presid_20_colnames) %>%
  mutate(읍면동명 = ifelse(읍면동명 == "", NA_character_, 읍면동명)) %>%
  fill(읍면동명, .direction = "down") %>%
  mutate(투표구명 = ifelse(투표구명 == "", 읍면동명, 투표구명)) %>%
  slice(2:n()) %>%
  mutate(across(선거인수:기권수, parse_number) )

presid_20_tbl

## 3.2. 함수 -------------------------------------------------

get_president_2022 <- function(sido_code = "1100", sgg_code = "1101") {

  cat("\n----------------------------------\n", sido_code, ":", sgg_code, "\n")

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
                                  "&townCode={sgg_code}",
                                  "&sggTownCode=-1",
                                  "&checkCityCode=-1&x=55&y=18")

  Sys.setlocale("LC_ALL", "C")
  presid_20_resp <- GET(presid_20_request) %>%
    content(as="text") %>%
    rvest::read_html()

  presid_20_raw <- presid_20_resp %>%
    html_elements(css ="#table01") %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  Sys.setlocale("LC_ALL", "Korean")

  ### 데이터 정제

  presid_20_colnames <- presid_20_raw %>%
    janitor::clean_names(ascii = FALSE) %>%
    slice(1) %>%
    unlist() %>%
    as.character()


  presid_20_tbl <- presid_20_raw %>%
    janitor::clean_names(ascii = FALSE) %>%
    set_names(presid_20_colnames) %>%
    mutate(읍면동명 = ifelse(읍면동명 == "", NA_character_, 읍면동명)) %>%
    fill(읍면동명, .direction = "down") %>%
    mutate(투표구명 = ifelse(투표구명 == "", 읍면동명, 투표구명)) %>%
    slice(2:n()) %>%
    mutate(across(선거인수:기권수, parse_number) )

  presid_20_tbl

}

get_president_2022("1100", "1102")

## 3.3. 가져오기 -------------------------------------------------

election_20220309_raw <- krvote::nec_sgg_code %>%
  unnest(data) %>%
  mutate(data = map2(시도코드, 구시군코드, get_president_2022) ) %>%
  unnest()

### 3.3.1. 투표율 ------------------------------------
election_20220309_casting <- election_20220309_raw %>%
  filter(읍면동명 != "합계",
         투표구명 != "소계") %>%
  select(시도명, 구시군명=구시군, 읍면동명, 투표구명, 선거인수, 투표수, 무효투표수, 기권수)

### 3.3.2. 득표율 ------------------------------------
election_20220309_voting <- election_20220309_raw %>%
  filter(읍면동명 != "합계",
             투표구명 != "소계") %>%
  select(시도명, 구시군명=구시군, 읍면동명, 투표구명, 선거인수, 투표수:계)


## 3.4. 단위테스트 검증 -------------

test_that("대선 2022 후보득표검증", {

  election_20220309_casting_unit_test <- election_20220309_casting %>%
    summarise(선거인수 = sum(선거인수),
              투표수   = sum(투표수),
              무효투표수 = sum(무효투표수))


  election_20220309_voting_unit_test <- election_20220309_voting %>%
    summarise(이재명 = sum(더불어민주당이재명),
              윤석열 = sum(국민의힘윤석열))

  ## 투표율
  expect_that( election_20220309_casting_unit_test %>% pull(선거인수), equals( parse_number("44,197,692")) )
  expect_that( election_20220309_casting_unit_test %>% pull(투표수), equals( parse_number("34,067,853")) )
  expect_that( election_20220309_casting_unit_test %>% pull(무효투표수), equals( parse_number("307,542")) )

  ## 득표율
  expect_that( election_20220309_voting_unit_test$윤석열, equals( parse_number("16,394,815") ))
  expect_that( election_20220309_voting_unit_test$이재명, equals( parse_number("16,147,738") ))
})


## 3.4. 데이터 내보내기 -------------
### 3.4.1. 인코딩 -------------------

election_20220309_casting <- krvote::clean_varnames(election_20220309_casting)
election_20220309_voting  <- krvote::clean_varnames(election_20220309_voting)

### 3.4.2. 내보내기 -------------------

election_20220309 <- list( meta = list(
  title =  stringi::stri_escape_unicode("제20대 대통령선거") %>% stringi::stri_unescape_unicode(.),
  data  = stringi::stri_escape_unicode("투표구별 투표, 투표구/후보별 득표") %>% stringi::stri_unescape_unicode(.) ),
  투표율 = election_20220309_casting,
  득표율 = election_20220309_voting )

list_names <- names(election_20220309) %>%
  stringi::stri_escape_unicode(.) %>%
  stringi::stri_unescape_unicode(.)

names(election_20220309) <- list_names

usethis::use_data(election_20220309, overwrite = TRUE)

