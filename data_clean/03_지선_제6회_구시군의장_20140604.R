
################################################################# ---
##             엑셀파일 유권자 투개표 : 지방선거/구시군의장    ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-03-14                       ##
################################################################# ---


# 제6회 지방선거 - 구시군의장 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)


##---------------------------------------------------------------- --
##                    제6회 지방선거 구시군의장 (2014)             --
##---------------------------------------------------------------- --
# 출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=14979
# 1. 경기도 ---------------------------
## 1.1. 데이터 가져오기 ---------------------------
sgg_gg_xls_path <- "../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(경기)/읍면동별개표결과-구시군의장"

sgg_gg_xls_path_files <- list.files(sgg_gg_xls_path, full.names = TRUE, recursive = TRUE)


sgg_gg_xls_raw <- tibble(filepath = sgg_gg_xls_path_files) %>%
  mutate(data = map(filepath, readxl::read_excel, skip = 3) ) %>%
  separate(filepath, into = c("trash", "엑셀파일"), sep = "읍면동별개표결과-구시군의장/") %>%
  select(-trash) %>%
  mutate(엑셀파일 = str_remove(엑셀파일, "\\.xls")) %>%
  separate(엑셀파일, into = c("구시군", "선거구"), sep = "/") %>%
  mutate(선거구 = ifelse(is.na(선거구), 구시군, 선거구))

## 1.2. 데이터 정제 ---------------------------

clean_sgg_data <- function(gusigun, precinct, raw_data) {

  cat("\n---------------------------------\n", gusigun, ":", precinct, "\n")

  candidate_name <- raw_data %>%
    select(grep("[ㄱ-힗]", names(raw_data), value = TRUE)) %>%
    names %>% setdiff(., "계") %>%
    str_replace_all(., "\r\n", " ")

  column_names <- c("읍면동명","구분","선거인수","투표수", candidate_name, "계", "무효투표수", "기권수")

  clean_data <- raw_data %>%
    set_names(column_names) %>%
    select(-contains("party")) %>%
    filter(row_number() != 1) %>%
    mutate(읍면동명 = zoo::na.locf(읍면동명)) %>% # 동별 NA 값 채워넣기
    filter(읍면동명 !="합계") %>%
    mutate(`구분` = ifelse(is.na(`구분`), `읍면동명`, `구분`)) %>%
    filter(`구분` !="소계") %>%
    mutate(across(선거인수:계, parse_number)) %>%
    mutate(무효투표수 = as.numeric(무효투표수),
                기권수 = as.numeric(기권수))

  clean_data
}


sgg_gg_tbl <- sgg_gg_xls_raw %>%
  mutate(data = pmap(list(구시군, 선거구, data), clean_sgg_data)) %>%
  mutate(시도명 = "경기도") %>%
  relocate(시도명, .before = 구시군)

## 1.3. 단위테스트 검증 -------------

test_that("지선 2018 구시군의장 득표검증", {

  sgg_gg_tbl_cast <- sgg_gg_tbl %>%
    filter(str_detect(구시군, "성남")) %>%
    unnest(data) %>%
    summarise(선거인수 = sum(선거인수),
                  투표수   = sum(투표수),
                  무효투표수 = sum(무효투표수))


  sgg_gg_tbl_vote <- sgg_gg_tbl %>%
    filter(str_detect(구시군, "성남")) %>%
    unnest(data) %>%
    summarise(`새누리당\n신영수` = sum(`새누리당\n신영수`),
              `새정치민주연합\n이재명` = sum(`새정치민주연합\n이재명`),
              `새정치당\n허재안` = sum(`새정치당\n허재안`))

  ## 투표율
  expect_that( sgg_gg_tbl_cast %>% pull(선거인수), equals( parse_number("793,411")) )
  expect_that( sgg_gg_tbl_cast %>% pull(투표수), equals( parse_number("448,996")) )
  expect_that( sgg_gg_tbl_cast %>% pull(무효투표수), equals( parse_number("13,661	")) )

  ## 득표율
  expect_that( sgg_gg_tbl_vote$`새누리당\n신영수`, equals(191749))
  expect_that( sgg_gg_tbl_vote$`새정치민주연합\n이재명`, equals(239685	))
  expect_that( sgg_gg_tbl_vote$`새정치당\n허재안`, equals(3901))
})


## 1.4. 내보내기 -------------------

local_sgg_20140604 <- list( meta = list(
  title =  stringi::stri_escape_unicode("제6회 지방선거 - 구시군의장") %>% stringi::stri_unescape_unicode(.),
  data  = stringi::stri_escape_unicode("투표구별 투표, 투표구/후보별 득표") %>% stringi::stri_unescape_unicode(.) ),
  경기도 = sgg_gg_tbl)

list_names <- names(local_sgg_20140604) %>%
  stringi::stri_escape_unicode(.) %>%
  stringi::stri_unescape_unicode(.)

names(local_sgg_20140604) <- list_names

usethis::use_data(local_sgg_20140604, overwrite = TRUE)

# 2. 서울특별시 ---------------------------
## 2.1. 데이터 가져오기 ---------------------------
sgg_seoul_xls_path <- "../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(서울)/02_구시군장/"

sgg_seoul_xls_path_files <- list.files(sgg_seoul_xls_path, full.names = TRUE, recursive = TRUE)


sgg_seoul_xls_raw <- tibble(filepath = sgg_seoul_xls_path_files) %>%
  mutate(data = map(filepath, readxl::read_excel, skip = 3) ) %>%
  separate(filepath, into = c("trash", "엑셀파일"), sep = "/02_구시군장/") %>%
  select(-trash) %>%
  mutate(엑셀파일 = str_remove(엑셀파일, "\\.xls")) %>%
  mutate(구시군명 = str_extract(엑셀파일, "[가-힣]+")) %>%
  mutate(시도명 = "서울특별시") %>%
  select(시도명, 구시군명, data)

## 2.2. 데이터 정제 ---------------------------

clean_gu_2014_data <- function(raw_data) {

  raw_data <- raw_data %>%
    janitor::clean_names(ascii = FALSE)

  candidate_names <- raw_data %>%
    select(!starts_with("x")) %>%
    names(.)

  column_names <- c("읍면동명", "구분", "선거인수", "투표수", candidate_names, "무표투표수", "기권수")

  clean_data <- raw_data %>%
    set_names(column_names) %>%
    mutate(구분 = ifelse(is.na(구분), 읍면동명, 구분)) %>%
    filter(읍면동명 != "합계",
               구분 != "소계") %>%
    mutate(across(선거인수:기권수, as.character)) %>%
    pivot_longer(선거인수:기권수, names_to = "후보", values_to = "득표수") %>%
    mutate(득표수 = parse_number(득표수))

  clean_data
}

sgg_seoul_tbl <- sgg_seoul_xls_raw %>%
  mutate(data = map(data, clean_gu_2014_data))

## 2.3. 단위테스트 검증 -------------

test_that("지선 2014 구시군의장 득표검증", {

  sgg_seoul_unit_test <- sgg_seoul_tbl %>%
    filter(str_detect(구시군명, "강남구")) %>%
    unnest(data) %>%
    group_by(후보) %>%
    summarise(득표수 = sum(득표수))



  ## 투표율
  expect_that( sgg_seoul_unit_test %>% filter(후보 == "새누리당_신연희") %>% pull(득표수), equals( parse_number("163,037")) )
  expect_that( sgg_seoul_unit_test %>% filter(후보 == "새정치민주연합_김명신") %>% pull(득표수), equals( parse_number("94,164")) )
})


## 2.4. 내보내기 -------------------

local_sgg_20140604 <- list( meta = list(
  title =  stringi::stri_escape_unicode("제6회 지방선거 - 구시군의장") %>% stringi::stri_unescape_unicode(.),
  data  = stringi::stri_escape_unicode("투표구별 투표, 투표구/후보별 득표") %>% stringi::stri_unescape_unicode(.) ),
  경기도 = sgg_gg_tbl,
  서울특별시 = sgg_seoul_tbl)

list_names <- names(local_sgg_20140604) %>%
  stringi::stri_escape_unicode(.) %>%
  stringi::stri_unescape_unicode(.)

names(local_sgg_20140604) <- list_names

usethis::use_data(local_sgg_20140604, overwrite = TRUE)




