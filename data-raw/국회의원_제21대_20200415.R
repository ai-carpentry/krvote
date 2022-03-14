
################################################################# ---
##             엑셀파일 유권자 투개표 : 국회의원               ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-03-13                       ##
################################################################# ---


# 제21대 국회의원 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)

##---------------------------------------------------------------- --
##                    제21대 국회의원                           --
##---------------------------------------------------------------- --
# 1. 데이터 -------------
# 출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=15052
## 1.1. 지역구 한곳 -----
### 후보정당과 후보 변수명 처리 자동화
var_names <- c("읍면동명","투표구명","선거인수","투표수",
               paste0("party_", seq(1:50)), "계", "무표투표수", "기권수")

one_dat <- read_excel("../../docs/krvotes/data-raw/제21대 국회의원선거 투표구별 개표결과/지역구/9경기/개표상황(투표구별)_성남시분당구을.xlsx", skip=4)

candidate_name <- one_dat %>%
  select(grep("[ㄱ-힗]", names(one_dat), value = TRUE)) %>%
  names %>% setdiff(., "계") %>%
  str_replace_all(., "\r\n", " ")

column_names <- c("읍면동명","투표구명","선거인수","투표수", candidate_name,
                  paste0("party_", seq(1:(50-length(candidate_name)))), "계", "무표투표수", "기권수")

### 함수 --------------------
extract_colnames_from_excel <- function(excel_file="../../docs/krvotes/data-raw/제21대 국회의원선거 투표구별 개표결과/지역구/9경기/개표상황(투표구별)_성남시분당구을.xlsx") {

  one_dat <- read_excel(excel_file, skip=4)

  candidate_name <- one_dat %>%
    select(grep("[ㄱ-힗]", names(one_dat), value = TRUE)) %>%
    names %>% setdiff(., "계") %>%
    str_replace_all(., "\r\n", " ")

  column_names <- c("읍면동명","투표구명","선거인수","투표수", candidate_name,
                    paste0("party_", seq(1:(50-length(candidate_name)))), "계", "무표투표수", "기권수")

  column_names
}

extract_colnames_from_excel()

### 데이터 정리

one_dat <- read_excel("../../docs/krvotes/data-raw/제21대 국회의원선거 투표구별 개표결과/지역구/9경기/개표상황(투표구별)_성남시분당구을.xlsx", sheet="2020년 제21대 국회의원선거", skip=4)

names(one_dat) <- enc2native(names(one_dat))

one_df <- one_dat %>%
  set_names(column_names) %>%
  select(-contains("party")) %>%
  filter(row_number() != 1) %>%
  mutate(읍면동명 = zoo::na.locf(읍면동명)) %>% # 동별 NA 값 채워넣기
  filter(읍면동명 !="합계") %>%
  mutate(`투표구명` = ifelse(is.na(`투표구명`), `읍면동명`, `투표구명`)) %>%
  filter(`투표구명` !="소계")

### 함수

make_dataframe_from_excel <- function(excel_file="../../docs/krvotes/data-raw/제21대 국회의원선거 투표구별 개표결과/지역구/9경기/개표상황(투표구별)_성남시분당구을.xlsx") {

  excel_colnames <- extract_colnames_from_excel(excel_file)

  one_dat <- read_excel(excel_file, skip=4)

  one_df <- one_dat %>%
    set_names(excel_colnames) %>%
    select(-contains("party")) %>%
    filter(row_number() != 1) %>%
    mutate(읍면동명 = zoo::na.locf(읍면동명)) %>% # 동별 NA 값 채워넣기
    filter(읍면동명 !="합계") %>%
    mutate(`투표구명` = ifelse(is.na(`투표구명`), `읍면동명`, `투표구명`)) %>%
    filter(`투표구명` !="소계")

  one_df
}

make_dataframe_from_excel()


### 데이터 정합성 확인

test_that("국회선거 성남시 분당구을  2020 후보득표검증", {

  one_df_check <- make_dataframe_from_excel() %>%
    summarise(`더불어민주당 김병욱` = sum(`더불어민주당 김병욱`),
              `미래통합당 김민수`     = sum(`미래통합당 김민수`),
              `정의당 양호영`     = sum(`정의당 양호영`),
              `무소속 이나영`       = sum(`무소속 이나영`))

  expect_that( one_df_check$`더불어민주당 김병욱`, equals(68387))
  expect_that( one_df_check$`미래통합당 김민수`,   equals(64342))
  expect_that( one_df_check$`정의당 양호영`,       equals(3021))
  expect_that( one_df_check$`무소속 이나영`,       equals(5662))
})

## 1.2. 지역구: 경기도 -----
### 1.2.1. 디렉토리 파일명 생성
gg_dir_names <- list.dirs("../../docs/krvotes/data-raw/제21대 국회의원선거 투표구별 개표결과/지역구/9경기/")

gg_file_names <- list.files(gg_dir_names) %>%
  str_extract(., "^(?!\\~).*")  # 임시 엑셀파일 제거
gg_file_names <- gg_file_names[!is.na(gg_file_names)]

gg_dir_file_names <- glue::glue("{gg_dir_names}{gg_file_names}")

### 1.2.2. 경기도 코드테이블

gg_codetable <- tibble(
  sido = gg_dir_names,
  precinct = gg_file_names,
  input_file_name = gg_dir_file_names
)

gg_dat <- gg_codetable %>%
  mutate(sido = str_extract(sido, "/([0-9])[ㄱ-흫].+/$") %>% str_remove_all(., "/|([0-9])")) %>%
  mutate(precinct = str_extract(precinct, "_[ㄱ-흫].+\\.") %>% str_remove_all(., "_|\\."))

## `purrr` 스타일 ------------

gg_raw <- gg_dat %>%
  mutate(data = map(input_file_name, make_dataframe_from_excel))

make_dataframe_clean <- function(raw_df) {

  clean_df <- raw_df %>%
    pivot_longer(선거인수:기권수, names_to = "구분", values_to="사람수")

  clean_df
}

gg_df <- gg_raw %>%
  rename(시도=sido, 선거구 = precinct) %>%
  select(-input_file_name) %>%
  mutate(data_clean = map(data, make_dataframe_clean))

### 데이터 정합성 확인

gg_df %>%
  filter(시도 == "경기" & 선거구 == "성남시분당구을") %>%
  pull(data_clean) %>% .[[1]] %>%
  group_by(구분) %>%
  summarise(득표수 = sum(사람수)) %>%
  pivot_wider(names_from = 구분, values_from = 득표수)

test_that("국회선거 경기도 2020 성남시분당구을 후보득표검증", {

  gg_check_df <- gg_df %>%
    filter(시도 == "경기" & 선거구 == "성남시분당구을") %>%
    pull(data_clean) %>% .[[1]] %>%
    group_by(구분) %>%
    summarise(득표수 = sum(사람수)) %>%
    pivot_wider(names_from = 구분, values_from = 득표수)

  expect_that( gg_check_df$`더불어민주당 김병욱`, equals(68387))
  expect_that( gg_check_df$`미래통합당 김민수`,   equals(64342))
  expect_that( gg_check_df$`정의당 양호영`,       equals(3021))
  expect_that( gg_check_df$`무소속 이나영`,       equals(5662))
})

test_that("국회선거 경기도 2020 안산시상록구을 후보득표검증", {

  gg_check_tbl <- gg_df %>%
    filter(시도 == "경기" & 선거구 == "안산시상록구을") %>%
    pull(data_clean) %>% .[[1]] %>%
    group_by(구분) %>%
    summarise(득표수 = sum(사람수)) %>%
    pivot_wider(names_from = 구분, values_from = 득표수)

  expect_that( gg_check_tbl$`더불어민주당 김철민`, equals(43599))
  expect_that( gg_check_tbl$`미래통합당 홍장표`,   equals(30747))
})


## 1.3. 지역구: 전국 -----
### 디렉토리 파일명 생성
congress_dir_file_names <- list()

congress_dir_names <- list.dirs("../../docs/krvotes/data-raw/제21대 국회의원선거 투표구별 개표결과/지역구/")

congress_dir_names <- congress_dir_names[-1] # 자기자신 디렉토리 제거

for(i in 1:length(congress_dir_names)) {
  cat(i, ":", congress_dir_names[i], "\n")
  congress_file_names <- list.files(congress_dir_names[[i]]) %>%
    str_extract(., "^(?!\\~).*")  # 임시 엑셀파일 제거
  congress_file_names <- congress_file_names[!is.na(congress_file_names)]

  congress_dir_file_names[[i]] <- enc2native(paste0(congress_dir_names[[i]],"/", congress_file_names))
  cat(i, ":", congress_dir_file_names[[i]], "\n")
}

### 전국 투표데이터 가져오기
#### 전국 선거구 데이터 프레임작성
congress_dat <- tibble(
  input_file_name = congress_dir_file_names %>% unlist
)

congress_dat <- congress_dat %>%
  separate(input_file_name, into=c("시도", "선거구"), "\\/개표상황\\(투표구별\\)_", remove = FALSE) %>%
  mutate(시도 = str_extract(시도, "/\\d+[ㄱ-힗].+$") %>% str_remove_all(., "/|([0-9])")) %>%
  mutate(선거구 = str_remove(선거구, "\\.xlsx")) %>%
  select(시도, 선거구, input_file_name)

#### 전국 선거구별 개표결과 데이터 생성

congress_raw <- congress_dat %>%
  mutate(data = map(input_file_name, make_dataframe_from_excel))


general_2020 <- congress_raw %>%
  select(-input_file_name) %>%
  mutate(data = map(data, make_dataframe_clean))



### 데이터 정합성 확인
test_that("국회선거 2020 성남시분당구을 후보득표검증", {

  congress_check_df <-  general_2020 %>%
    filter(시도 == "경기" & 선거구 == "성남시분당구을") %>%
    pull(data) %>% .[[1]] %>%
    group_by(구분) %>%
    summarise(득표수 = sum(사람수)) %>%
    pivot_wider(names_from = 구분, values_from = 득표수)

  expect_that( congress_check_df$`더불어민주당 김병욱`, equals(68387))
  expect_that( congress_check_df$`미래통합당 김민수`,   equals(64342))
  expect_that( congress_check_df$`정의당 양호영`,       equals(3021))
  expect_that( congress_check_df$`무소속 이나영`,       equals(5662))
})

## 1.4. 내보내기 ---------------------

general_2020 <- krvote::clean_varnames(general_2020)

general_2020 <- general_2020 %>%
  mutate(data = map(data, krvote::clean_varnames))

usethis::use_data(general_2020, overwrite = TRUE)

