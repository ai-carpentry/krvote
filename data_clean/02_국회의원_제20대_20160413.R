
################################################################# ---
##             엑셀파일 유권자 투개표 : 국회의원               ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-03-13                       ##
################################################################# ---


# 제20대 국회의원 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)


##---------------------------------------------------------------- --
##                    제20대 국회의원 (2016)                       --
##---------------------------------------------------------------- --
# 자료출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=14879
# 1. 함수 (2020에서 가져옮) --------------------------------------
## 변수명 추출함수 (2020년과 변수명에서 차이가 남)
extract_colnames_from_excel_2016 <- function(excel_file="../../docs/krvotes/data-raw/제21대 국회의원선거 투표구별 개표결과/지역구/9경기/개표상황(투표구별)_성남시분당구을.xlsx") {

  one_dat <- read_excel(excel_file, skip=4)

  candidate_name <- one_dat %>%
    select(grep("[ㄱ-힗]", names(one_dat), value = TRUE)) %>%
    names %>% setdiff(., "계") %>%
    str_replace_all(., "\r\n", " ")

  column_names <- c("읍면동명","투표구명","선거인수","투표수", candidate_name,
                    paste0("party_", seq(1:(21-length(candidate_name)))), "계", "무표투표수", "기권수")

  column_names
}

extract_colnames_from_excel_2016(general_2016_dat$input_file_name[[1]])

## 엑셀 --> 데이터프레임 변환
make_dataframe_from_excel_2016 <- function(excel_file="../../docs/krvotes/data-raw/제21대 국회의원선거 투표구별 개표결과/지역구/9경기/개표상황(투표구별)_성남시분당구을.xlsx") {

  excel_colnames <- extract_colnames_from_excel_2016(excel_file)

  one_dat <- read_excel(excel_file, sheet="sheet1", skip=4)

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

make_dataframe_from_excel_2016(general_2016_dat$input_file_name[[2]])

# 2. 지역구: 전국 -----
# 2.1. 디렉토리 파일명 생성 ------------------
general_2016_dir_file_names <- list()

general_2016_dir_names <- list.dirs("../../docs/krvotes/data-raw/제20대 국회의원선거 투표구별 개표결과/지역구")

general_2016_dir_names <- general_2016_dir_names[-1] # 자기자신 디렉토리 제거

for(i in 1:length(general_2016_dir_names)) {
  cat(i, ":", general_2016_dir_names[i], "\n")
  congress_file_names <- list.files(general_2016_dir_names[[i]]) %>%
    str_extract(., "^(?!\\~).*")  # 임시 엑셀파일 제거
  congress_file_names <- congress_file_names[!is.na(congress_file_names)]

  general_2016_dir_file_names[[i]] <- enc2native(paste0(general_2016_dir_names[[i]],"/", congress_file_names))
  cat(i, ":", general_2016_dir_file_names[[i]], "\n")
}

## 2.2. 전국 선거구 ---------------
general_2016_dat <- tibble(
  input_file_name = general_2016_dir_file_names %>% unlist) %>%
  separate(input_file_name, into=c("시도", "선거구"), "\\/개표상황\\(투표구별\\)_", remove = FALSE) %>%
  mutate(시도 = str_extract(시도, "/\\d+[ㄱ-힗].+$") %>% str_remove_all(., "/|([0-9])")) %>%
  mutate(선거구 = str_remove(선거구, "\\.xlsx")) %>%
  select(시도, 선거구, input_file_name)

### 전국 선거구별 개표결과 데이터 생성

general_2016_raw <- general_2016_dat %>%
  mutate(data = map(input_file_name, make_dataframe_from_excel_2016))


general_2016 <- general_2016_raw %>%
  select(-input_file_name) %>%
  mutate(data = map(data, make_dataframe_clean)) #krvote::make_dataframe_clean


## 2.3 데이터 정합성 확인 ------------------
test_that("국회선거 2016 성남시분당구을 후보득표검증", {

  one_df_check <- general_2016 %>%
    filter(시도 == "경기" & 선거구 == "성남시분당구을") %>%
    pull(data) %>% .[[1]] %>%
    group_by(구분) %>%
    summarise(득표수 = sum(사람수)) %>%
    pivot_wider(names_from = 구분, values_from = 득표수)

  expect_that( one_df_check$`더불어민주당 김병욱`, equals(50661))
  expect_that( one_df_check$`새누리당 전하진`,     equals(39367))
  expect_that( one_df_check$`국민의당 윤은숙`,     equals(11936))
  expect_that( one_df_check$`무소속 임태희`,       equals(23921))
})

## 2.4. 내보내기 ---------------------

general_2016 <- krvote::clean_varnames(general_2016)

general_2016 <- general_2016 %>%
  mutate(data = map(data, krvote::clean_varnames))

usethis::use_data(general_2016, overwrite = TRUE)










