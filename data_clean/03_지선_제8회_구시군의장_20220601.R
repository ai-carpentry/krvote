
################################################################# ---
##             엑셀파일 유권자 투개표 : 지방선거/구시군의장    ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-12-25                       ##
################################################################# ---


# 제8회 지방선거 - 구시군의장 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)

local_sgg_raw %>%
  filter( str_detect(구시군명, "수원시|성남시|안양시|고양시|안산시|용인시|청주시|천안시|전주시|포항시|창원시")) %>%
  unnest(data) %>%
  janitor::clean_names(ascii = FALSE) %>%
  names()


##---------------------------------------------------------------- --
##                    제8회 지방선거 구시군의장 (2022)             --
##---------------------------------------------------------------- --
# 출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=187608
# 1. 데이터 가져오기 ---------------------------
local_sgg_excel <- read_excel("Z:/dataset/선거데이터/01_대선_총선_지선_데이터/03_지방선거/제8회_지선_2022/구시군장선거.xlsx", sheet="구·시·군의장선거", skip=0,
                             col_types = c(rep("text", 15)))


local_sgg_raw <- local_sgg_excel %>%
  group_by(시도명, 구시군명, `선거구(구시군)`) %>%
  nest() %>%
  ungroup() %>%
  filter(!is.na(시도명))

## 1.1. 변수명 정제 ---------------------------
common_colnames <- local_sgg_raw %>%
  janitor::clean_names(ascii = FALSE) %>%
  unnest(cols = data) %>%
  janitor::clean_names(ascii = FALSE) %>%
  select(!matches("^x|후보자별_득표수")) %>%
  names(.)

hubo_colnames <- local_sgg_raw %>%
  janitor::clean_names(ascii = FALSE) %>%
  unnest(cols = data) %>%
  janitor::clean_names(ascii = FALSE) %>%
  slice(1) %>%
  pivot_longer(cols = everything()) %>%
  filter(str_detect(name, "^x|후보자별_득표수")) %>%
  pull(value) %>% dput()

sgg_colnames <- c(common_colnames[1:7], hubo_colnames, common_colnames[8:length(common_colnames)])

## 1.1. (함수)변수명 정제 ---------------------------

get_sgg_colnames <- function(raw_df) {

  common_colnames <- raw_df %>%
    janitor::clean_names(ascii = FALSE) %>%
    select(!matches("^x|후보자별_득표수")) %>%
    names(.)

  hubo_colnames <- raw_df %>%
    janitor::clean_names(ascii = FALSE) %>%
    slice(1) %>%
    pivot_longer(cols = everything()) %>%
    filter(str_detect(name, "^x|후보자별_득표수")) %>%
    pull(value) %>% dput()

  sgg_colnames <- c(common_colnames[1:4], hubo_colnames, common_colnames[5:length(common_colnames)])

  return(sgg_colnames)
}

get_sgg_colnames(local_sgg_raw$data[[1]])

## 1.2. 데이터 정제 ---------------------------

tmp_colnames <- get_sgg_colnames(local_sgg_raw$data[[1]])

local_sgg_raw$data[[1]] %>%
  set_names(tmp_colnames) %>%
  janitor::clean_names(ascii = FALSE) %>%
  select(!contains("na")) %>%
  filter(!is.na(읍면동명),
         !str_detect(읍면동명, "합계")) %>%
  mutate(구분 = ifelse(is.na(구분), 읍면동명, 구분)) %>%
  filter(구분 != "소계") %>%
  select(-계) %>%
  pivot_longer(cols = contains("_"), names_to = "정당_후보", values_to = "득표") %>%
  separate(정당_후보, into = c("정당", "후보"), sep = "_")

## 1.2. (함수) 데이터 정제 ---------------------------

tidy_rawdata <- function(raw_df) {

  tmp_colnames <- get_sgg_colnames(raw_df)

  tidy_data <- raw_df %>%
    set_names(tmp_colnames) %>%
    janitor::clean_names(ascii = FALSE) %>%
    select(!contains("na")) %>%
    filter(!is.na(읍면동명),
           !str_detect(읍면동명, "합계")) %>%
    mutate(구분 = ifelse(is.na(구분), 읍면동명, 구분)) %>%
    filter(구분 != "소계") %>%
    select(-계) %>%
    pivot_longer(cols = contains("_"), names_to = "정당_후보", values_to = "득표") %>%
    separate(정당_후보, into = c("정당", "후보"), sep = "_")

  return(tidy_data)
}


tidy_rawdata(local_sgg_raw$data[[1]])

## 1.3. 시군구 데이터 정제----------------

local_sgg_untidy <- local_sgg_raw %>%
  mutate(tidy_data = map(data, tidy_rawdata))

local_sgg_20220601 <- local_sgg_untidy %>%
  janitor::clean_names(ascii=FALSE) %>%
  select(-data) %>%
  unnest(tidy_data)

# local_sgg_tbl %>%
#   filter(str_detect(구시군명, "속초")) %>%
#   mutate(득표 = parse_number(득표)) %>%
#   group_by(시도명, 구시군명, 정당) %>%
#   summarise(득표 = sum(득표))

# 2. 내보내기 -------------

local_sgg_20220601 <- krvote::clean_varnames(local_sgg_20220601)

local_sgg_20220601 <- krvote::make_dataframe_clean(local_sgg_20220601)

usethis::use_data(local_sgg_20220601, overwrite = TRUE)
