
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


##---------------------------------------------------------------- --
##                    제8회 지방선거 구시군의장 (2022)             --
##---------------------------------------------------------------- --
# 출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=187608
# 1. 데이터 가져오기 ---------------------------
local_sgg_excel <- read_excel("Z:/dataset/선거데이터/01_대선_총선_지선_데이터/03_지방선거/제8회_지선_2022/구시군장선거.xlsx", sheet="구·시·군의장선거", skip=0,
                             col_types = c(rep("text", 15)))


local_sgg_raw <- local_sgg_excel %>%
  group_by(시도명, `선거구(구시군)`) %>%
  nest() %>%
  ungroup() %>%
  filter(!is.na(시도명))

local_sgg_raw %>%
  count(시도명) %>%
  summarise(sum(n))


# # A tibble: 8 × 2
# election_name     n
# <chr>         <int>
# 1 제1회           230
# 2 제2회           232
# 3 제3회           232
# 4 제4회           230
# 5 제5회           228
# 6 제6회           226
# 7 제7회           226
# 8 제8회           226

original <- local_sgg_excel %>%
  janitor::clean_names(ascii = FALSE) %>%
  filter(!is.na(시도명)) %>%
  group_by(시도명, 구시군명) %>%
  nest() %>%
  ungroup() %>%
  count(시도명)

reference <- krvote::local_sgg_winner %>%
  filter(election_code == "20220601") %>%
  count(sido_name)

original %>%
  left_join(reference, by = c("시도명" = "sido_name")) %>%
  mutate(diff = n.x - n.y) %>%
  filter(diff != 0)

krvote::local_sgg_winner %>%
  filter(election_code == "20220601") %>%
  filter(sido_name == "광주광역시") %>% View
  count(sido_name)

