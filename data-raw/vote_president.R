
################################################################# ---
##              대통령선거 투표 데이터                         ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-03-11                       ##
################################################################# ---
## 데이터 출처: https://bit.ly/2QRqyGQ

#팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)

##---------------------------------------------------------------- ---
##                      제19대 대통령                           --
##---------------------------------------------------------------- ---
# 1. 제19대 대통령 -----------------------------------------------

## 1.1. 원본데이터 가져오기 -------------

election_20170509_xlsx <- read_excel("inst/extdata/president/제19대선-투표구별개표자료.xlsx", sheet="19대선", skip=1) %>%
  janitor::clean_names(ascii = FALSE)

# Encoding(election_20170509_xlsx[[1]])

## 2. 데이터 정제작업 -------------

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


## 2.1. 투표율 ------------------------------------
election_20170509_casting <- election_20170509_tbl %>%
  select(시도명, 구시군명, 읍면동명, 투표구명, 선거인수, 투표수, 무효투표수, 기권수)

## 2.2. 득표율 ------------------------------------

election_20170509_voting <- election_20170509_tbl %>%
  select(시도명, 구시군명, 읍면동명, 투표구명, 선거인수, 투표수:계)


# 3. 단위테스트 검증 -------------

test_that("대선 2018 후보득표검증", {

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


# 4. 데이터 내보내기 -------------
## 4.1. 인코딩 -------------------

clean_varnames <- function(raw_data) {

  varnames <- names( raw_data )

  varnames_unicode <- map_chr(varnames, stringi::stri_escape_unicode )

  varnames_unicode_to_korean <- map_chr(varnames_unicode, stringi::stri_unescape_unicode)

  unicode_data <- raw_data %>%
    set_names(varnames_unicode_to_korean)

}

election_20170509_casting <- clean_varnames(election_20170509_casting)
election_20170509_voting  <- clean_varnames(election_20170509_voting)

## 4.2. 내보내기 -------------------

election_20170509 <- list( meta = list(
  title =  stringi::stri_escape_unicode("제19대 대통령선거") %>% stringi::stri_unescape_unicode(.),
  data  = stringi::stri_escape_unicode("투표구별 투표, 투표구/후보별 득표") %>% stringi::stri_unescape_unicode(.) ),
  득표율 = election_20170509_casting,
  투표율 = election_20170509_voting )

list_names <- names(election_20170509) %>%
  stringi::stri_escape_unicode(.) %>%
  stringi::stri_unescape_unicode(.)

names(election_20170509) <- list_names

usethis::use_data(election_20170509, overwrite = TRUE)


