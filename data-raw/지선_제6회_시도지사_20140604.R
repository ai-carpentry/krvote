
################################################################# ---
##             엑셀파일 유권자 투개표 : 지방선거/시도지사      ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-03-22                       ##
################################################################# ---


# 제6회 지방선거 - 시도지사 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)


##---------------------------------------------------------------- --
##                    제6회 지방선거 시도지사 (2014)            --
##---------------------------------------------------------------- --
# 출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=14979
# 1. 데이터

gangwon <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(강원)/01_강원도지사선거/강원도지사선거.xlsx")
kyunggi <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(경기)/읍면동별개표결과-경기도지사.xlsx")
kyungnam <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(경남)/01_도지사/경상남도.xls")
kyungbook <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(경북)/읍면동별 개표자료/01_시도지사/경상북도.xlsx")
kwangju_east <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(광주)/01_시도지사/광주광역시.xlsx", sheet = "동구")
kwangju_west <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(광주)/01_시도지사/광주광역시.xlsx", sheet = "서구")
kwangju_south <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(광주)/01_시도지사/광주광역시.xlsx", sheet = "남구")
kwangju_north <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(광주)/01_시도지사/광주광역시.xlsx", sheet = "북구")
kwangju_etc <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(광주)/01_시도지사/광주광역시.xlsx", sheet = "광산구")

daegu <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(대구)/1.시장/대구광역시.xls")
daejeon <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(대전)/01_시도지사/대전광역시.xls")
seoul <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(서울)/01_시도지사/서울특별시.xls")
sejong <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(세종).xlsx")
woolsan <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(울산)/시도지사/울산광역시.xls")

incheon <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(인천)/01_시도지사/인천광역시.xls")
jeonnam <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(전남)/읍면동별개표자료(전남)/01_시도지사/전라남도.xls")
jeonbook <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(전북)/도지사/전라북도.xls")
jeju <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(제주)/제주특별자치도_도지사.xls")
choongnam <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(충남)/1_시도지사선거/충청남도.xls")

choongbook <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(충북)/도지사.xls")
busan <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍변동별 개표자료(부산)/부산-시장-개표진행상황(읍면동별).xlsx")

## 1. 강원도 -------------------
gangwon_tbl <- gangwon %>%
  set_names(c("구시군명", "읍면동명", "구분", "선거인수", "투표수",
              "새누리당_최흥집", "새정치민주연합_최문순", "통합진보당_이승재", "계", "무효투표수", "기권수", "비고")) %>%
  slice(5:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "강원") %>%
  select(시도, everything()) %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 2. 경기도 -------------------
kyunggi_tbl <- kyunggi %>%
  set_names(c("구시군명", "읍면동명", "구분", "선거인수", "투표수",
              "새누리당_남경필", "새정치민주연합_김진표", "계", "무효투표수", "기권수")) %>%
  slice(4:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "경기") %>%
  select(시도, everything()) %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 3. 경상남도 -------------------
kyungnam_tbl <- kyungnam %>%
  set_names(c("구시군명", "읍면동명", "구분", "선거인수", "투표수",
              "새누리당_홍준표", "새정치민주연합_김경수", "통합진보당_강병기", "계", "무효투표수", "기권수")) %>%
  slice(4:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "경남") %>%
  select(시도, everything()) %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 4. 경상북도 -------------------
kyungbook_tbl <- kyungbook %>%
  set_names(c("구시군명", "읍면동명", "구분", "선거인수", "투표수",
              "새누리당_김관용", "새정치민주연합_오중기", "통합진보당_윤병태", "정의당_박창호", "계", "무효투표수", "기권수", "비고")) %>%
  slice(7:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "경북") %>%
  select(시도, everything()) %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 5. 광주 -------------------
kwangju_east_tbl <- kwangju_east %>%
  set_names(c("읍면동명", "구분", "선거인수", "투표수",
              "새누리당_이정재", "새정치민주연합_윤장현", "통합진보당_윤민호", "노동당_이병훈", "무소속_강운태", "무소속_이병완", "계", "무효투표수", "기권수")) %>%
  slice(5:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "광주",
           구시군명 = "동구") %>%
  select(시도, 구시군명, everything())

kwangju_west_tbl <- kwangju_west %>%
  set_names(c("읍면동명", "구분", "선거인수", "투표수",
              "새누리당_이정재", "새정치민주연합_윤장현", "통합진보당_윤민호", "노동당_이병훈", "무소속_강운태", "무소속_이병완", "계", "무효투표수", "기권수")) %>%
  slice(5:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "광주",
           구시군명 = "서구") %>%
  select(시도, 구시군명, everything())

kwangju_south_tbl <- kwangju_south %>%
  set_names(c("읍면동명", "구분", "선거인수", "투표수",
              "새누리당_이정재", "새정치민주연합_윤장현", "통합진보당_윤민호", "노동당_이병훈", "무소속_강운태", "무소속_이병완", "계", "무효투표수", "기권수")) %>%
  slice(5:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "광주",
           구시군명 = "남구") %>%
  select(시도, 구시군명, everything())

kwangju_north_tbl <- kwangju_north %>%
  set_names(c("읍면동명", "구분", "선거인수", "투표수",
              "새누리당_이정재", "새정치민주연합_윤장현", "통합진보당_윤민호", "노동당_이병훈", "무소속_강운태", "무소속_이병완", "계", "무효투표수", "기권수")) %>%
  slice(5:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "광주",
           구시군명 = "북구") %>%
  select(시도, 구시군명, everything())

kwangju_etc_tbl <- kwangju_etc %>%
  set_names(c("읍면동명", "구분", "선거인수", "투표수",
              "새누리당_이정재", "새정치민주연합_윤장현", "통합진보당_윤민호", "노동당_이병훈", "무소속_강운태", "무소속_이병완", "계", "무효투표수", "기권수")) %>%
  slice(5:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "광주",
           구시군명 = "광산구") %>%
  select(시도, 구시군명, everything())

kwangju <- bind_rows(kwangju_east_tbl, kwangju_west_tbl) %>%
  bind_rows(kwangju_south_tbl) %>%
  bind_rows(kwangju_north_tbl) %>%
  bind_rows(kwangju_etc_tbl)

kwangju_tbl <- kwangju %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 6. 대구 -------------------

daegu_raw <- daegu %>%
  set_names(c("읍면동명", "구분", "선거인수", "투표수",
              "새누리당_권영진", "새정치민주연합_김부겸", "통합진보당_송영우", "정의당_이원준", "무소속_이정숙", "계", "무효투표수", "기권수"))


daegu_split_list <- daegu_raw %>%
  mutate(구시군명 = ifelse(str_detect(읍면동명, "대구광역시"), 읍면동명, NA)) %>%
  select(구시군명, everything()) %>%
  mutate(구시군명 = str_remove(구시군명, "\\[시·도지사선거\\]\\[대구광역시\\]")) %>%
  fill(구시군명, .direction = "down") %>%
  mutate(구시군명 = str_extract(구시군명, pattern = "(?<=\\[).+?(?=\\])")) %>%
  dplyr::group_split(구시군명)

daegu_split_list <- map(daegu_split_list, slice, n = 4:n())

daegu_tbl <- map_df(daegu_split_list, rbind) %>%
  mutate(시도 = "대구") %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()


## 7. 대전 -------------------

daejeon_sheet <- excel_sheets("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(대전)/01_시도지사/대전광역시.xls")

daejeon_list <- list()

for(i in 1:length(daejeon_sheet)) {
  cat(i, "\n")
  daejeon_list[[i]] <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(대전)/01_시도지사/대전광역시.xls",
                                  sheet = daejeon_sheet[i])
  daejeon_list[[i]] <- daejeon_list[[i]] %>%
    set_names(c("읍면동명", "구분", "선거인수", "투표수",
                "새누리당_박성효", "새정치민주연합_권선택", "통합진보당_김창근", "정의당_한창민", "계", "무효투표수", "기권수")) %>%
    mutate(구시군명 = ifelse(str_detect(읍면동명, "대전광역시"), 읍면동명, NA)) %>%
    select(구시군명, everything()) %>%
    mutate(구시군명 = str_remove(구시군명, "\\[시·도지사선거\\]\\[대전광역시\\]")) %>%
    fill(구시군명, .direction = "down") %>%
    mutate(구시군명 = str_extract(구시군명, pattern = "(?<=\\[).+?(?=\\])")) %>%
    slice(4:n())
}

daegeon_tbl <- map_df(daejeon_list, rbind) %>%
  mutate(시도 = "대전") %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 8. 서울 -------------------

seoul_sheet <- excel_sheets("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(서울)/01_시도지사/서울특별시.xls")

seoul_list <- list()

for(i in 1:length(seoul_sheet)) {
  cat(i, "\n")
  seoul_list[[i]] <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(서울)/01_시도지사/서울특별시.xls",
                                sheet = seoul_sheet[i])
  seoul_list[[i]] <- seoul_list[[i]] %>%
    set_names(c("읍면동명", "구분", "선거인수", "투표수",
                "새누리당_정몽준", "새정치민주연합_박원순", "통합진보당_정태홍", "새정치당_홍정식", "계", "무효투표수", "기권수")) %>%
    mutate(구시군명 = ifelse(str_detect(읍면동명, "서울특별시"), 읍면동명, NA)) %>%
    select(구시군명, everything()) %>%
    mutate(구시군명 = str_remove(구시군명, "\\[시·도지사선거\\]\\[서울특별시\\]")) %>%
    fill(구시군명, .direction = "down") %>%
    mutate(구시군명 = str_extract(구시군명, pattern = "(?<=\\[).+?(?=\\])")) %>%
    slice(4:n())
}

seoul_tbl <- map_df(seoul_list, rbind) %>%
  mutate(시도 = "서울") %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 9. 울산 -------------------

woolsan_tbl <- woolsan %>%
  set_names(c("구시군명", "읍면동명", "구분", "선거인수", "투표수",
              "새누리당_김기현", "정의당_조승수", "노동당_이갑용", "계", "무효투표수", "기권수")) %>%
  slice(5:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "울산") %>%
  select(시도, everything()) %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 10. 인천 -------------------

incheon_tbl <- incheon %>%
  set_names(c("구시군명", "읍면동명", "구분", "선거인수", "투표수",
              "새누리당_유정복", "새정치민주연합_송영길", "통합진보당_신창현", "계", "무효투표수", "기권수")) %>%
  slice(5:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "인천") %>%
  select(시도, everything()) %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 11. 전라남도 -------------------

jeonnam_tbl <- jeonnam %>%
  set_names(c("구시군명", "읍면동명", "구분", "선거인수", "투표수",
              "새누리당_이중효", "새정치민주연합_이낙연", "통합진보당_이성수", "계", "무효투표수", "기권수")) %>%
  slice(6:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "전남") %>%
  select(시도, everything()) %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 12. 전라북도 -------------------

jeonbook_tbl <- jeonbook %>%
  set_names(c("구시군명", "읍면동명", "구분", "선거인수", "투표수",
              "새누리당_박철곤", "새정치민주연합_송하진", "통합진보당_이광석", "계", "무효투표수", "기권수")) %>%
  slice(5:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "전북") %>%
  select(시도, everything()) %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 13. 제주 -------------------

jeju_tbl <- jeju %>%
  set_names(c("구시군명", "읍면동명", "구분", "선거인수", "투표수",
              "새누리당_원희룡", "새정치민주연합_신구범", "통합진보당_고승완", "새정치당_주종근", "계", "무효투표수", "기권수")) %>%
  slice(5:n()) %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "제주") %>%
  select(시도, everything()) %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 14. 충남 -------------------

choongnam_tbl <- choongnam %>%
  set_names(c("구시군명", "읍면동명", "구분", "선거인수", "투표수",
              "새누리당_정진석", "새정치민주연합_안희정", "무소속_김기문", "계", "무효투표수", "기권수")) %>%
  slice(4:n()) %>%
  fill(구시군명, .direction = "down") %>%
  mutate(읍면동명 = str_remove_all(읍면동명, pattern = "\\s+")) %>%
  filter(!str_detect(읍면동명, "잘못")) %>%
  mutate(시도 = "충남") %>%
  select(시도, everything()) %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 15. 충북 -------------------

choongbook_sheet <- excel_sheets("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(충북)/충북도지사.xls") %>%
  setdiff("개표진행상황_위원회별합계")

choongbook_list <- list()

for(i in 1:length(choongbook_sheet)) {

  choongbook_list[[i]] <- read_excel("../../docs/krvotes/data-raw/제6회_전국동시지방선거_2014/제6회 전국동시지방선거 읍면동별 개표자료(충북)/충북도지사.xls",
                                     sheet = choongbook_sheet[i])
  choongbook_list[[i]] <- choongbook_list[[i]] %>%
    set_names(c("읍면동명", "구분", "선거인수", "투표수",
                "새누리당_윤진식", "새정치민주연합_이시종", "통합진보당_신장호", "계", "무효투표수", "기권수")) %>%
    mutate(구시군명 = ifelse(str_detect(읍면동명, "충청북도"), 읍면동명, NA)) %>%
    select(구시군명, everything()) %>%
    mutate(구시군명 = str_remove(구시군명, "\\[시·도지사선거\\]\\[충청북도\\]")) %>%
    fill(구시군명, .direction = "down") %>%
    mutate(구시군명 = str_extract(구시군명, pattern = "(?<=\\[).+?(?=\\])")) %>%
    slice(4:n())
}

choongbook_tbl <- map_df(choongbook_list, rbind) %>%
  mutate(시도 = "충북") %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 16. 부산 -------------------

busan_raw <- busan %>%
  set_names(c("읍면동명", "구분", "선거인수", "투표수",
              "새누리당_서병수", "무소속_오거돈", "계", "무효투표수", "기권수"))

# busan_gusigun <- busan_raw %>%
#     filter(str_detect(읍면동명, "부산광역시")) %>%
#     separate(읍면동명, into = c("선거", "시도", "구시군명"), sep = "\\]") %>%
#     mutate(구시군명 = str_remove(구시군명, "\\[")) %>%
#     pull(구시군명)

busan_split_list <- busan_raw %>%
  mutate(구시군명 = ifelse(str_detect(읍면동명, "부산광역시"), 읍면동명, NA)) %>%
  select(구시군명, everything()) %>%
  mutate(구시군명 = str_remove(구시군명, "\\[시·도지사선거\\]\\[부산광역시\\]")) %>%
  fill(구시군명, .direction = "down") %>%
  mutate(구시군명 = str_extract(구시군명, pattern = "(?<=\\[).+?(?=\\])")) %>%
  dplyr::group_split(구시군명)

busan_split_list <- map(busan_split_list, slice, n = 4:n())

busan_tbl <- map_df(busan_split_list, rbind) %>%
  mutate(시도 = "부산") %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

## 17. 세종 -------------------

sejong_tbl <- sejong %>%
  set_names(c("읍면동명", "구분", "선거인수", "투표수",
              "새누리당_유한식", "새정치민주연합_이춘희", "계", "무효투표수", "기권수")) %>%
  mutate(구시군명 = "세종특별자치시") %>%
  select(구시군명, everything()) %>%
  slice(5:n()) %>%
  mutate(시도 = "세종") %>%
  group_by(시도) %>%
  nest() %>%
  ungroup()

# **통합** -------

local_2014_raw <- bind_rows(busan_tbl, daegeon_tbl) %>%
  bind_rows(daegu_tbl) %>%
  bind_rows(gangwon_tbl) %>%
  bind_rows(jeju_tbl) %>%
  bind_rows(jeonbook_tbl) %>%
  bind_rows(jeonnam_tbl) %>%
  bind_rows(kwangju_tbl) %>%
  bind_rows(kyungbook_tbl) %>%
  bind_rows(kyunggi_tbl) %>%
  bind_rows(kyungnam_tbl) %>%
  bind_rows(sejong_tbl) %>%
  bind_rows(seoul_tbl) %>%
  bind_rows(choongnam_tbl) %>%
  bind_rows(incheon_tbl) %>%
  bind_rows(choongbook_tbl) %>%
  bind_rows(woolsan_tbl) %>%
  unique()


# 2.시도별 변수명 작업 수행 -------

clean_variable <- function(raw_data) {

  clean_tbl <- raw_data %>%
    filter(읍면동명 != "합계") %>%
    mutate(across(선거인수:기권수, as.character)) %>%
    mutate(구분 = ifelse(is.na(구분), 읍면동명, 구분)) %>%
    filter(str_detect(구분, "거소|관외|관내|일반|잘못") ) %>%
    # filter(str_detect(구분, "거소|관외|관내|일반") ) %>%
    pivot_longer(선거인수:기권수, values_to = "득표수") %>%
    separate(name, into = c("정당", "후보"), sep = "_") %>%
    mutate(득표수 = parse_number(득표수)) %>%
    filter(`읍면동명` != "합계",
           구분 != "소계") %>%
    mutate(후보 = ifelse(is.na(후보), 정당, 후보)) %>%
    select(구시군명, 읍면동명, 구분, 정당, 후보, 득표수)

  clean_tbl
}

local_sido_20140604 <- local_2014_raw %>%
  mutate(data = map(data, clean_variable))

# 3. 단위테스트 검증 -------------

test_that("지방선거 2014 후보득표검증", {

  ### 서울시장 후보 단위테스트 검정
  local_2014_seoul_check <- local_sido_20140604 %>%
    filter(str_detect(`시도`, "서울")) %>%
    unnest(data) %>%
    filter(str_detect(후보, "박원순|정몽준|정태홍|홍정식")) %>%
    group_by(후보) %>%
    summarise(득표수 = sum(득표수))

  expect_that( local_2014_seoul_check %>% filter(후보 == "박원순") %>% pull(득표수), equals(2752171))
  expect_that( local_2014_seoul_check %>% filter(후보 == "정몽준") %>% pull(득표수), equals(2109869))
  expect_that( local_2014_seoul_check %>% filter(후보 == "정태홍") %>% pull(득표수), equals(23638))
  expect_that( local_2014_seoul_check %>% filter(후보 == "홍정식") %>% pull(득표수), equals(17603))
})


# 4. 데이터 내보내기 -------------

usethis::use_data(local_sido_20140604, overwrite = TRUE)


# local_sido_20140604 %>%
#   filter(str_detect(`시도`, "강원")) %>%
#   unnest(data) %>%
#   filter(str_detect(후보, "최흥집|최문순|이승재")) %>%
#   group_by(후보) %>%
#   summarise(득표수 = sum(득표수))
