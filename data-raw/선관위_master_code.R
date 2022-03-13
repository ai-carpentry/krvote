
################################################################# ---
##              선관위 마스터 데이터                           ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-03-11                       ##
################################################################# ---

# 팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(httr)

##---------------------------------------------------------------- ---
##                      시도코드                                --
##---------------------------------------------------------------- ---
# 1. 시도코드 ---------------------------------------------------
presid_20_request <- glue::glue("http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml?",
                                "electionId=0020220309&requestURI=%2FWEB-INF%2Fjsp%2Felectioninfo%2F0020220309%2Fvc%2Fvccp08.jsp&topMenuId=VC&secondMenuId=VCCP08&menuId=VCCP08&statementId=VCCP08_%231&electionCode=1&cityCode=1100&sggCityCode=-1&townCodeFromSgg=-1&townCode=1101&sggTownCode=-1&checkCityCode=-1&x=55&y=18")

presid_20_resp <- GET(presid_20_request) %>%
  content(as="text") %>%
  rvest::read_html()

sido_text <- presid_20_resp %>%
  html_elements(css ="#cityCode") %>%
  html_elements("option") %>%
  html_text()

sido_value <- presid_20_resp %>%
  html_elements(css ="#cityCode") %>%
  html_elements("option") %>%
  html_attr("value")

nec_sido_code <- tibble(시도명 = sido_text, 시도코드 = sido_value) %>%
  filter(시도코드 != "-1")


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




