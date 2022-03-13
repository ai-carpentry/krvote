
#' clean_varnames
#'
#' @description 데이터 팩키지 변수명 한글깨짐 문제 윈도우 혹은 맥 해결
#'
#' @param raw_data dataframe or tibble
#'
#' @return robust unicoded dataframe or tibble
#' @export
#'
clean_varnames <- function(raw_data) {

  varnames <- names( raw_data )

  varnames_unicode <- map_chr(varnames, stringi::stri_escape_unicode )

  varnames_unicode_to_korean <- map_chr(varnames_unicode, stringi::stri_unescape_unicode)

  unicode_data <- raw_data %>%
    set_names(varnames_unicode_to_korean)

  unicode_data
}



# # 0. 패키지 ---------------------------
# library(tidyverse)
# library(bannerCommenter)
# library(ARTofR)
#
# # 1. 대통령 선거  ---------------------------
# ## 1.1. 득표 데이터  ---------------------------
# banner_txt <- copy_to_clipboard(banner("대통령선거 투표 데이터", "개발자: 이광춘", "최종수정일: 2022-03-11",
#                                        numLines = 1,
#                                        bandChar = "="))
# copy_to_clipboard(banner_txt)
#
#
bannerCommenter::copy_to_clipboard(bannerCommenter::banner("시도별 선거인수",
                                       numLines = 1,
                                       bandChar = "-"))
#
# ## 1.2. 데이터프레임 문서화
#
# # sinew::makeOxygen(election_20170509$득표율, add_fields = "source")
#
#

# banner_txt <- bannerCommenter::copy_to_clipboard(bannerCommenter::banner("제18대 대통령",
#                                        numLines = 1,
#                                        bandChar = "-"))
# bannerCommenter::copy_to_clipboard(banner_txt)
#
#
# sinew::makeOxygen(general_2020, add_fields = "source")
