
#' clean_varnames
#'
#' @description 데이터 팩키지 변수명 한글깨짐 문제 윈도우 혹은 맥 해결
#'
#' @param raw_data dataframe or tibble
#'
#' @return robust unicoded dataframe or tibble
#' @export
clean_varnames <- function(raw_data) {

  varnames <- names( raw_data )

  varnames_unicode <- map_chr(varnames, stringi::stri_escape_unicode )

  varnames_unicode_to_korean <- map_chr(varnames_unicode, stringi::stri_unescape_unicode)

  unicode_data <- raw_data %>%
    set_names(varnames_unicode_to_korean)

  unicode_data
}


#' make_dataframe_clean
#'
#' @description 국회의원선거 wide 형태를 unnest 가능한 long 형태 변환
#'
#' @param raw_data dataframe or tibble
#'
#' @return clean dataframe
#' @export
make_dataframe_clean <- function(raw_df) {

  clean_df <- raw_df %>%
    pivot_longer(선거인수:기권수, names_to = "구분", values_to="사람수") %>%
    mutate(사람수 = parse_number(사람수),
              구분 = str_replace(구분, "\n", " "))

  clean_df
}


