# 공공데이터 포털 중앙선거관리위원회 마스터 코드


#' @title 중앙선거관리위원회 선거코드
#' @description 공공데이터포털 - 중앙선거관리위원회 선거코드
#' 공공데이터포털 - 중앙선거관리위원회 선거코드
#' 검색어: 중앙선거관리위원회 코드정보
#'
#' @name code_election
#' @format A data frame with 79 rows and 3 variables:
#' \describe{
#'   \item{선거코드}{문자형, 선거실시일을 코드기준값으로 설정}
#'   \item{선거명}{문자형, 선거명칭}
#'   \item{선거구분}{문자형, 2자리 숫자로 구성된 선거구분코드}
#'}
#' @source \url{https://www.data.go.kr/}
#' @examples code_election
"code_election"


#' @title 중앙선거관리위원회 정당코드
#'
#' @description 공공데이터포털 - 중앙선거관리위원회 정당코드
#' 검색어: 중앙선거관리위원회 코드정보
#'
#' @format A data frame with 16 rows and 2 variables:
#' \describe{
#'   \item{선거코드}{문자형, 선거실시일을 코드기준값으로 설정}
#'   \item{data}{티블, 선거코드와 정당명}
#'}
#' @source \url{https://www.data.go.kr/}
#' @examples code_party
"code_party"

#' @title 중앙선거관리위원회 선거구 코드
#' @description 중앙선거관리위원회 선거구코드
#'
#' 공공데이터포털 - 중앙선거관리위원회 선거구코드
#' 검색어: 중앙선거관리위원회 코드정보
#'
#' @name code_precinct
#' @format 63개 관측점과 4개 변수를 갖는 데이터프레임
#' \describe{
#'   \item{선거코드}{문자형, 선거실시일을 코드기준값으로 설정}
#'   \item{선거명}{문자형, 선거명칭}
#'   \item{선거구분}{문자형, 2자리 숫자로 구성된 선거구분코드}
#'   \item{data}{티블, 선거코드, 선거명, 시도명, 구시군명 총 4 변수를 갖는 데이터프레임}
#' }
#' @source \url{https://www.data.go.kr/}
#' @examples code_precinct
"code_precinct"


#' @title 중앙선거관리위원회 구시군코드
#'
#' 공공데이터포털 - 중앙선거관리위원회 구시군코드
#' 검색어: 중앙선거관리위원회 코드정보
#'
#' @description 중앙선거관리위원회 구시군코드
#' @format A data frame with 15 rows and 2 variables:
#' \describe{
#'   \item{선거코드}{문자형, 선거실시일을 코드기준값으로 설정}
#'   \item{data}{티블, 선거코드, 시도명, 구시군명 총 3 변수를 갖는 데이터프레임}
#' }
#' @source \url{https://www.data.go.kr/}
#' @examples code_gusigun
"code_gusigun"



