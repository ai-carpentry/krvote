# 공공데이터 포털 중앙선거관리위원회 유권자수 -----------

# 1. 선거별 시도별 유권자수 -------

#' @title 선거별 시도별 유권자수
#' @description 시도별 선거인수 정보
#' @format A data frame with 1159 rows and 27 variables:
#' \describe{
#'   \item{\code{선거코드}}{character COLUMN_DESCRIPTION}
#'   \item{\code{선거명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{선거구분}}{character COLUMN_DESCRIPTION}
#'   \item{\code{결과순서}}{character COLUMN_DESCRIPTION}
#'   \item{\code{선거ID}}{character COLUMN_DESCRIPTION}
#'   \item{\code{시도명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{구시군수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{읍면동수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{투표구수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수(재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수(외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(계)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(계_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(남)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(남_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(여)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(여_재외국민)}}{character COLUMN_DESCRIPTION}
#'}
#' @source \url{http://somewhere.important.com/}
"voter_sido"

# 2. 선거별 구시군별 유권자수 -------

#' @title 선거별 구시군별 유권자수
#' @description 선거별 구시군별 유권자수 정보
#' @format A data frame with 16865 rows and 27 variables:
#' \describe{
#'   \item{\code{선거코드}}{character COLUMN_DESCRIPTION}
#'   \item{\code{선거명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{선거구분}}{character COLUMN_DESCRIPTION}
#'   \item{\code{결과순서}}{character COLUMN_DESCRIPTION}
#'   \item{\code{선거ID}}{character COLUMN_DESCRIPTION}
#'   \item{\code{시도명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{구시군명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{읍면동수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{투표구수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수(재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수(외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(계)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(계_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(남)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(남_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(여)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(여_재외국민)}}{character COLUMN_DESCRIPTION}
#'}
#' @source \url{http://somewhere.important.com/}
"voter_gusigun"

# 3. 선거별 구시군, 투표구별 유권자수 -------

#' @title 투표구별 선거인수
#' @description 투표구별 선거인수
#' @format A data frame with 196911 rows and 27 variables:
#' \describe{
#'   \item{\code{선거코드}}{character COLUMN_DESCRIPTION}
#'   \item{\code{시도명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{구시군명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{결과순서}}{character COLUMN_DESCRIPTION}
#'   \item{\code{선거ID}}{character COLUMN_DESCRIPTION}
#'   \item{\code{시도명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{구시군명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{읍면동명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{투표구명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수(재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수(외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(계)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(계_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(남)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(남_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(여)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(여_재외국민)}}{character COLUMN_DESCRIPTION}
#'}
#' @source \url{http://somewhere.important.com/}
"voter_station"

# 4. 선거별 구시군, 읍면동별 유권자수 -------

#' @title 선거별 구시군, 읍면동별 유권자수
#' @description 선거별 구시군, 읍면동별 유권자수
#' @format A data frame with 49653 rows and 27 variables:
#' \describe{
#'   \item{\code{선거코드}}{character COLUMN_DESCRIPTION}
#'   \item{\code{시도명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{구시군명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{결과순서}}{character COLUMN_DESCRIPTION}
#'   \item{\code{선거ID}}{character COLUMN_DESCRIPTION}
#'   \item{\code{시도명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{구시군명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{읍면동명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{투표구수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수(재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수(외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(계)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(계_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(남)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(남_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(여)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(여_재외국민)}}{character COLUMN_DESCRIPTION}
#'}
#' @source \url{http://somewhere.important.com/}
"voter_emd"

# 5. 선거별 구시군, 선거구별 유권자수 -------

#' @title 선거별 구시군, 선거구별 유권자수
#' @description 선거별 구시군, 선거구별 유권자수
#' 선거구분코드(sgTypecode)가 2:국회의원선거, 4:구·시·군의 장선거,
#' 5:시·도의회의원선거, 6:구·시·군의회의원선거, 10:교육의원선거일 경우 제공됨
#' @format A data frame with 96092 rows and 31 variables:
#' \describe{
#'   \item{\code{선거코드}}{character COLUMN_DESCRIPTION}
#'   \item{\code{선거구분}}{character COLUMN_DESCRIPTION}
#'   \item{\code{선거명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{시도명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{구시군명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{결과순서}}{character COLUMN_DESCRIPTION}
#'   \item{\code{선거ID}}{character COLUMN_DESCRIPTION}
#'   \item{\code{시도명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{구시군명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{선거구명}}{character COLUMN_DESCRIPTION}
#'   \item{\code{구시군수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{읍면동수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{투표구수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수(재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{인구수(외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(계_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(남_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{확정선거인수(여_외국인)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(계)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(계_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(남)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(남_재외국민)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(여)}}{character COLUMN_DESCRIPTION}
#'   \item{\code{거소투표 신고인명부 등재자수(여_재외국민)}}{character COLUMN_DESCRIPTION}
#'}
#' @source \url{http://somewhere.important.com/}
"voter_precinct"


