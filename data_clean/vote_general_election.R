
################################################################# ---
##              국회의원선거 투표 데이터                       ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2022-03-11                       ##
################################################################# ---

#팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)

##---------------------------------------------------------------- ---
##                      제21대 국회의원                         --
##---------------------------------------------------------------- ---


install.packages("httr") # HTTP통신을 위한 패키지 설치

data_general_url <- glue::glue("http://apis.data.go.kr/9760000/VoteXmntckInfoInqireService2/getVoteSttusInfoInqire?",
                               "serviceKey=서비스키",
                               "&pageNo=1",
                               "&numOfRows=10",
                               "&sgId=20220309",
                               "&sgTypecode=1",
                               "&sdName=",
                               "&wiwName=")




