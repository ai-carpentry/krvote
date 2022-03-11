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
# banner_txt <- copy_to_clipboard(banner("제19대 대통령",
#                                        numLines = 1,
#                                        bandChar = "-"))
# copy_to_clipboard(banner_txt)
#
# ## 1.2. 데이터프레임 문서화
#
# # sinew::makeOxygen(election_20170509$득표율, add_fields = "source")
#
#
