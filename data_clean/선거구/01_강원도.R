
################################################################# ---
##                    선거구 - 강원도                          ##
##                    개발자: 이광춘                           ##
##                최종수정일: 2023-01-25                       ##
################################################################# ---

# library(pins)
#
# od <- Microsoft365R::get_personal_onedrive()
# board <- board_ms365(od, "krvote_board")
# board %>% pin_write(iris)

# 1. 2022년 대선 ----------------

kangwon_code <- demographics::sigungu_code %>%
  filter(!is.na(구시군명)) %>%
  filter( str_detect(시도명, "강원"))

krvote_202203009 <- krvote::election_20220309$득표율 %>%
  filter( str_detect(시도명, "강원")) %>%
  pivot_longer(이재명:김민찬, names_to = "후보", values_to = "득표수") %>%
  group_by(시도명, 구시군명, 후보) %>%
  summarise( 선거인수 = mean(선거인수),
             투표수 = mean(투표수),
             득표수 = sum( 득표수, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(kangwon_code) %>%
  select( 시도코드, 시도명, 구시군명, 후보, 선거인수, 투표수, 득표수)

# 2. 2022년 지선 ----------------

kangwon_code <- demographics::sigungu_code %>%
  filter(!is.na(구시군명)) %>%
  filter( str_detect(시도명, "강원"))

krvote_202203009 <- krvote::election_20220309$득표율 %>%
  filter( str_detect(시도명, "강원")) %>%
  pivot_longer(이재명:김민찬, names_to = "후보", values_to = "득표수") %>%
  group_by(시도명, 구시군명, 후보) %>%
  summarise( 선거인수 = mean(선거인수),
             투표수 = mean(투표수),
             득표수 = sum( 득표수, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(kangwon_code) %>%
  select( 시도코드, 시도명, 구시군명, 후보, 선거인수, 투표수, 득표수)
