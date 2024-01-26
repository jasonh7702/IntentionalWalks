
baseball_seasons <- rbind(data2018, data2019, data2020, data2021, 
                          data2022, data2023)

library(dplyr)
library(stringr)


baseball_seasons %>%
  mutate(lletter = substr(PITCH_SEQ_TX, start = nchar(PITCH_SEQ_TX), stop = nchar(PITCH_SEQ_TX))) %>%
  filter(lletter == "V") %>%
  select(GAME_ID, AWAY_TEAM_ID, INN_CT, OUTS_CT, BAT_ID, PIT_ID, PITCH_SEQ_TX, BAT_HAND_CD,
         PIT_HAND_CD) -> tmp


tmp <- tmp %>%
  mutate(midpa = as.numeric(str_detect(PITCH_SEQ_TX, "[^VNa-z\\.]")))

tmp <- tmp %>%
  mutate(SEASON = as.numeric(substr(GAME_ID, 4, 7))) %>%
  mutate(MATCHUP = paste(BAT_HAND_CD, PIT_HAND_CD))



baseball_seasonswalks <- baseball_seasons %>%
  mutate(EVENT_15 = EVENT_CD == 15) %>%
  mutate(SEASON = as.numeric(substr(GAME_ID, 4, 7))) %>%
  mutate(IBB = str_ends(PITCH_SEQ_TX, "V"),
         midpa = ifelse(IBB, as.numeric(str_detect(PITCH_SEQ_TX, "[^VNa-z\\.]")), NA),
         abs_score_diff = ifelse(IBB, abs(AWAY_SCORE_CT - HOME_SCORE_CT), NA),
         abs_midpa_diff = ifelse(midpa, abs(AWAY_SCORE_CT - HOME_SCORE_CT), NA))

# Display the resulting data frame
print(baseball_seasonswalks)

baseball_seasonswalks %>% select(SEASON, EVENT_15) %>% table %>% prop.table(margin = 1)


baseball_seasonswalks %>% select(SEASON, abs_midpa_diff) %>% 
  table 

baseball_seasonswalks %>% select(SEASON, midpa) %>% 
  table

tmp %>% select(SEASON, MATCHUP) %>% table

library(ggplot2)
g
# Assuming your data frame is called baseball_seasonswalks
ggplot(baseball_seasonswalks %>% filter(midpa %in% c(0, 1)), 
       aes(x = as.factor(SEASON), fill = factor(midpa))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Midpa Over Seasons",
       x = "Season",
       y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), name = "Midpa") +
  theme_minimal()


