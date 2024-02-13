load("/Users/jasonhuang/Downloads/postdata2018.RData")
load("/Users/jasonhuang/Downloads/postdata2019.RData")
load("/Users/jasonhuang/Downloads/postdata2020.RData")
load("/Users/jasonhuang/Downloads/postdata2021.RData")
load("/Users/jasonhuang/Downloads/postdata2022.RData")

baseball_seasons <- rbind(data2018, data2019, data2020, data2021, 
                          data2022, data2023)

baseball_postseason <- rbind(postdata2018, postdata2019, postdata2020, postdata2021, 
                          postdata2022, postdata2023)

library(dplyr)
library(stringr)


baseball_seasons %>%
  mutate(lletter = substr(PITCH_SEQ_TX, start = nchar(PITCH_SEQ_TX), stop = nchar(PITCH_SEQ_TX))) %>%
  filter(lletter == "V") %>%
  select(GAME_ID, AWAY_TEAM_ID, INN_CT, OUTS_CT, BAT_ID, PIT_ID, PITCH_SEQ_TX, BAT_HAND_CD,
         PIT_HAND_CD) -> tmp


tmp <- tmp %>%
  mutate(midpa = as.numeric(str_detect(PITCH_SEQ_TX, "[^VNa-z\\.]")))

postdata2023 %>% 
  mutate(lletter = substr(PITCH_SEQ_TX, start = nchar(PITCH_SEQ_TX), stop = nchar(PITCH_SEQ_TX))) %>%
  filter(lletter == "V") %>%
  select(GAME_ID, AWAY_TEAM_ID, INN_CT, OUTS_CT, BAT_ID, PIT_ID, PITCH_SEQ_TX, BAT_HAND_CD,
         PIT_HAND_CD) -> postseason

tmp <- tmp %>%
  mutate(SEASON = as.numeric(substr(GAME_ID, 4, 7))) %>%
  mutate(MATCHUP = paste(BAT_HAND_CD, PIT_HAND_CD))


#creating new variables for baseball_seasons 
baseball_seasonswalks <- baseball_seasons %>%
  mutate(EVENT_15 = EVENT_CD == 15) %>%
  mutate(SEASON = as.numeric(substr(GAME_ID, 4, 7))) %>%
  mutate(IBB = str_ends(PITCH_SEQ_TX, "V"),
         midpa = ifelse(IBB, as.numeric(str_detect(PITCH_SEQ_TX, "[^VNa-z\\.]")), NA),
         abs_score_diff = ifelse(IBB, abs(AWAY_SCORE_CT - HOME_SCORE_CT), NA),
         abs_midpa_diff = ifelse(midpa, abs(AWAY_SCORE_CT - HOME_SCORE_CT), NA)) %>% 
  mutate(midpa2 =ifelse(IBB, as.numeric(str_detect(PITCH_SEQ_TX, "[^VNa-z\\.]")), 0)) %>% 
  mutate(HOME_TEAM_ID = ifelse(BAT_HOME_ID == 1, substr(GAME_ID, 1, 3), NA)) %>% 
  mutate(AWAY_TEAM_ID2 = ifelse(BAT_HOME_ID == 0, substr(AWAY_TEAM_ID, 1, 3), NA)) %>% 
  mutate(MATCHUP = paste(BAT_HAND_CD, PIT_HAND_CD))







#Displaying the data as tables
baseball_seasonswalks %>% select(SEASON, EVENT_15) %>% table %>% prop.table(margin = 1)


baseball_seasonswalks %>% select(SEASON, abs_midpa_diff) %>% 
  table 

baseball_seasonswalks %>% select(SEASON, midpa) %>% 
  table
#DO THIS FOR MATCHUP, use group by
baseball_seasonswalks %>% select(HOME_TEAM_ID, IBB, MATCHUP) %>% 
  table %>% prop.table(margin=c(1,2))

baseball_seasonswalks %>% select(HOME_TEAM_ID, IBB) %>% 
  table
#MEETING FEB 6th
baseball_seasonswalks %>% filter(IBB) %>%  select(SEASON, midpa2) %>% 
  table %>% prop.table(margin = 1)

baseball_seasonswalks %>% select(SEASON,IBB) %>%  table %>% prop.table(margin=1)
baseball_seasonswalks %>% filter(IBB) %>%  select(SEASON, midpa2) %>%  table 
baseball_seasonswalks %>% select(IBB, midpa) %>% 
  table

#multiple variables
baseball_seasonswalks %>% select(SEASON, midpa2, HOME_TEAM_ID) %>% 
  table

baseball_seasonswalks %>% select(SEASON, MATCHUP, IBB) %>% 
  table

model <- glm(IBB ~ HOME_TEAM_ID, data = baseball_seasonswalks)

summary(model)

model2 <- glm(IBB ~ AWAY_TEAM_ID2, data = baseball_seasonswalks)

summary(model2)

model3 <- glm(IBB ~ INN_CT + abs_score_diff, data = baseball_seasonswalks)

summary(model3)

#plots
library(ggplot2)

ggplot(baseball_seasonswalks %>% filter(midpa %in% c(0, 1)), 
       aes(x = as.factor(SEASON), fill = factor(midpa))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Midpa Over Seasons",
       x = "Season",
       y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), name = "Midpa") +
  theme_minimal()


top_bat_ids <- baseball_seasonswalks %>%
  filter(IBB) %>%
  count(BAT_ID, sort = TRUE) %>%
  slice_head(n = 20)

top_midpa <- baseball_seasonswalks %>%
  filter(midpa == 1) %>%
  count(BAT_ID, sort = TRUE) %>%
  slice_head(n = 20)


table_result <- baseball_seasonswalks %>% 
  select(SEASON, abs_midpa_diff) %>% 
  table(useNA = "ifany")

print(table_result)

#MATCHUP TABLE
table_result2 <- baseball_seasonswalks %>% 
  filter(!is.na(HOME_TEAM_ID)) %>%
  group_by(MATCHUP, HOME_TEAM_ID) %>%
  summarize(
    IBB_TRUE_Percentage = mean(IBB == TRUE, na.rm = TRUE),
    IBB_FALSE_Percentage = mean(IBB == FALSE, na.rm = TRUE)
  )

print(table_result2)

table_result3 <- baseball_seasonswalks %>% 
  filter(!is.na(HOME_TEAM_ID)) %>%
  group_by(MATCHUP, HOME_TEAM_ID) %>%
  summarize(
    IBB_TRUE_Count = sum(IBB == TRUE, na.rm = TRUE),
    IBB_FALSE_Count = sum(IBB == FALSE, na.rm = TRUE)
  )

print(table_result3)

#POST SEASON DATA

baseball_postwalks <- baseball_postseason %>%
  mutate(EVENT_15 = EVENT_CD == 15) %>%
  mutate(SEASON = as.numeric(substr(GAME_ID, 4, 7))) %>%
  mutate(IBB = str_ends(PITCH_SEQ_TX, "V"),
         midpa = ifelse(IBB, as.numeric(str_detect(PITCH_SEQ_TX, "[^VNa-z\\.]")), NA),
         abs_score_diff = ifelse(IBB, abs(AWAY_SCORE_CT - HOME_SCORE_CT), NA),
         abs_midpa_diff = ifelse(midpa, abs(AWAY_SCORE_CT - HOME_SCORE_CT), NA)) %>% 
  mutate(midpa2 =ifelse(IBB, as.numeric(str_detect(PITCH_SEQ_TX, "[^VNa-z\\.]")), 0)) %>% 
  mutate(HOME_TEAM_ID = ifelse(BAT_HOME_ID == 1, substr(GAME_ID, 1, 3), NA)) %>% 
  mutate(AWAY_TEAM_ID2 = ifelse(BAT_HOME_ID == 0, substr(AWAY_TEAM_ID, 1, 3), NA)) %>% 
  mutate(MATCHUP = paste(BAT_HAND_CD, PIT_HAND_CD))

top_bat_ids <- baseball_postwalks %>%
  filter(IBB) %>%
  count(BAT_ID, sort = TRUE) %>%
  slice_head(n = 20)

baseball_postwalks %>% select(SEASON, midpa) %>% 
  table

#MEETING FEB 6th
baseball_postwalks %>% filter(IBB) %>% select(SEASON, midpa2) %>%  table %>% prop.table(margin = 1)

baseball_postwalks %>% filter(IBB) %>%  select(PITCH_SEQ_TX, midpa2) %>% slice(1:20)

baseball_postwalks %>% select(SEASON, IBB) %>% 
  table

baseball_postmidpa <- baseball_postwalks %>% filter(!is.na(midpa) & midpa == 1) %>% 
  select(GAME_ID, AWAY_TEAM_ID, INN_CT, OUTS_CT, BAT_ID, PIT_ID, PITCH_SEQ_TX, BAT_HAND_CD,
         PIT_HAND_CD)

#CREATING A PLOT FOR MIDPLATE WALKS GIVEN INTENTIONAL WALKS 
library(ggplot2)


baseball_seasonswalks2 <- baseball_seasonswalks %>%
  mutate(MIDPA_TRUE = ifelse(!is.na(midpa2) & midpa2 == 1, TRUE, FALSE))

prob_data <- baseball_seasonswalks2 %>%
  filter(IBB == 1) %>%
  group_by(SEASON) %>%
  summarize(Prob_MIDPA = mean(MIDPA_TRUE, na.rm = TRUE))

baseball_postwalks2 <- baseball_postwalks %>%
  mutate(MIDPA_TRUE = ifelse(!is.na(midpa2) & midpa2 == 1, TRUE, FALSE))


prob_data2 <- baseball_postwalks2 %>%
  filter(IBB == 1) %>%  
  group_by(SEASON) %>%
  summarize(Prob_MIDPA2 = mean(MIDPA_TRUE, na.rm = TRUE))

combined_prob_data <- left_join(prob_data, prob_data2, by = "SEASON")

combined_plot <- ggplot(combined_prob_data, aes(x = as.factor(SEASON))) +
  geom_point(aes(y = Prob_MIDPA), size = 3, color = "blue") +
  geom_text(aes(y = Prob_MIDPA, label = "Regular Season"), vjust = -0.5, hjust = 0.5, color = "blue") +
  geom_point(aes(y = Prob_MIDPA2), size = 3, color = "red") +
  geom_text(aes(y = Prob_MIDPA2, label = "Postseason"), vjust = -0.5, hjust = 0.5, color = "red") +
  labs(title = "Probability of MIDPA given IBB",
       x = "Season",
       y = "Probability of MIDPA") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"))


print(combined_plot)


#CODE FOR IBB percentage regular season vs. postseason
baseball_seasonswalks3 <- baseball_seasonswalks %>%
  mutate(MIDPA_TRUE = ifelse(!is.na(midpa) & midpa == 1, TRUE, FALSE))

ibb_data <- baseball_seasonswalks3 %>%
  group_by(SEASON) %>%
  summarize(Percentage_IBB = mean(IBB == TRUE, na.rm = TRUE))

baseball_postwalks3 <- baseball_postwalks %>%
  mutate(MIDPA_TRUE = ifelse(!is.na(midpa) & midpa == 1, TRUE, FALSE))

ibb_data_postseason <- baseball_postwalks3 %>%
  group_by(SEASON) %>%
  summarize(Percentage_IBB_Postseason = mean(IBB == TRUE, na.rm = TRUE))

combined_ibb_data <- left_join(ibb_data, ibb_data_postseason, by = "SEASON")

combined_plot_ibb <- ggplot(combined_ibb_data, aes(x = as.factor(SEASON))) +
  geom_point(aes(y = Percentage_IBB), size = 3, color = "blue") +
  geom_text(aes(y = Percentage_IBB, label = "Regular Season"), vjust = -0.5, hjust = 0.5, color = "blue") +
  geom_point(aes(y = Percentage_IBB_Postseason), size = 3, color = "red") +
  geom_text(aes(y = Percentage_IBB_Postseason, label = "Postseason"), vjust = -0.5, hjust = 0.5, color = "red") +
  labs(title = "Percentage of Rows with IBB = TRUE",
       x = "Season",
       y = "Percentage of Rows with IBB = TRUE") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"))

print(combined_plot_ibb)

