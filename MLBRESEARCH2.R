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



IBB_frequency <- baseball_seasonswalks %>%
  group_by(MATCHUP, SEASON) %>%
  summarize(IBB_freq = mean(IBB, na.rm = TRUE))


midpa_frequency <- baseball_seasonswalks %>%
  filter(IBB) %>%
  group_by(MATCHUP, SEASON) %>%
  summarize(midpa_freq = mean(midpa > 0, na.rm = TRUE))





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
    IBB_FALSE_Count = sum(IBB == FALSE, na.rm = TRUE),
    FREQUENCY = IBB_TRUE_Count / IBB_FALSE_Count
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


## ADDING COUNTS and BASERUNNERS

baseball_seasonswalks <- baseball_seasonswalks %>% 
  mutate(RUNS =  AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED = (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
           (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3)) 

baseball_seasonswalks %>% 
  group_by(HALF.INNING) %>% 
  summarize(Outs.Inning = sum(EVENT_OUTS_CT),
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start) -> half_innings

baseball_seasonswalks %>%  inner_join(half_innings, by = "HALF.INNING") %>% 
  mutate(RUNS.ROI = MAX.RUNS - RUNS) -> baseball_seasonswalks

baseball_seasonswalks %>% 
  mutate(BASES = paste(ifelse(BASE1_RUN_ID > '', 1, 0),
                       ifelse(BASE2_RUN_ID > '', 1, 0),
                       ifelse(BASE3_RUN_ID > '', 1, 0), sep = ""),
         STATE = paste(BASES, OUTS_CT)) -> baseball_seasonswalks

baseball_seasonswalks %>% 
  mutate(NRUNNER1 = 
           as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1),
         NRUNNER2 =
           as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 |
                        BAT_DEST_ID == 2),
         NRUNNER3 =
           as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID == 3 |
                        RUN3_DEST_ID == 3 | BAT_DEST_ID == 3),
         NOUTS = OUTS_CT + EVENT_OUTS_CT,
         NEW.BASES = paste(NRUNNER1, NRUNNER2,
                           NRUNNER3, sep = ""),
         NEW.STATE = paste(NEW.BASES, NOUTS)) -> baseball_seasonswalks

baseball_seasonswalks %>% 
  filter(STATE != NEW.STATE | (RUNS.SCORED > 0)) -> baseball_seasonswalks

baseball_seasonswalks %>% 
  filter(Outs.Inning == 3) -> baseball_seasonswalksC 

baseball_seasonswalksC %>% 
  group_by(STATE) %>% 
  summarize(Mean = mean(RUNS.ROI)) %>% 
  mutate(Outs = substr(STATE, 5, 5)) %>% 
  arrange(Outs) -> RUNS


RUNS_out <- matrix(round(RUNS$Mean, 2),8, 3)
dimnames(RUNS_out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out)[[1]] <- c("000", "001", "010", "011"
                             ,"100", "101", "110", "111")

RUNS.2002 <- matrix(c(.51,1.40,1.14,1.96,.90,1.84,1.51,2.33,.27,.94,.68,1.36,.54,1.18,
                      .94,1.51,.10,.36,.32,.63,.23,.52,.45,.78),8,3)
dimnames(RUNS.2002) <- dimnames(RUNS_out)
cbind(RUNS_out, RUNS.2002)

baseball_seasonswalks <- baseball_seasonswalks %>% 
  mutate(sequence = gsub("[.>123+*N]","",PITCH_SEQ_TX))

baseball_seasonswalks <- baseball_seasonswalks %>% 
  mutate(c00 = TRUE,
         c10 = grepl("^[BIPV]", sequence),
         c01 = grepl("^[CFKLMOQRST]", sequence))

baseball_seasonswalks <- baseball_seasonswalks %>% 
  mutate(c20 = grepl("^[BIPV]{2}", sequence),
         c30 = grepl("^[BIPV]{3}", sequence),
         c02 = grepl("^[CFKLMOQRST]{2}", sequence))
b <- "[BIPV]"
s <- "[CFKLMOQRST]"
baseball_seasonswalks <- baseball_seasonswalks %>% 
  mutate(c11 = grepl(paste0("^",s,b,
                            "|",b,s), sequence),
         c21 = grepl(paste0("^",s,b,b,
                            "|",b,s,b,
                            "|",b,b,s), sequence),
         c31 = grepl(paste0("^",s,b,b,b,
                            "|",b,s,b,b,
                            "|",b,b,s,b,
                            "|",b,b,b,s), sequence))
baseball_seasonswalks <- baseball_seasonswalks %>% 
  mutate(c12 = grepl(paste0("^", b, s, s,
                            "|", s, b, s,
                            "l", s, s, "[FR]*", b), sequence),
         c22 = grepl(paste0("^",b,b,s,s,
                            "|",b,s,b,s,
                            "|",b,s,s,"[FR]*",b,
                            "|", s,b,b,s,
                            "|",s,b,s,"[FR]*", b,
                            "|",s,s,"[FR]*",b,"[FR]*",b),
                     sequence),
         c32 = grepl(paste0("^", s, "*", b, s,
                            "*", b, s, "*", b), sequence)
         & grepl(paste0("^",b,"*",s,b,"*",s),
                 sequence))
baseball_seasonswalks %>% mutate(abs_score_diff = abs(AWAY_SCORE_CT - HOME_SCORE_CT)) -> baseball_seasonswalks

baseball_seasonswalks %>% select(STATE, IBB) %>% 
  table

baseball_seasonswalks %>% filter(STATE== "000 0", midpa==1)



top_occurrences <- baseball_seasonswalks %>% 
  filter(IBB) %>%
  select(STATE, INN_CT, IBB) %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  head(10)

top_occurrences

baseball_seasonswalks %>% select(STATE, midpa) %>% 
  table

top_occurrences2 <- baseball_seasonswalks %>%
  filter(midpa == 1) %>%
  select(STATE, INN_CT, midpa) %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  head(10)
top_occurrences2

model <- glm(midpa ~ STATE + INN_CT + abs_score_diff + MATCHUP, family = binomial, data = baseball_seasonswalks)

summary(model)

model2 <- glm(midpa2 ~ STATE + INN_CT + abs_score_diff + MATCHUP, family = binomial, data = baseball_seasonswalks)

summary(model2)



devtools::install_github("rvlenth/estimability", dependencies = TRUE)

