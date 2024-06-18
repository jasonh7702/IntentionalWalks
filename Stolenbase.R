#loading in data
load("/Users/jasonhuang/Downloads/data2018.RData")
load("/Users/jasonhuang/Downloads/data2019.RData")
load("/Users/jasonhuang/Downloads/data2020.RData")
load("/Users/jasonhuang/Downloads/data2021.RData")
load("/Users/jasonhuang/Downloads/data2022.RData")
load("/Users/jasonhuang/Downloads/data2023.RData")
library(tidyverse)
#run expectancy matrices for 2022
data2022 %>% 
  mutate(RUNS =  AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED = (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
           (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3)) -> data2022
data2022 %>% 
  group_by(HALF.INNING) %>% 
  summarize(Outs.Inning = sum(EVENT_OUTS_CT),
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start) -> half_innings

data2022 %>%  inner_join(half_innings, by = "HALF.INNING") %>% 
  mutate(RUNS.ROI = MAX.RUNS - RUNS) -> data2022

data2022 %>% 
  mutate(BASES = paste(ifelse(BASE1_RUN_ID > '', 1, 0),
                       ifelse(BASE2_RUN_ID > '', 1, 0),
                       ifelse(BASE3_RUN_ID > '', 1, 0), sep = ""),
         STATE = paste(BASES, OUTS_CT)) -> data2022

data2022 %>% 
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
         NEW.STATE = paste(NEW.BASES, NOUTS)) -> data2022

data2022 %>% 
  filter(STATE != NEW.STATE | (RUNS.SCORED > 0)) -> data2022

data2022 %>% 
  filter(Outs.Inning == 3) -> data2022C 

data2022C %>% 
  group_by(STATE) %>% 
  summarize(Mean = mean(RUNS.ROI)) %>% 
  mutate(Outs = substr(STATE, 5, 5)) %>% 
  arrange(Outs) -> RUNS2022


RUNS_out_2022 <- matrix(round(RUNS$Mean, 2),8, 3)
dimnames(RUNS_out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out)[[1]] <- c("000", "001", "010", "011"
                             ,"100", "101", "110", "111")
#run expectancy matrices for 2023

data2023 %>% 
  mutate(RUNS =  AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED = (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
           (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3)) -> data2023
data2023 %>% 
  group_by(HALF.INNING) %>% 
  summarize(Outs.Inning = sum(EVENT_OUTS_CT),
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start) -> half_innings

data2023 %>%  inner_join(half_innings, by = "HALF.INNING") %>% 
  mutate(RUNS.ROI = MAX.RUNS - RUNS) -> data2023

data2023 %>% 
  mutate(BASES = paste(ifelse(BASE1_RUN_ID > '', 1, 0),
                       ifelse(BASE2_RUN_ID > '', 1, 0),
                       ifelse(BASE3_RUN_ID > '', 1, 0), sep = ""),
         STATE = paste(BASES, OUTS_CT)) -> data2023

data2023 %>% 
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
         NEW.STATE = paste(NEW.BASES, NOUTS)) -> data2023

data2023 %>% 
  filter(STATE != NEW.STATE | (RUNS.SCORED > 0)) -> data2023

data2023 %>% 
  filter(Outs.Inning == 3) -> data2023C 

data2023C %>% 
  group_by(STATE) %>% 
  summarize(Mean = mean(RUNS.ROI)) %>% 
  mutate(Outs = substr(STATE, 5, 5)) %>% 
  arrange(Outs) -> RUNS2023


RUNS_out_2023 <- matrix(round(RUNS$Mean, 2),8, 3)
dimnames(RUNS_out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out)[[1]] <- c("000", "001", "010", "011"
                             ,"100", "101", "110", "111")

#Filter data2022
filtered_data2022 <- data2022 %>% 
  filter(EVENT_CD %in% c(4, 6))

#Filter data2023
filtered_data2023 <- data2023 %>% 
  filter(EVENT_CD %in% c(4, 6))

#finding the most frequent occurrences for each year and combining datasets
filtered_data2022 %>% 
  count(NEW.STATE) %>% 
  mutate(Percentage = n / sum(n) * 100)

filtered_data2023 %>% 
  count(NEW.STATE) %>% 
  mutate(Percentage = n / sum(n) * 100)

combined_data <- bind_rows(filtered_data2022, filtered_data2023)

combined_data <- combined_data %>% mutate(SEASON = substr(GAME_ID, 4, 7))

#The most frequent occurrence is "100 2" for both years
s0_2022 <- filtered_data2022 %>% filter(STATE == "100 2")

s0_2023 <- filtered_data2023 %>% filter(STATE == "100 2")

s0 <- bind_rows(s0_2022, s0_2023)

#Looking at the transition probabilities
s0 %>% count(NEW.STATE) %>% 
  mutate(Percentage = n / sum(n) * 100)

#Calculate transition probabilities from STATE "100 2"
transition_probs_2022 <- s0_2022 %>% 
  count(NEW.STATE) %>% 
  mutate(Percentage = n / sum(n) * 100)

transition_probs_2023 <- s0_2023 %>% 
  count(NEW.STATE) %>% 
  mutate(Percentage = n / sum(n) * 100)

#Giving dimnames attribute for array
dimnames(RUNS_out_2022) <- list(
  c("000", "001", "010", "011", "100", "101", "110", "111"),
  c("0 outs", "1 out", "2 outs")
)

dimnames(RUNS_out_2023) <- list(
  c("000", "001", "010", "011", "100", "101", "110", "111"),
  c("0 outs", "1 out", "2 outs")
)
#Define probabilities (p18,19 and p2,3)
p18_25_2022 <- transition_probs_2022 %>% filter(NEW.STATE == "000 3") %>% pull(Percentage) / 100
p18_19_2022 <- transition_probs_2022 %>% filter(NEW.STATE == "010 2") %>% pull(Percentage) / 100

#Define run expectancies for 2022
R25_2022 <- RUNS_out_2022["000", "0 outs"]
R19_2022 <- RUNS_out_2022["010", "2 outs"]
R18_2022 <- RUNS_out_2022["100", "2 outs"]

#Expected return calculation for 2022
expected_return_SB_2022 <- (p18_25_2022 * R25_2022 + p18_19_2022 * R19_2022) - R18_2022
expected_return_SB_2022

#Define probabilities for 2023
p18_25_2023 <- transition_probs_2023 %>% filter(NEW.STATE == "000 3") %>% pull(Percentage) / 100
p18_19_2023 <- transition_probs_2023 %>% filter(NEW.STATE == "010 2") %>% pull(Percentage) / 100

#Define run expectancies for 2023
R25_2023 <- RUNS_out_2023["000", "0 outs"]
R19_2023 <- RUNS_out_2023["010", "2 outs"]
R18_2023 <- RUNS_out_2023["100", "2 outs"]

#Expected return calculation for 2023
expected_return_SB_2023 <- (p18_25_2023 * R25_2023 + p18_19_2023 * R19_2023) - R18_2023
expected_return_SB_2023

