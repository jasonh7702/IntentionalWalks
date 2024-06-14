library(tidyverse)
#loading in data
#load("/Users/jasonhuang/Downloads/data2018.RData")
#load("/Users/jasonhuang/Downloads/data2019.RData")
#load("/Users/jasonhuang/Downloads/data2020.RData")
#load("/Users/jasonhuang/Downloads/data2021.RData")
#load("/Users/jasonhuang/Downloads/data2022.RData")
#load("/Users/jasonhuang/Downloads/data2023.RData")
setwd("/home/jaosborn/research/sports/mlb/parkfactors/datafiles/retrosheet/")
load("data2018.RData")
load("data2019.RData")
load("data2020.RData")
load("data2021.RData")
load("data2022.RData")
load("data2023.RData")
setwd("/home/jaosborn/research/sports/jason_huang")
#run expectancy matrices for 2022
addstatevar <- function(df){
df %>% 
  mutate(RUNS =  AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED = (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
           (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3)) -> df
df %>% 
  group_by(HALF.INNING) %>% 
  summarize(Outs.Inning = sum(EVENT_OUTS_CT),
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start) -> half_innings

df %>%  inner_join(half_innings, by = "HALF.INNING") %>% 
  mutate(RUNS.ROI = MAX.RUNS - RUNS) -> df

df %>% 
  mutate(BASES = paste(ifelse(BASE1_RUN_ID > '', 1, 0),
                       ifelse(BASE2_RUN_ID > '', 1, 0),
                       ifelse(BASE3_RUN_ID > '', 1, 0), sep = ""),
         STATE = paste(BASES, OUTS_CT)) -> df

df %>% 
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
         NEW.STATE = paste(NEW.BASES, NOUTS)) -> df

df %>% 
  filter(STATE != NEW.STATE | (RUNS.SCORED > 0)) -> df
return(df)
}

data2022 <- addstatevar(data2022)

data2022 %>% 
  filter(Outs.Inning == 3) -> data2022C 

data2022C %>% 
  group_by(STATE) %>% 
  summarize(Mean = mean(RUNS.ROI)) %>% 
  mutate(Outs = substr(STATE, 5, 5)) %>% 
  arrange(Outs) -> RUNS2022


#RUNS_out <- matrix(round(RUNS$Mean, 2),8, 3) #Jason H forgot name change
RUNS_out <- matrix(round(RUNS2022$Mean, 2),8, 3)
dimnames(RUNS_out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out)[[1]] <- c("000", "001", "010", "011"
                             ,"100", "101", "110", "111")
#run expectancy matrices for 2023

data2023 <- addstatevar(data2023)

data2023 %>% 
  filter(Outs.Inning == 3) -> data2023C 

data2023C %>% 
  group_by(STATE) %>% 
  summarize(Mean = mean(RUNS.ROI)) %>% 
  mutate(Outs = substr(STATE, 5, 5)) %>% 
  arrange(Outs) -> RUNS2023


#RUNS_out <- matrix(round(RUNS$Mean, 2),8, 3)  #Jason H forgot name change
RUNS_out <- matrix(round(RUNS2023$Mean, 2),8, 3)
dimnames(RUNS_out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out)[[1]] <- c("000", "001", "010", "011"
                             ,"100", "101", "110", "111")

#Filter data2022
filtered_data2022 <- data2022 %>% 
  filter(EVENT_CD %in% c(4, 6))

#Filter data2023
filtered_data2023 <- data2023 %>% 
  filter(EVENT_CD %in% c(4, 6))

#finding the most frequent occurrences
filtered_data2022 %>% count(STATE) %>% mutate(Percentage = n / sum(n) * 100)

filtered_data2023 %>% count(STATE) %>% mutate(Percentage = n / sum(n) * 100)
combined_data <- bind_rows(filtered_data2022, filtered_data2023)

#The most frequent occurrence is "100 2" for both years
s0_2022 <- filtered_data2022 %>% filter(STATE == "100 2")

s0_2023 <- filtered_data2023 %>% filter(STATE == "100 2")

s0 <- bind_rows(s0_2022, s0_2023)

#Looking at the transition probabilities
s0 %>% count(NEW.STATE) %>% 
  mutate(Percentage = n / sum(n) * 100)

#Calculate transition probabilities from STATE "100 2"
transition_probs <- s0 %>% 
  count(NEW.STATE) %>% 
  mutate(Percentage = n / sum(n) * 100)

#Define probabilities (p18,19 and p2,3)
p18_25 <- transition_probs %>% filter(NEW.STATE == "000 3") %>% pull(Percentage) / 100
p18_19 <- transition_probs %>% filter(NEW.STATE == "010 2") %>% pull(Percentage) / 100

#Define run expectancies (R9 and R3)
R25 <- RUNS_out["000", "0 outs"]
R19 <- RUNS_out["010", "2 outs"]
R18 <- RUNS_out["100", "2 outs"]

#Expected return calculation
expected_return_SB <- (p18_25 * R25 + p18_19 * R19) - R18
expected_return_SB


