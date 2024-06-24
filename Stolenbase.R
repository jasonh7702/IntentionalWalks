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
  count(STATE) %>% 
  mutate(Percentage = n / sum(n) * 100)

filtered_data2023 %>% 
  count(STATE) %>% 
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

#adding the functionalized state code 
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

#Starting with 2018 
data2018 <- addstatevar(data2018)

data2018 %>% 
  filter(Outs.Inning == 3) -> data2018C 

data2018C %>% 
  group_by(STATE) %>% 
  summarize(Mean = mean(RUNS.ROI)) %>% 
  mutate(Outs = substr(STATE, 5, 5)) %>% 
  arrange(Outs) -> RUNS2018

RUNS_out_2018 <- matrix(round(RUNS2018$Mean, 2),8, 3)
dimnames(RUNS_out_2018)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out_2018)[[1]] <- c("000", "001", "010", "011"
                             ,"100", "101", "110", "111")
filtered_data2018 <- data2018 %>% 
  filter(EVENT_CD %in% c(4, 6))

filtered_data2018 %>% 
  count(NEW.STATE) %>% 
  mutate(Percentage = n / sum(n) * 100)

s0_2018 <- filtered_data2018 %>% filter(STATE == "100 2")

transition_probs_2018 <- s0_2018 %>% 
  count(NEW.STATE) %>% 
  mutate(Percentage = n / sum(n) * 100)

p18_25_2018 <- transition_probs_2018 %>% filter(NEW.STATE == "000 3") %>% pull(Percentage) / 100
p18_19_2018 <- transition_probs_2018 %>% filter(NEW.STATE == "010 2") %>% pull(Percentage) / 100

R25_2022 <- RUNS_out_2018["000", "0 outs"]
R19_2022 <- RUNS_out_2018["010", "2 outs"]
R18_2022 <- RUNS_out_2018["100", "2 outs"]

expected_return_SB_2018 <- (p18_25_2022 * R25_2022 + p18_19_2022 * R19_2022) - R18_2022
expected_return_SB_2018

#Expected return calculations for 2019 
data2019 <- addstatevar(data2019)

data2019 %>% 
  filter(Outs.Inning == 3) -> data2019C 

data2019C %>% 
  group_by(STATE) %>% 
  summarize(Mean = mean(RUNS.ROI)) %>% 
  mutate(Outs = substr(STATE, 5, 5)) %>% 
  arrange(Outs) -> RUNS2019

RUNS_out_2019 <- matrix(round(RUNS2019$Mean, 2), 8, 3)
dimnames(RUNS_out_2019)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out_2019)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")

filtered_data2019 <- data2019 %>% 
  filter(EVENT_CD %in% c(4, 6))

s0_2019 <- filtered_data2019 %>% filter(STATE == "100 2")

transition_probs_2019 <- s0_2019 %>% 
  count(NEW.STATE) %>% 
  mutate(Percentage = n / sum(n) * 100)

p18_25_2019 <- transition_probs_2019 %>% filter(NEW.STATE == "000 3") %>% pull(Percentage) / 100
p18_19_2019 <- transition_probs_2019 %>% filter(NEW.STATE == "010 2") %>% pull(Percentage) / 100

R25_2019 <- RUNS_out_2019["000", "0 outs"]
R19_2019 <- RUNS_out_2019["010", "2 outs"]
R18_2019 <- RUNS_out_2019["100", "2 outs"]

expected_return_SB_2019 <- (p18_25_2019 * R25_2019 + p18_19_2019 * R19_2019) - R18_2019
expected_return_SB_2019

#Expected return for 2020
data2020 <- addstatevar(data2020)

data2020 %>% 
  filter(Outs.Inning == 3) -> data2020C 

data2020C %>% 
  group_by(STATE) %>% 
  summarize(Mean = mean(RUNS.ROI)) %>% 
  mutate(Outs = substr(STATE, 5, 5)) %>% 
  arrange(Outs) -> RUNS2020

RUNS_out_2020 <- matrix(round(RUNS2020$Mean, 2), 8, 3)
dimnames(RUNS_out_2020)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out_2020)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")

filtered_data2020 <- data2020 %>% 
  filter(EVENT_CD %in% c(4, 6))

s0_2020 <- filtered_data2020 %>% filter(STATE == "100 2")

transition_probs_2020 <- s0_2020 %>% 
  count(NEW.STATE) %>% 
  mutate(Percentage = n / sum(n) * 100)

p18_25_2020 <- transition_probs_2020 %>% filter(NEW.STATE == "000 3") %>% pull(Percentage) / 100
p18_19_2020 <- transition_probs_2020 %>% filter(NEW.STATE == "010 2") %>% pull(Percentage) / 100

R25_2020 <- RUNS_out_2020["000", "0 outs"]
R19_2020 <- RUNS_out_2020["010", "2 outs"]
R18_2020 <- RUNS_out_2020["100", "2 outs"]

expected_return_SB_2020 <- (p18_25_2020 * R25_2020 + p18_19_2020 * R19_2020) - R18_2020
expected_return_SB_2020

#Expected return for 2021
data2021 <- addstatevar(data2021)

data2021 %>% 
  filter(Outs.Inning == 3) -> data2021C 

data2021C %>% 
  group_by(STATE) %>% 
  summarize(Mean = mean(RUNS.ROI)) %>% 
  mutate(Outs = substr(STATE, 5, 5)) %>% 
  arrange(Outs) -> RUNS2021

RUNS_out_2021 <- matrix(round(RUNS2021$Mean, 2), 8, 3)
dimnames(RUNS_out_2021)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out_2021)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")

filtered_data2021 <- data2021 %>% 
  filter(EVENT_CD %in% c(4, 6))

s0_2021 <- filtered_data2021 %>% filter(STATE == "100 2")

transition_probs_2021 <- s0_2021 %>% 
  count(NEW.STATE) %>% 
  mutate(Percentage = n / sum(n) * 100)

p18_25_2021 <- transition_probs_2021 %>% filter(NEW.STATE == "000 3") %>% pull(Percentage) / 100
p18_19_2021 <- transition_probs_2021 %>% filter(NEW.STATE == "010 2") %>% pull(Percentage) / 100

R25_2021 <- RUNS_out_2021["000", "0 outs"]
R19_2021 <- RUNS_out_2021["010", "2 outs"]
R18_2021 <- RUNS_out_2021["100", "2 outs"]

expected_return_SB_2021 <- (p18_25_2021 * R25_2021 + p18_19_2021 * R19_2021) - R18_2021
expected_return_SB_2021

#Creating a data frame for the expected return on stolen bases
expected_returns_df <- data.frame(
  Year = 2018:2023,
  Expected_Return_SB = c(
    expected_return_SB_2018, 
    expected_return_SB_2019, 
    expected_return_SB_2020, 
    expected_return_SB_2021, 
    expected_return_SB_2022, 
    expected_return_SB_2023
  )
)

print(expected_returns_df)

library(ggplot2)
#creating a plot to visualize the returns
ggplot(expected_returns_df, aes(x = Year, y = Expected_Return_SB)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Expected Return of a Stolen Base Attempt (2018-2023)",
       x = "Year",
       y = "Expected Return SB") +
  theme_minimal()

#Including caught stealing and pickoffs as well.
#Creating a function that pulls all the percentages for the 2018-2023 seasons for
#Steals (4), caught stealing (6), pickoffs (8)
calculate_event_counts <- function(data) {
  data %>% 
    filter(EVENT_CD %in% c(4, 6, 8)) %>% 
    count(EVENT_CD) %>% 
    mutate(Percentage = n / sum(n) * 100)
}

steals_2018 <- calculate_event_counts(data2018)
steals_2019 <- calculate_event_counts(data2019)
steals_2020 <- calculate_event_counts(data2020)
steals_2021 <- calculate_event_counts(data2021)
steals_2022 <- calculate_event_counts(data2022)
steals_2023 <- calculate_event_counts(data2023)

#Extract the percentage of steals for each year to see if there are 
#any trends in the percentage of successful stolen base attempts.
percentage_event_4 <- function(event_counts, year) {
  event_counts %>% 
    filter(EVENT_CD == 4) %>% 
    select(Percentage) %>% 
    mutate(Year = year)
}

percentage_2018 <- percentage_event_4(steals_2018, 2018)
percentage_2019 <- percentage_event_4(steals_2019, 2019)
percentage_2020 <- percentage_event_4(steals_2020, 2020)
percentage_2021 <- percentage_event_4(steals_2021, 2021)
percentage_2022 <- percentage_event_4(steals_2022, 2022)
percentage_2023 <- percentage_event_4(steals_2023, 2023)

percentage_data <- bind_rows(percentage_2018, percentage_2019, percentage_2020,
                             percentage_2021, percentage_2022, percentage_2023)

#Plot the line graph
ggplot(percentage_data, aes(x = Year, y = Percentage)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Percentage of EVENT_CD 4 from 2018 to 2023",
       x = "Year",
       y = "Percentage of EVENT_CD 4") +
  theme_minimal()

#We want to find all the expected returns for the 2nd most frequent SB Attempt
calculate_expected_return <- function(filtered_data, RUNS_out, year) {
  s1 <- filtered_data %>% filter(STATE == "100 1")
  
  transition_probs <- s1 %>% 
    count(NEW.STATE) %>% 
    mutate(Percentage = n / sum(n) * 100)
  
  p10_17 <- transition_probs %>% filter(NEW.STATE == "000 2") %>% pull(Percentage) / 100
  p10_11 <- transition_probs %>% filter(NEW.STATE == "010 1") %>% pull(Percentage) / 100
  p10_12 <- transition_probs %>% filter(NEW.STATE == "001 1") %>% pull(Percentage) / 100
  
  R17 <- RUNS_out["000", "2 outs"]
  R11 <- RUNS_out["010", "1 out"]
  R10 <- RUNS_out["100", "1 out"]
  R12 <- RUNS_out["001", "1 out"]
  
  expected_return <- (p10_17 * R17 + p10_11 * R11 + p10_12 * R12) - R10
  return(expected_return)
}

#Calculate expected returns for each year
expected_return2_SB_2018 <- calculate_expected_return(filtered_data2018, RUNS_out_2018, 2018)
expected_return2_SB_2019 <- calculate_expected_return(filtered_data2019, RUNS_out_2019, 2019)
expected_return2_SB_2020 <- calculate_expected_return(filtered_data2020, RUNS_out_2020, 2020)
expected_return2_SB_2021 <- calculate_expected_return(filtered_data2021, RUNS_out_2021, 2021)
expected_return2_SB_2022 <- calculate_expected_return(filtered_data2022, RUNS_out_2022, 2022)
expected_return2_SB_2023 <- calculate_expected_return(filtered_data2023, RUNS_out_2023, 2023)

#Combine the expected returns into a dataframe
expected_returns_df2 <- data.frame(
  Year = 2018:2023,
  Expected_Return2_SB = c(
    expected_return2_SB_2018, 
    expected_return2_SB_2019, 
    expected_return2_SB_2020, 
    expected_return2_SB_2021, 
    expected_return2_SB_2022, 
    expected_return2_SB_2023
  )
)
print(expected_returns_df2)
#Creating a visual to see a trend in the data. The expected value seems to go up
ggplot(expected_returns_df2, aes(x = Year, y = Expected_Return2_SB)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Expected Return of a Stolen Base Attempt (2018-2023)",
       x = "Year",
       y = "Expected Return SB") +
  theme_minimal()