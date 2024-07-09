#loading in data
load("/Users/jasonhuang/Downloads/data2018.RData")
load("/Users/jasonhuang/Downloads/data2019.RData")
load("/Users/jasonhuang/Downloads/data2020.RData")
load("/Users/jasonhuang/Downloads/data2021.RData")
load("/Users/jasonhuang/Downloads/data2022.RData")
load("/Users/jasonhuang/Downloads/data2023.RData")
library(tidyverse)

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

  df$NEW.STATE <- ifelse(df$NOUTS==3,"3 outs",df$NEW.STATE)
  
  df %>% 
    filter(STATE != NEW.STATE | (RUNS.SCORED > 0)) -> df
  return(df)
}

data2018.states <- addstatevar(data2018)
data2019.states <- addstatevar(data2019)
data2020.states <- addstatevar(data2020)
data2021.states <- addstatevar(data2021)
data2022.states <- addstatevar(data2022)
data2023.states <- addstatevar(data2023)


source("RunExp-fcn.r")
Rmtx.2023 <- runExpMtx(data2023)
as.vector(t(Rmtx.2023))

data2023.states <- addstatevar(data2023)
tmp2 <- ifelse(data2023.states$NOUTS==3,"3 outs",data2023.states$NEW.STATE)
tmp2 %>% table

c(as.vector(t(Rmtx.2023)),0) %*% transition_probs_2023


calculate_transition_probs <- function(data.states, state_filter) {
  #Filter data for specific event codes
  filtered_data <- data.states %>%
    filter(EVENT_CD %in% c(4, 6))
  
  #Calculate transition probabilities for the given state
  s0 <- filtered_data %>% filter(STATE == state_filter)
  state.names <- data.states %>% select(NEW.STATE) %>% table %>% names
  s0 <- s0 %>% mutate(NEW.STATE.FACTOR = factor(NEW.STATE, levels = state.names))
  transition_probs <- s0 %>% select(NEW.STATE.FACTOR) %>% table %>% prop.table
  
  return(transition_probs)
}

#Apply the function for each year
transition_probs_2018 <- calculate_transition_probs(data2018.states, "100 2")
transition_probs_2019 <- calculate_transition_probs(data2019.states, "100 2")
transition_probs_2020 <- calculate_transition_probs(data2020.states, "100 2")
transition_probs_2021 <- calculate_transition_probs(data2021.states, "100 2")
transition_probs_2022 <- calculate_transition_probs(data2022.states, "100 2")
transition_probs_2023 <- calculate_transition_probs(data2023.states, "100 2")

#Function to calculate expected return
calculate_expected_return <- function(transition_probs, Rmatrix) {
  #Ensure Rmatrix is a vector
  Rvector <- as.vector(t(Rmatrix))
  
  #Calculate expected return
  expected_return <- c(Rvector, 0) %*% transition_probs
  return(expected_return)
}

#Assuming runExpMtx is the function to generate the run expectancy matrix
Rmtx_2018 <- runExpMtx(data2018)
Rmtx_2019 <- runExpMtx(data2019)
Rmtx_2020 <- runExpMtx(data2020)
Rmtx_2021 <- runExpMtx(data2021)
Rmtx_2022 <- runExpMtx(data2022)
Rmtx_2023 <- runExpMtx(data2023)

#Calculate expected returns
expected_return_2018 <- calculate_expected_return(transition_probs_2018, Rmtx_2018)
expected_return_2019 <- calculate_expected_return(transition_probs_2019, Rmtx_2019)
expected_return_2020 <- calculate_expected_return(transition_probs_2020, Rmtx_2020)
expected_return_2021 <- calculate_expected_return(transition_probs_2021, Rmtx_2021)
expected_return_2022 <- calculate_expected_return(transition_probs_2022, Rmtx_2022)
expected_return_2023 <- calculate_expected_return(transition_probs_2023, Rmtx_2023)

#Combine the expected returns into a dataframe
expected_returns_df <- data.frame(
  Year = 2018:2023,
  Expected_Return_SB = c(
    expected_return_2018, 
    expected_return_2019, 
    expected_return_2020, 
    expected_return_2021, 
    expected_return_2022, 
    expected_return_2023
  )
)

print(expected_returns_df)

ggplot(expected_returns_df, aes(x = Year, y = Expected_Return_SB)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Expected Return of a Stolen Base Attempt (2018-2023)",
       x = "Year",
       y = "Expected Return SB") +
  theme_minimal()

#Repeating this process for 100 1 
transition_probs2_2018 <- calculate_transition_probs(data2018.states, "100 1")
transition_probs2_2019 <- calculate_transition_probs(data2019.states, "100 1")
transition_probs2_2020 <- calculate_transition_probs(data2020.states, "100 1")
transition_probs2_2021 <- calculate_transition_probs(data2021.states, "100 1")
transition_probs2_2022 <- calculate_transition_probs(data2022.states, "100 1")
transition_probs2_2023 <- calculate_transition_probs(data2023.states, "100 1")

#Calculate expected returns
expected_return2_2018 <- calculate_expected_return(transition_probs2_2018, Rmtx_2018)
expected_return2_2019 <- calculate_expected_return(transition_probs2_2019, Rmtx_2019)
expected_return2_2020 <- calculate_expected_return(transition_probs2_2020, Rmtx_2020)
expected_return2_2021 <- calculate_expected_return(transition_probs2_2021, Rmtx_2021)
expected_return2_2022 <- calculate_expected_return(transition_probs2_2022, Rmtx_2022)
expected_return2_2023 <- calculate_expected_return(transition_probs2_2023, Rmtx_2023)

#Combine the expected returns into a dataframe
expected_returns2_df <- data.frame(
  Year = 2018:2023,
  Expected_Return_SB = c(
    expected_return2_2018, 
    expected_return2_2019, 
    expected_return2_2020, 
    expected_return2_2021, 
    expected_return2_2022, 
    expected_return2_2023
  )
)

print(expected_returns2_df)

ggplot(expected_returns2_df, aes(x = Year, y = Expected_Return_SB)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Expected Return of a Stolen Base Attempt (2018-2023)",
       x = "Year",
       y = "Expected Return SB") +
  theme_minimal()

#Repeating this process for 100 0 
transition_probs3_2018 <- calculate_transition_probs(data2018.states, "100 0")
transition_probs3_2019 <- calculate_transition_probs(data2019.states, "100 0")
transition_probs3_2020 <- calculate_transition_probs(data2020.states, "100 0")
transition_probs3_2021 <- calculate_transition_probs(data2021.states, "100 0")
transition_probs3_2022 <- calculate_transition_probs(data2022.states, "100 0")
transition_probs3_2023 <- calculate_transition_probs(data2023.states, "100 0")

#Calculate expected returns
expected_return3_2018 <- calculate_expected_return(transition_probs3_2018, Rmtx_2018)
expected_return3_2019 <- calculate_expected_return(transition_probs3_2019, Rmtx_2019)
expected_return3_2020 <- calculate_expected_return(transition_probs3_2020, Rmtx_2020)
expected_return3_2021 <- calculate_expected_return(transition_probs3_2021, Rmtx_2021)
expected_return3_2022 <- calculate_expected_return(transition_probs3_2022, Rmtx_2022)
expected_return3_2023 <- calculate_expected_return(transition_probs3_2023, Rmtx_2023)

#Combine the expected returns into a dataframe
expected_returns3_df <- data.frame(
  Year = 2018:2023,
  Expected_Return_SB = c(
    expected_return3_2018, 
    expected_return3_2019, 
    expected_return3_2020, 
    expected_return3_2021, 
    expected_return3_2022, 
    expected_return3_2023
  )
)

print(expected_returns3_df)

ggplot(expected_returns3_df, aes(x = Year, y = Expected_Return_SB)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Expected Return of a Stolen Base Attempt (2018-2023)",
       x = "Year",
       y = "Expected Return SB") +
  theme_minimal()
          
# Save the dataframe to a CSV file
write.csv(expected_returns3_df, "expected_returns3_df.csv", row.names = FALSE)

#test