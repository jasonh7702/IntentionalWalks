library(tidyverse)
#loading in data
#load("/Users/jasonhuang/Downloads/data2018.RData")
#load("/Users/jasonhuang/Downloads/data2019.RData")
#load("/Users/jasonhuang/Downloads/data2020.RData")
#load("/Users/jasonhuang/Downloads/data2021.RData")
#load("/Users/jasonhuang/Downloads/data2022.RData")
#load("/Users/jasonhuang/Downloads/data2023.RData")
setwd("/home/jaosborn/research/sports/mlb/parkfactors/datafiles/retrosheet/")
#load("data2018.RData")
#load("data2019.RData")
#load("data2020.RData")
#load("data2021.RData")
load("data2022.RData")
load("data2023.RData")
setwd("/home/jaosborn/research/sports/jason_huang")
#run expectancy matrices for 2022
source("addstatevar.R")
source("RunExp-fcn.r")
data2022 <- data2022 %>% mutate(season="2022")
data2023 <- data2023 %>% mutate(season="2023")
data2022.states <- addstatevar(data2022)
data2023.states <- addstatevar(data2023)
Rmtx.2022 <- runExpMtx(data2022)
Rmtx.2023 <- runExpMtx(data2023)


# 1a) create a vector with the names for  all 25 states
#data2023.states %>% select(NEW.STATE) %>% table %>% names -> state.names
source("define-state-names.r")

# 1b) filter data to include only those where SB attempted
data2022.states %>% filter(EVENT_CD %in% c(4,6)) -> data2022.states.sba

# 2) mutate NEW.STATE into a new factor
data2022.states.sba %>% mutate(NEW.STATE.FACTOR = factor(NEW.STATE,levels=state.names)) -> 
  data2022.states.sba

# 3) obtain relative frequencies (including 0s) for all 25 states
#    save as "transition_probs_2023"
data2022.states.sba %>% filter(STATE=="100 2") -> data2022.states.sba.1002
data2022.states.sba.1002 %>% select(NEW.STATE.FACTOR) %>% table %>% 
  prop.table  -> tpm2022.1002
runExp.2022.1002 <- tpm2022.1002 %*% c(as.vector(Rmtx.2022),0) + 1*tpm2022.1002[17]
sbaReturn.2022.1002 <- runExp.2022.1002  - Rmtx.2022[2,3] 

# repeat for 2023
data2023.states %>% filter(EVENT_CD %in% c(4,6)) -> data2023.states.sba
data2023.states.sba %>% mutate(NEW.STATE.FACTOR = factor(NEW.STATE,levels=state.names)) -> 
  data2023.states.sba
data2023.states.sba %>% filter(STATE=="100 2") -> data2023.states.sba.1002
data2023.states.sba.1002 %>% select(NEW.STATE.FACTOR) %>% table %>% 
  prop.table  -> tpm2023.1002
runExp.2023.1002 <- tpm2023.1002 %*% c(as.vector(Rmtx.2023),0) + 1*tpm2023.1002[17]
sbaReturn.2023.1002 <- Rmtx.2023[2,3] - runExp.2023.1002 

# should be able to repeat this for all years without much change to code
# might be nice to functionalize it

# computing return on SB attempts from other states will require a little more work
