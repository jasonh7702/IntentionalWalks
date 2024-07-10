#library(tidyverse)
#source("define-state-names.r")
#source("addstatevar.r")
#source("RunExp-fcn.r")


sba.return.1002 <- function(df)
{
Rmtx <- runExpMtx(df)
df.states <- addstatevar(df)
df.states %>% filter(EVENT_CD %in% c(4,6)) -> df.states.sba

# 2) mutate NEW.STATE into a new factor called NEW.STATE.FACTOR
df.states.sba %>% mutate(NEW.STATE.FACTOR = factor(NEW.STATE,levels=state.names)) -> 
  df.states.sba

# 3) obtain relative frequencies (including 0s) for all 25 states
#    save as "transition_probs_2023"
df.states.sba %>% filter(STATE=="100 2") -> df.states.sba.1002
df.states.sba.1002 %>% select(NEW.STATE.FACTOR) %>% table %>% 
  prop.table  -> tpm.1002 #transition probability mtx as vector
Rmtx.1002 <- tpm.1002 %*% c(as.vector(Rmtx),0) + 1*tpm.1002[17]
sbaReturn.1002 <- Rmtx.1002  - Rmtx[2,3] 
return(list(state="100 2",tpm=tpm.1002,runExp=Rmtx[2,3],runExp.sba=Rmtx.1002,sba.return=sbaReturn.1002))
}

