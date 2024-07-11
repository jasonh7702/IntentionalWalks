load("/home/jaosborn/research/sports/mlb/parkfactors/datafiles/retrosheet/data2023.RData")

source("addstatevar.R")
data2023.states <- addstatevar(data2023)
data2023.states %>% select(NEW.STATE) %>% table %>% names -> state.names
oindx <- c(1,13,7,4,19,16,10,22,1+c(1,13,7,4,19,16,10,22),2+c(1,13,7,4,19,16,10,22),25)
state.names <- state.names[oindx]

# as a transition probability matrix
# t(matrix(state.names,nrow=8)) 
