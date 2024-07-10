#library(tidyverse)
#source("define-state-names.r")
#source("addstatevar.r")
#source("RunExp-fcn.r")
#source("sbaReturn1002.r") # function is called "sba.return.1002()"


returns.mtx <- c(season=integer(),state=character(),runExp = double(), runExp.sba=double(),sbaReturn = double())

# loop over 2018-2023
setwd("/home/jaosborn/research/sports/mlb/parkfactors/datafiles/retrosheet/")
for(season in 2018:2023){
fn <- paste0("data",season,".RData")
load(fn)
df <- get(paste0("data",season))
out.list <- sba.return.1002(df)
datarow <- c(season=season,state=out.list$state,runExp = out.list$runExp, runExp.sba=out.list$runExp.sba,sbaReturn = out.list$sba.return)
returns.mtx <- rbind(returns.mtx,datarow)
}
returns.df <- data.frame(returns.mtx)
row.names(returns.df) <- NULL
