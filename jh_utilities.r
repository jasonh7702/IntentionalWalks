#The functions you'll need to obtain the estimated
#distribution of runs in the remainder of the game
#
#1) mod9()   this function just returns the remainder of 
#            any integer after division by 9.  
#            e.g. mod9(4)=4, mod9(15)=6
#
#2) CreatePlayerMtx() 
#            this function creates a 25x25 transition
#            probability matrix for a player, based
#            on his summary statistics, collected in "dev"
#            dvec - HR,Tr,Db,S,BB,PA,GDP,SF
#
#3) UseTeamDFPitcher()
#            this function just grabs the team for which you're    
#            computing P(R=r) from the large object "alldata"
#            For example, 
#              astros.df <- UseTeamDFPitcher(team="HOU",2019)
#
#4) MakeTeamArray()
#            makes array of transition probability matrices
#            for an entire team, given a batting order
#
#5) decomP.fcn()
#            decomposition function
#            P0,P1,...,P4 are matrices of transition probabilities
#            corresponding to 0,1,2,3 or 4 runs scoring during
#            the transition
# 
#6) rundistn.fcn() 
#            this is the function that pulls everything together
#            and returns a vector giving P(R=r) for r=0,...,20
# 
#7) rundistplot.fcn()
#            plots  runs versus probabilities
#              
#first, load the data with the command below
#
#> load("alldata.RData")
#
#next, source this file
#
#> source("jh_utilities.r")
#
#choose a team and year and leave the rest as default
#
#> astros.df <- UseTeamDFPitcher("HOU",2019)
#
#compute run distribution, say for the Astros, 2019
#
#> dist.astros2019 <- rundistn.fcn(astros.df)
#
#> rundistplot.fcn(0:20,dist.astros2019,maxruns=10)


mod9 <- function(i){
  y <- i %% 9
  y[y==0] <- 9
  y
}

CreatePlayerMtx <- function(dvec,GDPflag=TRUE,SFflag=TRUE) {
  dvec <- as.numeric(dvec[c("HR","Tr","Db","S","BB","PA","GDP","SF")])
  hr <- dvec[1]/dvec[6]
  trip <- dvec[2]/dvec[6]
  d <- dvec[3]/dvec[6]
  s <- dvec[4]/dvec[6]
  w <- dvec[5]/dvec[6]
  o <- (dvec[6] - sum(dvec[1:5]))/dvec[6]

  o.gdp <- dvec[7]/(dvec[6]*0.1904)
  o.nogdp <- o-o.gdp
  o.sf <- dvec[8]/(0.0498*dvec[6])
  o.nosf <- o-dvec[8]/(0.0498*dvec[6])
  o.nosfnogdp <- o-o.gdp-o.sf
  # looking above, o denotes P(1 out) when no DP possible
  # looking above, o.gdp denotes P(1 out) when DP possible

  # o.nosf denotes P(out, no SF)
  
  smalltmtx <- diag(0, 8)
  smalltmtx[1, ] <- c(hr, w + s, d, trip, 0, 0, 0, 0)
  smalltmtx[2, ] <- c(hr, 0, 0.42*d, trip, w + 0.736*s, 0.264*s, 0.58*d, 0)
  smalltmtx[3, ] <- c(hr, 0.576*s, d, trip, w, 0.424*s, 0, 0)
  smalltmtx[4, ] <- c(hr, s, d, trip, 0, w, 0, 0) 
  smalltmtx[5, ] <- c(hr, 0, 0.449*d, trip, 0.359*s, 0.24*s, 0.551*d, w + 0.401*s)
  smalltmtx[6, ] <- c(hr, 0, 0.448*d, trip, 0.766*s, 0.234*s, 0.552*d, w)
  smalltmtx[7, ] <- c(hr, 0.518*s, d, trip, 0, 0.482*s, 0, w)
  smalltmtx[8, ] <- c(hr, 0, 0.438*d, trip, 0.344*s, 0.223*s, 0.562*d, w+0.433*s)
  #updated above using Albert's calculations from 2015,
  # https://gist.github.com/bayesball/36fba464d294944268f09630aa65ab61
  tmtx <- diag(0, 24 + 1)
  for (i in 1:3) {
    tmtx[((i - 1) * 8 + 1:8), ((i - 1) * 8 + 1:8)] <- smalltmtx
  }
  for (j in 1:2) {
    tmtx[(j - 1) * 8 + 1:8, (j - 1) * 8 + 9:16] <- o * diag(8)
  }
  
  tmtx[17:24, 24 + 1] <- o * rep(1, 8)
  tmtx[24 + 1, 24 + 1] <- 1
  if(SFflag){
    tmtx[4,12] <- o.nosf
    tmtx[4,9] <- o.sf
    tmtx[12,17] <- o.sf
    tmtx[12,20] <- o.nosf
    tmtx[6,10] <- o.sf
    tmtx[6,14] <- o.nosf
    tmtx[14,18] <- o.sf
    tmtx[14,22] <- o.nosf
    tmtx[7,11] <- o.sf
    tmtx[7,15] <- o.nosf
    tmtx[8,13] <- o.sf 
    tmtx[8,16] <- o.nosf 
    tmtx[16,21] <- o.sf 
    tmtx[16,24] <- o.nosf 
  }
  # GIDP, events in decreasing order of 2019 empirical frequency 
  if(GDPflag){
    tmtx[2,10] <- o.nogdp
    tmtx[2,17] <- o.gdp
    tmtx[10,18] <- o.nogdp
    tmtx[10,25] <- o.gdp # run scores
    tmtx[13,21] <- o.nogdp
    tmtx[13,25] <- o.gdp # run scores
    tmtx[5,13] <- o.nogdp
    tmtx[5,20] <- 0.86 * o.gdp
    tmtx[5,19] <- 0.07 * o.gdp
    tmtx[5,18] <- 0.07 * o.gdp
    tmtx[14,22] <- o.nogdp
    tmtx[14,25] <- o.gdp
    tmtx[16,24] <- o.nogdp
    tmtx[16,25] <- o.gdp 
    tmtx[8,16] <- o.nogdp
    tmtx[8,20] <- 0.64*o.gdp # 2 runs score
    tmtx[8,23] <- 0.22*o.gdp # run scores
    tmtx[8,19] <- 0.04*o.gdp # 2 runs score
    tmtx[8,18] <- 0.04*o.gdp # 2 runs score
    tmtx[8,21] <- 0.03*o.gdp # run scores
    tmtx[8,22] <- 0.03*o.gdp # run scores
    
    tmtx[6,14] <- o.nogdp
    tmtx[6,17] <- 0.9*o.gdp # run scores
    tmtx[6,20] <- 0.1*o.gdp 
    tmtx[11,19] <- o.nogdp
    tmtx[11,25] <- o.gdp 
  }
if(GDPflag & SFflag){
    tmtx[6,14] <- o.nosfnogdp 
    tmtx[14,22] <- o.nosfnogdp 
    tmtx[8,16] <- o.nosfnogdp 
    tmtx[16,24] <- o.nosfnogdp 
}

# -adding possibility of baserunner advance on plays where
# batter is out, based on 2019 data
t513 <- tmtx[5,13]
o.1011<- .19  #sitch15
o.1101<- .22  #sitch14
o.1110<- .36  #sitch13
o.sum <- .19+.22+.36
tmtx[5,13] <- o.1110/o.sum * t513
tmtx[5,14] <- o.1101/o.sum * t513
tmtx[5,15] <- o.1011/o.sum * t513
# 2) Situation #3 is 0 010
t311 <- tmtx[3,11]
o.noadv <- .509/(.509+.477)
tmtx[3,11] <- o.noadv*t311
tmtx[3,12] <- (1-o.noadv)*t311

# 3) Situation #2 0 100
o.noadv <- .605/(.605+.156)
t210<- tmtx[2,10]
tmtx[2,10]<- o.noadv*t210
tmtx[2,11]<- (1-o.noadv)*t210

# 4) Situation #10 1 100
t1018 <- tmtx[10,18]
o.noadv <- .6/(.6+.137)
tmtx[10,18] <- o.noadv*t1018
tmtx[10,19] <- (1-o.noadv)*t1018

  tmtx
}
  
UseTeamDFPitcher <- function (team, year,pitcher="RHP",timeframe="career") {
  alldata %>% filter(Team==team,Year==year,PHand==pitcher) -> teamdf 
  teamdf %>% select(-"Pos") -> teamdf.nopos
  teamdf %>% select(Name,Pos) %>% filter(Pos != "N/A") -> teamdf.pos
  teamdf <- inner_join(teamdf.nopos,teamdf.pos,by="Name") 
  teamdf %>% select(Pos,everything()) %>% filter(Timeframe==timeframe) %>% arrange(-PA) -> teamdf
  teamdf %>% arrange(desc(PA)) -> teamdf
  teamdf %>% rename(Db=Doub,Tr=Trip) -> teamdf
  
  teamdf
}


MakeTeamArray <- function(teamdf,border=1:9,...){
#MakeTeamArray <- function(teamdf,border=1:9){
  teamarray <- array(0,dim=c(9,24+1,24+1))
  for(i in 1:9){ 
    teamarray[i,,] <- CreatePlayerMtx(teamdf[border[i],],...)
  }
  teamarray
}

decomP.fcn <- function (tpm) 
{
  P4 <- matrix(0, nrow = 25, ncol = 25)
  P3 <- matrix(0, nrow = 25, ncol = 25)
  P2 <- matrix(0, nrow = 25, ncol = 25)
  P1 <- matrix(0, nrow = 25, ncol = 25)
  P0 <- matrix(0, nrow = 25, ncol = 25)
  Rmtx <- matrix(0,nrow=25,ncol=25)
  for(i in 1:3){
    c8=8*(i-1)
    Rmtx[c8+1, c8+1:8] <- c(1, rep(0, 7))
    Rmtx[c8+2, c8+1:8] <- c(2, 1, 1, 1, rep(0, 4))
    Rmtx[c8+3, c8+1:8] <- c(2, 1, 1, 1, rep(0, 4))
    Rmtx[c8+4, c8+1:8] <- c(2, 1, 1, 1, rep(0, 4))
    Rmtx[c8+5, c8+1:8] <- c(3, 2, 2, 2, 1, 1, 1, 0)
    Rmtx[c8+6, c8+1:8] <- c(3, 2, 2, 2, 1, 1, 1, 0)
    Rmtx[c8+7, c8+1:8] <- c(3, 2, 2, 2, 1, 1, 1, 0)
    Rmtx[c8+8, c8+1:8] <- c(4, 3, 3, 3, 2, 2, 2, 1)
  }
  # off-diagonals of run matrix for scoring plays with one out below
  # 0 outs to 1 out
  Rmtx[2,9] <- 1
  Rmtx[3,9] <- 1 
  Rmtx[4,9] <- 1
  Rmtx[5,9:12] <- c(2,1,1,1)
  Rmtx[6,9:12] <- c(2,1,1,1)
  Rmtx[7,9:12] <- c(2,1,1,1)
  Rmtx[8,9:15] <- c(3,2,2,2,1,1,1)
  
  # 1 outs to 2 outs
  Rmtx[8+2,8+9] <- 1
  Rmtx[8+3,8+9] <- 1 
  Rmtx[8+4,8+9] <- 1
  Rmtx[8+5,8+9:12] <- c(2,1,1,1)
  Rmtx[8+6,8+9:12] <- c(2,1,1,1)
  Rmtx[8+7,8+9:12] <- c(2,1,1,1)
  Rmtx[8+8,8+9:15] <- c(3,2,2,2,1,1,1)
  
  # 0 outs to 2 outs
  Rmtx[5,17] <- 1
  Rmtx[6,17] <- 1
  Rmtx[7,18] <- 1
  Rmtx[8,17:24] <- c(2,1,1,1,0,0,0,0)
  
  mtx4 <- (Rmtx == 4)
  mtx3 <- (Rmtx == 3)
  mtx2 <- (Rmtx == 2)
  mtx1 <- (Rmtx == 1)
  mtx0 <- (Rmtx == 0)
  
  P4 <- mtx4 * tpm
  P3 <- mtx3 * tpm
  P2 <- mtx2 * tpm
  P1 <- mtx1 * tpm
  P0 <- mtx0 * tpm
  list(P0 = P0, P1 = P1, P2 = P2, P3 = P3, P4 = P4)
}


rundistn.fcn <- function(teamdf,border=1:9,sitvec=c(1,rep(0,24)),user.inning=1,...){
  
  teamarray <- MakeTeamArray(teamdf,border)
  # u0 denotes initial probability distn across 25 states
  
  #define.rows <- function(inning,h){  # modifying elements of a global matrix inside a function does not work 
  define.rows <- function(inning,h,tpm){  # modifying elements of a global matrix inside a function does not work 
    Unew <- matrix(0,nrow=21,ncol=25)
    hbd <- 21*(inning-1)
    Unew[1,] <- U[h,hbd+1,] + U[h-1,hbd+1,] %*% tpm$P0
    Unew[2,] <- U[h,hbd+2,] + U[h-1,hbd+2,] %*% tpm$P0 + U[h-1,hbd+1,] %*% tpm$P1 
    Unew[3,] <- U[h,hbd+3,] + U[h-1,hbd+3,] %*% tpm$P0 + U[h-1,hbd+2,] %*% tpm$P1  + U[h-1,hbd+1,] %*% tpm$P2 
    Unew[4,] <- U[h,hbd+4,] + U[h-1,hbd+4,] %*% tpm$P0 + U[h-1,hbd+3,] %*% tpm$P1  + U[h-1,hbd+2,] %*% tpm$P2 + U[h-1,hbd+1,] %*% tpm$P3 
    for(j in 5:21){
      Unew[j,] <- U[h,hbd+j,] + U[h-1,hbd+j,] %*% tpm$P0 + U[h-1,hbd+j-1,] %*% tpm$P1  + U[h-1,hbd+j-2,] %*% tpm$P2 + U[h-1,hbd+j-3,] %*% tpm$P3 + U[h-1,hbd+j-4,] %*% tpm$P4
    }
    Unew
  }
  
  translate.column <- function(inning,h){
    Unew <- rep(NA,25)
    hbt <- 21*(inning-1)
    if(h > 1){
      Unew <- U[h,hbt+1:21,25]-U[h-1,hbt+1:21,25]
    }
    else{
      Unew <- U[h,hbt+1:21,25]
    }
    Unew
  }
  
  U <- array(0,dim=c(60,189,25)) # will allow for other innings later.
  array1 <- teamarray
  # h <- 1
  tpm <- decomP.fcn(array1[1,,])
  U0 <- sitvec
  
  row.count <- 21*(user.inning-1)
  
  U[1,row.count+1,] <- U0 %*% tpm$P0 
  U[1,row.count+2,] <- U0 %*% tpm$P1 
  U[1,row.count+3,] <- U0 %*% tpm$P2 
  U[1,row.count+4,] <- U0 %*% tpm$P3 
  U[1,row.count+5,] <- U0 %*% tpm$P4
  #cat("maxU1rowcount_125,25",max(U[1,row.count+1:21,25]>0))
  #if(max(U[1,row.count+1:21,25]>0)){
  if(max(U[1,row.count+1:21,25]>0) & (user.inning < 9)){
    #cat("in here before")
    U[1,21*user.inning+1:21,1] <- translate.column(user.inning,1)
    #cat("in here after")
  }
  
  for(h in 2:60){
    tpm <- decomP.fcn(array1[mod9(h),,])
    U[h,row.count+1:21,] <- define.rows(inning=user.inning,h,tpm)
    if(user.inning < 9){
      for(inning in user.inning:8){
        hb <- 21*inning
        U[h,hb+1:21,1] <- translate.column(inning,h)
        U[h,hb+1:21,] <- define.rows(inning+1,h,tpm)
      }
    }
  }
  U[60,8*21+1:21,25]   # 3 out state at end of 9 innings
}

#fancier run distn plot
rundistplot.fcn <- function (runs,runprobs, maxruns=5, ymax=0.3,add2plot=FALSE,skooch=0.05,...) 
{
  nruns <- length(runs)
  rstop <- sum(runprobs[(1 + maxruns):nruns])
  yplot <- c(runprobs[1:maxruns], rstop)
  if(add2plot){
    #   lines(0:maxruns+skooch, c(runprobs[1:maxruns], sum(runprobs[(maxruns + 1):nruns])),col="red",type="h",...)
    #   points(0:maxruns+skooch, c(runprobs[1:maxruns], sum(runprobs[(maxruns + 1):nruns])),col="red",...)
    lines(0:maxruns+skooch, c(runprobs[1:maxruns], sum(runprobs[(maxruns + 1):nruns])),type="h",...)
    points(0:maxruns+skooch, c(runprobs[1:maxruns], sum(runprobs[(maxruns + 1):nruns])),...)
    rmean <- 0:(nruns - 1) %*% runprobs
    points(rmean, 0.005, pch = 6,...)
  }
  else{
    plot(0:maxruns, c(runprobs[1:maxruns], sum(runprobs[(maxruns + 1):nruns])), 
         type = "h", xlab = "r:Runs", ylab = "P(R=r)", ylim = c(0, 
                                                                ymax), xlim = c(-0.5, maxruns + 1.5), cex = 1.5, axes = F,pch=16,...)
    points(0:maxruns, c(runprobs[1:maxruns], sum(runprobs[(maxruns + 1):nruns])),...)
    rmean <- 0:(nruns - 1) %*% runprobs
    points(rmean, 0.005, pch = 16,...)
  }
  par(cex = 1.5)
  axis(2)
  par(cex = 1.1)
  xlabels <- as.character(0:maxruns)
  xlabels[maxruns + 1] <- paste0(">=", maxruns)
  axis(1, 0:maxruns, labels = xlabels)
  cat(paste0("mean=",round(rmean,3),"\n"))
  #if(!add2plot){legend(.8*maxruns,0.8*ymax,paste("mean=",round(rmean,3)))}
  if(!add2plot){legend(.2*maxruns,0.9*ymax,paste("mean=",round(rmean,3)))}
}
