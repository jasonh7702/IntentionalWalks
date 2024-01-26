library(tidyverse)
#Contents
#1  addRuns - adds RUNS.VALUE, STATE,NEW.STATE to data frame 
#            with attention restricted to full innings
#2a add Counts0 to add count variables (c00,c10,...) using ABDWR2
#2b add Counts0 to add count variables (c00,c10,...) using amendment


crcblue <- '#0000FF'
crcblue <- "#2905a1"
#count_plot <- df %>%
#  ggplot(aes(x=strikes,y=balls, fill=value))+
#  geom_tile() +
#  geom_text(aes(label=round(value,3))) +
#  scale_fill_gradient2("xRV",low="grey10",high=crcblue,mid="white")


addRuns <- function(df){

#section 5.2, ABDWR2

df %>% 
  mutate(RUNS =  AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED = 
           (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
           (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3)) -> 
df

df %>% 
  group_by(HALF.INNING) %>% 
  summarize(Outs.Inning = sum(EVENT_OUTS_CT),
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start) -> half_innings

df %>%  inner_join(half_innings, by = "HALF.INNING") %>% 
  mutate(RUNS.ROI = MAX.RUNS - RUNS) -> df

#section 5.3, ABDWR2

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

df %>% 
  filter(Outs.Inning == 3) -> dfC 
#dfC <- df

#df %>% 
dfC %>% 
  group_by(STATE) %>% 
  summarize(Mean = mean(RUNS.ROI)) %>% 
  mutate(Outs = substr(STATE, 5, 5)) %>% 
  arrange(Outs) -> RUNS

#section 5.4, ABDWR2
#dfC %>%  Dec 12,2023
df %>%
  left_join(select(RUNS,-Outs),by="STATE") %>%
  rename(Runs.State=Mean) %>%
  left_join(select(RUNS,-Outs),
            by=c("NEW.STATE"="STATE")) %>%
   rename(Runs.New.State=Mean) %>%
   replace_na(list(Runs.New.State=0)) %>%
   mutate(RUNS.VALUE=Runs.New.State-Runs.State +
                    RUNS.SCORED) -> df
return(df)
}


addCounts <- function(df){
df <- df %>% 
  mutate(sequence = gsub("[.>123+*N]","",PITCH_SEQ_TX))

df <- df %>% 
  mutate(c00 = TRUE,
         c10 = grepl("^[BIPV]", sequence),
         c01 = grepl("^[CFKLMOQRST]", sequence))

df <- df %>% 
  mutate(c20 = grepl("^[BIPV]{2}", sequence),
         c30 = grepl("^[BIPV]{3}", sequence),
         c02 = grepl("^[CFKLMOQRST]{2}", sequence))
b <- "[BIPV]"
s <- "[CFKLMOQRST]"
df <- df %>% 
  mutate(c11 = grepl(paste0("^",s,b,
                            "|^",b,s), sequence),
         c21 = grepl(paste0("^",s,b,b,
                            "|^",b,s,b,
                            "|^",b,b,s), sequence),
         c31 = grepl(paste0("^",s,b,b,b,
                            "|^",b,s,b,b,
                            "|^",b,b,s,b,
                            "|^",b,b,b,s), sequence))
df <- df %>% 
  mutate(c12 = grepl(paste0("^", b, s, s,
                            "|^", s, b, s,
                            "|^", s, s, "[FR]*", b), sequence),
         c22 = grepl(paste0("^",b,b,s,s,
                            "|^",b,s,b,s,
                            "|^",b,s,s,"[FR]*",b,
                            "|^", s,b,b,s,
                            "|^",s,b,s,"[FR]*", b,
                            "|^",s,s,"[FR]*",b,"[FR]*",b),
                     sequence),
         c32 = grepl(paste0("^", s, "*", b, s,
                            "*", b, s, "*", b), sequence)
         & grepl(paste0("^",b,"*",s,b,"*",s),
                 sequence))
df
}

addCounts0 <- function(df){
df <- df %>% 
  mutate(sequence = gsub("[.>123+*N]","",PITCH_SEQ_TX))

df <- df %>% 
  mutate(c00 = TRUE,
         c10 = grepl("^[BIPV]", sequence),
         c01 = grepl("^[CFKLMOQRST]", sequence))

df <- df %>% 
  mutate(c20 = grepl("^[BIPV]{2}", sequence),
         c30 = grepl("^[BIPV]{3}", sequence),
         c02 = grepl("^[CFKLMOQRST]{2}", sequence))
b <- "[BIPV]"
s <- "[CFKLMOQRST]"
df <- df %>% 
  mutate(c11 = grepl(paste0("^",s,b,
                            "|",b,s), sequence),
         c21 = grepl(paste0("^",s,b,b,
                            "|",b,s,b,
                            "|",b,b,s), sequence),
         c31 = grepl(paste0("^",s,b,b,b,
                            "|",b,s,b,b,
                            "|",b,b,s,b,
                            "|",b,b,b,s), sequence))
df <- df %>% 
  mutate(c12 = grepl(paste0("^", b, s, s,
                            "|", s, b, s,
                            "|", s, s, "[FR]*", b), sequence),
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
df
}
