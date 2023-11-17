load("/Users/jasonhuang/Downloads/data2022.RData")
library(tidyverse)
library(Lahman)

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
  arrange(Outs) -> RUNS


RUNS_out <- matrix(round(RUNS$Mean, 2),8, 3)
dimnames(RUNS_out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out)[[1]] <- c("000", "001", "010", "011"
                             ,"100", "101", "110", "111")

RUNS.2002 <- matrix(c(.51,1.40,1.14,1.96,.90,1.84,1.51,2.33,.27,.94,.68,1.36,.54,1.18,
                      .94,1.51,.10,.36,.32,.63,.23,.52,.45,.78),8,3)
dimnames(RUNS.2002) <- dimnames(RUNS_out)
cbind(RUNS_out, RUNS.2002)

data2022 <- data2022 %>% 
  mutate(sequence = gsub("[.>123+*N]","",PITCH_SEQ_TX))

data2022 <- data2022 %>% 
  mutate(c00 = TRUE,
         c10 = grepl("^[BIPV]", sequence),
         c01 = grepl("^[CFKLMOQRST]", sequence))

data2022 <- data2022 %>% 
  mutate(c20 = grepl("^[BIPV]{2}", sequence),
         c30 = grepl("^[BIPV]{3}", sequence),
         c02 = grepl("^[CFKLMOQRST]{2}", sequence))
b <- "[BIPV]"
s <- "[CFKLMOQRST]"
data2022 <- data2022 %>% 
  mutate(c11 = grepl(paste0("^",s,b,
                            "|",b,s), sequence),
         c21 = grepl(paste0("^",s,b,b,
                            "|",b,s,b,
                            "|",b,b,s), sequence),
         c31 = grepl(paste0("^",s,b,b,b,
                            "|",b,s,b,b,
                            "|",b,b,s,b,
                            "|",b,b,b,s), sequence))
data2022 <- data2022 %>% 
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
data2022 %>% select(RUNS.ROI, STATE, c10) %>% 
  group_by(STATE, c10) %>%  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> runc10

data2022 %>% filter(c10 == TRUE) %>% 
  group_by(STATE, c20) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c10_c20


data2022 %>% filter(c20 == TRUE) %>% 
  group_by(STATE, c30) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c10_c20_c30

data2022 %>% filter(c30 == TRUE) %>% 
  group_by(STATE, c31) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c10_c20_c30_c31

data2022 %>% filter(c31 == TRUE) %>% 
  group_by(STATE, c31) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c10_c20_c30_c31

data2022 %>% filter(c20 == TRUE) %>% 
  group_by(STATE, c21) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c10_c20_c21

data2022 %>% filter(c21 == TRUE) %>% 
  group_by(STATE, c22) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c10_c20_c21_c22

data2022 %>% filter(c22 == TRUE) %>% 
  group_by(STATE, c32) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c10_c20_c21_c22_c32


data2022 %>% filter(c01 == TRUE) %>% 
  group_by(STATE, c01) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c01

data2022 %>% filter(c01 == TRUE) %>% 
  group_by(STATE, c11) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c01_c11

data2022 %>% filter(c01 == TRUE) %>% 
  group_by(STATE, c02) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c01_c02

data2022 %>% filter(c11 == TRUE) %>% 
  group_by(STATE, c12) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c01_c11_c12

transitions <- list(runc10 = runc10, c10_c20 = c10_c20, c10_c20_c30 = 
                      c10_c20_c30, c10_c20_c30_c31=c10_c20_c30_c31, 
                    c10_c20_c21=c10_c20_c21,c10_c20_c21_c22=c10_c20_c21_c22,
                    c10_c20_c21_c22_c32=c10_c20_c21_c22_c32,c01=c01,
                    c01_c11=c01_c11,c01_c02=c01_c02,c01_c11_c12=c01_c11_c12)

runc10 %>% pivot_wider(values_from = rmean, names_from  = c10, names_prefix = "c10_") %>% 
  mutate(diff = c10_TRUE - c10_FALSE) 

data2022 %>% select(GAME_ID, EVENT_ID, Runs.Inning, c00, c10, c20,
                    c11, c01, c30, c21, c31, c02, c12, c22, c32) -> new.data2022

new.data2022 %>% filter(c10 == 1| c01 == 1) %>%
  group_by(c10, c01) %>% 
  summarize(N = n(), mean_run_value = mean(Runs.Inning))

pbp_counts <- new.data2022 %>% 
  select(starts_with("c"), Runs.Inning)

pbp_counts_tidy <- pbp_counts %>% 
  gather(key = "count", value = "passed_thru", -Runs.Inning)
sample_n(pbp_counts_tidy, 6)

runs_value_by_count <- pbp_counts_tidy %>% 
  filter(passed_thru == 1) %>% 
  group_by(count) %>% 
  summarize(N = n(), value = mean(Runs.Inning))

runs_value_by_count <- runs_value_by_count %>% 
  mutate(balls = str_sub(count, 2, 2),
         strikes = str_sub(count, 3, 3))

library(ggplot2)
crcblue <- "#0000FF"

# Modify the existing scale_fill_gradient2
count_plot <- runs_value_by_count %>% 
  ggplot(aes(x=strikes, y=balls, fill=value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white")

count_plot



count_plot2 <- RUNS %>% 
  ggplot(aes(x=STATE, y=Outs, fill=Mean)) +
  geom_tile() +
  geom_text(aes(label = round(Mean, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

count_plot2
#group_by(count,state), use facet to create a different plot for each state
#create a plot for every count combination 


library(dplyr)

library(dplyr)

# Filter data to separate outs and baserunners to put on countplot
data2022 %>%
  filter(c01 == TRUE) %>%
  mutate(outs = as.numeric(sub("(.+)\\s(\\d)$", "\\2", STATE))) %>%
  mutate(STATE = sub("\\s\\d$", "", STATE)) %>%
  group_by(STATE, outs, c01) %>%
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI > 0)) -> c01filter

count_plot01 <- c01filter %>% 
  ggplot(aes(x=STATE, y=outs, fill=rpos)) +
  geom_tile() +
  geom_text(aes(label = round(rpos, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "01 Pitch Count")
count_plot01


count_plot012 <- c01filter %>% 
  ggplot(aes(x=STATE, y=outs, fill=rmean)) +
  geom_tile() +
  geom_text(aes(label = round(rmean, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "01 Pitch Count")
count_plot012


#filtering c20 to be true, to only show true values
data2022 %>% filter(c10 == TRUE, c20 == TRUE) %>% 
  mutate(outs = as.numeric(sub("^(.+) (\\d)$", "\\2", STATE))) %>%
  mutate(STATE = sub("^(.+) \\d$", "\\1", STATE)) %>% 
  group_by(STATE, outs, c20) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c10_c20filter

count_plot1_2 <- c10_c20filter %>% 
  ggplot(aes(x=STATE, y=outs, fill=rpos)) +
  geom_tile() +
  geom_text(aes(label = round(rpos, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "10+20 Pitch Count")
count_plot1_2


count_plot1_22 <- c10_c20filter %>% 
  ggplot(aes(x=STATE, y=outs, fill=rmean)) +
  geom_tile() +
  geom_text(aes(label = round(rmean, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "10+20 Pitch Count")
count_plot1_22

data2022 %>% select(RUNS.ROI, STATE, c10) %>% 
  filter(c10 == TRUE) %>% 
  mutate(outs = as.numeric(sub("(.+)\\s(\\d)$", "\\2", STATE))) %>%
  mutate(STATE = sub("\\s\\d$", "", STATE)) %>% 
  group_by(STATE,outs, c10) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> runc10filter

count_plot1_ <- runc10filter %>% 
  ggplot(aes(x=STATE, y=outs, fill=rpos)) +
  geom_tile() +
  geom_text(aes(label = round(rpos, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "10 Pitch Count")
count_plot1_

count_plot12_ <- runc10filter %>% 
  ggplot(aes(x=STATE, y=outs, fill=rmean)) +
  geom_tile() +
  geom_text(aes(label = round(rmean, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "10 Pitch Count")
count_plot12_


#creating a 0-0 count 
data2022 %>% filter(c01 == FALSE | c02 == FALSE | c10 == FALSE |
                      c20 == FALSE | c30 == FALSE | c11 == FALSE
                    | c21 == FALSE | c31 == FALSE | c32 == FALSE | c22 == FALSE
                    | c12 == FALSE) %>%
  mutate(outs = as.numeric(sub("(.+)\\s(\\d)$", "\\2", STATE))) %>%
  mutate(STATE = sub("\\s\\d$", "", STATE)) %>% 
  group_by(STATE, outs, c00) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c00

data2022 %>% filter(c01 == FALSE | c02 == FALSE | c10 == FALSE |
                      c20 == FALSE | c30 == FALSE | c11 == FALSE
                    | c21 == FALSE | c31 == FALSE | c32 == FALSE | c22 == FALSE
                    | c12 == FALSE) %>% 
  mutate(outs = as.numeric(sub("(.+)\\s(\\d)$", "\\2", STATE))) %>%
  mutate(STATE = sub("\\s\\d$", "", STATE)) %>% 
  group_by(STATE, outs, c00) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c00filtered


#plot for 00 plots
count_plot00 <- c00 %>% 
  ggplot(aes(x=STATE, y=outs, fill=rpos)) +
  geom_tile() +
  geom_text(aes(label = round(rpos, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "00 Pitch Count")
count_plot00

count_plot002 <- c00 %>% 
  ggplot(aes(x=STATE, y=outs, fill=rmean)) +
  geom_tile() +
  geom_text(aes(label = round(rmean, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "00 Pitch Count")
count_plot002


data2022 %>% filter(c20 == TRUE, c30 == TRUE) %>% 
  mutate(outs = as.numeric(sub("(.+)\\s(\\d)$", "\\2", STATE))) %>%
  mutate(STATE = sub("\\s\\d$", "", STATE)) %>% 
  group_by(STATE, outs, c30) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c10_c20_c30filter

data2022 %>% filter(c31 == TRUE) %>% 
  mutate(outs = as.numeric(sub("(.+)\\s(\\d)$", "\\2", STATE))) %>%
  mutate(STATE = sub("\\s\\d$", "", STATE)) %>% 
  group_by(STATE, outs, c31) %>%  
  summarize(rmean = mean(RUNS.ROI), rpos = mean(RUNS.ROI>0)) -> c10_c20_c30_c31filter

count_plot30 <- c10_c20_c30filter %>% 
  ggplot(aes(x=STATE, y=outs, fill=rpos)) +
  geom_tile() +
  geom_text(aes(label = round(rpos, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "30 Pitch Count")
count_plot30

count_plot302 <- c10_c20_c30filter %>% 
  ggplot(aes(x=STATE, y=outs, fill=rmean)) +
  geom_tile() +
  geom_text(aes(label = round(rmean, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "30 Pitch Count")
count_plot302 



#Load the patchwork package
library(patchwork)

#Combine the plots into a 2x2 grid
combined_plots <- count_plot00 + count_plot002 + count_plot30 + count_plot302

#Arrange the plots as a 2x2 grid
combined_plots <- combined_plots + plot_layout(ncol = 2)

combined_plots

print(c01, n=100)

#combining the 0-0 and 3-1 data sets
combined_data31 <- bind_rows(c10_c20_c30_c31filter, c00filtered)

#calculating the differences between data sets
combined_data31 <- combined_data31 %>%
  mutate(rmean_diff = lag(rmean, default = first(rmean)) - rmean,
         rpos_diff = lag(rpos, default = first(rpos)) - rpos) %>% 
  select(STATE, outs, rmean_diff, rpos_diff) %>%
  slice(25:n())

combined_data30 <- bind_rows(c10_c20_c30filter, c00filtered)
combined_data30 <- combined_data30 %>%
  mutate(rmean_diff = lag(rmean, default = first(rmean)) - rmean,
         rpos_diff = lag(rpos, default = first(rpos)) - rpos) %>% 
  select(STATE, outs, rmean_diff, rpos_diff) %>%
  slice(25:n())

combined_data20 <- bind_rows(c10_c20filter, c00filtered)
combined_data20 <- combined_data20 %>%
  mutate(rmean_diff = lag(rmean, default = first(rmean)) - rmean,
         rpos_diff = lag(rpos, default = first(rpos)) - rpos) %>% 
  select(STATE, outs, rmean_diff, rpos_diff) %>%
  slice(25:n())

combined_data10 <- bind_rows(runc10filter, c00filtered)
combined_data10 <- combined_data10 %>%
  mutate(rmean_diff = lag(rmean, default = first(rmean)) - rmean,
         rpos_diff = lag(rpos, default = first(rpos)) - rpos) %>% 
  select(STATE, outs, rmean_diff, rpos_diff) %>%
  slice(25:n())

combined_data31rmean <- combined_data31 %>% 
  ggplot(aes(x=STATE, y=outs, fill=rmean_diff)) +
  geom_tile() +
  geom_text(aes(label = round(rmean_diff, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "31 Pitch Count")
combined_data31rmean 

combined_data31rpos <- combined_data31 %>% 
  ggplot(aes(x=STATE, y=outs, fill=rpos_diff)) +
  geom_tile() +
  geom_text(aes(label = round(rpos_diff, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "31 Pitch Count")
combined_data31rpos 

combined_data30rmean <- combined_data30 %>% 
  ggplot(aes(x=STATE, y=outs, fill=rmean_diff)) +
  geom_tile() +
  geom_text(aes(label = round(rmean_diff, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "30 Pitch Count")
combined_data30rmean 

combined_data30rpos <- combined_data30 %>% 
  ggplot(aes(x=STATE, y=outs, fill=rpos_diff)) +
  geom_tile() +
  geom_text(aes(label = round(rpos_diff, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "30 Pitch Count")
combined_data30rpos 

combined_data20rmean <- combined_data20 %>% 
  ggplot(aes(x=STATE, y=outs, fill=rmean_diff)) +
  geom_tile() +
  geom_text(aes(label = round(rmean_diff, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "20 Pitch Count")
combined_data20rmean 

combined_data20rpos <- combined_data20 %>% 
  ggplot(aes(x=STATE, y=outs, fill=rpos_diff)) +
  geom_tile() +
  geom_text(aes(label = round(rpos_diff, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "20 Pitch Count")
combined_data20rpos 

combined_data10rmean <- combined_data10 %>% 
  ggplot(aes(x=STATE, y=outs, fill=rmean_diff)) +
  geom_tile() +
  geom_text(aes(label = round(rmean_diff, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "10 Pitch Count")
combined_data10rmean 

combined_data10rpos <- combined_data10 %>% 
  ggplot(aes(x=STATE, y=outs, fill=rpos_diff)) +
  geom_tile() +
  geom_text(aes(label = round(rpos_diff, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "10 Pitch Count")
combined_data10rpos 

#combining all counts and 0-0 counts into a single grid
library(gridExtra)
grid.arrange(combined_data31rmean, combined_data31rpos,
             combined_data30rmean, combined_data30rpos,
             combined_data20rmean, combined_data20rpos,
             combined_data10rmean, combined_data10rpos, nrow = 4)

count_plot31 <- c10_c20_c30_c31filter %>% 
  ggplot(aes(x=STATE, y=outs, fill=rmean)) +
  geom_tile() +
  geom_text(aes(label = round(rmean, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "31 Pitch Count")
count_plot31

count_plot312 <-  c10_c20_c30_c31filter %>% 
  ggplot(aes(x=STATE, y=outs, fill=rpos)) +
  geom_tile() +
  geom_text(aes(label = round(rpos, 3))) +
  scale_fill_gradient2(low = "grey10", high = crcblue, 
                       mid = "white") +
  labs(title = "31 Pitch Count")
count_plot312
grid.arrange(count_plot00 ,count_plot002, count_plot30 ,count_plot302, 
             count_plot1_, count_plot12_, count_plot1_2, count_plot1_22,
             count_plot312, count_plot31, nrow = 5)

data2022 %>% filter(STATE == "111 0") %>% filter(max(c30, c31) == 1) -> df


filtered_data <- subset(data2022, EVENT_CD == 15 & c31 == TRUE & STATE == "011 1")
filtered_data2 <- subset(data2022, c31 == TRUE & STATE == "011 1")
filtered_data3 <- subset(data2022, EVENT_CD == 15 & c30 == TRUE & STATE == "001 2")
filtered_data4 <- subset(data2022, c30 == TRUE & STATE == "001 2")

num_obs_filtered_data <- nrow(filtered_data)

num_obs_filtered_data2 <- nrow(filtered_data2)

ratio <- num_obs_filtered_data / num_obs_filtered_data2
ratio

num_obs_filtered_data3 <- nrow(filtered_data3)

num_obs_filtered_data4 <- nrow(filtered_data4)

ratio2 <- num_obs_filtered_data3 / num_obs_filtered_data4
ratio2

#3-0  001 2
#3-1 011 1

