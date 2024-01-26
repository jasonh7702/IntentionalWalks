#This program successfully reproduces Figure 6.2 from p.147 of ABDWR2
#It also produces the same figure after recomputing the count variables
#where there was a slight error (see Appendix A.3.3, p. 301)
#0 load data
#1 add runs value variable (RUNS.VALUE)
#2a add counts using ABDWR2 to produce data2016.0
#2b add counts using amendment to produce data2016
#3 count_plot for both

source("utilities.r")
load("data2016.RData")
#data2016 %>% addRuns %>% filter(BAT_EVENT_FL==TRUE) -> data2016
data2016 %>% addRuns  -> data2016
data2016 %>% addCounts0 -> data2016.0
data2016 %>% addCounts -> data2016

pbp2016.0 <- data2016.0 %>% select(starts_with("c"),RUNS.VALUE)
pbp2016 <- data2016 %>% select(starts_with("c"),RUNS.VALUE)

#reproduce output on p.145
pbp2016.0 %>% filter(c10==1|c01==1) %>% group_by(c10,c01) %>% 
  summarize(N=n(),mean_run_value=mean(RUNS.VALUE))

pbp_counts_tidy.0 <- pbp2016.0 %>% gather(key="count",value="passed_thru",-RUNS.VALUE)
pbp_counts_tidy <- pbp2016 %>% gather(key="count",value="passed_thru",-RUNS.VALUE)

# sample_n(pbp_counts_tidy,10)


run_value_by_count.0 <- pbp_counts_tidy.0 %>% 
  filter(passed_thru==1) %>%
  group_by(count) %>%
  summarize(N=n(),value=mean(RUNS.VALUE))

run_value_by_count <- pbp_counts_tidy %>% 
  filter(passed_thru==1) %>%
  group_by(count) %>%
  summarize(N=n(),value=mean(RUNS.VALUE))

run_value_by_count.0 <- run_value_by_count.0 %>% 
  mutate(balls=str_sub(count,2,2),
         strikes=str_sub(count,3,3))

run_value_by_count <- run_value_by_count %>% 
  mutate(balls=str_sub(count,2,2),
         strikes=str_sub(count,3,3))

run_value_by_count.0 <- run_value_by_count.0 %>% 
  mutate(balls=str_sub(count,2,2),
         strikes=str_sub(count,3,3))


crcblue <- '#0000FF'
crcblue <- "#2905a1"
count_plot.0 <- run_value_by_count.0 %>%
  ggplot(aes(x=strikes,y=balls, fill=value))+
  geom_tile() + 
  geom_text(aes(label=round(value,3)))+
  scale_fill_gradient2(low="grey10",high=crcblue,mid="white")

count_plot <- run_value_by_count %>%
  ggplot(aes(x=strikes,y=balls, fill=value))+
  geom_tile() + 
  geom_text(aes(label=round(value,3)))+
  scale_fill_gradient2(low="grey10",high=crcblue,mid="white")

