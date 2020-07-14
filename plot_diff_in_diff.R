# -----------------------------------------------------------------------------
# Title: Impact of Reform Prosecution in Dallas - Stage 2 (Plotting & Analysis)
# Author: Jacqueline Woo, jacqwoo@gmail.com
# Date: 4/27/2020
# R Version: 3.6.3
# RStudio Version: 1.2.5033
# Note: This analysis aims to examine the impact of elected a reform-minded DA, 
# ## Craig Watkins, in Dallas County, TX, on serious crim. Watkins is one of the 
# ## first "reform-minded" DAs elected in the US, which translates a larger 
# ## dataset to work with. In the case of my analysis, with was matchin and 
# ## differences-in-differences, this enabled a longer post period. There is a 
# ## big caveat to this analysis. The influence of DAs lies mostly in the 
# ## judicial branch and I was not able to identify specific policies or 
# ## procedures that would plausibly reduce serious crime. This hinders the 
# ## internal validity of this analysis. I still think this is an interesting 
# ## question that will hopefully generate more answers with more time and data.
# -----------------------------------------------------------------------------

rm(list = ls())       # clear objects in memory
library(dplyr)
library(tidyr)
library(prettyR)
library(dygraphs)
library(xts)
library(ggplot2)
library(plotly)
library(reshape2)

setwd("C:/Users/jacqw/Documents/Columbia/Spring 2020/U8353 Race Policy/
      Pretrial Detention/UCR")

#reading data and cleaning
data <- read.csv("TX_UCR.csv")
data_melt <- data %>% rename(County="County.") %>% 
  melt(.,id.vars=c("NN_M","NN_PSM","City","County","Agency","State",
                   "Variable")) %>%
  mutate(date1 = paste0(gsub("\\D","",variable),"-01-01")) %>% #\\D means non-digits in regex
  mutate(date = as.Date(date1,"%Y-%m-%d")) %>%
  select(-variable, -date1, -State) %>%
  dcast(.,NN_M + NN_PSM + City + County + Agency + date ~ Variable, 
        value.var="value")


#1. Using controls developed from Nearest Neigbor (PSM)
#just deal with NN_PSM and Dallas data
##NN_PSM
NN_PSM <- data_melt %>% filter(NN_PSM == 1 | (City == "Dallas" & County == 0)) %>%
  select(-`Property Crime Rate`,-`Property Crime Total`,`Violent Crime Rate`) %>%
  mutate(treated = as.factor(ifelse(City == "Dallas",1,0)))
NN_PSM_sum <- NN_PSM %>% group_by(treated,date) %>%
  summarise(violent_crime = sum(`Violent Crime Total`, na.rm = T),
            pop=sum(Population))%>%
  mutate(violent_crime_rate = violent_crime/(pop/100000))%>%
  select(treated, date,violent_crime_rate)

#plotting data
data_plot <- NN_PSM_sum
p <- ggplot(data = data_plot, aes(x = date, y = violent_crime_rate, 
                                  group = treated)) +
  geom_line(aes(color = treated)) + theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  ggtitle("Violent Crime, 1985 - 2014") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") + ylab("Violent Crimes per 100K")

p + geom_vline(xintercept = as.Date("2007-01-01")) +
  annotate("text", x = as.Date("2007-01-01"), y = 2500, 
           label = "Dallas DA: Craig Watkins", hjust = 0, size = 3) +
  scale_color_discrete(name = "City", labels = c("Control Group", "Dallas"))

#plotting data starting in 1992
data_plot <- NN_PSM_sum %>% filter(date >= "1992-01-01")
p <- ggplot(data = data_plot, aes(x = date, y = violent_crime_rate, 
                                  group = treated)) +
  geom_line(aes(color = treated), size = 1) + theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  ggtitle("Violent Crime per 100K, 1992 - 2014") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") + ylab("Violent Crime Rate")

p + geom_vline(xintercept = as.Date("2007-01-01")) +
  annotate("text", x = as.Date("2007-01-01"), y = 2500, 
           label = "Dallas DA: Craig Watkins", hjust = 0, size = 3) +
  scale_color_discrete(name = "City", labels = c("Control Group", "Dallas"))

  
#2. Using controls developed from Nearest Neigbor (Mahalanobis)
##NN_M
NN_M <- data_melt %>% filter(NN_M == 1 | (City=="Dallas" & County == 0)) %>%
  select(-`Property Crime Rate`,-`Property Crime Total`,`Violent Crime Rate`) %>%
  mutate(treated = as.factor(ifelse(City == "Dallas",1,0)))
NN_M_sum <- NN_PSM %>% group_by(treated,date) %>%
  summarise(violent_crime = sum(`Violent Crime Total`, na.rm = T),
            pop = sum(Population))%>%
  mutate(violent_crime_rate = violent_crime/(pop/100000))%>%
  select(treated, date,violent_crime_rate)

#plotting data
data_plot <- NN_M_sum
p <- ggplot(data = data_plot, aes(x = date, y = violent_crime_rate, 
                                  group = treated)) +
  geom_line(aes(color = treated)) + theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  ggtitle("Violent Crime, 1985 - 2014") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") + ylab("Violent Crimes per 100K")

p + geom_vline(xintercept = as.Date("2007-01-01")) +
  annotate("text", x = as.Date("2007-01-01"), y = 2500, 
           label = "Dallas DA: Craig Watkins", hjust = 0, size = 3)+
  scale_color_discrete(name = "City", labels = c("Control Group", "Dallas"))

#3. All TX cities' violent crime rates + top 5 largest cities
##identifying top 5 cities by population
population <- data %>% filter(Variable == "Population")
pop_rank <- population %>% 
  select(-NN_M, -State, -Variable, -County., -City,-NN_PSM,-Agency) %>%
  apply(.,1,function(x) mean(x, na.rm = T)) %>%
  as.data.frame(.) %>% 
  cbind(.,population$City) %>%
  rename(City="population$City", pop = ".") %>% mutate(rank = rank(-pop))
data_rank <- data_melt %>% merge(.,pop_rank,by = "City") %>%
  select(-pop)

#total TX violent crime rate
TX <- data_melt %>% select(-`Property Crime Rate`,-`Property Crime Total`,
                           `Violent Crime Rate`) %>%
  group_by(date) %>%
  summarise(violent_crime = sum(`Violent Crime Total`, na.rm = T),
            pop = sum(Population))%>%
  mutate(violent_crime_rate = violent_crime/(pop/100000))%>%
  mutate(City="all TX", line = 1, color=2)%>%
  select(City, date, violent_crime_rate, line)

#top 5 TX cities by avg population over this period
top_3 <- data_rank %>%
  filter(rank <= 3) %>%
  select(-`Property Crime Rate`,-`Property Crime Total`,
         `Violent Crime Rate`,-NN_M, -NN_PSM) %>%
  group_by(City,date) %>%
  summarise(violent_crime = sum(`Violent Crime Total`, na.rm = T),
            pop = sum(Population))%>%
  mutate(violent_crime_rate = violent_crime/(pop/100000), line = 0)%>%
  select(City, date, violent_crime_rate, line) %>%
  as.data.frame(.) #because it gets grouped into a tibble

top_cities <- data_rank %>% filter(rank <= 5) %>% 
  select(City, rank) %>% unique(.) #1 - Houston, 2 - Dallas, 3 - San Antonio, 
  # 4 - Austin, 5 - El Paso

#merge top 5 and all_TX
top_3_comb <- rbind(TX,top_3) %>% 
  mutate(line1 = as.factor(line), 
         City = factor
         (City,levels = c("all TX", "Houston","Dallas","San Antonio"))) 
  ## reorder to make the graph colours make consistent

#plotting data
data_plot <- top_3_comb
p <- ggplot(data = data_plot, aes(x = date, y = violent_crime_rate)) +
  geom_line(aes(color = City, linetype = line1), size = 1) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_linetype_manual(values = c("dashed","solid")) +
  ggtitle("Violent Crime per 100K, 1985 - 2014") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") + ylab("Violent Crimes Rate")

p + geom_vline(xintercept = as.Date("2007-01-01")) +
  annotate("text", x=as.Date("2007-01-01"), y = 2500, 
           label = "Dallas DA: Craig Watkins", hjust = 0, size = 3) +
  guides(linetype = F)

#save global environment
save(list=ls(all = TRUE),file = "TX_2.RData")
