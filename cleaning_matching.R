# -----------------------------------------------------------------------------
# Title: Impact of Reform Prosecution in Dallas
# Author: Jacqueline Woo, jacqwoo@gmail.com
# Date: 4/22/2020
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

library(ipumsr)
library(dplyr)
library(survey)
library(MatchIt) #for PSM and other matching methods
library(tableone) #helps format a nice-looking table
library(optmatch) #for optimal matching within MatchIt 
  #(smallest average absolute distance)
library(rgenoud) #for genetic matching within MatchIt

rm(list = ls())  

## 1. Administrative Stuff
#set wd
setwd("C:/Users/jacqw/Documents/Columbia/Spring 2020/U8353 Race Policy/
      Pretrial Detention/UCR/Census Data/data_TX")

#load Census Data (IPUMS)
load("usa_00004.RData")

#obtaining IPUMS data
ddi <- read_ipums_ddi("usa_00004.xml")
data <- read_ipums_micro(ddi)
test <- data[sample(nrow(data),20), ]
top <- top_n(data,20)

## 2. Exploring Data
table(data$YEAR, useNA = "always") #ok, I understand these codes
table(data$PUMA, useNA = "always") #POVERTY is a 3-digit numeric code 
  ## expressing each family's total income for the previous year as a percentage 
  ## of the poverty thresholds established by the Social Security Administration 
  ## in 1964 and subsequently revised in 1980, adjusted for inflation
  ## 000-NA, 001-<1% of poverty threshold, 501->501% of poverty threshold

#how many rows are in each PUMA?
PUMA_count <- data %>% group_by(PUMA) %>% tally()

# what PUMAs are in CPUMA=973?
PUMA973 <- data %>% filter(CPUMA0010 == 973) %>% group_by(PUMA) %>% tally()
  # CPUMA0010s are consistent between 2000 and 2010. These are larger areas than 
  # PUMAs. The dallas city/county overlapping PUMAs that I'm interested in 
  # belongs to just one CPUMA0010 - 973

  ## given that Craig Watkins was elected DA for Dallas County, 
  ## we will designate all PUMAs within CPUMA--973 to be treated 
  ## (this will be done later after means calc)

#how many cities are available in the data?
city <- data %>% select(CITY, SAMPLE) %>% group_by(CITY) %>% tally()

## 3. Cleaning/Merging Data
# read crosswalk data and Dallas City/County data
crosswalk <- read.csv("PUMA_crosswalk.csv")
crosswalk_data <- crosswalk %>% select(PUMA00, PUMA10, PUMA10_Name, 
                                      PUMA00_Pop00, PUMA10_Pop00, 
                                      pPUMA00_Pop00)
dallas_city_county <- read.csv("Dallas_City_County.csv")
dallas_city_county <- dallas_city_county %>% mutate(treated = 
                                                      ifelse(is.na(Dallas) == T,
                                                             0, Dallas))
dallas_data <- dallas_city_county %>% filter(treated == 1) %>% 
  select(PUMA010,treated)

# translate 2010 Dallas City/County PUMAs to 2000 PUMAs
crosswalk_data <- crosswalk_data %>% merge(.,dallas_data, by.x = "PUMA10", 
                                           by.y = "PUMA010", 
                                           all.x = T) %>% 
  unique(.) %>% mutate(treated=ifelse(is.na(treated) == T,0,treated))
table(crosswalk_data$treated, useNA = "always")
crosswalk_PUMA <- crosswalk_data %>% group_by(PUMA00,treated) %>% 
  summarise(pPUMA00_Pop00=sum(pPUMA00_Pop00)) %>%
  arrange(treated,pPUMA00_Pop00) 

## designate treated 2000 PUMAs. This will be used later during matching.
treated_PUMA <- crosswalk_PUMA %>% filter(treated == 1 & pPUMA00_Pop00 > 50) %>% 
  unique(.)

# cleaning data to create the format I'd like when I calculate means
means <- data %>% mutate(farm = ifelse(FARM == 0,NA,ifelse(FARM == 1,0,1)), #1-farm, 0-nonfarm
                         ownership = ifelse(OWNERSHP == 0,NA,ifelse(OWNERSHP == 2,0,1)), #1-Owned, 0-rented
                         acreage=ifelse(ACREHOUS == 0,NA,ifelse(ACREHOUS == 1,0,1)), #1-House on >10 acres
                         vacant=ifelse(VACANCY == 0 | VACANCY == 3,0,1), #1-vacant, 0-not vacant                         
                         sex=ifelse(SEX == 2,0,1), #1-male, 0-female
                         race=ifelse(RACE == 1,1,0), #1-white, 0-nonwhite
                         hisp=ifelse(HISPAN == 0,0,ifelse(HISPAN == 9,NA,1)), #1-hispanic, 0-not hispanic
                         hs=ifelse(AGE < 25, NA, ifelse(EDUCD >= 62,1,0)), #1-graduated HS/GED or higher, proportion of adults 25+
                         ba=ifelse(AGE < 25, NA, ifelse(EDUCD >= 101,1,0)), #1-graduated HS/GED or higher, proportion of adults 25+                         
                         employed=ifelse(EMPSTAT == 0|EMPSTAT == 3,NA,ifelse(EMPSTAT == 2,0,1)), #1-employed, 0-unemployed, NA - NA or not in labour force
                         laborforce=ifelse(LABFORCE == 1,0,ifelse(LABFORCE == 2,1,NA)) #1 - in labor force, 0-not in labor force
                         )
test_means <- means[sample(nrow(means),20), ]

# calculating mean household-level data by PUMA. I'm going to match on means.
design_hh <- svydesign(id =~ 0, 
                       probs = NULL, 
                       strata = ~STRATA, 
                       weights = ~HHWT,
                       data = means)
options(survey.lonely.psu = "certainty")
hh_means <- svyby(~HHINCOME + DENSITY + PROPTX99 + RENT + farm + ownership + 
                    acreage + vacant, ~PUMA, design_hh, svymean, na.rm=T,
                  deff=F, influence=F) 

svymean(~HHINCOME + DENSITY + PROPTX99 + RENT + farm + ownership + acreage + 
          vacant, design_hh, na.rm = T,deff = F, influence = F)

# calculating mean individual-level data by CPUMA
design_indv <- svydesign(id = ~0, 
                       probs = NULL, 
                       strata = ~STRATA, 
                       weights = ~PERWT,
                       data = means)
indv_means <- svyby(~AGE + sex + race + hisp + hs + ba + employed +laborforce, 
                    ~PUMA, design_indv, svymean, na.rm = T,deff = F, 
                    influence = F)
svymean(~AGE + sex + race + hisp + hs + ba + employed + laborforce, design_indv, 
        na.rm = T,deff = F, influence=F)

# joining individual-level and hh-level by PUMA
means_PUMA <- full_join(hh_means,indv_means, by = "PUMA") 
  # CPUMA 973 (Dallas) tends to have slightly higher than average means for 
  # almost everything (I had previously calculated by CPUMA)
  # (excl hisp and employment). Lower mean for race (more diverse)
  # Calculating by PUMA to provide more variation for PSM

# removing variables vacant and laborforce because no variation
means_PUMA$vacant <- NULL
means_PUMA$se.vacant <- NULL
means_PUMA$laborforce <- NULL
means_PUMA$se.laborforce <- NULL

# designating treated PUMAs
means_PUMA <- means_PUMA %>% 
  mutate(treated = ifelse(PUMA %in% treated_PUMA$PUMA00 ==T, 1, 0),
         PUMA00 = PUMA) %>% select(-PUMA)
table(means_PUMA$treated, useNA = "always") ## i did it right!

## 4. Matching treated PUMAs to control PUMAs using a variety of matching
## algorithms
# Using these references: 
  ##https://www.kdnuggets.com/2018/01/propensity-score-matching-r.html
  ##https://cran.r-project.org/web/packages/MatchIt/vignettes/matchit.pdf
# Subset the Treatment and Control Population, then obtain summary statistics
Treats <- subset(means_PUMA, treated==1)
Control <- subset(means_PUMA, treated==0)
colMeans(Treats)
colMeans(Control)
  ## treated PUMAs have a higher average hhincome, density, pay higher property 
  ## taxes, higher rent, lower acreage, less white, slightly higher education 
  ## (but not a whole lot), similar employment status and ownership likelihood 
  ## compared to control PUMAs

# Using Exact Matching
## Matches each treated unit to all possible control units with exactly
## the same values. I think it's unlikely this will be a good outcome.
exact_match_out <- matchit(treated ~ HHINCOME + DENSITY + PROPTX99 + RENT + 
                             farm + ownership + acreage + AGE + sex + race + 
                             hisp + hs + ba + employed, data = means_PUMA, 
                           method = "exact")
  #no units matched

# Using Subclassification
## Forms subclasses such that in each subclass the distribution of covariates for 
## the treated and control groups are as similar as possible. Estimate ATE
## separately from each subclass (not really what I'm looking for)
subcl_match_out <- matchit(treated ~ HHINCOME + DENSITY + PROPTX99 + RENT + 
                             farm + ownership + acreage + AGE + sex + race + 
                             hisp + hs + ba + employed,
                           data = means_PUMA, method = "subclass")

## Checking balance
summary(subcl_match_out)
warnings() #not enough control and treatment units in subclasses 3, 4, 5 and 6
plot(subcl_match_out, type = "jitter")

# Nearest Neighbor Matching
## Selects the best control match for each individual in the treatment group, 
## done using a distance measure (default=logit). You can specify the order
## with which a match is chosen for each treated unit
nn_match_out <- matchit(treated ~ HHINCOME + DENSITY + PROPTX99 + RENT + farm + 
                          ownership + acreage + AGE + sex + race + hisp + hs + 
                          ba + employed, data = means_PUMA, method = "nearest")
summary(nn_match_out) # matched 11 control and 11 treated
plot(nn_match_out, type = "jitter")
plot(nn_match_out, type = "hist") #Balance has improved but doesn't completely match

## Obtaining the datapoints
nn_data <- match.data(nn_match_out)

## merging nn_data with crosswalk to obtain PUMA names (in 2010) and CPUMS0100
nn_data_merged <- merge(nn_data,crosswalk_data, by = c("PUMA00", "treated"), 
                        all.x = T)
  ## there are multiple rows because PUMA00 to PUMA10 is a multiple-to-multiple 
  ## match. but i'm ok with that as long as the treated flags don't overlap 
  ## because I want to make sure I have all the names of all possible locales

## NN using Mahalanobis distance instead of PS
nn_match_out_m <- matchit(treated ~ HHINCOME + DENSITY + PROPTX99 + RENT + 
                            farm + ownership + acreage + AGE + sex + race + 
                            hisp + hs + ba + employed, data = means_PUMA, 
                          method = "nearest", distance="mahalanobis")
summary(nn_match_out_m) #Should compare this with nn PSM
nn_data_m <- match.data(nn_match_out_m)
nn_data_m_merged <- merge(nn_data_m,crosswalk_data, by = c("PUMA00", "treated"), 
                          all.x = T)

# Optimal Matching
## Finds the matched samples with the smallest average absolute distance across all
## the matched pairs
## I don't know how big of a difference this will be compared to NN
opt_match_out <- matchit(treated ~ HHINCOME + DENSITY + PROPTX99 + RENT + farm + 
                           ownership + acreage + AGE + sex + race + hisp + hs + 
                           ba + employed, data = means_PUMA, method = "optimal", 
                         ratio = 2)
summary(opt_match_out) #matched 2 control to each treated
plot(opt_match_out, type = "jitter")
plot(opt_match_out, type = "hist") # a lot more control obs with lower propensity scores

## Try it with just ratio = 1?
opt_match_out_1 <- matchit(treated ~ HHINCOME + DENSITY + PROPTX99 + RENT + farm
                           + ownership + acreage + AGE + sex + race + hisp + 
                             hs + ba + employed, data = means_PUMA, 
                           method = "optimal", ratio = 1)
summary(opt_match_out_1) #matched 2 control to each treated
plot(opt_match_out_1, type = "jitter")
plot(opt_match_out_1, type = "hist") #very similar dist to NN. I wouldn't be 
  # surprised if they were the exact same thing

# Full Matching
## Forms subclasses in an optimal way--matched sets contain one treated unit 
## and > 1 controls
full_match_out <- matchit(treated ~ HHINCOME + DENSITY + PROPTX99 + RENT + 
                            farm + ownership + acreage + AGE + sex + race + hisp
                           + hs + ba + employed, data = means_PUMA, 
                          method = "full")
plot(full_match_out, type = "jitter")
plot(full_match_out, type = "hist")
summary(full_match_out) #the balance actually looks pretty good, even though the
  # hist comparison doesn't (because weights are involved). All control units 
  # were used.
full_data <- match.data(full_match_out) #saving selected data in a dataframe

## Using Mahalanobis distance instead of PS
full_match_out_m <- matchit(treated ~ HHINCOME + DENSITY + PROPTX99 + RENT + 
                              farm + ownership + acreage + AGE + sex + race + 
                              hisp + hs + ba + employed, data = means_PUMA, 
                            method = "full", distance = "mahalanobis")
plot(full_match_out_m)
summary(full_match_out_m) 
full_data_m <- match.data(full_match_out_m)

## Providing a "discard" option
full_match_out_d <- matchit(treated ~ HHINCOME + DENSITY + PROPTX99 + RENT + 
                              farm + ownership + acreage + AGE + sex + race + 
                              hisp + hs + ba + employed, data = means_PUMA, 
                            method = "full", discard = "control")
plot(full_match_out_d)
plot(full_match_out_d, type = "jitter")
plot(full_match_out_d, type = "hist")
summary(full_match_out_d) #39 control units matching 11 treated
full_data_d <- match.data(full_match_out_d)
full_data_d_merged <- merge(full_data_d,crosswalk_data, 
                            by = c("PUMA00", "treated"), all.x = T)

## "Discard" + "Mahalanobis"
full_match_out_dm <- matchit(treated ~ HHINCOME + DENSITY + PROPTX99 + RENT + 
                               farm + ownership + acreage + AGE + sex + race + 
                               hisp + hs + ba + employed, data = means_PUMA, 
                             method = "full", discard="control",
                             distance="mahalanobis")
plot(full_match_out_dm)
summary(full_match_out_dm) #All control units matched, none discarded 
  # (interesting, why such a big difference with regular PSM?)
full_data_dm <- match.data(full_match_out_dm)

# Genetic Matching
## Automated method of finding a good matching solution. Uses a genetic search
## Algorithm to find a set of weights for each covariate such that a version
## of optimal balance is achieved after maching. Matching is done with repl.
## Do I want to weight my covariates? I guess it depends on what the weights are...
genetic_match_out <- matchit(treated ~ HHINCOME + DENSITY + PROPTX99 + RENT + 
                               farm + ownership + acreage + AGE + sex + race + 
                               hisp + hs + ba + employed, data = means_PUMA, 
                             method = "genetic")
plot(genetic_match_out, type="jitter")
plot(genetic_match_out, type="hist")

summary(genetic_match_out) #10 control units selected, seems like all units 
  # weighted equally. I don't want to weight the covariates differently.


#write matched data to csv
write.csv(full_data,file="full_data.csv")
write.csv(full_data_m,file="full_data_m.csv")
write.csv(nn_data,file="nn_data.csv")
write.csv(nn_data_m,file="nn_data_m.csv")
write.csv(nn_data_merged,file="nn_data_merged.csv")
write.csv(nn_data_merged,file="nn_data_m_merged.csv")
write.csv(full_data_d, file="full_data_d.csv")
write.csv(full_data_d_merged, file="full_data_d_merged.csv")
write.csv(city, file="city.csv")

#save global environment
save(list=ls(all = TRUE),file="usa_00004.RData")
