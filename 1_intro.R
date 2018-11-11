### -- Introduction and refreshers for R
### -- By - Matt Boone (2015) & Auriel Fournier (2015)
### -- Modified by Auriel Fournier for 2016 NAOC Workshop
### -- https://github.com/aurielfournier/GERS2018  

#######################################
### -- Necessary packages
#######################################

library(dplyr)
library(tidyr)
library(ggplot2)

###################
### -- Loading In The Data
####################

# set working directory!!

# discuss eBird data

ebird <- read.csv("eBird_workshop.csv")

# Explain What Pipes are %>% 

# Explain the verbs of dplyr

#########################
### -- Filtering
#########################

ebird %>%
        filter(state=='AK',
               year==2008)

a = 100
a <- 100

ebird %>%
  filter(state=='AK',
         year==2008) %>% 
        select(state, samplesize, presence)

# the "|" means 'or' in R
ebird %>%
      filter(state=="AK"|state=="AZ") %>%
      # comments here 
      distinct(state)

# the "&" means "and" in R
ebird %>%
          filter(year>=2014&year<=2018) %>% distinct(year)

#########################
### -- Match %in%   
#########################

sub_state <- c("AZ","NM","NV","CO")

ebird %>%
          filter(state %in% sub_state) %>% 
          distinct(state)

gdat <- ebird %>%
        filter(state %in% sub_state)

distinct(gdat, state)


#########################
### -- GROUPING
#########################

ebird %>%
  group_by(state) %>%
  summarize(mean = mean(samplesize),
            median = median(samplesize))


ebird %>% 
          group_by(state, year) %>%
          summarize(mean=mean(presence))

#########################################
### -- CHALLENGE
#########################################

# What is the median samplesize and presence for 
# Arizona, Alaska, Arkansas and Alabama after 2014?

a_states <- c("AZ","AK","AR","AL")

new_data <- ebird %>% 
  filter(state %in% a_states,
         year > 2014) %>% #distinct(continent)
  group_by(state) %>%
  summarise(medianS = median(samplesize),
            medianP = median(presence))


#note to self talk about Kiwi vs Us spelling


#########################
## MUTATE
#########################

colors <- c("red","green")


mebird <- ebird %>%  
  mutate(a_state = ifelse(state %in% a_states, 1, 0),
         state_year = paste0(state,"_",year)) 

mebird %>%
  tail()
  


########################
## Separate
########################

mebird %>% 
  separate(state_year, 
           sep="_", 
           into=c("state",
                  "year"),
           remove=FALSE) %>%
  head()

# or

mebird %>% 
  separate(year, sep=c(2), 
           into=c("century","endpart"),
           remove=FALSE) %>%
  head()

########################
## Joins
########################

cool_birds <- c("Sora","Virginia Rail","Yellow Rail")

ebird1 <- ebird %>% 
            filter(state %in% a_states,
                   species %in% cool_birds) %>%
            select(species, state, year, samplesize) %>%
            filter(year >= 2014)

# point out that you can use multiple filter statements if youwant, or you can put them all in one statement, same result. 

years_to_keep <- c(2008:2012, 2015)

ebird2 <- ebird %>%
            filter(state %in% a_states,
                  species %in% cool_birds) %>%
            select(species, state, year, presence) %>%
            filter(year %in% years_to_keep)

unique(ebird1$year)
unique(ebird2$year)

# 
full_join(ebird1, ebird2, by=c("year","species","state")) %>% distinct(year)

full_join(ebird1, ebird2, by=c("year","species","state")) %>% head()

# 

right_join(ebird1, ebird2, by=c("year","species","state")) %>% distinct(year)

right_join(ebird1, ebird2, by=c("year","species","state")) %>% head()

# 

left_join(ebird1, ebird2, by=c("year","species","state")) %>% distinct(year)

left_join(ebird1, ebird2, by=c("year","species","state")) %>% head()

# 

inner_join(ebird1, ebird2, by=c("year","species","state")) %>% distinct(year)

inner_join(ebird1, ebird2, by=c("year","species","state")) %>% head()



g1 <- ebird %>% select(state, year)

g2 <- ebird %>% select(samplesize, presence)


gg <- cbind(g1, g2)
gg <- bind_cols(g1, g2)

# rbind()
# bind_rows()


#####################################
## CHALLENGE
#####################################

# Calculate the mean presence in 2010
# of 2 randomly selected a_states 
# Hint: Use the dplyr functions sample_n(), 
# they have similar syntax to other dplyr functions.
# ?sample_n for help

ebird %>%
  filter(year==2010,
         state %in% a_states) %>%
  group_by(state) %>%
  sample_n(2) %>%
  summarize(mean=mean(presence))
