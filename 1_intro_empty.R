### -- Introduction and refreshers for R
### -- https://github.com/aurielfournier/AOS19AK

#######################################
### -- Necessary packages
#######################################

library(dplyr)
library(tidyr)
library(ggplot2)
# if these do not load, run the matching one of these below
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggplot2")


###################
### -- Loading In The Data
####################

# set working directory!!

# discuss eBird data

ebird <- read.csv("eBird_workshop.csv")

# Explain how to leave comments 

#########################
### -- Filtering
#########################

ebird %>%
        filter(          ,
                          )

# Explain What Pipes are %>% 

# explain assignment operators
a = 100
a <- 100

# filter is for rows
# select is for columns

## With pipes
ebird %>%
  filter(         ,
                  ) %>% 
        select( ,  ,  )

## Without pipes

ebird_filter <- filter( , ,)

select( ,  ,  ,  )


# the "|" means 'or' in R
ebird %>%
      filter() %>%
      # comments here 
      distinct()

# the "&" means "and" in R
ebird %>%
          filter() %>% 
          distinct()

#########################
### -- Match %in%   
#########################

a_states <- c(  ,  ,  ,  )

ebird %>%
          filter( %in% ) %>% 
          distinct()


#########################
### -- GROUPING
#########################

ebird %>%
  group_by() %>%
  summarize(mean = ,
            median = )


ebird %>% 
          group_by(, ) %>%
          summarize(mean=)

#########################################
### -- CHALLENGE
#########################################

# What is the median samplesize for 
# Arizona, Alaska, Arkansas and Alabama after 2014?

new_data <- ebird %>% 
  filter( %in% ,
                    ) %>% 
  group_by() %>%
  summarise(medianS = )


#note to self talk about Kiwi vs Us spelling


#########################
## MUTATE
#########################

colors <- c()

mebird <- ebird %>%  
  mutate(a_state = ,
         state_year = ) 

mebird %>%
  tail()
  
########################
## Separate
########################

mebird %>% 
  separate(, 
           sep=, 
           into=c(,
                     ),
           remove=   ) %>%
  head()

# or

mebird %>% 
  separate(year, sep=c( ), 
           into=c( , ),
           remove=) %>%
  head()

########################
## Joins
########################

cool_birds <- c(   ,   ,   )

ebird1 <- ebird %>% 
            filter( %in% ,
                    %in% ) %>%
            select(, , , ) %>%
            filter()

# point out that you can use multiple filter statements if you want, or you can put them all in one statement, same result. 

years_to_keep <- c(2008:2012, 2015)

ebird2 <- ebird %>%
            filter( %in% ,
                   %in% ) %>%
            select(, , , ) %>%
            filter( %in% )

unique(ebird1$   )
unique(ebird2$   )

# 
full_join(, , by=c(,,)) %>% distinct()

full_join(, , by=c(,,)) %>% head()

# 

right_join(, , by=c(,,)) %>% distinct()

right_join(, , by=c(,,)) %>% head()

# 

left_join(, , by=c(,,))  %>% distinct()

left_join(, , by=c(,,))  %>% head()

# 

inner_join(, , by=c(,,))  %>% distinct()

inner_join(, , by=c(,,))  %>% head()


#####################################
## CHALLENGE
#####################################

# Calculate the mean presence in 2010
# of 2 randomly selected a_states 
# Hint: Use the dplyr functions sample_n(), 
# they have similar syntax to other dplyr functions.
# ?sample_n for help

ebird %>%
  filter(      ,
          %in% ) %>%
  group_by() %>%
  sample_n() %>%
  summarize(mean=  )
