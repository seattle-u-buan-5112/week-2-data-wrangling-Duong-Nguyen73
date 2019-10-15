# problem set 2 #

# call-up libraries #
library(tidyverse)
library(haven)
library(tidyr)
library(dbplyr)

# call-up data #
nfhs <- read_dta("./raw_data/IAHR52FL.dta")

# Q1 #
# answer: household #

# Q2 #
# hv208-has television  hv270-wealth #
new_df <- select(nfhs, hhid:hv208, hv270)
# answer: 48 #

# Q3 #
# ind.line.no.-hv003 edu.sin.yr.-hv108,v133 #
hh_edu <- select(nfhs, hhid, hv003, contains("hv108")) # WRONG! # # ?hvidx? #
hh_edu1 <- select(nfhs, hhid, starts_with("hvidx"), contains("hv108"))
# answer: 71 #

# Q4 #
female_bio <- select(nfhs, hhid, matches("ha\\d_\\d+"))
# answer: 78 #

# Q5 #
male_bio <- select(nfhs, hhid, matches("hb\\d_\\d+"))
# answer: 127 #

# Q6 #
hh_gat <- hh_edu %>% gather(key = "all_variables", value = "years", -hhid)
hh_sep <- hh_gat %>% separate('all_variables', into = c('column_name', 'number_of_person'), sep = "_")
hh_spr <- hh_sep %>% spread(key = column_name, value = years)
hh_fdf <- hh_spr %>% filter(!is.na(hv108)) %>% 
  rename('Family_Members' = number_of_person, 'Years_of_ducation' = hv108)

# Q7 #
fem_gat <- female_bio %>% 
  select(c(hhid, matches('ha[1-3]'))) %>%
  gather(matches('ha[1-3]'),
  key = 'all variables', value = 'yrs, lbs, feetinches', na.rm = TRUE)
fem_sep <- fem_gat %>% separate('all variables', 
  into = c('Age Weight Height', 'Family Members', 'Years of Education'))
fem_spr <- fem_sep %>% 
  spread(key = 'Age Weight Height', value = 'yrs, lbs, feetinches')
fem_fdf <- fem_spr %>%
  rename('Age' = ha1_, 'Weight' = ha2_, 'Height' = ha3_)

# 8 #
male_gat <- male_bio %>% 
  select(c(hhid, matches('hb[1-3]'))) %>% 
  gather(matches('hb[1-3]'),
  key = 'all variables', value = 'yrs, lbs, feetinches', na.rm = TRUE)
male_sep <- male_gat %>% separate('all variables',
  into = c('Age Weight Height', 'Family Members', ' Years of Education'))
male_spr <- male_sep %>% 
  spread(key = 'Age Weight Height', value = 'yrs, lbs, feetinches')
male_fdf <- male_spr %>%
  rename('Age' = hb1_, 'Weight' = hb2_, 'Height' = hb3_)

# 9 #


