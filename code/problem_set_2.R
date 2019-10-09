# problem set 2 #
library(tidyverse)
library(haven)

nfhs <- read_dta("./raw_data/IAHR52FL.dta")
hh <- select(nfhs, hhid:hv208, hv270)
