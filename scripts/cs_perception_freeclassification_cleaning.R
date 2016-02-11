## LOAD PACKAGES ####
library(dplyr)


## READ IN DATA ####
data = read.table("data/data_full.txt", header=T, sep="\t")


## CLEAN DATA ####
data_clean = data %>%
  filter(Condition == "A_E")