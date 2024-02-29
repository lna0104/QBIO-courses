##This is the main script file for the project

library(tidyverse)
library(boilrdata)

marsupialsTibble <- as_tibble(marsupials)
summary(lm(MaxLifeSpan ~ Mass, data = marsupialsTibble)) 
