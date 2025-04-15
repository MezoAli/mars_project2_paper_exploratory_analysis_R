rm(list = ls())
graphics.off()

library(tidyverse)
library(psych)
library(janitor)
library(gtsummary)
library(cardx)

data<- read_csv("./data.csv") %>% 
  clean_names(.)

data <- data %>% 
  mutate(service_in_years = as.numeric(service_in_years)) %>% 
  replace_na(list(service_in_years = 0))


data %>% 
  gtsummary::tbl_summary()


tb1 <- data %>% 
  select(1:12) %>% 
  tbl_summary() %>% 
  bold_labels() %>% 
  modify_caption("**Demographic Data**")
tb1


