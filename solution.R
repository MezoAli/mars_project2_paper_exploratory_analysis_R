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

tb2 <- data %>% 
  select(1:12) %>% 
  tbl_summary(by = professional_category) %>% 
  bold_labels() %>% 
  modify_caption("**Table 1. Participants' Characteristics By Profession (n = 50)**") %>% 
  add_p() %>% 
  bold_p()
tb2

plot_barplot_fn <- function(var){
  require(tidyverse)
  var <- rlang::enquo(var)
  ggplot(data,aes(x = !!var,fill = !!var)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 45))
}


plot_barplot_fn(race)
plot_barplot_fn(professional_category)
plot_barplot_fn(age_in_years)





