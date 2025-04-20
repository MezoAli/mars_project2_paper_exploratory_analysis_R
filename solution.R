rm(list = ls())
graphics.off()

library(tidyverse)
library(psych)
library(janitor)
library(gtsummary)
library(cardx)
library(corrplot)

data<- read_csv("./data.csv") %>% 
  clean_names(.)

data <- data %>% 
  mutate(service_in_years = as.numeric(service_in_years)) %>% 
  replace_na(list(service_in_years = 0)) %>% 
  mutate(across(.cols = 13:44,.fn = as.factor))

summary(data)

data_numeric <- data %>%
  select(13:44) %>% 
  mutate(across(everything(), ~ as.integer(as.factor(.))))

names(data_numeric) <- paste0("Q",1:32)

cor.matrix = cor(data_numeric,use = "pairwise.complete.obs")

cor.matrix.plot <- corrplot(cor.matrix,method = "square", type = "lower", tl.cex = 0.6)

which(abs(cor.matrix) > 0.7 & abs(cor.matrix) < 1, arr.ind = TRUE)



KMO(data_numeric)

cortest.bartlett(data_numeric)




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
    theme(axis.text.x = element_text(angle = 90,margin = margin(t=5),size = 8),
          plot.title = element_text(face = "bold",hjust = .5)) +
    ggtitle(paste("Participants",rlang::as_label(var),sep = " "))
}


plot_barplot_fn(race)
plot_barplot_fn(professional_category)
plot_barplot_fn(age_in_years)
plot_barplot_fn(working_unit_category)



fa.results <- fa(data_numeric,nfactors = 2)
fa.diagram(fa.results)

# according to fa.results and fa.diagram, some questions need to be dropped due to
# very low loadings ( less than 0.3) and/or very high uniqueness (near to 1)
# and these questions are 8,13,14,16,19,22,25,27,28,31

data_numeric_modified <- data_numeric %>% 
  select(-c(8,13,14,16,19,22,25,27,28,31))

fa.results_modified <- fa(data_numeric_modified,nfactors = 2)
fa.diagram(fa.results_modified)

respect.satisfaction.df <- data_numeric %>% 
  select(1:7)

