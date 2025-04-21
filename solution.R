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

names(data_numeric) <- c("angry","frustrated","understood","respected","pleased","satisfied","equal","talking",
                         "correct","change_ttt","pat_saftey","pat_care","schedule","exch_info","tired","help",
                         "listen","corr_info","non_compliance","Negligence","abuse","poor_att","uncooperative","gender_diff",
                         "unfavor_att","poor_commun","disruptive","differential_ttt","absc_forum","shar_vision","malfunctioning","suppl_short")

cor.matrix = cor(data_numeric,use = "pairwise.complete.obs")

cor.matrix.plot <- corrplot(cor.matrix,method = "square", type = "lower", tl.cex = 0.6)

which(abs(cor.matrix) > 0.7 & abs(cor.matrix) < 1, arr.ind = TRUE)



KMO.result <- KMO(data_numeric)

sort(KMO.result$MSAi,decreasing = TRUE)

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
plot_barplot_fn(salary_category_in_aed)



fa.results <- fa(data_numeric,nfactors = 2)
fa.diagram(fa.results)

# according to fa.results and fa.diagram, some questions need to be dropped due to
# very low loadings ( less than 0.3) and/or very high uniqueness (near to 1)
# and these questions are 8,13,14,16,19,22,25,27,28,31 but i will continue with
# the same questions according to instructor 


respect.satisfaction.df <- data_numeric_modified %>% 
  select(1:7) %>% 
  mutate(across(everything(),~replace_na(.,0)))

cron.alpha.respect <- alpha(respect.satisfaction.df,check.keys = T)

