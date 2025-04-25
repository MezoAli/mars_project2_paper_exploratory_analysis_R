rm(list = ls())
graphics.off()

install.packages("likert")

library(tidyverse)
library(psych)
library(gtsummary)
library(cardx)
library(corrplot)
library(likert)

data<- read_csv("./data.csv")
names(data)

recode_scale <- function(x) {
  case_when(
    x == "always" ~ 5,
    x == "usually" ~ 4,
    x == "sometimes" ~ 3,
    x == "rarely" ~ 2,
    x == "never" ~ 1,
    TRUE ~ NA # Handle missing/other values
  )
}

data <- data %>% 
  mutate(`Service  (in years)` = as.numeric(`Service  (in years)`)) %>% 
  replace_na(list(`Service  (in years)` = 0)) %>%
  mutate(across(.cols = 13:44, tolower)) %>% 
  mutate(across(.cols = c(40), ~case_when(. == "rarley" ~ "rarely",
                                          T ~ .))) %>% 
  mutate(across(.cols = 13:44,.fn = as.factor)) %>% 
  na.omit()

summary(data)

data_numeric <- data %>%
  select(13:44) %>%
  mutate(across(everything(), recode_scale)) %>%
  mutate(across(everything(), as.integer))



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
  modify_caption("**Table 1. Participants' Characteristics By Profession (n = 37)**") %>% 
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


plot_barplot_fn(Race)
plot_barplot_fn(`Professional category`)
plot_barplot_fn(`Age in years`)
plot_barplot_fn(`Working unit category`)
plot_barplot_fn(`Salary category in AED`)



fa.results <- fa(data_numeric,nfactors = 2)
fa.diagram(fa.results)

# according to fa.results and fa.diagram, some questions need to be dropped due to
# very low loadings ( less than 0.3) and/or very high uniqueness (near to 1)
# and these questions are 8,13,14,16,19,22,25,27,28,31 but i will continue with
# the same questions according to instructor 


respect.satisfaction.df <- data_numeric %>% 
  select(1:9)

summary(respect.satisfaction.df)

cron.alpha.respect <- alpha(respect.satisfaction.df,check.keys = T)

tab3 <- data %>% 
  select(c(1,13:21)) %>% 
  tbl_summary(by = `Professional category`) %>% 
  bold_labels() %>% 
  add_p() %>% 
  bold_p()
tab3

tab4 <- data %>% 
  select(13:21) %>% 
  tbl_summary() %>% 
  modify_header(label = "Respect and Satisfaction on communication subscale items (a = 0.51)") %>% 
  modify_caption("**Table 2. Frequency of perceived professional respect and satisfaction items during nurse-physician communication among nurses and physicians (n = 37):**") %>%
  bold_labels()
tab4

names(data)

plot_barplot_fn(`Feeling not angry after nurse and physician interaction?`)
plot_barplot_fn(`Feeling not frustrated after nurse and physician interaction ?`)
plot_barplot_fn(`Feeling understood after nurse and physician interaction?`)
plot_barplot_fn(`Feeling pleased after nurse physician interaction?`)

data_long <- data %>%
  pivot_longer(c(13:21), names_to = "Question", values_to = "Response") %>%
  group_by(Question, Response) %>% 
  tally() %>% 
  group_by(Question) %>%
  mutate(Percentage = round(n / sum(n) * 100,0),
         Proportion = n / sum(n))

ggplot(data_long, aes(x = Question, y = Proportion, fill = Response)) +
  geom_col(position = "fill") +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  coord_flip() +
  labs(
    title = "Respect and Satisfaction on Communication Subscale",
    y = "Percentage" ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()



openess.sharing.df <- data_numeric %>% 
  select(10:18)

summary(openess.sharing.df)

cron.alpha.openess <- alpha(openess.sharing.df,check.keys = T)

tab5 <- data %>% 
  select(c(1,22:30)) %>% 
  tbl_summary(by = `Professional category`) %>% 
  bold_labels() %>% 
  add_p() %>% 
  bold_p()
tab5

tab6 <- data %>% 
  select(22:30) %>% 
  tbl_summary() %>% 
  modify_header(label = "Openness & Sharing of information subscale item scpre (a = 0.38)") %>% 
  modify_caption("**Table 3. Frequency of perceived Openness and Sharing of information items during nurse-physician communication among nurses and physicians (n = 37):**") %>%
  bold_labels()
tab6


names(data[,22:30])
plot_barplot_fn(`In the event of a change in treatment plan, the nurse and the physicians have a mutual understanding`)
plot_barplot_fn(`Feeling not frustrated after nurse and physician interaction ?`)
plot_barplot_fn(`Feeling understood after nurse and physician interaction?`)
plot_barplot_fn(`Feeling pleased after nurse physician interaction?`)

