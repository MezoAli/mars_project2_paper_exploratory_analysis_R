rm(list = ls())
graphics.off()

install.packages("likert")

library(tidyverse)
library(psych)
library(gtsummary)
library(cardx)
library(corrplot)
library(likert)
library(janitor)
library(ggfortify)
library(broom)
library(car)
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



fa.results.1<- fa(data_numeric,nfactors = 2,rotate = "promax",fm = "ml")
fa.results.2 <- fa(data_numeric,nfactors = 2,rotate = "varimax",fm = "ml")
fa.diagram(fa.results.1)
fa.diagram(fa.results.2)
fa.results$e.values
scree(data_numeric)
fa.parallel(data_numeric, fa = "fa", n.iter = 100)
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

plot.subscale <- function(col1,col2,title){
  data_long <- data %>%
    pivot_longer(all_of(names(data)[col1:col2]), names_to = "Question", values_to = "Response") %>%
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
      title = title,
      y = "Percentage" ) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal()
}

plot.subscale(13,21,"Respect and Satisfaction on Communication Subscale")



names(data)

plot_barplot_fn(`Feeling not angry after nurse and physician interaction?`)
plot_barplot_fn(`Feeling not frustrated after nurse and physician interaction ?`)
plot_barplot_fn(`Feeling understood after nurse and physician interaction?`)
plot_barplot_fn(`Feeling pleased after nurse physician interaction?`)


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

plot.subscale(22,30,"Openness and Sharing Patient Information on Communication Subscale")


names(data[,22:30])
plot_barplot_fn(`In the event of a change in treatment plan, the nurse and the physicians have a mutual understanding`)
plot_barplot_fn(`Feeling not frustrated after nurse and physician interaction ?`)
plot_barplot_fn(`Feeling understood after nurse and physician interaction?`)
plot_barplot_fn(`Feeling pleased after nurse physician interaction?`)


# Max possible score
max_score <- 9 * 5  # 9 questions, max 5 points each = 45
# Min possible score
min_score <- 9 * 1  # 9 questions, max 5 points each = 9

respect_scores <- respect.satisfaction.df %>% 
  mutate(total_score = rowSums(across(everything()), na.rm = TRUE),
         sm_score = round((total_score - min_score) / (max_score - min_score) * 100,0))

fig1 <- respect_scores %>% 
  ggplot(.,aes(x = total_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1,linetype = "dotted") +
  scale_x_continuous(limits = c(32,45),
                     breaks = seq(32,45,2)) +
  labs(x = "Respect and Satisfaction sum score",
       y = "Density",
       title = "Fig.1 Respect and Satisfaction sub scale items sum score") +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))



fig2 <- respect_scores %>% 
  ggplot(.,aes(x = sm_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 4, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1,linetype = "dotted") +
  scale_x_continuous(limits = c(60,95),
                     breaks = seq(60,95,5)) +
  labs(x = "Respect and Satisfaction percentage",
       y = "Density",
       title = "Fig.2 Respect and Satisfaction %SM score") +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))


# Calculate mean and SD
mean_respect_scores <- mean(respect_scores$total_score)
sd_respect_scores <- sd(respect_scores$total_score)


# Convert to percentage
percent_mean_respect_scores <- (mean_respect_scores / max_score) * 100
percent_sd_respect_scores <- (sd_respect_scores / max_score) * 100



openness_scores <- openess.sharing.df %>% 
  mutate(total_score = rowSums(across(everything()), na.rm = TRUE),
         sm_score = round((total_score - min_score) / (max_score - min_score) * 100,0))

fig3 <- openness_scores %>% 
  ggplot(.,aes(x = total_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1,linetype = "dotted") +
  scale_x_continuous(limits = c(36,45),
                     breaks = seq(36,45,2)) +
  labs(x = "Openness and Sharing sum score",
       y = "Density",
       title = "Fig.3 Openness and Sharing on Information sub scale items sum score") +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

fig4 <- openness_scores %>% 
  ggplot(.,aes(x = sm_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1,linetype = "dotted") +
  scale_x_continuous(limits = c(70,95),
                     breaks = seq(70,95,5)) +
  labs(x = "Openness and Sharing Percentage",
       y = "Density",
       title = "Fig.4 Openness and Sharing on Information  %SM score") +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

# Calculate mean and SD
mean_openness_scores <- mean(openness_scores$total_score)
sd_openness_scores <- sd(openness_scores$total_score)


# Convert to percentage
percent_mean_openness_scores <- (mean_openness_scores / max_score) * 100
percent_sd_openness_scores <- (sd_openness_scores / max_score) * 100

#PCA

communication.df <- data_numeric %>% 
  select(1:18)

pca.communication <- prcomp(communication.df,scale. = TRUE)
summary(pca.communication)
plot(pca.communication$x[,1],pca.communication$x[,2])
pca.communication$rotation[,1:2]
pca.communication$x[,1:2]
pca.communication.var <- pca.communication$sdev^2
pca.communication.var.per <- round(pca.communication.var/sum(pca.communication.var) * 100,1)
barplot(pca.communication.var.per)


# regression and hypothesis testing
regression.df <- data %>% 
  select(1:12) %>%
  clean_names(.) %>% 
  add_column(respect_score = respect_scores$total_score,
             respect_percent = respect_scores$sm_score,
             openness_score = openness_scores$total_score,
             openness_percent = openness_scores$sm_score)

names(regression.df)

respect.regression <- lm(respect_percent ~ professional_category + working_hospital + sex
                         + age_in_years + marital_status + last_educational_qualification +
                           professional_training + salary_category_in_aed + position_presently_hold_in_the_hospital +
                           service_in_years + working_unit_category + race,data = regression.df) 
summary(respect.regression)
autoplot(respect.regression,which = 1:3,nrow = 3,ncol=1)
tidy(respect.regression)
glance(respect.regression)
broom::augment(respect.regression)
vif(respect.regression)
alias(respect.regression)


openness.regression <- lm(openness_percent ~ professional_category + working_hospital + sex
                         + age_in_years + marital_status + last_educational_qualification +
                           professional_training + salary_category_in_aed + position_presently_hold_in_the_hospital +
                           service_in_years + working_unit_category + race,data = regression.df) 
summary(openness.regression)
autoplot(openness.regression,which = 1:3,nrow = 3,ncol=1)
tidy(openness.regression,conf.int = T)
glance(openness.regression)
broom::augment(openness.regression)
vif(openness.regression)





# rest of domins
pca.results1 <- data_numeric %>% 
  select(19:ncol(data_numeric)) %>% 
  prcomp(.,scale. = T)

pca.results1
pca.results1.var <- pca.results1$sdev^2
pca.results1.var.per <- round(pca.results1.var/sum(pca.results1.var) * 100,1)
barplot(pca.results1.var.per)

# work attitude
max.attitude.score <- 6 * 5
min.attiude.score <- 6 * 1
attitude_scores <- data_numeric %>% 
  select(19:24) %>% 
  mutate(total_score = rowSums(across(everything()), na.rm = TRUE),
         sm_score = round((total_score - min.attiude.score) / (max.attitude.score - min.attiude.score) * 100,0))


fig5 <- attitude_scores %>% 
  ggplot(.,aes(x = total_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1,linetype = "dotted") +
  scale_x_continuous(limits = c(6,22),
                     breaks = seq(6,22,2)) +
  labs(x = "Work Attitude Factors sum score",
       y = "Density",
       title = "Fig.5 Work Attitude Factors sub scale items sum score") +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

fig6 <- attitude_scores %>% 
  ggplot(.,aes(x = sm_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 8, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1,linetype = "dotted") +
  scale_x_continuous(limits = c(0,65),
                     breaks = seq(0,65,5)) +
  labs(x = "Work Attitude Percentage",
       y = "Density",
       title = "Fig.6 Work Attitude Factors %SM score") +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))


# organizational factors

max.organization.score <- 5 * 5
min.organization.score <- 5 * 1
organization_scores <- data_numeric %>% 
  select(c(28,29,30,31,32)) %>% 
  mutate(total_score = rowSums(across(everything()), na.rm = TRUE),
         sm_score = round((total_score - min.organization.score) / (max.organization.score - min.organization.score) * 100,0))

fig7 <- organization_scores %>% 
  ggplot(.,aes(x = total_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1,linetype = "dotted") +
  scale_x_continuous(limits = c(2,14),
                     breaks = seq(2,14,1)) +
  labs(x = "Organizational Factors sum score",
       y = "Density",
       title = "Fig.7 Organizational Factors sub scale items sum score") +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))


fig8 <- organization_scores %>% 
  ggplot(.,aes(x = sm_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 6, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1,linetype = "dotted") +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,5)) +
  labs(x = "Organizational Factors Percentage",
       y = "Density",
       title = "Fig.6 Organizational Factors %SM score") +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))



# personal behavior

max.behavior.score <- 3 * 5
min.behavior.score <- 3 * 1
behavior_scores <- data_numeric %>% 
  select(c(25,26,27)) %>% 
  mutate(total_score = rowSums(across(everything()), na.rm = TRUE),
         sm_score = round((total_score - min.behavior.score) / (max.behavior.score - min.behavior.score) * 100,0))


