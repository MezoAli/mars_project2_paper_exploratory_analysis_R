rm(list = ls())
graphics.off()


# for running data manipulation
install.packages("tidyverse")
# for running the factor analysis and crohbach alpha tests
install.packages("psych")
#for creating publication ready tables
install.packages("gtsummary")
# helper package for gtsummary to do the hypothesis testing
install.packages("cardx")
# show correlation matrix plot
install.packages("corrplot")
# used to show regression assumptions plot to validate the model
install.packages("ggfortify")
# used to show regression coefficients in tidy form
install.packages("broom")
# used vif function to detect multicollinerity
install.packages("car")
# used to plot the PCA loadings, scores and and PCAs
install.packages("factoextra")


library(tidyverse)
library(psych)
library(gtsummary)
library(cardx)
library(corrplot)
library(ggfortify)
library(broom)
library(car)
library(factoextra)



# read the data
data <- read_csv("./data.csv")

##  ---------------------------------- helper functions -------------------------- START
# helper function to recode the levels of all factors to get consistant levels accross
# all variables
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

# helper function to plot the counts for each level inside single variable
# takes the var name as in the original dataset
# i will use the names(df) function to get the name of each original var before running
plot_barplot_fn <- function(var){
  require(tidyverse)
  var <- rlang::enquo(var)
  ggplot(data,aes(x = !!var,fill = !!var)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90,margin = margin(t=5),size = 8),
          plot.title = element_text(face = "bold",hjust = .5)) +
    ggtitle(paste("Participants",rlang::as_label(var),sep = " "))
}

# helper function to create subscale of all items within specific domin
# must specify 1st col number, 2nd col number and the title of the plot
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

##  ---------------------------------- helper functions -------------------------- END


# convert `service in years` into numeric values and replace NAs with 0
# convert all observations into lower case to avoid typos
# col`40` has a typo regarding rarely level so modify it
# convert needed variables into factors
# omit the observations that contain NA ( acc. to dr. Amira instractions)
data <- data %>% 
  mutate(`Service  (in years)` = as.numeric(`Service  (in years)`)) %>% 
  replace_na(list(`Service  (in years)` = 0)) %>%
  mutate(across(.cols = 13:44, tolower)) %>% 
  mutate(across(.cols = c(40), ~case_when(. == "rarley" ~ "rarely",
                                          T ~ .))) %>% 
  mutate(across(.cols = 13:44,.fn = as.factor)) %>% 
  na.omit()

summary(data)


# recode the selected coulmns to have consistant factors levels
# convert them into numeric values for further analysis
data_numeric <- data %>%
  select(13:44) %>%
  mutate(across(everything(), recode_scale)) %>%
  mutate(across(everything(), as.integer))


# re-name the variables names like the guidence files
names(data_numeric) <- c("angry","frustrated","understood","respected","pleased","satisfied","equal","talking",
                         "correct","change_ttt","pat_saftey","pat_care","schedule","exch_info","tired","help",
                         "listen","corr_info","non_compliance","Negligence","abuse","poor_att","uncooperative","gender_diff",
                         "unfavor_att","poor_commun","disruptive","differential_ttt","absc_forum","shar_vision","malfunctioning","suppl_short")

# create correlation matrix between all variables
cor.matrix = cor(data_numeric,use = "pairwise.complete.obs")

# show correlation matrix as a plot to detect relation reasily
cor.matrix.plot <- corrplot(cor.matrix,method = "square", type = "lower", tl.cex = 0.6)

# to show variables that shows strong relation between others
# results shows weak or moderate relations between variables and none shows 
# strong correlations
which(abs(cor.matrix) >= 0.7 & abs(cor.matrix) < 1, arr.ind = TRUE)

## Factor Analysis

# used to test sample adequency, however the overall = .19 which is very low
# recommended shreshold is like above 0.5 or 0.6
KMO.result <- KMO(data_numeric)

# sort the individual variables scores according to MSA
sort(KMO.result$MSAi,decreasing = TRUE)

# used to test the if the correlation matrix is an identity matrix(vars are uncorrelated)
# results : significant p-value so we reject the H0 and vaiables are correlated
cortest.bartlett(data_numeric)


# create demographic data tables
tb1 <- data %>% 
  select(1:12) %>% 
  tbl_summary() %>% 
  bold_labels() %>% 
  modify_caption("**Demographic Data**")
tb1

# create demographic data tables and test if there is significant
# difference between the professional category
tb2 <- data %>% 
  select(1:12) %>% 
  tbl_summary(by = `Professional category`) %>% 
  bold_labels() %>% 
  modify_caption("**Table 1. Participants' Characteristics By Profession (n = 37)**") %>% 
  add_p() %>% 
  bold_p()
tb2

# to get the names of variables we need to plot
names(data[,1:12])
# plot the demographic data levels counts
plot_barplot_fn(Race)
plot_barplot_fn(`Professional category`)
plot_barplot_fn(`Age in years`)
plot_barplot_fn(`Working unit category`)
plot_barplot_fn(`Salary category in AED`)
plot_barplot_fn(`Position presently hold in the hospital`)


# running factor analysis 
# fa.results.1<- fa(data_numeric,nfactors = 2,rotate = "promax",fm = "ml")
# fa.results.2 <- fa(data_numeric,nfactors = 2,rotate = "varimax",fm = "ml")
# fa.diagram(fa.results.1)
# fa.diagram(fa.results.2)
# fa.results.1$e.values
# fa.results.2$e.values
# scree(data_numeric)
# fa.parallel(data_numeric, fa = "fa", n.iter = 100)
# according to fa.results and fa.diagram, some questions need to be dropped due to
# very low loadings ( less than 0.3) and/or very high uniqueness (near to 1)
# and these questions are 8,13,14,16,19,22,25,27,28,31 but i will continue with
# the same questions according to instructor 



#-------------------- respect and satisfaction ------------------ START
respect.satisfaction.df <- data_numeric %>% 
  select(1:9)

# running cronbach alpha to test reliability of question
# results => alpha of 0.51 which is not so good (only marginal acceptance) and
# in future survey more items can be added or bad items should be removed
cron.alpha.respect <- alpha(respect.satisfaction.df,check.keys = T)


# create respect and satisfaction table and compare between professional groups to
# test if there is significant difference
tab3 <- data %>% 
  select(c(1,13:21)) %>% 
  tbl_summary(by = `Professional category`) %>% 
  bold_labels() %>% 
  add_p() %>% 
  bold_p()
tab3


# Frequency of perceived professional respect and satisfaction items
# during nurse-physician communication among nurses and physicians
tab4 <- data %>% 
  select(13:21) %>% 
  tbl_summary() %>% 
  modify_header(label = "Respect and Satisfaction on communication subscale items (a = 0.51)") %>% 
  modify_caption("**Table 2. Frequency of perceived professional respect and satisfaction items during nurse-physician communication among nurses and physicians (n = 37):**") %>%
  bold_labels()
tab4

# plot all variables with their responses
plot.subscale(13,21,"Respect and Satisfaction on Communication Subscale")


# to get the names of variables we need to plot
names(data[,13:21])
# plot the respect and satisfaction data levels counts
plot_barplot_fn(`Feeling not angry after nurse and physician interaction?`)
plot_barplot_fn(`Feeling not frustrated after nurse and physician interaction ?`)
plot_barplot_fn(`Feeling understood after nurse and physician interaction?`)
plot_barplot_fn(`Feeling pleased after nurse physician interaction?`)

#-------------------- respect and satisfaction ------------------ END

#-------------------- Openness and Sahring Information ------------------ START



openess.sharing.df <- data_numeric %>% 
  select(10:18)

# running cronbach alpha to test reliability of question
# results => alpha of 0.38 which is bad and in future survey more items
# can be added or bad items should be removed
cron.alpha.openess <- alpha(openess.sharing.df,check.keys = T)


# create Openness and Sahring Information table and compare between professional groups to
# test if there is significant difference
tab5 <- data %>% 
  select(c(1,22:30)) %>% 
  tbl_summary(by = `Professional category`) %>% 
  bold_labels() %>% 
  add_p() %>% 
  bold_p()
tab5


# Frequency of perceived Openness and Sharing of information items during
# nurse-physician communication among nurses and physicians
tab6 <- data %>% 
  select(22:30) %>% 
  tbl_summary() %>% 
  modify_header(label = "Openness & Sharing of information subscale item scpre (a = 0.38)") %>% 
  modify_caption("**Table 3. Frequency of perceived Openness and Sharing of information items during nurse-physician communication among nurses and physicians (n = 37):**") %>%
  bold_labels()
tab6


# plot all variables with their responses
plot.subscale(22,30,"Openness and Sharing Patient Information on Communication Subscale")

#to get the names of variables we need to plot
names(data[,22:30])
# plot the Openness and Sharing of information data levels counts
plot_barplot_fn(`In the event of a change in treatment plan, the nurse and the physicians have a mutual understanding`)
plot_barplot_fn(`Feeling not frustrated after nurse and physician interaction ?`)
plot_barplot_fn(`Feeling understood after nurse and physician interaction?`)
plot_barplot_fn(`Feeling pleased after nurse physician interaction?`)


#-------------------- Openness and Sahring Information ------------------ END


#-------------------- All Domins Figures ------------------ START
# Max possible score
respect_max_score <- 9 * 5  # 9 questions, max 5 points each = 45
# Min possible score
respect_min_score <- 9 * 1  # 9 questions, max 5 points each = 9

#add total score and sm score to the existing data frame include respect and satisfaction data
respect_scores <- respect.satisfaction.df %>% 
  mutate(total_score = rowSums(across(everything()), na.rm = TRUE),
         sm_score = round((total_score - respect_min_score) / (respect_max_score - respect_min_score) * 100,0))


# Respect and Satisfaction sub scale items sum score
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


# Respect and Satisfaction %SM score
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
percent_mean_respect_scores <- (mean_respect_scores / respect_max_score) * 100
percent_sd_respect_scores <- (sd_respect_scores / respect_max_score) * 100

# As shown in figures above, the perceived professional respect and satisfaction
# during nurse-physician communication had mean and maximum scale percentage mean score 
# 39.87±2.8 (Figure 1) and 88.59±6.22% (Figure 2) respectively.


# Max possible score
openness_max_score <- 9 * 5  # 9 questions, max 5 points each = 45
# Min possible score
openness_min_score <- 9 * 1  # 9 questions, max 5 points each = 9

openness_scores <- openess.sharing.df %>% 
  mutate(total_score = rowSums(across(everything()), na.rm = TRUE),
         sm_score = round((total_score - openness_min_score) / (openness_max_score - openness_min_score) * 100,0))


# Openness and Sharing on Information sub scale items sum score
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


# Openness and Sharing on Information  %SM score
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
percent_mean_openness_scores <- (mean_openness_scores / openness_max_score) * 100
percent_sd_openness_scores <- (sd_openness_scores / openness_max_score) * 100

# The results in the perceived openness and sharing of information during nurse-physician
# communication showed mean and maximum scale percentage mean score of
# 40.51±1.5 (Figure 3) and 90±3.3% (Figure 4) respectively.


# work attitude
max.attitude.score <- 6 * 5
min.attiude.score <- 6 * 1
attitude_scores <- data_numeric %>% 
  select(19:24) %>% 
  mutate(total_score = rowSums(across(everything()), na.rm = TRUE),
         sm_score = round((total_score - min.attiude.score) / (max.attitude.score - min.attiude.score) * 100,0))


# Work Attitude Factors sub scale items sum score
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


# Work Attitude Factors %SM score
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


# Organizational Factors sub scale items sum score
fig7 <- organization_scores %>% 
  ggplot(.,aes(x = total_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1,linetype = "dotted") +
  scale_x_continuous(limits = c(4,14),
                     breaks = seq(4,14,1)) +
  labs(x = "Organizational Factors sum score",
       y = "Density",
       title = "Fig.7 Organizational Factors sub scale items sum score") +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))



# Organizational Factors %SM score
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



# Personal Behavior Factors sub scale items sum score
fig9 <- behavior_scores %>% 
  ggplot(.,aes(x = total_score)) +
  geom_histogram(aes(y = ..density..), binwidth = .75, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1,linetype = "dotted") +
  scale_x_continuous(limits = c(2,6),
                     breaks = seq(2,6,0.5)) +
  labs(x = "Personal Behavior Factors sum score",
       y = "Density",
       title = "Fig.9 Personal Behavior Factors sub scale items sum score") +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))


# Personal Behavior Factors  %SM score
fig10 <- behavior_scores %>% 
  ggplot(.,aes(x = sm_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 3, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1,linetype = "dotted") +
  scale_x_continuous(limits = c(-2,20),
                     breaks = seq(-2,20,2)) +
  labs(x = "Personal Behavior Percentage",
       y = "Density",
       title = "Fig.9 Personal Behavior Factors  %SM score") +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

#-------------------- All Domins Figures ------------------ END


#PCA

communication.df <- data_numeric %>% 
  select(1:18)

pca.communication <- prcomp(communication.df,scale. = TRUE)
fviz_eig(pca.communication, addlabels = TRUE)
fviz_pca_biplot(pca.communication)
fviz_pca_var(pca.communication)
summary(pca.communication)
plot(pca.communication$x[,1],pca.communication$x[,2])
pca.communication$rotation[,1:2]
pca.communication$x[,1:2]
#pca.communication.var <- pca.communication$sdev^2
#pca.communication.var.per <- round(pca.communication.var/sum(pca.communication.var) * 100,1)
#barplot(pca.communication.var.per)


# regression and hypothesis testing
regression.df.domin1 <- data %>% 
  select(1:12) %>%
  clean_names(.) %>% 
  add_column(respect_percent = respect_scores$sm_score,
             openness_percent = openness_scores$sm_score)

names(regression.df.domin1)

respect.regression <- lm(respect_percent ~ professional_category + working_hospital + sex
                         + age_in_years + marital_status + last_educational_qualification +
                           professional_training + salary_category_in_aed + position_presently_hold_in_the_hospital +
                           service_in_years + working_unit_category + race,data = regression.df.domin1) 
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
                            service_in_years + working_unit_category + race,data = regression.df.domin1) 
summary(openness.regression)
autoplot(openness.regression,which = 1:3,nrow = 3,ncol=1)
tidy(openness.regression,conf.int = T)
glance(openness.regression)
broom::augment(openness.regression)
vif(openness.regression)


pca.results1 <- data_numeric %>% 
  select(19:ncol(data_numeric)) %>% 
  prcomp(.,scale. = T)

pca.results1
pca.results1.var <- pca.results1$sdev^2
pca.results1.var.per <- round(pca.results1.var/sum(pca.results1.var) * 100,1)
barplot(pca.results1.var.per)


# regression

regression.df.domin2 <- data %>% 
  select(1:12) %>%
  clean_names(.) %>% 
  add_column(attitude_percent = attitude_scores$sm_score,
             organization_percent = organization_scores$sm_score,
             behavior_percent = behavior_scores$sm_score
             )

attitude.regression <- lm(attitude_percent ~ professional_category + working_hospital + sex
                         + age_in_years + marital_status + last_educational_qualification +
                           professional_training + salary_category_in_aed + position_presently_hold_in_the_hospital +
                           service_in_years + working_unit_category + race,data = regression.df.domin2) 
summary(attitude.regression)
vif(attitude.regression)
autoplot(attitude.regression,which = 1:3,nrow = 3,ncol=1)
tidy(attitude.regression)
glance(attitude.regression)
broom::augment(attitude.regression)
alias(attitude.regression)


organization.regression <- lm(organization_percent ~ professional_category + working_hospital + sex
                          + age_in_years + marital_status + last_educational_qualification +
                            professional_training + salary_category_in_aed + position_presently_hold_in_the_hospital +
                            service_in_years + working_unit_category + race,data = regression.df.domin2) 
summary(organization.regression)
vif(organization.regression)
autoplot(organization.regression,which = 1:3,nrow = 3,ncol=1)
tidy(organization.regression)
glance(organization.regression)
broom::augment(organization.regression)
alias(organization.regression)


behavior.regression <- lm(behavior_percent ~ professional_category + working_hospital + sex
                              + age_in_years + marital_status + last_educational_qualification +
                                professional_training + salary_category_in_aed + position_presently_hold_in_the_hospital +
                                service_in_years + working_unit_category + race,data = regression.df.domin2) 
summary(behavior.regression)
vif(behavior.regression)
autoplot(behavior.regression,which = 1:3,nrow = 3,ncol=1)
tidy(behavior.regression)
glance(behavior.regression)
broom::augment(behavior.regression)
alias(behavior.regression)


