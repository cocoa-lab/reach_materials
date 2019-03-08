# file for relating mean end-points to training variance
# essentially a sanity check
# model estimate should shift away from penalty as training variance grows

library(data.table)
library(dplyr)
library(ggplot2)
input = fread("fitness_input.csv")

# summaries of sigmas and non-zero mean model predictions by subject
data3 = input %>%
  filter(Experiment==3 & Model=='train_var' & Penalty != 0) %>%
  group_by(Subject) %>%
  summarise(mean_reach = mean(NormX,na.rm=TRUE), observed_sd = mean(Sigma))
data5 = input %>%
  filter(Experiment==5 & Model=='train_var' & Penalty != 0) %>%
  group_by(Subject) %>%
  summarise(mean_reach = mean(NormX,na.rm=TRUE), observed_sd = mean(Sigma))

# plots of mean model predictions by sigmas
ggplot(data3,aes(x=observed_sd,y=mean_reach)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_classic() +
  scale_y_continuous("Horizontal Mean of Reaching End-points in Test (mm)") +
  scale_x_continuous("Horizontal SD of Reaching End-points in Training (mm)") +
  ggtitle("Experiment 2")
ggsave("figures/exp2_aim_by_var.png", height=6,width=6,units="in")

ggplot(data5,aes(x=observed_sd,y=mean_reach)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_classic() +
  scale_y_continuous("Horizontal Mean of Reaching End-points in Test (mm)") +
  scale_x_continuous("Horizontal SD of Reaching End-points in Training (mm)") 
ggsave("figures/exp1_aim_by_var.png", height=6,width=6,units="in")

# correlations between mean model predictions and sigmas
cor.test(data3$mean_reach,data3$observed_sd,method="spearman")
cor.test(data5$mean_reach,data5$observed_sd,method="spearman")


# For Large-Sep-dist data from experiment 1
ls_input = fread("44_fitness_input.csv")
ls_data = ls_input %>%
  filter(Model=='train_var' & Penalty != 0) %>%
  group_by(Subject) %>%
  summarise(mean_reach = mean(NormX,na.rm=TRUE), observed_sd = mean(Sigma))

ggplot(ls_data,aes(x=observed_sd,y=mean_reach)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_classic() +
  scale_y_continuous("Horizontal Mean of Reaching End-points in Test (mm)") +
  scale_x_continuous("Horizontal SD of Reaching End-points in Training (mm)") +
  ggtitle("Experiment 1: Large Penalty-Reward Separation")
ggsave("figures/exp1_wide_aim_by_var.png", height=6,width=6,units="in")

cor.test(ls_data$mean_reach,ls_data$observed_sd,method="spearman")


# Analysis for data from 1.4R Separation Distance
