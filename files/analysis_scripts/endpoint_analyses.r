# follow-up exploration of data
library(dplyr)
library(ggplot2)
library(data.table)
library(ggforce)
library(lme4)
library(nlme)
library(plotrix)

pix_to_mm = function(N) {
  #mm between pixels HP Pavilion 23tm
  MMBP = 0.265
  return(MMBP*N)
}


setwd("/Users/adkinsty/Box Sync/LeeLab/Experiments/Exp_files/reaching/new_analyses/")

# Experiment 1
exp1.data = fread("../standard_data/all_standard_test_data5.csv")[,-1] %>%
  select(-c(1,2)) %>% # get rid of initial junk columns
  mutate(rew = rewardVal,
         pen = penaltyVal,
         isPen = ifelse(pen==0,0,1),
         prr = abs(penaltyVal)/rewardVal,
         sep = factor(abs(sepDist),levels=c(32,44)),
         sub = factor(subject),
         x = pix_to_mm(standardPokePosX),
         prev_pen = shift(ifelse(score<0,1,0),1,"right")) %>%
  select(c(x,rew,prr,pen,sep,sub,targetPos,block,prev_pen,isPen))

# sep & prr effects
null = lmer(exp1.data,formula= x ~ sep + (1+prr+sep|sub))
prr  = lmer(exp1.data,formula= x ~ sep + prr + (1+prr+sep|sub)) # prr=penalty:reward
s = anova(null,prr)
sep  = lmer(exp1.data,formula= x ~ sep + (1+isPen|sub))
sep_ef = anova(null,sep)
null2 = lmer(exp1.data,formula= x ~ sep + prr + (1|sub))
intr = lmer(exp1.data,formula= x ~ sep + prr + sep*prr + (1|sub))
intr_ef = anova(null2,intr)
isPen = lmer(exp1.data,formula= x ~ sep + isPen + (1+isPen|sub))



# plot of x ~ sep+prr+sep*prr
pr_dt = exp1.data%>%group_by(sub,prr,sep)%>%
  summarise(x=mean(x,na.rm=TRUE)) %>% group_by(prr,sep) %>%
  summarise(se=std.error(x,na.rm=TRUE),x=mean(x,na.rm=TRUE)) %>%
  mutate(pr=factor(prr,levels=c(0,1,5)))
ggplot(pr_dt,aes(x=pr,y=x,colour=sep,group=sep))+geom_point()+geom_line()+
  geom_errorbar(aes(ymin=x-se,ymax=x+se),width=0.05)

# Experiment 2
exp2.data = fread("../standard_data/all_standard_test_data3.csv")[,-1] %>%
  dplyr::select(-c(1)) %>% # get rid of initial junk columns
  mutate(pen = abs(pen_val),
         sub = subject,
         x = standardPokePosX,
         prev_pen = shift(ifelse(points<0,1,0),1,"right")) %>%
  dplyr::select(pen,sub,x,prev_pen)

# penalty effect
null = lmer(exp2.data,formula= x ~ (1+pen|sub))
pen  = lmer(exp2.data,formula= x ~ pen + (1+pen|sub))
pen_ef = anova(null,pen)
# plot of x ~ pen
pr_dt = exp2.data%>%group_by(sub,pen)%>%
  summarise(x=mean(x,na.rm=TRUE)) %>% group_by(pen) %>%
  summarise(se=std.error(x,na.rm=TRUE),x=mean(x,na.rm=TRUE)) %>%
  mutate(pen=factor(as.character(pen),levels=c("0","-100","-500")))
ggplot(pr_dt,aes(x=pen,y=x))+geom_point()+geom_line()+
  geom_errorbar(aes(ymin=x-se,ymax=x+se),width=0.05)
