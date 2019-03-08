# follow-up exploration of data
library(dplyr)
library(ggplot2)
library(data.table)
library(ggforce)
library(lme4)
library(nlme)
library(plotrix)
library(mvtnorm)
library(tidyr)
library(scales)

setwd("/Users/adkinsty/Box Sync/LeeLab/Experiments/Exp_files/reaching/new_analyses/")

##################################################################################################################
# Model Fitting
##################################################################################################################
# Compute -log p(d|m) for each observation

thickness = 3 # px thickness of reward circle for experiment 2

all_model_fit_input = fread("model_fit_input.csv") %>%
  mutate(aimX = ifelse(model!="heuristic",aimX, # else
                       ifelse(pen==0,0, # else
                              ifelse(exp==2,(penX+radius)+(abs(penX)-(thickness/2))/2, # else
                                     (penX+radius)+abs(penX)/2))),
         prr = ifelse(pen==0,0,ifelse(pen%in%c(-1,-3,-100),1,5)))

model_fits = all_model_fit_input %>%
  filter(lambda > -15 & lambda <= -1 & !is.na(lambda)) %>%
  rowwise() %>%
  mutate(aimX = ifelse(pen==0,0,aimX)) %>%
  mutate(fit = -dmvnorm(x=c(reachX,reachY),
                        mean=c(aimX,aimY),
                        sigma=matrix(c(varX,cov,cov,varY),ncol=2),
                        log=TRUE),
         pen = factor(pen,levels=c(0,-1,-3,-5,-15,-100,-500)),
         prr = factor(prr,levels=c(0,1,5)),
         exp = factor(exp,levels=c(1,2)))

sub_cond_fits = model_fits %>%
  filter(lambda > -15 & lambda <= -1) %>%
  group_by(exp,sub,prr,penX,model) %>%
  summarise(fit = sum(-fit,na.rm=TRUE))
sub_fits = model_fits %>%
  filter(lambda > -15 & lambda <= -1) %>%
  group_by(sub,model) %>%
  summarise(fit = sum(-fit,na.rm=TRUE))
cond_fits = model_fits %>%
  filter(lambda > -15 & lambda <= -1) %>%
  group_by(exp,prr,penX,model) %>%
  summarise(fit = sum(-fit,na.rm=TRUE))
glob_fits = sub_fits %>%
  group_by(model) %>%
  summarise(fit = mean(fit,na.rm=TRUE))

##################################################################################################################
# Vuong's tests
##################################################################################################################
vuong_stat    = function(ll1, ll2) {
  # computes the vuong statistic to test for
  # a significant difference between two models
  # in terms of how well they fit some observed dataset
  #
  # ll1 is a vector of logliklihoods for some model
  # ll2 is a vector of logliklihoods for another model

  # differences of log probabiltiies of the two models at every observed data-point
  ll.ratios = ll1 - ll2

  llr.mean  = mean(ll.ratios, na.rm = TRUE) # mean of log-likelihood differences
  llr.s     = sd(ll.ratios, na.rm = TRUE)   # standard deviation of log-likelihood differences
  # vuong statistic, from voung.test() documentation
  vuong = sqrt(length(ll1)) * llr.mean / llr.s
  return(vuong)
}

vuong_p = function(vuong_stat){
  # P2 is the p-value we want; two-tailed and more interpretable
  p1 = pnorm(vuong_stat)
  if (is.nan(p1)) {
    p2 = NaN
  } else {
    if (p1 < 0.5) {
      p2 = 2*p1
    } else {
      p2 = 2*(1-p1)
    }
  }
  return(p2)
}

loss_averse_fits = model_fits %>% ungroup %>% filter(model=="loss_averse")
ideal_fits       = model_fits  %>% ungroup %>% filter(model=="ideal")
heuristic_fits   = model_fits %>% ungroup %>% filter(model=="heuristic")
payoff_fits      = model_fits %>% ungroup %>% filter(model=="mean_payoff")
var_fits         = model_fits %>% ungroup %>% filter(model=="mean_var")

vuongs_in = loss_averse_fits %>%
  select(prr,penX,exp,sub,reachX,reachY,lambda) %>%
  mutate(la_fit = loss_averse_fits$fit,ideal_fit=ideal_fits$fit,heur_fit=heuristic_fits$fit,
         pay_fit=payoff_fits$fit,var_fit=var_fits$fit)

exp.vuongs = vuongs_in %>% group_by(exp,penX) %>%
  summarise(heur_ideal_v = vuong_stat(-heur_fit,-ideal_fit), heur_ideal_p = vuong_p(heur_ideal_v),
            heur_la_v = vuong_stat(-heur_fit,-la_fit),heur_la_p = vuong_p(heur_la_v),
            ideal_la_v = vuong_stat(-ideal_fit,-la_fit), ideal_la_p = vuong_p(ideal_la_v))

pen.vuongs = vuongs_in %>% group_by(exp,penX,prr) %>%
  summarise(heur_ideal_v = vuong_stat(-heur_fit,-ideal_fit),heur_ideal_p = vuong_p(heur_ideal_v),
            heur_la_v = vuong_stat(-heur_fit,-la_fit),heur_la_p = vuong_p(heur_la_v),
            ideal_la_v = vuong_stat(-ideal_fit,-la_fit),ideal_la_p = vuong_p(ideal_la_v))

sub.vuongs = vuongs_in %>% group_by(exp,penX,sub) %>%
  summarise(heur_ideal_v = vuong_stat(-heur_fit,-ideal_fit), heur_ideal_p = vuong_p(heur_ideal_v),
            heur_la_v = vuong_stat(-heur_fit,-la_fit), heur_la_p = vuong_p(heur_la_v),
            ideal_la_v = vuong_stat(-ideal_fit,-la_fit),ideal_la_p = vuong_p(ideal_la_v)) %>%
  mutate(better_model = ifelse(heur_ideal_p < 0.05 & heur_ideal_v > 0,"heuristic",
                        ifelse(heur_ideal_p < 0.05 & heur_ideal_v < 0,"ideal","none")))
  # mutate(best_model = ifelse((heur_ideal_p < 0.05 & heur_ideal_v > 0) & (heur_la_p < 0.05 & heur_la_v > 0), "heuristic",
  #                     ifelse((heur_ideal_p < 0.05 & heur_ideal_v < 0) & (ideal_la_p < 0.05 & ideal_la_v > 0), "ideal",
  #                     ifelse((ideal_la_p < 0.05 & ideal_la_v < 0) & (heur_la_p < 0.05 & heur_ideal_v < 0), "loss_averse",
  #                             "none"))))


##################################################################################################################
# Visualization
##################################################################################################################

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#theme_update(plot.title = element_text(hjust = 0.5))

##################################################################################################################
# Model fit plots
##################################################################################################################
exp1.cond_fits = cond_fits%>%filter(exp==1 & penX==-32 & prr!=0) %>%
  mutate(model=factor(model,
                      levels=c("heuristic","ideal","loss_averse",
                               "mean_var_pay","mean_payoff","mean_var")))
f1 = ggplot(exp1.cond_fits,aes(x=prr,y=-1/fit,colour=model,group=model)) +
  #geom_point(size=2,position=position_dodge(0.)) +
  geom_linerange(aes(ymin=0.0001575,ymax=-1/fit,x=prr),
                 size=8,position=position_dodge(.88)) +
  scale_x_discrete("Penalty to Reward Ratio",labels=c("1:1","5:1")) +
  scale_y_continuous("Model Fit") +
  scale_colour_manual("Model",
                      labels=c("Heuristic","Optimal","Loss-averse",
                               "Ignore P & V","Ignore Payoff","Ignore Variability"),
                      values=c("#56B4E9" ,"#009E73","#E69F00","#0072B2","#D55E00","#000000")) +
  guides(linetype=FALSE) +
  ggtitle("Experiment 1") +
  #coord_flip() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,vjust=-3.5))

exp2.cond_fits = cond_fits%>%filter(exp==2 & prr!=0) %>%
  mutate(model=factor(model,
                      levels=c("heuristic","ideal","loss_averse",
                               "mean_var_pay","mean_payoff","mean_var")))
f2 = ggplot(exp2.cond_fits,aes(x=prr,y=-1/fit,colour=model,group=model)) +
  #geom_point(size=2,position=position_dodge(0.)) +
  geom_linerange(aes(ymin=0.000099,ymax=-1/fit,x=prr),
                 size=8,position=position_dodge(.88)) +
  scale_x_discrete("Penalty to Reward Ratio", labels=c("1:1","5:1")) +
  scale_y_continuous("Model Fit") +
  scale_colour_manual("Model",
                      labels=c("Heuristic","Optimal","Loss-averse",
                               "Ignore P & V","Ignore Payoff","Ignore Variability"),
                      values=c("#56B4E9" ,"#009E73","#E69F00","#0072B2","#D55E00","#000000")) +
  guides(linetype=FALSE) +
  ggtitle("Experiment 2") +
  #coord_flip() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,vjust=-5))


png(filename = "mod_fit_plot.png", units = "in", res = 400,height=4,width=12, pointsize = 12, bg = "white")
multiplot(f1,f2,cols=2)
dev.off()
# Model fit overall
ggplot(glob_fits,aes(x=model,y=-1/fit,group=1)) + geom_point() + geom_line() +
  scale_y_continuous("Model Fit") + theme_classic()

##################################################################################################################
# Predicted mean with observed mean
##################################################################################################################
obs_by_pred = all_model_fit_input %>%
  #filter(lambda > -15 & lambda <= -1) %>%
  mutate(exp  = factor(exp,levels=c(1,2)),aimX = ifelse(pen==0,0,aimX),
         penX = factor(penX,levels=c(-44,-32)),
         pen  = factor(pen,levels=c(0,-1,-3,-5,-15,-100,-500)),
         prr = factor(prr,levels=c(0,1,5))) %>%
  group_by(sub,prr,penX,exp,model) %>%
  summarise(obs=mean(reachX,na.rm=TRUE),
            pred=mean(aimX,na.rm=TRUE),
            varX = mean(varX),EV=mean(EV),
            obsY=mean(reachY,na.rm=TRUE),
            varY = mean(varY),cov=mean(cov),
            penY=mean(penY))

obs_model = obs_by_pred %>% filter(model=="ideal") %>%
  mutate(model = "observed", pred = obs)

obs_and_pred = obs_by_pred %>% rbind(obs_model)

e1_obs_and_pred = obs_and_pred %>% filter(exp==1 & penX==-32 & prr != 0)
dodge=.5
oap1 = ggplot(e1_obs_and_pred,aes(x=prr,y=pred,fill=model,group=model))+
  #geom_hline(linetype="dashed",yintercept=32, color='black',alpha=0.2,size=2) +
  stat_summary(fun.y="mean", geom="bar", alpha=0.8,position=position_dodge(dodge),width=0.5) +
  #geom_hline(linetype="solid",yintercept=0, color='red',alpha=0.2,size=2) +
  geom_point(aes(colour=model),position=position_dodge(dodge),size=.9,alpha=1,show.legend = FALSE) +
  scale_x_discrete("Penalty to Reward Ratio",labels=c("1:1","5:1")) +
  scale_y_continuous("Shift from Penalty Region (pixels)",limits = c(0,32)) +
  scale_fill_manual("Model",
                      labels=c("Heuristic","Optimal","Loss-averse","Observed"),
                      values=c("#56B4E9" ,"#009E73","#E69F00","#000000")) +
  scale_colour_manual("Model",
                    labels=c("Heuristic","Optimal","Loss-averse","Observed"),
                    values=c("#56B4E9" ,"#009E73","#E69F00","#000000")) +
  guides(colour=FALSE,linetype=FALSE) +
  theme_classic() +
  ggtitle("Experiment 1") +
  theme(plot.title = element_text(hjust = 0.5,vjust=-5)) +
  theme(legend.justification = 'right',
        legend.key.size =  unit(0.2, "in"),
        legend.position=c(.95,0.2)) + coord_flip()

e2_obs_and_pred = obs_and_pred %>% filter(exp==2 & prr != 0)
dodge=0.5
oap2 = ggplot(e2_obs_and_pred,aes(x=prr,y=pred,fill=model,colour=model,group=model))+
  #geom_hline(linetype="dashed",yintercept=32, color='black',alpha=0.2,size=2) +
  stat_summary(fun.y="mean", geom="bar", alpha=0.8,position=position_dodge(dodge),width=0.5) +
  #geom_hline(linetype="solid",yintercept=0, color='red',alpha=0.2,size=2) +
  geom_point(position=position_dodge(dodge),size=.9,alpha=1,show.legend = FALSE) +
  scale_x_discrete("Penalty to Reward Ratio",labels=c("1:1","5:1")) +
  scale_y_continuous("Shift from Penalty Region (pixels)",limits = c(0,32)) +
  scale_fill_manual("Model",
                    labels=c("Heuristic","Optimal","Loss-averse","Observed"),
                    values=c("#56B4E9" ,"#009E73","#E69F00","#000000")) +
  scale_colour_manual("Model",
                      labels=c("Heuristic","Optimal","Loss-averse","Observed"),
                      values=c("#56B4E9" ,"#009E73","#E69F00","#000000")) +
  guides(fill=FALSE,colour=FALSE,linetype=FALSE) +
  theme_classic() +
  ggtitle("Experiment 2") +
  theme(plot.title = element_text(hjust = 0.5,vjust=-5)) +
  theme(legend.justification = 'right', legend.key.size =  unit(0.15, "in"),
        legend.position=c(.9,0.2)) + coord_flip()

png(filename = "obs_and_pred_plot.png", units = "in", res = 400, height=4,width=8, pointsize = 12, bg = "white")
multiplot(oap1,oap2,cols = 2)
dev.off()


png(filename = "obs_pred_fit_plot.png",
    units = "in", res = 400, height=8,width=9,
    pointsize = 12, bg = "white")
multiplot(oap1,oap2,f1,f2,cols = 2)
dev.off()






##################################################################################################################
# Observed mean by predicted mean plots
##################################################################################################################
ggplot(obs_by_pred,aes(x=pred,y=obs,colour=exp,group=exp))+
  geom_abline(slope=1,intercept=0) +
  #geom_smooth(method="lm",size=0.75,se=TRUE) +
  geom_point(aes(colour=exp),size=0.5,alpha=0.5) +
  facet_grid(penX~model) +
  xlim(-2,20) + ylim(-2,20) +
  coord_fixed() + theme_classic()


##################################################################################################################
# Observed mean by condition plots
##################################################################################################################
obs =  obs_by_pred %>%
  group_by(sub,pen,penX,exp) %>%
  summarise(obs=mean(obs,na.rm=TRUE),varX = mean(varX))
s32exp1.obs = obs %>% filter(exp==1) %>% filter(penX==-32)
o132 = ggplot(s32exp1.obs,aes(x=pen,y=obs,group=penX)) +
  geom_point(position=position_jitter(.15),size=0.5,alpha=0.2) +
  stat_summary(fun.y="mean", geom="point", size=2) +
  stat_summary(fun.y="mean", geom="line",size=1) +
  scale_x_discrete("") +
  scale_y_continuous("Observed mean endpoint (px)",limits = c(0,25)) +
  theme_classic() +
  ggtitle("(a) Experiment 1: 32 px") +
  theme(plot.title = element_text(hjust = -0.1,face="bold")) +
  guides(colour=FALSE,linetype=FALSE)
s44exp1.obs = obs %>% filter(exp==1) %>% filter(penX==-44)
o144 = ggplot(s44exp1.obs,aes(x=pen,y=obs,group=penX)) +
  geom_point(position=position_jitter(.15),size=0.5,alpha=0.2) +
  stat_summary(fun.y="mean", geom="point", size=2) +
  stat_summary(fun.y="mean", geom="line",size=1) +
  scale_x_discrete("") +
  scale_y_continuous("",limits = c(0,25)) +
  theme_classic() +
  ggtitle("(b) Experiment 1: 44 px") +
  theme(plot.title = element_text(hjust = -0.1,face="bold")) +
  guides(colour=FALSE,linetype=FALSE)

exp2.obs = obs %>% filter(exp==2)
o2 = ggplot(exp2.obs,aes(x=pen,y=obs,group=penX)) +
  geom_point(position=position_jitter(.15),size=0.5,alpha=0.2) +
  stat_summary(fun.y="sd", geom="errorbar",width=0.03, alpha=0.8) +
  stat_summary(fun.y="mean", geom="point", size=2) +
  stat_summary(fun.y="mean", geom="line",size=1) +
  scale_x_discrete("") +
  scale_y_continuous("",limits = c(0,25)) +
  theme_classic() +
  ggtitle("(c) Experiment 2") +
  theme(plot.title = element_text(hjust = -0.1,face="bold")) +
  guides(colour=FALSE,linetype=FALSE)

##################################################################################################################
# Predicted mean by condition
##################################################################################################################
s32exp1.obs_by_pred = obs_by_pred %>% filter(exp==1) %>% filter(penX==-32)
p132 = ggplot(s32exp1.obs_by_pred,aes(x=pen,y=pred,colour=model,group=model)) +
  geom_point(position=position_jitter(.15),size=0.5,alpha=0.2) +
  stat_summary(fun.y="mean", geom="point", size=2) +
  stat_summary(fun.y="mean", geom="line",size=1) +
  scale_x_discrete("Penalty Value ($)") +
  scale_y_continuous("Predicted mean endpoint (px)",limits = c(0,25)) +
  scale_colour_manual("Model",labels=c("Heuristic","Ideal","Loss-averse"),values=c("#56B4E9" ,"#009E73","#E69F00")) +
  guides(linetype=FALSE) +
  theme_classic() +
  ggtitle("(d)") +
  theme(plot.title = element_text(hjust = -0.1,face="bold")) +
  theme(legend.justification = 'right', legend.key.size =  unit(0.15, "in"),
        legend.position=c(1,0.3))

s44exp1.obs_by_pred = obs_by_pred %>% filter(exp==1) %>% filter(penX==-44)
p144 = ggplot(s44exp1.obs_by_pred,aes(x=pen,y=pred,colour=model,group=model)) +
  geom_point(position=position_jitter(.15),size=0.5,alpha=0.2) +
  stat_summary(fun.y="mean", geom="point", size=2) +
  stat_summary(fun.y="mean", geom="line",size=1) +
  scale_x_discrete("Penalty ($)") +
  scale_y_continuous("",limits = c(0,25)) +
  scale_colour_manual(values=c("#56B4E9" ,"#009E73","#E69F00")) +
  guides(colour=FALSE,linetype=FALSE) +
  theme_classic() +
  ggtitle("(e)") +
  theme(plot.title = element_text(hjust = -0.1,face="bold"))

exp2.obs_by_pred = obs_by_pred %>% filter(exp==2)
p2 = ggplot(exp2.obs_by_pred,aes(x=pen,y=pred,colour=model,group=model)) +
  geom_point(position=position_jitter(.15),size=0.5,alpha=0.2) +
  stat_summary(fun.y="mean", geom="point", size=2) +
  stat_summary(fun.y="mean", geom="line",size=1) +
  scale_x_discrete("Penalty Value (points)") +
  scale_y_continuous("",limits = c(0,25)) +
  scale_colour_manual(values=c("#56B4E9" ,"#009E73","#E69F00")) +
  guides(colour=FALSE,linetype=FALSE) +
  theme_classic() +
  ggtitle("(f)") +
  theme(plot.title = element_text(hjust = -0.1,face="bold"))

png(filename = "obs_pred_plot.png", units = "in", res = 200,height=6,width=9, pointsize = 32, bg = "white")
multiplot(o132,p132,o144,p144,o2,p2,cols = 3)
dev.off()

##################################################################################################################
# Observed mean by estimated variability
##################################################################################################################
sub_mean_sd = obs_by_pred %>% group_by(sub,exp,model,penX) %>%
  summarise(obs=mean(obs),pred=mean(pred),sdX=sqrt(mean(varX)),ev=mean(ev))

ggplot(sub_mean_sd,aes(x=sdX,y=obs,linetype=penX,group=penX)) +
  geom_point(size=0.75,alpha=0.75) + geom_smooth(method="lm") +
  ylim(0,20) + theme_classic()


##################################################################################################################
# Expected value
##################################################################################################################
obs_by_pred = all_model_fit_input %>%
  #filter(lambda > -15 & lambda <= -1) %>%
  mutate(exp  = factor(exp,levels=c(1,2)),aimX = ifelse(pen==0,0,aimX),
         penX = factor(penX,levels=c(-44,-32)),
         pen  = factor(pen,levels=c(0,-1,-3,-5,-15,-100,-500)),
         prr = factor(prr,levels=c(0,1,5))) %>%
  group_by(sub,pen,penX,exp,model) %>%
  summarise(obs=mean(reachX,na.rm=TRUE),
            pred=mean(aimX,na.rm=TRUE),
            varX = mean(varX),EV=mean(EV),
            obsY=mean(reachY,na.rm=TRUE),
            varY = mean(varY),cov=mean(cov),
            penY=mean(penY))
eff_in = obs_by_pred %>% ungroup() %>% filter(model=="ideal") %>% mutate(pen=as.numeric(levels(pen))[pen],
                                                                         penX=as.numeric(levels(penX))[penX])

evs = c() # for storing expected values of aim-points
nSamples = 100000
for (i in 1:nrow(eff_in)) {
  # get trial conditions
  d = eff_in[i,]; exp = d$exp; pen = mean(as.numeric(d$pen)); rew = ifelse(exp==1,ifelse(pen%in%c(0,-1,-5),1,3),100)

  # get subject's loss-aversion, letting it be 1 if the estimated paramter is unrealistic
  lambda = 1 #ifelse(is.na(mean(d$lambda)) | mean(d$lambda) > -1 | mean(d$lambda) < -20, 1, abs(d$lambda))
  # bivariate normal distribution around the aim-point, with sub's sd estimated from training
  samples = rmvnorm(nSamples,c(d$obs,d$obsY),sigma=matrix(c(d$varX,d$cov,d$cov,d$varY),ncol=2))
  # sample's distances from centers of circle
  rewDist = dist2D(samples[,1],samples[,2],0,0); penDist = dist2D(samples[,1],samples[,2],d$penX,d$penY)
  # get cummulative score across samples
  score = ifelse(rewDist<radius & penDist<radius,rew+pen*lambda, # if in both cirlces, sum pen*lambda and rew
                 ifelse(rewDist<radius & penDist>radius,rew, # if only in reward, get reward
                        ifelse(rewDist>radius & penDist<radius, pen*lambda, # if only in penalty, get penalty*lambda
                               0))) # otherwise get 0
  # compute mean score (expected value)
  ev = mean(score); evs = append(evs,ev)
}

#############################################################################################################
efficiency = eff_in %>%
  mutate(obs_ev = evs, penXf = factor(penX,levels=c(-32,-44)),
         penf = factor(pen,levels=c(0,-1,-3,-5,-15,-100,-500)),
         eff = obs_ev / EV)
#write.csv2(efficiency,"efficiencies.csv")

#efficiency = fread("efficiencies.csv")
exp1.32.eff = efficiency %>% filter(exp==1 & penX == -32)
e1.32 = ggplot(exp1.32.eff,aes(x=penf,y=eff,group=penXf)) +
  geom_point(position=position_jitter(.15),size=0.5,alpha=0.2) +
  stat_summary(fun.y="mean", geom="point", size=2) +
  stat_summary(fun.y="mean", geom="line",size=1) +
  scale_x_discrete("Penalty Value ($)") +
  scale_y_continuous("Efficiency",limits=c(0.7,1.01)) +
  theme_classic() +
  ggtitle("(a) Experiment 1") +
  theme(plot.title = element_text(hjust = -0.05,face="bold")) +
  guides(colour=FALSE,linetype=FALSE)
exp1.44.eff = efficiency %>% filter(exp==1 & penX == -44)
# e1.44 = ggplot(exp1.44.eff,aes(x=penf,y=eff,group=penXf)) +
#   geom_point(position=position_jitter(.15),size=0.5,alpha=0.2) +
#   stat_summary(fun.y="mean", geom="point", size=2) +
#   stat_summary(fun.y="mean", geom="line",size=1) +
#   scale_x_discrete("") +
#   scale_y_continuous("Observed / Ideal EV") +
#   theme_classic() +
#   ggtitle("(b)") +
#   theme(plot.title = element_text(hjust = -0.1,face="bold")) +
#   guides(colour=FALSE,linetype=FALSE)
exp2.eff = efficiency %>% filter(exp==2)
e2 = ggplot(exp2.eff,aes(x=penf,y=eff,group=penX)) +
  geom_point(position=position_jitter(.15),size=0.5,alpha=0.2) +
  stat_summary(fun.y="mean", geom="point", size=2) +
  stat_summary(fun.y="mean", geom="line",size=1) +
  scale_x_discrete("Penalty Value (ponts)") +
  scale_y_continuous("",limits=c(0.7,1.01)) +
  theme_classic() +
  ggtitle("(b) Experiment 2") +
  theme(plot.title = element_text(hjust = -0.05,face="bold")) +
  guides(colour=FALSE,linetype=FALSE)


png(filename = "eff_plot.png", units = "in", res = 400,height=4,width=8, pointsize = 12, bg = "white")
multiplot(e1.32,e2,cols = 2)
dev.off()

png(filename = "mod_fit_eff_plot.png", units = "in", res = 200,height=8,width=8, pointsize = 32, bg = "white")
multiplot(f1,e1.32,f2,e2,cols=2)
dev.off()

####################################################################################
# efficiency models
####################################################################################
e2.eff = exp2.eff %>% mutate(prr = ifelse(pen==0,0,ifelse(pen==-100,1,5)))
null2 = lmer(data=e2.eff,formula=eff ~ (1 + prr|sub))
prr.2 = lmer(data=e2.eff,formula=eff ~ prr + (1 + prr|sub))
prr.eff = anova(prr.2,null2)
#
# e1eff = efficiency %>% filter(exp==1) %>% mutate(prr = factor(ifelse(pen==0,0,ifelse(pen%in%c(-1,-3),1,5))))
# null.1.32 = lmer(data=e1.32.eff,formula=eff ~ Pe (1|sub))
# prr.1.32 = lmer(data=e1.32.eff,formula=eff ~ prr + (1|sub))
# prr.1.32.eff = anova(prr.1.32,null.1.32)

e1.32.eff = exp1.32.eff %>% mutate(prr = factor(ifelse(pen==0,0,ifelse(pen%in%c(-1,-3),1,5))))
null.1.32 = lmer(data=e1.32.eff,formula=eff ~ (1 + prr|sub))
prr.1.32 = lmer(data=e1.32.eff,formula=eff ~ prr + (1 + prr|sub))
prr.1.32.eff = anova(prr.1.32,null.1.32)

e1.44.eff = exp1.44.eff %>% mutate(prr = ifelse(pen==0,0,ifelse(pen%in%c(-1,-3),1,5)))
null.1.44 = lmer(data=e1.44.eff,formula=eff ~ (1 + prr|sub))
prr.1.44 = lmer(data=e1.44.eff,formula=eff ~ prr + (1 + prr|sub))
prr.1.44.eff = anova(prr.1.44,null.1.44)


e1.eff = efficiency %>% filter(exp==1) %>% mutate(prr = ifelse(pen==0,0,ifelse(pen%in%c(-1,-3),1,5)),
                                                     spread = factor(penX))
null.1 = lmer(data=e1.eff,formula=eff ~ spread + (1 + prr + spread|sub))
prr.1 = lmer(data=e1.eff,formula=eff ~ spread + prr + (1 + prr + spread|sub))
prr.1.eff = anova(prr.1,null.1)


