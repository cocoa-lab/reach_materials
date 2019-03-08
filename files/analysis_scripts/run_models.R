# follow-up exploration of data
library(dplyr)
library(ggplot2)
library(data.table)
library(ggforce)
library(lme4)
library(nlme)
library(plotrix)
library(mvtnorm)

dist2D = function(x1, y1, x2, y2) {
  #computes 2D euclidean distance between x,y vectors
  dist = (sqrt((x2 - x1)^2 + (y2 - y1)^2))
  return(dist)
}
pix_to_mm = function(pix) {
  # converts mm to pixels
  mmbp = 0.265   # mm between pixels HP Pavilion 23tm
  mm = mmbp*pix
  return(mm)
}



##################################################################################################################
# Useful Parameters
##################################################################################################################
radius = 32 # radius of the stimulus circles, used to compute score for each sample
nSamples = 100000 # number of simulation samples at each aim-point
xAimPoints = seq(-2,30) # set of x-aim points to be considered
nAim = length(xAimPoints) # number of aim-points to be considered


##################################################################################################################
# Experiment 1 Models
##################################################################################################################
# read in data
exp1.data.full = fread("../exp1/standard_data/all_standard_test_data5.csv")[,-1] %>%
  select(-c(1,2))# get rid of initial junk columns
exp1.data = exp1.data.full %>%
  group_by(subject,penaltyVal,sepDist) %>% # these groupings will be useful when setting up input below
  summarise(lambda=mean(lambda))

# set up input for the models: cov matrices, aim-points, lambdas, etc.
model_input1 = data.table()
for (sub in unique(exp1.data$subject)){
  # get training data
  sub.train.data = fread(
    sprintf("../exp1/standard_data/%s_standard_train1_data.csv",sub))
  sub.data = exp1.data%>%filter(subject==sub)
  # use training data to estimate variance and covariance of the subject's reaching distribution
  varX = var(sub.train.data$standardPokePosX,use="complete.obs")
  varY = var(sub.train.data$standardPokePosY,use="complete.obs")
  covXY = cov(x=sub.train.data$standardPokePosX,y=sub.train.data$standardPokePosY,use="complete.obs")
  lambda = mean(sub.data$lambda) # get the subject's loss-averse parameter
  for (sep in unique(abs(exp1.data$sepDist))){
    for (pen in unique(exp1.data$penaltyVal)){
      rew_val = ifelse(pen %in% c(-3,-15), 3, 1) # specify reward value, depending on penalty value
      mean_rew = (1+3)/2
      mean_pen = mean(unique(exp1.data$penaltyVal))

      model_input1 = rbind(model_input1,
                           data.table(
        # points around which distributions will be generated
        aimX = xAimPoints, aimY = rep(0,nAim),
        # target/penalty locations, used for determining hits vs misses
        targX = rep(0,nAim), targY = rep(0,nAim),
        penX = rep(-sep,nAim), penY = rep(0,nAim),
        # subject specific info
        sub = rep(sub,nAim),varX=rep(varX,nAim),varY=rep(varY,nAim),cov=rep(covXY,nAim),lambda=rep(lambda,nAim),
        # condition specific info
        pen = rep(pen,nAim),rew=rep(rew_val,nAim),mean_rew=rep(mean_rew,nAim),mean_pen=rep(mean_pen,nAim)))
    }
  }
}
# add data for the population variance model
unique_varX = unique(model_input1$varX)
unique_varY = unique(model_input1$varY)
unique_cov = unique(model_input1$cov)
model_input1 = model_input1 %>% mutate(mean_varX = mean(unique_varX),
                                       mean_varY = mean(unique_varY),
                                       mean_cov  = mean(unique_cov))

##################################################################################################################
# Ideal Model
##################################################################################################################
i1evs = c() # for storing expected values of aim-point candidatess
for (i in 1:nrow(model_input1)) {
  # get trial condition
  d = model_input1[i,]; rew = mean(d$rew); pen = mean(d$pen)
  # simulate bivariate normal distribution around the aim-point, with sub's variance estimated from training
  samples = rmvnorm(nSamples,c(d$aimX,d$aimY),sigma=matrix(c(d$varX,d$cov,d$cov,d$varY),ncol=2))
  # sample's distances from centers of circle
  rewDist = dist2D(samples[,1],samples[,2],d$targX,d$targY); penDist = dist2D(samples[,1],samples[,2],d$penX,d$penY)
  # get cummulative score across samples
  score = ifelse(rewDist<radius & penDist<radius,rew+pen, # if in both cirlces, sum pen and rew
                 ifelse(rewDist<radius & penDist>radius,rew, # if only in reward, get reward
                        ifelse(rewDist>radius & penDist<radius, pen, # if only in penalty, get penalty
                               0))) # otherwise get 0
  # compute mean score (expected value)
  ev = mean(score); i1evs = append(i1evs,ev)
}
# get the mean of aim-points within +- 1% of the maxiumum Expected Value
ideal_model_output1 = model_input1%>%mutate(EV=i1evs)%>%
  group_by(sub,pen,penX) %>%
  filter(EV >= max(EV)-abs(max(EV))*.01)%>%
  summarise(aimX=mean(aimX),aimY=mean(aimY),
            targX=mean(targX),targY=mean(targY),
            rew=mean(rew),penY=mean(penY),
            varX=mean(varX),varY=mean(varY),cov=mean(cov),
            mean_varX=mean(mean_varX),mean_varY=mean(mean_varY),mean_cov=mean(mean_cov),
            mean_pen=mean(mean_pen),mean_rew=mean(mean_rew),
            lambda=mean(lambda,na.rm=TRUE),EV=mean(EV)) %>%
  mutate(model="ideal")



##################################################################################################################
# Mean Payoff Model
##################################################################################################################
p1evs = c() # for storing expected values of aim-point candidatess
for (i in 1:nrow(model_input1)) {
  # get trial condition
  d = model_input1[i,]; rew = 1; pen = -1
  # simulate bivariate normal distribution around the aim-point, with sub's variance estimated from training
  samples = rmvnorm(nSamples,c(d$aimX,d$aimY),sigma=matrix(c(d$varX,d$cov,d$cov,d$varY),ncol=2))
  # sample's distances from centers of circle
  rewDist = dist2D(samples[,1],samples[,2],d$targX,d$targY); penDist = dist2D(samples[,1],samples[,2],d$penX,d$penY)
  # get cummulative score across samples
  score = ifelse(rewDist<radius & penDist<radius,rew+pen, # if in both cirlces, sum pen and rew
                 ifelse(rewDist<radius & penDist>radius,rew, # if only in reward, get reward
                        ifelse(rewDist>radius & penDist<radius, pen, # if only in penalty, get penalty
                               0))) # otherwise get 0
  # compute mean score (expected value)
  ev = mean(score); p1evs = append(p1evs,ev)
}
payoff_model_output1 = model_input1%>%mutate(EV=p1evs)%>%
  group_by(sub,pen,penX) %>%
  filter(EV >= max(EV)-abs(max(EV))*.01)%>%
  summarise(aimX=mean(aimX),aimY=mean(aimY),
            targX=mean(targX),targY=mean(targY),
            rew=mean(rew),penY=mean(penY),
            varX=mean(varX),varY=mean(varY),cov=mean(cov),
            mean_varX=mean(mean_varX),mean_varY=mean(mean_varY),mean_cov=mean(mean_cov),
            mean_pen=mean(mean_pen),mean_rew=mean(mean_rew),
            lambda=mean(lambda,na.rm=TRUE),EV=mean(EV)) %>%
  mutate(model="mean_payoff")




##################################################################################################################
# Mean Variability Model
##################################################################################################################
v1evs = c() # for storing expected values of aim-point candidatess
for (i in 1:nrow(model_input1)) {
  # get trial condition
  d = model_input1[i,]; rew = mean(d$rew); pen = mean(d$pen)
  # simulate bivariate normal distribution around the aim-point, with sub's variance estimated from training
  samples = rmvnorm(nSamples,c(d$aimX,d$aimY),sigma=matrix(c(d$mean_varX,d$mean_cov,d$mean_cov,d$mean_varY),ncol=2))
  # sample's distances from centers of circle
  rewDist = dist2D(samples[,1],samples[,2],d$targX,d$targY); penDist = dist2D(samples[,1],samples[,2],d$penX,d$penY)
  # get cummulative score across samples
  score = ifelse(rewDist<radius & penDist<radius,rew+pen, # if in both cirlces, sum pen and rew
                 ifelse(rewDist<radius & penDist>radius,rew, # if only in reward, get reward
                        ifelse(rewDist>radius & penDist<radius, pen, # if only in penalty, get penalty
                               0))) # otherwise get 0
  # compute mean score (expected value)
  ev = mean(score); v1evs = append(v1evs,ev)
}
# get the mean of aim-points within +- 1% of the maxiumum Expected Value
var_model_output1 = model_input1%>%mutate(EV=v1evs)%>%
  group_by(sub,pen,penX) %>%
  filter(EV >= max(EV)-abs(max(EV))*.01)%>%
  summarise(aimX=mean(aimX),aimY=mean(aimY),
            targX=mean(targX),targY=mean(targY),
            rew=mean(rew),penY=mean(penY),
            varX=mean(varX),varY=mean(varY),cov=mean(cov),
            mean_varX=mean(mean_varX),mean_varY=mean(mean_varY),mean_cov=mean(mean_cov),
            mean_pen=mean(mean_pen),mean_rew=mean(mean_rew),
            lambda=mean(lambda,na.rm=TRUE),EV=mean(EV)) %>%
  mutate(model="mean_var")


##################################################################################################################
# Ignore Variability & Penalty Model
##################################################################################################################
vp1evs = c() # for storing expected values of aim-point candidatess
for (i in 1:nrow(model_input1)) {
  # get trial condition
  d = model_input1[i,]; rew = 1; pen = -1
  # simulate bivariate normal distribution around the aim-point, with sub's variance estimated from training
  samples = rmvnorm(nSamples,c(d$aimX,d$aimY),sigma=matrix(c(d$mean_varX,d$mean_cov,d$mean_cov,d$mean_varY),ncol=2))
  # sample's distances from centers of circle
  rewDist = dist2D(samples[,1],samples[,2],d$targX,d$targY); penDist = dist2D(samples[,1],samples[,2],d$penX,d$penY)
  # get cummulative score across samples
  score = ifelse(rewDist<radius & penDist<radius,rew+pen, # if in both cirlces, sum pen and rew
                 ifelse(rewDist<radius & penDist>radius,rew, # if only in reward, get reward
                        ifelse(rewDist>radius & penDist<radius, pen, # if only in penalty, get penalty
                               0))) # otherwise get 0
  # compute mean score (expected value)
  ev = mean(score); vp1evs = append(vp1evs,ev)
}
# get the mean of aim-points within +- 1% of the maxiumum Expected Value
var_pay_model_output1 = model_input1%>%mutate(EV=vp1evs)%>%
  group_by(sub,pen,penX) %>%
  filter(EV >= max(EV)-abs(max(EV))*.01)%>%
  summarise(aimX=mean(aimX),aimY=mean(aimY),
            targX=mean(targX),targY=mean(targY),
            rew=mean(rew),penY=mean(penY),
            varX=mean(varX),varY=mean(varY),cov=mean(cov),
            mean_varX=mean(mean_varX),mean_varY=mean(mean_varY),mean_cov=mean(mean_cov),
            mean_pen=mean(mean_pen),mean_rew=mean(mean_rew),
            lambda=mean(lambda,na.rm=TRUE),EV=mean(EV)) %>%
  mutate(model="mean_var_pay")



##################################################################################################################
# Loss averse model
##################################################################################################################
l1evs = c() # for storing expected values of aim-point candidates
for (i in 1:nrow(model_input1)) {
  # get trial conditions
  d = model_input1[i,]; rew = mean(d$rew); pen = mean(d$pen)
  # get subject's loss-aversion, letting it be 1 if the estimated paramter is unrealistic
  lambda = ifelse(is.na(mean(d$lambda)) | mean(d$lambda) > -1 | mean(d$lambda) < -20, 1, abs(d$lambda))
  # bivariate normal distribution around the aim-point, with sub's sd estimated from training
  samples = rmvnorm(nSamples,c(d$aimX,d$aimY),sigma=matrix(c(d$varX,d$cov,d$cov,d$varY),ncol=2))
  # sample's distances from centers of circle
  rewDist = dist2D(samples[,1],samples[,2],d$targX,d$targY); penDist = dist2D(samples[,1],samples[,2],d$penX,d$penY)
  # get cummulative score across samples
  score = ifelse(rewDist<radius & penDist<radius,rew+pen*lambda, # if in both cirlces, sum pen*lambda and rew
                 ifelse(rewDist<radius & penDist>radius,rew, # if only in reward, get reward
                        ifelse(rewDist>radius & penDist<radius, pen*lambda, # if only in penalty, get penalty*lambda
                               0))) # otherwise get 0
  # compute mean score (expected value)
  ev = mean(score); l1evs = append(l1evs,ev)
}
# get the mean of aim-points within +- 1% of the maxiumum Expected Value
la_model_output1 = model_input1%>%cbind(EV=l1evs) %>%
  group_by(sub,pen,penX) %>%
  filter(EV >= max(EV)-abs(max(EV))*.01)%>%
  summarise(aimX=mean(aimX),aimY=mean(aimY),
            targX=mean(targX),targY=mean(targY),
            rew=mean(rew),penY=mean(penY),
            varX=mean(varX),varY=mean(varY),cov=mean(cov),
            mean_varX=mean(mean_varX),mean_varY=mean(mean_varY),mean_cov=mean(mean_cov),
            mean_pen=mean(mean_pen),mean_rew=mean(mean_rew),
            lambda=mean(lambda,na.rm=TRUE),EV=mean(EV)) %>%
  mutate(model="loss_averse")

##################################################################################################################
# Heuristic model
##################################################################################################################
heur_model_input1 = la_model_output1 %>%
  mutate(aimY = 0,
         aimX = ifelse(pen==0,0,(penX+radius)+abs(penX)/2), # the heuristic strategy
         model = "heuristic")
h1evs = c() # for storing expected values of aim-point candidates
for (i in 1:nrow(heur_model_input1)) {
  # get trial conditions
  d = heur_model_input1[i,]; rew = mean(d$rew); pen = mean(d$pen)
  # bivariate normal distribution around the aim-point, with sub's sd estimated from training
  samples = rmvnorm(nSamples,c(d$aimX,d$aimY),sigma=matrix(c(d$varX,d$cov,d$cov,d$varY),ncol=2))
  # sample's distances from centers of circle
  rewDist = dist2D(samples[,1],samples[,2],d$targX,d$targY); penDist = dist2D(samples[,1],samples[,2],d$penX,d$penY)
  # get cummulative score across samples
  score = ifelse(rewDist<radius & penDist<radius,rew+pen*lambda, # if in both cirlces, sum pen*lambda and rew
                 ifelse(rewDist<radius & penDist>radius,rew, # if only in reward, get reward
                        ifelse(rewDist>radius & penDist<radius, pen*lambda, # if only in penalty, get penalty*lambda
                               0))) # otherwise get 0
  # compute mean score (expected value)
  ev = mean(score); h1evs = append(h1evs,ev)
}
heur_model_output1 = heur_model_input1 %>% ungroup() %>% mutate(EV = h1evs)

















##################################################################################################################
# Experiment 2 Models
##################################################################################################################

# read in data
exp2.data.full = fread("../standard_data/all_standard_test_data3.csv")[,-1] %>%
  select(-c(1)) # get rid of initial junk columns
exp2.data = exp2.data.full %>%
  group_by(subject,pen_val) %>%
  summarise(lambda=mean(lambda))

# set up input for the models: cov matrices, aim-points, lambdas, etc.
model_input2 = data.table()
for (sub in unique(exp2.data$subject)){
  sub.train.data = fread(sprintf("../standard_data/%s_standard_train_data.csv",sub)) %>%
    filter(block>4) # time limit constant after block
  sub.data = exp2.data%>%filter(subject==sub)
  varX = var(sub.train.data$standardPokePosX,use="complete.obs")
  varY = var(sub.train.data$standardPokePosY,use="complete.obs")
  covXY = cov(x=sub.train.data$standardPokePosX,y=sub.train.data$standardPokePosY,use="complete.obs")
  lambda = mean(sub.data$lambda)
  mean_pen = mean(unique(exp2.data$pen_val))
  for (pen in unique(exp2.data$pen_val)){
    model_input2 = rbind(model_input2,data.table(
      # points around which distributions will be generated
      aimX = xAimPoints,aimY = rep(0,nAim),
      # target/penalty locations, used for determining hits vs misses
      targX = rep(0,nAim), targY = rep(0,nAim),
      penX = rep(-radius,nAim), penY = rep(0,nAim),
      # subject specific info
      sub = rep(sub,nAim),varX=rep(varX,nAim),varY=rep(varY,nAim),cov=rep(covXY,nAim),lambda=rep(lambda,nAim),
      # condition specific info
      pen = rep(pen,nAim),rew=rep(100,nAim),mean_pen=rep(mean_pen,nAim),mean_rew=rep(100,nAim)))
  }
}
# add data for the population variance model
unique_varX = unique(model_input2$varX)
unique_varY = unique(model_input2$varY)
unique_cov = unique(model_input2$cov)
model_input2 = model_input2 %>% mutate(mean_varX = mean(unique_varX),
                                       mean_varY = mean(unique_varY),
                                       mean_cov  = mean(unique_cov))


##################################################################################################################
# Ideal Model
##################################################################################################################
i2evs = c() # for storing expected values of aim-point candidates
for (i in 1:nrow(model_input2)) {
  # get trial condition
  d = model_input2[i,]; rew = mean(d$rew); pen = mean(d$pen)
  # simulate bivariate normal distribution around the aim-point, with sub's sd estimated from training
  samples = rmvnorm(nSamples,c(d$aimX,d$aimY),sigma=matrix(c(d$varX,d$cov,d$cov,d$varY),ncol=2))
  # sample's distances from centers of circle
  rewDist = dist2D(samples[,1],samples[,2],d$targX,d$targY); penDist = dist2D(samples[,1],samples[,2],d$penX,d$penY)
  # get cummulative score across samples
  score = ifelse(rewDist<radius & penDist<radius,rew+pen, # if in both cirlces, sum pen and rew
                 ifelse(rewDist<radius & penDist>radius,rew, # if only in reward, get reward
                        ifelse(rewDist>radius & penDist<radius, pen, # if only in penalty, get penalty
                               0))) # otherwise get 0
  # compute mean score (expected value)
  ev = mean(score); i2evs = append(i2evs,ev)
}
ideal_model_output2 = model_input2%>%cbind(EV=i2evs)%>%
  group_by(sub,pen,penX) %>%
  filter(EV >= max(EV)-abs(max(EV))*.01)%>%
  summarise(aimX=mean(aimX),aimY=mean(aimY),
            targX=mean(targX),targY=mean(targY),
            rew=mean(rew),penY=mean(penY),
            varX=mean(varX),varY=mean(varY),cov=mean(cov),
            mean_varX=mean(mean_varX),mean_varY=mean(mean_varY),mean_cov=mean(mean_cov),
            mean_pen=mean(mean_pen),mean_rew=mean(mean_rew),
            lambda=mean(lambda,na.rm=TRUE),EV=mean(EV)) %>%
  mutate(model="ideal")

##################################################################################################################
# Mean Payoff Model
##################################################################################################################
p2evs = c() # for storing expected values of aim-point candidatess
for (i in 1:nrow(model_input2)) {
  # get trial condition
  d = model_input1[i,]; rew = 100; pen = -100
  # simulate bivariate normal distribution around the aim-point, with sub's variance estimated from training
  samples = rmvnorm(nSamples,c(d$aimX,d$aimY),sigma=matrix(c(d$varX,d$cov,d$cov,d$varY),ncol=2))
  # sample's distances from centers of circle
  rewDist = dist2D(samples[,1],samples[,2],d$targX,d$targY); penDist = dist2D(samples[,1],samples[,2],d$penX,d$penY)
  # get cummulative score across samples
  score = ifelse(rewDist<radius & penDist<radius,rew+pen, # if in both cirlces, sum pen and rew
                 ifelse(rewDist<radius & penDist>radius,rew, # if only in reward, get reward
                        ifelse(rewDist>radius & penDist<radius, pen, # if only in penalty, get penalty
                               0))) # otherwise get 0
  # compute mean score (expected value)
  ev = mean(score); p2evs = append(p2evs,ev)
}
payoff_model_output2 = model_input2%>%mutate(EV=p2evs)%>%
  group_by(sub,pen,penX) %>%
  filter(EV >= max(EV)-abs(max(EV))*.01)%>%
  summarise(aimX=mean(aimX),aimY=mean(aimY),
            targX=mean(targX),targY=mean(targY),
            rew=mean(rew),penY=mean(penY),
            varX=mean(varX),varY=mean(varY),cov=mean(cov),
            mean_varX=mean(mean_varX),mean_varY=mean(mean_varY),mean_cov=mean(mean_cov),
            mean_pen=mean(mean_pen),mean_rew=mean(mean_rew),
            lambda=mean(lambda,na.rm=TRUE),EV=mean(EV)) %>%
  mutate(model="mean_payoff")




##################################################################################################################
# Mean Variability Model
##################################################################################################################
v2evs = c() # for storing expected values of aim-point candidatess
for (i in 1:nrow(model_input2)) {
  # get trial condition
  d = model_input1[i,]; rew = mean(d$rew); pen = mean(d$pen)
  # simulate bivariate normal distribution around the aim-point, with sub's variance estimated from training
  samples = rmvnorm(nSamples,c(d$aimX,d$aimY),sigma=matrix(c(d$mean_varX,d$mean_cov,d$mean_cov,d$mean_varY),ncol=2))
  # sample's distances from centers of circle
  rewDist = dist2D(samples[,1],samples[,2],d$targX,d$targY); penDist = dist2D(samples[,1],samples[,2],d$penX,d$penY)
  # get cummulative score across samples
  score = ifelse(rewDist<radius & penDist<radius,rew+pen, # if in both cirlces, sum pen and rew
                 ifelse(rewDist<radius & penDist>radius,rew, # if only in reward, get reward
                        ifelse(rewDist>radius & penDist<radius, pen, # if only in penalty, get penalty
                               0))) # otherwise get 0
  # compute mean score (expected value)
  ev = mean(score); v2evs = append(v2evs,ev)
}
# get the mean of aim-points within +- 1% of the maxiumum Expected Value
var_model_output2 = model_input2%>%mutate(EV=v2evs)%>%
  group_by(sub,pen,penX) %>%
  filter(EV >= max(EV)-abs(max(EV))*.01)%>%
  summarise(aimX=mean(aimX),aimY=mean(aimY),
            targX=mean(targX),targY=mean(targY),
            rew=mean(rew),penY=mean(penY),
            varX=mean(varX),varY=mean(varY),cov=mean(cov),
            mean_varX=mean(mean_varX),mean_varY=mean(mean_varY),mean_cov=mean(mean_cov),
            mean_pen=mean(mean_pen),mean_rew=mean(mean_rew),
            lambda=mean(lambda,na.rm=TRUE),EV=mean(EV)) %>%
  mutate(model="mean_var")


##################################################################################################################
# Ignore Variability + Payoff Model
##################################################################################################################
vp2evs = c() # for storing expected values of aim-point candidatess
for (i in 1:nrow(model_input2)) {
  # get trial condition
  d = model_input1[i,]; rew = 100; pen = -100
  # simulate bivariate normal distribution around the aim-point, with sub's variance estimated from training
  samples = rmvnorm(nSamples,c(d$aimX,d$aimY),sigma=matrix(c(d$mean_varX,d$mean_cov,d$mean_cov,d$mean_varY),ncol=2))
  # sample's distances from centers of circle
  rewDist = dist2D(samples[,1],samples[,2],d$targX,d$targY); penDist = dist2D(samples[,1],samples[,2],d$penX,d$penY)
  # get cummulative score across samples
  score = ifelse(rewDist<radius & penDist<radius,rew+pen, # if in both cirlces, sum pen and rew
                 ifelse(rewDist<radius & penDist>radius,rew, # if only in reward, get reward
                        ifelse(rewDist>radius & penDist<radius, pen, # if only in penalty, get penalty
                               0))) # otherwise get 0
  # compute mean score (expected value)
  ev = mean(score); vp2evs = append(vp2evs,ev)
}
# get the mean of aim-points within +- 1% of the maxiumum Expected Value
var_pay_model_output2 = model_input2%>%mutate(EV=vp2evs)%>%
  group_by(sub,pen,penX) %>%
  filter(EV >= max(EV)-abs(max(EV))*.01)%>%
  summarise(aimX=mean(aimX),aimY=mean(aimY),
            targX=mean(targX),targY=mean(targY),
            rew=mean(rew),penY=mean(penY),
            varX=mean(varX),varY=mean(varY),cov=mean(cov),
            mean_varX=mean(mean_varX),mean_varY=mean(mean_varY),mean_cov=mean(mean_cov),
            mean_pen=mean(mean_pen),mean_rew=mean(mean_rew),
            lambda=mean(lambda,na.rm=TRUE),EV=mean(EV)) %>%
  mutate(model="mean_var_pay")



##################################################################################################################
# Loss-averse model
##################################################################################################################
l2evs = c() # for storing expected values of aim-point candidates
for (i in 1:nrow(model_input2)) {
  # get trial conditions
  d = model_input2[i,]; rew = mean(d$rew); pen = mean(d$pen)
  # get subject's loss-aversion, letting it be 1 if the estimated paramter is unrealistic
  lambda = ifelse(is.na(mean(d$lambda)) | mean(d$lambda) > -1 | mean(d$lambda) < -20, 1, abs(d$lambda))
  # bivariate normal distribution around the aim-point, with sub's sd estimated from training
  samples = rmvnorm(nSamples,c(d$aimX,d$aimY),sigma=matrix(c(d$varX,d$cov,d$cov,d$varY),ncol=2))
  # sample's distances from centers of circle
  rewDist = dist2D(samples[,1],samples[,2],d$targX,d$targY); penDist = dist2D(samples[,1],samples[,2],d$penX,d$penY)
  # get cummulative score across samples
  score = ifelse(rewDist<radius & penDist<radius,rew+pen*lambda, # if in both cirlces, sum pen*lambda and rew
                 ifelse(rewDist<radius & penDist>radius,rew, # if only in reward, get reward
                        ifelse(rewDist>radius & penDist<radius, pen*lambda, # if only in penalty, get penalty*lambda
                               0))) # otherwise get 0
  # compute mean score (expected value)
  ev = mean(score); l2evs = append(l2evs,ev)
}
la_model_output2 = model_input2%>%cbind(EV=l2evs)%>%
  group_by(sub,pen,penX) %>%
  filter(EV >= max(EV)-abs(max(EV))*.01)%>%
  summarise(aimX=mean(aimX),aimY=mean(aimY),
            targX=mean(targX),targY=mean(targY),
            penY=mean(penY),varX=mean(varX),varY=mean(varY),
            cov=mean(cov),lambda=mean(lambda,na.rm=TRUE),rew=mean(rew),
            EV=mean(EV)) %>%
  mutate(model="loss_averse")



##################################################################################################################
# Heuristic model
##################################################################################################################
heur_model_input2 = la_model_output2 %>%
  mutate(aimY = 0,
         aimX = ifelse(pen==0,0,(penX+radius)+abs(penX)/2), # the heuristic strategy
         model = "heuristic")
h2evs = c() # for storing expected values of aim-point candidates
for (i in 1:nrow(heur_model_input2)) {
  # get trial conditions
  d = heur_model_input2[i,]; rew = mean(d$rew); pen = mean(d$pen)
  # bivariate normal distribution around the aim-point, with sub's sd estimated from training
  samples = rmvnorm(nSamples,c(d$aimX,d$aimY),sigma=matrix(c(d$varX,d$cov,d$cov,d$varY),ncol=2))
  # sample's distances from centers of circle
  rewDist = dist2D(samples[,1],samples[,2],d$targX,d$targY); penDist = dist2D(samples[,1],samples[,2],d$penX,d$penY)
  # get cummulative score across samples
  score = ifelse(rewDist<radius & penDist<radius,rew+pen*lambda, # if in both cirlces, sum pen*lambda and rew
                 ifelse(rewDist<radius & penDist>radius,rew, # if only in reward, get reward
                        ifelse(rewDist>radius & penDist<radius, pen*lambda, # if only in penalty, get penalty*lambda
                               0))) # otherwise get 0
  # compute mean score (expected value)
  ev = mean(score); h2evs = append(h2evs,ev)
}
heur_model_output2 = heur_model_input2 %>% ungroup() %>% mutate(EV = h2evs)


##################################################################################################################
# Data Compilation
##################################################################################################################
# Experiment 1: compile model predictions and attach to observed data
model_outputs1 = ideal_model_output1 %>%
  bind_rows(payoff_model_output1) %>%
  bind_rows(var_model_output1) %>%
  bind_rows(la_model_output1) %>%
  bind_rows(var_pay_model_output1) %>%
  bind_rows(heur_model_output1)
exp1.fit_input = exp1.data.full %>%
  mutate(pen=penaltyVal,sub=subject,penX=sepDist,
         reachX=standardPokePosX,reachY=standardPokePosY) %>%
  dplyr::select(c(reachX,reachY,pen,sub,penX)) %>%
  group_by(sub,penX,pen) %>%
  inner_join(model_outputs1) %>%
  mutate(exp=1)

# Experiment 2: compile model predictions and attach to observed data
model_outputs2 =  ideal_model_output2 %>%
  bind_rows(payoff_model_output2) %>%
  bind_rows(var_model_output2) %>%
  bind_rows(la_model_output2) %>%
  bind_rows(var_pay_model_output2) %>%
  bind_rows(heur_model_output2)
exp2.fit_input = exp2.data.full %>%
  mutate(pen=pen_val,sub=subject,penX=-radius,
         reachX=standardPokePosX,reachY=standardPokePosY) %>%
  dplyr::select(c(reachX,reachY,pen,sub,penX)) %>%
  group_by(sub,penX,pen) %>%
  inner_join(model_outputs2) %>%
  mutate(exp=2)

# combine both experiments modeling data
all_model_fit_input = exp1.fit_input %>% bind_rows(exp2.fit_input)
write.csv(all_model_fit_input,"model_fit_input.csv")
