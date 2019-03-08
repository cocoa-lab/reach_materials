################################################################################
# Just a file full of functions that are used in this analysis
################################################################################



################################################################################
# ALL-PURPOSE
################################################################################
pix_to_mm = function(N) {
  #mm between pixels HP Pavilion 23tm
  MMBP = 0.265

  return(MMBP*N)
}
LeveneGrade = function(lev.test.p) {
  lev.alpha = 0.05
  return(ifelse(lev.test.p < lev.alpha, "Fail", "Pass"))
}
summarySE = function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    ## Summarizes data.
    ## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
    ##   data: a data frame.
    ##   measurevar: the name of a column that contains the variable to be summariezed
    ##   groupvars: a vector containing names of columns that contain grouping variables
    ##   na.rm: a boolean that indicates whether to ignore NA's
    ##   conf.interval: the percent range of the confidence interval (default is 95%)
    library(plyr)
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                       c(N    = length2(xx[[col]], na.rm=na.rm),
                         mean = mean   (xx[[col]], na.rm=na.rm),
                         sd   = sd     (xx[[col]], na.rm=na.rm)
                       )
                   },
                   measurevar
    )
    # Rename the "mean" column
    datac <- rename(datac, c("mean" = measurevar))
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    return(datac)
}




get_penalties = function(pen_rew_ratio, experiment) {
# returns a list of penalty magnitudes corresponding to
# a given penalty-reward ratio (0, 1, or 5) and experiment (3 or 5)
    if (pen_rew_ratio == 1){
        if (experiment == 5) {
            penalties = c(1, 3)
        } else {
            penalties = c(100)
        }
    } else if (pen_rew_ratio == 5) {
        if (experiment == 5) {
            penalties = c(5, 15)
        } else {
            penalties = c(500)
        }
    } else {
        penalties = c(0)
    }

    return(penalties)
}


get_pen_col = function(experiment) {
    # returns the correct penalty column name in test output for the current experiment
    if (experiment == 5) {
        pen_col = "penaltyVal"
    } else {
        pen_col = "pen_val"
    }
    return(pen_col)
}
get_exp_dir = function(experiment) {
    # returns the correct subdirectory for the current experiment
    if (experiment == 5) {
        dir = "/pre_replicat_code"
    } else {
        dir = ""
    }
    return(dir)
}
get_point_col = function(experiment) {
    # returns the correct points column name for the current experiment
    if (experiment == 5) {
        pcol = "score"
    } else {
        pcol = "points"
    }
    return(pcol)
}

drop_mistimed = function(data_frame, experiment) {
    # returns the input dataframe sans the mistimed reaches
    if (experiment == 5) {
        data_frame = data_frame[data_frame['pokeTime'] < 1,]
    } else {
        data_frame = data_frame[which(data_frame$too_slow == 0 &
                                      data_frame$too_soon == 0),]
    }
    return(data_frame)
}

################################################################################
# FOR COMPUTING EXPECTED VALUE LANDSCAPES
################################################################################

dist2D = function(x1, y1, x2, y2) {
    #computes distance between pairs of vectors of equal length
    dist = (sqrt((x2 - x1)^2 + (y2 - y1)^2))
    return(dist)
}
insertRow = function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] = existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] = newrow
  return(existingDF)
}
isPen = function(pen_dist, radius) {
    #given vector of distances from the penalty center
    #decides whether point represented by this distance is within the penalty circle
    #returns vector of these decisions
    pen_poke = pen_dist < radius
    return(pen_poke)
}
isTarg = function(targ_dist, radius) {
    #given vector of distances from the target center
    #decides whether point represented by this distance is within the target circle
    #returns vector of these decisions
    targ_poke = targ_dist < radius
    return(targ_poke)
}
targAndPen = function(targ_poke, pen_poke) {
    #given vectors of decisions about whether a point is inside the target and penalty circles
    #decides whether the point is inside both circles
    #returns vector of these decisions
    targ_pen_poke = targ_poke & pen_poke
}
getExpectedValue = function(aim_point, num_pokes, poke_var, pen_val, rew_val) {
    #given an (x,y) aim point, N number of pokes, poke variability, penalty value, and separation distance
    #simulates N pokes around aimpoint and computes and returns the mean earnings (i.e., expected value)
    pen = -pen_val
    RADIUS = rep(pix_to_mm(32), num_pokes)
    #generate gaussian distribution around the grid point
    x_sample   = rnorm(num_pokes, mean = aim_point[1], sd = poke_var)
    y_sample   = rnorm(num_pokes, mean = aim_point[2], sd = poke_var)
    pen_x      = rep(pix_to_mm(-32), num_pokes)
    targ_x     = rep(0, num_pokes)
    targ_y     = rep(0, num_pokes)
    pen_dists  = dist2D(x_sample, y_sample, pen_x, targ_y)
    targ_dists = dist2D(x_sample, y_sample, targ_x, targ_y)
    pen_pokes  = isPen(pen_dists, RADIUS)
    targ_pokes = isTarg(targ_dists, RADIUS)
    dual_pokes = targAndPen(targ_pokes, pen_pokes)
    #earnings for each sim poke: dual_poke -> pen+rew; pen_poke -> pen; targ_poke -> rew
    earnings   = ifelse(dual_pokes == TRUE, pen+rew_val, ifelse(pen_pokes == TRUE, pen, ifelse(targ_pokes == TRUE, rew_val, 0)))
    return(mean(earnings))
}
writeGraphData = function(exp_vals, pen_val, lambda, std_train, output_folder){
    #given ordered vector of expected values, penalty value, and sep dist
    #creates dataframe of aim points and their expected values (for specific pen and sep)
    #saves this dataframe as csv for future input into EV landscape graph functions
    graph_x          = as.numeric(XY_GRID[, 1])
    graph_y          = as.numeric(XY_GRID[, 2])
    mod_pen_rep      = rep((pen_val*lambda), length(graph_x))
    std_rep          = rep((std_train), length(graph_x))
    graph.data       = data.frame(x_aim = graph_x,  y_aim = graph_y, ev = exp_vals, mod_pen = mod_pen_rep, std = std_rep)
    pen_val          = abs(pen_val)
    pen_string       = toString(pen_val)
    graph_data_file  = paste(output_folder,'_', pen_string, GD_SUFFIX, sep = "", collapse = "")
    write.table(graph.data, file = graph_data_file, row.names=FALSE, sep = ",")
}
getReward = function(experiment, penalty) {
    if (experiment == 5) {
        if (penalty %in% c(0, 1, 5)) {
            reward = 1
        } else {
            reward = 3
        }
    } else {
        reward = 100
    }
    return(reward)
}






################################################################################
# FOR COMPUTING FITNESS
################################################################################

GetModSubFit = function(sub_fit_input, model) {
  # ln( P(D|H,V) ), where
  # D is a vector of normalized x end-points,
  # H is a vector of x end-points predicted by some model, and
  # V is the sigma that was used in the model's *_model.R file
  # equation and R-code from McElreath, Statistical Rethinking, p. 183
  sub_fit_input = sub_fit_input[Model == model,]
  
  log_likelihoods = dnorm(sub_fit_input$NormX,
                          mean = sub_fit_input$PredictedX,
                          sd   = sub_fit_input$Sigma,
                          log  = TRUE)
  
  # add the log-likelihoods together
  sum_log_lik = sum(log_likelihoods, na.rm = TRUE)
  
  return(sum_log_lik)
}

  
  
get_log_lik = function(sub_fit_input) {
    # ln( P(D|H,V) ), where
    # D is a vector of normalized x end-points,
    # H is a vector of x end-points predicted by some model, and
    # V is the sigma that was used in the model's *_model.R file
    # equation and R-code from McElreath, Statistical Rethinking, p. 183
    log_likelihoods = dnorm(sub_fit_input$NormX,
                            mean = sub_fit_input$PredictedX,
                            sd   = sub_fit_input$Sigma,
                            log  = TRUE)

    # add the log-likelihoods together
    sum_log_lik = sum(log_likelihoods, na.rm = TRUE)

    return(sum_log_lik)
}

AICc = function(ll, k, n) {
    # computes AICc, which is AIC corrected for finite sample sizes

    dev = -2*ll # deviance term, which is -2 * the log likelihood
    ppt = 2*k # parameter penalty term
    bat = (ppt * k * (k + 1))/(n - k - 1) # bias adjustment term

    # aic is the deviance plus bias adjustment and parameter penalty terms
    aic = dev + ppt + bat
    return(aic)
}
VuongTest     = function(ll1, ll2, exp, models, pen) {
  # computes the vuong statistic to test for
  # a significant difference between two models
  # in terms of how well they fit some observed dataset
  #
  # ll1 is a vector of logliklihoods for some model
  # ll2 is a vector of logliklihoods for another model
  # exp is the current experiment from which observations were taken
  # models is a tag specifiying which models are being compared
  
  # differences of log probabiltiies of the two models at every observed data-point
  ll.ratios = ll1 - ll2
  
  llr.mean  = mean(ll.ratios, na.rm = TRUE) # mean of log-likelihood differences
  llr.s     = sd(ll.ratios, na.rm = TRUE)   # standard deviation of log-likelihood differences
  # vuong statistic, from voung.test() documentation
  vuong = sqrt(length(ll1)) * llr.mean / llr.s
  # (sqrt(length(y)) * ((1/length(y) * sum(ll.ratios, na.rm = T)))) / 
  # (sqrt(1/length(y)) * (sum((ll.ratios - llr.mean)^2, na.rm = T)))
  p1 = pnorm(vuong) 
  if (p1 < 0.5) {
    p2 = 2*p1
  } else {
    p2 = 2*(1-p1)}
  
  l = data.table(experiment = exp, vuong.stat = vuong, p.1 = 1 - p1, p.2 = p2,
                 l.mean = llr.mean, l.s = llr.s, comparison = models, penalty = pen)
  return(l)
}
GetCondLabels = function(penalties) {
  if (length(penalties) >= 3) {
    conditions = c(rep("-1, +1 $", 3), rep("-3, +3 $",3), rep("-5, +1 $",3), rep("-15, +3 $",3))
  } else {
    conditions = c(rep("-100, +100 points", 3), rep("-500, +100 points",3))
  }
  return(conditions)
}





################################################################################
# FOR VISUALIZING
################################################################################

library(ggplot2); library(scales); library(ggforce);
library(grid); library(RColorBrewer)



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
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
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
fte_theme <- function() {

  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]

  # Begin construction of chart
  theme_bw(base_size=9) +

    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +

    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +

    # Format the legend, but hide by default
    #theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background, color = color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +

    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25, hjust = 0.5)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
