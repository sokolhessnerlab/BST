
# --- Trust Only Base Level Analysis Script --- #


#NOTE: Run this script while connected to the shlab drive,
# script's data sources are under /Volumes
# and set working directory via shlab Github
#setwd("/Users/shlab/Documents/GitHub/bst/") #desktop
#setwd("~/Documents/GitHub/bst") #laptop


#set up config: data & script sources
config <- config::get()
setup_path <- file.path(config$path$code$r_scripts, config$code_files$setup_data)
source(setup_path)  #tells R from where to grab data files

#load packages
library(ggplot2)
library(lme4)
library(nlme)
library(lmerTest) # adds more useful info to the output of lmer's

#converts scientific notation to decimal
options(scipen=999)

# TRUST RATINGS ################################################################################


# Trust Rating Task Order Effects
#Q: What are the effects of task order on peoples' trust perceptions? 
# (collapsed)
mean(aggregate(rating ~ subjectID, data = trustRating, FUN=mean)$rating) #Mean Trust Rating = 4.703949
mean(aggregate(rating ~ subjectID, data = trustRating, FUN=sd)$rating)  #SD Trust Rating = 1.50436

by(data = trustRating$rating, INDICES = trustRating$taskOrder, FUN = mean, na.rm = T)
by(data = trustRating$rating, INDICES = trustRating$taskOrder, FUN = sd, na.rm = T)
# On average, people rate people as more trustworthy when the CPT (stressor) is given on day 2)
# Task Order 1: 4.50(1.84), Task Order 2: 4.81(1.86)

#(subject-level)
tmpvectR = aggregate(rating ~ subjectID + taskOrder, data = trustRating, FUN=mean);
by(data = tmpvectR$rating, INDICES = tmpvectR$taskOrder, FUN = mean) 
#A: Participants are rating people as more trustworthy when task order is 1 vs. 2
# Task Order 1: 4.69, Task Order 2: 4.85


# Trust Rating Day Effects
#Q: What are the effects of day on peoples' trust perceptions? 
tmpvectR2 = aggregate(rating ~ subjectID + day, data = trustRating, FUN=mean);
by(data = tmpvectR2$rating, INDICES = tmpvectR2$day, FUN = mean) 
#A: Participants' trust perceptions decrease from day 1 to day 2.
# Day 1: 4.73, Day 2: 4.68


# Trust Rating Stress Effects
#Q: What are the effects of stress on peoples' trust perceptions? 
tmpvectR3 = aggregate(rating ~ subjectID + stressedBool, data = trustRating, FUN=mean);
# Trust Rating means within condition
by(data = tmpvectR3$rating, INDICES = tmpvectR3$stressedBool, FUN = mean) 
#A: Acute stress seems to decrease peoples' trust perceptions.
# control = 4.74, stressed = 4.67


## TR: Averages & Variances in Ratings & RTs #######################################################
### Subject-Level Loop ##########################################################################
tr_sub_level_colnames = c(
  'tr_mean_rating',
  'tr_mean_ratingW',
  'tr_mean_ratingB',
  'tr_mean_ratingO',
  'tr_var_rating',
  'tr_var_ratingW',
  'tr_var_ratingB',
  'tr_var_ratingO',
  'tr_rt_rating',
  'tr_rt_ratingW',
  'tr_rt_ratingB',
  'tr_rt_ratingO',
  'tr_rt_var_rating',
  'tr_rt_var_ratingW',
  'tr_rt_var_ratingB',
  'tr_rt_var_ratingO'
)

tr_sub_level = array(data = NA, dim = c(number_of_subjects,length(tr_sub_level_colnames)))
tr_sub_level = as.data.frame(tr_sub_level)
colnames(tr_sub_level) <- tr_sub_level_colnames

for (s in 1:number_of_subjects){
  sub_ind = trustRating$subjectID == subjectIDs[s];
  tmp_data = trustRating[sub_ind,];
  
  tr_sub_level$tr_mean_rating[s] = mean(tmp_data$rating, na.rm = T)
  tr_sub_level$tr_mean_ratingW[s] = mean(tmp_data$rating[tmp_data$partnerRace_0w_1b_2o == 0], na.rm = T)
  tr_sub_level$tr_mean_ratingB[s] = mean(tmp_data$rating[tmp_data$partnerRace_0w_1b_2o == 1], na.rm = T)
  tr_sub_level$tr_mean_ratingO[s] = mean(tmp_data$rating[tmp_data$partnerRace_0w_1b_2o == 2], na.rm = T)
  
  tr_sub_level$tr_var_rating[s] = var(tmp_data$rating, na.rm = T)
  tr_sub_level$tr_var_ratingW[s] = var(tmp_data$rating[tmp_data$partnerRace_0w_1b_2o == 0], na.rm = T)
  tr_sub_level$tr_var_ratingB[s] = var(tmp_data$rating[tmp_data$partnerRace_0w_1b_2o == 1], na.rm = T)
  tr_sub_level$tr_var_ratingO[s] = var(tmp_data$rating[tmp_data$partnerRace_0w_1b_2o == 2], na.rm = T)
  
  tr_sub_level$tr_rt_rating[s] = mean(tmp_data$responseTime, na.rm = T)
  tr_sub_level$tr_rt_ratingW[s] = mean(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 0], na.rm = T)
  tr_sub_level$tr_rt_ratingB[s] = mean(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 1], na.rm = T)
  tr_sub_level$tr_rt_ratingO[s] = mean(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 2], na.rm = T)
  
  tr_sub_level$tr_rt_var_rating[s] = var(tmp_data$responseTime, na.rm = T)
  tr_sub_level$tr_rt_var_ratingW[s] = var(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 0], na.rm = T)
  tr_sub_level$tr_rt_var_ratingB[s] = var(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 1], na.rm = T)
  tr_sub_level$tr_rt_var_ratingO[s] = var(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 2], na.rm = T)
}


### Mean Rating Visualizations & Analysis ############################################################

# Visualize Distributions of Mean Ratings
mean_rating_hist = hist(tr_sub_level$tr_mean_rating, breaks = seq(from = 0, to = 8, by = 1), plot = F);
mean_ratingW_hist = hist(tr_sub_level$tr_mean_ratingW, breaks = seq(from = 0, to =8, by = 1), plot = F)
mean_ratingB_hist = hist(tr_sub_level$tr_mean_ratingB, breaks = seq(from = 0, to = 8, by = 1), plot = F)
mean_ratingO_hist = hist(tr_sub_level$tr_mean_ratingO, breaks = seq(from = 0, to = 8, by = 1), plot = F)

plot(mean_rating_hist$mids, mean_rating_hist$density, col = rgb(0,0,0), type = 'l', lwd = 3, xlim = c(0,8), ylim = c(0,.5), 
     xlab = 'Mean Trust Rating', ylab = 'Frequency')
lines(mean_ratingW_hist$mids, mean_ratingW_hist$density, col = rgb(1,0,0), lwd = 3)
lines(mean_ratingB_hist$mids, mean_ratingB_hist$density, col = rgb(0,1,0), lwd = 3)
lines(mean_ratingO_hist$mids, mean_ratingO_hist$density, col = rgb(0,0,1), lwd = 3)
legend(x = 7, y = .4, c('Overall','White','Black','Other'), lwd = 2, col = c('black','red','green','blue'))

#t-tests for Mean Ratings
t.test(tr_sub_level$tr_mean_ratingW, tr_sub_level$tr_mean_ratingB, paired = T) # p = 0.0001  B > W
t.test(tr_sub_level$tr_mean_ratingW, tr_sub_level$tr_mean_ratingO, paired = T) # p = 0.0000  O > W
t.test(tr_sub_level$tr_mean_ratingB, tr_sub_level$tr_mean_ratingO, paired = T) # p = 0.0743  O = B
# Mean trust ratings across participants show B > W & O > W, but B = O.

par(mfrow = c(1,3)) # Returning graphs to plot 1 at a time
plot(tr_sub_level$tr_mean_ratingW, tr_sub_level$tr_mean_ratingB, bg = rgb(.6, .3, 0, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Black', main = 'Mean Rating', xlim = c(0,8), ylim = c(0,8))
abline(a = 0, b = 1, col = 'black')
points(x = mean(tr_sub_level$tr_mean_ratingW), y = mean(tr_sub_level$tr_mean_ratingB), pch = 18, cex = 6)
plot(tr_sub_level$tr_mean_ratingW, tr_sub_level$tr_mean_ratingO, bg = rgb(.6, .13, .94, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Other', main = 'Mean Rating', xlim = c(0,8), ylim = c(0,8))
abline(a = 0, b = 1, col = 'black')
points(x = mean(tr_sub_level$tr_mean_ratingW), y = mean(tr_sub_level$tr_mean_ratingO), pch = 18, cex = 6)
plot(tr_sub_level$tr_mean_ratingB, tr_sub_level$tr_mean_ratingO, bg = rgb(.5, 1, .83, .5), pch = 21, cex = 4,
     xlab = 'Black', ylab = 'Other', main = 'Mean Rating', xlim = c(0,8), ylim = c(0,8))
abline(a = 0, b = 1, col = 'black')
points(x = mean(tr_sub_level$tr_mean_ratingB), y = mean(tr_sub_level$tr_mean_ratingO), pch = 18, cex = 6)
par(mfrow = c(1,1)) # Returning graphs to plot 1 at a time


### Response Time Visualizations & Analysis ###########################################################

# Reaction Times (decision speed)

rt_rating_hist = hist(tr_sub_level$tr_rt_rating, breaks = seq(from = 0, to = 10, length.out = 12), plot = F);
rt_ratingW_hist = hist(tr_sub_level$tr_rt_ratingW, breaks = seq(from = 0, to = 10, length.out = 12), plot = F)
rt_ratingB_hist = hist(tr_sub_level$tr_rt_ratingB, breaks = seq(from = 0, to = 10, length.out = 12), plot = F)
rt_ratingO_hist = hist(tr_sub_level$tr_rt_ratingO, breaks = seq(from = 0, to = 10, length.out = 12), plot = F)


plot(rt_rating_hist$mids, rt_rating_hist$density, col = rgb(0,0,0), type = 'l', lwd = 3, xlim = c(0,10), ylim = c(0,.5), 
     xlab = 'Response Time (seconds)', ylab = 'Frequency')
lines(rt_ratingW_hist$mids, rt_ratingW_hist$density, col = rgb(1,0,0), lwd = 3)
lines(rt_ratingB_hist$mids, rt_ratingB_hist$density, col = rgb(0,1,0), lwd = 3)
lines(rt_ratingO_hist$mids, rt_ratingO_hist$density, col = rgb(0,0,1), lwd = 3)
points(x = mean(tr_sub_level$tr_rt_rating), y = 0, lwd = 4, col = 'black')
points(x = mean(tr_sub_level$tr_rt_ratingW), y = 0.025, lwd = 4, col = 'red')
points(x = mean(tr_sub_level$tr_rt_ratingB), y = 0.05, lwd = 4, col = 'green')
points(x = mean(tr_sub_level$tr_rt_ratingO), y = 0.075, lwd = 4, col = 'blue')
legend(x = 8.25, y = .375, c('Overall','White','Black','Other'), lwd = 2, col = c('black','red','green','blue'))


#t-tests for Mean RTs (race comparisons)
t.test(tr_sub_level$tr_rt_ratingW, tr_sub_level$tr_rt_ratingB, paired = T) # p = 0.06   B = W
t.test(tr_sub_level$tr_rt_ratingW, tr_sub_level$tr_rt_ratingO, paired = T) # p = 0.69   O = W
t.test(tr_sub_level$tr_rt_ratingB, tr_sub_level$tr_rt_ratingO, paired = T) # p = 0.07   B = O


par(mfrow = c(1,3)) # Returning graphs to plot 1 at a time
plot(tr_sub_level$tr_rt_ratingW, tr_sub_level$tr_rt_ratingB, bg = rgb(.6, .3, 0, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Black', main = 'Response Time for Ratings', xlim = c(0,10), ylim = c(0,10))
abline(a = 0, b = 1, col = 'black')
points(x = mean(tr_sub_level$tr_rt_ratingW), y = mean(tr_sub_level$tr_rt_ratingB), pch = 18, cex = 6)
plot(tr_sub_level$tr_rt_ratingW, tr_sub_level$tr_rt_ratingO, bg = rgb(.6, .13, .94, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Other', main = 'Response Time for Ratings', xlim = c(0,10), ylim = c(0,10))
abline(a = 0, b = 1, col = 'black')
points(x = mean(tr_sub_level$tr_rt_ratingW), y = mean(tr_sub_level$tr_rt_ratingO), pch = 18, cex = 6)
plot(tr_sub_level$tr_rt_ratingB, tr_sub_level$tr_rt_ratingO, bg = rgb(.5, 1, .83, .5), pch = 21, cex = 4,
     xlab = 'Black', ylab = 'Other', main = 'Response Time for Ratings', xlim = c(0,10), ylim = c(0,10))
abline(a = 0, b = 1, col = 'black')
points(x = mean(tr_sub_level$tr_rt_ratingB), y = mean(tr_sub_level$tr_rt_ratingO), pch = 18, cex = 6)
par(mfrow = c(1,1)) # Returning graphs to plot 1 at a time

for (s in 1:number_of_subjects){
  fp = paste0(config$path$code$r_scripts,'/figures/responsetimes_trustRating',sprintf('/tr_rt_BST%03i.pdf',subjectIDs[s]))
  pdf(file=fp)
  hist(trustRating$responseTime[trustRating$subjectID == subjectIDs[s]], 
       main = sprintf('RTs for BST%03i', subjectIDs[s]), xlab = 'response time (seconds)')
  dev.off()
}

t.test(tr_sub_level$tr_rt_var_ratingW, tr_sub_level$tr_rt_var_ratingB, paired = T) # p = 0.55
t.test(tr_sub_level$tr_rt_var_ratingW, tr_sub_level$tr_rt_var_ratingO, paired = T) # p = 0.67
t.test(tr_sub_level$tr_rt_var_ratingB, tr_sub_level$tr_rt_var_ratingO, paired = T) # p = 0.38
# Within-subject variance in RTs during trust ratings are equal


### Variances in Rating Visualizations & Analysis #######################################

# Visualize Variance in Distributions of Mean Ratings
var_rating_hist = hist(tr_sub_level$tr_var_rating, breaks = seq(from = 0, to = 10, by = .1), plot = F);
var_ratingW_hist = hist(tr_sub_level$tr_var_ratingW, breaks = seq(from = 0, to = 10, by = .1), plot = F)
var_ratingB_hist = hist(tr_sub_level$tr_var_ratingB, breaks = seq(from = 0, to = 10, by = .1), plot = F)
var_ratingO_hist = hist(tr_sub_level$tr_var_ratingO, breaks = seq(from = 0, to = 10, by = .1), plot = F)

plot(var_rating_hist$mids, var_rating_hist$density, col = rgb(0,0,0), type = 'l', lwd = 3, xlim = c(0.1,8), ylim = c(0,.8), xlab = 'Variance in Ratings', ylab = 'Frequency')
lines(var_ratingW_hist$mids, var_ratingW_hist$density, col = rgb(1,0,0), lwd = 3)
lines(var_ratingB_hist$mids, var_ratingB_hist$density, col = rgb(0,1,0), lwd = 3)
lines(var_ratingO_hist$mids, var_ratingO_hist$density, col = rgb(0,0,1), lwd = 3)
legend(x = 7, y = .6, c('Overall','White','Black','Other'), lwd = 2, col = c('black','red','green','blue'))

#t-tests for Rating Variances
t.test(tr_sub_level$tr_var_ratingW, tr_sub_level$tr_var_ratingB, paired = T) # p = 0.03   W > B
t.test(tr_sub_level$tr_var_ratingW, tr_sub_level$tr_var_ratingO, paired = T) # p = 0.86   W = O
t.test(tr_sub_level$tr_var_ratingB, tr_sub_level$tr_var_ratingO, paired = T) # p = 0.10   B = O
#A: Participants vary significantly on trust perceptions for white vs. black partners, showing greater variance for white vs. black partners in rating amounts
#A: This difference in variance for partner trust perception is not significant when comparing shares for black vs. other partners or white vs. other partners.

par(mfrow = c(1,3)) # Returning graphs to plot 1 at a time
plot(tr_sub_level$tr_var_ratingW, tr_sub_level$tr_var_ratingB, bg = rgb(.6, .3, 0, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Black', main = 'Variance in Ratings', xlim = c(0,6), ylim = c(0,6))
abline(a = 0, b = 1, col = 'black')
points(x = var(tr_sub_level$tr_var_ratingW), y = var(tr_sub_level$tr_var_ratingB), pch = 18, cex = 6)
plot(tr_sub_level$tr_var_ratingW, tr_sub_level$tr_var_ratingO, bg = rgb(.6, .13, .94, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Other', main = 'Variance in Ratings', xlim = c(0,6), ylim = c(0,6))
abline(a = 0, b = 1, col = 'black')
points(x = var(tr_sub_level$tr_var_ratingW), y = var(tr_sub_level$tr_var_ratingO), pch = 18, cex = 6)
plot(tr_sub_level$tr_var_ratingB, tr_sub_level$tr_var_ratingO, bg = rgb(.5, 1, .83, .5), pch = 21, cex = 4,
     xlab = 'Black', ylab = 'Other', main = 'Variance in Ratings', xlim = c(0,6), ylim = c(0,6))
abline(a = 0, b = 1, col = 'black')
points(x = var(tr_sub_level$tr_var_ratingB), y = var(tr_sub_level$tr_var_ratingO), pch = 18, cex = 6)
par(mfrow = c(1,1)) # Returning graphs to plot 1 at a time

# Trust ratings of White partners are most variable and greater than of black partners but not of other partners.
# Only W>B is significant. 
# Potentially explanations:
#   - ???

# Between-Subject Variances
between_subj_var_rating_W = var(tr_sub_level$tr_mean_ratingW); #1.12
between_subj_var_rating_B = var(tr_sub_level$tr_mean_ratingB); #1.20
between_subj_var_rating_O = var(tr_sub_level$tr_mean_ratingO); #1.40
# numeric difference here (0 > B > W)... 

var.test(tr_sub_level$tr_mean_ratingW, tr_sub_level$tr_mean_ratingB) # p = 0.84   W = B
var.test(tr_sub_level$tr_mean_ratingW, tr_sub_level$tr_mean_ratingO) # p = 0.50   W = O
var.test(tr_sub_level$tr_mean_ratingB, tr_sub_level$tr_mean_ratingO) # p = 0.64   B = O
# ... but no significant differences in variance across subjects in mean ratings.

### Takeaways: Analyses of TR Rating Amounts, RTs, and Variance #####################################



# TRUST GAME ####################################################################################


# Trust Game Task Order Effects
#Q: What are the effects of task order on peoples' trust behaviors? 
# (collapsed)
by(data = trustGame$shared, INDICES = trustGame$taskOrder, FUN = mean, na.rm = T)
by(data = trustGame$shared, INDICES = trustGame$taskOrder, FUN = sd, na.rm = T)
# On average, people share more when the CPT (stressor) is given on day 1)
# Task Order 1: 2.722(1.640), Task Order 2: 2.272(1.477)
#(subject-level)
tmpvect = aggregate(shared ~ subjectID + taskOrder, data = trustGame, FUN=mean);
by(data = tmpvect$shared, INDICES = tmpvect$taskOrder, FUN = mean) # order 1 = 2.60, order 2 = 2.30
#A: Participants are sharing more when task order is 1 vs. 2
# Task Order 1: 2.600, Task Order 2: 2.304

# Trust Game Stress Effects
# Q: Were participants stressed before doing the trust task?


# Q: What are the effects of stress on peoples' trust behaviors? 
tmpvectG2 = aggregate(shared ~ subjectID + stressedBool, data = trustGame, FUN=mean);
# Trust Behavior means within condition
by(data = tmpvectG2$shared, INDICES = tmpvectG2$stressedBool, FUN = mean) 
#A: Acute stress does not seem to affect peoples' trust behaviors at the subject level.
# control = 2.53, stressed = 2.52


## TG: Averages & Variances in Offers & RTs #######################################################
### Subject-Level Loop ##########################################################################
tr_sub_level_colnames = c(
  'tg_mean_shared',
  'tg_mean_sharedW',
  'tg_mean_sharedB',
  'tg_mean_sharedO',
  'tg_var_shared',
  'tg_var_sharedW',
  'tg_var_sharedB',
  'tg_var_sharedO',
  'tg_rt_shared',
  'tg_rt_sharedW',
  'tg_rt_sharedB',
  'tg_rt_sharedO',
  'tg_rt_var_shared',
  'tg_rt_var_sharedW',
  'tg_rt_var_sharedB',
  'tg_rt_var_sharedO'
)

tg_sub_level = array(data = NA, dim = c(number_of_subjects,length(tg_sub_level_colnames)))
tg_sub_level = as.data.frame(tg_sub_level)
colnames(tg_sub_level) <- tg_sub_level_colnames

for (s in 1:number_of_subjects){
  sub_ind = trustGame$subjectID == subjectIDs[s];
  tmp_data = trustGame[sub_ind,];
  
  tg_sub_level$tg_mean_shared[s] = mean(tmp_data$shared, na.rm = T)
  tg_sub_level$tg_mean_sharedW[s] = mean(tmp_data$shared[tmp_data$partnerRace_0w_1b_2o == 0], na.rm = T)
  tg_sub_level$tg_mean_sharedB[s] = mean(tmp_data$shared[tmp_data$partnerRace_0w_1b_2o == 1], na.rm = T)
  tg_sub_level$tg_mean_sharedO[s] = mean(tmp_data$shared[tmp_data$partnerRace_0w_1b_2o == 2], na.rm = T)
  
  tg_sub_level$tg_var_shared[s] = var(tmp_data$shared, na.rm = T)
  tg_sub_level$tg_var_sharedW[s] = var(tmp_data$shared[tmp_data$partnerRace_0w_1b_2o == 0], na.rm = T)
  tg_sub_level$tg_var_sharedB[s] = var(tmp_data$shared[tmp_data$partnerRace_0w_1b_2o == 1], na.rm = T)
  tg_sub_level$tg_var_sharedO[s] = var(tmp_data$shared[tmp_data$partnerRace_0w_1b_2o == 2], na.rm = T)
  
  tg_sub_level$tg_rt_shared[s] = mean(tmp_data$responseTime, na.rm = T)
  tg_sub_level$tg_rt_sharedW[s] = mean(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 0], na.rm = T)
  tg_sub_level$tg_rt_sharedB[s] = mean(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 1], na.rm = T)
  tg_sub_level$tg_rt_sharedO[s] = mean(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 2], na.rm = T)
  
  tg_sub_level$tg_rt_var_shared[s] = var(tmp_data$responseTime, na.rm = T)
  tg_sub_level$tg_rt_var_sharedW[s] = var(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 0], na.rm = T)
  tg_sub_level$tg_rt_var_sharedB[s] = var(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 1], na.rm = T)
  tg_sub_level$tg_rt_var_sharedO[s] = var(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 2], na.rm = T)
}


### Mean Offer Visualizations & Analysis ############################################################

# Visualize Distributions of Mean Offers
mean_shared_hist = hist(tg_sub_level$tg_mean_shared, breaks = seq(from = 0, to = 5, by = 0.5), plot = F);
mean_sharedW_hist = hist(tg_sub_level$tg_mean_sharedW, breaks = seq(from = 0, to = 5, by = 0.5), plot = F)
mean_sharedB_hist = hist(tg_sub_level$tg_mean_sharedB, breaks = seq(from = 0, to = 5, by = 0.5), plot = F)
mean_sharedO_hist = hist(tg_sub_level$tg_mean_sharedO, breaks = seq(from = 0, to = 5, by = 0.5), plot = F)

plot(mean_shared_hist$mids, mean_shared_hist$density, col = rgb(0,0,0), type = 'l', lwd = 3, xlim = c(0,5), ylim = c(0,.7), 
     xlab = 'Mean $ Shared', ylab = 'Frequency')
lines(mean_sharedW_hist$mids, mean_sharedW_hist$density, col = rgb(1,0,0), lwd = 3)
lines(mean_sharedB_hist$mids, mean_sharedB_hist$density, col = rgb(0,1,0), lwd = 3)
lines(mean_sharedO_hist$mids, mean_sharedO_hist$density, col = rgb(0,0,1), lwd = 3)
legend(x = 4, y = .675, c('Overall','White','Black','Other'), lwd = 2, col = c('black','red','green','blue'))

#t-tests for Mean Offers
t.test(tg_sub_level$tg_mean_sharedW, tg_sub_level$tg_mean_sharedB, paired = T) # p = 0.00000000287  B > W
t.test(tg_sub_level$tg_mean_sharedW, tg_sub_level$tg_mean_sharedO, paired = T) # p = 0.000311       O > W
t.test(tg_sub_level$tg_mean_sharedB, tg_sub_level$tg_mean_sharedO, paired = T) # p = 0.00000959     B > O

par(mfrow = c(1,3)) # Returning graphs to plot 1 at a time
plot(tg_sub_level$tg_mean_sharedW, tg_sub_level$tg_mean_sharedB, bg = rgb(.6, .3, 0, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Black', main = 'Mean $ Shared', xlim = c(0,5), ylim = c(0,5))
abline(a = 0, b = 1, col = 'black')
points(x = mean(tg_sub_level$tg_mean_sharedW), y = mean(tg_sub_level$tg_mean_sharedB), pch = 18, cex = 6)
plot(tg_sub_level$tg_mean_sharedW, tg_sub_level$tg_mean_sharedO, bg = rgb(.6, .13, .94, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Other', main = 'Mean $ Shared', xlim = c(0,5), ylim = c(0,5))
abline(a = 0, b = 1, col = 'black')
points(x = mean(tg_sub_level$tg_mean_sharedW), y = mean(tg_sub_level$tg_mean_sharedO), pch = 18, cex = 6)
plot(tg_sub_level$tg_mean_sharedB, tg_sub_level$tg_mean_sharedO, bg = rgb(.5, 1, .83, .5), pch = 21, cex = 4,
     xlab = 'Black', ylab = 'Other', main = 'Mean $ Shared', xlim = c(0,5), ylim = c(0,5))
abline(a = 0, b = 1, col = 'black')
points(x = mean(tg_sub_level$tg_mean_sharedB), y = mean(tg_sub_level$tg_mean_sharedO), pch = 18, cex = 6)
par(mfrow = c(1,1)) # Returning graphs to plot 1 at a time


### Response Time Visualizations & Analysis ###########################################################

# Reaction Times (decision speed)

rt_shared_hist = hist(tg_sub_level$tg_rt_shared, breaks = seq(from = 0, to = 10, length.out = 12), plot = F);
rt_sharedW_hist = hist(tg_sub_level$tg_rt_sharedW, breaks = seq(from = 0, to = 10, length.out = 12), plot = F)
rt_sharedB_hist = hist(tg_sub_level$tg_rt_sharedB, breaks = seq(from = 0, to = 10, length.out = 12), plot = F)
rt_sharedO_hist = hist(tg_sub_level$tg_rt_sharedO, breaks = seq(from = 0, to = 10, length.out = 12), plot = F)


plot(rt_shared_hist$mids, rt_shared_hist$density, col = rgb(0,0,0), type = 'l', lwd = 3, xlim = c(0,10), ylim = c(0,.5), 
     xlab = 'Response Time (seconds)', ylab = 'Frequency')
lines(rt_sharedW_hist$mids, rt_sharedW_hist$density, col = rgb(1,0,0), lwd = 3)
lines(rt_sharedB_hist$mids, rt_sharedB_hist$density, col = rgb(0,1,0), lwd = 3)
lines(rt_sharedO_hist$mids, rt_sharedO_hist$density, col = rgb(0,0,1), lwd = 3)
points(x = mean(tg_sub_level$tg_rt_shared), y = 0, lwd = 4, col = 'black')
points(x = mean(tg_sub_level$tg_rt_sharedW), y = 0.025, lwd = 4, col = 'red')
points(x = mean(tg_sub_level$tg_rt_sharedB), y = 0.05, lwd = 4, col = 'green')
points(x = mean(tg_sub_level$tg_rt_sharedO), y = 0.075, lwd = 4, col = 'blue')
legend(x = 8.25, y = .375, c('Overall','White','Black','Other'), lwd = 2, col = c('black','red','green','blue'))


#t-tests for Mean RTs (race comparisons)
t.test(tg_sub_level$tg_rt_sharedW, tg_sub_level$tg_rt_sharedB, paired = T) # p = 0.20   B = W
t.test(tg_sub_level$tg_rt_sharedW, tg_sub_level$tg_rt_sharedO, paired = T) # p = 0.27   O = W
t.test(tg_sub_level$tg_rt_sharedB, tg_sub_level$tg_rt_sharedO, paired = T) # p = 0.98   B = O


par(mfrow = c(1,3)) # Returning graphs to plot 1 at a time
plot(tg_sub_level$tg_rt_sharedW, tg_sub_level$tg_rt_sharedB, bg = rgb(.6, .3, 0, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Black', main = 'Response Time', xlim = c(0,10), ylim = c(0,10))
abline(a = 0, b = 1, col = 'black')
points(x = mean(tg_sub_level$tg_rt_sharedW), y = mean(tg_sub_level$tg_rt_sharedB), pch = 18, cex = 6)
plot(tg_sub_level$tg_rt_sharedW, tg_sub_level$tg_rt_sharedO, bg = rgb(.6, .13, .94, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Other', main = 'Response Time', xlim = c(0,10), ylim = c(0,10))
abline(a = 0, b = 1, col = 'black')
points(x = mean(tg_sub_level$tg_rt_sharedW), y = mean(tg_sub_level$tg_rt_sharedO), pch = 18, cex = 6)
plot(tg_sub_level$tg_rt_sharedB, tg_sub_level$tg_rt_sharedO, bg = rgb(.5, 1, .83, .5), pch = 21, cex = 4,
     xlab = 'Black', ylab = 'Other', main = 'Response Time', xlim = c(0,10), ylim = c(0,10))
abline(a = 0, b = 1, col = 'black')
points(x = mean(tg_sub_level$tg_rt_sharedB), y = mean(tg_sub_level$tg_rt_sharedO), pch = 18, cex = 6)
par(mfrow = c(1,1)) # Returning graphs to plot 1 at a time

for (s in 1:number_of_subjects){
  fp = paste0(config$path$code$r_scripts,'/figures/responsetimes_trustGame',sprintf('/tg_rt_BST%03i.pdf',subjectIDs[s]))
  pdf(file=fp)
  hist(trustGame$responseTime[trustGame$subjectID == subjectIDs[s]], 
       main = sprintf('RTs for BST%03i', subjectIDs[s]), xlab = 'response time (seconds)')
  dev.off()
}

t.test(tg_sub_level$tg_rt_var_sharedW, tg_sub_level$tg_rt_var_sharedB, paired = T) # p = 0.47
t.test(tg_sub_level$tg_rt_var_sharedW, tg_sub_level$tg_rt_var_sharedO, paired = T) # p = 0.94
t.test(tg_sub_level$tg_rt_var_sharedB, tg_sub_level$tg_rt_var_sharedO, paired = T) # p = 0.49
# Within-subject variance in RTs during offers are equal



### Variances in Offer Decisions Visualizations & Analysis #######################################

# Visualize Variance in Distributions of Mean Offers
var_shared_hist = hist(tg_sub_level$tg_var_shared, breaks = seq(from = 0, to = 7, by = 0.5), plot = F);
var_sharedW_hist = hist(tg_sub_level$tg_var_sharedW, breaks = seq(from = 0, to = 7, by = 0.5), plot = F)
var_sharedB_hist = hist(tg_sub_level$tg_var_sharedB, breaks = seq(from = 0, to = 7, by = 0.5), plot = F)
var_sharedO_hist = hist(tg_sub_level$tg_var_sharedO, breaks = seq(from = 0, to = 7, by = 0.5), plot = F)

plot(var_shared_hist$mids, var_shared_hist$density, col = rgb(0,0,0), type = 'l', lwd = 3, xlim = c(0.1,4.5), ylim = c(0,.65), xlab = 'Variance in Shared', ylab = 'Frequency')
lines(var_sharedW_hist$mids, var_sharedW_hist$density, col = rgb(1,0,0), lwd = 3)
lines(var_sharedB_hist$mids, var_sharedB_hist$density, col = rgb(0,1,0), lwd = 3)
lines(var_sharedO_hist$mids, var_sharedO_hist$density, col = rgb(0,0,1), lwd = 3)
legend(x = 3.7, y = .6, c('Overall','White','Black','Other'), lwd = 2, col = c('black','red','green','blue'))

#t-tests for Offer Variances
t.test(tg_sub_level$tg_var_sharedW, tg_sub_level$tg_var_sharedB, paired = T) # p = 0.33      B = W
t.test(tg_sub_level$tg_var_sharedW, tg_sub_level$tg_var_sharedO, paired = T) # p = 0.025   W > O
t.test(tg_sub_level$tg_var_sharedB, tg_sub_level$tg_var_sharedO, paired = T) # p = 0.18     B = O
#A: Participants vary significantly on trust for white vs. other partners, showing greater variance for other vs. white partners in shared amounts
#A: Interestingly, this difference in variance for partner trust is not significant when comparing shares for black vs. other partners or black vs. white partners.
#RQ: Is there a different model of sharing depending on partner race (black vs. white) and when partner is "other" race, participants are more varied in how they share?

par(mfrow = c(1,3)) # Returning graphs to plot 1 at a time
plot(tg_sub_level$tg_var_sharedW, tg_sub_level$tg_var_sharedB, bg = rgb(.6, .3, 0, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Black', main = 'Variance in Shared $', xlim = c(0,6), ylim = c(0,6))
abline(a = 0, b = 1, col = 'black')
points(x = var(tg_sub_level$tg_var_sharedW), y = var(tg_sub_level$tg_var_sharedB), pch = 18, cex = 6)
plot(tg_sub_level$tg_var_sharedW, tg_sub_level$tg_var_sharedO, bg = rgb(.6, .13, .94, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Other', main = 'Variance in Shared $', xlim = c(0,6), ylim = c(0,6))
abline(a = 0, b = 1, col = 'black')
points(x = var(tg_sub_level$tg_var_sharedW), y = var(tg_sub_level$tg_var_sharedO), pch = 18, cex = 6)
plot(tg_sub_level$tg_var_sharedB, tg_sub_level$tg_var_sharedO, bg = rgb(.5, 1, .83, .5), pch = 21, cex = 4,
     xlab = 'Black', ylab = 'Other', main = 'Variance in Shared $', xlim = c(0,6), ylim = c(0,6))
abline(a = 0, b = 1, col = 'black')
points(x = var(tg_sub_level$tg_var_sharedB), y = var(tg_sub_level$tg_var_sharedO), pch = 18, cex = 6)
par(mfrow = c(1,1)) # Returning graphs to plot 1 at a time

# Offers to White partners are most variable, then offers to black partners, then offers to other partners.
# Only W>O is significant. 
# Potentially explanations:
#   - More individuation of white partners (and less with Other)
#   - More learning or other adjustment during the task w/ white partners (and less with Other)

# Between-Subject Variances
between_subj_var_shared_W = var(tg_sub_level$tg_mean_sharedW);
between_subj_var_shared_B = var(tg_sub_level$tg_mean_sharedB);
between_subj_var_shared_O = var(tg_sub_level$tg_mean_sharedO);
# numeric difference here (W = O > B)... 

var.test(tg_sub_level$tg_mean_sharedW, tg_sub_level$tg_mean_sharedB) # p = 0.52   W = B
var.test(tg_sub_level$tg_mean_sharedW, tg_sub_level$tg_mean_sharedO) # p = 0.97   W = O
var.test(tg_sub_level$tg_mean_sharedB, tg_sub_level$tg_mean_sharedO) # p = 0.54   B = O
# ... but no significant differences in variance across subjects in mean offers.


### Takeaways: Analyses of TG Offer Amounts, RTs, and Variance #####################################
# Offers: People offered more to B > O > W
# RTs: No differences in mean RTs or in the variance of RTs (by race).
# Offer Variances (within): Order is W > B > O, but only W > O is significant
# Offer Variances (between): No differences in variance in mean offers
#
# TL;DR: People offer different amounts on average (B > O > W), and might be more variable
# in their offers to white partners/less variable in their offers to other partners, 
# but RTs suggest that similar processes might be at work here, on average.

## TG: Changes over time ########################################################################

# Goal: to look at how offers and RTs change over time.

### Cumulative Trial Number Effects ################################################################

trustGame$dayrs = trustGame$day - 1; # dayrs = 0 on FIRST day, and 1 on SECOND day; additive effect of 2nd day
trustGame$cumTrialNumAcrossDays = trustGame$cumTrialNum + trustGame$dayrs * (max(trustGame$cumTrialNum))

# Q: What are the effects of cumulative trials on OFFERS
fit_shared_cumTrial = lmer(shared ~ 1 + cumTrialNum + dayrs + (1 | subjectID), data = trustGame)
summary(fit_shared_cumTrial)
# A1: Offer amounts decline within-day, but appear to have a big day 2 boost

tmpdata = trustGame[1:4,]
tmpdata$cumTrialNum = c(1, 76, 1, 76)
# tmpdata$cumTrialNumAcrossDays = c(1, 76, 77, 152)
tmpdata$dayrs = c(0, 0, 1, 1)
firstlast_Offer = predict(fit_shared_cumTrial, newdata = tmpdata)
# A2: DAY 1, Trial 1:  $2.18
#     DAY 1, Trial 76: $2.04
#     DAY 2, Trial 1:  $2.31
#     Day 2, Trial 76: $2.17
# Trust declines within session, but increases across days. 

# TO DO's:
# Model-free calculation of mean offer by window (4 windows every day; 19 trials/window).
# Do that by race.
# Then return to linear regressions to model the effects we see.



# Patterns over time in RT
# Q: What are the effects of cumulative trials on reaction times?
trustGame$sqrtrt = sqrt(trustGame$responseTime) # handle skew

fit_rt_cumTrial = lmer(sqrtrt ~ 1 + cumTrialNum + (1 | subjectID), data = trustGame)
summary(fit_rt_cumTrial)
# A1: with increasing trial number, people get significantly faster at making their trust decisions (RT decreases). 

tmpdata = trustGame[1:2,]
tmpdata$cumTrialNum = c(1, 138)
x = predict(fit_rt_cumTrial, newdata = tmpdata)
firstlast_RT = x^2
# A2: On the first trial, rt is 3.675 seconds, while on the last trial rt is 1.111 seconds
# People make trust decisions much faster over time. 
# Attribute to learning the task

#### Takeaways: Cumulative Time ################################################################
# People share less and do so more quickly with trial. Effects are big(ish).
# $2.25 in 3.2s -> $1.99 in 1.7s
# (decline of $0.26 and 1.6s!)






# NOTES BELOW HERE ################################################################################

# Q: Did people trust more in black, white, other races under stress (acute/chronic stress)?

# Q: If the previous share was not reciprocated, how did that affect subsequent sharing?
# Q: If the previous share was not reciprocated, was subsequent sharing affected differently for white vs. black race partners?
# Q: Is previous share reciprocity different for participants who are white vs. non-white?
