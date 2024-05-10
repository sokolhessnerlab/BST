
# --- Trust Only Base Level Analysis Script --- #


#NOTE: Run this script while connected to the shlab drive,
# script's data sources are under /Volumes
# and set working directory via shlab Github
#setwd("/Users/shlab/Documents/GitHub/bst/")


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

#### TRUST RATINGS ####

#BST trust rating for the trust perception task.

#Trust Rating descriptives (collapsed)
mean(aggregate(rating ~ subjectID, data = trustRating, FUN=mean)$rating) #Mean Trust Rating = 4.703949
mean(aggregate(rating ~ subjectID, data = trustRating, FUN=sd)$rating)  #SD Trust Rating = 1.50436


# Trust Rating descriptives (subject level)
#Q: What are the effects of stress on peoples' trust perceptions? 
tmpvect = aggregate(rating ~ subjectID + stressedBool, data = trustRating, FUN=mean);
# Trust Rating means within condition
by(data = tmpvect$rating, INDICES = tmpvect$stressedBool, FUN = mean) 
#A: Acute stress seems to decrease peoples' trust perceptions.
# control = 4.74, stressed = 4.67


# Trust Rating Day Effects
#Q: What are the effects of day on peoples' trust perceptions? 
tmpvect = aggregate(rating ~ subjectID + day, data = trustRating, FUN=mean);
by(data = tmpvect$rating, INDICES = tmpvect$day, FUN = mean) 
#A: Participants' trust perceptions decrease from day 1 to day 2.
#Day 1: 4.730305
#Day 2: 4.680925


#### TRUST GAME ####


# Trust Game Task Order Effects
#Q: What are the effects of task order on peoples' trust behaviors? 
# (collapsed)
by(data = trustGame$shared, INDICES = trustGame$taskOrder, FUN = mean)
by(data = trustGame$shared, INDICES = trustGame$taskOrder, FUN = sd)
# On average, people share more when the CPT (stressor) is given on day 1)
# Task Order 1: 2.722(1.640), Task Order 2: 2.272(1.477)
#(subject-level)
tmpvect = aggregate(shared ~ subjectID + taskOrder, data = trustGame, FUN=mean);
by(data = tmpvect$shared, INDICES = tmpvect$taskOrder, FUN = mean) # order 1 = 4.69, order 2 = 4.84
#A: Participants are sharing more when task order is 1 vs. 2
# Task Order 1: 2.600, Task Order 2: 2.304


# Q: Were participants stressed before doing the trust task?


##### Subject-Level Loop #####
tg_sub_level_colnames = c(
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
  
  tg_sub_level$tg_mean_shared[s] = mean(tmp_data$shared)
  tg_sub_level$tg_mean_sharedW[s] = mean(tmp_data$shared[tmp_data$partnerRace_0w_1b_2o == 0])
  tg_sub_level$tg_mean_sharedB[s] = mean(tmp_data$shared[tmp_data$partnerRace_0w_1b_2o == 1])
  tg_sub_level$tg_mean_sharedO[s] = mean(tmp_data$shared[tmp_data$partnerRace_0w_1b_2o == 2])
  
  tg_sub_level$tg_var_shared[s] = var(tmp_data$shared)
  tg_sub_level$tg_var_sharedW[s] = var(tmp_data$shared[tmp_data$partnerRace_0w_1b_2o == 0])
  tg_sub_level$tg_var_sharedB[s] = var(tmp_data$shared[tmp_data$partnerRace_0w_1b_2o == 1])
  tg_sub_level$tg_var_sharedO[s] = var(tmp_data$shared[tmp_data$partnerRace_0w_1b_2o == 2])
  
  tg_sub_level$tg_rt_shared[s] = mean(tmp_data$responseTime)
  tg_sub_level$tg_rt_sharedW[s] = mean(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 0])
  tg_sub_level$tg_rt_sharedB[s] = mean(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 1])
  tg_sub_level$tg_rt_sharedO[s] = mean(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 2])
  
  tg_sub_level$tg_rt_var_shared[s] = var(tmp_data$responseTime)
  tg_sub_level$tg_rt_var_sharedW[s] = var(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 0])
  tg_sub_level$tg_rt_var_sharedB[s] = var(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 1])
  tg_sub_level$tg_rt_var_sharedO[s] = var(tmp_data$responseTime[tmp_data$partnerRace_0w_1b_2o == 2])
}


##### Mean Offer Visualizations & Analysis #####

# Visualize Distributions of Mean Offers
mean_shared_hist = hist(tg_sub_level$tg_mean_shared, breaks = seq(from = 0, to = 5, by = 0.5), plot = F);
mean_sharedW_hist = hist(tg_sub_level$tg_mean_sharedW, breaks = seq(from = 0, to = 5, by = 0.5), plot = F)
mean_sharedB_hist = hist(tg_sub_level$tg_mean_sharedB, breaks = seq(from = 0, to = 5, by = 0.5), plot = F)
mean_sharedO_hist = hist(tg_sub_level$tg_mean_sharedO, breaks = seq(from = 0, to = 5, by = 0.5), plot = F)

plot(mean_shared_hist$mids, mean_shared_hist$density, col = rgb(0,0,0), type = 'l', lwd = 3, xlim = c(0,5), ylim = c(0,.7), xlab = 'Mean Shared', ylab = 'Frequency')
lines(mean_sharedW_hist$mids, mean_sharedW_hist$density, col = rgb(1,0,0), lwd = 3)
lines(mean_sharedB_hist$mids, mean_sharedB_hist$density, col = rgb(0,1,0), lwd = 3)
lines(mean_sharedO_hist$mids, mean_sharedO_hist$density, col = rgb(0,0,1), lwd = 3)
legend(x = 4, y = .675, c('Overall','White','Black','Other'), lwd = 2, col = c('black','red','green','blue'))

#t-tests for Mean Offers
t.test(tg_sub_level$tg_mean_sharedW, tg_sub_level$tg_mean_sharedB, paired = T) # p = 0.0006      B > W
t.test(tg_sub_level$tg_mean_sharedW, tg_sub_level$tg_mean_sharedO, paired = T) # p = 0.000057    O > W
t.test(tg_sub_level$tg_mean_sharedB, tg_sub_level$tg_mean_sharedO, paired = T) # p = 0.00000015  B > O

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

# Patterns over time in sharing
# Q: What are the effects of cumulative trials on sharing?
fit_shared_cumTrial = lmer(shared ~ 1 + cumTrialNum + (1 | subjectID), data = trustGame)
summary(fit_shared_cumTrial)
# A1: with increasing trial number, people share less. 

tmpdata = trustGame[1:2,]
tmpdata$cumTrialNum = c(1, 138)
x = predict(fit_shared_cumTrial, newdata = tmpdata)
# A2: On the first trial, mean shared is ~ $2.23, while on the last trial it's ~ $1.97
# Trust declines over time. 


##### Response Time Visualizations & Analysis #####

# Reaction Times (decision speed)

tmp_data %>%
  ggplot(aes(responseTime)) +
  geom_density()

#tmp_data %>%
  #ggplot(aes(responseTime, fill = partnerRace_0w_1b_2o)) +
  #geom_histogram(binwidth = 200, color = "black", alpha = .7) +
  #geom_vline(aes(xintercept = mean(responseTime, na.rm = TRUE)), color = "red", 
             #linetype = "dashed", size = 1)

rt_shared_hist = hist(tg_sub_level$tg_rt_shared, breaks = seq(from = 0, to = 10, length.out = 12), plot = F);
rt_sharedW_hist = hist(tg_sub_level$tg_rt_sharedW, breaks = seq(from = 0, to = 10, length.out = 12), plot = F)
rt_sharedB_hist = hist(tg_sub_level$tg_rt_sharedB, breaks = seq(from = 0, to = 10, length.out = 12), plot = F)
rt_sharedO_hist = hist(tg_sub_level$tg_rt_sharedO, breaks = seq(from = 0, to = 10, length.out = 12), plot = F)


plot(rt_shared_hist$mids, rt_shared_hist$density, col = rgb(0,0,0), type = 'l', lwd = 3, xlim = c(0,10), ylim = c(0,.4), xlab = 'Response Time', ylab = 'Frequency')
lines(rt_sharedW_hist$mids, rt_sharedW_hist$density, col = rgb(1,0,0), lwd = 3)
lines(rt_sharedB_hist$mids, rt_sharedB_hist$density, col = rgb(0,1,0), lwd = 3)
lines(rt_sharedO_hist$mids, rt_sharedO_hist$density, col = rgb(0,0,1), lwd = 3)
legend(x = 8.25, y = .375, c('Overall','White','Black','Other'), lwd = 2, col = c('black','red','green','blue'))


#t-tests for Mean RTs (race comparisons)
t.test(tg_sub_level$tg_rt_sharedW, tg_sub_level$tg_rt_sharedB, paired = T) # p = 0.9346   B = W
t.test(tg_sub_level$tg_rt_sharedW, tg_sub_level$tg_rt_sharedO, paired = T) # p = 0.9143   O = W
t.test(tg_sub_level$tg_rt_sharedB, tg_sub_level$tg_rt_sharedO, paired = T) # p = 0.9711   B = O


par(mfrow = c(1,3)) # Returning graphs to plot 1 at a time
plot(tg_sub_level$tg_rt_sharedW, tg_sub_level$tg_rt_sharedB, bg = rgb(.6, .3, 0, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Black', main = 'Response Time', xlim = c(0,10), ylim = c(0,10))
abline(a = 0, b = 1, col = 'black')
#points(x = rt(tg_sub_level$tg_rt_sharedW), y = rt(tg_sub_level$tg_rt_sharedB), pch = 18, cex = 6)
plot(tg_sub_level$tg_rt_sharedW, tg_sub_level$tg_rt_sharedO, bg = rgb(.6, .13, .94, .5), pch = 21, cex = 4,
     xlab = 'White', ylab = 'Other', main = 'Response Time', xlim = c(0,10), ylim = c(0,10))
abline(a = 0, b = 1, col = 'black')
#points(x = rt(tg_sub_level$tg_rt_sharedW), y = rt(tg_sub_level$tg_rt_sharedO), pch = 18, cex = 6)
plot(tg_sub_level$tg_rt_sharedB, tg_sub_level$tg_rt_sharedO, bg = rgb(.5, 1, .83, .5), pch = 21, cex = 4,
     xlab = 'Black', ylab = 'Other', main = 'Response Time', xlim = c(0,10), ylim = c(0,10))
abline(a = 0, b = 1, col = 'black')
#points(x = rt(tg_sub_level$tg_rt_sharedB), y = rt(tg_sub_level$tg_rt_sharedO), pch = 18, cex = 6)
par(mfrow = c(1,1)) # Returning graphs to plot 1 at a time


# Patterns over time in RT
# Q: What are the effects of cumulative trials on reaction times?
fit_rt_cumTrial = lmer(responseTime ~ 1 + cumTrialNum + (1 | subjectID), data = trustGame)
summary(fit_rt_cumTrial)
# A1: with increasing trial number, people get significantly faster at making their trust decisions (RT decreases). 

tmpdata = trustGame[1:2,]
tmpdata$cumTrialNum = c(1, 138)
x = predict(fit_rt_cumTrial, newdata = tmpdata)
# A2: On the first trial, rt is 3.675 seconds, while on the last trial rt is 1.111 seconds
# People make trust decisions much faster over time. 
# Attribute to learning the task


##### Variances in Offer Decisions Visualizations & Analysis #####

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
t.test(tg_sub_level$tg_var_sharedW, tg_sub_level$tg_var_sharedB, paired = T) # p = 0.314      B = W
t.test(tg_sub_level$tg_var_sharedW, tg_sub_level$tg_var_sharedO, paired = T) # p = 0.008454   O > W
t.test(tg_sub_level$tg_var_sharedB, tg_sub_level$tg_var_sharedO, paired = T) # p = 0.1231     B = O
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



# Q: Did people trust more in black, white, other races?
# Q: Did people trust more in black, white, other races under stress (acute)?
# Q: Did people trust more in black, white, other races under stress (chronic)?

# Q: If the previous share was not reciprocated, how did that affect subsequent sharing?
# Q: If the previous share was not reciprocated, was subsequent sharing affected differently for white vs. black race partners?
# Q: Is previous share reciprocity different for participants who are white vs. non-white?
