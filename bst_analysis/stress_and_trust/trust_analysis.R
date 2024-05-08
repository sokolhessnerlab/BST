
# --- Trust Only Base Level Analysis Script --- #

#setwd("/Users/shlab/Documents/GitHub/bst")

#NOTE: Run this script while connected to the shlab drive, 
# where the script's data sources are under /Volumes

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
mean(aggregate(rating ~ subjectID, data = trustRating, FUN=mean)$rating)
mean(aggregate(rating ~ subjectID, data = trustRating, FUN=sd)$rating)

#TO DO: EB create new df with stressed bool in set up then approach
# Trust Rating descriptives (subject level)
tmpvect = aggregate(rating ~ subjectID + stressedBool, data = trustRating, FUN=mean);
# Trust Rating means within condition
by(data = tmpvect$rating, INDICES = tmpvect$stressedBool, FUN = mean) 
# control = 4.74, stressed = 4.67

# Trust Rating Day Effects
#by(data = trustRating$rating, INDICES = trustRating$day, FUN = mean)
#by(data = trustRating$rating, INDICES = trustRating$day, FUN = sd)

# Trust Rating Day Effects
tmpvect = aggregate(rating ~ subjectID + day, data = trustRating, FUN=mean);
by(data = tmpvect$rating, INDICES = tmpvect$day, FUN = mean) # day 1 = 4.73, day 2 = 4.68

# Trust Game Task Order Effects
by(data = trustGame$shared, INDICES = trustGame$taskOrder, FUN = mean)
by(data = trustGame$shared, INDICES = trustGame$taskOrder, FUN = sd)

tmpvect = aggregate(shared ~ subjectID + taskOrder, data = trustGame, FUN=mean);
by(data = tmpvect$shared, INDICES = tmpvect$taskOrder, FUN = mean) # order 1 = 4.69, order 2 = 4.84

# Q: Were participants stressed before doing the trust task?

#### TRUST GAME ####

##### Subject-Level Loop #####
tg_sub_level_colnames = c(
  'tg_mean_shared',
  'tg_mean_sharedW',
  'tg_mean_sharedB',
  'tg_mean_sharedO',
  'tg_var_shared',
  'tg_var_sharedW',
  'tg_var_sharedB',
  'tg_var_sharedO'
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
}

# Visualize Distributions of Mean Offers
mean_shared_hist = hist(tg_sub_level$tg_mean_shared, breaks = seq(from = 0, to = 5, by = 0.5), plot = F);
mean_sharedW_hist = hist(tg_sub_level$tg_mean_sharedW, breaks = seq(from = 0, to = 5, by = 0.5), plot = F)
mean_sharedB_hist = hist(tg_sub_level$tg_mean_sharedB, breaks = seq(from = 0, to = 5, by = 0.5), plot = F)
mean_sharedO_hist = hist(tg_sub_level$tg_mean_sharedO, breaks = seq(from = 0, to = 5, by = 0.5), plot = F)

plot(mean_shared_hist$mids, mean_shared_hist$density, col = rgb(0,0,0), type = 'l', lwd = 3, xlim = c(0,5), ylim = c(0,.7), xlab = 'Mean Shared', ylab = 'Frequency')
lines(mean_sharedW_hist$mids, mean_sharedW_hist$density, col = rgb(1,0,0), lwd = 3)
lines(mean_sharedB_hist$mids, mean_sharedB_hist$density, col = rgb(0,1,0), lwd = 3)
lines(mean_sharedO_hist$mids, mean_sharedO_hist$density, col = rgb(0,0,1), lwd = 3)
legend(x = 3.5, y = .7, c('Overall','White','Black','Other'), lwd = 2, col = c('black','red','green','blue'))

t.test(tg_sub_level$tg_mean_sharedW, tg_sub_level$tg_var_sharedB, paired = T) # p = 0.0006      B > W
t.test(tg_sub_level$tg_mean_sharedW, tg_sub_level$tg_var_sharedO, paired = T) # p = 0.000057    O > W
t.test(tg_sub_level$tg_mean_sharedB, tg_sub_level$tg_var_sharedO, paired = T) # p = 0.00000015  B > O

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
fit_shared_cumTrial = lmer(shared ~ 1 + cumTrialNum + (1 | subjectID), data = trustGame)
summary(fit_shared_cumTrial)
# with increasing trial number, people share less. 

tmpdata = trustGame[1:2,]
tmpdata$cumTrialNum = c(1, 138)
x = predict(fit_shared_cumTrial, newdata = tmpdata)
# On the first trial, mean shared is ~ $2.23, while on the last trial it's ~ $1.97
# Trust declines over time. 



# Reaction Times (decision speed)



# Variances in decisions





# Q: Did people choose to share more or less under acute stress?
# Q: Did people choose to share more or less  under chronic stress?

# Q: Did people choose to share or not more or less for black, white, other races?
# Q: #Did people choose to share or not more or less for black, white, other races under stress?

# Q: If the previous share was not unreciprocated, how did that affect subsequent sharing?
# Q:  what if the unreciprocated share was not of white race vs. POC race?
