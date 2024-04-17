# --- Stress Only Base Level Analysis Script --- #

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


#### ACUTE ####

#Acute descriptives
mean(bst_bath$stressUnpleasantnessRating)
sd(bst_bath$stressUnpleasantnessRating)
mean(bst_bath$controlUnpleasantnessRating)
sd(bst_bath$controlUnpleasantnessRating)

#effect of stressor on ratings
#paired sample t-test (ie a test of differences)
t.test(bst_bath$stressUnpleasantnessRating - bst_bath$controlUnpleasantnessRating, mu = 0, var.equal = TRUE)
# PSH NOTE: This is run as a one-sample t-test, you could also run it as follows:
t.test(bst_bath$stressUnpleasantnessRating,bst_bath$controlUnpleasantnessRating, paired = T, var.equal = T)
# PSH NOTE: paired t-tests are reducible to one-sample t-tests against 0, so there's no
# meaningful statistical difference here, but it can be easier/clearer to communicate and/or
# think through b/c it's one less data-prep step.

#Key Results (p = 3.0e-16, mean difference = 4.1) - the ratings are statistically very probably different
# PSH NOTE: This is a BIG difference. A diff of 4.1 when your scale goes 1-7 is huge.
# PSH NOTE: use exact p-value estimates when possible; no reason not to.
# EB NOTE: t(38) = 13.66, p < .001; Mean diff = 4.10
# EB NOTE: Those in the stress condition reported significantly lower pleasantness compared to the control condition (M = 4.10).

#checking for an ORDER effect on the CHANGE in ratings
#t.test(bst_bath$diffPleasantnessRating ~ bst_bath$day2StressedBool)  #EB : not sig different
#t.test(bst_bath$stressUnpleasantnessRating ~ bst_bath$day2StressedBool)  #EB : not sig different
#t.test(bst_bath$controlUnpleasantnessRating ~ bst_bath$day2StressedBool)  #EB : not sig different

# checking for an effect of DAY on change in ratings
# Put the change in ratings into DAY space (Day 2 - Day 1)
diffratings_byday = bst_bath$diffPleasantnessRating[bst_bath$day2StressedBool==1];
diffratings_byday = append(diffratings_byday,-bst_bath$diffPleasantnessRating[bst_bath$day2StressedBool==0])
t.test(diffratings_byday)
# p = 0.20, t(38) = 1.29, M = -0.92 (meaning ratings are non-significantly higher on average on day 1)
# No significant change in ratings overall from Day 1 to Day 2

#### CHRONIC ####

#descriptives of PSS
mean(bst_pss$pssSum)
sd(bst_pss$pssSum)
range(bst_pss$pssSum)
count(bst_pss$pssSumCategorical)
pss_median <- median(bst_pss$pssSum)
pss_median

#coninued descriptives only on the two median split groups
by(data = bst_pss$pssSum, INDICES = bst_pss$pssMedianSplit, FUN = mean)
by(data = bst_pss$pssSum, INDICES = bst_pss$pssMedianSplit, FUN = sd)
by(data = bst_pss$pssSum, INDICES = bst_pss$pssMedianSplit, FUN = median)
by(data = bst_pss$pssSum, INDICES = bst_pss$pssMedianSplit, FUN = range)

#histogram of PSS scores
ggplot(bst_pss, aes(x=pssSum)) +
  geom_histogram(binwidth = 3, color = "black", fill = "gray") +
  scale_x_continuous(name = "PSS Score", breaks = c(0,5,10,15,20,25,30,35,40), limits = c(0,40), expand = c(0,0)) +
  scale_y_continuous(name = "number of participants", expand = c(0,0), breaks = seq(0,20,2), limits = c(0,10)) + # Removes the padding between data and the axis
  # labs(title="PSS Scores") + # Arguably redundant w/ the x-label
  geom_vline(xintercept = 13, linetype="dotted", color = "red", size=1.5) + # Add vertical lines at the categorical boundaries
  geom_vline(xintercept = 27, linetype="dotted", color = "red", size=1.5) +
  geom_vline(xintercept = pss_median, linetype="dashed", color = "blue", size = 1.5) + #adds vertical line for the median
  theme_classic()

#### ACUTE & CHRONIC INTERACTIONS ####

#effect of acute stressor n chronic stressor
#t-test on if there was a difference in PSS scores based on what type of bath was received on day 2 (the PSS was only administered on day 2)
t.test(bst_bath_pss$pssSum ~ bst_bath_pss$day2StressedBool)
#Key results (p = .94, t=-.075) received cold bath mean = 16, didn't received lukewarm bath mean = 15.86
# the PSS scores are not significantly affected by the presence of the cold water bath on day 2




