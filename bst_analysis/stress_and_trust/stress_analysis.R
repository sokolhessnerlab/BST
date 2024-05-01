
# --- Stress Only Base Level Analysis Script --- #

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


#### ACUTE ####

#BST Acute Stressor measures include bath unpleasantness ratings & cortisol.

#Bath rating descriptives
mean(bath$stressUnpleasantnessRating) #6.128
sd(bath$stressUnpleasantnessRating) #1.301
mean(bath$controlUnpleasantnessRating) #2.026
sd(bath$controlUnpleasantnessRating)  #1.038

hist(bath$stressUnpleasantnessRating) 
hist(bath$controlUnpleasantnessRating) 
#bath rating under stress condition was fairly consistently rated highly unpleasant
#bath rating under control condition was rated mostly pleasant with a few participants rating the lukewarm bath moderately unpleasant

# Effect of acute stressor - Cold Pressor Task (CPT) - on stress ratings:

# Q: Was the bath rating significantly more unpleasant for the stress (CPT) vs. control (lukewarm) conditions?
t.test(bath$stressUnpleasantnessRating,bath$controlUnpleasantnessRating, paired = T, var.equal = T)
# A: Bath ratings under the stress condition were rated significantly more UNpleasant than under the control condition (Mean Difference = 4.10).
# Considering the bath rating scale ranges from 1-7, a mean difference of 4.1 between rating scores is substantial.
# t(38) = 13.66, p < .00001; Mean diff = 4.10

# Q: Are there ORDER effects on the change in bath ratings?
t.test(bath$diffUnPleasantnessRating ~ bath$day2bool_0control_1stress)
# A: Whether stressor was given on day 1 or day 2 does not have a significant effect on difference in unpleasantness ratings between control and stress conditions
t.test(bath$stressUnpleasantnessRating ~ bath$day2bool_0control_1stress)
# A: Whether stressor was given on day 1 or day 2 does not have a significant effect on degree of unpleasantness rating for the stress condition
t.test(bath$controlUnpleasantnessRating ~ bath$day2bool_0control_1stress)
# A: Whether stressor was given on day 1 or day 2 does not have a significant effect on degree of unpleasantness rating for the control condition,
# Participants' mean unpleasantness rating for the control (lukewarm bath) was trending lower when the stressor was given on day 1 than day 2
# but this effect was not significant

# Q: Are there DAY effects on the change in bath ratings?
# Q: Does difference in stressor and control bath unpleasantness rating vary significantly by stressor day?
diffratings_byday = bath$diffUnPleasantnessRating[bath$day2bool_0control_1stress==1]; #stressor day 2
diffratings_byday = append(diffratings_byday,-bath$diffUnPleasantnessRating[bath$day2bool_0control_1stress==0])  #stressor day 1
t.test(diffratings_byday)
# A: Bath ratings are non-significantly higher on average if stressor was given on day 1 or day 2
# p = 0.20, t(38) = 1.29, M = -0.92 
# There do not appear to be significant day effects on bath ratings.

# Cortisol descriptives
# Add cortisol measures here.

#### CHRONIC ####

#BST Chronic Stressor measure is Perceived Stress Scale (PSS Score, PSS Category, PSS by median split)

# PSS Score Descriptives
mean(pss$pssSum) #15.917
sd(pss$pssSum) #5.709
range(pss$pssSum) # 3 27
pss_median <- median(pss$pssSum)  # 16.5

# Q: How chronically stressed are participants in our study?
#histogram of PSS scores by categorical splits (low stress 1-13, moderate stress 14-27, high stress 28+)
ggplot(pss, aes(x=pssSum)) +
  geom_histogram(binwidth = 3, color = "black", fill = "gray") +
  scale_x_continuous(name = "PSS Score", breaks = c(0,5,10,15,20,25,30,35,40), limits = c(0,40), expand = c(0,0)) +
  scale_y_continuous(name = "Number of Participants", expand = c(0,0), breaks = seq(0,20,2), limits = c(0,10)) + # Removes the padding between data and the axis
  geom_vline(xintercept = 13, linetype="dotted", color = "red", size=1.5) + # Add vertical lines at the categorical boundaries
  geom_vline(xintercept = 27, linetype="dotted", color = "red", size=1.5) +
  geom_vline(xintercept = pss_median, linetype="dashed", color = "blue", size = 1.5) + #adds vertical line for the median
  theme_classic()
# A: Participants in this study appear to be primarily under low to moderate chronic stress.

#PSS Median Split Descriptives
by(data = pss$pssSum, INDICES = pss$pssMedianSplit, FUN = mean)
# Mean of lower median split PSS scores = 11.389; Mean of higher median split PSS scores = 20.444
by(data = pss$pssSum, INDICES = pss$pssMedianSplit, FUN = sd)
# SD of lower median split PSS scores = 3.775; SD of higher median split PSS scores = 3.072
by(data = pss$pssSum, INDICES = pss$pssMedianSplit, FUN = median)
# Median of lower median split PSS scores = 12.5; Median of higher median split PSS scores = 20
by(data = pss$pssSum, INDICES = pss$pssMedianSplit, FUN = range)
# Range of lower median split PSS scores = 3:16; Range of higher median split PSS scores = 17:27


#### ACUTE & CHRONIC INTERACTIONS ####

# Q: Are there interaction effects of acute stressor & chronic stress?
# Examine whether people reacted differently to CPT (acute stressor) and/or lukewarm bath (control) if chronically stressed.
# A: ADD

# Q: Was there a difference in PSS scores (self-reporting of chronic stress) based on what type of bath was received on day 2?
# Note: PSS was only administered on day 2
t.test(STRESS$pssSum ~ STRESS$day2bool_0control_1stress==0)
# A: The difference in self-reported PSS scores were not significantly affected by the presence of the cold water bath on day 2
# Key results - Received cold bath mean = 16, Didn't received lukewarm bath mean = 15.86 (p = .94, t=-.075) 





