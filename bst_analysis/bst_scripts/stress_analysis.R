
# --- Stress Only Base Level Analysis Script --- #

#setwd("/Users/shlab/Documents/GitHub/bst/") #desktop
#setwd("~/Documents/GitHub/bst") #laptop

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

##### BATH Ratings ####

#Bath rating descriptives
mean(bath$stressUnpleasantnessRating) #6.128
sd(bath$stressUnpleasantnessRating) #1.301
mean(bath$controlUnpleasantnessRating) #2.026
sd(bath$controlUnpleasantnessRating)  #1.038

hist(bath$stressUnpleasantnessRating) # Max 7 (highly unpleasant)
hist(bath$controlUnpleasantnessRating) # Max 4 (mildly unpleasant)
# Participants consistently rated the CPT bath discomfort (under stress condition) highly unpleasant.
# Bath rating under control condition was rated mostly pleasant with a few participants rating the lukewarm bath moderately unpleasant.

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

##### CORT Scores ####

# Cortisol scores (1 & 2) across all participants #
# NOTE: These include cort scores of participants who did not complete both days of the experiment.

#Cort 1 Value Readings
mean(cort$cort_1_value, na.rm = T) #2.12
sd(cort$cort_1_value, na.rm = T) #2.56 
range(cort$cort_1_value, na.rm = T) # 0 to 26.51

#Cort 2 Value Readings
mean(cort$cort_2_value, na.rm = T) #2.03
sd(cort$cort_2_value, na.rm = T) #2.34 
range(cort$cort_2_value, na.rm = T) # 0 to 18.66

#Mean of Cort 1 & 2 Values
mean(cort$cortisol_mean_nmol_to_l, na.rm = T) #2.09
sd(cort$cortisol_mean_nmol_to_l, na.rm = T) #2.42
range(cort$cortisol_mean_nmol_to_l, na.rm = T) # 0 to 21.66

#Cort COV Percentage Means
mean(cort$cort_coeff_of_variance_as_percent, na.rm = T) #7.33
sd(cort$cort_coeff_of_variance_as_percent, na.rm = T) #6.50
range(cort$cort_coeff_of_variance_as_percent, na.rm = T) # 0.0 to 38.6

# Cortisol score means subject-level - ALL participants #
# Note: These include Stress_Subj_Level for participants who completed BOTH days of the experiment.

#Cort 1 Value Readings
mean(subj_level_cortisol$cort_1_value, na.rm = T) #2.02
sd(subj_level_cortisol$cort_1_value, na.rm = T) #1.73 
range(subj_level_cortisol$cort_1_value, na.rm = T) # 0.00 to 11.87

#Cort 2 Value Readings
mean(subj_level_cortisol$cort_2_value, na.rm = T) #1.95
sd(subj_level_cortisol$cort_2_value, na.rm = T) #1.61 
range(subj_level_cortisol$cort_2_value, na.rm = T) # 0.00 to 10.86

#Cort 1 & 2 Value Means
mean(subj_level_cortisol$cortisol_mean_nmol_to_l, na.rm = T) #1.997
sd(subj_level_cortisol$cortisol_mean_nmol_to_l, na.rm = T) #1.664
range(subj_level_cortisol$cortisol_mean_nmol_to_l, na.rm = T) # 0 to 11.37

#Cort COV Percentage Means
mean(subj_level_cortisol$cort_coeff_of_variance_as_percent, na.rm = T) #7.298
sd(subj_level_cortisol$cort_coeff_of_variance_as_percent, na.rm = T) #2.87
range(subj_level_cortisol$cort_coeff_of_variance_as_percent, na.rm = T) # 1.83 to 14.24



# Sample Effects on Cortisol (Across Trials)

subj_cort_mean_sample <- aggregate(cortisol_mean_nmol_to_l ~ sample, data = cort, FUN = mean, na.rm = TRUE)
# Q: Are there differences in cortisol levels from reading 1 to 4
#  sample cortisol_mean_nmol_to_l
#      1                1.808261
#      2                1.859425
#      3                2.816782
#      4                1.872442
# A: There is an upward trend in cort readings from readings 1 to 3 with a spike between readings 2 and 3,
# then a fall of cortisol level from reading 3 to 4.
# Cortisol does not fall below the reading 1 and 2, indicating stress is likely still elevated at the end of the tasks.


# Day Effects on Cortisol (Across Subjects)

# Means of each cort sample by day across participants (subject-level)
cort_means_by_sample_and_day = apply(cort_mtx, c(1, 2), mean, na.rm = TRUE)
#       [Day 1]     [Day 2]
# [1,] 1.541000 2.126429
# [2,] 1.735957 2.004500
# [3,] 3.026596 2.570250
# [4,] 1.823404 1.931538

# Standard deviations of each cort sample by day across participants (subject-level)
cort_sd_by_sample_and_day = apply(cort_mtx, c(1, 2), sd, na.rm = TRUE)
#       [Day 1]     [Day 2]
# [1,] 1.202253 3.321143
# [2,] 1.278518 2.890279
# [3,] 3.517918 2.768810
# [4,] 1.346669 1.690566

#Question: Are there differences for the four cort samples across days?
t.test(cort_mtx[1,1,,], cort_mtx[1,2,,], paired = F) # compare all sample 1 values across DAYS; p = 0.28
t.test(cort_mtx[2,1,,], cort_mtx[2,2,,], paired = F) # compare all sample 2 values across DAYS; p = 0.59
t.test(cort_mtx[3,1,,], cort_mtx[3,2,,], paired = F) # compare all sample 3 values across DAYS; p = 0.50
t.test(cort_mtx[4,1,,], cort_mtx[4,2,,], paired = F) # compare all sample 4 values across DAYS; p = 0.75
# No significant differences in paired t-tests between sample numbers across days.


# Using cort_mtx, what is the average cort trajectory as a function of day? 
#cort_means_by_sample_and_day = rowMeans(cort_mtx, dims = 2, na.rm = T) # Used apply in setup_data script
plot(x = 1:4, y = cort_means_by_sample_and_day[,1], col = "blue",type = 'l', lwd = 2)
lines(x = 1:4, y = cort_means_by_sample_and_day[,2], col = "red", type = "b", lty = 'dotted', lwd = 2)
# People enter Day 2 with greater cortisol than day 1


#Question: Are there differences for the CONTROL condition between four cort samples across days?
t.test(cort_mtx[1,1,1,], cort_mtx[1,2,1,], paired = F)
#A: t-test reveals cort reading 1 from day 1 to day 2 is NOT significant. p = 0.2462
t.test(cort_mtx[2,1,1,], cort_mtx[2,2,1,], paired = F) 
#A: t-test reveals cort reading 2 from day 1 to day 2 is NOT significant. p = 0.3542
t.test(cort_mtx[3,1,1,], cort_mtx[3,2,1,], paired = F) 
#A: t-test reveals cort reading 3 from day 1 to day 2 is NOT significant. p = 0.6568
t.test(cort_mtx[4,1,1,], cort_mtx[4,2,1,], paired = F) 
#A: t-test reveals cort reading 4 from day 1 to day 2 is NOT significant. p = 0.638
#Key take-away: There does not appear to be a difference of day on CONTROL cort sample reading across days.



# Condition Effects on Cortisol (Across Subjects)

# Question: Are there differences in cort readings across days per condition ?

# Calculate mean cort for 4 readings by conditions (control, stress)
cort_means_by_sample_and_condition = apply(cort_mtx, c(1, 3), mean, na.rm = TRUE)
#     [Control]     [Stress]
# [1,] 2.085682 1.553958
# [2,] 2.025814 1.696818
# [3,] 1.877907 3.734318
# [4,] 1.512791 2.232093

# Standard deviations of each cort sample by condition across participants (subject-level)
cort_sd_by_sample_and_condition = apply(cort_mtx, c(1, 3), sd, na.rm = TRUE)
#     [Control]     [Stress]
# [1,] 3.274019 1.162003
# [2,] 2.860131 1.147460
# [3,] 2.050817 3.800791
# [4,] 1.210964 1.687216
# There is a clear increase in reading 3 under the stress condition.

#Question: Are there differences between the control vs stress condition between four cort samples?
t.test(cort_mtx[1,1,1,], cort_mtx[1,1,2,], paired = F)
#A: t-test reveals cort reading 1 control vs. stress condition difference is NOT significant. p = 0.83
t.test(cort_mtx[2,1,1,], cort_mtx[2,1,2,], paired = F) 
#A: t-test reveals cort reading 2 control vs. stress condition difference is NOT significant. p = 0.56
t.test(cort_mtx[3,1,1,], cort_mtx[3,1,2,], paired = F) 
#A: t-test reveals cort reading 3 control vs. stress condition difference IS significant. p = 0.02
t.test(cort_mtx[4,1,1,], cort_mtx[4,1,2,], paired = F) 
#A: t-test reveals cort reading 4 control vs. stress condition difference is approaching significance. p = 0.06
#Key take-away: There is an impact of the stress vs. control condition on the reading 3 cort response.


#Plot mean cort for each reading with control/stress conditions with corrected time stamp (from stressor) 
matplot(x = c(-2, 3, 13, 30), y = apply(cort_mtx, c(1,3), mean, na.rm = T)) 
# A: Yes, there is a clear difference between cort reading 3 under stress vs. control


#Question: Are the differences in readings (3 & 1) significant across conditions?
t.test(apply(cort_mtx[1,,1,], 2, sumna), apply(cort_mtx[1,,2,], 2, sumna), paired = T)  
#A: t-test reveals cort reading 1 differences for control and stress condition are NOT significant. p = .33
t.test(apply(cort_mtx[2,,1,], 2, sumna), apply(cort_mtx[2,,2,], 2, sumna), paired = T)  
#A: t-test reveals cort reading 2 differences for control and stress condition are NOT significant. p = .39
t.test(apply(cort_mtx[3,,1,], 2, sumna), apply(cort_mtx[3,,2,], 2, sumna), paired = T)  
#A: t-test reveals cort reading 3 differences for control and stress condition ARE significant. p = 0.0007266
t.test(apply(cort_mtx[4,,1,], 2, sumna), apply(cort_mtx[4,,2,], 2, sumna), paired = T)  
#A: t-test reveals cort reading 4 differences for control and stress condition ARE significant. p = 0.006921


hist(apply(cort_mtx[3,,1,], 2, sumna) - apply(cort_mtx[1,,1,], 2, sumna))
hist(apply(cort_mtx[3,,2,], 2, sumna) - apply(cort_mtx[1,,2,], 2, sumna))

plot(apply(cort_mtx[3,,1,], 2, sumna) - apply(cort_mtx[1,,1,], 2, sumna), apply(cort_mtx[3,,2,], 2, sumna) - apply(cort_mtx[1,,2,], 2, sumna)); lines(x = c(-100, 100), y = c(-100, 100)) 
#Plot of reading 3 to reading 1 difference for stress condition with line,
#below line is reading 1 higher than reading 3 even under stress. Most are above the line, plus t-test shows sig


##### T0-DO ####

# REGRESSIONS for Cort including condition, controlling for day.

# Reviewed day effects but need to examine day with stress.
# Rework sample and day with subj-level data. COMPLETE: The day and sample differences are not significant.

# Examine day effects on cort reading 1 for 

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
t.test(Stress_Subj_Level$pssSum ~ Stress_Subj_Level$day2bool_0control_1stress==0)
# A: The difference in self-reported PSS scores were not significantly affected by the presence of the cold water bath on day 2
# Key results - Received cold bath mean = 16, Didn't received lukewarm bath mean = 15.86 (p = .94, t=-.075) 

# --- Combining Acute & Chronic Stress Measures --- #
#combining acute and chronic stressor data into wide data





