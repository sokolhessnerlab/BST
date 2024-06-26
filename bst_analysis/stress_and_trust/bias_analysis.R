
# --- Bias (Race Attitudes) Only Base Level Analysis Script --- #

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


# IMPLICIT Bias Measures ######################

## AMP ########################################

#AMP descriptives
summary(bst_amp)
str(bst_amp)


#Examine Response Times
mean(bst_amp$responseTime) #0.912
sd(bst_amp$responseTime) #0.7649 High compared to mean
max(bst_amp$responseTime)  #CHECK: Max high?
min(bst_amp$responseTime)  #CHECK: Min low?

#low-end quantile
quantile(bst_amp$responseTime, c(.0001,.05,.125,.5,.875,.95,.9999))

hist(bst_amp$responseTime, breaks = 100)

#plot distribution on low end
#ggplot(bst_amp, aes(x=responseTime)) +
#geom_histogram() +
#labs(title = "Density of AMP Response Time", y = "Density") +
#scale_x_continuous(name = "Response Time (S)", limits = c(0,1)) +
#theme_classic()



#Examine Stimulus Race (image) by Pleasantness Ratings (of image)
by(data = bst_amp$unPleasant0_Pleasant1, INDICES = bst_amp$stimulusRace_0w_1b_2o, FUN = mean)
by(data = bst_amp$unPleasant0_Pleasant1, INDICES = bst_amp$stimulusRace_0w_1b_2o, FUN = sd)
#white stim AMP unpleasantness .4947 mean
#black stim AMP unpleasantness .4934 mean
#other stim AMP unpleasantness .4932 mean
#White stimuli (.4947), black stimuli (.4934), and other stimuli (.4932) rated approximately equally pleasant

#Stimulus race (white,black,other) by frequency of unpleasant (0) vs pleasant (1) rating

#library(gt)
#library(gtExtras)
#bst_amp %>%
#dplyr::select(responseTime) %>%
#head(100) %>%
#gt()

#xtabs(~stimulusRace_0w_1b_2o + unPleasant0_Pleasant1, data = bst_amp)
prop.table(table(bst_amp$stimulusRace_0w_1b_2o, bst_amp$unPleasant0_Pleasant1),1)*100
#                   unPleasant0_Pleasant1
#stimulusRace_0w_1b_2o         0         1
#                   0       42.72%    57.28%
#                   1       41.87%    58.13%
#                   2       41.74%    58.26%
# across stimuli race (w,b,o), participants rated stimuli pleasant more than unpleasant

#Factoring for use in ggplot
bst_amp$unPleasant0_Pleasant1_F <- factor(bst_amp$unPleasant0_Pleasant1)
bst_amp$stimRace_F <- factor(bst_amp$stimulusRace_0w_1b_2o)
bst_amp$amp1amp2_F <- factor(bst_amp$amp1_amp2)

# SUMMARY OF BASIC DESCRIPTIVES
# Average pleasant/unpleasant judgments are pretty similar at the group-level, though RTs may be different.
# There are excessively fast RTs and excessively slow RTs - will need to remove at least some trials and/or
# participants.

# Subject-level Analyses

# Set acceptable bounds for RTs
lower_RT_bound = 50/1000; # Written in milliseconds, converted to seconds.
upper_RT_bound = 6000/1000; # Written in milliseconds, converted to seconds.

number_of_AMP_subjects = length(unique(bst_amp$subjectID));
subject_IDs = unique(bst_amp$subjectID);

amp_colnames = c('subjectID',
                 'judgments_mean_overall',
                 'judgments_mean_white',
                 'judgments_mean_black',
                 'judgments_mean_other',
                 'responseTime_mean_overall',
                 'responseTime_mean_white',
                 'responseTime_mean_black',
                 'responseTime_mean_other',
                 'RTs_outside_bounds');

amp_summary_stats = array(data = NA, dim = c(number_of_AMP_subjects,length(amp_colnames)));
colnames(amp_summary_stats) <- amp_colnames
amp_summary_stats = as.data.frame(amp_summary_stats)

for (s in 1:number_of_AMP_subjects){
  sID = subject_IDs[s];
  amp_summary_stats$subjectID[s] = sID;
  
  tmpdata = bst_amp[bst_amp$subjectID == sID,];
  
  amp_summary_stats$judgments_mean_overall[s] = mean(tmpdata$unPleasant0_Pleasant1);
  amp_summary_stats$judgments_mean_white[s] = mean(tmpdata$unPleasant0_Pleasant1[tmpdata$stimulusRace_0w_1b_2o == 0]);
  amp_summary_stats$judgments_mean_black[s] = mean(tmpdata$unPleasant0_Pleasant1[tmpdata$stimulusRace_0w_1b_2o == 1]);
  amp_summary_stats$judgments_mean_other[s] = mean(tmpdata$unPleasant0_Pleasant1[tmpdata$stimulusRace_0w_1b_2o == 2]);
  
  amp_summary_stats$responseTime_mean_overall[s] = mean(tmpdata$responseTime);
  amp_summary_stats$responseTime_mean_white[s] = mean(tmpdata$responseTime[tmpdata$stimulusRace_0w_1b_2o == 0]);
  amp_summary_stats$responseTime_mean_black[s] = mean(tmpdata$responseTime[tmpdata$stimulusRace_0w_1b_2o == 1]);
  amp_summary_stats$responseTime_mean_other[s] = mean(tmpdata$responseTime[tmpdata$stimulusRace_0w_1b_2o == 2]);
  
  amp_summary_stats$RTs_outside_bounds[s] = sum(tmpdata$responseTime < lower_RT_bound) + sum(tmpdata$responseTime > upper_RT_bound)
}

# SUMMARY:
# BST012, 030, and 035 have meaningful numbers of trials that are either excessively fast or slow (esp. 030 and 035).
# Judgments don't appear to be singular (i.e. all one response or one button).

bst_amp$unPleasant0_Pleasant1[bst_amp$responseTime < lower_RT_bound] = NA
bst_amp$unPleasant0_Pleasant1[bst_amp$responseTime > upper_RT_bound] = NA

bst_amp$responseTime[bst_amp$responseTime < lower_RT_bound] = NA
bst_amp$responseTime[bst_amp$responseTime > upper_RT_bound] = NA


amp_rts_byRaceResp = array(data = NA, dim = c(6,3));

for (race_category in 0:2){
  for (response_type in 0:1){
    subj_level_averages = array(data = NA, dim = c(number_of_AMP_subjects,1));
    for (subj in 1:number_of_AMP_subjects){
      tmp_index = (bst_amp$stimulusRace_0w_1b_2o == race_category) &
        (bst_amp$unPleasant0_Pleasant1 == response_type) &
        (bst_amp$subjectID == subject_IDs[subj]);
      
      subj_level_averages[subj] = mean(bst_amp$responseTime[tmp_index], na.rm = T) # NOTE: doing this with MEDIAN produces very similar pattern (maybe less diff. btwn pleas/unpleas for other?)
    }
    amp_rts_byRaceResp[race_category*2 + response_type+1,1] = mean(subj_level_averages);
    amp_rts_byRaceResp[race_category*2 + response_type+1,2] = race_category;
    amp_rts_byRaceResp[race_category*2 + response_type+1,3] = response_type;
  }
}

colnames(amp_rts_byRaceResp) = c('responseTime', 'raceCategory', 'responseType')
amp_rts_byRaceResp = as.data.frame(amp_rts_byRaceResp)
amp_rts_byRaceResp$raceCategory = as.factor(amp_rts_byRaceResp$raceCategory)
amp_rts_byRaceResp$responseType = as.factor(amp_rts_byRaceResp$responseType)


#Response Times by stimulus race & unPleasantness Ratings
ggplot(amp_rts_byRaceResp, aes(x = raceCategory, y = responseTime, fill = responseType, colour = responseType)) +
  labs(x="Stimulus Race", y="Response Time", fill = "unPleasnantness Rating") +
  scale_fill_discrete(labels = c("unPleasant", "Pleasant")) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Response Time by Stimulus Race & unPleasantness Rating") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 0, w; 1, b; 3, oth
# mean RTs are slower for unpleasant vs. pleasant judgments in general. Unclear from the graph
# if there would be any effects of stimulus race or interactions with stimulus race.


bst_amp$responseType = bst_amp$unPleasant0_Pleasant1*2-1; # +1 = pleasant, -1 = unpleasant

bst_amp$isblack = (bst_amp$stimulusRace_0w_1b_2o == 1)*1;
bst_amp$isother = (bst_amp$stimulusRace_0w_1b_2o == 2)*1;

bst_amp$sqrtRT = sqrt(bst_amp$responseTime)

rt_model = lmer(sqrtRT ~ 1 + responseType*isblack + responseType*isother + (1 | subjectID) , data = bst_amp);
summary(rt_model)

# strong main effect of judgment type (pleasant is faster than unpleasant), no main effects of stimulus race
# or interactions btwn stimulus race & judgment type (pleasant or unpleasant).


# Add acute & chronic stress indicators
bst_amp$pss = NA;
bst_amp$amponly_stressedBool = 0; # set the default stress bool to NOT STRESSED/CONTROL
# IMPORTANT: THIS ONLY APPLIES TO AMP ANALYSIS, AND NOT OTHER TASKS WHICH DIDN'T HAVE PRE/POST

for (s in 1:number_of_AMP_subjects){
  amp_SID = subject_IDs[s];
  for (subj_PSS in 1:length(bst_pss$subjectID)){
    if (amp_SID == bst_pss$subjectID[subj_PSS]){
      bst_amp$pss[bst_amp$subjectID == amp_SID] = bst_pss$pssSum[subj_PSS];
    }
  }
  for (day in 1:2){
    tmp_index = (bst_amp$subjectID == amp_SID) & (bst_amp$day == day) & (bst_amp$amp1_amp2 == 2);
    bst_amp$amponly_stressedBool[tmp_index] = abs(bst_bath$day2StressedBool[s]-(-day+2))
  }
}

rt_model_stress = lmer(sqrtRT ~ 1 + responseType*isblack*amponly_stressedBool + responseType*isother*amponly_stressedBool + (1 | subjectID) , data = bst_amp);
summary(rt_model_stress)

# Strong main effects of judgment type (pleasant is faster) and stressed indicating that BST responses after the stressor are
# uniquely faster than all other AMPs (on control day, or on stress day BEFORE the stressor).
#
# Difference is 120ms FASTER after stressor. No diff. by stim type or response type.

bst_amp$day_recode = bst_amp$day*2-3; # Day 1 = -1, Day 2 = +1
bst_amp$amp1_amp2_recode = bst_amp$amp1_amp2*2-3; # AMP1 = -1, AMP 2 = +1

rt_model_day_ampNum = lmer(sqrtRT ~ 1 + responseType*day_recode + responseType*amp1_amp2_recode + (1 | subjectID) , data = bst_amp);
summary(rt_model_day_ampNum)
# STRONG effects of...
#   - response type (faster for pleasant)
#   - day (faster on day 2)
#   - AMP number (faster on AMP 2 on each day)
#   - 2-way interactions:
#     - less difference between pleasant/unpleasant on day 2
#     - MORE difference between pleasant/unpleasant on AMP #2.
#
# TAKEAWAY: there are potentially strong effects of practice (across days and within day across measurements)

rt_model_day_ampNum_stress = lmer(sqrtRT ~ 1 + responseType*day_recode*amponly_stressedBool + responseType*amp1_amp2_recode + (1 | subjectID) , data = bst_amp);
summary(rt_model_day_ampNum_stress)
# STRONG effects of...
#   - response type (faster for pleasant)
#   - day (faster on day 2)
#   - AMP number (faster on AMP 2 on each day)
#   - Stress (faster under stress)            <----- stress effect survives day & AMP # regressors!
#   - 2-way interactions:
#     - less difference between pleasant/unpleasant on day 2
#     - stress effect stronger on day 1 (vs. day 2)
#
# TAKEAWAY: there appears to be an overall effect of stress on reaction times ABOVE & BEYOND the
# main effects of day & measurement number.

rt_model_day_ampNum_stress_race = lmer(sqrtRT ~ 1 +
                                         responseType*day_recode*amponly_stressedBool*isblack +
                                         responseType*day_recode*amponly_stressedBool*isother +
                                         responseType*amp1_amp2_recode*isblack +
                                         responseType*amp1_amp2_recode*isother +
                                         (1 | subjectID) , data = bst_amp);
summary(rt_model_day_ampNum_stress_race)
# Findings are identical to the same model WITHOUT race (isother & isblack don't seem to add anything!)
# This model is WORSE
anova(rt_model_day_ampNum_stress_race, rt_model_day_ampNum_stress) # p = 0.73 for more complex model
AIC(rt_model_day_ampNum_stress_race) # AIC -5589
AIC(rt_model_day_ampNum_stress)      # AIC -5791 <-- MUCH better

# Calculate AMP Scores based on the judgments
amp_scores_colnames = c('subjectID',
                        'amp_overall',
                        'amp_d1_s1',
                        'amp_d1_s2',
                        'amp_d2_s1',
                        'amp_d2_s2',
                        'change_amp_stress',
                        'change_amp_control');

amp_scores = array(data = NA, dim = c(number_of_AMP_subjects,length(amp_scores_colnames)));
colnames(amp_scores) <- amp_scores_colnames
amp_scores = as.data.frame(amp_scores)

for (s in 1:number_of_AMP_subjects){
  sID = subject_IDs[s];
  amp_scores$subjectID[s] = sID;
  
  tmpdata = bst_amp[bst_amp$subjectID == sID,];
  
  amp_scores$amp_overall[s] = mean(tmpdata$unPleasant0_Pleasant1[tmpdata$stimulusRace_0w_1b_2o == 0], na.rm = T) -
    mean(tmpdata$unPleasant0_Pleasant1[tmpdata$stimulusRace_0w_1b_2o == 1], na.rm = T);
  
  amp_scores$amp_d1_s1[s] = mean(tmpdata$unPleasant0_Pleasant1[(tmpdata$stimulusRace_0w_1b_2o == 0) & (tmpdata$day == 1) & (tmpdata$amp1_amp2 == 1)], na.rm = T) -
    mean(tmpdata$unPleasant0_Pleasant1[(tmpdata$stimulusRace_0w_1b_2o == 1) & (tmpdata$day == 1) & (tmpdata$amp1_amp2 == 1)], na.rm = T);
  amp_scores$amp_d1_s2[s] = mean(tmpdata$unPleasant0_Pleasant1[(tmpdata$stimulusRace_0w_1b_2o == 0) & (tmpdata$day == 1) & (tmpdata$amp1_amp2 == 2)], na.rm = T) -
    mean(tmpdata$unPleasant0_Pleasant1[(tmpdata$stimulusRace_0w_1b_2o == 1) & (tmpdata$day == 1) & (tmpdata$amp1_amp2 == 2)], na.rm = T);
  amp_scores$amp_d2_s1[s] = mean(tmpdata$unPleasant0_Pleasant1[(tmpdata$stimulusRace_0w_1b_2o == 0) & (tmpdata$day == 2) & (tmpdata$amp1_amp2 == 1)], na.rm = T) -
    mean(tmpdata$unPleasant0_Pleasant1[(tmpdata$stimulusRace_0w_1b_2o == 1) & (tmpdata$day == 2) & (tmpdata$amp1_amp2 == 1)], na.rm = T);
  amp_scores$amp_d2_s2[s] = mean(tmpdata$unPleasant0_Pleasant1[(tmpdata$stimulusRace_0w_1b_2o == 0) & (tmpdata$day == 2) & (tmpdata$amp1_amp2 == 2)], na.rm = T) -
    mean(tmpdata$unPleasant0_Pleasant1[(tmpdata$stimulusRace_0w_1b_2o == 1) & (tmpdata$day == 2) & (tmpdata$amp1_amp2 == 2)], na.rm = T);
  
  # Calculate change in AMP on a per-day basis
  # AMP 2 - AMP 1
  # Positive values = bias increased
  # Negative values = bias decreased
  
  #Creates participants' AMP change by session 1 and 2 AND by acute stress/control conditions
  if (bst_bath$day2StressedBool[s] == 0) { # If they are stressed on day 1, control day 2
    amp_scores$change_amp_stress[s] = amp_scores$amp_d1_s2[s] - amp_scores$amp_d1_s1[s]; # AMP num. 2 - AMP num. 1, if stress day 1
    amp_scores$change_amp_control[s] = amp_scores$amp_d2_s2[s] - amp_scores$amp_d2_s1[s]; # control
  } else { # control on day 1, stress on day 2
    amp_scores$change_amp_control[s] = amp_scores$amp_d1_s2[s] - amp_scores$amp_d1_s1[s];  #control
    amp_scores$change_amp_stress[s] = amp_scores$amp_d2_s2[s] - amp_scores$amp_d2_s1[s]; # AMP num. 2 - AMP num. 1, if stress day 2
  }
}

#sets up column in amp_scores with pss_sum (matching subjID) and adds NA when missing pss_sum values
amp_scores$pss = NA;

for (s in 1:number_of_AMP_subjects){
  amp_SID = amp_scores$subjectID[s];
  for (subj_PSS in 1:length(bst_pss$subjectID)){
    if (amp_SID == bst_pss$subjectID[subj_PSS]){
      amp_scores$pss[s] = bst_pss$pssSum[subj_PSS];
    }
  }
}

# STOPPED HERE on 2/14/24
# TO-DO:

# 1. Did stress increase AMP scores?

#CPT/Control by frequency of AMP unpleasant (0)/pleasant (1) rating
prop.table(table(bst_amp_bath$day2StressedBool, bst_amp_bath$unPleasant0_Pleasant1),1)*100

#                unPleasant0_Pleasant1
#     day2StressedBool      0     1
#           Control 0   42.42% 57.55%
#               CPT 1   41.64% 58.36%
# participants rated AMP stimuli pleasant more often than unpleasant for both control and experimental (CPT) conditions

#xtabs(~day2StressedBool + unPleasant0_Pleasant1, data = bst_amp_bath)

hist(amp_scores$change_amp_stress)
hist(amp_scores$change_amp_control)
#distribution of amp scores more normal under control condition, possible individual differences?

#basic modelling of ACUTE stress vs Control conditions and AMP scores
summary(amp_scores)
sd(amp_scores$change_amp_control)
sd(amp_scores$change_amp_stress)
#mean (sd) change in AMP under stress condition = 0.0127 (0.156)
#mean (sd) change in AMP under control condition = -0.0049 (0.144)
#NOTE: large standard deviations

hist(amp_scores$change_amp_stress)
t.test(amp_scores$change_amp_stress)
wilcox.test(amp_scores$change_amp_stress)
# Scores do not significantly increase under stress t(38) = 0.51, p = 0.61, even if they NUMERICALLY do

t.test(amp_scores$change_amp_stress, amp_scores$change_amp_control, paired = T)
# There is also NOT a larger change under stress than under control t(38) = 0.52, p = 0.61
plot(amp_scores$change_amp_stress, amp_scores$change_amp_control, xlim = c(-1, 1), ylim = c(-1, 1))
lines(x = c(-1, 1), y = c(-1,1), col = 'red')

# ANSWER:
# AMP score was not significantly effected by presence of acute stressor (either absolutely or in comparison to control)



#basic modelling of CHRONIC stress and AMP scores
summary(bst_amp_bath_pss)

plot(amp_scores$pss,amp_scores$amp_overall) # one person is waaaaay out there with a low/neg. AMP score
cor.test(amp_scores$pss,amp_scores$amp_overall)
cor.test(amp_scores$pss,amp_scores$amp_overall, method = 'spearman')
# No relationship between overall AMP scores (collapsing across the 4 measurements) and Chronic stress (PSS scores)
# Same in parametric & non-parametric

plot(amp_scores$pss,amp_scores$change_amp_stress)
cor.test(amp_scores$pss,amp_scores$change_amp_stress)
# Also no relationship between chronic stress and CHANGE in AMP under acute stress

# ANSWER:
# AMP score was not significantly related to chronic stress levels


## OVERALL TAKEAWAY: ##
# AMP is not affected by acute stress, and levels of AMP or changes of AMP under acute stress are unrelated to chronic stress.


# 2. Did stress change AMP scores more than control did?

t.test(amp_scores$change_amp_stress, amp_scores$change_amp_control, paired = T)
# ANSWER
# NO. Change is numerically positive under stress and negative under control, but not diff. from each other.


# 3. Are changes in AMP scores within-day correlated?

cor.test(amp_scores$change_amp_stress, amp_scores$change_amp_control)
# No, changes under stress are not correlated with changes under control


plot(amp_scores$amp_d1_s1, amp_scores$amp_d1_s2, xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.6)) # there are a few outlier (?) scores so try non-parametric too
lines(x = c(-0.6, 0.6), y = c(-0.6, 0.6), col = 'red')
cor.test(amp_scores$amp_d1_s1, amp_scores$amp_d1_s2, method = 'pearson') # p = 0.60
cor.test(amp_scores$amp_d1_s1, amp_scores$amp_d1_s2, method = 'spearman') # p = 0.19
#Results: No, AMP scores are not significantly correlated across measurements on Day 1


plot(amp_scores$amp_d2_s1, amp_scores$amp_d2_s2, xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.6)) # there's a real outlier score so try non-parametric too
lines(x = c(-0.6, 0.6), y = c(-0.6, 0.6), col = 'red')
cor.test(amp_scores$amp_d2_s1, amp_scores$amp_d2_s2, method = 'pearson') # p = 0.0008
cor.test(amp_scores$amp_d2_s1, amp_scores$amp_d2_s2, method = 'spearman') # p = 0.02
#Results: Yes, AMP scores ARE significantly correlated across measurements on Day 2


# Are the initial measurements of AMP scores correlated across days
plot(amp_scores$amp_d1_s1, amp_scores$amp_d2_s1, xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.6)) # there's a real outlier score so try non-parametric too
lines(x = c(-0.6, 0.6), y = c(-0.6, 0.6), col = 'red')
cor.test(amp_scores$amp_d1_s1, amp_scores$amp_d2_s1, method = 'pearson') # p = 0.0008
cor.test(amp_scores$amp_d1_s1, amp_scores$amp_d2_s1, method = 'spearman') # p = 0.02

# ANSWER - Yes, Initial AMP scores are correlated with each other on Day 1 and Day 2


# 4. How did AMP scores change (or not) across days & measurements? (e.g., D1S1 vs. D2S1, all S2s vs. all S1s, all D2s vs. all D1s...)
summary(amp_scores)
#mean d1_s1  Day 1_AMP 1 mean =  0.00225
#mean d1_s2  Day 1_AMP 2 mean =  0.01076
#mean d2_s1  Day 2_AMP 1 mean = -0.02473
#mean d2_s2  Day 2_AMP 2 mean = -0.02553

#mean AMP change for stress =   0.01270 # Bias numerically increases under stress, but not sig.
#mean AMP change for control = -0.00499 # Bias does not change much on the control day (may decrease numerically?)

t.test(amp_scores$change_amp_stress, amp_scores$change_amp_control, paired = T) #not sig diff
t.test(amp_scores$amp_d1_s1, amp_scores$amp_d1_s2, paired = T)  #Day 1, not sig diff
t.test(amp_scores$amp_d2_s1, amp_scores$amp_d2_s2, paired = T)  #Day 2, not sig diff
t.test(amp_scores$change_amp_control) # n.s.
t.test(amp_scores$change_amp_stress) # n.s.


#ANSWER: AMP scores do not significantly change at the group level between measurements on Day 1, on Day 2,
# on the control day, or on the stress day.



# 5. Re-organize AMP scores into a long (not wide) matrix, with columns indicating day 1 or 2, measurement 1 or 2, control or stress, etc.
#   to facilitate regression on *scores* should we wish to do that (not just regression on choices).


## IAT ########################################

#IAT descriptives
summary(bst_iat)
str(bst_iat)
dim(bst_iat) #15600 x 18

#Examine Response Times
mean(bst_iat$RT) #0.647
sd(bst_iat$RT) #0.320 Somewhat high compared to mean
max(bst_iat$RT)  #10.65
min(bst_iat$RT)  #0.20

#distribution on low-end quantile for reaction times
quantile(bst_iat$RT, c(.0001,.05,.125,.5,.875,.95,.9999))
hist(bst_iat$RT, xlim = c(0, 3), breaks = 100) #normal distribution along mean

prop.table(table(bst_iat$cattype, bst_iat$corrans),1)*100
#              Black Pleasant Unpleasant White
#CONGRUENT    25.0     25.0       25.0  25.0
#INCONGRUENT  25.0     25.0       25.0  25.0
#SINGLE       12.5     37.5       37.5  12.5
# even trial bins for congruent/incongruint stimuli (words/images)

#Factoring for use in ggplot
bst_iat$stimulus.F <- factor(bst_iat$stimulus)
bst_iat$corrans.F <- factor(bst_iat$corrans)

#Response Times by stimulus & correct answers
ggplot(bst_iat, aes(x = factor(corrans.F), y = RT, fill = stimulus.F, colour = stimulus.F)) +
  labs(x="Correct Answer", y="Response Time", fill = "Stimulus") +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Response Time by Stimulus Type & Correct Answer") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# When responding correctly to black stimuli, RTs are longer than when correctly responding to white stimuli
# When responding correctly to pleasant vs unpleasant words, RTs seem fairly equally dispersed.

#NOTE: Need to categorize and code all pos stim vs neg stim (words), all black faces, all white faces

# EXPLICIT Bias Measures ######################

## MRS ########################################
#Modern Racism Scale

#MRS descriptives
summary(bst_mrs)
#NOTE: mrsSum from -14 (less bias) to 14 (more bias)

#Examine Sums
mean(bst_mrs$mrsSum) #-11.10 (across participants, results leaning heavily towards less explicit bias)
sd(bst_mrs$mrsSum) #3.38
max(bst_mrs$mrsSum)  #-3
min(bst_mrs$mrsSum)  #-14 (lowest possible on the MRS scale reached)

hist(bst_mrs$mrsSum, breaks = 10) #clear pos skew

bst_mrs %>%
  dplyr::select(subjectID,mrsSum) %>%
  head(39) %>%
  gt()

## SRS ########################################
#Symbolic Racism Scale

#SRS descriptives
summary(bst_srs)
#NOTE: srsSum from 8 (less bias) to 31 (more bias)

#Examine Sums
mean(bst_srs$srsSum) #11.97 (across participants, results leaning heavily towards less explicit bias)
sd(bst_srs$srsSum) #3.26
max(bst_srs$srsSum)  #21
min(bst_srs$srsSum)  #8 (lowest possible on the SRS scale reached)

hist(bst_srs$srsSum, breaks = 10) #clear pos skew, but wider distribution than MRS

#library(gt)
#library(gtExtras)
bst_srs %>%
  dplyr::select(subjectID,srsSum) %>%
  head(50) %>%
  gt()

## IMS-EMS ########################################
# Internal & External Motivations

#IMS-EMS descriptives
summary(bst_ims_ems)
#NEG indicates more internally motivated to be less biased, POS score indicates more externally motivated to be less biased

#Examine Sums
mean(bst_ims_ems$EmsImsDiff) #-13.18 (across participants, results leaning heavily towards less explicit bias)
sd(bst_ims_ems$EmsImsDiff) #9.66
max(bst_ims_ems$EmsImsDiff)  #5
min(bst_ims_ems$EmsImsDiff)  #-33
#Mean = -13.18 (Range 5 to -33)

hist(bst_ims_ems$EmsImsDiff, breaks = 10) #neg skew
#participants overall are more internally motivated than externally motivated, but there is a wide range of distribution within negative scores

## Additional ########################################
# Explicit Interaction Analyses

#correlation btwn srs and mrs
cor.test(bst_srs$srsSum, bst_mrs$mrsSum, method = 'pearson')
#SMS & MRS significantly and strongly correlated r=.75, p < .001

#NOTE - can't cor.test EMS-IMS data against mrs and ems, b/c one participant missing from EMS-IMS data
#cor.test(bst_mrs$mrsSum, bst_ims_ems$EmsImsDiff, method = 'pearson')

# SUMMARY OF EXPLICIT BASIC DESCRIPTIVES
# Across participants, MRS and SRS sums leaned heavily towards a low explicit bias score
# SRS scores were more widely distributed than MRS scores
# As expected, SRS and MRS highly correlated


# CONTACT MEASURES ######################

bst_cm %>%
  dplyr::select(subjectID, Q10_PerCloseWhite:Q17_PerMediaBlack) %>%
  head(39) %>%
  gt()

#CM descriptives
summary(bst_cm)

# % of white/black contact analysis (items Q10-Q17)
# results indicate participants overall had a higher percentage of white contacts than black contacts.
# this effect of more contacts for whites than blacks held for:
#   "close" relationships - White (M=63.56); Black (M=12.82)
#   "acquaintances" - White (M=62.94); Black (M=17.10)
#   "daily" contacts - White (M=78.51); Black (M=10.73)
#   "media" contacts - White (M=70.08); Black (M=20.81)
# Overall, participants' black contacts were highest for "media" contacts and "aquiantances"

bst_cm %>%
  dplyr::select(subjectID,EmsImsDiff) %>%
  head(39) %>%
  gt()

hist(bst_cm$Q1_close_white_recode,breaks = 5)
hist(bst_cm$Q4_close_black_recode, breaks = 5) #very positively skewed

hist(bst_cm$Q2_aquaint_white_recode, breaks = 5)
hist(bst_cm$Q5_aquaint_black_recode, breaks = 5) #not as skewed as close contacts

hist(bst_cm$Q3_dated_white_recode, breaks = 5)
hist(bst_cm$Q6_dated_black_recode, breaks = 5)

hist(bst_cm$Q7_environ_USR_recode, breaks = 3) #0=urban, 1=suburban, 2=rural
hist(bst_cm$Q8_environ_race_diverse_recode, breaks = 2)
hist(bst_cm$Q9_envir_cult_diverse_recode, breaks = 2)


# MODELS ######################

#logistic regression to see if unpleasant/pleas ratings is affected by stim race and by AMP 1-AMP 2
#AMP 1 is participant baseline AMP, AMP 2 is participant experiment/control AMP

amp_mod1 <- lmer(responseTime ~ 1 + stimulusRace_0w_1b_2o * amp1_amp2 + ( 1 | subjectID), data = bst_amp)
summary(amp_mod1)
#stim race alone does not have an effect on response time, however there is an effect of AMP 1 vs 2
#Response times for baseline AMP and AMP after experimental task are sig. different

amp_mod2 <- lmer(responseTime ~ 1 + stimulusRace_0w_1b_2o + amp1_amp2 + ( 1 | subjectID), data = bst_amp)
summary(amp_mod2)
#simpler model, same results as amp_mod1 which included interaction effects

amp_mod3 <- glm(unPleasant0_Pleasant1 ~ stimulusRace_0w_1b_2o + amp1_amp2, data = bst_amp)
summary(amp_mod3)
#when controlling for stimulus race, amp1/amp2 significantly related to unpleasant/pleasant AMP ratings.

#To-DO: Check AMP 1 vs 2 by whether or not they had control or exper on day 1! Then compare with stimulus type.

prop.table(table(bst_amp$amp1_amp2, bst_amp$unPleasant0_Pleasant1),1)*100
#                unPleasant0_Pleasant1
#                           0     1
#           AMP_1     43.79% 56.21%
#           AMP_2     40.43% 59.57%
#Collapsed across stimuli races, there were more pleasant ratings than unpleasant ratings for both AMP1 and AMP2




