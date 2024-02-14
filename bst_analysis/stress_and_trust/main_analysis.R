
#loads up the dataframes from setupData, also loads the plyr library (note this in case you use dply)
#NOTE: this assumes that you are connected to the shlab drive, and it is attached under /Volumes
#if this is not the case, add your own configuration in the config.yml
config <- config::get()

setup_path <- file.path(config$path$code$r_scripts, config$code_files$setup_data)
source(setup_path)

library(ggplot2)
library(lme4)
library(nlme)
library(lmerTest) # adds more useful info to the output of lmer's

options(scipen=999)  #EB NOTE: sci notation to decimal


#Base Level Analysis#

#### STRESS ####

#Stress:
  #Acute
    #descriptives
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

  #Chronic
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

    #effect of acute stressor n chronic stressor
        #t-test on if there was a difference in PSS scores based on what type of bath was received on day 2 (the PSS was only administered on day 2)
        t.test(bst_bath_pss$pssSum ~ bst_bath_pss$day2StressedBool)
        #Key results (p = .94, t=-.075) received cold bath mean = 16, didn't received lukewarm bath mean = 15.86
        # the PSS scores are not significantly affected by the presence of the cold water bath on day 2

        head(bst_bath_pss)

#### TRUST ####

#Trust:
  #Trust Rating
    #descriptives
      # mean(bst_tr$rating) # PSH NOTE: if everyone has the same # of trials, this is OK; if they don't, this is biased toward people with more trials
      mean(aggregate(rating ~ subjectID, data = bst_tr, FUN=mean)$rating)
      sd(bst_tr$rating) # Not a particularly useful quantity b/c it's collapsing across people
      mean(aggregate(rating ~ subjectID, data = bst_tr, FUN=sd)$rating)

      #prelim looking at different effect
      # PSH NOTE: same problem below as a few lines above; this collapses across people.
      # OK for preliminary look, but not OK beyond that.
        #underwent stressor or not
          by(data = bst_tr$rating, INDICES = bst_tr$stressedBool, FUN = mean)
          by(data = bst_tr$rating, INDICES = bst_tr$stressedBool, FUN = sd)

          # PSH NOTE: A 'correct' approach: do the means per subj & condition first
          tmpvect = aggregate(rating ~ subjectID + stressedBool, data = bst_tr, FUN=mean);
          # THEN do the means within condition
          by(data = tmpvect$rating, INDICES = tmpvect$stressedBool, FUN = mean) # control = 4.74, stressed = 4.67
        #day
          by(data = bst_tr$rating, INDICES = bst_tr$day, FUN = mean)
          by(data = bst_tr$rating, INDICES = bst_tr$day, FUN = sd)

          tmpvect = aggregate(rating ~ subjectID + day, data = bst_tr, FUN=mean);
          by(data = tmpvect$rating, INDICES = tmpvect$day, FUN = mean) # day 1 = 4.73, day 2 = 4.68
        #task order
          by(data = bst_tr$rating, INDICES = bst_tr$taskOrder, FUN = mean)
          by(data = bst_tr$rating, INDICES = bst_tr$taskOrder, FUN = sd)

          tmpvect = aggregate(rating ~ subjectID + taskOrder, data = bst_tr, FUN=mean);
          by(data = tmpvect$rating, INDICES = tmpvect$taskOrder, FUN = mean) # order 1 = 4.69, order 2 = 4.84

        #what day they got the stressor on (0 means they got on day 1, 1 means they got it on day 2)
          by(data = bst_tr$rating, INDICES = bst_tr$day2StressedBool, FUN = mean)
          by(data = bst_tr$rating, INDICES = bst_tr$day2StressedBool, FUN = sd)
          # PSH NOTE: I've been thinking about this, not sure what it means exactly.
          # This averages across all of a participant's data (day 1 & day 2)...
          # and technically it's the interaction of stressedBool & taskOrder
        #task order had a means difference of ~.3, maybe something there (sd was about 1.85)
        #day of stressor also has something there (maybe) with a means difference of .5 and a difference of ~.28 in the sd as well
        #following up on whether these possible confounds had any effect task order
          #note I include subjectID as a random effect, because without it, task order becomes close to a proxy for individual differences
          tr_confound_mod <- lme(rating ~ 1 + taskOrder + day + day2StressedBool, random = ~ 1 | subjectID, data = bst_tr_pss)
          summary(tr_confound_mod)
          #results: while none of these were technically significant, task order was trending (p=.058). That said the effect size was pretty small (~-.08)

          # PSH NOTE: Love the regression; the predictors aren't well set up for a regression situation though.
          bst_tr$taskOrderrecode = bst_tr$taskOrder * 2 - 3; # Converts 1, 2 coding into -1/+1
          bst_tr$dayrecode = bst_tr$day * 2 - 3; # Converts 1, 2 coding into -1/+1 coding
          bst_tr$stressrecode = bst_tr$stressedBool*2-1 # converts 0,1 coding into -1/+1 coding
            # PSH follow-up: Not sure this last one was actually all that helpful.

          # Below regression only includes day & stress. The logic is that many of these
          # regressors are too closely aligned (task order is the interaction of day & stress,
          # for example), so while it's good to check these things, focusing on regressors of
          # stress, day, and their interaction is the way to go
          tr_confound_mod <- lmer(rating ~ 1 + dayrecode * stressedBool +
                                    ( 1 | subjectID), data = bst_tr)
          summary(tr_confound_mod)
          #                          Estimate Std. Error         df t value Pr(>|t|)
          # (Intercept)             4.751e+00  1.578e-01  3.703e+01  30.102   <2e-16 ***
          # dayrecode              -2.832e-02  1.586e-02  1.029e+04  -1.785   0.0742 .
          # stressrecode           -3.763e-02  1.586e-02  1.029e+04  -2.373   0.0177 *
          # dayrecode:stressrecode  2.608e-01  1.578e-01  3.703e+01   1.653   0.1069             #
          #
          # stress has a significant effect in lowering ratings of trustworthiness
          # Day has a trending effect (lower on day 2), and day X stress is marginal

          #response times
            #descriptives
              mean(bst_tr$responseTime)
              sd(bst_tr$responseTime)
              max(bst_tr$responseTime)
              min(bst_tr$responseTime)
              quantile(bst_tr$responseTime, c(.0001,.05,.125,.5,.875,.95,.9999))

            #plot of the low end
              ggplot(bst_tr, aes(x=responseTime)) +
                geom_histogram() +
                labs(title = "Density of TR Response Time", y = "Density") +
                scale_x_continuous(name = "Response Time (S)", limits = c(0,1)) +
                theme_classic()

            #low times specifically
              bst_tr$responseTimeLow <- ifelse(bst_tr$responseTime < .1, 1, 0)
              count(bst_tr$responseTimeLow)
  #Trust Game
    #descriptives
      # mean(bst_tg$shared) # PSH NOTE: See above w/ TR, same issue, tho here it doesn't matter b/c # of trials is identical across people
      # sd(bst_tg$shared)
      mean(aggregate(shared ~ subjectID, data = bst_tg, FUN=mean)$shared)
      mean(aggregate(shared ~ subjectID, data = bst_tg, FUN=sd)$shared)

      #prelim looking at different effect
        #underwent stressor
          by(data = bst_tg$shared, INDICES = bst_tg$stressedBool, FUN = mean)
          by(data = bst_tg$shared, INDICES = bst_tg$stressedBool, FUN = sd)
        #day
          by(data = bst_tg$shared, INDICES = bst_tg$day, FUN = mean)
          by(data = bst_tg$shared, INDICES = bst_tg$day, FUN = sd)
        #task order
          by(data = bst_tg$shared, INDICES = bst_tg$taskOrder, FUN = mean)
          by(data = bst_tg$shared, INDICES = bst_tg$taskOrder, FUN = sd)
        #what day they got the stressor on (0 means they got on day 1, 1 means they got it on day 2)
          by(data = bst_tg$shared, INDICES = bst_tg$day2StressedBool, FUN = mean)
          by(data = bst_tg$shared, INDICES = bst_tg$day2StressedBool, FUN = sd)
        #day had a means difference of ~.15, but again task order had a difference of ~.45 and a sd difference of ~.16 (all the other sd was ~1.57)
        #day of stressor also had a means difference of ~.22, and a sd difference of ~.13
          #note: like before I include the random intercepts of subject to accoutn for task order being a sort of proxy for that
          tg_confound_mod <- lme(shared ~ 1 + taskOrder + day + day2StressedBool, random = ~ 1 | subjectID, data = bst_tg)
          summary(tg_confound_mod)
          #Results: something interesting here, task order and stressor order weren't significant but day was, with an effect size of ~.14

          bst_tg$taskOrderrecode = bst_tg$taskOrder * 2 - 3; # Converts 1, 2 coding into -1/+1
          bst_tg$dayrecode = bst_tg$day * 2 - 3; # Converts 1, 2 coding into -1/+1 coding
          bst_tg$stressrecode = bst_tg$stressedBool*2-1 # converts 0,1 coding into -1/+1 coding

          tg_confound_mod <- lmer(shared ~ 1 + dayrecode * stressrecode +
                                    ( 1 | subjectID), data = bst_tg)
          summary(tg_confound_mod)
          #                         Estimate Std. Error         df t value Pr(>|t|)
          # (Intercept)             2.506e+00  1.907e-01  3.700e+01  13.140 1.65e-15 ***
          # dayrecode               6.789e-02  1.450e-02  5.887e+03   4.682 2.90e-06 ***
          # stressrecode            9.824e-03  1.450e-02  5.887e+03   0.678    0.498
          # dayrecode:stressrecode -1.109e-01  1.907e-01  3.700e+01  -0.582    0.564
          #
          # Significant effect of day (more shared on Day 2)
          # No effect of stress or interaction w/ day

    #sequential effects
      tg_seq_mod0 <- lm(shared ~ 1 + prevTrialShared, data = bst_tg)
        # prevTrialShared is +1 if they shared, -1 if they did not
      tg_seq_mod1 <- lm(shared ~ 1 + prevTrialFeedback, data = bst_tg)
        # prevTrialFeedback is +1 if reciprocated (they got $ back), -1 if they did not get $ back
      summary(tg_seq_mod0)
        # if they shared on the prev. trial, then shared more $ next trial
      summary(tg_seq_mod1)

      tg_seq_mod2 <- lmer(shared ~ 1 + prevTrialShared + prevTrialFeedback +
                            (1 | subjectID), data = bst_tg)
      summary(tg_seq_mod2)
      #                     Estimate Std. Error         df t value Pr(>|t|)
      # (Intercept)          2.37847    0.17532   38.62218  13.566 2.83e-16 ***
      # prevTrialShared      0.21346    0.02530 5918.56093   8.436  < 2e-16 ***
      # prevTrialFeedback   -0.04699    0.01632 5895.18678  -2.879  0.00401 **
      #
      # Both effects remain in RFX model & alongside each other

        # if they got money back, they shared less the next trial / if they did not get $ back, they shared more the next trial
      #Results: both of these models showed significance for the previous trial, notably people shared ~.32 more following a loss
        # PSH NOTE: This is cool!

      #the next regression looks at whether stress had anything to do with it, so feel free to uncomment the following lines to change the coding of the "stressedBool" variable
      # bst_tg$stressedBool <- as.numeric(bst_tg$stressedBool)
      # bst_tg$stressedBool <- ifelse(bst_tg$stressedBool == 0, -1, 1)
      tg_seq_mod3 <- lm(shared ~ 1 + prevTrialFeedback:stressedBool, data = bst_tg)
        # PSH NOTE: this only includes the interaction & no main effects
      tg_seq_mod3 <- lmer(shared ~ 1 + prevTrialFeedback*stressrecode + (1 | subjectID), data = bst_tg)
      #                                 Estimate Std. Error         df t value Pr(>|t|)
      # (Intercept)                     2.528e+00  1.845e-01  3.797e+01  13.699 2.84e-16 ***
      # prevTrialFeedback              -4.061e-02  1.640e-02  5.893e+03  -2.476   0.0133 *
      # stressrecode                   -2.314e-03  1.431e-02  5.886e+03  -0.162   0.8715
      # prevTrialFeedback:stressrecode  4.233e-03  1.565e-02  5.887e+03   0.270   0.7868
      #
      # Consistent effect of prev. trial feedback; no interaction w/ stress.
      summary(tg_seq_mod3)
      #Results: previous trial feedback was ~.19, so when a participant was stressed, the effect of previous trials is actually smaller?

      tg_seq_mod4 <- lmer(shared ~ 1 + prevTrialShared*stressrecode + prevTrialFeedback*stressrecode +
                            (1 | subjectID), data = bst_tg)
      summary(tg_seq_mod4)
      #                                  Estimate Std. Error         df t value Pr(>|t|)
      # (Intercept)                     2.375e+00  1.750e-01  3.862e+01  13.573 2.79e-16 ***
      # prevTrialShared                 2.182e-01  2.523e-02  5.916e+03   8.649  < 2e-16 ***
      # stressrecode                    9.225e-02  2.025e-02  5.886e+03   4.556 5.31e-06 ***
      # prevTrialFeedback              -4.608e-02  1.627e-02  5.892e+03  -2.833  0.00463 **
      # prevTrialShared:stressrecode   -1.352e-01  2.062e-02  5.888e+03  -6.557 5.97e-11 ***
      # stressrecode:prevTrialFeedback  9.383e-03  1.552e-02  5.885e+03   0.605  0.54540
      #
      # Main effects of Prev events remain
      # Stress increases $ shared
      # Stress REDUCES effect of prev. sharing, but does not interact w/ effects of feedback

    #response times
    #EB - bst_tg - Looking at recodes

      #trust game
        #descriptives
          mean(bst_tg$responseTime)
          sd(bst_tg$responseTime)
          max(bst_tg$responseTime)
          min(bst_tg$responseTime)
          quantile(bst_tg$responseTime, c(.0001,.05,.125,.5,.875,.95,.9999))

        #plot of times under 1s
          ggplot(bst_tg, aes(x=responseTime)) +
          geom_histogram(fill = "white", col = "black") +
          labs(title = "Density of TG Response Time", y = "Density") +
          scale_x_continuous(name = "Response Time (S)", limits = c(0,1)) +
          theme_classic()

        #finding out who had the low times
          by(data <- bst_tg$responseTime, INDICES = bst_tg$subjectID, FUN = mean)
          by(data <- bst_tg$responseTime, INDICES = bst_tg$subjectID, FUN = sd)

        #looking at those low response times
          bst_tg$responseTimeLow <- ifelse(bst_tg$responseTime < .25, 1, 0)
          count(bst_tg$responseTimeLow)


#### BIAS ####

#AMP descriptives
summary(bst_amp)
str(bst_amp)
mean(bst_amp$responseTime)
sd(bst_amp$responseTime)
max(bst_amp$responseTime)  #CHECK: Why Max high?
min(bst_amp$responseTime)

#low-end quantile
quantile(bst_amp$responseTime, c(.0001,.05,.125,.5,.875,.95,.9999))

#plot AMP distribution up to 3 for distribution
ggplot(bst_amp, aes(x=responseTime)) +
  geom_histogram() +
  labs(title = "Density of AMP Response Time", y = "Density") +
  scale_x_continuous(name = "Response Time (S)", limits = c(0,3)) +
  theme_classic()

hist(bst_amp$responseTime, breaks = 50)

#plot distribution on low end
ggplot(bst_amp, aes(x=responseTime)) +
  geom_histogram() +
  labs(title = "Density of AMP Response Time", y = "Density") +
  scale_x_continuous(name = "Response Time (S)", limits = c(0,1)) +
  theme_classic()

#Breakdowns of stimRace by Pleasantness or Unpleasantness Ratings


by(data = bst_amp$unPleasant0_Pleasant1, INDICES = bst_amp$stimulusRace_0w_1b_2o, FUN = mean)
by(data = bst_amp$unPleasant0_Pleasant1, INDICES = bst_amp$stimulusRace_0w_1b_2o, FUN = sd)
#white stim AMP unpleasantness .4947 mean
#black stim AMP unpleasantness .4934 mean
#other stim AMP unpleasantness .4932 mean
#White stimuli were rated slightly more pleasant (.4947) than black stimuli (.4934)
#or than other stimuli (.4932)

#stim race (white,black,other) by frequency of unpleasant (0) vs pleasant (1) rating
xtabs(~stimulusRace_0w_1b_2o + unPleasant0_Pleasant1, data = bst_amp)
# across stimuli (w,b,o), participants rated stimuli pleasant more times than unpleasant

#                   unPleasant0_Pleasant1
#stimulusRace_0w_1b_2o        0    1
#                   0       3332 4468
#                   1       3266 4534
#                   2       3256 4544


#Factoring
bst_amp$unPleasant0_Pleasant1_F <- factor(bst_amp$unPleasant0_Pleasant1)
bst_amp$stimRace_F <- factor(bst_amp$stimulusRace_0w_1b_2o)
bst_amp$amp1amp2_F <- factor(bst_amp$amp1_amp2)

str(bst_amp)

#Response Times by Stim Raxce & unPleasantness Ratings
ggplot(bst_amp, aes(x = factor(stimRace_F), y = responseTime, fill = unPleasant0_Pleasant1_F, colour = unPleasant0_Pleasant1_F)) +
  labs(x="Stimulus Race", y="Response Time", fill = "unPleasnantness Rating") +
  scale_fill_discrete(labels = c("unPleasant", "Pleasant")) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Response Time by Stimulus Race & unPleasantness Rating") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
 # 0, w; 1, b; 3, oth
#For white stimuli, giving unpleasant ratings led to faster response time than when giving Pleasant ratings
#A similar effect was observed for black stimuli, but with the effect more pronounced
#For other stimuli, this effect is reversed, with slower response times for unpleasant ratings vs Pleasant
#white stim unPl RT < Pl RT
#black stim unPl RT < Pl RT (more pronounced difference than with white stim)
#other stim unPl RT > Pl RT

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
  
  if (bst_bath$day2StressedBool[s] == 0) { # If they are stressed on day 1, control day 2
    amp_scores$change_amp_stress[s] = amp_scores$amp_d1_s2[s] - amp_scores$amp_d1_s1[s]; # AMP num. 2 - AMP num. 1
    amp_scores$change_amp_control[s] = amp_scores$amp_d2_s2[s] - amp_scores$amp_d2_s1[s];
  } else { # control on day 1, stress on day 2
    amp_scores$change_amp_control[s] = amp_scores$amp_d1_s2[s] - amp_scores$amp_d1_s1[s];
    amp_scores$change_amp_stress[s] = amp_scores$amp_d2_s2[s] - amp_scores$amp_d2_s1[s];
  }
}

# STOPPED HERE on 2/14/24
# TO-DO:
# 1. Did stress increase AMP scores?
# 2. Did stress change AMP scores more than control did?
# 3. Are changes in AMP scores within-day correlated? 
# 4. How did AMP scores change (or not) across days & measurements? (e.g., D1S1 vs. D2S1, all S2s vs. all S1s, all D2s vs. all D1s...)








#logistic regression to see if unpleasant/pleas ratings is affected by stim race and by AMP 1-AMP 2
#AMP 1 is participant baseline AMP, AMP 2 is participant experiment/control AMP

amp_mod1 <- lmer(responseTime ~ 1 + stimRace_F * amp1amp2_F + ( 1 | subjectID), data = bst_amp)
summary(amp_mod1)
#stim race alone does not appear to have effect on response time, however there does seem to be an effect of AMP 1 vs 2
#the response times for baseline AMP and AMP after experimental task are sig. different

amp_mod2 <- lmer(responseTime ~ 1 + stimRace_F + amp1amp2_F + ( 1 | subjectID), data = bst_amp)
summary(amp_mod2)
#same results as amp_mod1 which included interaction effects
#Seems to be a sig difference in pleasantness/unpleasantness ratings for baseline AMP vs Control AMP

#amp_mod3 <- glm(unPleasant0_Pleasant1_F ~ stimRace_F + amp1amp2_F, data = bst_amp)
#summary(amp_mod3)

# !! Q: Check AMP 1 vs 2 by whether or not they had control or exper on day 1! Then compare with stimulus type.

xtabs(~amp1amp2_F + unPleasant0_Pleasant1, data = bst_amp)
#In general, AMP 1 & 2 have more frequency of pleasant than unpleasant ratings of images
#however, the difference between frequency of pleasant and unpleasant is greater on AMP2
#participants pleasant image rating frequency is higher on day 2 vs day 1

#interaction effects are not sig.
model.2 <- glmer(unPleasant0_Pleasant1_F ~ stimRace_F + amp1amp2_F + stimRace_F:amp1amp2_F + (1|subjectID), data=bst_amp, family="binomial")
summary(model.2)







#### CORRELATIONS ####
#stress
  #pss score and the pleasantness ratings correlations
    #normal
    cor.test(bst_bath_pss$pssSum, bst_bath_pss$diffPleasantnessRating, method = 'pearson')
    cor.test(bst_bath_pss$pssSum, bst_bath_pss$controlPleasantnessRating, method = 'pearson')
    cor.test(bst_bath_pss$pssSum, bst_bath_pss$stressPleasantnessRating, method = 'pearson')
    #Results: no statistically significant correlations

    #as the 3 levels (also not significant)
    cor.test(bst_bath_pss$pssSumCategorical, bst_bath_pss$diffPleasantnessRating, method = 'pearson')

#trust
  #on participant by participant means (TG x TR)
    #correlations:
      #on means
        cor.test(bst_t_compiled_part_avg$sharedAvg, bst_t_compiled_part_avg$ratingAvg, method = 'pearson')
        cor.test(bst_t_compiled_part_avg$sharedAvg, bst_t_compiled_part_avg$ratingAvg, method = 'spearman', exact=FALSE)
        #results: the pearson was not significant but trending (p~.08) and r~.27, the spearman was significant (p~.02) and rho~.36
      #on variances
        cor.test(bst_t_compiled_part_avg$sharedSD, bst_t_compiled_part_avg$ratingSD, method = 'pearson')
        cor.test(bst_t_compiled_part_avg$sharedSD, bst_t_compiled_part_avg$ratingSD, method = 'spearman', exact = FALSE)
        #results: no significant correlation here on standard deviations

    #plot of correlation
      ggplot(data = bst_t_compiled_part_avg, aes(x = sharedAvg, y = ratingAvg)) +
        geom_point(size = 2) +
        scale_x_continuous(name = "Average Amount Shared ($)") +
        scale_y_continuous(name = "Average Trust Rating", limits = c(1,9), breaks = c(1,3,6,9)) +
        geom_smooth(method = lm, se = FALSE) +
        geom_errorbarh(aes(xmin = sharedLowSE, xmax = sharedHighSE)) +
        geom_errorbar(aes(ymin = ratingLowSE, ymax = ratingHighSE)) +
        theme_classic()
      # PSH NOTE: This nicely highlights WHY pearson is weak, but spearman is stronger - there
      # are a few people who shared ~$5 every time who mess it up; w/o them, the relationship
      # is clear and positive.

    #seeing if the correlations changed when separating out by stressed or not condition as well
        #on means
          with(data = (subset(bst_t_compiled_part_avg_stress, stressedBool == 1)), cor.test(sharedAvg, ratingAvg, method = 'pearson')) #stressed
          with(data = (subset(bst_t_compiled_part_avg_stress, stressedBool == 0)), cor.test(sharedAvg, ratingAvg, method = 'pearson')) #not stressed
          #Results, the not stressed was trending (p = .051) r = .31, the stressed was not significant (p~.15), r = .23
          with(data = (subset(bst_t_compiled_part_avg_stress, stressedBool == 1)), cor.test(sharedAvg, ratingAvg, method = 'spearman')) #stressed
          with(data = (subset(bst_t_compiled_part_avg_stress, stressedBool == 0)), cor.test(sharedAvg, ratingAvg, method = 'spearman')) #not stressed
          #Results: both are significant (p = 0.01, p = 0.04; both positive correlations)
        #variances
          with(data = (subset(bst_t_compiled_part_avg_stress, stressedBool == 1)), cor.test(sharedSD, ratingSD, method = 'pearson')) #stressed
          with(data = (subset(bst_t_compiled_part_avg_stress, stressedBool == 0)), cor.test(sharedSD, ratingSD, method = 'pearson')) #not stressed
          #Results: neither here was significant

#stress x trust
  #on participant level means
    #correlations:
      #trust rating
        cor.test(bst_t_compiled_part_avg_pss$ratingAvg, bst_t_compiled_part_avg_pss$pssSum, method = 'pearson')
        cor.test(bst_t_compiled_part_avg_pss$ratingAvg, bst_t_compiled_part_avg_pss$pssSum, method = 'spearman', exact = FALSE)
        #Results: neither was statistically significant

      #trust game
        cor.test(bst_t_compiled_part_avg_pss$sharedAvg, bst_t_compiled_part_avg_pss$pssSum, method = 'pearson')
        cor.test(bst_t_compiled_part_avg_pss$sharedAvg, bst_t_compiled_part_avg_pss$pssSum, method = 'spearman', exact = FALSE)
        #Results: neither here was statistically significant


#Main Main Analysis#
#Trust Rating:
  #basic modeling of stress and trust
    tr_mod_acuteCat_chronicCont <- lme(rating ~ 1 + stressedBool * pssSum, random = ~ 1 | subjectID, data = bst_tr_pss) #categorical stress
    tr_mod_acuteRat_chronciCont <- lme(rating ~ 1 + diffPleasantnessRating * pssSum, random = ~ 1 | subjectID, data = bst_tr_pss) #pleasantness ratings
    tr_mod_acuteCat_chronicCat <- lme(rating ~ 1 + stressedBool * pssSumCategorical, random = ~ 1 | subjectID, data = bst_tr_pss) #categorical PSS scores

    # PSH NOTE: diffPleasantnessRating isn't appropriate to use here. That's a value that's constant for
    # each participant, across both days, etc. If you wanted, the BATH RATING itself could be used here, but
    # that'd have to be rating by day, of course, so it lines up with the trust rating.

    summary(tr_mod_acuteCat_chronicCont)
    # summary(tr_mod_acuteRat_chronciCont)
    summary(tr_mod_acuteCat_chronicCat)
    #Results: both predictors and their interaction were not sifnificant (aside from the intercept),
    #however, using the categorical stress, the interaction of stress and pss score was p~.06, although the effect size was pretty small all things considered (~-.01)
    #Finally, using the categorical pss, the the stressed category and the interaction of the acute and chronic stressor were signficant, with an effect size of -.23 for the interaction
  #including task order because that appeared to maybe have an effect in prelim analysis
    tr_mod_cat1 <- lme(rating ~ 1 + taskOrder + stressedBool * pssSum, random = ~ 1 | subjectID, data = bst_tr_pss)

    summary(tr_mod_cat1)
    #Results: main effect thing to note here was that task order was not signficant (p=.075), howeverthe stressed:pss interaction did move to significance

    bst_tr_pss$dayrecode = bst_tr_pss$day * 2 - 3; # Converts 1, 2 coding into -1/+1 coding
    bst_tr_pss$stressrecode = bst_tr_pss$stressedBool*2-1 # converts 0,1 coding into -1/+1 coding

    tr_reg1 = lmer(rating ~ 1 + stressrecode*dayrecode*pssSum + (1 | subjectID), data = bst_tr_pss)
    summary(tr_reg1)

    #                      Estimate Std. Error         df t value Pr(>|t|)
    # (Intercept)                    5.260e+00  5.994e-01  3.203e+01   8.774 4.99e-10 ***
    # stressrecode                   7.054e-02  5.806e-02  9.836e+03   1.215   0.2244
    # dayrecode                      1.399e-02  5.806e-02  9.836e+03   0.241   0.8097
    # pssSum                        -3.520e-02  3.598e-02  3.202e+01  -0.978   0.3352
    # stressrecode:dayrecode         3.460e-01  5.994e-01  3.203e+01   0.577   0.5678
    # stressrecode:pssSum           -6.554e-03  3.468e-03  9.835e+03  -1.890   0.0588 .
    # dayrecode:pssSum              -2.387e-03  3.468e-03  9.835e+03  -0.688   0.4913
    # stressrecode:dayrecode:pssSum -6.495e-03  3.598e-02  3.202e+01  -0.181   0.8579
    #
    # Stress MAY interact w/ PSS, such that trust ratings are lower for people with high PSS
    # scores in the Stress condition, but no main effects elsewhere. Consistent with a
    # Chronic + Acute is the worst story.
    #
    # FWIW, if you simplify the regression by removing the 3-way interaction and the
    # PSS x Day interaction, the story remains unchanged (trend stress x PSS)
    #
    # FWIW 2, if you do the above regression with the categorical PSS, it's strongly significant,
    # (stress x PSScategorical), suggestive of a possible nonlinearity.

    tr_reg1b = lmer(rating ~ 1 + stressedBool*pssSum + (1 | subjectID), data = bst_tr_pss)
    summary(tr_reg1b)
    # Slightly easier regression to visualize. Numerically identical.

    # Extract fixed effects coefficient values for plotting
    interceptcoef = fixef(tr_reg1b)['(Intercept)']
    stresscoef = fixef(tr_reg1b)['stressedBool'];
    psscoef = fixef(tr_reg1b)['pssSum'];
    stressXpsscoef = fixef(tr_reg1b)['stressedBool:pssSum'];

    PSS = c(0,27); # possible PSS values, focused on the range we observe in this study
    Ctrl = interceptcoef + PSS*psscoef;
    Strs = Ctrl + stresscoef + PSS*stressXpsscoef;
    tr_viz = data.frame(PSS = PSS, Control = Ctrl, Stress = Strs); # assemble data frame for plotting
    ggplot(data = tr_viz, aes(x = PSS)) +
      geom_line(aes(y = Control), color='#111111', size=2) +
      geom_line(aes(y = Stress), color = "#eb5600", size=2) +
      scale_x_continuous(name = "PSS Score", breaks = c(0,5,10,15,20,25,30), limits = c(0,27), expand = c(0,0)) +
      scale_y_continuous(name = "Trust Rating", expand = c(0,0), breaks = seq(1,9,1), limits = c(3.8,5.5)) +
      theme_classic()


    tr_reg2 = lmer(rating ~ 1 + stressrecode*dayrecode*pssSumCategorical + (1 | subjectID), data = bst_tr_pss)
    summary(tr_reg2)
    #                                            Estimate Std. Error         df t value Pr(>|t|)
    # (Intercept)                                 4.84448    0.31086   32.02803  15.584  < 2e-16 ***
    # stressrecode                                0.04216    0.02985 9835.94702   1.413  0.15779
    # dayrecode                                  -0.04966    0.02985 9835.94702  -1.664  0.09620 .
    # pssSumCategorical                          -0.20238    0.35977   32.02072  -0.563  0.57768
    # stressrecode:dayrecode                      0.20649    0.31086   32.02803   0.664  0.51128
    # stressrecode:pssSumCategorical             -0.10544    0.03433 9835.47804  -3.071  0.00214 **
    # dayrecode:pssSumCategorical                 0.03395    0.03433 9835.47804   0.989  0.32275
    # stressrecode:dayrecode:pssSumCategorical    0.04488    0.35977   32.02072   0.125  0.90150
    #
    # Stress x PSS Cat is p = 0.002

    # To follow up on the nonlinearity idea, try an exponentiated version of the PSS
    exponent = 4;
    bst_tr_pss$pssExp = bst_tr_pss$pssSum^exponent / max(bst_tr_pss$pssSum^exponent); # normalized to 0-1
    plot(bst_pss$pssSum,bst_pss$pssSum^exponent / max(bst_pss$pssSum^exponent))

    tr_reg3 = lmer(rating ~ 1 + stressrecode*dayrecode*pssExp + (1 | subjectID), data = bst_tr_pss)
    summary(tr_reg3)
    # This is pretty exploratory; wouldn't trust it too hard. but stress x pssExp is p = 0.0415 with
    # an exponent of 4. So this idea works, but hard to tell what it means.

    tr_reg4 = lmer(rating ~ 1 + stressrecode*pssMedianSplit + (1 | subjectID), data = bst_tr_pss)
    summary(tr_reg4)
    #tried just using the median split, actually nothing was signficant here (other than the intercept)

    anova(tr_reg1,tr_reg2) # reg 2 (categorical) outperforms reg 1
    anova(tr_reg2,tr_reg3) # reg 2 (cat) outperforms reg 3 (exponential)
    anova(tr_reg1,tr_reg3)

    # CATEGORICAL REGRESSION does best here.

    #including previous trial actions in the regressions

    #most basic previous trial analysis
    tr_prev_reg <- lmer(rating ~ 1 + prevTrialRatingAmt + (1 | subjectID), data = bst_tr)
    summary(tr_prev_reg)

    #expanding a bit to hypothezied predictors
    tr_reg1_prev_a <- lmer(rating ~ 1 + prevTrialRating*stressrecode*pssSum + (1 | subjectID), data = bst_tr_pss) #using the boolean version of previous trial ratings
    summary(tr_reg1_prev_a)
                                             # Estimate      Std. Error              df  t value   Pr(>|t|)
    # (Intercept)                          5.07348310e+00  4.41991681e-01  3.38725616e+01 11.47868 3.2084e-13 ***
    # prevTrialRating                      1.83041413e-01  5.32409290e-02  9.86031959e+03  3.43798 0.00058849 ***
    # stressrecode                        -1.70117237e-03  4.89609500e-02  9.83275308e+03 -0.03475 0.97228339
    # pssSum                              -2.77027519e-02  2.61770351e-02  3.38569945e+01 -1.05828 0.29741813
    # prevTrialRating:stressrecode         2.02578000e-01  4.93777826e-02  9.83485756e+03  4.10261 4.1180e-05 ***
    # prevTrialRating:pssSum               3.86053181e-03  3.13631246e-03  9.86081068e+03  1.23091 0.21838428
    # stressrecode:pssSum                 -2.14995138e-03  2.87648302e-03  9.83246583e+03 -0.74742 0.45482576
    # prevTrialRating:stressrecode:pssSum -1.05518246e-02  2.90585313e-03  9.83546310e+03 -3.63123 0.00028351 ***

    #so using the previous trial ratings in boolean form doesn't appear to reallly shift the narrative. In and of itself it is signficant, but interestingly the pss*acute interaction disappears from trending.
    #the effect of the stress interactions is still relatively small

    tr_reg1_prev_b <- lmer(rating ~ 1 + prevTrialRatingAmt*stressrecode*pssSum + (1 | subjectID), data = bst_tr_pss) #using the actual rating on previous trials
    summary(tr_reg1_prev_b)

    #     Fixed effects:
    #                                               Estimate      Std. Error              df  t value   Pr(>|t|)
    # (Intercept)                             4.36247578e+00  4.44704895e-01  4.21503661e+01  9.80982 1.9095e-12 ***
    # prevTrialRatingAmt                      1.52110260e-01  2.85947190e-02  9.86503523e+03  5.31952 1.0632e-07 ***
    # stressrecode                           -4.37780215e-01  1.39916328e-01  9.83549242e+03 -3.12887 0.00175992 **
    # pssSum                                 -2.77409606e-02  2.63010774e-02  4.18943627e+01 -1.05475 0.29758647
    # prevTrialRatingAmt:stressrecode         9.53885569e-02  2.60071991e-02  9.83561283e+03  3.66778 0.00024596 ***
    # prevTrialRatingAmt:pssSum               3.09726928e-04  1.69052630e-03  9.86593550e+03  0.18321 0.85463441
    # stressrecode:pssSum                     2.09606434e-02  8.11548714e-03  9.83597256e+03  2.58280 0.00981465 **
    # prevTrialRatingAmt:stressrecode:pssSum -4.96264955e-03  1.52062943e-03  9.83627202e+03 -3.26355 0.00110402 **

    #things get kinda funky here, the acute*pss intereaction is significant again, the acute is as well, too

    anova(tr_reg1_prev_a, tr_reg1_prev_b) #the second model, not using the boolean value performs better

    #including day effects
    tr_reg2_prev <- lmer(rating ~ 1 + dayrecode*prevTrialRatingAmt*stressrecode*pssSum + (1 | subjectID), data = bst_tr_pss)
    summary(tr_reg2_prev)
                                                            # Estimate      Std. Error              df  t value   Pr(>|t|)
    # (Intercept)                                       4.61502399e+00  5.42795674e-01  4.23783747e+01  8.50232 1.0481e-10 ***
    # dayrecode                                         6.43121135e-01  1.85053904e-01  9.82886718e+03  3.47532 0.00051245 ***
    # prevTrialRatingAmt                                1.24484238e-01  3.74247548e-02  9.85600078e+03  3.32625 0.00088344 ***
    # stressrecode                                     -1.66357093e-02  1.85053904e-01  9.82886716e+03 -0.08990 0.92837125
    # pssSum                                           -3.89314413e-02  3.24134313e-02  4.15436198e+01 -1.20109 0.23651964
    # dayrecode:prevTrialRatingAmt                     -1.22403853e-01  3.29134947e-02  9.82824231e+03 -3.71896 0.00020116 ***
    # dayrecode:stressrecode                            4.79468312e-01  5.42795674e-01  4.23783747e+01  0.88333 0.38204112
    # prevTrialRatingAmt:stressrecode                   1.99588095e-02  3.29134947e-02  9.82824229e+03  0.60640 0.54426184
    # dayrecode:pssSum                                 -3.91898502e-02  1.09417175e-02  9.82900804e+03 -3.58169 0.00034303 ***
    # prevTrialRatingAmt:pssSum                         1.71213897e-03  2.17824084e-03  9.85787181e+03  0.78602 0.43187517
    # stressrecode:pssSum                              -5.00937899e-03  1.09417175e-02  9.82900802e+03 -0.45782 0.64708914
    # dayrecode:prevTrialRatingAmt:stressrecode        -3.50479033e-02  3.74247548e-02  9.85600078e+03 -0.93649 0.34904398
    # dayrecode:prevTrialRatingAmt:pssSum               7.19949224e-03  1.95990522e-03  9.82865156e+03  3.67339 0.00024063 ***
    # dayrecode:stressrecode:pssSum                    -1.43312579e-02  3.24134313e-02  4.15436198e+01 -0.44214 0.66068155
    # prevTrialRatingAmt:stressrecode:pssSum           -3.33694509e-04  1.95990522e-03  9.82865153e+03 -0.17026 0.86480875
    # dayrecode:prevTrialRatingAmt:stressrecode:pssSum  1.62562711e-03  2.17824084e-03  9.85787181e+03  0.74630 0.45550235

    #Lots of stuff going on and not going on, the only significant effects other than previous trial Amt include day, not exactly sure what to make of this

    anova(tr_reg1_prev_b, tr_reg2_prev) #the model including day performs slightly better,

    tr_reg3_prev_a <- lmer(rating ~ 1 + prevTrialRating*stressrecode*pssSumCategorical + (1 | subjectID), data = bst_tr_pss)
    summary(tr_reg3_prev_a)
    #couple things to note here, as before the categorical pss works better in conjunction with acute stress, but the final interactant of all the variables is no longer significant

    tr_reg3_prev_b <- lmer(rating ~ 1 + prevTrialRating*stressrecode*pssMedianSplit + (1 | subjectID), data = bst_tr_pss)
    summary(tr_reg3_prev_b)

                                                      # Estimate      Std. Error              df  t value   Pr(>|t|)
    # (Intercept)                                     3.91863656e+00  1.45460089e-01  4.09740232e+01 26.93960 < 2.22e-16 ***
    # prevTrialRatingAmt                              1.58715072e-01  9.66087369e-03  9.86311775e+03 16.42865 < 2.22e-16 ***
    # stressrecode                                   -1.05405224e-01  4.18182410e-02  9.83687673e+03 -2.52056   0.011733 *
    # pssMedianSplit                                 -2.84614886e-01  1.45460089e-01  4.09740232e+01 -1.95665   0.057227 .
    # prevTrialRatingAmt:stressrecode                 1.58005031e-02  8.42787746e-03  9.83763940e+03  1.87479   0.060851 .
    # prevTrialRatingAmt:pssMedianSplit               2.07712293e-02  9.66087369e-03  9.86311775e+03  2.15004   0.031577 *
    # stressrecode:pssMedianSplit                     1.89745495e-01  4.18182410e-02  9.83687673e+03  4.53739 5.7631e-06 ***
    # prevTrialRatingAmt:stressrecode:pssMedianSplit -3.91583164e-02  8.42787746e-03  9.83763940e+03 -4.64628 3.4236e-06 ***

    #again this is the most exploratory but all terms is significant or trending, there is a lot going on here.
    #note here, that the pattern doesn't change hugely whether you use the rating amount or the rating boolean, but the acute*pss interaction disappears

    tr_reg4_prev <- lmer(rating ~ 1 + dayrecode*prevTrialRatingAmt*stressrecode*pssMedianSplit + (1 | subjectID), data = bst_tr_pss)
    summary(tr_reg4_prev)
    #this is most exploratory, the day stil does figure in though with significant terms though. Beyond that interpretation gets difficult

#Trust Game:
    #basic modelling of stress and trust
      tg_mod_acuteCat_chronicCont <- lme(shared ~ 1 + stressedBool * pssSum, random = ~ 1 | subjectID, data = bst_tg_pss) #categorical stress
      # tg_mod_acuteRat_chronciCont <- lme(shared ~ 1 + diffPleasantnessRating * pssSum, random = ~ 1 | subjectID, data = bst_tg_pss) #pleasantness ratings
      tg_mod_acuteCat_chronicCat <- lme(shared ~ 1 + stressedBool * pssSumCategorical, random = ~ 1 | subjectID, data = bst_tg_pss) #categorical PSS scores

      summary(tg_mod_acuteCat_chronicCont)
      summary(tg_mod_acuteCat_chronicCat)
      #Results: like before the pleasantness ratings did not account for anything significant
      #however, the categorical stressed or not did have some interesting effects, it itself was significant (and an effect of .32), and the interaction with PSS was significant (with an effect of -.25)
      #also the continuous PSS scores were also not significant
  #including day because in the prelim analysis that was significant
      tg_mod_acuteCat_chronicCont_day <- lme(shared ~ 1 + day + stressedBool * pssSum, random = ~ 1 | subjectID, data = bst_tg_pss)

      summary(tg_mod_acuteCat_chronicCont_day)
      #Results: here it was interesting to note that the findings didn't change a ton from the previous model, however day was significant with an effect size of .073

      bst_tg_pss$dayrecode = bst_tg_pss$day * 2 - 3; # Converts 1, 2 coding into -1/+1 coding
      bst_tg_pss$stressrecode = bst_tg_pss$stressedBool*2-1 # converts 0,1 coding into -1/+1 coding

      tg_reg1_withoutday = lmer(shared ~ 1 + stressedBool*pssSum + (1 | subjectID), data = bst_tg_pss)
      summary(tg_reg1_withoutday)
      #                       Estimate Std. Error         df t value Pr(>|t|)
      # (Intercept)          2.355e+00  6.102e-01  3.400e+01   3.860 0.000483 ***
      # stressrecode         1.637e-01  4.432e-02  5.434e+03   3.694 0.000223 ***
      # pssSum               9.938e-03  3.614e-02  3.400e+01   0.275 0.785022
      # stressrecode:pssSum -1.258e-02  2.625e-03  5.434e+03  -4.793 1.69e-06 ***

      # Alternative formulation using `stressedBool` for simplicity
      #                       Estimate Std. Error         df t value Pr(>|t|)
      # (Intercept)            2.19163    0.61183   34.35962   3.582 0.001043 **
      # stressedBool           0.32743    0.08864 5434.00000   3.694 0.000223 ***
      # pssSum                 0.02252    0.03624   34.35962   0.621 0.538430
      # stressedBool:pssSum   -0.02516    0.00525 5434.00000  -4.793 1.69e-06 ***

      # Extract fixed effects coefficient values for plotting
      interceptcoef = fixef(tg_reg1_withoutday)['(Intercept)']
      stresscoef = fixef(tg_reg1_withoutday)['stressedBool'];
      psscoef = fixef(tg_reg1_withoutday)['pssSum'];
      stressXpsscoef = fixef(tg_reg1_withoutday)['stressedBool:pssSum'];

      PSS = c(0,27); # possible PSS values, focused on the range we observe in this study
      Ctrl = interceptcoef + PSS*psscoef;
      Strs = Ctrl + stresscoef + PSS*stressXpsscoef;
      tg_viz = data.frame(PSS = PSS, Control = Ctrl, Stress = Strs); # assemble data frame for plotting
      ggplot(data = tg_viz, aes(x = PSS)) +
        geom_line(aes(y = Control), color='#111111', size=2) +
        geom_line(aes(y = Stress), color = "#eb5600", size=2) +
        scale_x_continuous(name = "PSS Score", breaks = c(0,5,10,15,20,25,30), limits = c(0,27), expand = c(0,0)) +
        scale_y_continuous(name = "Dollars Shared", expand = c(0,0), breaks = seq(0,5,1), limits = c(1.75,3.25)) +
        theme_classic()

      tg_reg1_withday = lmer(shared ~ 1 + stressrecode*dayrecode*pssSum + (1 | subjectID), data = bst_tg_pss)
      summary(tg_reg1_withday)
      #                                 Estimate Std. Error         df t value Pr(>|t|)
      # (Intercept)                    2.711e+00  7.409e-01  3.200e+01   3.659 0.000903 ***
      # stressrecode                   5.362e-02  5.308e-02  5.432e+03   1.010 0.312381
      # dayrecode                     -1.724e-01  5.308e-02  5.432e+03  -3.248 0.001170 **
      # pssSum                        -1.383e-02  4.447e-02  3.200e+01  -0.311 0.757747
      # stressrecode:dayrecode         5.583e-01  7.409e-01  3.200e+01   0.754 0.456609
      # stressrecode:pssSum           -5.218e-03  3.185e-03  5.432e+03  -1.638 0.101440
      # dayrecode:pssSum               1.308e-02  3.185e-03  5.432e+03   4.106 4.08e-05 ***
      # stressrecode:dayrecode:pssSum -4.222e-02  4.447e-02  3.200e+01  -0.949 0.349498
      #
      # Less shared on Day 2, but is attenuated by high PSS

      tg_reg2 = lmer(shared ~ 1 + stressrecode*dayrecode*pssSumCategorical + (1 | subjectID), data = bst_tg_pss)
      summary(tg_reg2)
      #                                            Estimate Std. Error         df t value Pr(>|t|)
      # (Intercept)                                 2.59786    0.37412   31.99999   6.944 7.28e-08 ***
      # stressrecode                                0.09659    0.02712 5432.00000   3.562 0.000371 ***
      # dayrecode                                  -0.07192    0.02712 5432.00000  -2.652 0.008016 **
      # pssSumCategorical                          -0.15806    0.43301   31.99999  -0.365 0.717496
      # stressrecode:dayrecode                      0.32484    0.37412   31.99999   0.868 0.391711
      # stressrecode:pssSumCategorical             -0.17239    0.03138 5432.00000  -5.493 4.13e-08 ***
      # dayrecode:pssSumCategorical                 0.14772    0.03138 5432.00000   4.707 2.58e-06 ***
      # stressrecode:dayrecode:pssSumCategorical   -0.61201    0.43301   31.99999  -1.413 0.167201
      #
      # Stress significantly increases amount shared
      # Less shared on day 2
      # High chronic stress (PSS) reduces effect of acute stress and effect of day
      #   ... is this a blunting thing? Less reactive to things others respond to?

      anova(tg_reg1_withday, tg_reg2)
      # CATEGORICAL regression outperforms continuous

  #modeling looking at gambler's fallacy/previous trial results (using the categorical stress)
      tg_mod_pt_acuteCat_chronicCont <- lme(shared ~ 1 + prevTrialFeedback + prevTrialFeedback:stressedBool + stressedBool * pssSum, random = ~ 1 | subjectID, data = bst_tg_pss) #I include the interaction term here with stressed as that was one of the main findings of FeldmannHall
      tg_mod_pt_acuteCat_chronicCont_day <- lme(shared ~ 1 + day + prevTrialFeedback + prevTrialFeedback:stressedBool + stressedBool * pssSum, random = ~ 1 | subjectID, data = bst_tg_pss) #including the possible confound of day

      summary(tg_mod_pt_acuteCat_chronicCont)
      summary(tg_mod_pt_acuteCat_chronicCont_day)
      #Results: interesting here that there didn't really seem to be an effect of the outcome of a previous trial, and the patterns seen elsewhere held true
      #day, stressed, and pss:stressed were all signficant, with the interaction having the largest effect size when taking into account the min to max possibilities

      # PSH NOTE: Taking the best-performing TG regression from above, and adding prev. trial factors
      tg_reg3 = lmer(shared ~ 1 + stressrecode*dayrecode*pssSumCategorical +
                       prevTrialFeedback*stressrecode*pssSumCategorical +
                       prevTrialShared*stressrecode*pssSumCategorical +
                       (1 | subjectID), data = bst_tg_pss)
      summary(tg_reg3)
      #                                                    Estimate Std. Error         df t value Pr(>|t|)
      # (Intercept)                                       2.571e+00  3.544e-01  3.246e+01   7.254 2.81e-08 ***
      # stressrecode                                      2.843e-01  3.815e-02  5.425e+03   7.453 1.05e-13 ***
      # dayrecode                                        -7.228e-03  2.826e-02  5.424e+03  -0.256 0.798126
      # pssSumCategorical                                -3.043e-01  4.103e-01  3.248e+01  -0.742 0.463662
      # prevTrialFeedback                                -6.349e-03  2.930e-02  5.430e+03  -0.217 0.828476
      # prevTrialShared                                   5.184e-02  4.520e-02  5.450e+03   1.147 0.251439
      # stressrecode:dayrecode                            3.026e-01  3.530e-01  3.194e+01   0.857 0.397747
      # stressrecode:pssSumCategorical                   -2.924e-01  4.502e-02  5.425e+03  -6.495 9.05e-11 ***
      # dayrecode:pssSumCategorical                       7.594e-02  3.220e-02  5.424e+03   2.358 0.018394 *
      # stressrecode:prevTrialFeedback                    1.697e-02  2.822e-02  5.425e+03   0.601 0.547611
      # pssSumCategorical:prevTrialFeedback              -4.336e-02  3.285e-02  5.430e+03  -1.320 0.186924
      # stressrecode:prevTrialShared                     -2.603e-01  3.662e-02  5.426e+03  -7.107 1.34e-12 ***
      # pssSumCategorical:prevTrialShared                 1.979e-01  5.259e-02  5.449e+03   3.763 0.000170 ***
      # stressrecode:dayrecode:pssSumCategorical         -5.696e-01  4.085e-01  3.192e+01  -1.394 0.172835
      # stressrecode:pssSumCategorical:prevTrialFeedback -2.302e-02  3.165e-02  5.425e+03  -0.727 0.467128
      # stressrecode:pssSumCategorical:prevTrialShared    1.677e-01  4.342e-02  5.426e+03   3.863 0.000113 ***

      # ONLY the significant portions (not incl. intercept):
      # stressrecode                                      2.843e-01  3.815e-02  5.425e+03   7.453 1.05e-13 ***
      # stressrecode:pssSumCategorical                   -2.924e-01  4.502e-02  5.425e+03  -6.495 9.05e-11 ***
      # stressrecode:prevTrialShared                     -2.603e-01  3.662e-02  5.426e+03  -7.107 1.34e-12 ***
      # pssSumCategorical:prevTrialShared                 1.979e-01  5.259e-02  5.449e+03   3.763 0.000170 ***
      # stressrecode:pssSumCategorical:prevTrialShared    1.677e-01  4.342e-02  5.426e+03   3.863 0.000113 ***
      # dayrecode:pssSumCategorical                       7.594e-02  3.220e-02  5.424e+03   2.358 0.018394 *

      # Stress increases the amount shared, but... this effect is blunted w/ high PSS scores
        # Low PSS: +0.28 for stress, -0.28 for control
        # High PSS: -0.0081 for stress, 0.0081 for control
        # Oddly, acute stress only has its effect in people w/ low chronic stress

      # The effect of prev. trial shared significantly varies between acute & chronic stress
        # pos. under high chronic stress (more sharing after sharing), but not low chronic stress
        # People w/ low chronic stress have pos. effect in control but neg. under ACUTE stress
        # 3-way interaction is... confusing

      # People with high chronic stress share much more on day 2 than day 1


      # Because the categorical PSS is poorly distributed (so we might be overfitting that one person
      # at the highest level of the PSS), let's also do the continuous version
      tg_reg3b = lmer(shared ~ 1 + stressrecode*dayrecode*pssSum +
                       prevTrialFeedback*stressrecode*pssSum +
                       prevTrialShared*stressrecode*pssSum +
                       (1 | subjectID), data = bst_tg_pss)
      summary(tg_reg3b)
      #                                         Estimate Std. Error         df t value Pr(>|t|)
      # (Intercept)                            2.711e+00  6.981e-01  3.222e+01   3.884 0.000481 ***
      # stressrecode                           3.571e-01  6.631e-02  5.424e+03   5.385 7.54e-08 ***
      # dayrecode                             -5.607e-02  5.464e-02  5.424e+03  -1.026 0.304823
      # pssSum                                -2.259e-02  4.190e-02  3.224e+01  -0.539 0.593529
      # prevTrialFeedback                     -3.786e-03  5.162e-02  5.428e+03  -0.073 0.941538
      # prevTrialShared                        3.612e-02  7.253e-02  5.452e+03   0.498 0.618498
      # stressrecode:dayrecode                 5.029e-01  6.964e-01  3.192e+01   0.722 0.475531
      # stressrecode:pssSum                   -1.854e-02  4.053e-03  5.425e+03  -4.575 4.87e-06 ***
      # dayrecode:pssSum                       6.563e-03  3.260e-03  5.424e+03   2.013 0.044134 *
      # stressrecode:prevTrialFeedback        -8.174e-03  5.047e-02  5.426e+03  -0.162 0.871343
      # pssSum:prevTrialFeedback              -2.199e-03  3.029e-03  5.428e+03  -0.726 0.467854
      # stressrecode:prevTrialShared          -4.455e-01  5.736e-02  5.426e+03  -7.767 9.52e-15 ***
      # pssSum:prevTrialShared                 1.046e-02  4.387e-03  5.451e+03   2.384 0.017157 *
      # stressrecode:dayrecode:pssSum         -3.879e-02  4.180e-02  3.192e+01  -0.928 0.360285
      # stressrecode:pssSum:prevTrialFeedback  6.287e-04  2.961e-03  5.426e+03   0.212 0.831847
      # stressrecode:pssSum:prevTrialShared    1.967e-02  3.521e-03  5.426e+03   5.586 2.44e-08 ***

      # JUST THE SIGNIFICANT BITS
      # stressrecode                           3.571e-01  6.631e-02  5.424e+03   5.385 7.54e-08 ***
      # stressrecode:pssSum                   -1.854e-02  4.053e-03  5.425e+03  -4.575 4.87e-06 ***
      # stressrecode:prevTrialShared          -4.455e-01  5.736e-02  5.426e+03  -7.767 9.52e-15 ***
      # pssSum:prevTrialShared                 1.046e-02  4.387e-03  5.451e+03   2.384 0.017157 *
      # stressrecode:pssSum:prevTrialShared    1.967e-02  3.521e-03  5.426e+03   5.586 2.44e-08 ***
      # dayrecode:pssSum                       6.563e-03  3.260e-03  5.424e+03   2.013 0.044134 *

      # Overall pattern is identical as categorical regression.
      #
      # Effect of acute stress on sharing varies by chronic stress
      #    Low PSS: +.36 stress, -.36 control (more sharing under acute stress)
      #   High PSS: -.18 stress, +.18 control (less sharing under acute stress)
      #
      # Effect of Prev. trial choice on current choice varies by acute & chronic stress
      #    Low PSS: -.45 stress, +.45 control (less sharing after sharing under acute stress)
      #   High PSS: +.37 stress, +.20 control (more sharing after sharing... all the time)

      # (if you re-format tg_reg3b to use stressedBool, it's a little simpler to see, but pattern
      # is identical)


      #                                         Estimate Std. Error         df t value Pr(>|t|)
      # (Intercept)                            2.354e+00  7.007e-01  3.271e+01   3.360    0.002 **
      # stressedBool                           7.142e-01  1.326e-01  5.424e+03   5.385 7.54e-08 ***
      # dayrecode                             -5.589e-01  6.987e-01  3.234e+01  -0.800    0.430
      # pssSum                                -4.047e-03  4.208e-02  3.280e+01  -0.096    0.924
      # prevTrialFeedback                      4.389e-03  7.368e-02  5.426e+03   0.060    0.953
      # prevTrialShared                        4.816e-01  9.171e-02  5.450e+03   5.252 1.56e-07 ***
      # stressedBool:dayrecode                 1.006e+00  1.393e+00  3.192e+01   0.722    0.476
      # stressedBool:pssSum                   -3.708e-02  8.106e-03  5.425e+03  -4.575 4.87e-06 ***
      # dayrecode:pssSum                       4.536e-02  4.193e-02  3.233e+01   1.082    0.287
      # stressedBool:prevTrialFeedback        -1.635e-02  1.009e-01  5.426e+03  -0.162    0.871
      # pssSum:prevTrialFeedback              -2.828e-03  4.308e-03  5.426e+03  -0.656    0.512
      # stressedBool:prevTrialShared          -8.910e-01  1.147e-01  5.426e+03  -7.767 9.52e-15 ***
      # pssSum:prevTrialShared                -9.208e-03  5.646e-03  5.448e+03  -1.631    0.103
      # stressedBool:dayrecode:pssSum         -7.759e-02  8.359e-02  3.192e+01  -0.928    0.360
      # stressedBool:pssSum:prevTrialFeedback  1.257e-03  5.921e-03  5.426e+03   0.212    0.832
      # stressedBool:pssSum:prevTrialShared    3.934e-02  7.042e-03  5.426e+03   5.586 2.44e-08 ***

      # JUST SIG
      # stressedBool                           7.142e-01  1.326e-01  5.424e+03   5.385 7.54e-08 ***
      # stressedBool:pssSum                   -3.708e-02  8.106e-03  5.425e+03  -4.575 4.87e-06 ***
      # prevTrialShared                        4.816e-01  9.171e-02  5.450e+03   5.252 1.56e-07 ***
      # stressedBool:prevTrialShared          -8.910e-01  1.147e-01  5.426e+03  -7.767 9.52e-15 ***
      # stressedBool:pssSum:prevTrialShared    3.934e-02  7.042e-03  5.426e+03   5.586 2.44e-08 ***

      # Effect of acute stress
      #   more money shared under stress (+.71, vs. control)
      #   but with high PSS, effect is wiped out/reversed (-.29 stress, vs. control)
      #
      # Effect of prev. trial
      #   more shared after sharing (+.48 in control)
      #   in low PSS, acute stress reverses the pattern (-.47)
      #   in high PSS, acute stress does v. little (+.65)


      tg_reg3b_withoutday = lmer(shared ~ 1 + stressrecode*pssSum +
                        prevTrialFeedback*stressrecode*pssSum +
                        prevTrialShared*stressrecode*pssSum +
                        (1 | subjectID), data = bst_tg_pss)
      summary(tg_reg3b_withoutday)

      #                                         Estimate Std. Error         df t value Pr(>|t|)
      # (Intercept)                            2.386e+00  5.744e-01  3.422e+01   4.154 0.000206 ***
      # stressrecode                           4.059e-01  5.461e-02  5.426e+03   7.434 1.22e-13 ***
      # pssSum                                -8.368e-04  3.404e-02  3.429e+01  -0.025 0.980529
      # prevTrialFeedback                     -1.721e-03  5.168e-02  5.432e+03  -0.033 0.973433
      # prevTrialShared                        3.774e-02  7.246e-02  5.458e+03   0.521 0.602486
      # stressrecode:pssSum                   -2.217e-02  3.380e-03  5.427e+03  -6.559 5.93e-11 ***
      # stressrecode:prevTrialFeedback        -1.613e-02  5.025e-02  5.428e+03  -0.321 0.748290
      # pssSum:prevTrialFeedback              -2.425e-03  3.032e-03  5.432e+03  -0.800 0.423863
      # stressrecode:prevTrialShared          -4.477e-01  5.558e-02  5.428e+03  -8.056 9.64e-16 ***
      # pssSum:prevTrialShared                 1.051e-02  4.383e-03  5.456e+03   2.398 0.016500 *
      # stressrecode:pssSum:prevTrialFeedback  1.372e-03  2.945e-03  5.428e+03   0.466 0.641256
      # stressrecode:pssSum:prevTrialShared    1.966e-02  3.438e-03  5.428e+03   5.717 1.14e-08 ***

      # JUST SIG
      # stressrecode                           4.059e-01  5.461e-02  5.426e+03   7.434 1.22e-13 ***
      # stressrecode:pssSum                   -2.217e-02  3.380e-03  5.427e+03  -6.559 5.93e-11 ***
      # stressrecode:prevTrialShared          -4.477e-01  5.558e-02  5.428e+03  -8.056 9.64e-16 ***
      # pssSum:prevTrialShared                 1.051e-02  4.383e-03  5.456e+03   2.398 0.016500 *
      # stressrecode:pssSum:prevTrialShared    1.966e-02  3.438e-03  5.428e+03   5.717 1.14e-08 ***

      # Running the regression _without_ Day produces VERY similar output. Only differences are...
      # WITHOUT Day has no sig. main effect of prevTrialShared, and does have an interaction
      # between prevTrialShared and PSS.


      # FeldmanHall found effects of previous feedback. We don't, above. Try to simplify the
      # regression to check.
      tg_reg3b_withoutdaywithoutPrevShared = lmer(shared ~ 1 + stressrecode*pssSum +
                                   prevTrialFeedback*stressrecode*pssSum +
                                   # prevTrialShared*stressrecode*pssSum +
                                   (1 | subjectID), data = bst_tg_pss)
      summary(tg_reg3b_withoutdaywithoutPrevShared)

      # no significant effects of prev. feedback.

      #trying the above regression, the fullest version without day using the amount shared, rather than the boolean
      tg_reg4 = lmer(shared ~ 1 + stressrecode*pssSum +
                        prevTrialFeedback*stressrecode*pssSum +
                        prevTrialSharedAmt*stressrecode*pssSum +
                        (1 | subjectID), data = bst_tg_pss)
      summary(tg_reg4)
      #the story doesn't change a ton, however, previous trial feedback does have a weak interaction with stress

      #added the pss median split to follow up more on nonlinearities in chronic stress
      tg_reg5 = lmer(shared ~ 1 + stressrecode*pssMedianSplit +
                        prevTrialFeedback*stressrecode*pssMedianSplit +
                        prevTrialShared*stressrecode*pssMedianSplit +
                        (1 | subjectID), data = bst_tg_pss)
      summary(tg_reg5)

                                                         # Estimate      Std. Error              df  t value   Pr(>|t|)
      # (Intercept)                                    2.36502775e+00  1.92218671e-01  3.44547331e+01 12.30384 3.6602e-14 ***
      # stressrecode                                   6.91443415e-02  2.08953185e-02  5.42775200e+03  3.30908 0.00094212 ***
      # pssMedianSplit                                -8.50520873e-02  1.92218671e-01  3.44547331e+01 -0.44248 0.66091038
      # prevTrialFeedback                             -3.93766248e-02  1.69461398e-02  5.43348266e+03 -2.32363 0.02018183 *
      # prevTrialShared                                2.13627524e-01  2.60188735e-02  5.45398261e+03  8.21048 2.7252e-16 ***
      # stressrecode:pssMedianSplit                    3.59513351e-03  2.08953185e-02  5.42775200e+03  0.17205 0.86340110
      # stressrecode:prevTrialFeedback                 3.75916515e-03  1.60711916e-02  5.42662121e+03  0.23391 0.81506596
      # pssMedianSplit:prevTrialFeedback              -2.26672858e-02  1.69461398e-02  5.43348266e+03 -1.33761 0.18108036
      # stressrecode:prevTrialShared                  -1.54542139e-01  2.12970511e-02  5.42938363e+03 -7.25650 4.5311e-13 ***
      # pssMedianSplit:prevTrialShared                 6.90002831e-02  2.60188735e-02  5.45398261e+03  2.65193 0.00802643 **
      # stressrecode:pssMedianSplit:prevTrialFeedback  5.69448533e-03  1.60711916e-02  5.42662121e+03  0.35433 0.72310629
      # stressrecode:pssMedianSplit:prevTrialShared    7.28209698e-02  2.12970511e-02  5.42938363e+03  3.41930 0.00063245 ***

      #a few interesting things here although for how exploratory this is, I'm not sure how much it changes.
      #Feedback and shared amounts are significant, albeit especially with the former, a small effect size. Stress*pss is no
      # longer significant interestingly as well. overall very similar though

