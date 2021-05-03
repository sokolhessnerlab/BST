
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

#Base Level Analysis#
#Stress:
  #Acute

    #descriptives
      mean(bst_bath$stressPleasantnessRating)
      sd(bst_bath$stressPleasantnessRating)
      mean(bst_bath$controlPleasantnessRating)
      sd(bst_bath$controlPleasantnessRating)

    #effect of stressor on ratings
      #paired sample t-test (ie a test of differences)
      t.test(bst_bath$stressPleasantnessRating - bst_bath$controlPleasantnessRating, mu = 0, var.equal = TRUE)
      # PSH NOTE: This is run as a one-sample t-test, you could also run it as follows:
      t.test(bst_bath$stressPleasantnessRating,bst_bath$controlPleasantnessRating, paired = T, var.equal = T)
      # PSH NOTE: paired t-tests are reducible to one-sample t-tests against 0, so there's no
      # meaningful statistical difference here, but it can be easier/clearer to communicate and/or
      # think through b/c it's one less data-prep step.

      #Key Results (p = 3.0e-16, mean difference = 4.1) - the ratings are statistically very probably different
      # PSH NOTE: This is a BIG difference. A diff of 4.1 when your scale goes 1-7 is huge.
      # PSH NOTE: use exact p-value estimates when possible; no reason not to.

    #checking for an ORDER effect on the CHANGE in ratings
      t.test(bst_bath$diffPleasantnessRating ~ bst_bath$day2StressedBool)
      # t.test(bst_bath) # <-- doesn't run?
      # t.test(bst_bath$stressPleasantnessRating ~ bst_bath$day2StressedBool)
      # t.test(bst_bath$controlPleasantnessRating ~ bst_bath$day2StressedBool)

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
        #t-test on if there was a difference in PSS scores based on what type of bath was recieved on day 2 (the PSS was only administered on day 2)
        t.test(bst_bath_pss$pssSum ~ bst_bath_pss$day2StressedBool)
        #Key results (p = .94, t=-.075) received cold bath mean = 16, didn't recieved lukewarm bath mean = 15.86
        # the PSS scores are not significantly affected by the presence of the cold water bath on day 2

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
          #results: while none of these were techinically significant, task order was trending (p=.058). That said the effect size was pretty small (~-.08)

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

#Var Associations#
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

    tr_reg1b = lmer(rating ~ 1 + stressedBool*dayrecode*pssSum + (1 | subjectID), data = bst_tr_pss)
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
      geom_line(aes(y = Control), color='#1a9988', size=2) +
      geom_line(aes(y = Stress), color = "#eb5600", size=2) +
      scale_x_continuous(name = "PSS Score", breaks = c(0,5,10,15,20,25,30), limits = c(0,27), expand = c(0,0)) +
      scale_y_continuous(name = "Trust Rating", expand = c(0,0), breaks = seq(1,9,1), limits = c(3.5,6)) +
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

    anova(tr_reg1,tr_reg2) # reg 2 (categorical) outperforms reg 1
    anova(tr_reg2,tr_reg3) # reg 2 (cat) outperforms reg 3 (exponential)
    anova(tr_reg1,tr_reg3)

    # CATEGORICAL REGRESSION does best here.


#Trust Game:
    #basic modelling of stress and trust
      tg_mod_acuteCat_chronicCont <- lme(shared ~ 1 + stressedBool * pssSum, random = ~ 1 | subjectID, data = bst_tg_pss) #categorical stress
      # tg_mod_acuteRat_chronciCont <- lme(shared ~ 1 + diffPleasantnessRating * pssSum, random = ~ 1 | subjectID, data = bst_tg_pss) #pleasantness ratings
      tg_mod_acuteCat_chronicCat <- lme(shared ~ 1 + stressedBool * pssSumCategorical, random = ~ 1 | subjectID, data = bst_tg_pss) #categorical PSS scores

       summary(tg_mod_acuteCat_chronicCont)
      summary(tg_mod_acuteRat_chronciCont)
      summary(tg_mod_acuteCat_chronicCat)
      #Results: like before the pleasantness ratings did not accont for anything significant
      #however, the categorical stressed or not did have some interesting effects, it itself was signficant (and an effect of .32), and the interaction with PSS was significant (with an effect of -.25)
      #also the continuous PSS scores were also not signficant
  #including day because in the prelim analysis that was significant
      tg_mod_acuteCat_chronicCont_day <- lme(shared ~ 1 + day + stressedBool * pssSum, random = ~ 1 | subjectID, data = bst_tg_pss)

      summary(tg_mod_acuteCat_chronicCont_day)
      #Results: here it was interesting to note that the findings didn't change a ton from the previous model, however day was significant with an effect size of .073

      bst_tg_pss$dayrecode = bst_tg_pss$day * 2 - 3; # Converts 1, 2 coding into -1/+1 coding
      bst_tg_pss$stressrecode = bst_tg_pss$stressedBool*2-1 # converts 0,1 coding into -1/+1 coding

      tg_reg1_withoutday = lmer(shared ~ 1 + stressrecode*pssSum + (1 | subjectID), data = bst_tg_pss)
      summary(tg_reg1_withoutday)
      #                       Estimate Std. Error         df t value Pr(>|t|)
      # (Intercept)          2.355e+00  6.102e-01  3.400e+01   3.860 0.000483 ***
      # stressrecode         1.637e-01  4.432e-02  5.434e+03   3.694 0.000223 ***
      # pssSum               9.938e-03  3.614e-02  3.400e+01   0.275 0.785022
      # stressrecode:pssSum -1.258e-02  2.625e-03  5.434e+03  -4.793 1.69e-06 ***

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

      anova(tg_reg1, tg_reg2)
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
