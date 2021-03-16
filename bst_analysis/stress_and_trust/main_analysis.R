
#loads up the dataframes from setupData, also loads the plyr library (note this in case you use dply)
#NOTE: this assumes that you are connected to the shlab drive, and it is attached under /Volumes
#if this is not the case, add your own configuration in the config.yml
config <- config::get()

setup_path <- file.path(config$path$code$r_scripts, config$code_files$setup_data)
source(setup_path)

library(ggplot2)
library(lme4)
library(nlme)

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
      #Key Results (p < .001, mean difference = 4.1) - the ratings are statiscally very probably different

    #checking for a day effect on ratings
      t.test(bst_bath$diffPleasantnessRating ~ bst_bath$day2StressedBool)
      # t.test(bst_bath$stressPleasantnessRating ~ bst_bath$day2StressedBool)
      # t.test(bst_bath$controlPleasantnessRating ~ bst_bath$day2StressedBool)

  #Chronic
    #descriptives of PSS
      mean(bst_pss$pssSum)
      sd(bst_pss$pssSum)
      range(bst_pss$pssSum)
      count(bst_pss$pssSumCategorical)

      #histogram of PSS scores
        ggplot(bst_pss, aes(x=pssSum)) +
          geom_histogram(binwidth = 1, color = "black", fill = "white") +
          scale_x_continuous(name = "Sum", breaks = c(0,5,10,15,20,25,30,35,40), limits = c(0,40)) +
          labs(title="Sum of PSS Scores Histogram") +
          theme_classic()

    #effect of acute stressor n chronic stressor
        #t-test on if there was a difference in PSS scores based on what type of bath was recieved on day 2 (the PSS was only administered on day 2)
        t.test(bst_bath_pss$pssSum ~ bst_bath_pss$day2StressedBool)
        #Key results (p = .94, t=-.075) recieved cold bath mean = 16, didn't recieved lukewarm bath mean = 15.86 - the PSS scores are statistically not probably different

#Trust:
  #Trust Rating
    #descriptives
      mean(bst_tr$rating)
      sd(bst_tr$rating)

      #prelim looking at different effect
        #underwent stressor or not
          by(data = bst_tr$rating, INDICES = bst_tr$stressedBool, FUN = mean)
          by(data = bst_tr$rating, INDICES = bst_tr$stressedBool, FUN = sd)
        #day
          by(data = bst_tr$rating, INDICES = bst_tr$day, FUN = mean)
          by(data = bst_tr$rating, INDICES = bst_tr$day, FUN = sd)
        #task order
          by(data = bst_tr$rating, INDICES = bst_tr$taskOrder, FUN = mean)
          by(data = bst_tr$rating, INDICES = bst_tr$taskOrder, FUN = sd)
        #what day they got the stressor on (0 means they got on day 1, 1 means they got it on day 2)
          by(data = bst_tr$rating, INDICES = bst_tr$day2StressedBool, FUN = mean)
          by(data = bst_tr$rating, INDICES = bst_tr$day2StressedBool, FUN = sd)
        #task order had a means difference of ~.3, maybe something there (sd was about 1.85)
        #day of stressor also has something there (maybe) with a means difference of .5 and a difference of ~.28 in the sd as well
        #following up on whether these possible confounds had any effect task order
          #note I include subjectID as a random effect, because without it, task order becomes close to a proxy for individual differences
          tr_confound_mod <- lme(rating ~ 1 + taskOrder + day + day2StressedBool, random = ~ 1 | subjectID, data = bst_tr_pss)
          summary(tr_confound_mod)
          #results: while none of these were techinically significant, task order was trending (p=.058). That said the effect size was pretty small (~-.08)
  #Trust Game
    #descriptives
      mean(bst_tg$shared)
      sd(bst_tg$shared)

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
    #sequential effects
      tg_seq_mod0 <- lm(shared ~ 1 + prevTrialShared, data = bst_tg)
      tg_seq_mod1 <- lm(shared ~ 1 + prevTrialFeedback, data = bst_tg)
      summary(tg_seq_mod0)
      summary(tg_seq_mod1)
      #Results: both of these models showed significance for the previous trial, notably people shared ~.32 more following a loss

      #the next regression looks at whether stress had anything to do with it, so feel free to uncomment the following lines to change the coding of the "stressedBool" variable
      # bst_tg$stressedBool <- as.numeric(bst_tg$stressedBool)
      # bst_tg$stressedBool <- ifelse(bst_tg$stressedBool == 0, -1, 1)
      tg_seq_mod2 <- lm(shared ~ 1 + prevTrialFeedback:stressedBool, data = bst_tg)
      summary(tg_seq_mod2)
      #Results: previous trial feedback was ~.19, so when a participant was stressed, the effect of previous trials is actually smaller?

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
        scale_x_continuous(name = "Shared Amount Average ($)") +
        scale_y_continuous(name = "Trust Rating Average", limits = c(1,9), breaks = c(1,3,6,9)) +
        geom_smooth(method = lm, se = FALSE) +
        geom_errorbarh(aes(xmin = sharedLowSE, xmax = sharedHighSE)) +
        geom_errorbar(aes(ymin = ratingLowSE, ymax = ratingHighSE)) +
        theme_classic()

    #seeing if the correlations changed when seperating out by stressed or not condition as well
        #on means
          with(data = (subset(bst_t_compiled_part_avg_stress, stressedBool == 1)), cor.test(sharedAvg, ratingAvg, method = 'pearson')) #stressed
          with(data = (subset(bst_t_compiled_part_avg_stress, stressedBool == 0)), cor.test(sharedAvg, ratingAvg, method = 'pearson')) #not stressed
          #Results, the not stressed was trending (p = .051) r = .31, the stressed was not significant (p~.15), r = .23

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
    tr_mod_acuteCat_chronicCont <- lme(rating~ 1 + stressedBool * pssSum, random = ~ 1 | subjectID, data = bst_tr_pss) #categorical stress
    tr_mod_acuteRat_chronciCont <- lme(rating ~ 1 + diffPleasantnessRating * pssSum, random = ~ 1 | subjectID, data = bst_tr_pss) #pleasantness ratings
    tr_mod_acuteCat_chronicCat <- lme(rating ~ 1 + stressedBool * pssSumCategorical, random = ~ 1 | subjectID, data = bst_tr_pss) #categorical PSS scores

    summary(tr_mod_acuteCat_chronicCont)
    summary(tr_mod_acuteRat_chronciCont)
    summary(tr_mod_acuteCat_chronicCat)
    #Results: both precitors and their interaction were not sifnificant (aside from the intercept),
    #however, using the categorical stress, the interaction of stress and pss score was p~.06, although the effect size was pretty small all things considered (~-.01)
    #Finally, using the categorical pss, the the stressed category and the interaction of the acute and chronic stressor were signficant, with an effect size of -.23 for the interaction
  #including task order because that appeared to maybe have an effect in prelim analysis
    tr_mod_cat1 <- lme(rating ~ 1 + taskOrder + stressedBool * pssSum, random = ~ 1 | subjectID, data = bst_tr_pss)

    summary(tr_mod_cat1)
    #Results: main effect thing to note here was that task order was not signficant (p=.075), howeverthe stressed:pss interaction did move to significance

#Trust Game:
    #basic modelling of stress and trust
      tg_mod_acuteCat_chronicCont <- lme(shared ~ 1 + stressedBool * pssSum, random = ~ 1 | subjectID, data = bst_tg_pss) #categorical stress
      tg_mod_acuteRat_chronciCont <- lme(shared ~ 1 + diffPleasantnessRating * pssSum, random = ~ 1 | subjectID, data = bst_tg_pss) #pleasantness ratings
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

  #modeling looking at gambler's fallacy/previous trial results (using the categorical stress)
      tg_mod_pt_acuteCat_chronicCont <- lme(shared ~ 1 + prevTrialFeedback + prevTrialFeedback:stressedBool + stressedBool * pssSum, random = ~ 1 | subjectID, data = bst_tg_pss) #I include the interaction term here with stressed as that was one of the main findings of FeldmannHall
      tg_mod_pt_acuteCat_chronicCont_day <- lme(shared ~ 1 + day +prevTrialFeedback + prevTrialFeedback:stressedBool + stressedBool * pssSum, random = ~ 1 | subjectID, data = bst_tg_pss) #including the possible confound of day

      summary(tg_mod_pt_acuteCat_chronicCont)
      summary(tg_mod_pt_acuteCat_chronicCont_day)
      #Results: interesting here that there didn't really seem to be an effect of the outcome of a previous trial, and the patterns seen elsewhere held true
      #day, stressed, and pss:stressed were all signficant, with the interaction having the largest effect size when taking into account the min to max possibilities
