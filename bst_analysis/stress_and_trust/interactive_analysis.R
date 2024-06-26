
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


# STRESS & PLEASANTNESS ######################

#pss score and the pleasantness ratings correlations
#normal
cor.test(bst_bath_pss$pssSum, bst_bath_pss$diffPleasantnessRating, method = 'pearson')
cor.test(bst_bath_pss$pssSum, bst_bath_pss$controlPleasantnessRating, method = 'pearson')
cor.test(bst_bath_pss$pssSum, bst_bath_pss$stressPleasantnessRating, method = 'pearson')
#Results: no statistically significant correlations

#as the 3 levels (also not significant)
cor.test(bst_bath_pss$pssSumCategorical, bst_bath_pss$diffPleasantnessRating, method = 'pearson')

# TRUST GAME & RATING ######################
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

# STRESS & TRUST ######################
## Participant-level Means ####

#correlations:
### Trust Rating ####
cor.test(bst_t_compiled_part_avg_pss$ratingAvg, bst_t_compiled_part_avg_pss$pssSum, method = 'pearson')
cor.test(bst_t_compiled_part_avg_pss$ratingAvg, bst_t_compiled_part_avg_pss$pssSum, method = 'spearman', exact = FALSE)
#Results: neither was statistically significant

### Trust Game ####
cor.test(bst_t_compiled_part_avg_pss$sharedAvg, bst_t_compiled_part_avg_pss$pssSum, method = 'pearson')
cor.test(bst_t_compiled_part_avg_pss$sharedAvg, bst_t_compiled_part_avg_pss$pssSum, method = 'spearman', exact = FALSE)
#Results: neither here was statistically significant

## Participant-level Models ####
### Trust Rating ####
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

#### Prev Trial on trust rating ####
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

#### Day Effects added ####

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

### Trust Game ####
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

#### Day Effects aadded  ####

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

