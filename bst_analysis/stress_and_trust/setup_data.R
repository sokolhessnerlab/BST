#-loads up a series of data frames of the stress constructs and trust constructs for BST for later analysis
#-aggregates data into additional data frames
#-performs coding and computation of variables

#loads up the necessary libraries
library(plyr)
library(dplyr)
library(car)

#Color Blind Palette for graphing
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##Start Data Retrieval, Cleaning, Formatting: ##

#### STRESS ####

#setting Up Stress Data Frames#

# --- Chronic Stress Measures--- #

# PSS DFs #

#Loads in PSS .csv file
pss_csv <- file.path(config$path$data$survey, config$csvs$pss)
pss <- read.csv(pss_csv) #reads in the .csv
names(pss)[names(pss) == "subjectNumber"] <- 'subjectID' #renames the subject id column to maintain consistency across DFs

#recode reverse coded values for the 4 questions that need it
pss$personalProblemConfidenceRecode <- 4 - pss$personalProblemConfidence
pss$goingYourWayRecode <- 4 - (pss$goingYourWay)
pss$irritationControlRecode <- 4 - pss$irritationControl
pss$onTopOfThingsRecode <- 4 - pss$onTopOfThings
#recoded items added as 4 new columns to DF

# PSS subject-level mean score, category, & median split #

#PSS mean score: calculate and add new column with PSS numerical score
pss$pssSum <- (pss$unexpectedUpset + pss$unableControl + pss$nervous + pss$personalProblemConfidenceRecode + pss$goingYourWayRecode + pss$couldNotCope + pss$irritationControlRecode + pss$onTopOfThingsRecode + pss$outsideOfControl + pss$diffPilingUp)

#PSS category: calculate and add new column for PSS 3-level category
pss$pssCat_0low_1mod_2high <- ifelse(pss$pssSum <= 13, 0,
                                    ifelse(pss$pssSum >= 27, 2, 1)) #calculates the pss score in the 3 level categorical version

#PSS median split: calculate and add new column with pss median split -1/1
pss$pssMedianSplit <- ifelse(pss$pssSum < median(pss$pssSum), -1, 1)

#re-orders participant data (subjectID) in ascending order
pss <- pss[order(pss$subjectID, decreasing = FALSE),] 

#creates truncated PSS DF with main PSS scores (without PSS response categories as columns)
Stress_Chronic <- pss[c(1,16:18) ]


# --- Acute Stress Measures --- #

# Bath Rating DFs #

#Loads Bath Ratings .csv file
bath_pleasantness_csv <- file.path(config$path$data$current, config$csvs$bath_pleasantness)
bathRatingByCondition <- read.csv(bath_pleasantness_csv) #reads in the unpleasantness ratings
# Control & Stress conditions' bath unpleasantness ratings: 1 = Very Pleasant to 7 = Very Unpleasant

#Loads Bath Order .csv file
bath_order_csv <- file.path(config$path$data$current, config$csvs$bath_order)
bathOrder <- read.csv(bath_order_csv) #reads in the bath ordering
names(bathOrder)[names(bathOrder) == "BST.."] <- 'subjectID' #renames the subject id column to maintain consistency across DFs

#merges the two bath DF's into one AND removes bath orders of participants who do not have an bath rating score
bath <- merge(bathRatingByCondition, bathOrder, by = "subjectID") #DF of bath order & rating for those who completed the study

# Bath subject-level difference in bath unpleasantness score (between stress/control) & dat 2 stress/control boolean #

#Difference in Bath Unpleasantness: calculates the difference between the stress and control bath unpleasantness ratings
bath$diffUnPleasantnessRating <- (bath$STRESS - bath$CONTROL) 

#Stress Day 2 Boolean: calculates boolean for whether participant was given stressor (1) or control (0) on 2nd day
bath$day2bool_0control_1stress <- ifelse(bath$Day.2 == "CONTROL", 0, 1) 

#renaming columns for clarity
names(bath)[names(bath) == "CONTROL"] <- "controlUnpleasantnessRating"
names(bath)[names(bath) == "STRESS"] <- "stressUnpleasantnessRating"
names(bath)[names(bath) == "Day.1"] <- "bathReceivedDay1"
names(bath)[names(bath) == "Day.2"] <- "bathReceivedDay2"

#creates truncated Bath order & Bath rating DF 
Stress_Acute <- bath[c(1,6:7) ]


# --- Combining Acute & Chronic Stress Measures --- #

#combining acute and chronic stressors for wide data
STRESS <- merge(Stress_Acute, Stress_Chronic, by = "subjectID")


#### TRUST ####
#Setting Up Trust DFs:#
#Trust Game:
tg_csv <- file.path(config$path$data$current, config$csvs$tg)
bst_tg <- read.csv(tg_csv) #reads in trust game data

#renaming columns for clarity
names(bst_tg)[names(bst_tg) == "condition"] <- "taskOrder"
names(bst_tg)[names(bst_tg) == "RT"] <- "responseTime"

#combines the trust game data with the bath data for calculation of variables and later analysis
bst_tg <- merge(bst_tg, bst_bath, by = "subjectID")
  # PSH NOTE: Careful with things like this. This puts data on two very different scales (e.g.
  # the PSS with one sum value per person; TG data with many individual trials) together in
  # a way that could create issues if someone forgets that... Put another way, this merge
  # creates a dataframe that makes it look like the PSS was measured 152 times per person
  # and it looked the same every time.
  #
  # That issue makes this of limited utility to do; if you do want to relate acute or
  # chronic stressors to TG performance, you'll need to summarize TG data to the same space
  # (e.g. person-level summary stats) anyway. [PSH]


#calculating new factors for whether a participant was stressed or not before doing the trust task
bst_tg$stressedBool <- ifelse((bst_tg$day2StressedBool == 1 & bst_tg$day == 1), 0,
                              ifelse((bst_tg$day2StressedBool == 0 & bst_tg$day == 1), 1,
                                     ifelse((bst_tg$day2StressedBool == 1 & bst_tg$day == 2), 1,
                                            ifelse((bst_tg$day2StressedBool == 0 & bst_tg$day == 2), 0, NA))))
#double checking the math above worked out (there should be no NA)
# count(bst_tg$stressedBool)

#calculating the previous trial sharing data (does the same as the for loop was here previously)
bst_tg$prevTrialSharedAmt <- bst_tg$shared #sets up column
bst_tg$prevTrialSharedAmt <- c(0, bst_tg$prevTrialSharedAmt[-nrow(bst_tg)]) #shifts column by 1, replacing the first element with a 0
bst_tg$prevTrialSharedAmt <- ifelse(bst_tg$cumTrialNum == 1, 0, bst_tg$prevTrialSharedAmt) #replaces all trials without a previous element with a 0
bst_tg$prevTrialShared <- ifelse(bst_tg$cumTrialNum == 1, 0,
                                 ifelse(bst_tg$prevTrialSharedAmt > 0, 1, -1)) #creates the boolean coding
#calculating feedback from previous trial
bst_tg$prevTrialFeedback <- bst_tg$received
bst_tg$prevTrialFeedback <- c(0, bst_tg$prevTrialFeedback[-nrow(bst_tg)])
bst_tg$prevTrialFeedback <- ifelse(bst_tg$cumTrialNum == 1, 0,
                                   ifelse(bst_tg$prevTrialShared == -1, 0,
                                          ifelse(bst_tg$prevTrialFeedback > 0, 1, -1)))

#make sure the counts are where they should be (the coding worked right)
# count(bst_tg$shared)
# count(bst_tg$prevTrialShared)
# count(bst_tg$partnerChoice)
# count(bst_tg$prevTrialFeedback)
# count(bst_tg$prevTrialSharedAmt)


#setting up a categorical for if a participant shared any money at all
bst_tg$sharedBool <- ifelse(bst_tg$shared == 0, 0, 1)

#because merging with the PSS will remove 3 participants, this is a seperate DF for only those participants that have a PSS score
bst_tg_pss <- merge(bst_pss, bst_tg, by = "subjectID") #merges with the pss for later analysis

#Trust Rating:
tr_csv <- file.path(config$path$data$current, config$csvs$tr)
bst_tr <- read.csv(tr_csv)

#renaming columns for clarity
names(bst_tr)[names(bst_tr) == "response"] <- "rating"
names(bst_tr)[names(bst_tr) == "condition"] <- "taskOrder"
names(bst_tr)[names(bst_tr) == "RT"] <- "responseTime"

#combines the trust rating data with the bath data for calculation of variables and later analysis
bst_tr <- merge(bst_tr, bst_bath, by = "subjectID")

#calculating new factors for whether a participant was stressed or not before doing the trust task
bst_tr$stressedBool <- ifelse((bst_tr$day2StressedBool == 1 & bst_tr$day == 1), 0,
                              ifelse((bst_tr$day2StressedBool == 0 & bst_tr$day == 1), 1,
                                     ifelse((bst_tr$day2StressedBool == 1 & bst_tr$day == 2), 1,
                                            ifelse((bst_tr$day2StressedBool == 0 & bst_tr$day == 2), 0, NA))))
#double checking the math above worked out (there should be no NA)
# count(bst_tg$stressedBool)

#Previous trial stuff for the trust rating task
bst_tr$prevTrialRatingAmt <- bst_tr$rating #sets up the previous amount as the current amount
bst_tr$prevTrialRatingAmt <- c(0, bst_tr$prevTrialRatingAmt[-nrow(bst_tr)]) #shifts the previous rating column down by 1
bst_tr$prevTrialRatingAmt <- ifelse(bst_tr$cumTrialNum == 1, 0, bst_tr$prevTrialRatingAmt) #converts all the first trials to 0
bst_tr$prevTrialRating <- ifelse(bst_tr$cumTrialNum == 1, 0,
                                 ifelse(bst_tr$prevTrialRatingAmt >= 5, 1,
                                        ifelse(bst_tr$prevTrialRatingAmt < 5, -1, 0))) #converts the previous amount to the boolean coding
#just checking to see whether the coding looks like it worked out
# count(bst_tr$rating)
# count(bst_tr$prevTrialRatingAmt)
# count(bst_tr$prevTrialRating)

#because merging with the PSS will remove 3 participants, this is a seperate DF for only those participants that have a PSS score
bst_tr_pss <- merge(bst_pss, bst_tr, by = "subjectID") #for use when looking at pss x trust rating


#setting up aggregated DFs:
#overall:
#for the trust game:
bst_tg_part_avg <- aggregate(shared ~ subjectID, data = bst_tg, FUN = mean) #aggregates the trust game by participant
names(bst_tg_part_avg)[names(bst_tg_part_avg) == "shared"] <- "sharedAvg"
bst_tg_part_avg$sharedSD <- (aggregate(shared ~ subjectID, data =bst_tg, FUN = sd))$shared #calculates each participants standard deviation shared amount
bst_tg_part_avg$sharedSE <- (bst_tg_part_avg$sharedSD / sqrt(nrow(bst_tg_part_avg))) #calculates the standard error
  # PSH NOTE: Don't think this is makes sense to do here. SD makes sense on a per-person level,
  # but I'd only calculate the SE of the mean across people... but normalizing everyone's
  # unique SD by the number of people isn't a useful quantity, I don't think.
  # i.e. I'd take the sd across everyone's means, and normalize that single number by
  # the sqrt of the number of participants.
bst_tg_part_avg$sharedHighSE <- bst_tg_part_avg$sharedAvg +  bst_tg_part_avg$sharedSE #calculates the standard error high bound
bst_tg_part_avg$sharedLowSE <- bst_tg_part_avg$sharedAvg - bst_tg_part_avg$sharedSE #calculates the standard error low bound

bst_tg_part_avg

#for the trust rating
bst_tr_part_avg <- aggregate(rating ~ subjectID, data = bst_tr, FUN = mean) #aggregates the trust rating task for each participant
names(bst_tr_part_avg)[names(bst_tr_part_avg) == "rating"] <- "ratingAvg"
bst_tr_part_avg$ratingSD <- aggregate(rating ~ subjectID, data = bst_tr, FUN = mean)$rating #calculates each particpant's standard deviation shared amount
bst_tr_part_avg$ratingSE <- (bst_tr_part_avg$ratingSD / sqrt(nrow(bst_tr_part_avg)))
  # PSH NOTE: same as above
bst_tr_part_avg$ratingHighSE <- bst_tr_part_avg$ratingAvg + bst_tr_part_avg$ratingSE #calculates the standard error high bound
bst_tr_part_avg$ratingLowSE <- bst_tr_part_avg$ratingAvg - bst_tr_part_avg$ratingSE #calculates the standard error low bound

#compiles all the averaged participant data into one spot
bst_t_compiled_part_avg <- merge(bst_tg_part_avg, bst_tr_part_avg, by = 'subjectID')
bst_t_compiled_part_avg <- merge(bst_t_compiled_part_avg, bst_bath, by = 'subjectID')

bst_t_compiled_part_avg_pss <- merge(bst_t_compiled_part_avg, bst_pss, by = 'subjectID')

#split up by stressed or not
#for the trust game:
bst_tg_part_avg_stress <- aggregate(shared ~ subjectID + stressedBool, data = bst_tg, FUN = mean) #aggregates the trust game by participant
names(bst_tg_part_avg_stress)[names(bst_tg_part_avg_stress) == "shared"] <- "sharedAvg"
#names(bst_tg_part_avg_stress)[names(bst_tg_part_avg_stress) == "stressedBool"] <- ""
bst_tg_part_avg_stress$sharedSD <- (aggregate(shared ~ subjectID + stressedBool, data =bst_tg, FUN = sd))$shared #calculates each participants standard deviation shared amount
bst_tg_part_avg_stress$sharedSE <- (bst_tg_part_avg_stress$sharedSD / sqrt(nrow(bst_tg_part_avg_stress))) #calculates the standard error
bst_tg_part_avg_stress$sharedHighSE <- bst_tg_part_avg_stress$sharedAvg +  bst_tg_part_avg_stress$sharedSE #calculates the standard error high bound
bst_tg_part_avg_stress$sharedLowSE <- bst_tg_part_avg_stress$sharedAvg - bst_tg_part_avg_stress$sharedSE #calculates the standard error low bound

#for the trust rating
bst_tr_part_avg_stress <- aggregate(rating ~ subjectID + stressedBool, data = bst_tr, FUN = mean) #aggregates the trust rating task for each participant
names(bst_tr_part_avg_stress)[names(bst_tr_part_avg_stress) == "rating"] <- "ratingAvg"
bst_tr_part_avg_stress$ratingSD <- aggregate(rating ~ subjectID + stressedBool, data = bst_tr, FUN = mean)$rating #calculates each particpant's standard deviation shared amount
bst_tr_part_avg_stress$ratingSE <- (bst_tr_part_avg_stress$ratingSD / sqrt(nrow(bst_tr_part_avg_stress)))
bst_tr_part_avg_stress$ratingHighSE <- bst_tr_part_avg_stress$ratingAvg + bst_tr_part_avg_stress$ratingSE #calculates the standard error high bound
bst_tr_part_avg_stress$ratingLowSE <- bst_tr_part_avg_stress$ratingAvg - bst_tr_part_avg_stress$ratingSE #calculates the standard error low bound

#compiles all the averaged participant data into one spot
bst_t_compiled_part_avg_stress <- merge(bst_tg_part_avg_stress, bst_tr_part_avg_stress, by = c('subjectID', 'stressedBool'))
bst_t_compiled_part_avg_stress <- merge(bst_t_compiled_part_avg_stress, bst_bath, by = 'subjectID')

bst_t_compiled_part_avg_stress_pss <- merge(bst_t_compiled_part_avg_stress, bst_pss, by = 'subjectID')

#cleans up the strictly unnecessary DFs and vars
rm(bst_bathOrder)
rm(bst_bathPleasantness)
rm(bst_tg_part_avg)
rm(bst_tg_part_avg_stress)
rm(bst_tr_part_avg)
rm(bst_tr_part_avg_stress)

#you could also remove these if you really feel the need, the data is contained in other DFs, but I keep them for clarity in later analysis
#rm(bst_pss)
#rm(bst_bath)
#rm(bst_bath_pss)



#### BIAS ####

##IMPLICIT Bias


#AMP

amp_csv <- file.path(config$path$data$current, config$csvs$amp)
bst_amp <- read.csv(amp_csv) #reads in AMP data

#renaming columns for clarity
names(bst_amp)[names(bst_amp) == "RT"] <- "responseTime"
names(bst_amp)[names(bst_amp) == "stimulusRace"] <- "stimulusRace_0w_1b_2o"
names(bst_amp)[names(bst_amp) == "response"] <- "unPleasant0_Pleasant1"
names(bst_amp)[names(bst_amp) == "condition"] <- "PleasOnLeft0_PleasOnRight1"
names(bst_amp)[names(bst_amp) == "session"] <- "amp1_amp2"

amp_rt_mean <- aggregate(responseTime ~ subjectID, data = bst_amp, FUN=mean)
amp_rt_sd <- aggregate(responseTime ~ subjectID, data = bst_amp, FUN=sd)
bst_amp_sum <- merge(amp_rt_mean, amp_rt_sd)

amp_list <- list(amp_rt_mean, amp_rt_sd)
amp_reduced <- Reduce(function(x, y) merge(x, y, all.x=TRUE), amp_list)



#IAT

iat_csv <- file.path(config$path$data$current, config$csvs$iat)
bst_iat <- read.csv(iat_csv) #reads in iat data


#EXPLICIT Bias

#MRS (Modern Racism Scale)
#Measures how much you agree/disagree with statements on race
mrs_csv <- file.path(config$path$data$current, config$csvs$mrs)
bst_mrs <- read.csv(mrs_csv)

#remove participant 1 (which was a trial run)
bst_mrs <- bst_mrs[-c(1), ]

#Reverse code 1st questionnaire item only
bst_mrs$Q1_Easy_Understand_Recode = recode(bst_mrs$Q1_Easy_Understand, '-2=2; -1=1; 0=0; 1=-1; 2=-2')
#sum MRS
bst_mrs$mrsSum <- (bst_mrs$Q1_Easy_Understand_Recode + bst_mrs$Q2_Segregation_Influence + bst_mrs$Q3_Too_Demanding + bst_mrs$Q4_Economical_Help + bst_mrs$Q5_Press_Affinity + bst_mrs$Q6_Push_Unwanted + bst_mrs$Q7_Discim_Not_Prob)

bst_mrs_sum <- bst_mrs[c(1,11) ]



#SRS (Symbolic Racism Scale)
#Measures "your thoughts" regarding race
srs_csv <- file.path(config$path$data$current, config$csvs$srs)
bst_srs <- read.csv(srs_csv)

#remove participant 1 (which was a trial run)
bst_srs <- bst_srs[-c(1), ]

#Reverse code items 1, 2, 4, 8
bst_srs$Q1_try_more_recode = recode(bst_srs$Q1_try_more, '1=4; 2=3; 3=2; 4=1')
bst_srs$Q2_other_minorities_recode = recode(bst_srs$Q2_other_minorities, '1=4; 2=3; 3=2; 4=1')
bst_srs$Q4_blacks_responsible_recode = recode(bst_srs$Q4_blacks_responsible, '1=4; 2=3; 3=2; 4=1')
bst_srs$Q8_more_than_deserve_recode = recode(bst_srs$Q8_more_than_deserve,'1=4; 2=3; 3=2; 4=1')

#Reverse code item 3 (has only 3 response choices)
bst_srs$Q3_push_too_hard_recode = recode(bst_srs$Q3_push_too_hard, '1=3; 2=1; 3=2')

#sum SRS
bst_srs$srsSum <- (bst_srs$Q1_try_more_recode + bst_srs$Q2_other_minorities_recode + bst_srs$Q3_push_too_hard_recode + bst_srs$Q4_blacks_responsible_recode + bst_srs$Q5_limit_chances + bst_srs$Q6_slavery_difficulty + bst_srs$Q7_less_than_deserve + bst_srs$Q8_more_than_deserve_recode)

bst_srs_sum <- bst_srs[c(1,16) ]



#IMS-EMS (Internal and External Motivation to Respond Without Prejudice)
#Measures feelings towards statements on race
ims_ems_csv <- file.path(config$path$data$current, config$csvs$ims_ems)
bst_ims_ems <- read.csv(ims_ems_csv)
# NOTE - missing participant 43's IMS-EMS, not in scanned docs either

#remove participant 1 (which was a trial run)
bst_ims_ems <- bst_ims_ems[-c(1), ]

#Reverse code item 7
bst_ims_ems$Q7_StereotypesOK_recode = recode(bst_ims_ems$Q7_StereotypesOK, '1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1')

#sum IMS-EMS
bst_ims_ems$imsEmsSum <- (bst_ims_ems$Q1_Try_to_be_PC + bst_ims_ems$Q2_HideThoughts + bst_ims_ems$Q3_OthersAngry + bst_ims_ems$Q4_AvoidDisapproval + bst_ims_ems$Q5_Due2Pressure +
                            bst_ims_ems$Q6_PersonallyImp + bst_ims_ems$Q7_StereotypesOK_recode + bst_ims_ems$Q8_PersonallyMotiv + bst_ims_ems$Q9_StereotypesWrong + bst_ims_ems$Q10_SelfConcept)

bst_ims_ems$EmsSum <- (bst_ims_ems$Q1_Try_to_be_PC + bst_ims_ems$Q2_HideThoughts + bst_ims_ems$Q3_OthersAngry + bst_ims_ems$Q4_AvoidDisapproval + bst_ims_ems$Q5_Due2Pressure)
bst_ims_ems$ImsSum <- (bst_ims_ems$Q6_PersonallyImp + bst_ims_ems$Q7_StereotypesOK_recode + bst_ims_ems$Q8_PersonallyMotiv + bst_ims_ems$Q9_StereotypesWrong + bst_ims_ems$Q10_SelfConcept)

bst_ims_ems$EmsImsDiff <- (bst_ims_ems$EmsSum - bst_ims_ems$ImsSum) #calculates the difference in EMS and IMS scores per participant
#NEG indicates more internally motivated to be less biased, POS score indicates more externally motivated to be less biased

bst_ims_ems_sum <- bst_ims_ems[c(1,14:17) ]



#CM (Contact Measures)
#Measures contact with same/other race
cm_csv <- file.path(config$path$data$explicit, config$csvs$cm)
bst_cm <- read.csv(cm_csv)

#remove participant 1 (which was a trial run)
bst_cm <- bst_cm[-c(1), ]


#Recode character values as integers for CM items 1-6
bst_cm$Q1_close_white_recode  = recode(bst_cm$Q1_close_white , "'0'=0; '1_2'=1; '3_4'=2; '5_9'=3; '10+'=4")
bst_cm$Q2_aquaint_white_recode  = recode(bst_cm$Q2_aquaint_white , "'0'=0; '1_2'=1; '3_4'=2; '5_9'=3; '10+'=4")
bst_cm$Q3_dated_white_recode  = recode(bst_cm$Q3_dated_white , "'0'=0; '1_2'=1; '3_4'=2; '5_9'=3; '10+'=4")
bst_cm$Q4_close_black_recode  = recode(bst_cm$Q4_close_black , "'0'=0; '1_2'=1; '3_4'=2; '5_9'=3; '10+'=4")
bst_cm$Q5_aquaint_black_recode  = recode(bst_cm$Q5_aquaint_black , "'0'=0; '1_2'=1; '3_4'=2; '5_9'=3; '10+'=4")
bst_cm$Q6_dated_black_recode  = recode(bst_cm$Q6_dated_black , "'0'=0; '1_2'=1; '3_4'=2; '5_9'=3; '10+'=4")

#Recode character values as integers for CM items 7-9
bst_cm$Q7_environ_USR_recode  = recode(bst_cm$Q7_environ_USR , "'urb'=0; 'sub'=1; 'rur'=2")
bst_cm$Q8_environ_race_diverse_recode  = recode(bst_cm$Q8_environ_race_diverse , "'no'=0; 'yes'=1")
bst_cm$Q9_envir_cult_diverse_recode  = recode(bst_cm$Q9_envir_cult_diverse , "'no'=0; 'yes'=1")

#recode 10-17 as percentages?

#recode character self-report race/ethnicity values
#NOTE: 5 represents multiple entries (i.e., "mixed race" and "black")
bst_cm <- mutate(bst_cm, w0_his1_as2_bl3_birac4_mult5 = ifelse(Race_Eth_Self_Report=="Wht", 0,
                                                         ifelse(Race_Eth_Self_Report=="Hispan_Latin", 1,
                                                                ifelse(Race_Eth_Self_Report=="AsianAm_PacIsl", 2,
                                                                       ifelse(Race_Eth_Self_Report=="Black_Am", 3,
                                                                              ifelse(Race_Eth_Self_Report=="BiRac_MultiRac", 4, 5))))))




#### WIDE DATA ####

#create a data frame with a basic version of subject-level "wide" data

bst_wide_list <- list(STRESS, bst_mrs_sum, bst_srs_sum, bst_ims_ems_sum, amp_rt_mean)

#merge all data frames together
bst_wide <- Reduce(function(x, y) merge(x, y, all.x=TRUE, all.y=TRUE), bst_wide_list)



#Working Notes for creating the Wide data frame (erase when done)

#bst_wide <- bst_wide_reduced[c(1,11,25) ]

#From stress
  # chronic - add PSS score
  #         - add PSS median split
  # acute   - bath unpleasantness
  #         - bath order
  #         - bath rating difference (on day of stress vs control)
  #         - stressed bool
#From trust
  # trust perception
  #         - average ratings
  #         - average RTs
  #         - average per ...
  # trust behavior
  #         - task order
  #         - average RTs
  #         - stressed book
  #         - prev trial shared amount
  #         - prev trial feedback
  #         - shared/not bool
  #         - shared amount avg.
#From Bias
  # implic. - add average IAT score
  #         - add average AMP score
  # explic. - SRS
  #         - MRS
  #         - IMS-EMS
  #         - Contact Measures

#Working Notes for creating the Wide data frame (erase when done)
