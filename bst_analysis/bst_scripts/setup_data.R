#-loads up a series of data frames of the stress constructs and trust constructs for BST for later analysis
#-aggregates data into additional data frames
#-performs coding and computation of variables

#loads up the necessary libraries
library(plyr)
library(dplyr)
library(car)
library(IATanalytics)

#Color Blind Palette for graphing
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##Start Data Retrieval, Cleaning, Formatting: ##

# STRESS ########

#setting Up Stress Data Frames#

## Chronic Stress Measures ########

### PSS Subj-Level DF ########

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


## Acute Stress Measures ########

### Bath Rating DFs ########

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

#### Subj-Level Bath ####

#creates truncated Bath order & Bath rating DF 
# includes subj ID, diffUnPleanantnessRating, day2bool_0control_1stress
Stress_Acute <- bath[c(1,6:7) ]
#QUESTION : Do we want stress and control unpleasantness ratings?

### Cortisol DFs ########

#### Trial Level Cort ####

#Loads in cortisol .csv file
cort_csv <- file.path(config$path$data$cortisol, config$csvs$cort)
cort <- read.csv(cort_csv) #reads in the .csv
names(cort)[names(cort) == "Subject_ID"] <- 'subjectID' #renames the subject id column to maintain consistency across DFs
cort[cort == ""] <- NA

# Cortisol - split sample_ID into components - ALL participants #

split_sample_ID <- strsplit(cort$sample_ID, "_") 

# Convert list to dataframe
cort_split <- do.call(rbind, split_sample_ID)
cort_split <- as.data.frame(cort_split[,2:3])
names(cort_split) <- c("day", "sample")

# Combine with original dataframe
cort <- cbind(cort, cort_split)


#### Subj-Level Cort ####


##### Creating Subj-Level DF #####

# Mean cortisol values (per subject)
subj_mean_cort <- aggregate(cortisol_mean_nmol_to_l ~ subjectID, data = cort, FUN = mean, na.rm = TRUE)
# Q: Do any subject's have extreme cortisol mean values?
# A: Subject 23 has a very high mean cortisol value
#    (roughly 6 s.d. above the group mean: 11.37; group mean is 2.00, s.d. is 1.66; 6 s.d.'s above the mean is 11.98)
hist(subj_mean_cort$cortisol_mean_nmol_to_l)
# Note: most between 0-2 (good) with a couple of high readings

# Overall, mean cortisol subject-level readings
mean(subj_mean_cort$cortisol_mean_nmol_to_l) # 2.00
sd(subj_mean_cort$cortisol_mean_nmol_to_l) # 1.66

# Mean cort_coeff_of_variance_as_percent values (per subject)
subj_mean_cort_COV <- aggregate(cort_coeff_of_variance_as_percent ~ subjectID, data = cort, FUN = mean, na.rm = TRUE)
# Q: Do any subject's have extreme cortisol COV mean values?
# A: Range 1.825 to 14.2375
hist(subj_mean_cort_COV$cort_coeff_of_variance_as_percent)
# normal distribution

# Overall, mean cort_coeff_of_variance_as_percent subject-level 
mean(subj_mean_cort_COV$cort_coeff_of_variance_as_percent) # 7.30
sd(subj_mean_cort_COV$cort_coeff_of_variance_as_percent) # 2.87

#Creates truncated cortisol DF for use in subj-level Subj-level Stress DF
cort_subj_level_temp <- list(subj_mean_cort, subj_mean_cort_COV)
cort_subj_level <- Reduce(function(x, y) merge(x, y, all.x=TRUE, all.y=TRUE), cort_subj_level_temp)


##### Sample Assay Consistency Check #####
# How consistent were the duplicate analyses of a given sample? 
assay_difference_value = cort$cort_1_value - cort$cort_2_value;

sum(is.na(assay_difference_value)) # 13 samples do not have duplicate testing values
  # i.e. for 13 SAMPLES, we have a cort1 value, but not a cort2 value

hist(assay_difference_value) # Not a particularly useful measure, b/c doesn't take into account
# The overall value of the assay (i.e. a larger discrepancy is ok at larger values)

hist(cort$cort_coeff_of_variance_as_percent, breaks = 20)
#   Most are < 20 (~96%)
#   Few are > 20 (~4%, or 12 samples)

# Where are these high CV samples?
plot(cort$cort_1_value, cort$cort_2_value)
lines(x = c(0, 30), y = c(0, 30), lty = 'dashed', col = "lightblue", lwd = 2)
points(cort$cort_1_value[cort$cort_coeff_of_variance_as_percent > 20], cort$cort_2_value[cort$cort_coeff_of_variance_as_percent > 20], col = 'red', pch = 5)
# Most (11) are small values (< 2.5), and most have cort 1 > cort 2 (why? don't know). 
# One with highest CV value is NOT the highest cort value (in absolute terms).

# ANSWER: Pretty consistent; scatterplot indicates close clustering. Probably good to go.


##### Cortisol Reorganization #####
# Reshape the cort object into a 4 (sample) x 2 (day) x 2 (condition [1 = ctrl, 2 = strs]) x N (subjects) three dimensional object
cort_subjectIDs = unique(cort$subjectID);
cort_mtx = array(data = NA, dim = c(4, 2, 2, length(cort_subjectIDs)))

for (samp_num in 1:4){
  for (day_num in 1:2){
    for (subj_num in 1:length(cort_subjectIDs)){
      
      subjID = cort_subjectIDs[subj_num]; # extract subject ID
      
      # Use the bath object to pull out the boolean values for stress
      tmp_condition = as.numeric(bathOrder[bathOrder$subjectID == subjID,day_num+1] == 'STRESS') # gives 1 if stress, 0 if control

      ind = (cort$sample == samp_num) & 
        (cort$day == day_num) & 
        (cort$subjectID == cort_subjectIDs[subj_num]);
      
      if(any(ind)){

        cort_mtx[samp_num, day_num, tmp_condition + 1, subj_num] =
          cort$cortisol_mean_nmol_to_l[
              (cort$sample == samp_num) & 
              (cort$day == day_num) & 
              (cort$subjectID == cort_subjectIDs[subj_num])];
      }
    }
  }
}

# Add Stress condition information to `cort` df for analytic purposes
cort$control0stress1 = NA;

for(subj_num in 1:length(cort_subjectIDs)){
  subjID = cort_subjectIDs[subj_num]; # extract subject ID
  
  # Use the bath object to pull out the boolean values for stress
  tmp_day1 = as.numeric(bathOrder$Day.1[bathOrder$subjectID == subjID] == 'STRESS') # gives 1 if stress, 0 if control
  tmp_day2 = as.numeric(bathOrder$Day.2[bathOrder$subjectID == subjID] == 'STRESS') 
  
  # where do we propose putting these values
  ind_day1 = (cort$subjectID == subjID) & (cort$day == 1); 
  ind_day2 = (cort$subjectID == subjID) & (cort$day == 2);
  
  if (any(ind_day1)){ # if this index exists
    cort$control0stress1[ind_day1] = tmp_day1
  }
  if (any(ind_day2)){ # if this index exists
    cort$control0stress1[ind_day2] = tmp_day2
  }
}

cort$day = as.numeric(cort$day) # it's in there as a string
cort$day_diff = cort$day*2-3 # -1 for day 1, +1 for day 2

cort$sample = as.numeric(cort$sample) # also in there as a string!


#Create new sum function that accounts for NA removal issues in "apply":
sumna <- function(x) {
  if(any(is.finite(x))) {
    sum_val = sum(x, na.rm = TRUE) 
  } else {
    sum_val = NA
  }
  return(sum_val)
}



## Subj-Level Stress ########

# STRESS subject-level data frame with ALL participants
Stress_Subj_noCort <- merge(Stress_Acute, Stress_Chronic, by = "subjectID", all = TRUE)
Stress_Subj_Level_wCort <- merge(Stress_Subj_noCort, cort_subj_level, by = "subjectID", all = TRUE)

# STRESS subject-level dataframe with participants who completed BOTH days of the experiment
Stress_Subj_Level <- Stress_Subj_Level_wCort[!is.na(Stress_Subj_Level_wCort$diffUnPleasantnessRating), ]

# Excludes those missing PSS
Stress_Subj_Level_No_PSS <- Stress_Subj_Level[!is.na(Stress_Subj_Level$pssSum), ]

# TO:DO Means cort sample 1 to 3 differences by subject to put in subject-level df
# cort_rdg_1_to_3_diff_by_subj = apply(cort_mtx, c(1, 2), mean, na.rm = TRUE)
# cort_rdg_1_to_3_diff_by_subj = (apply(cort_mtx[3,,2,], 1, sumna) - apply(cort_mtx[1,,2,], 1, sumna)) #Stress


## To-do Stress ########

# TO-DO: 
# (1) COMPLETE. Extract sampleID: 
# (2) COMPLETE. Address one participant that has very high cort reading: (checked cort data overall)
# (3) COMPLETE. Create day/sample at subj-level: 
# (4) _____ Create mean cort sample 1 to 3 differences by subject to put in subject-level df




# TRUST ########

#setting Up Trust Data Frames#

## Trust Game Measures ########

### Trust Game DFs ########

tg_csv <- file.path(config$path$data$current, config$csvs$tg)
trustGame <- read.csv(tg_csv) #reads in trust game data

#renaming columns for clarity
names(trustGame)[names(trustGame) == "condition"] <- "taskOrder"
names(trustGame)[names(trustGame) == "RT"] <- "responseTime_TG"
names(trustGame)[names(trustGame) == "partnerRace"] <- "partnerRace_0w_1b_2o"


# Create subject level stressed bool with stress data & Trust Game data

subjectIDs = unique(trustGame$subjectID);
number_of_subjects = length(subjectIDs);
trustGame$stressedBool = NA;
trustGame$pssSum = NA;
trustGame$pssMedianSplit = NA;
trustGame$bathRating = NA;

for (s in 1:number_of_subjects){
  sID = subjectIDs[s];
  
  tmp_stress_conditions = c(1-Stress_Subj_Level$day2bool_0control_1stress[s], Stress_Subj_Level$day2bool_0control_1stress[s]);

  for (d in 1:2){
    trust_ind = (trustGame$subjectID == sID) & (trustGame$day == d);
    trustGame$stressedBool[trust_ind] = tmp_stress_conditions[d]; # condition binary (0 = control, 1 = stress)
    trustGame$pssSum[trust_ind] = Stress_Subj_Level$pssSum[s]; # include PSS sum score
    trustGame$pssMedianSplit[trust_ind] = Stress_Subj_Level$pssMedianSplit[s]; # include median split based on PSS
    
    if(tmp_stress_conditions[d] == 0){
      trustGame$bathRating[trust_ind] = bath$controlUnpleasantnessRating[s]; # correct bath rating by day
    } else if(tmp_stress_conditions[d] == 1){
      trustGame$bathRating[trust_ind] = bath$stressUnpleasantnessRating[s];
    }
  }
}


# Data Quality: Response Times
cutoff_upper = 8 # 8 seconds
cutoff_lower = 0.1 # 100ms

# Data to eliminate
col_to_blank = c('shared','partnerChoice','received','responseTime_TG')

trials_to_remove = (trustGame$responseTime_TG <= cutoff_lower) | 
                   (trustGame$responseTime_TG >= cutoff_upper)

trustGame[trials_to_remove,col_to_blank] = NA;
trials_removed = array(dim = number_of_subjects)
percent_removed = array(dim = number_of_subjects)

for (s in 1:number_of_subjects){
  trials_removed[s] = sum(is.na(trustGame$responseTime_TG[trustGame$subjectID == subjectIDs[s]]))
  percent_removed[s] = trials_removed[s]/sum(trustGame$subjectID == subjectIDs[s])
}

# Some of these are very high! (30%, 37%) Should we remove those *people*?


##### Previous trial TG #####

#Calculate the previous trial sharing data
trustGame$prevTrialSharedAmt <- c(NA, trustGame$shared[-nrow(trustGame)]) #shifts column by 1, replacing the first element with a 0
trustGame$prevTrialSharedAmt[trustGame$cumTrialNum == 1] = NA # Ensure everyone's first trial has an NA prev-trial-shared-amount (because there was NOT a previous trial)
  # cumulative trial is the one to use here (as there are multiple blocks/person, and trialnumber resets on each block)

trustGame$prevTrialSharedBool = ifelse(trustGame$prevTrialSharedAmt > 0, 1, -1) #creates the boolean coding

#calculating feedback from previous trial
trustGame$prevTrialreceived = c(NA, trustGame$received[-nrow(trustGame)])
trustGame$prevTrialreceived[trustGame$cumTrialNum == 1] = NA

# Did the partner reciprocate any offered trust? (+1, -1, 0)
trustGame$reciprocatedTrust = 0; # zero if the participant does not trust and/or is the first trial
trustGame$reciprocatedTrust[trustGame$partnerChoice == 1] = 1 # +1 if the partner reciprocates trust
trustGame$reciprocatedTrust[trustGame$partnerChoice == 0] = -1 # -1 if the partner does NOT reciprocate trust

# make the previous-trial version
trustGame$prevTrialreciprocatedTrust = c(0, trustGame$reciprocatedTrust[-nrow(trustGame)])
trustGame$prevTrialreciprocatedTrust[trustGame$cumTrialNum == 1] = 0;

# Set up a categorical for whether or not a participant shared any money at all
trustGame$sharedBool <- ifelse(trustGame$shared == 0, 0, 1)

### MAKE PREVIOUS PARTNER REGRESSORS ###
# Previous Partner Race
trustGame$prevTrialpartnerRace_0w_1b_2o = c(NA, trustGame$partnerRace_0w_1b_2o[-nrow(trustGame)])
trustGame$prevTrialpartnerRace_0w_1b_2o[trustGame$cumTrialNum == 1] = NA

# Previous partner race (separate regressors)
trustGame$prevTrialpartnerWhite = trustGame$prevTrialpartnerRace_0w_1b_2o == 0
trustGame$prevTrialpartnerBlack = trustGame$prevTrialpartnerRace_0w_1b_2o == 1
trustGame$prevTrialpartnerOther = trustGame$prevTrialpartnerRace_0w_1b_2o == 2

# Previous partner black vs. white
trustGame$prevTrialwhiteVSblack = trustGame$prevTrialpartnerWhite - trustGame$prevTrialpartnerBlack
# Contrast regressor that captures difference in current trial behavior as a function of 
# the racial identity of the partner on the previous trial (+1 when was white, -1 when was black).


# GOAL: Make within-race previous-interaction variables
# e.g., if current trial is black interaction, then how did previous black interaction go (regardless
# of how far back it was), and same for current white interactions, etc. 
# - Reciprocation binary
# - received
# - shared amount
index_partnerWhite = which(trustGame$partnerRace_0w_1b_2o == 0)
index_partnerBlack = which(trustGame$partnerRace_0w_1b_2o == 1)
index_partnerOther = which(trustGame$partnerRace_0w_1b_2o == 2)

trustGame$past_White_Reciprocation = 0
trustGame$past_White_Received = 0
trustGame$past_White_Shared = 0

trustGame$past_Black_Reciprocation = 0
trustGame$past_Black_Received = 0
trustGame$past_Black_Shared = 0

trustGame$past_Other_Reciprocation = 0
trustGame$past_Other_Received = 0
trustGame$past_Other_Shared = 0

for (t in 2:length(index_partnerWhite)){
  if (trustGame$subjectID[index_partnerWhite[t]] != trustGame$subjectID[index_partnerWhite[t-1]]){
    next 
  } else {
    trustGame$past_White_Reciprocation[index_partnerWhite[t]] = trustGame$reciprocatedTrust[index_partnerWhite[t-1]];
    trustGame$past_White_Received[index_partnerWhite[t]] = trustGame$received[index_partnerWhite[t-1]];
    trustGame$past_White_Shared[index_partnerWhite[t]] = trustGame$shared[index_partnerWhite[t-1]];
  }
}

for (t in 2:length(index_partnerBlack)){
  if (trustGame$subjectID[index_partnerBlack[t]] != trustGame$subjectID[index_partnerBlack[t-1]]){
    next 
  } else {
    trustGame$past_Black_Reciprocation[index_partnerBlack[t]] = trustGame$reciprocatedTrust[index_partnerBlack[t-1]];
    trustGame$past_Black_Received[index_partnerBlack[t]] = trustGame$received[index_partnerBlack[t-1]];
    trustGame$past_Black_Shared[index_partnerBlack[t]] = trustGame$shared[index_partnerBlack[t-1]];
  }
}

for (t in 2:length(index_partnerOther)){
  if (trustGame$subjectID[index_partnerOther[t]] != trustGame$subjectID[index_partnerOther[t-1]]){
    next 
  } else {
    trustGame$past_Other_Reciprocation[index_partnerOther[t]] = trustGame$reciprocatedTrust[index_partnerOther[t-1]];
    trustGame$past_Other_Received[index_partnerOther[t]] = trustGame$received[index_partnerOther[t-1]];
    trustGame$past_Other_Shared[index_partnerOther[t]] = trustGame$shared[index_partnerOther[t-1]];
  }
}


##### Subj-Level Trust Game DF #####

# Mean/sd trust game reaction time values (per subject)
subj_mean_tG_rt <- aggregate(responseTime_TG ~ subjectID, data = trustGame, FUN = mean, na.rm = TRUE)
names(subj_mean_tG_rt)[names(subj_mean_tG_rt) == "responseTime_TG"] <- "mean_RT_TG"

subj_sd_tG_rt <- aggregate(responseTime_TG ~ subjectID, data = trustGame, FUN = sd, na.rm = TRUE)
names(subj_sd_tG_rt)[names(subj_sd_tG_rt) == "responseTime_TG"] <- "sd_RT_TG"

hist(subj_mean_tG_rt$mean_RT_TG)
# Q: Do any subject's have high response time mean values?
# A: Most subjects respond to a trust game interaction within < 3.5 seconds
#    Subject 50 has a mean response time of 5.10 for trust game interaction
#    (group mean trust game rt is 2.41, s.d. is 1.29)

# Overall, mean & sd of trust game response time (subject-level)
mean(subj_mean_tG_rt$mean_RT_TG) # 2.41
sd(subj_mean_tG_rt$mean_RT_TG) # 0.89

# Mean/sd trust game shared amounts (per subject)
subj_mean_tG_shared <- aggregate(shared ~ subjectID, data = trustGame, FUN = mean, na.rm = TRUE)
names(subj_mean_tG_shared)[names(subj_mean_tG_shared) == "shared"] <- "mean_shared_amount_TG"

subj_sd_tG_shared <- aggregate(shared ~ subjectID, data = trustGame, FUN = sd, na.rm = TRUE)
names(subj_sd_tG_shared)[names(subj_sd_tG_shared) == "shared"] <- "sd_shared_amount_TG"
# Q: Do any subject's have uniformity in their sharing behavior?
# A: Participants 27, 42, & 54 have 0 sd in trust game sharing, indicating they may be using a uniform approach to sharing.

#Creates truncated tg DF for use in Subj-level Trust DF
trust_game_subj_level <- merge(subj_mean_tG_shared, subj_mean_tG_rt, by = "subjectID", all = TRUE)
#TO ADD: 
# - task order ...
# - stressed bool  ...


#More complex subj-level DF includes SD of shared amount and reaction time per subject
trust_game_subj_level_full <- merge(trust_game_subj_level, subj_sd_tG_shared, by = "subjectID", all = TRUE)
trust_game_subj_level_full <- merge(trust_game_subj_level_full, subj_sd_tG_rt, by = "subjectID", all = TRUE)




## Trust Rating Measures ########

### Trust Rating DFs ########

tr_csv <- file.path(config$path$data$current, config$csvs$tr)
trustRating <- read.csv(tr_csv)

# Renaming columns for clarity
names(trustRating)[names(trustRating) == "response"] <- "trust_rating"
names(trustRating)[names(trustRating) == "condition"] <- "taskOrder"
names(trustRating)[names(trustRating) == "RT"] <- "responseTime_TR"
names(trustRating)[names(trustRating) == "partnerRace"] <- "partnerRace_0w_1b_2o"

# Combines trust rating data with bath data for calculation of variables and later analysis
#trustRating <- merge(trustRating, bath, by = "subjectID")

trustRating$stressedBool = NA;
trustRating$pssSum = NA;
trustRating$pssMedianSplit = NA;
trustRating$bathRating = NA;

for (s in 1:number_of_subjects){
  sID = subjectIDs[s];
  
  tmp_stress_conditions = c(1-Stress_Subj_Level$day2bool_0control_1stress[s], Stress_Subj_Level$day2bool_0control_1stress[s]);
  
  for (d in 1:2){
    trust_ind = (trustRating$subjectID == sID) & (trustRating$day == d);
    trustRating$stressedBool[trust_ind] = tmp_stress_conditions[d]; # condition binary (0 = control, 1 = stress)
    trustRating$pssSum[trust_ind] = Stress_Subj_Level$pssSum[s]; # include PSS sum score
    trustRating$pssMedianSplit[trust_ind] = Stress_Subj_Level$pssMedianSplit[s]; # include median split based on PSS
    
    if(tmp_stress_conditions[d] == 0){
      trustRating$bathRating[trust_ind] = bath$controlUnpleasantnessRating[s]; # correct bath rating by day
    } else if(tmp_stress_conditions[d] == 1){
      trustRating$bathRating[trust_ind] = bath$stressUnpleasantnessRating[s];
    }
  }
}

##### Previous trial TR #####

#Previous trial stuff for the trust rating task
trustRating$past_Rating <- c(NA, trustRating$trust_rating[-nrow(trustRating)]) #shifts the previous rating column down by 1
trustRating$past_Rating[trustRating$cumTrialNum == 1] = NA;


index_partnerWhite = which(trustRating$partnerRace_0w_1b_2o == 0)
index_partnerBlack = which(trustRating$partnerRace_0w_1b_2o == 1)
index_partnerOther = which(trustRating$partnerRace_0w_1b_2o == 2)

trustRating$past_White_Rating = 0
trustRating$past_Black_Rating = 0
trustRating$past_Other_Rating = 0

for (t in 2:length(index_partnerWhite)){
  if (trustRating$subjectID[index_partnerWhite[t]] != trustRating$subjectID[index_partnerWhite[t-1]]){
    next 
  } else {
    trustRating$past_White_Rating[index_partnerWhite[t]] = trustRating$trust_rating[index_partnerWhite[t-1]];
  }
}

for (t in 2:length(index_partnerBlack)){
  if (trustRating$subjectID[index_partnerBlack[t]] != trustRating$subjectID[index_partnerBlack[t-1]]){
    next 
  } else {
    trustRating$past_Black_Rating[index_partnerBlack[t]] = trustRating$trust_rating[index_partnerBlack[t-1]];
  }
}

for (t in 2:length(index_partnerOther)){
  if (trustRating$subjectID[index_partnerOther[t]] != trustRating$subjectID[index_partnerOther[t-1]]){
    next 
  } else {
    trustRating$past_Other_Rating[index_partnerOther[t]] = trustRating$trust_rating[index_partnerOther[t-1]];
  }
}


##### Subj-Level Trust Rating DF #####

# Mean trust rating reaction time values (per subject)
subj_mean_tR_rt <- aggregate(responseTime_TR ~ subjectID, data = trustRating, FUN = mean, na.rm = TRUE) # M = 2.62
names(subj_mean_tR_rt)[names(subj_mean_tR_rt) == "responseTime_TR"] <- "mean_RT_TR"

subj_sd_tR_rt <- aggregate(responseTime_TR ~ subjectID, data = trustRating, FUN = sd, na.rm = TRUE) # M = 1.81
names(subj_sd_tR_rt)[names(subj_sd_tR_rt) == "responseTime_TR"] <- "sd_RT_TR"
# Note: Participants 11, 29, and 50 have high SD in trust ratings

hist(subj_mean_tR_rt$mean_RT_TR)
# Q: Do any subject's have high response time mean values?
# A: Most subjects give a trust rating within < 3 seconds.
#   Similar to trust interactions, Subject 50 has a mean response time of approx. 8 secs. for trust ratings
#   (group mean trust game rt is 2.62, s.d. is 1.81)

# Overall, mean response time, subject-level readings
mean(subj_mean_tR_rt$mean_RT_TR) # 2.62
sd(subj_mean_tR_rt$mean_RT_TR) # 1.15

# Mean trust ratings (per subject)
subj_mean_tR_ratings <- aggregate(trust_rating ~ subjectID, data = trustRating, FUN = mean, na.rm = TRUE) # M = 4.71
names(subj_mean_tR_ratings)[names(subj_mean_tR_ratings) == "trust_rating"] <- "mean_trust_rating"

subj_sd_tR_ratings <- aggregate(trust_rating ~ subjectID, data = trustRating, FUN = sd, na.rm = TRUE) # M = 1.00
names(subj_sd_tR_ratings)[names(subj_sd_tR_ratings) == "trust_rating"] <- "sd_trust_rating"

hist(subj_mean_tR_ratings$mean_trust_rating) #fairly normal distribution of subject-level average (M = 4.7) trust rating

#Creates truncated tr DF for use in Subj-level Trust DF
trust_rating_subj_level <- merge(subj_mean_tR_ratings, subj_mean_tR_rt, by = "subjectID", all = TRUE)

#DF includes SD of trust ratings and reaction time per subject
trust_rating_subj_level_full <- merge(trust_rating_subj_level, subj_sd_tR_ratings, by = "subjectID", all = TRUE)
trust_rating_subj_level_full <- merge(trust_rating_subj_level_full, subj_sd_tR_rt, by = "subjectID", all = TRUE)

## Subj-Level Trust ########

# --- Combining Trust Ratings & Trust Game Measures --- #

trust_subj_level_temp <- list(trust_game_subj_level, trust_rating_subj_level)
trust_subj_level <- Reduce(function(x, y) merge(x, y, all.x=TRUE, all.y=TRUE), trust_subj_level_temp)



## To-do Trust ########

# (1) I'd take the sd across everyone's means, and normalize that single number by the sqrt of the number of participants.
#   trustGame_part_avg$sharedHighSE <- trustGame_part_avg$sharedAvg +  trustGame_part_avg$sharedSE #calculates the standard error high bound
#   trustGame_part_avg$sharedLowSE <- trustGame_part_avg$sharedAvg - trustGame_part_avg$sharedSE #calculates the standard error low bound

# (2) #trustRating_part_avg$ratingHighSE <- trustRating_part_avg$ratingAvg + trustRating_part_avg$ratingSE #calculates the standard error high bound
#trustRating_part_avg$ratingLowSE <- trustRating_part_avg$ratingAvg - trustRating_part_avg$ratingSE #calculates the standard error low bound

# (3) Review from past script

# For the Trust Game:

#trustGame_part_avg_stress <- aggregate(shared ~ subjectID + stressedBool, data = trustGame, FUN = mean) #aggregates the trust game by participant
#names(trustGame_part_avg_stress)[names(trustGame_part_avg_stress) == "shared"] <- "sharedAvg"
#names(trustGame_part_avg_stress)[names(trustGame_part_avg_stress) == "stressedBool"] <- ""
#trustGame_part_avg_stress$sharedSD <- (aggregate(shared ~ subjectID + stressedBool, data =trustGame, FUN = sd))$shared #calculates each participants standard deviation shared amount
#trustGame_part_avg_stress$sharedSE <- (trustGame_part_avg_stress$sharedSD / sqrt(nrow(trustGame_part_avg_stress))) #calculates the standard error
#trustGame_part_avg_stress$sharedHighSE <- trustGame_part_avg_stress$sharedAvg +  trustGame_part_avg_stress$sharedSE #calculates the standard error high bound
#trustGame_part_avg_stress$sharedLowSE <- trustGame_part_avg_stress$sharedAvg - trustGame_part_avg_stress$sharedSE #calculates the standard error low bound

# For the Trust Rating

#trustRating_part_avg_stress <- aggregate(rating ~ subjectID + stressedBool, data = trustRating, FUN = mean) #aggregates the trust rating task for each participant
#names(trustRating_part_avg_stress)[names(trustRating_part_avg_stress) == "rating"] <- "ratingAvg"
#trustRating_part_avg_stress$ratingSD <- aggregate(rating ~ subjectID + stressedBool, data = trustRating, FUN = mean)$rating #calculates each particpant's standard deviation shared amount
#trustRating_part_avg_stress$ratingSE <- (trustRating_part_avg_stress$ratingSD / sqrt(nrow(trustRating_part_avg_stress)))
#trustRating_part_avg_stress$ratingHighSE <- trustRating_part_avg_stress$ratingAvg + trustRating_part_avg_stress$ratingSE #calculates the standard error high bound
#trustRating_part_avg_stress$ratingLowSE <- trustRating_part_avg_stress$ratingAvg - trustRating_part_avg_stress$ratingSE #calculates the standard error low bound

#  rethink "compiling"
#compiles all the averaged participant data into one spot
# t_compiled_part_avg_stress <- merge(trustGame_part_avg_stress, trustRating_part_avg_stress, by = c('subjectID', 'stressedBool'))
# t_compiled_part_avg_stress <- merge(t_compiled_part_avg_stress, bath, by = 'subjectID')
# t_compiled_part_avg_stress_pss <- merge(t_compiled_part_avg_stress, pss, by = 'subjectID')

# NOTE: once dfs are finalized, edit this section
#cleans up the strictly unnecessary DFs and vars
#rm(bst_bathOrder)
#rm(bst_bathPleasantness)
#rm(trustGame_part_avg)
#rm(trustGame_part_avg_stress)
#rm(trustRating_part_avg)
#rm(trustRating_part_avg_stress)



# ATTITUDES ######## 

## Implicit Attitudes ######## 

### AMP ########

amp_csv <- file.path(config$path$data$current, config$csvs$amp)
amp <- read.csv(amp_csv) #reads in AMP data

#renaming columns for clarity
names(amp)[names(amp) == "RT"] <- "responseTime_AMP"
names(amp)[names(amp) == "stimulusRace"] <- "stimulusRace_0w_1b_2o"
names(amp)[names(amp) == "response"] <- "amp_unPleasant0_Pleasant1"
names(amp)[names(amp) == "condition"] <- "PleasOnLeft0_PleasOnRight1"
names(amp)[names(amp) == "session"] <- "amp1_amp2"

amp_rt_mean <- aggregate(responseTime_AMP ~ subjectID, data = amp, FUN=mean)
amp_rt_sd <- aggregate(responseTime_AMP ~ subjectID, data = amp, FUN=sd)

amp_bathrating_mean <- aggregate(amp_unPleasant0_Pleasant1 ~ subjectID, data = amp, FUN=mean)
amp_bathrating_sd <- aggregate(amp_unPleasant0_Pleasant1 ~ subjectID, data = amp, FUN=sd)

#amp_sum <- merge(amp_rt_mean, amp_rt_sd, amp_bathrating_mean, amp_bathrating_sd)

#subject-level data frame
amp_list <- list(amp_rt_mean, amp_rt_sd, amp_bathrating_mean, amp_bathrating_sd)
amp_Subj_Level <- Reduce(function(x, y) merge(x, y, all.x=TRUE), amp_list)



### IAT ########

#library(IATScore)

iat_csv <- file.path(config$path$data$current, config$csvs$iat)
iat <- read.csv(iat_csv) #reads in iat data

# (1) Clean IAT - Remove from the dataset the pure practice blocks of the IAT 
# iat_wrangled used in IATanalytics function
iat_wrangled = filter(iat, trialtype != "PRAC")

iat_wrangled = iat_wrangled %>%
  arrange(subjectID, cumTrialNum) %>%
  group_by(subjectID) %>%
  mutate(trial_index = row_number())

iat_wrangled$blockNum[iat_wrangled$blockNum ==3] <- 1
iat_wrangled$blockNum[iat_wrangled$blockNum ==4] <- 2

# (2) Run the IATanalytics function ()

# Storage's function = IATanalytics(IAT, Trials, First)
# Note: default to have first trial as "congruent", 200 trials, IAT as df
# Updated to reflect 139 trials, first condition as "incongruent", and iat_wrangled as df

iat_results <- IATanalytics(IAT = iat_wrangled, Trials = 139, First="Incongruent")

iat_results <- IATanalytics(IAT = iat, Trials = 200, First="Incongruent")

#iat_results <- IATanalytics(data = iat_wrangled, 
                           #participant_id = "subjectID",
                           #block_name = "blockNum",
                           #trial_latency = "RT",
                           #error = "correct")



#REVIEW w PSH ---- Attempting  to loop through subject level IAT results

# create function to get d-score per subject
get_IAT_dscore_per_subject <- function(iat_results) {
  result <- IATanalytics(
    IAT = iat_results,
    Trials = 139,
    First = "Incongruent"
    )
  return(result$D_score)
}


#version 1 dplyr
#results_IAT_dscore_per_subject <- iat_wrangled %>%
  #group_by(subjectID) %>%
  #summarize(D_score = get_IAT_dscore_per_subject(cur_data()), .groups = 'drop') %>%
  #filter(!is.na(D_score))

#only prints subjectIDs
#print(results_IAT_dscore_per_subject)

#version2 looping
results_IAT_dscore_per_subject2 <- data.frame(subjectID = integer(), D_score = numeric(), stringsAsFactors = FALSE)

unique_subjectIDs <- unique(iat$subjectID)

for (subject in unique_subjectIDs) {
  #subset
  subject_data <- subset(iat, subjectID == subject)
  
  #compute D-score
  d_score <- tryCatch(
    get_IAT_dscore_per_subject(subject_data),
    error = function(e) NA #handle NAs
  )
  
  if (is.numeric(d_score)){
    results_IAT_dscore_per_subject2 <- rbind(results_IAT_dscore_per_subject2, data.frame(subjectID = subject, D_score = d_score))
    } else {
    warning(paste("Unexpected D-score for subject:"))
    }
  
}

print(results_IAT_dscore_per_subject2)
#alternative version of getting d-scores

#iat_wrangled_Dscoring <- iat_wrangled[, c("subjectID", "blockNum", "RT", "correct")]

#iat_wrangled_Dscoring <- iat_wrangled_Dscoring %>%
# rename(participant = subjectID,
#        block = blockNum,
#       latency = RT,
#       correct = correct)



## Explicit Attitudes ######## 


### MRS ########
# (Modern Racism Scale)
# Measures how much you agree/disagree with statements on race

mrs_csv <- file.path(config$path$data$current, config$csvs$mrs)
mrs <- read.csv(mrs_csv)

#remove participant 1 (which was a trial run)
mrs <- mrs[-c(1), ]

#Reverse code 1st questionnaire item only
mrs$Q1_Easy_Understand_Recode = recode(mrs$Q1_Easy_Understand, '-2=2; -1=1; 0=0; 1=-1; 2=-2')


#sum MRS
mrs$mrsSum <- (mrs$Q1_Easy_Understand_Recode + mrs$Q2_Segregation_Influence + mrs$Q3_Too_Demanding + mrs$Q4_Economical_Help + mrs$Q5_Press_Affinity + mrs$Q6_Push_Unwanted + mrs$Q7_Discim_Not_Prob)


#mean MRS
mrs$mrsMean <-mrs$mrsSum/7


#subject-level data frame
mrs_Subj_Level <- mrs[c(1,11:12) ]



### SRS ########
# (Symbolic Racism Scale)
# Measures "your thoughts" regarding race

srs_csv <- file.path(config$path$data$current, config$csvs$srs)
srs <- read.csv(srs_csv)

#remove participant 1 (which was a trial run)
srs <- srs[-c(1), ]

#Reverse code items 1, 2, 4, 8
srs$Q1_try_more_recode = recode(srs$Q1_try_more, '1=4; 2=3; 3=2; 4=1')
srs$Q2_other_minorities_recode = recode(srs$Q2_other_minorities, '1=4; 2=3; 3=2; 4=1')
srs$Q4_blacks_responsible_recode = recode(srs$Q4_blacks_responsible, '1=4; 2=3; 3=2; 4=1')
srs$Q8_more_than_deserve_recode = recode(srs$Q8_more_than_deserve,'1=4; 2=3; 3=2; 4=1')

#Reverse code item 3 (has only 3 response choices)
srs$Q3_push_too_hard_recode = recode(srs$Q3_push_too_hard, '1=3; 2=1; 3=2')


#sum SRS (8-31 range)
srs$srsSum <- (srs$Q1_try_more_recode + srs$Q2_other_minorities_recode + srs$Q3_push_too_hard_recode + srs$Q4_blacks_responsible_recode + srs$Q5_limit_chances + srs$Q6_slavery_difficulty + srs$Q7_less_than_deserve + srs$Q8_more_than_deserve_recode)


#mean SRS
srs$srsMean <-srs$srsSum/8


#subject-level data frame
srs_Subj_Level <- srs[c(1,16:17) ]



### IMS-EMS ########
# (Internal and External Motivation to Respond Without Prejudice)
# Measures feelings towards statements on what motivates to respond without prejudice

ims_ems_csv <- file.path(config$path$data$current, config$csvs$ims_ems)
ims_ems <- read.csv(ims_ems_csv)

# NOTE - missing participant 43's IMS-EMS, not in scanned docs either

# Remove participant 1 (which was a trial run)
ims_ems <- ims_ems[-c(1), ]

#NOTE: Question #7 - "According to my personal values, using stereotypes about Black people is OK" - in the survey is reverse-coded. 

# Reverse code item 7
ims_ems$Q7_StereotypesOK_recode = recode(ims_ems$Q7_StereotypesOK, '1=10; 2=9; 3=8; 4=7; 5=6; 6=5; 7=4; 8=3; 9=2; 10=1')

# NOTE: Questions 1-5 are external motivation items, while questions 6-10 are internal motivation items.


#IMS and EMS Sums
ims_ems$EmsSum <- (ims_ems$Q1_Try_to_be_PC + ims_ems$Q2_HideThoughts + ims_ems$Q3_OthersAngry + ims_ems$Q4_AvoidDisapproval + ims_ems$Q5_Due2Pressure)
ims_ems$ImsSum <- (ims_ems$Q6_PersonallyImp + ims_ems$Q7_StereotypesOK_recode + ims_ems$Q8_PersonallyMotiv + ims_ems$Q9_StereotypesWrong + ims_ems$Q10_SelfConcept)

#IMS/EMS Sum Difference
ims_ems$EmsImsSumDiff <- (ims_ems$EmsSum - ims_ems$ImsSum) #calculates the difference in EMS and IMS scores per participant
# Note: More negative values indicate more internally motivated to be less biased, more positive scores indicate more externally motivated to be less biased


#IMS and EMS Means
ims_ems$EmsMean <- (ims_ems$Q1_Try_to_be_PC + ims_ems$Q2_HideThoughts + ims_ems$Q3_OthersAngry + ims_ems$Q4_AvoidDisapproval + ims_ems$Q5_Due2Pressure)/5
ims_ems$ImsMean <- (ims_ems$Q6_PersonallyImp + ims_ems$Q7_StereotypesOK_recode + ims_ems$Q8_PersonallyMotiv + ims_ems$Q9_StereotypesWrong + ims_ems$Q10_SelfConcept)/5

#IMS/EMS Mean Difference
ims_ems$EmsImsMeanDiff <- (ims_ems$EmsMean - ims_ems$ImsMean) #calculates the difference in EMS and IMS scores per participant
# Note: More negative values indicate more internally motivated to be less biased, more positive scores indicate more externally motivated to be less biased


#subject-level data frame
ims_ems_Subj_Level <- ims_ems[c(1,14:19) ]
# Includes IMS & EMS sums and averages, as well as differences between EMS/IMS for the sums and averages.



## Contact Measures (CM) ######## 
# Measures contact with same/other race

cm_csv <- file.path(config$path$data$explicit, config$csvs$cm)
cm <- read.csv(cm_csv)

#remove participant 1 (which was a trial run)
cm <- cm[-c(1), ]


# Recode all variables for CM dataframe 

# Recode character values as integers for CM items 1-6
cm$Q1_close_white_recode  = recode(cm$Q1_close_white , "'0'=0; '1_2'=1; '3_4'=2; '5_9'=3; '10+'=4")
cm$Q2_aquaint_white_recode  = recode(cm$Q2_aquaint_white , "'0'=0; '1_2'=1; '3_4'=2; '5_9'=3; '10+'=4")
cm$Q3_dated_white_recode  = recode(cm$Q3_dated_white , "'0'=0; '1_2'=1; '3_4'=2; '5_9'=3; '10+'=4")
cm$Q4_close_black_recode  = recode(cm$Q4_close_black , "'0'=0; '1_2'=1; '3_4'=2; '5_9'=3; '10+'=4")
cm$Q5_aquaint_black_recode  = recode(cm$Q5_aquaint_black , "'0'=0; '1_2'=1; '3_4'=2; '5_9'=3; '10+'=4")
cm$Q6_dated_black_recode  = recode(cm$Q6_dated_black , "'0'=0; '1_2'=1; '3_4'=2; '5_9'=3; '10+'=4")

# Recode character values as integers for CM items 7-9
cm$Q7_environ_USR_recode  = recode(cm$Q7_environ_USR , "'urb'=2; 'sub'=1; 'rur'=0")
cm$Q8_environ_race_diverse_recode  = recode(cm$Q8_environ_race_diverse , "'no'=0; 'yes'=1")
cm$Q9_envir_cult_diverse_recode  = recode(cm$Q9_envir_cult_diverse , "'no'=0; 'yes'=1")

# Recode character self-report race/ethnicity values
#NOTE: 5 represents multiple entries (i.e., "mixed race" and "black")
cm <- mutate(cm, w0_his1_as2_bl3_birac4_mult5 = ifelse(Race_Eth_Self_Report=="Wht", 0,
                                                         ifelse(Race_Eth_Self_Report=="Hispan_Latin", 1,
                                                                ifelse(Race_Eth_Self_Report=="AsianAm_PacIsl", 2,
                                                                       ifelse(Race_Eth_Self_Report=="Black_Am", 3,
                                                                              ifelse(Race_Eth_Self_Report=="BiRac_MultiRac", 4, 5))))))


# Create Numerical dataframe for CM
# for use in PCA
cm_num = cm[,c(12:19,21:29)]


#subject-level data frame
cm_Subj_Level <- cm[,c(1,30)] #update later
# includes subject ID and self-report race-ethnicity item

# cm_Subj_Level <- cm[,c(1,12:19,21:30)] #update later with what to include from PCA


## Subj-Level Attitudes ########

# Add


## To-do Attitudes ########

# (1) Process IAT data.


# WIDE DATA ########

#create a data frame with a basic version of subject-level "wide" data
bst_wide_list <- list(Stress_Subj_Level, trust_subj_level, mrs_Subj_Level, srs_Subj_Level, ims_ems_Subj_Level, amp_Subj_Level, cm_Subj_Level)
#Note: cm_Subj_Level df only includes self-identified subject race-ethnicity.  Items to include in subj_level df can be updated after PCA.

#merge all data frames together
BST_Subj_Level_DF <- Reduce(function(x, y) merge(x, y, all.x=TRUE, all.y=TRUE), bst_wide_list)


#Working Notes for creating the Wide data frame (erase when done)

#bst_wide <- bst_wide_reduced[c(1,11,25) ]

#From stress
  # chronic - add PSS score X
  #         - add PSS median split X
  # acute   - bath unpleasantness __? Do we want this for stress or control or both?
  #         - bath order __? Do we need this if we have the stressed bool?
  #         - bath rating difference (on day of stress vs control) X
  #         - stressed bool X
#From trust
  # trust perception
  #         - average ratings X
  #         - average RTs X
  #         - average per ... __? Do this per race or ..?
  # trust behavior
  #         - task order ...
  #         - average RTs X
  #         - stressed bool  ...
  #         - prev trial shared amount __? How best might we do this at the subj-level?
  #         - prev trial feedback __? How best might we do this at the subj-level?
  #         - shared/not bool __? How best might we do this at the subj-level?
  #         - shared amount avg. X
#From Bias
  # implic. - add average IAT score ___
  #         - add average IAT RT ___
  #         - add average AMP score X
  #         - add average AMP RT X
  # explic. - SRS Sum X
  #         - SRS Mean X
  #         - MRS Sum X
  #         - MRS Mean X
  #         - IMS-EMS EMS Sum X
  #         - IMS-EMS IMS Sum X
  #         - IMS-EMS Sum Difference X
  #         - IMS-EMS EMS Avg X
  #         - IMS-EMS IMS Avg X
  #         - IMS-EMS Avg Difference X
  #         - Contact Measures X (Can add other dimensions as needed)

# Working Notes for creating the Wide data frame (erase when done)


# SURVEYS ########

#Survey Data Day 1
#Participants' experience with the study
ptsD1_csv <- file.path(config$path$data$current, config$csvs$ptsD1)
post_task_survey_day1 <- read.csv(ptsD1_csv)


#Survey Data Day 2
#Participants' experience with the study
ptsD2_csv <- file.path(config$path$data$current, config$csvs$ptsD2)
post_task_survey_day2 <- read.csv(ptsD2_csv)

