#-loads up a series of data frames of the stress constructs and trust constructs for BST for later analysis
#-aggregates data into additional data frames
#-performs coding and computation of variables

#loads up the necessary libraries
library(plyr)

#Color Blind Palette for graphing
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##Start Data Retreival, Cleaning, Formatting: ##

#setting Up Stress DFs:#

#PSS:
pss_csv <- file.path(config$path$data$survey, config$csvs$pss)
bst_pss <- read.csv(pss_csv) #reads in the .csv
#bst_pss <- bst_pss[-c(1),] #removes the first row which just has variable descriptions
names(bst_pss)[names(bst_pss) == "subjectNumber"] <- 'subjectID' #renames the subject identification column to maintain consistency

#recode reverse coded values for the 4 questions that need it
bst_pss$personalProblemConfidenceRecode <- 4 - bst_pss$personalProblemConfidence
bst_pss$goingYourWayRecode <- 4 - (bst_pss$goingYourWay)
bst_pss$irritationControlRecode <- 4 - bst_pss$irritationControl
bst_pss$onTopOfThingsRecode <- 4 - bst_pss$onTopOfThings
#calculate and check the sum of all the parts of the pss (the pss score)
bst_pss$pssSum <- (bst_pss$unexpectedUpset + bst_pss$unableControl + bst_pss$nervous + bst_pss$personalProblemConfidenceRecode + bst_pss$goingYourWayRecode + bst_pss$couldNotCope + bst_pss$irritationControlRecode + bst_pss$onTopOfThingsRecode + bst_pss$outsideOfControl + bst_pss$diffPilingUp)

bst_pss$pssSumCategorical <- ifelse(bst_pss$pssSum <= 13, 0,
                                    ifelse(bst_pss$pssSum >= 27, 2, 1)) #calculates the pss score in the 3 level categorical version

#checking the categorical math worked out:
# count(bst_pss$pssSum)
# count(bst_pss$pssSumcategorical)

#Bath Ratings:
bath_pleasantness_csv <- file.path(config$path$data$current, config$csvs$bath_pleasantness)
bst_bathPleasantness <- read.csv(bath_pleasantness_csv) #reads in the pleasantness ratings

bath_order_csv <- file.path(config$path$data$current, config$csvs$bath_order)
bst_bathOrder <- read.csv(bath_order_csv) #reads in the bath ordering
names(bst_bathOrder)[names(bst_bathOrder) == "BST.."] <- 'subjectID'

bst_bath <- merge(bst_bathPleasantness, bst_bathOrder, by = "subjectID") #merges the two bath DF's into a single one

bst_bath$diffPleasantnessRating<- (bst_bath$STRESS - bst_bath$CONTROL) #calculates the rating difference between the stress and control ratings

bst_bath$day2StressedBool <- ifelse(bst_bath$Day.2 == "CONTROL", 0, 1) #calculates a boolean for whether a participant was stressed on the second day or if they recieved the control

#double checks the math worked out:
# count(bst_bath$Day.2) #prints out how many recieved the control on day 2 and how many recieved the stressor on day (in string format)
# count(bst_bath$day2Stressed) #prints out how many recieved the control on day 2 and how many recieved the stressor on day 2 (in numerical format)

#renaming columns for clarity
names(bst_bath)[names(bst_bath) == "CONTROL"] <- "controlPleasantnessRating"
names(bst_bath)[names(bst_bath) == "STRESS"] <- "stressPleasantnessRating"
names(bst_bath)[names(bst_bath) == "Day.1"] <- "bathRecievedDay1"
names(bst_bath)[names(bst_bath) == "Day.2"] <- "bathRecievedDay2"

#combining acute and chronic stressors
bst_bath_pss <- merge(bst_bath, bst_pss, by = "subjectID")

#Setting Up Trust DFs:#
#Trust Game:
tg_csv <- file.path(config$path$data$current, config$csvs$tg)
bst_tg <- read.csv(tg_csv) #reads in trust game data

#renaming columns for clarity
names(bst_tg)[names(bst_tg) == "condition"] <- "taskOrder"
names(bst_tg)[names(bst_tg) == "RT"] <- "responseTime"

#combines the trust game data with the bath data for calculation of variables and later analysis
bst_tg <- merge(bst_tg, bst_bath, by = "subjectID")

#calculating new factors for whether a participant was stressed or not before doing the trust task
bst_tg$stressedBool <- ifelse((bst_tg$day2StressedBool == 1 & bst_tg$day == 1), 0,
                              ifelse((bst_tg$day2StressedBool == 0 & bst_tg$day == 1), 1,
                                     ifelse((bst_tg$day2StressedBool == 1 & bst_tg$day == 2), 1,
                                            ifelse((bst_tg$day2StressedBool == 0 & bst_tg$day == 2), 0, NA))))
#double checking the math above worked out (there should be no NA)
# count(bst_tg$stressedBool)

#calculating results of previous trials
bst_tg$prevTrialShared <- 0 #sets up as 'neutral condition'
bst_tg$prevTrialFeedback <- 0 #sets up as 'neutral condition'
#calculates what the previous trial was:
bst_tg <- bst_tg[order(bst_tg$subjectID, bst_tg$day, bst_tg$cumTrialNum),] #orders dataset by cumulative trial (within a day and subjectID)
for(i in 1:nrow(bst_tg)) #iterates through the now ordered dataset
{
  if(bst_tg[i, "cumTrialNum"] != 1) #if its the first trial, then there is nothing to change, as there was no 'previous trial'
  {
    if(bst_tg[(i-1), "shared"] >= 1) #if they shared any money on the previous trial
    {
      bst_tg[i, "prevTrialShared"] <- 1 #then the current trials prev trial shared amount can be set to 1 (ie they did share on the last trial)
      if(bst_tg[(i-1), "received"] == 0) #of they didn't recieve any money back, the feedback is set to -1 (ie on the last trial they lost the money they shared)
      {
        bst_tg[i, "prevTrialFeedback"] <- -1
      }
      else #if they did recieve money back after sharing, their feedback is set to 1
      {
        bst_tg[i, "prevTrialFeedback"] <- 1
      }
    }
    else #if they didn't share any money, the shared amount is set so, and the feedback isn't changed
    {
      bst_tg[i, "prevTrialShared"] <- -1
    }
  }
}
#make sure the counts are where they should be (the coding worked right)
# count(bst_tg$shared)
# count(bst_tg$prevTrialShared)
# count(bst_tg$partnerChoice)
# count(bst_tg$prevTrialFeedback)

#setting up a categorical for if a participant shared any money at all
bst_tg$sharedBool <- ifelse(bst_tg$shared == 0, 0, 1)

#because merging with the PSS will remove 3 participants, this is a sperate DF for only those participants that have a PSS score
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
count(bst_tg$stressedBool)

#because merging with the PSS will remove 3 participants, this is a sperate DF for only those participants that have a PSS score
bst_tr_pss <- merge(bst_pss, bst_tr, by = "subjectID") #for use when looking at pss x trust rating


#setting up aggregated DFs:
#overall:
#for the trust game:
bst_tg_part_avg <- aggregate(shared ~ subjectID, data = bst_tg, FUN = mean) #aggregates the trust game by participant
names(bst_tg_part_avg)[names(bst_tg_part_avg) == "shared"] <- "sharedAvg"
bst_tg_part_avg$sharedSD <- (aggregate(shared ~ subjectID, data =bst_tg, FUN = sd))$shared #calculates each participants standard deviation shared amount
bst_tg_part_avg$sharedSE <- (bst_tg_part_avg$sharedSD / sqrt(nrow(bst_tg_part_avg))) #calculates the standard error
bst_tg_part_avg$sharedHighSE <- bst_tg_part_avg$sharedAvg +  bst_tg_part_avg$sharedSE #calculates the standard error high bound
bst_tg_part_avg$sharedLowSE <- bst_tg_part_avg$sharedAvg - bst_tg_part_avg$sharedSE #calculates the standard error low bound

#for the trust rating
bst_tr_part_avg <- aggregate(rating ~ subjectID, data = bst_tr, FUN = mean) #aggregates the trust rating task for each participant
names(bst_tr_part_avg)[names(bst_tr_part_avg) == "rating"] <- "ratingAvg"
bst_tr_part_avg$ratingSD <- aggregate(rating ~ subjectID, data = bst_tr, FUN = mean)$rating #calculates each particpant's standard deviation shared amount
bst_tr_part_avg$ratingSE <- (bst_tr_part_avg$ratingSD / sqrt(nrow(bst_tr_part_avg)))
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
rm(i)

#you could also remove these if you really feel the need, the data is contained in other DFs, but I keep them for clarity in later analysis
#rm(bst_pss)
#rm(bst_bath)
#rm(bst_bath_pss)

