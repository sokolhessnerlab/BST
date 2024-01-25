#loads up the necessary libraries
library(plyr)

options(scipen=999)  #sci notation to decimal


#Color Blind Palette for graphing
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#Setting Up Trust DFs:#
#Trust Game:
tg_csv <- file.path(config$path$data$current, config$csvs$tg)
bst_tg <- read.csv(tg_csv) #reads in trust game data

head(bst_tg)

#renaming columns for clarity
names(bst_tg)[names(bst_tg) == "condition"] <- "taskOrder"
names(bst_tg)[names(bst_tg) == "RT"] <- "responseTime"

#combines the trust game data with the bath data for calculation of variables and later analysis
# bst_tg <- merge(bst_tg, bst_bath, by = "subjectID")
# PSH NOTE: Careful with things like this. This puts data on two very different scales (e.g.
# the PSS with one sum value per person; TG data with many individual trials) together in
# a way that could create issues if someone forgets that... Put another way, this merge
# creates a dataframe that makes it look like the PSS was measured 152 times per person
# and it looked the same every time.
#
# That issue makes this of limited utility to do; if you do want to relate acute or
# chronic stressors to TG performance, you'll need to summarize TG data to the same space
# (e.g. person-level summary stats) anyway. [PSH]

# ??? SHOULD WE COLLAPSE THE PARTICIPANT RT DATA IN PARTICULAR WAY ???

#calculating new factors for whether a participant was stressed or not before doing the trust task
#bst_tg$stressedBool <- ifelse((bst_tg$day2StressedBool == 1 & bst_tg$day == 1), 0,
                              #ifelse((bst_tg$day2StressedBool == 0 & bst_tg$day == 1), 1,
                                     #ifelse((bst_tg$day2StressedBool == 1 & bst_tg$day == 2), 1,
                                            #ifelse((bst_tg$day2StressedBool == 0 & bst_tg$day == 2), 0, NA))))
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
