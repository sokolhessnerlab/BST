
# --- Trust Only Base Level Analysis Script --- #

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

#### TRUST RATINGS ####

#BST trust rating for the trust perception task.

#Trust Rating descriptives (collapsed)
mean(aggregate(rating ~ subjectID, data = trustRating, FUN=mean)$rating)
mean(aggregate(rating ~ subjectID, data = trustRating, FUN=sd)$rating)

#TO DO: EB create new df with stressed bool in set up then approach
# Trust Rating descriptives (subject level)
tmpvect = aggregate(rating ~ subjectID + stressedBool, data = trustRating, FUN=mean);
# Trust Rating means within condition
by(data = tmpvect$rating, INDICES = tmpvect$stressedBool, FUN = mean) 
# control = 4.74, stressed = 4.67

# Trust Rating Day Effects
#by(data = trustRating$rating, INDICES = trustRating$day, FUN = mean)
#by(data = trustRating$rating, INDICES = trustRating$day, FUN = sd)

# Trust Rating Day Effects
tmpvect = aggregate(rating ~ subjectID + day, data = trustRating, FUN=mean);
by(data = tmpvect$rating, INDICES = tmpvect$day, FUN = mean) # day 1 = 4.73, day 2 = 4.68

# Trust Rating Rask Order Effects
by(data = trustGame$rating, INDICES = trustGame$taskOrder, FUN = mean)
by(data = trustGame$rating, INDICES = trustGame$taskOrder, FUN = sd)

tmpvect = aggregate(rating ~ subjectID + taskOrder, data = trustGame, FUN=mean);
by(data = tmpvect$rating, INDICES = tmpvect$taskOrder, FUN = mean) # order 1 = 4.69, order 2 = 4.84

# Q: Were participants stressed before doing the trust task?

# Q: Did people choose to share or not more or less under acute stress?
# Q: Did people choose to share or not more or less  under chronic stress?

# Q: Did people choose to share or not more or less for black, white, other races?
# Q: #Did people choose to share or not more or less for black, white, other races under stress?

# Q: If the previous share was not unreciprocated, how did that affect subsequent sharing?
# Q:  what if the unreciprocated share was not of white race vs. POC race?
