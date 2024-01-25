#loads up the necessary libraries
library(plyr)
library(lme4)
library(nlme)
library(lmerTest)

options(scipen=999)  #sci notation to decimal


#Color Blind Palette for graphing
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#PSS:
pss_csv <- file.path(config$path$data$survey, config$csvs$pss)
bst_pss <- read.csv(pss_csv) #read in .csv
#bst_pss <- bst_pss[-c(1),] #removes the first row which just has variable descriptions
names(bst_pss)[names(bst_pss) == "subjectNumber"] <- 'subjectID' #renames the subject identification column to maintain consistency

head(bst_pss)

#recode reverse coded values for the 4 questions that need it
bst_pss$personalProblemConfidenceRecode <- 4 - bst_pss$personalProblemConfidence
bst_pss$goingYourWayRecode <- 4 - (bst_pss$goingYourWay)
bst_pss$irritationControlRecode <- 4 - bst_pss$irritationControl
bst_pss$onTopOfThingsRecode <- 4 - bst_pss$onTopOfThings

#calculate and check the sum of all the parts of the pss (the pss score)
bst_pss$pssSum <- (bst_pss$unexpectedUpset + bst_pss$unableControl + bst_pss$nervous + bst_pss$personalProblemConfidenceRecode + bst_pss$goingYourWayRecode + bst_pss$couldNotCope + bst_pss$irritationControlRecode + bst_pss$onTopOfThingsRecode + bst_pss$outsideOfControl + bst_pss$diffPilingUp)

head(bst_pss) #note new sum column

bst_pss$pssSumCategorical <- ifelse(bst_pss$pssSum <= 13, 0,
                                    ifelse(bst_pss$pssSum >= 27, 2, 1)) #calculates the pss score in the 3 level categorical version

head(bst_pss, 30) #note new sum category column (0, 1, 2 - few 2's)

#checking the categorical math worked out:
#count(bst_pss$pssSum)
count(bst_pss$pssSumCategorical) #NOTE!  Only one participant had "very stressed", also more than double in "mildly stressed" vs. "no stress"


#median split for pss
bst_pss$pssMedianSplit <- ifelse(bst_pss$pssSum < median(bst_pss$pssSum), -1, 1)
#double checking the median split worked out.
count(bst_pss$pssMedianSplit)

head(bst_pss, 30) #note new median split column

#Bath Ratings:
bath_pleasantness_csv <- file.path(config$path$data$current, config$csvs$bath_pleasantness)
bst_bathPleasantness <- read.csv(bath_pleasantness_csv) #reads in the pleasantness ratings

head(bst_bathPleasantness, 30)  #note, subjID with pleasantness rating for control & stress condition

bath_order_csv <- file.path(config$path$data$current, config$csvs$bath_order)
bst_bathOrder <- read.csv(bath_order_csv) #reads in the bath ordering
names(bst_bathOrder)[names(bst_bathOrder) == "BST.."] <- 'subjectID'

head(bst_bathOrder, 30)  #note, subjID with Day 1 & 2 control vs stress condition identifiers. Run or NO.

bst_bath <- merge(bst_bathPleasantness, bst_bathOrder, by = "subjectID") #merges the two bath DF's into a single one

head(bst_bath, 30)  #note, subjID with Day 1 & 2 control vs stress condition identifiers. Run or NO. AND pleasantness

bst_bath$diffPleasantnessRating <- (bst_bath$STRESS - bst_bath$CONTROL) #calculates the rating difference between the stress and control ratings

head(bst_bath, 30) #note new column with diffPleasantnessRating, NOTE! Why P20 had neg val (more stress under control condition)
#MORE Pos the value, Greater diff in stress vs. control condition with stress being increasingly higher than control

bst_bath$day2StressedBool <- ifelse(bst_bath$Day.2 == "CONTROL", 0, 1) #calculates a boolean for whether a participant was stressed on the second day (1) or if they received the control on day 2 (0)

head(bst_bath, 20) #note new column with day2 stress T or F

#double checks the math worked out:
#prints out how many received the control on day 2 and how many received the stressor on day (in string format)
count(bst_bath$Day.2) #NOTE!  23 Control on 2nd Day vs 16 Stress on 2nd day.  More Stress on 1st day, drop out?
#count(bst_bath$day2Stressed) #prints out how many received the control on day 2 and how many received the stressor on day 2 (in numerical format)

bst_bath$day1StressedBool <- ifelse(bst_bath$Day.1 == "CONTROL", 0, 1) #calculates a boolean for whether a participant was stressed on the second day (1) or if they received the control on day 2 (0)

head(bst_bath, 20) #note new column with day1 stress T or F

count(bst_bath$Day.1) #NOTE!  16 Control on 1st Day vs 23 Stress on 1st day.

#renaming columns for clarity
names(bst_bath)[names(bst_bath) == "CONTROL"] <- "controlPleasantnessRating"
names(bst_bath)[names(bst_bath) == "STRESS"] <- "stressPleasantnessRating"
names(bst_bath)[names(bst_bath) == "Day.1"] <- "bathReceivedDay1"
names(bst_bath)[names(bst_bath) == "Day.2"] <- "bathReceivedDay2"

head(bst_bath, 20) #note new column names, descriptive

#combining acute and chronic stressors
bst_bath_pss <- merge(bst_bath, bst_pss, by = "subjectID")

bst_bath_pss_2 <- bst_bath_pss[-c(10:23)]
head(bst_bath_pss_2) #NOTE: took out PSS breakdown in this df for ease of use

summary(bst_bath_pss_2)
#control pleasantness ratings mean = 2.083, range 1-4
#stress pleasantness ratings mean = 6.083, rang 1-7
#pleasantness diff mean = 4
#pss sum = 15.92

#---------- Logistic Regress for Stress/Pleasantness -----------------#

xtabs(~pssMedianSplit + diffPleasantnessRating, data = bst_bath_pss_2)
xtabs(~pssMedianSplit + pssSumCategorical, data = bst_bath_pss_2)
# -1 split = 11 from 0, 7 from 1 (18 total)
#  1 split = 17 from 1, 1 from 2 (18 total)

str(bst_bath_pss_2)

levels(bst_bath_pss_2$pssMedianSplit) <- list(LessStress = "-1", "MoreStressed" = "1")
head(bst_bath_pss_2)

#run logistic regression with median split groups and pleasantness diff
bst_bath_pss_2$pssMedianSplit <- factor(bst_bath_pss_2$pssMedianSplit)
PSS_split_pleas <- glm(pssMedianSplit ~ diffPleasantnessRating, data = bst_bath_pss_2, family = "binomial")
summary(PSS_split_pleas) #NOTE! does not seem to be a sig diff in pleasantness rating dichotomy for less vs more stressed groups

#run logistic regression with median split groups and PSS Sum
PSS_split_stressSumcat <- glm(pssMedianSplit ~ pssSumCategorical, data = bst_bath_pss_2, family = "binomial")
summary(PSS_split_stressSumcat) #NOTE! does not seem to be a sig diff in pleasantness rating dichotomy for less vs more stressed groups

tapply(bst_bath_pss_2$pssSumCategorical, bst_bath_pss_2$pssMedianSplit, mean)
tapply(bst_bath_pss_2$pssSum, bst_bath_pss_2$pssMedianSplit, mean)

aggregate(diffPleasantnessRating ~ pssMedianSplit, bst_bath_pss_2, mean )

t.test(bst_bath_pss_2$stressPleasantnessRating, bst_bath_pss_2$controlPleasantnessRating, paired = T, var.equal = T)

#Key Results (p = 3.0e-16, mean difference = 4.1) - the ratings are statistically very probably different
# PSH NOTE: This is a BIG difference. A diff of 4.1 when your scale goes 1-7 is huge.
# EB NOTE: t(38) = 13.66, p < .0001; Mean diff = 4.10
# EB NOTE: Those in the stress condition reported significantly lower pleasantness compared to the stress condition (M = 4.10).

t.test(bst_bath_pss_2$diffPleasantnessRating, bst_bath_pss_2$pssSum, paired = T, var.equal = T)
# EB NOTE: t(35) = -11.29, p < .0001;
# EB NOTE: Those who reported significantly higher pleasantness differences (feelings of unpleasant v pleasant) in stress vs. control
# had lower PSS Sums (M = -11.9167).


t.test(bst_bath_pss_2$diffPleasantnessRating, bst_bath_pss_2$pssMedianSplit, paired = T, var.equal = T)
# EB NOTE: t(35) = -11.29, p < .0001;
# EB NOTE: Those who reported significantly higher pleasantness differences (feelings of unpleasant v pleasant) in stress vs. control
# had lower PSS Sums (M = -11.9167).

head(bst_bath_pss_2)

stress_pleas_mod <- lmer(diffPleasantnessRating ~ 1 + pssMedianSplit * pssSum +
                          ( 1 | subjectID), data = bst_bath_pss_2)
summary(stress_pleas_mod)
