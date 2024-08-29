# BST Data Frame and Variable Descriptions

**Last updated: 2024.07.10**

# Note: When data frames were merged for analysis, columns are carried over. For a description of a column, go to the "original" DF

### Names of dataframes/variables are marked with **xyz**



> STRESS <

- **bath** -
  - The df merges the two bath DF's into one AND removes bath orders of participants who do not have an bath rating score
  - The df contains the combined bath pleasantness rating data, as well as the bath order data for each individual participant
    $(control/stress)PleasantnessRating: the rating on a 1-7 scale of how pleasant to unpleasant the bath was
    $bathRecievedDay(1/2): what bath type a participant recieved on a given day (word format)
    $Run.: incomplete data on whether a participant was run? *Don't Use*
    $bdiffPleasantnessRating: the pleasantness rating after the cold bath minus the pleasantess rating after the lukewarm bath
    $day2StressedBool: whether a participant was given the cold bath or the lukewarm bath on day 2 (1=true, they were stressed, 
        ie. they had the cold bath on day 2)

- **pss** -
  -contains each participant's PSS data (who had it)
    $unexpectedUpset/unableControl/nervous/$personalProblemConfidence/goingYourWay/couldNotCope/irritationControl/onTopOfThings/outsideOfControl/diffPilingUp: each of these is associated with a specific question in the PSS
    $(*)Recode: any of the aforementioned questions with a Recode on the end is simply a reverse scored version of the question, used in calculating the PSS score
    $pssSum: PSS score (0-40)
    $pssSumCategorical: using the "low" "moderate" "high" scoreing, a 3 way categorical version of the pssSum
    $pssMedianSplit: a binary split on whether a participant's pssSum was above (1) or below (-1) the median across the dataset

# Note: Three participants did not take the PSS (thus lack PSS scores), any data frame with _pss will have 36 participants, while those without _pss will have 39. This issue has been addressed in the wide, subject-level dataframe with NA values assigned to missing subject data. 

- **cort**
  - contains much of the cortisol data. 
  - Containts batch, sample_number, subjectID, sample_ID, date_of_collection, cort_1_value, cort_2_value, cortisol_mean_nmol_to_l, cort_coeff_of_variance_as_percent, day, sample

- **cort_subj_level**
  -a truncated cortisol DF for use in subj-level Subj-level Stress DF
  -Contains subjectID, cortisol_mean_nmol_to_l, cort_coeff_of_variance_as_percent only for all participants

- **cort_mtx**
  -Reshapes cort data into a 4 (sample) x 2 (day) x 2 (condition [1 = ctrl, 2 = strs]) x N (subjects) three dimensional object
  
- **Stress_Subj_Level_wCort**
  -Stress subject-level dataframe with participants with ALL participants
  -Combines key acute stressor data (bath unpleasnantness rating, cortisol means/COV) and chronic stressor data (PSS sum, PSS median split, etc.)

- **Stress_Subj_Level**
  -Stress subject-level dataframe with participants who completed BOTH days of the experiment
  -Combines key acute stressor data (bath unpleasnantness rating, cortisol means/COV) and chronic stressor data (PSS sum, PSS median split, etc.)



> TRUST <

- **trustGame** -
  -this contains the Trust Game data
    $taskOrder: whether the trust game was first (ahead of the trust rating task) or second (behind the trust rating task) on a given day
    $blockNum: what block number a given trial was in
    $trialNum: what trial number a trial was (in a given block)
    $cumTrialNum: what trial number a trial was (overall)
    $stimulus: what picture a person was shown as their partner
    $shared: how much money in dollars a person shared with their partner
    $partnerChoice: whether a partner shared money back (1 = true) or not (0 = false), NaN if there was no choice to make
    $partnerRace: the stimulus picture (participant's partner) race, 0=white, 1=black, 2=other
    $received: how much money in dollars a person recieved on a given trial
    $responseTime: response time after seeing the stimulus
    $startTime: computer registered start time (should probably only be used in a relativistic fashion)
    $bonusTrial: which trial was randomly selected as the bonus for particpant
    $bonus: what the actual bonus was based on that randomly selected trial
    $stressedBool: whether a person had recieved the cold bath on the day this trust game was administered
    $prevTrialShared: whether a person shared anything on the previous trial (-1=they didn't, 0=no previous trial, 1=they did)
    $prevTrialSharedAmt: how much a person shared on the previous trial, 0 also including if there was no previous trial
    $prevTrialFeedback: whether a person saw their trust reciprocated on the previous trial (-1=they didn't, 0=no previous trial/they didn't share anything in the first place, 1=they did)
    $sharedBool: whether a person actually shared money or not on a given trial

- **trustRating** -
  -this contains the Trust Rating data
    $taskOrder/day/blockNum/trialNum/cumtrialNum/stimuls/partnerRace/responseTime/startTime/stressedBool: all the same as previously described, only for the trust rating instead
    $rating: what the trust rating was by a participant for a given trial was from untrustworthy (1) to trustworthy (9)
    $prevTrialRating: a lagged variable representing in boolean format whether a participant gave an untrustworthy (ie a rating from 1-4) (-1), trustworthy (ie a rating from 5-9) (1) rating for the previous trial, or whether there was no previous trial (0)
    $prevTrialRatingAmt: the rating given to a person by the participant on a previous trial (1-9), 0 being if there was not previous trial



> ATTITUDES <

- **amp** -
  -contains all of the amp data

- **amp_Subj_Level** -
  - contains amp RTs and unpleasant vs. pleasant ratings
  - includes subjectID, unPleasant0_Pleasant1, responseTime_AMP

- **iat** -
  -contains all of the iat data
  
- **srs** -
  -contains all of the Symbolic Racism Scale survey data

- **amp_Subj_Level** -
  - contains amp RTs and unpleasant vs. pleasant ratings
  - includes subjectID, unPleasant0_Pleasant1, responseTime_AMP
  
- **mrs** -
  -contains all of the Modern Racism Scale survey data
  -mrs measures how much you agree/disagree with statements on race

- **mrs_Subj_Level** -
  - compiles from mrs df the subjectID, mrsSum, mrsMean

- **srs** -
  -contains all of the Symbolic Racism Scale survey data
  -srs measures "your thoughts" regarding race

- **srs_Subj_Level** -
  -subject-level df that compiles from srs df the subjectID, srsSum, srsMean

- **IMS-EMS** -
  -contains all of the Internal and External Motivation to Respond Without Prejudice
  -IMS-EMS measures feelings towards statements on what motivates to respond without prejudice

- **ims_ems_Subj_Level** -
  -subject-level df that compiles from IMS-EMS df the IMS & EMS sums and averages, as well as differences between EMS/IMS sums and averages.

- **cm** -
  -contains much of the Contact Measures survey data
  -CM survey measures contact with same/other race

- **cm_Subj_Level** -
  -subject-level cm df that contains subject ID and self-report race-ethnicity item



> WIDE (subject-level) BST DFs <

- **BST_Subj_Level_DF** -
  -comprehensive, subject-level data frame of most pertinent BST data.
  -contains subjectID, diffUnPleasantnessRating, day2bool_0control_1stress, pssSum, pssCat_0low_1mod_2high, pssMedianSplit, cortisol_mean_nmol_to_l, 
      cort_coeff_of_variance_as_percent, mrsSum, mrsMean, srsSum, srsMean, EmsSum, ImsSum, EmsImsSumDiff, EmsMean, ImsMean, EmsImsMeanDiff,
      unPleasant0_Pleasant1, responseTime_AMP, w0_his1_as2_bl3_birac4_mult5


  


> Legacy <

NOTE: Return to this section and remove/edit df/var names

- **bst_t_compiled_part_avg** -
  -this contains the average data for each participant for each trust task (Game and Rating)
    $sharedAvg: the average amount shared by a given participant in the trust game
    $shareSD: a participants standard deviation in the shared amount in the trust game
    $sharedSE: a standard error for a participant in the shared amount in the trust game
    $shared(High/Low)SE: the average shared amount (+/-) the standard error for a given participant
    $ratingAvg: the average rating (ie trust rating) by a given participant
    $ratingSD: a participant's standard deviation in the rating in the trust rating task
    $ratingSE: a standard error for a participant in the rating in the trust rating task
    $rating(High/Low)SE: the average ratin (+/-) the standard error for a given participant

- **bst_t_compiled_part_avg_stress** -
  -this DF is exactly the same as bst_t_compiled_part_avg except when everything was agreggated, it was also split out by whether a partcipant was stressed or not. That is, there are now 2 rows for every participant, 1 for their responses on the control day, and 1 for their responses on the stress day


