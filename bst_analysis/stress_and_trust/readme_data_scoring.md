#  BST/data/Current_Dataframes README 

**Last updated: 2024.07.10**


### Folder Contents

The primary function of this Read Me file is to outline the methods for compiling the BST data, and provide additional helpful information and references on the data compiling methods.

Overall, this "Current_Dataframes" folder contains the most up-to-date compiled dataframes, which are directly used in the BST setup_data.R script and subsequent scripts for analyses. These data are created from the "Create_Dataframes" and "scannedpaperdocs" folders. Note: this section will be updated as new data is compiled and readied for analysis.

** NOTE: These folders specifically compile COMPLETE participant data for participants who completed both sessions 1 and 2 of the study. For all participant data, regardless of state/completion, see the "output" sub-folder in "tasks".**

- **.cvs file descriptions** - Data from the BST experiment has been converted into csv files:
    - allAMPData_20200626 contains all the AMP data (both sessions included).
    - allIATData_20200626.csv contains all the IAT data.
    - allTGData_20200626.csv contains the Trust Game data.
    - allTRData_20200626.csv contains the Trust Rating data.
    - bathOrder.csv references on which day of the experiment subjects were given control (lukewarm) or CPT (cold) bath.
    - bathPleasantness.csv contains subjects' ratings of the baths' unpleasantness.
    - BST_ContactMeas_Data.csv contains data on contact measures.
    - BST_IMS_EMS_Data.csv contains data on internal and external motives to avoid racism.
    - BST_ModernRacismScale_Data.csv contains data from Modern Racism Scale survey data.
    - BST_SymbolicRacismScale_Data.csv contains data from Symbolic Racism Scale survey data.
    - BST_Data_CPTRating_PostTaskSurvey_D1.csv contains data from Day 1 post-task survey.
    - BST_Data_CPTRating_PostTaskSurvey_D2.csv contains data from Day 2 post-task survey.
    

### Data Descriptions, Scoring Procedures, References

- **AMP Data** - allAMPData_20200626.csv - Contains all the affect misattribution procedure (AMP) data.
    - Description: The AMP examines a subject's sensitivity to favorable and unfavorable evaluations. AMP is a measure of responses that are activated automatically due to misattributions a person makes about the source of the cognition or affect.  In the AMP task, participants see black and white faces, followed by images of an inkplot then noise. Participants are then asked to rate the noise as pleasant or unpleasant.
   
    - Scoring Procedures: ADD
    
    - References: 
    (1) Payne, B. K., Cheng, C. M., Govorun, O., & Stewart, B. D. (2005). An inkblot for attitudes: affect misattribution as implicit measurement. Journal of personality and social psychology, 89(3), 277–293.
    (2) Payne, K., & Lundberg, K.B. (2014). The Affect Misattribution Procedure: Ten Years of Evidence on Reliability, Validity, and Mechanisms. Social and Personality Psychology Compass, 8, 672-686.
    - Note: AMP is correlated with explicit race attitudes, but this effect is moderated by motives to control prejudice.
    
    
- **IAT Data** - allIATData_20200626.csv  - Contains all the Implicit Association Test (IAT) data.
    - Description: The IAT evaluates a 2-choice task in which two highly associated and unassociated categories share a response key. For example, a group of black faces is paired with "good" words and "bad" words and response times differences are examined.
    
    - Scoring Procedures: ADD
    
    - References:
    (1) Greenwald, A.G., McGhee,D.E., & Schwartz, J.L.K. (1998). Measuring Individual Differences in Implicit Cognition: ThenImplicit Association Test. Journal of Personality and Social Psychology, 74, 1464-1480.
    
    
- **Trust Game Data** - allTGData_20200626.csv  - contains all the trust game (TG) data.
    - Description
    - Scoring Procedures
    - References.
    
    
- **Trust Rating Data** - allTRData_20200626.csv  - contains all the trust rating (TR) data.
    - Description
    - Scoring Procedures
    - References.
    
    
- **Contact Measures Data** - BST_ContactMeas_Data.csv  - contains all the contact measures (CM) survey data.
    - Description
    - Scoring Procedures
    - References.
    
    
- **IMS-EMS Data** - BST_IMS_EMS_Data.csv  - contains all the internal and external motivation (IMS-EMS) to respond without prejudice survey data.
    - Description: Participants are asked to honestly rate their racial motives in a survey of 10 scaled items. The IMS-EMS survey consists of two measures with 5 items each, half of which are "external motivation" and half "internal motivation" items for responding without prejudice.
    
    - Scoring Procedures: Participants rated 10 items on a scale ranging from 1 (strongly disagree) to 9 (strongly agree). When participants complete the scales, the IMS and EMS items were NOT intermixed. Question #7 - "According to my personal values, using stereotypes about Black people is OK" - in the survey is reverse-coded. Questions 1-5 are external motivation items, while questions 6-10 are internal motivation items.
    
    - References:
    (1) Plant, E. A., & Devine, P. G. (1998). Internal and external motivation to respond without prejudice. Journal of Personality and Social Psychology, 75(3), 811–832.
    (2) Kim Bamberg & Maykel Verkuyten (2022) Internal and external motivation to respond without prejudice: a person-centered approach, The Journal of Social Psychology, 162:4, 435-454
    
    
- **MRS Data** - BST_ModernRacismScale_Data.csv  - contains all the modern racism scale (MRS) survey data.
    - Description: The MRS is a seven Likert-type items survey originally intended to measure the cognitive component of White Americans’covert racial attitudes toward Black Americans.
    From the authors: "Modern Racism Scale (MRS; McConahay, Hardee & Batts, 1980) is a relatively nonreactive scale of racial attitudes. The MRS consists of 7 items worded as opinion statements rated on the following Likert-type scale: +2 (agree strongly), +1 (agree somewhat), 0 (neither agree nor disagree or no opinion), -1 (disagree somewhat), and -2 (disagree strongly)...In an experiment conducted by McConahey (1983), the MRS was administered to undergraduate introductory psychology students. Results provided strong support for the construct validity of the Modern Racism Scale as a measure of racial prejudice."
    
    - Scoring Procedures: The first question is reverse coded, so we recode in the script as +2 for strongly disagree and -2 for strongly agree. We then sum the scores.
    
    - References:
    (1) McConahay, J. B., Hardee, B. B., & Batts, V. (1980). Modern Racism Scale (MRS)
    (2) Morrison, T. & Kiss, M. (2017). Modern Racism Scale. 10.1007/978-3-319-28099-8_1251-1. 
    
    
- **SRS Data** - BST_SymbolicRacismScale_Data.csv  - contains all the modern racism scale (SRS) survey data.
    - Description: This scale was created in response to issues surrounding the Modern Racism Scale (MRS).  The SRS attempts to help prevent response biases and unreliable response patterns. 
    
    - Scoring Procedures: The following is the standard procedure for combining the items into a scale:
    (1) After collecting the data, items 1, 2, 4, and 8 need to be recoded so that a 1 = 4, 2 = 3, 3 = 2, and 4 = 1.  
    (2) Item 3 needs to be recoded so that 1 = 3, 2 = 1, and 3 = 2.  
    
    Scoring Options: For combining the items into a scale, there are several options, ranging from simplest to most precise.  
    (a) Add the raw scores together for each item, so that each individual has a score that could range from 8 to 31.
    (b) To compensate for any missing data, average the raw scores.  
    (c) To compensate for the differences in the number of response alternatives, one could recode each of the items on a 0 to 1 scale, so for item #3, a 1 = 1, 2 = 0, and 3 = .50, and for the other items the high response is a 1, the next a .66, the next a .33, and the low response is a 0. (This third technique is the one used in Henry & Sears, 2002.) 
    (d) To equate the variability across items, one could create standardized (z) scores for each of the items in the scale, then average the responses.
    
    - References:
    (1) Henry, P. J., & Sears, D. O.  (2002).  The symbolic racism 2000 scale.  Political Psychology, 23, 253-283.
    (2) Sears, D. O., & Henry, P. J.  (2005).  Over thirty years later: A contemporary look at symbolic racism and its critics.  Advances in Experimental Social Psychology. 37,  95-150.




