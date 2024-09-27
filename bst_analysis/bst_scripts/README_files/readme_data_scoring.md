#  Github/bst/data/bst_analysis/bst_scripts README 

**Last updated: 2024.08.07**


### Folder Contents

The primary function of this Read Me file is to outline the methods for compiling the BST data and provide additional helpful information and references on the data compiling methods.

Overall, this "Current_Dataframes" folder contains the most up-to-date compiled dataframes, which are directly used in the BST setup_data.R script and subsequent scripts for analyses. These data are created from the "Create_Dataframes" and "scannedpaperdocs" folders. Note: this section will be updated as new data is compiled and readied for analysis.

** NOTE:** These folders specifically compile COMPLETE participant data for participants who completed both sessions 1 and 2 of the study. For all participant data, regardless of state/completion, see the "output" in the Shlab server sub-folder labeled "task" (shlab/Projects/BST/task).

- **.cvs file descriptions** - Data from the BST experiment has been converted into csv files that can be found in the Shlab server under the BST project folder (shlab/Projects/BST/data/Current_Dataframes):
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
    - In CortisolData subfolder:
        -Cortisol_Data.csv - contains all cortisol data without assay processing notes (used in setup and stress analysis scripts)
        -Cortisol_w_Notes.csv - contains all cortisol data with assay processing notes
    - In SurveyData subfolder:
        -PSSData_noVarDescrip.csv - contains all PSS data without notes
        -PSSData.csv - contains all PSS data with notes
    

### Data Descriptions, Scoring Procedures, References

- **AMP Data** - allAMPData_20200626.csv - Contains all the affect misattribution procedure (AMP) data.
    - Description: The AMP examines a subject's sensitivity to favorable and unfavorable evaluations. AMP is a measure of responses that are activated automatically due to misattributions a person makes about the source of the cognition or affect.  In the AMP task, participants see black and white faces, followed by images of an inkplot then noise. Participants are then asked to rate the noise as pleasant or unpleasant.
   
    - Scoring Procedures: Scoring the AMP in BST:
Subjects respond as “pleasant” and “unpleasant” to images of static preceded by the faces of individuals of black or white race.  These responses are categorized as “0” for “unpleasant” and “1” for “pleasant”.  We then created a subject-level response average for black and white prime ratings across trials.

      Standard AMP “score” is calculated by subtracting the proportion of incongruent trials from congruent trials.  For the BST, the standard AMP score would be: BST AMP Score = (Positive judgments after white faces/Total white prime trials) - (Positive judgments after black faces/Total black prime trials).

    
    - References: 
    (1) Payne, B. K., Cheng, C. M., Govorun, O., & Stewart, B. D. (2005). An inkblot for attitudes: affect misattribution as implicit measurement. Journal of personality and social psychology, 89(3), 277–293.
    (2) Kurdi, B., Melnikoff, D. E., Hannay, J. W., Korkmaz, A., Lee, K. M., Ritchie, E., Surdel, N., Vuletich, H. A., Yang, X., Payne, B. K., & Ferguson, M. J. (2024). Testing the automaticity features of the affect misattribution procedure: The roles of awareness and intentionality. Behavior research methods, 56(4), 3161–3194. https://doi.org/10.3758/s13428-023-02291-2
    (3) Payne, K., & Lundberg, K.B. (2014). The Affect Misattribution Procedure: Ten Years of Evidence on Reliability, Validity, and Mechanisms. Social and Personality Psychology Compass, 8, 672-686.
    - Note: AMP is correlated with explicit race attitudes, but this effect is moderated by motives to control prejudice.
    
    
- **IAT Data** - allIATData_20200626.csv  - Contains all the Implicit Association Test (IAT) data.
    - Description: The IAT evaluates a 2-choice task in which two highly associated and unassociated categories share a response key. For example, a group of black faces is paired with "good" words and "bad" words and response times differences are examined.
    -NOTE: Greenwald had a traditional IAT D-Scoring algorithm, then Greenwald updated his own scoring algorithm in 2003. Both methods are cited below.
    
    - "An implicit association test (IAT) measures differential association of 2 target concepts with an attribute" (Greenwald, 1998)
    
    - Scoring Procedures: Participants attribute stimuli "good" and "bad" word stimuli with images of Black and White individuals. Associative tasks are conducted with feedback following incorrect responses.
    
    "D is computed as the difference between mean latencies of the two BIAT blocks divided by the inclusive (not pooled) standard deviation of latencies in the two blocks. This measure has been shown to have psychometric properties superior to those of a wide variety of alternative strategies for using latencies from the IAT’s two tasks
(Greenwald et al., 2003; Sriram, Nosek, & Greenwald, 2007)." (Sriram & Greeenwald, 2009)

    
    - References:
    (1) Greenwald, A.G., McGhee,D.E., & Schwartz, J.L.K. (1998). Measuring Individual Differences in Implicit Cognition: ThenImplicit Association Test. Journal of Personality and Social Psychology, 74, 1464-1480. *(Used in Stanley, 2011)
    (2) Sriram, N., & Greenwald, A. G. (2009). The brief implicit association test. Experimental Psychology, 56(4), 283–294. https://doi.org/10.1027/1618-3169.56.4.283
    (3) Nosek, B. A., Bar-Anan, Y., Sriram, N., Axt, J., & Greenwald, A. G. (2014). Understanding and using the brief Implicit Association Test: recommended scoring procedures. PloS one, 9(12), e110938. https://doi.org/10.1371/journal.pone.0110938
    (4) Röhner, J., & Thoss, P. J. (2019). A tutorial on how to compute traditional IAT effects with R. The Quantitative Methods for Psychology, 15(2), 134–147.
    (5) Greenwald, A. G., Nosek, B. A., & Banaji, M. R. (2003). Understanding and using the implicit association test: I. An improved scoring algorithm. Journal of personality and social psychology, 85(2), 197–216. https://doi.org/10.1037/0022-3514.85.2.197
    
    
    
    
- **Trust Game Data** - allTGData_20200626.csv  - contains all the trust game (TG) data.
    - Description
    - Scoring Procedures
    - References.
    
    
- **Trust Rating Data** - allTRData_20200626.csv  - contains all the trust rating (TR) data.
    - Description
    - Scoring Procedures
    - References.
    
    
- **Contact Measures Data** - BST_ContactMeas_Data.csv  - contains all the contact measures (CM) survey data.
    - Description: Participants are asked to reflect on how many White and Black "contacts" they have.  Friends, acquaintances, dating partners are considered, as well as entertainers and neighborhood environment. The survey concludes with a self-identification of one's own race or ethnicity.
    
    - Scoring Procedures:
      (1) One possible approach: "We constructed intergroup contact difference scores by averaging the percentages for Black and White contact separately for each subject and then subtracting the percentage of Black contact from White contact." (Dunsmoor et al., 2016)
    
    - References:
      (1) Dunsmoor, J. E., Kubota, J. T., Li, J., Coelho, C. A., & Phelps, E. A. (2016). Racial stereotypes impair flexibility of emotional learning. Social cognitive and affective neuroscience, 11(9), 1363–1373. https://doi.org/10.1093/scan/nsw053
      (2) Pettigrew, T. F., & Tropp, L. R. (2006). A meta-analytic test of intergroup contact theory. Journal of personality and social psychology, 90(5), 751–783. https://doi.org/10.1037/0022-3514.90.5.751
      
    
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




