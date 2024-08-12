#' Function to Analyze Raw Data from an Implicit Association Test (IAT)
#' @description This function is designed to analyze raw data from an Implicit Association Test (IAT). The only required input is the name of the dataset, but you can also specify manually the number of trials in your IAT (e.g., for Brief IATs) or whether the participant first saw incongruent category pairings. Refer to the package's DESCRIPTION file for more detailed information (or contact me directly at danielstorage@icloud.com).
#' @param IAT The name of the dataset to be analyzed.
#' @param Trials The number of trials across your entire IAT. The default is set to 220, which is typical of most IATs.
#' @param First Whether participants first sorted Congruent or Incongruent trials. The default is set to Congruent.
#' @keywords IAT
#' @examples
#' IATanalytics(sampledata)
#' IATanalytics(IAT=sampledata)
#' IATanalytics(sampledata, First="Congruent")
#' IATanalytics(sampledata, First="Incongruent")
#' @importFrom stats na.omit sd
#' @export

IATanalytics <- function(IAT, Trials, First){

  start.time <- Sys.time()

  RT <- NULL

  colnames(IAT) <- c("Block", "Trial", "Category", "Cat_Item", "Correct", "RT")

  # Set default number of trials to 220, which is standard for the IAT, unless another value is specified by the user
  if(missing(Trials)){
    Trials = 221
  } else {
    Trials = Trials + 1
  }

  # Sets the default value for "First" to "Congruent", so the function can run without specifying this argument
  if(missing(First)){
    First = "Congruent"
  }

  # Step 1: Delete any reaction times > 10,000 ms ####
  i <- 1 # define i counting variable for while loop
  while (i < Trials) { # define while loop for Step 1
    if (IAT$RT[i] > 10000) {IAT$RT[i] <- 0}
    i = i + 1
  }
  IAT2 <- subset(IAT, RT!=0) # new data frame, excluding trials over 10,000 ms

  # Step 2: Check for exclusion based on response speed (10% trials < 300 ms) ####
  SpeedCount <- length(which(IAT2$RT<300)) # count number of RTs under 300
  SpeedCount # display the number of RTs under 300
  SpeedProp <- SpeedCount/nrow(IAT2) # calculate proportion of RTs under 300
  SpeedProp # display proportion of RTs under 300

  # Step 3: Subset trials into two separate datasets, which will be analyzed separately to produce two D values for each participant
  IAToddrows <- seq(1, nrow(IAT2), by=2)
  IATodd <- IAT2[IAToddrows,]
  IATodd <- na.omit(IATodd) #first subsetted data frame

  IATevenrows <- seq(0, nrow(IAT2), by=2)
  IATeven <- IAT2[IATevenrows,]
  IATeven <- na.omit(IATeven) #second subsetted data frame

  # Step 4a: Compute means of correct trials and replace incorrect trials this value + 600
  Block2newmean <- mean(IAT2$RT[IAT2$Block==2 & IAT2$Correct==0]) + 600
  Block3newmean <- mean(IAT2$RT[IAT2$Block==3 & IAT2$Correct==0]) + 600
  Block5newmean <- mean(IAT2$RT[IAT2$Block==5 & IAT2$Correct==0]) + 600
  Block6newmean <- mean(IAT2$RT[IAT2$Block==6 & IAT2$Correct==0]) + 600

  i <- 1 # define i counting variable for while loop
  while (i <= nrow(IAT2)) { # create while loop for Block 2 incorrect trial replacement
    if (IAT2$Block[i] == 2 && IAT2$Correct[i] == 1) {
      IAT2$RT[i] <- Block2newmean }
    i <- i + 1
  }

  i <- 1 # define i counting variable for while loop
  while (i <= nrow(IAT2)) { # create while loop for Block 3 incorrect trial replacement
    if (IAT2$Block[i] == 3 && IAT2$Correct[i] == 1) {
      IAT2$RT[i] <- Block3newmean }
    i <- i + 1
  }

  i <- 1 # define i counting variable for while loop
  while (i <= nrow(IAT2)) { # create while loop for Block 5 incorrect trial replacement
    if (IAT2$Block[i] == 5 && IAT2$Correct[i] == 1) {
      IAT2$RT[i] <- Block5newmean }
    i <- i + 1
  }

  i <- 1 # define i counting variable for while loop
  while (i <= nrow(IAT2)) { # create while loop for Block 6 incorrect trial replacement
    if (IAT2$Block[i] == 6 && IAT2$Correct[i] == 1) {
      IAT2$RT[i] <- Block6newmean }
    i <- i + 1
  }

  # Step 5a: Calculate standard deviations for all trials in blocks 2 & 5 as well as 3 & 6
  sd25 <- sd(IAT2$RT[IAT2$Block==2 | IAT2$Block==5])
  sd36 <- sd(IAT2$RT[IAT2$Block==3 | IAT2$Block==6])

  # Step 6a: Calculate means for all trials in each test block (i.e., blocks 2, 3, 5, and 6) and compute mean differences in test trials
  mdiff25 <- mean(IAT2$RT[IAT2$Block==5]) - mean(IAT2$RT[IAT2$Block==2]) #first mean difference (blocks 5 - 2)
  mdiff36 <- mean(IAT2$RT[IAT2$Block==6]) - mean(IAT2$RT[IAT2$Block==3]) #second mean difference (blocks 6 - 3)

  # Step 7a: Divide each mean difference by its associted stdev and average the two values to get an IAT effect size
  values <- c(mdiff25/sd25, mdiff36/sd36)
  IATeffect <- mean(values) #this is the first value that will be outputted from this function (the sign will be flipped if First = "Incongruent")

  # Flips the sign of the IAT effect if the IAT dataset comes from an "incongruent first" condition
  if(First == "Incongruent"){
    IATeffect = IATeffect*(-1)
  }

  # Now, do the same calculations, but for only odd trials in the dataset

  # Step 4b: Compute means of correct trials in the odd data frame and replace incorrect trials with this value + 600
  Block2oddnewmean <- mean(IATodd$RT[IATodd$Block==2 & IATodd$Correct==0]) + 600
  Block3oddnewmean <- mean(IATodd$RT[IATodd$Block==3 & IATodd$Correct==0]) + 600
  Block5oddnewmean <- mean(IATodd$RT[IATodd$Block==5 & IATodd$Correct==0]) + 600
  Block6oddnewmean <- mean(IATodd$RT[IATodd$Block==6 & IATodd$Correct==0]) + 600

  i <- 1 # define i counting variable for while loop
  while (i <= nrow(IATodd)) { # create while loop for Block 2 incorrect trial replacement
    if (IATodd$Block[i] == 2 && IATodd$Correct[i] == 1) {
      IATodd$RT[i] <- Block2oddnewmean }
    i <- i + 1
  }

  i <- 1 # define i counting variable for while loop
  while (i <= nrow(IATodd)) { # create while loop for Block 3 incorrect trial replacement
    if (IATodd$Block[i] == 3 && IATodd$Correct[i] == 1) {
      IATodd$RT[i] <- Block3oddnewmean }
    i <- i + 1
  }

  i <- 1 # define i counting variable for while loop
  while (i <= nrow(IATodd)) { # create while loop for Block 5 incorrect trial replacement
    if (IATodd$Block[i] == 5 && IATodd$Correct[i] == 1) {
      IATodd$RT[i] <- Block5oddnewmean }
    i <- i + 1
  }

  i <- 1 # define i counting variable for while loop
  while (i <= nrow(IATodd)) { # create while loop for Block 6 incorrect trial replacement
    if (IATodd$Block[i] == 6 && IATodd$Correct[i] == 1) {
      IATodd$RT[i] <- Block6oddnewmean }
    i <- i + 1
  }

  # Step 5b: Calculate standard deviations for all trials in blocks 2 & 5 as well as 3 & 6
  sd25odd <- sd(IATodd$RT[IATodd$Block==2 | IATodd$Block==5])
  sd36odd <- sd(IATodd$RT[IATodd$Block==3 | IATodd$Block==6])

  # Step 6b: Calculate means for all trials in each test block (i.e., blocks 2, 3, 5, and 6) and compute mean differences in test trials
  mdiff25odd <- mean(IATodd$RT[IATodd$Block==5]) - mean(IATodd$RT[IATodd$Block==2]) #first mean difference (blocks 5 - 2)
  mdiff36odd <- mean(IATodd$RT[IATodd$Block==6]) - mean(IATodd$RT[IATodd$Block==3]) #second mean difference (blocks 6 - 3)

  # Step 7b: Divide each mean difference by its associted stdev and average the two values to get an IAT effect size
  values <- c(mdiff25odd/sd25odd, mdiff36odd/sd36odd)
  IATeffectodd <- mean(values) #this is the first value that will be outputted from this function (the sign will be flipped if First = "Incongruent")

  # Flips the sign of the IAT effect if the IAT dataset comes from an "incongruent first" condition
  if(First == "Incongruent"){
    IATeffectodd = IATeffectodd*(-1)
    message("NOTE: The signs for this participants' IAT effect sizes were flipped (Incongruent First)")
    message(" ")
  }

  # Now, do the same calculations for the even trials (which will result in a second D score for this participant)

  # Step 4c: Compute means of correct trials in the even data frame and replace incorrect trials with this value + 600
  Block2evennewmean <- mean(IATeven$RT[IATeven$Block==2 & IATeven$Correct==0]) + 600
  Block3evennewmean <- mean(IATeven$RT[IATeven$Block==3 & IATeven$Correct==0]) + 600
  Block5evennewmean <- mean(IATeven$RT[IATeven$Block==5 & IATeven$Correct==0]) + 600
  Block6evennewmean <- mean(IATeven$RT[IATeven$Block==6 & IATeven$Correct==0]) + 600

  i <- 1 # define i counting variable for while loop
  while (i <= nrow(IATeven)) { # create while loop for Block 2 incorrect trial replacement
    if (IATeven$Block[i] == 2 && IATeven$Correct[i] == 1) {
      IATeven$RT[i] <- Block2evennewmean }
    i <- i + 1
  }

  i <- 1 # define i counting variable for while loop
  while (i <= nrow(IATeven)) { # create while loop for Block 3 incorrect trial replacement
    if (IATeven$Block[i] == 3 && IATeven$Correct[i] == 1) {
      IATeven$RT[i] <- Block3evennewmean }
    i <- i + 1
  }

  i <- 1 # define i counting variable for while loop
  while (i <= nrow(IATeven)) { # create while loop for Block 5 incorrect trial replacement
    if (IATeven$Block[i] == 5 && IATeven$Correct[i] == 1) {
      IATeven$RT[i] <- Block5evennewmean }
    i <- i + 1
  }

  i <- 1 # define i counting variable for while loop
  while (i <= nrow(IATeven)) { # create while loop for Block 6 incorrect trial replacement
    if (IATeven$Block[i] == 6 && IATeven$Correct[i] == 1) {
      IATeven$RT[i] <- Block6evennewmean }
    i <- i + 1
  }

  # Step 5c: Calculate standard deviations for all trials in blocks 2 & 5 as well as 3 & 6
  sd25even <- sd(IATeven$RT[IATeven$Block==2 | IATeven$Block==5])
  sd36even <- sd(IATeven$RT[IATeven$Block==3 | IATeven$Block==6])

  # Step 6c: Calculate means for all trials in each test block (i.e., blocks 2, 3, 5, and 6) and compute mean differences in test trials
  mdiff25even <- mean(IATeven$RT[IATeven$Block==5]) - mean(IATeven$RT[IATeven$Block==2]) #first mean difference (blocks 5 - 2)
  mdiff36even <- mean(IATeven$RT[IATeven$Block==6]) - mean(IATeven$RT[IATeven$Block==3]) #second mean difference (blocks 6 - 3)

  # Step 7c: Divide each mean difference by its associted stdev and average the two values to get an IAT effect size
  values <- c(mdiff25even/sd25even, mdiff36even/sd36even)
  IATeffecteven <- mean(values) #this is the second value that will be outputted from this function (the sign will be flipped if First = "Incongruent")

  # Flips the sign of the IAT effect if the IAT dataset comes from an "incongruent first" condition
  if(First == "Incongruent"){
    IATeffecteven = IATeffecteven*(-1)
  }

  Analysis <- c("Overall IAT effect size:",
                "Effect size for ODD trials only:",
                "Effect size for EVEN trials only:",
                "Proportion of trials with RTs under 300ms:")

  Value <- c(IATeffect, IATeffectodd, IATeffecteven, SpeedProp)

  results <- data.frame(Analysis, Value)

  darray <- c(IATeffectodd, IATeffecteven) #array containing the two D effect sizes, to be returned by the function

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  message("Seconds taken to run this code:")
  message(round(time.taken, 3))
  message(" ")

  # Tell the researcher to exclude this participant if they went too fast on over 10% of the trials (< 300 ms)
  if (SpeedProp > 0.10) {
    message("NOTE: THE PARTICIPANT WENT TOO FAST ON THE IAT -- EXCLUDE")
    message("Proportion of trials with responses faster than 300 ms:")
    message(round(SpeedProp, 3))
    message(" ")
    return(results)
  }

  # Otherwise, give them the appropriate IAT effect size for this participant
  if (SpeedProp < 0.10) {
    return(results)
  }

}

