# Load in IAT_Data
# library(IATanalytics)
iat_processed <- subset(IAT_Data[61:300,], select = c(blockNum, # first column should be Block (0-6)
                                           trialNum, # second column should be Trial (0-19 for training blocks, 0-39 for test blocks)
                                           corrans, # third column should be the category the trial belongs to (e.g., "pleasant," dependent on your IAT
                                           stimulus, # fourth column should be type of item within that category (again, dependent on your IAT)
                                           correct, # fifth column should be a dummy variable indicating whether the participant was correct
                                           RT)) # sixth and final column should be RT in ms))
# iat_processed$RT <- iat_processed$RT*1000 # change RT from seconds to ms 

# Re-code blockNum so it's sequential:
iat_processed$blockNum[1:40] <- 0
iat_processed$blockNum[41:80] <- 1
iat_processed$blockNum[81:100] <- 2
iat_processed$blockNum[101:140] <- 3
iat_processed$blockNum[141:160] <- 4
iat_processed$blockNum[161:180] <- 5
iat_processed$blockNum[181:240] <- 6

IATanalytics(iat_processed)
