# Load in IAT_Data
library(IATanalytics)
df <- subset(IAT_Data[61:300,], select = c(blockNum, # first column should be Block (0-6)
                                           trialNum, # second column should be Trial (0-19 for training blocks, 0-39 for test blocks)
                                           corrans, # third column should be the category the trial belongs to (e.g., "pleasant," dependent on your IAT
                                           stimulus, # fourth column should be type of item within that category (again, dependent on your IAT)
                                           correct, # fifth column should be a dummy variable indicating whether the participant was correct
                                           RT)) # sixth and final column should be RT in ms))
df$RT <- df$RT*1000 # change RT from seconds to ms 

# Re-code blockNum so it's sequential:
df$blockNum[1:40] <- 0
df$blockNum[41:80] <- 1
df$blockNum[81:100] <- 2
df$blockNum[101:140] <- 3
df$blockNum[141:160] <- 4
df$blockNum[161:180] <- 5
df$blockNum[181:240] <- 6

IATanalytics(df)