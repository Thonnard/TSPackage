# Description
# formatData returns...
#
# Parameters
# data, requires csv dataset containing 29 columns non-randomly ordered. This dataset can be acquired using this ABET output file: TSPackage/GetOutput/PD_RV_Analysis.abetRpt 
# group, optional, default includes all groups
#
# Examples
# formatData("mydata.csv")
# formatData(data="mydata.csv", group=c("controls", "drug1", "drug3"))
#
# TODO
# check variable type for Sessions

formatData <- function(data, group = "all"){
  # read data
  data <- read.csv(data, header = TRUE, sep = ",")
  
  # change column names (coln should contain every column name in this dataset in right order)
  coln <- c("Protocol", "Box", "TestDate", "Animal", "Group", "User", "Duration", "Trials", "CorrectionTrials",
            "PercCorrect", "LeftITITouches", "RightITITouches", "Latency_Correct", "Latency_Incorrect", 
            "Latency_CorrectLeft", "Latency_CorrectRight", "Latency_RewardCollection", 
            "Duration_block_1", "Duration_block_2", "Duration_block_3", "CorrectTrials_block_1",
            "CorrectTrials_block_2", "CorrectTrials_block_3", "PercCorrect_block_1", "PercCorrect_block_2", 
            "PercCorrect_block_3", "CorrectionTrials_block_1", "CorrectionTrials_block_2", "CorrectionTrials_block_3")
  for (i in 1:ncol(data)) {names(data)[i] <- coln[i]}
  
  # set variable type
  data$Protocol <- factor(data$Protocol)
  data$Box <- factor(data$Box)
  data$Animal <- factor(data$Animal)
  data$Group <- factor(data$Group)
  data$Duration <- as.numeric(gsub(",", ".", data$Duration))
  data$Trials <- as.numeric(data$Trials)
  data$CorrectionTrials <- as.numeric(data$CorrectionTrials)
  data$PercCorrect <- as.numeric(gsub(",", ".", data$PercCorrect))
  data$LeftITITouches <- as.numeric(data$LeftITITouches)
  data$RightITITouches <- as.numeric(data$RightITITouches)
  data$Latency_Correct <- as.numeric(gsub(",", ".", data$Latency_Correct))
  data$Latency_Incorrect <- as.numeric(gsub(",", ".", data$Latency_Incorrect))
  data$Latency_CorrectLeft <- as.numeric(gsub(",", ".", data$Latency_CorrectLeft))
  data$Latency_CorrectRight <- as.numeric(gsub(",", ".", data$Latency_CorrectRight))
  data$Latency_RewardCollection <- as.numeric(gsub(",", ".", Latency_Reward$Collection))
  data$Duration_block_1 <- as.numeric(gsub(",", ".", data$Duration_block_1))
  data$Duration_block_2 <- as.numeric(gsub(",", ".", data$Duration_block_2))
  data$Duration_block_3 <- as.numeric(gsub(",", ".", data$Duration_block_3))
  data$CorrectionTrials_block_1 <- as.numeric(data$CorrectionTrials_block_1)
  data$CorrectionTrials_block_2 <- as.numeric(data$CorrectionTrials_block_2)
  data$CorrectionTrials_block_3 <- as.numeric(data$CorrectionTrials_block_3)
  data$PercCorrect_block_1 <- as.numeric(gsub(",", ".", data$PercCorrect_block_1))
  data$PercCorrect_block_2 <- as.numeric(gsub(",", ".", data$PercCorrect_block_2))
  data$PercCorrect_block_3 <- as.numeric(gsub(",", ".", data$PercCorrect_block_3))
  data$CorrectionTrials_block_1 <- as.numeric(data$CorrectionTrials_block_1)
  data$CorrectionTrials_block_2 <- as.numeric(data$CorrectionTrials_block_2)
  data$CorrectionTrials_block_3 <- as.numeric(data$CorrectionTrials_block_3)
  
  # order data
  data <- data[with(data, order(Animal,as.Date(TestDate, format="%d/%m/%Y"))), ]

  # extra columns
      # sessions
      data$Session <- sequence(rle(as.character(data$Animal))$lengths)
  
      # log transformation reaction time data
      logs <- c("Latency_Correct", "Latency_Incorrect", "Latency_CorrectLeft", "Latency_CorrectRight", 
                    "Latency_RewardCollection")
      data[paste("Log", logs, sep="_")] <- log(data[logs])
  
      #correct trials per session
      data <- transform(data, CorrectTrials = Trials*(PercCorrect)/100)
      data$CorrectTrials <- round(data$CorrectTrials)
      
      #incorrect trials per session
      data <- transform(data, IncorrectTrials = Trials - CorrectTrials)
  
      #perseveration index (e.g., Brigman, et al., 2008; Piiponniemi, et al., 2017)
      data <- transform(data, PerseverationIndex = CorrectionTrials/(IncorrectTrials))

  # select groups (optional)
  if (group != "all") {
    data <- data[data[, "Group"] == group,]
  }
  
  # save data
  write.csv(data, "data.csv", row.names=FALSE)
  
  # create return table
  a <- unique(data$Group)
  N <- vector(length = 0)
  for(i in a){
    temp <- data[data[,"Group"] == i,]
    N[i] <- length(unique(temp$Animal))
    }
  table <- as.data.frame(N)
  
  # return
  return(table)  
}
