formatData <- function(data){
  # read data
  data <- read.csv(data, header = TRUE, sep = ",")
  
  # change column names (coln should contain every column name)
  coln <- c("Protocol","Box", "RunTime", "Animal", "Group", "User", "Duration", "TotalTrials", "CorrectionTrials",
            "PercCorrect", "LeftITITouches", "RightITITouches", "Latency_CorrectTouch", "Latency_IncorrectTouch", 
            "Latency_CorrectLeftTouch", "Latency_CorrectRightTouch", "Latency_CorrectRewardCollection", 
            "Duration_10block_1", "Duration_10block_2", "Duration_10block_3", "CorrectTrials_10block_1",
            "CorrectTrials_10block_2", "CorrectTrials_10block_3", "PercCorrect_10block_1", "PercCorrect_10block_2", 
            "PercCorrect_10block_3", "CorrectionTrials_10block_1", "CorrectionTrials_10block_2", "CorrectionTrials_10block_3")
  for (i in 1:ncol(data)) {names(data)[i] <- coln[i]}
  
  # correct duration (comma vs dots)
  # if(x > 3700){x <- x/1000}
  
  # add extra columns (perseveration etc, SESSION!!)
  # data$Session <- 
  # data$Perseversation <-
  
  # order data
  data <- data[with(data, order(Animal,Session)), ]
  
  # save data
  write.csv(data, "data.cvs", row.names=FALSE)
}
