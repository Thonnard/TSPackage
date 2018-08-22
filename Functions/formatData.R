formatData <- function(data, group = "all"){
  # read data
  data <- read.csv(data, header = TRUE, sep = ",")
  
  # change column names (coln should contain every column name in this dataset in right order)
  coln <- c("Protocol", "Box", "RunTime", "Animal", "Group", "User", "Duration", "TotalTrials", "CorrectionTrials",
            "PercCorrect", "LeftITITouches", "RightITITouches", "Latency_CorrectTouch", "Latency_IncorrectTouch", 
            "Latency_CorrectLeftTouch", "Latency_CorrectRightTouch", "Latency_CorrectRewardCollection", 
            "Duration_block_1", "Duration_block_2", "Duration_block_3", "CorrectTrials_block_1",
            "CorrectTrials_block_2", "CorrectTrials_block_3", "PercCorrect_block_1", "PercCorrect_block_2", 
            "PercCorrect_block_3", "CorrectionTrials_block_1", "CorrectionTrials_block_2", "CorrectionTrials_block_3")
  for (i in 1:ncol(data)) {names(data)[i] <- coln[i]}
  
  # set variable type
  data$Group <- factor(data$Group)
  #...
  
  # order data
  data <- data[with(data, order(Animal,as.Date(RunTime, format="%d/%m/%Y"))), ]
  
  # correct duration (comma vs dots)
  # if(x > 3700){x <- x/1000}
  
  # add extra columns (perseveration etc, SESSION!!)
  data$Session <- sequence(rle(as.character(data$Animal))$lengths)
  # data$Perseversation <-
  
  # select groups
  
  # remove NAs or give warning
  
  # save data
  write.csv(data, "data.csv", row.names=FALSE)
}
