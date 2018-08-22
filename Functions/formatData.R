formatData <- function(data){
  # read data
  data <- read.csv(data, header = TRUE, sep = ",")
  
  # change column names
  names(data)[names(data)=="CorrectionTrials..1."] <- "CorrectionTrials"
  names(data)[names(data)=="Schedule.run.date"] <- ""
  names(data)[names(data)=="Duration..1."] <- ""
  names(data)[names(data)=="Trials..1."] <- ""
  names(data)[names(data)=="CorrectionTrials..1."] <- ""
  names(data)[names(data)=="PercCorrect..1."] <- ""
  names(data)[names(data)=="LeftITITouches..1."] <- ""
  names(data)[names(data)=="RightITITouches..1."] <- ""
  names(data)[names(data)=="Duration_10block..1."] <- ""
  names(data)[names(data)=="Duration_10block..2."] <- ""
  names(data)[names(data)=="Duration_10block..3."] <- ""
  names(data)[names(data)=="CorrectTrials_10block..1."] <- ""
  names(data)[names(data)=="CorrectTrials_10block..2."] <- ""
  names(data)[names(data)=="CorrectTrials_10block..3."] <- ""
  names(data)[names(data)=="PercCorrect_10block..1."] <- ""
  names(data)[names(data)=="PercCorrect_10block..2."] <- ""
  names(data)[names(data)=="PercCorrect_10block..3."] <- ""
  names(data)[names(data)=="CorrectionTrials_10block..1."] <- ""
  names(data)[names(data)=="CorrectionTrials_10block..2."] <- ""
  names(data)[names(data)=="CorrectionTrials_10block..3."] <- ""

  # maybe it's better to work with a list and for loop, in case the column names differ depending on platform... (should work fine as long as order doesn't change)
  coln <- c("Schedule","Box", "...")  # should contain every column name
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
