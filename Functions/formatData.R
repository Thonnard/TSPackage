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

