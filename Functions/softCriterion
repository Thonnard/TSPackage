# Description
# When using a fixed number of test sessions, this function returns at which session the subject reached the soft criterion
# Output in csv format, including animal id, group information and criterion (i.e. session)
# When criterion was never reached, criterion value will be "NA" 
#
# Parameters
# data: csv format
# crit: number of successive sessions to reach criterion, default = 2
# score: % correct needed to reach criterion, default = 80
#
# Example
# softCriterion("data.csv")
#
# TODO
# add optional dv parameter

softCriterion <- function(data, crit = 2, score = 80) {
  # read data
  data <- read.csv(data, header = TRUE, sep = ",")
  
  # attach
  attach(data)

  # calculate success
  data$Success <- ifelse(PercCorrect >= score, "TRUE", "FALSE")
  
  # variables for output
  list <- unique(Animal)
  id <- vector(length = 0)
  group <- vector(length = 0)
  criterion <- vector(length = 0)
  
  # for loop for every subject
  for(i in list) {
    # select data for every animal
    dataID <- data[data[, "Animal"] == i,]
    
    # order data correctly
    dataID <- dataID[with(dataID, order(Session)), ]
    
    # determine sequence | this needs to be in this for loop to avoid mistakes!
    dataID$seq <- sequence(rle(as.character(dataID$Success))$lengths)
    
    # isolate critical session number
    criterion[i] <- dataID[dataID[, "Success"] == "TRUE" & dataID[, "seq"] == crit,]$Session[1]
    
    # ID
    id[i] <- i
    
    # Group
    group[i] <- as.character(dataID$Group[1])
  }
  
  # create output table
  output <- data.frame(id, group, criterion)
  rownames(output) <- c()
  
  # write table to csv file
  write.csv(output, file = "softCriterionOutput.csv")
  
  # detach
  detach(data)
  
  # return table in console
  return(output)
}
