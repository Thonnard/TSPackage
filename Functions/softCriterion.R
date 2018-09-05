# Description
# When using a fixed number of test sessions, this function returns at which session the subject reached the soft criterion
# Output in csv format, including animal id, group information and criterion (i.e. session)
# When criterion was never reached, criterion value will be "NA" 
#
# Parameters
# dv: dependent varialbe (e.g. percentage correct)
# session: session information for every subject
# id: animals
# group: group information (optional)
# data: data frame (long format) containing at least one dependent variable, session, id and group information (optional) 
# crit: number of successive sessions to reach criterion, default = 2
# score: score needed to reach criterion, default = 80
#
# Examples
# softCriterion(dv="Scooore", session="Numero", id="Beeste", group="Gang", data=data)
# softCriterion(dv="PercCorrect", session="Session", id="Animal", data=data)
#
# TODO: add group information and create graph

softCriterion <- function(dv, session, id, group="FOO", data, crit = 2, score = 80) {
  # dependencies
  require(xlsx)
  
  # create data frame
  data <- as.data.frame(data)
  
  # create dir for all output
  wd <- getwd()
  dir <- paste("softCriterion_", Sys.Date(), sep="")
  dir.create(dir)
  
  # create readable variables from parameter input
  depvar <- eval(parse(text = paste("data$", dv, sep="")))
  animal <- eval(parse(text = paste("data$", id, sep="")))
  
  # calculate success
  data$Success <- ifelse(depvar >= score, "TRUE", "FALSE")
  
  # variables for for loop and output
  list <- unique(animal)
  subject <- vector(length = 0)
  gr <- vector(length = 0)
  criterion <- vector(length = 0)
  
  # for loop for every subject to isolate critical session number
  for(i in list) {
    # select data for every animal
    dataID <- data[data[, id] == i,]
    
    # order data correctly
    dataID <- dataID[with(dataID, order(eval(parse(text = session)))), ]
    
    # determine sequence | this needs to be in this for loop to avoid mistakes!
    dataID$seq <- sequence(rle(as.character(dataID$Success))$lengths)
    
    # isolate critical session number
    colnames(dataID)[colnames(dataID) == session] <- "Session"  # changing name with parameter input so session information can be extracted
    criterion[i] <- dataID[dataID[, "Success"] == "TRUE" & dataID[, "seq"] == crit,]$Session[1]
    
    # ID
    subject[i] <- i
    
    # Group
    if(group!="FOO"){
      colnames(dataID)[colnames(dataID) == group] <- "Group"  # changing name with parameter input so group information can be extracted
      gr[i] <- as.character(dataID$Group[1])
    }
  }
  
  # create output table
  if(group=="FOO") {
    output <- data.frame(subject, criterion)
  }
  else {
    output <- data.frame(subject, gr, criterion)
  }
  rownames(output) <- c()
  
  # create and set dir for graphic output
  setwd(dir)
  dir.create("Plots")
  setwd("Plots")
  
  # create graph
  
  
  # create dir and write table to csv file
  setwd(wd)
  setwd(dir)
  dir.create("Data")
  setwd("Data")
  write.csv(output, file = "softCriterion.csv")
  write.xlsx(output, "softCriterion.xlsx", col.names = TRUE, row.names = FALSE, append = FALSE)
  setwd(wd)
  
  # return data but don't show it
  invisible(output)
}
