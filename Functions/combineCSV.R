# Description
# The combineCSV function combines all csv files in a specific folder and creates a single output data file ("data (current date).csv")
# WARNING: the input folder should only contain CSV files and all CSV files should have an equal number of columns in the same order!
# WARNING: if the output file already exists (e.g. when you run this function twice), an ERROR message will appear to prevent overwriting
#
# Parameters
# dir, folder with csv files, default: getwd() (i.e., original working directory in R)
#
# Examples
# combineCSV()
# combineCSV("Data")
# combineCSV(dir="C:/Users/David/Documents/R/TSPackage/Data")

combineCSV <- function(dir = getwd()){
  # save original working directory
  wd <- getwd()

  # set working directory
  setwd(dir)

  # get file names
  files <- list.files(full.names=TRUE)
  
  # read data 
  read <- lapply(files, function(i){read.csv(i, header = TRUE, sep = ",")})
  
  # create data frame
  data <- do.call(rbind.data.frame, read)
  
  # extract headers
  header <- read.csv(list.files()[1], header = TRUE, sep = ",")
  header <- names(header)
  
  # name columns in output file
  names(data) <- header
  
  # create output directory
  diroutput <- paste("Combined_data_", Sys.Date(), sep="")
  dir.create(diroutput)
  
  # save file in output dir
  setwd(diroutput)
  getoutputdir <- getwd()
  output <- paste("data (", Sys.Date(), ").csv", sep="")
  if (!file.exists(output)) {
    write.csv(data, output, row.names=FALSE)
  }
  else {
    writeLines(paste("WARNING: Output file (", output, ") already exists!\n", sep=""))
    stop("Please check output folder and remove old output files!")
  }
  
  # set original working directory
  setwd(wd)
  
  # return
  message <- paste("CSV files in folder ", dir, " were combined in 1 output file (", output, ") that was saved in ", getoutputdir, ".", sep="")
  return(message)
}
