formatData <- function(data){
  # read data
  data <- read.csv(data, header = TRUE, sep = ",")
  
  # change column names
  
  # correct duration (comma vs dots)
  # x <- x/1000
  
  # add extra columns (perseveration etc, SESSION!!)
  
  # order data
  data <- data[with(data, order(Animal,Session)), ]
  
  # save data
  write.csv(data, "data.cvs", row.names=FALSE)
}
