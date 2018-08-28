# Description
# Using the descriptives.R function will provide the user with summary statistics by group of a specified variable derived
# from a dataset. It will return an output and write a "[var_name]Descriptives.csv" file in the current directory.
# 
# Parameters
# data: .csv format
# var_name: name of variable of interest
# group: optional, default includes all groups
#
# Example
# descriptives(data, "latency", group = "all")
# descriptives(data = data, var_name = "latency", group = "all")
# descriptives(data = data, var_name = "latency", group = c("control", "drug3"))
#
# TO DO: Define Boolean TRUE/FALSE argument in function for group, with default = TRUE. 
#        (E.g., if group = TRUE, report summary statistics by group. If group = FALSE, report general summary statistics.)

Descriptives <- function (data = data, var_name, group="all") {
  # create data frame
  data <- as.data.frame(data)
  
  # select groups (optional)
  if (group[1] != "all") {
    data <- data[data[, "Group"] %in% group,]
  }
  
  # core function
  my.summary <- function(x) c(N = length(x), 
                              Missing = sum(is.na(x), na.rm = TRUE),
                              Mean = mean(x, na.rm = TRUE), 
                              Median = median(x, na.rm = TRUE), 
                              SD = sd(x, na.rm = TRUE), 
                              SEM = sd((x)/sqrt(length(x)), na.rm = TRUE), 
                              Min = min(x, na.rm = TRUE), 
                              Max = max(x, na.rm = TRUE), 
                              quantile(x, .25, na.rm = TRUE), 
                              IQR = IQR (x, na.rm = TRUE), 
                              quantile(x, .75, na.rm = TRUE))
  
  # execute core function on var_name
  do.call(rbind, tapply(data[,var_name], data$Group, my.summary))
  
  # write table to csv file
  output <- data.frame(do.call(rbind, tapply(data[,var_name], data$Group, my.summary)))
  write.csv(output, file = paste(var_name,"Descriptives.csv", sep = ""))
  
  # return table in console
  return(output)
}
