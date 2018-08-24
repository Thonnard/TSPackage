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
# Descriptives(data, "latency")
# Descriptives(data = "data.csv", var_name = "latency")
# Descriptives(data = "data.csv", var_name = "latency", group = c("control", "drug3"))

descriptives <- function (data, var_name, group = "all") {
  
  # select groups (optional)
  if (group[1] != "all") {
    data <- data[data[, "Group"] %in% group,]
  }
  
  # core function
  my.summary <- function(x) c(N = length(x), Mean = mean(x), Median = median(x), SD = sd(x), SEM = sd(x)/sqrt(length(x)), Min = min(x), Max = max(x), quantile(x, .25), IQR = IQR (x), quantile(x, .75))
  
  # execute core function on var_name
  do.call(rbind, tapply(data[,var_name], data$Group, my.summary))
  
  # write table to csv file
  output <- data.frame(do.call(rbind, tapply(data[,var_name], data$Group, my.summary)))
  write.csv(output, file = paste(var_name,"Descriptives.csv", sep = ""))
  
  # return table in console
  return(output)
}
