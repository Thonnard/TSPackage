# Description
# Using the descriptives.R function will provide the user with summary statistics by group of a specified variable derived
# from a dataset. It will return an output and write a "[var_name]Descriptives.csv" file in the current directory.
# 
# Parameters
# data: name of dataset
# var_name: name of variable of interest
# group: name of grouping variable (e.g.: "Genotype", "Sex", "Treatment", ...)
# includeGroups: decide which groups you'd like to evaluate, includes by default all groups
#
# Example
# descriptives(data, "latency", group = "Treatment")
# descriptives(data = data, var_name = "latency", group = "Treatment", includeGroups = "all")
# descriptives(data = data, var_name = "latency", group = "Treatment", includeGroups = c("control", "drug3"))
#
# TO DO: Direct plots to [var_name] folder within wd(). This folder could be used for ANOVA function?
#        Adjust N to group size instead of overall observations.
#
# ERRATUM : Function plots boxplot with name of first observation; no idea why. Needs to be solved.
#
descriptives <- function (data, var_name, group, includeGroups = "all") {
  # dependencies
  require(ggplot2) # graphs
  require(xlsx) 

  # create data frame
  data <- as.data.frame(data)
  
  # changing group column name with parameter input so group information can be extracted
  colnames(data)[colnames(data) == group] <- "Group"
  
  # select groups (optional)
  if (includeGroups[1] != "all") {
    data <- data[data[, "Group"] %in% includeGroups,]
  }
  
  # core function
  my.summary <- function(x) c(Observations = (length(x)-sum(is.na(x))), 
                              Missing = sum(is.na(x), na.rm = TRUE),
                              Mean = mean(x, na.rm = TRUE), 
                              Median = median(x, na.rm = TRUE), 
                              SD = sd(x, na.rm = TRUE), 
                              SEM = sd((x)/sqrt(length(x)-sum(is.na(x))), na.rm = TRUE), 
                              Min = min(x, na.rm = TRUE), 
                              Max = max(x, na.rm = TRUE), 
                              quantile(x, .25, na.rm = TRUE), 
                              IQR = IQR (x, na.rm = TRUE), 
                              quantile(x, .75, na.rm = TRUE))
  
  # execute core function on var_name
  output <- data.frame(do.call(rbind, tapply(data[,var_name], data$Group, my.summary)))
  output$Group <- rownames(output)
  rownames(output) <- c()
  
  # find N per group and add to output
  a <- unique(data$Group)
  N <- vector(length = 0)
  G <- vector(length = 0)
  for(i in a){
    temp <- data[data[,"Group"] == i,]
    N[i] <- length(unique(temp$Animal))
    G[i] <- i
  }
  temptable <- as.data.frame(cbind(N, G))
  output$N <- temptable[match(output$Group, temptable$G),1]
  output <- output[,c(12,13, 1:11)]

  # create dir for all output
    wd <- getwd()
  dir <- paste("descriptives_", Sys.Date(), sep="")
  dir.create(dir)
  
  # create and set dir for graphic output
  setwd(dir)
  dir.create("Plots")
  setwd("Plots")
  
  # create boxplots to visualize descriptives
  DescriptivesPlot <- ggplot(data = data, aes(x = Group, y = data[,var_name], color = Group)) + 
    stat_boxplot(geom = "errorbar", width = 0.1, size = 0.2) +      
    geom_boxplot(alpha = 1, size = 0.2, width = 0.5, outlier.colour = "red", outlier.fill = "red", outlier.shape = 16, outlier.size = 1, outlier.alpha = .5) +
    stat_summary(fun.y=mean, geom = "point", shape = 3, size = 1) +
    scale_y_continuous(expand = c(0.05, 0), limits = c(min(data[,var_name]), max(data[,var_name])), breaks = pretty(data[,var_name], n = 12)) +
    scale_colour_grey(start = 0.1, end = .8) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.title.x = element_text(size = 9), axis.text.x = element_text(size = 7), 
          axis.title.y = element_text(size = 9), axis.text.y = element_text(size = 7), 
          plot.title = element_text(size = 10, hjust = 0.5)) +
    labs(title = paste("Box plots of summary statistics of", " ", "'", var_name, "'", sep = ""),  x = "", y = var_name)
  
  ggsave(filename = paste(var_name, "Descriptives.jpeg", sep = " "), 
         plot = DescriptivesPlot, 
         width = 16, height = 6, units = "cm", dpi = 600)
  
  # create data dir and write table to csv file
  setwd(wd)
  setwd(dir)
  dir.create("Data")
  setwd("Data")
  write.csv(output, file = paste(var_name, "Descriptives.csv", sep=" "))
  write.xlsx(output, file = paste(var_name, " ", "Descriptives.xlsx", sep = ""), col.names = TRUE, row.names = FALSE, append = FALSE)
  setwd(wd)
  
  # return table and plot (this might cause issues for .rmd).
  return(list("Descriptives" = output, "Plot" = DescriptivesPlot))    
}
