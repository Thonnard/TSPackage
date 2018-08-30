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
# TO DO: Insert possibility for flexible column names.
#        Direct plots to the 'Plots' folder within wd()? Consistent with tsnlm function.
#
Descriptives <- function (data, var_name, group) {
    # create data frame
    data <- as.data.frame(data)
    
    # select groups (optional)
    if (group[1] != "all") {
      data <- data[data[, "Group"] %in% group,]
    }
    
    # core function
    my.summary <- function(x) c(N = (length(x)-sum(is.na(x))), 
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
    do.call(rbind, tapply(data[,var_name], data$Group, my.summary))
    
    # write table to csv file
    output <- data.frame(do.call(rbind, tapply(data[,var_name], data$Group, my.summary)))
    write.csv(output, file = paste(var_name,"Descriptives.csv", sep = ""))
    
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
            ggsave(filename = paste(var_name, " ", "Descriptives", ".jpeg", sep = ""), 
                   plot = DescriptivesPlot, 
                   device = "jpeg", 
                   path = getwd(), 
                   width = 16, height = 6, units = "cm", dpi = 600)
    
    # return table and plot (this might cause issues for .rmd).
    return(list (output, DescriptivesPlot))
  }
