# Description
# Using the varAnova.R function will provide the user with an extended analysis of variance of a specified variable. 
# The output will consist of an Anova tabel, contrast analysis, homoscedasticity and normality tests, and plots to support 
# the findings. Data tables and plots will be directed to separate folders. The parent folder carries the name ANOVA_[var_name].
# 
# Parameters
# data: name of dataset
# var_name: name of variable of interest
# id: name of subject (e.g.: "Patient", "Animal", ...)
# group: name of grouping variable (e.g.: "Genotype", "Sex", "Treatment", ...)
# SS: type Sum of Squares (e.g.: 3, 2, "II", "I")
# adjust: correction used for contrast analysis (e.g., "Tukey", "Bonferroni", ...)
#
# Example
# varAnova(data, "latency", id = "Animal", group = "Treatment")
# varAnova(data = data, var_name = "latency", id = "Animal", group = "Treatment", SS = 3)
# varAnova(data = data, var_name = "latency", id = "Animal", group = "Treatment", SS = "II", adjust = "Tukey")
#
# TO DO: CODE IS STILL UNDER CONSTRUCTION !!!
#        Only functional for var_name = "PercCorrect". Few lines still not dynamic (see line 42 and 101).
#        Implement standardized residuals instead of actual residuals? Couldn't extract them from aov_ez(). Maybe lm()?
#        Add a common legend for "AllPlots"? Questioning the necessity of the triple plot. We have all three single plots (without legend).
#        Add some more formal tests for assumptions? Maybe make a general .csv file entitled "[var_name]_Assumptions" and paste all
#        formal tests into this file using sink() function.
#
varAnova <- function (data, id, var_name, group, SS = 3, adjust) {
    # dependencies
    require(ggplot2)  # graphs
    require(afex)     # statistical analyses
    require(emmeans)  # post hoc comparisons and contrast testing
    require(cowplot)  # Pastes plots together (used for "AllPlots")
    
    # create data frame
    data <- as.data.frame(data)
    
    # changing group column name with parameter input so group information can be extracted
    colnames(data)[colnames(data) == group] <- "Group"
    
    # analysis of variance
    m <- aov_ez(data = data, id = id, dv = var_name, between = "Group", type = SS)
    M <- print(m)
    
    contrast <- emmeans(m, pairwise ~ Group, adjust = adjust)
    CONTRAST <- (as.data.frame(contrast))
    
    df <- as.data.frame(aggregate(PercCorrect ~ Animal + Group, FUN = mean, data = data)) # trouble with aggregate function, doens't allow var_name to be implemented?
    res <- residuals(m$lm)                                                                
    df[paste("Residuals")] <- residuals(m$lm)
    
    logs <- c("Latency_Correct", "Latency_Incorrect", "Latency_CorrectLeft", "Latency_CorrectRight", 
              "Latency_RewardCollection")
    data[paste("Log", logs, sep="_")] <- log(data[logs])
    
    # create dir for all output
    wd <- getwd()
    dir <- paste("ANOVA_",var_name, Sys.Date(), sep="")
    dir.create(dir)
    
    # create and set dir for graphic output
    setwd(dir)
    dir.create("Plots")
    setwd("Plots")
    
    # create plot showing main results anova
    AnovaPlot <-  ggplot(data = CONTRAST, aes(x = emmeans.Group, y = emmeans.emmean, group = emmeans.Group)) +
      geom_crossbar(aes(ymin = emmeans.lower.CL, ymax = emmeans.upper.CL, alpha = 0.4, group = emmeans.Group), width = 0.3, fatten = 3) +
      geom_errorbar(aes(ymin = emmeans.emmean - emmeans.SE, ymax = emmeans.emmean + emmeans.SE), width = 0, color = "black") +
      geom_dotplot(data = df, aes(x = Group, y = df[,var_name], group = Group), binaxis = "y", stackdir = "center", binwidth = 1, alpha = 0.4) +
      #geom_point(data = df, aes(x = Group, y = PercCorrect, color = Group), size = 2, alpha = 0.5, position = position_jitter(0.1)) +
      scale_y_continuous(expand = c(0.1, 0.1), limits = c(min(df[,var_name]), max(df[,var_name])), breaks = pretty(df[,var_name], n = 7)) +
      theme_bw() +
      scale_fill_grey(start = 0.1, end = .9) +
      scale_color_grey(start = 0.1, end = .9) +
      theme(legend.position = "none", panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.title.x = element_text(size = 9), axis.text.x = element_text(size = 7), 
            axis.title.y = element_text(size = 9), axis.text.y = element_text(size = 7), 
            plot.title = element_text(size = 10, hjust = 0.5)) +
      labs(title = paste("ANOVA of", " ", "'", var_name, "'", " ", sep = ""),  x = "", y = var_name)
      ggsave(filename = paste(var_name, "ANOVA.jpeg", sep = ""), 
           plot = AnovaPlot, 
           width = 16, height = 6, units = "cm", dpi = 600)
    
    # create residual plot
    Levene <- as.data.frame(leveneTest(df[,var_name] ~ as.factor(df[,group]), data = df))
    ResPlot <- ggplot(data = df, aes(x = Group, y = Residuals)) + 
      geom_point(alpha = 0.5, position = position_jitter(width = 0.1)) +
      geom_hline(yintercept = 0, size = 0.75, linetype = "dotted") +
      scale_y_continuous(breaks = seq(-15, 15, by = 10), limits = c(-15, 15)) +
      labs(title = "Residual Plot", x = "Lesion", y = "Residuals") +
      theme_bw() +
      scale_fill_grey(start = 0.1, end = .9) +
      scale_color_grey(start = 0.1, end = .9) +
      theme(aspect.ratio = 1) +
      theme(legend.position = "none", panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.title.x = element_text(size = 9), axis.text.x = element_text(size = 7), 
            axis.title.y = element_text(size = 9), axis.text.y = element_text(size = 7), 
            plot.title = element_text(size = 10, hjust = 0.5))
      ggsave(filename = paste(var_name, "ResPlot.jpeg", sep = ""), 
           plot = ResPlot, 
           width = 10, height = 10, units = "cm", dpi = 600)
    
    # control for normal distribution of residuals by using the Shapiro-Wilk test and Q-Q plot.
    Shapiro <- data.matrix(shapiro.test(df$PercCorrect)) # var_name needs to be implemented here. Error: is.numeric(x) is not TRUE
    QQPlot <- ggplot(data = df, aes(sample = Residuals)) +
      stat_qq(inherit.aes = TRUE, alpha = 0.5, na.rm = TRUE) +
      #geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 0.4, alpha = 0.5) +
      theme_bw() +
      theme(aspect.ratio = 1) +
      labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Residuals") +
      theme(legend.position = "none", panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.title.x = element_text(size = 9), axis.text.x = element_text(size = 7), 
            axis.title.y = element_text(size = 9), axis.text.y = element_text(size = 7), 
            plot.title = element_text(size = 10, hjust = 0.5))
      ggsave(filename = paste(var_name, "QQPlot.jpeg", sep = ""), 
           plot = QQPlot, 
           width = 10, height = 10, units = "cm", dpi = 600)
    
    #Control for distribution of data via a density plot.
    DensityPlot <- ggplot(data = df, aes(x = PercCorrect, fill = Group)) +
      geom_density(inherit.aes = TRUE, alpha = 0.6, size = 0.2) + 
      scale_fill_grey(start = 0, end = .9) +
      scale_x_continuous(expand = c(0.05, 0), limits = c(min(df[,var_name]), max(df[,var_name])), breaks = pretty(df[,var_name], n = 6)) +
      labs(title = "Density Plot", x = var_name, y = "Density") +
      #geom_rug(data = df, aes(x = PercCorrect, group = Group, color = Group), size = 0.1, alpha = 1) +
      scale_color_grey(start = 0, end = .9) +
      theme_bw() +
      theme(aspect.ratio = 1) +
      theme(legend.position = "none", panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.title.x = element_text(size = 9), axis.text.x = element_text(size = 7), 
            axis.title.y = element_text(size = 9), axis.text.y = element_text(size = 7), 
            plot.title = element_text(size = 10, hjust = 0.5))
      ggsave(filename = paste(var_name, "DensityPlot.jpeg", sep = ""), 
           plot = DensityPlot, 
           width = 10, height = 10, units = "cm", dpi = 600)
    
    AllPlots <- plot_grid(ResPlot, QQPlot, DensityPlot, align = "h", nrow = 1, rel_widths = c(6/20, 6/20, 6/20))
    save_plot(filename = paste(var_name, "AllPlots.jpeg", sep = ""), AllPlots, base_aspect_ratio = 1.3)
    #save_plot(filename = paste(var_name, "AllPlots.jpeg", sep = ""), AllPlots, base_width = 7.48)
    
    # create data dir and write table to csv file
    setwd(wd)
    setwd(dir)
    dir.create("Data")
    setwd("Data")
    write.csv(M, file = paste(var_name, "_", "ANOVA.csv", sep=" "))
    write.csv(contrast, file = paste(var_name, "_", "contrast.csv", sep=" "))
    write.csv(Levene, file = paste(var_name, "_", "Levene's Test.csv", sep = ""))
    write.csv(Shapiro, file = paste(var_name, "_", "Normality Test.csv", sep = ""))
    setwd(wd)
    
    # return table in console
    output <- list("Anova Info" = df, "Anova Table" = M, "Contrast Analysis" = contrast, "Normality Test" = Shapiro, "Homoscedasticity Test" = Levene, "Plot" = AllPlots)
    return(output)
}

varAnova(data = data, id = "Animal", var_name = "PercCorrect", group = "Group", adjust = "tukey", SS = 3)
