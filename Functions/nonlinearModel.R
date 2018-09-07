# version 2
# todo
# test assumptions / correct for deviations
# adapt graphical parameters in flexible way (eg xlim)
# optimize size of graphs
#
# Parameters
# graph: output type passed to ggplot device, default: jpeg
# adjust: correction method for post hoc comparisons passed to emmeans, default: tukey
#
# Note for graphical output parameters:
# height and width specify the final(!) output dimensions (in your paper). DPI specifies the quality. The higher, the more pixels in the final
# graph. cf. https://www.elsevier.com/authors/author-schemas/artwork-and-media-instructions/artwork-sizing
# graph: output format, jpeg (or jpg), tiff, eps, png, svg or pdf
#
# Examples
# nonlinearModel(dv="PercCorrect", session="Session", id="Animal", group="Group", data, lambda=10, graph="jpeg", dpi=600, adjust="bonferroni")
# nonlinearModel(dv="PercCorrect", session="Session", id="Animal", group="Group", data, lambda=10, graph="jpeg", dpi=700, layout_width = 19, layout_length = 22, adjust="bonferroni")

nonlinearModel <- function(dv, session, id, group, data, lambda = 10, adjust="tukey", 
                           graph="jpeg", dpi=600, layout_panel_size = 4.2, layout_width = 19,layout_length = NULL, units="cm"){
  # dependencies
  require(ggplot2) # graphs
  require(afex) # statistical analyses
  require(emmeans) # post hoc comparisons and contrast testing
  require(svglite) # used by ggplot to save in svg format
  require(gridExtra) # for layout
  require(egg) # to set panel size
  require(cowplot) # for the save_plot function
  require(xlsx) # output
  
  # create data frame
  data <- as.data.frame(data)
  
  # create readable variables from parameter input
  depvar <- eval(parse(text = paste("data$", dv, sep="")))
  ses <- eval(parse(text = paste("data$", session, sep="")))
  animal <- eval(parse(text = paste("data$", id, sep="")))
  
  # set e
  e <- exp(1)
  
  # changing group column name with parameter input so group information can be extracted
  colnames(data)[colnames(data) == group] <- "Group"
  # changing dep var column name with parameter input so init, max and goodness of fit can be calculated
  colnames(data)[colnames(data) == dv] <- "PercCorrect"
  # changing session column name with parameter input so m can be obtained 
  colnames(data)[colnames(data) == session] <- "Session"
  
  # create output variables
  subject <- vector(length = 0)
  lam <- vector(length = 0)
  gof <- vector(length = 0)
  gr <- vector(length = 0)
  init <- vector(length = 0)
  max <- vector(length = 0)
  
  # create list of animals
  list <- unique(animal)
  
  # creaste list to store predicted values
  preddatalist <- list()
  
  # create dir for all output
  wd <- getwd()
  dir <- paste("nonLinearModel_", format(Sys.time(), "%F_%H-%M-%S"), sep="")
  dir.create(dir)
  
  # create dir for graphic output
  setwd(dir)
  dir.create("Plots")
  setwd("Plots")

  # run analysis for every animal in list
  writeLines("Please wait...")
  for(i in list){
    # subset data
    dataID <- data[data[,id] == i,]
    # subject
    subject[i] <- (i) 
    # group information
    gr[i] <- as.character(dataID$Group[1])
    # initial value and max
    init[i] <- as.numeric(dataID[1,"PercCorrect"])
    max[i] <- max(dataID[,"PercCorrect"])
    # non-linear regression model
    m <-nls(PercCorrect ~ max[i] - (max[i]-init[i])*e^(-lambda*Session/100), data = dataID, start = list(lambda=lambda))
    # lambda
    lam[i] <- summary(m)$coefficients[1]
    # goodness of fit
    gof[i] <- cor(dataID$PercCorrect, predict(m))
    # save predicted values
    preddata <- data.frame(Predicted = predict(m))
    preddata$PercCorrect <- dataID$PercCorrect
    preddata$Session <- dataID$Session
    preddata$Animal <- i
    preddata$Group <- gr[i]
    preddatalist[[i]] <- preddata
    # save plots (one plot per animal)
    filename <- paste(i, ".", graph, sep="")
    p <- ggplot() + 
      geom_point(data = dataID, aes(Session, PercCorrect), size = 1, alpha = 0.5) +
      geom_line(data = preddata, aes(Session, Predicted), color = "red") + 
      geom_hline(yintercept = 50, size = 0.2, linetype = "dotted") +
      geom_hline(yintercept = 80, size = 0.2, linetype = "dashed") +
      annotate("text", x = 20, y = 10, hjust = 1, vjust = 0, label = paste("Animal: ", subject[i], "\nLambda: ", round(lam[i],2), "\nGoodness of fit: ", round(gof[i],2), sep="")) +
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(filename = filename, width = 9, height = 9, plot = p, units=units)
  }
  
  # create data frame with predicted values
  pd <- do.call(rbind,preddatalist)
  rownames(pd) <- c()
  
  # layout graph
  grouplist <- unique(pd$Group)
  if (any(is.na(unique(pd$Group)))==T) {
    datasub <- pd
    filename2 <- paste("layout.", graph, sep="")
    if (is.null(layout_length)) {
      (layout_length_temp <- (ceiling(length(unique(datasub$Animal)) / 4)) * 5.8)
    } else {layout_length_temp <- layout_length}
    gr_layout <-  ggplot(pd, aes(Session, PercCorrect), color = "red", linetype = "solid") + 
      geom_point(size = 1, alpha = .5) + 
      geom_line(aes(Session, Predicted), color = "red", linetype = "solid") +
      geom_hline(yintercept = 50, size = 0.2, linetype = "dotted") +
      geom_hline(yintercept = 80, size = 0.2, linetype = "dashed") +
      facet_wrap(~ Animal, ncol=4) + 
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    p1 <- grid.arrange(grobs = lapply(
      list(gr_layout),
      set_panel_size,
      width = unit(layout_panel_size, "cm"),
      height = unit(layout_panel_size, "cm")
    ))
    save_plot(filename2, p1,  base_width = layout_width, base_height = layout_length_temp, base_aspect_ratio=1, units=units, dpi = dpi, device = graph)
  } else {
    for(i in grouplist) {
      datasub <- pd[pd[, "Group"] == i,]
      filename3 <- paste(i, ".", graph, sep="")
      if (is.null(layout_length)) {
        (layout_length_temp <- (ceiling(length(unique(datasub$Animal)) / 4)) * 5.8)
      } else {layout_length_temp <- layout_length}
      gr_layout <-  ggplot(datasub, aes(Session, PercCorrect)) + 
        geom_point(size = 1, alpha = .5) + 
        geom_line(aes(Session, Predicted), color = "red", linetype = "solid") +
        geom_hline(yintercept = 50, size = 0.2, linetype = "dotted") +
        geom_hline(yintercept = 80, size = 0.2, linetype = "dashed") +
        facet_wrap(~ Animal, ncol=4) + 
        theme_bw() + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      p1 <- grid.arrange(grobs = lapply(
        list(gr_layout),
        set_panel_size,
        width = unit(layout_panel_size, "cm"),
        height = unit(layout_panel_size, "cm")
      ))
      save_plot(filename3, p1,  base_width = layout_width, base_height = layout_length_temp, base_aspect_ratio=1, units=units, dpi = dpi, device = graph)
    }
  }
  
  # summary graph
  filename4 <- paste("summary.", graph, sep="")
  if (any(is.na(unique(pd$Group)))==T) {
    sum <- aggregate(pd$Predicted, list(pd$Session), mean)
    colnames(sum) <- c("Session", "Predicted")
    gr_sum <- ggplot(data=sum, aes(x=Session, y=Predicted)) + 
      geom_line() + 
      geom_point() + 
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(filename = filename4, plot = gr_sum)
  } else {
    sum <- aggregate(pd$Predicted, list(pd$Session, pd$Group), mean)
    colnames(sum) <- c("Session", "Group", "Predicted")
    gr_sum <- ggplot(data=sum, aes(x=Session, y=Predicted, group=Group)) + 
      geom_line() + 
      geom_point() + 
      theme_bw() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(filename = filename4, plot = gr_sum)
  }
  
  # output
  output <- data.frame(subject, gr, lam, gof, init, max)
  rownames(output) <- c()
  colnames(output) <- c("Animal", "Group", "Lambda", "Goodness of fit", "Initial value", "Maximum value")
  output_sum <- output
  if (any(is.na(unique(output_sum$Group)))==T) output_sum$Group <- "No_group_information"
  tempdf <- aggregate(Animal ~ Group, data = output, length) # determine N per group
  output_sum$Animal <- NULL
  output_sum <- aggregate(.~Group, output_sum, FUN = function(x) mean(as.numeric(as.character(x))))
  output_sum$N <- tempdf$Animal
  output_sum <- output_sum[,c(1,6,2:5)]
  
  # write output and predicted values to file in new dir
  setwd(wd)
  setwd(dir)
  dir.create("Data")
  setwd("Data")
  write.csv(output, file = "nonlinearModel.csv")
  write.csv(pd, file = "nonlinearModel_predicted.csv")
  write.xlsx(output, "nonlinearModel.xlsx", col.names = TRUE, row.names = FALSE, append = FALSE)
  write.xlsx(pd, "nonlinearModel_predicted.xlsx", col.names = TRUE, row.names = FALSE, append = FALSE)
  setwd(wd)
  
  # statistical analysis
  if (any(is.na(unique(output$Group)))==TRUE || length(unique(output$Group)) < 2) {
    m <- c("No group information available")
    posthoc <- c("No group information available")
  } else {
    m <- aov_ez(id = "Animal", dv = "Lambda", data = output, between = "Group")
    posthoc <- emmeans(m, pairwise ~ Group, , adjust = adjust)
  }
  
  # return table in console
  outputlist <- list("Nonlinear Least Squares model" = output_sum, "Anova"= m, "Post hoc comparisons" = posthoc)
  return(outputlist)
}
