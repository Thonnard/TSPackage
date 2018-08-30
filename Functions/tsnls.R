# version 2
# todo
# test includeGroups
# create summary graph
# create summary for output
# adapt graphical parameters in flexible way (eg xlim)
# add n per group in summary
#
# Parameters
# graph: output type, default: jpeg, options: tiff, svg, png, jpeg, eps, pdf
#
# Examples
# tsnls(dv="PercCorrect", session="Session", id="Animal", group="Group", data, lambda=10, graph="jpeg", res=600)

tsnls <- function(dv, session, id, group, data, lambda = 10, graph="jepg", res=600){
  # dependencies
  require(ggplot2)
  require(afex)
  require(emmeans)
  require(xlsx) # depends on java!
  
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
  
  # create dir for graphic output
  dir.create("Plots")
  wd <- getwd()
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
    if (graph == "tiff") {
      tiff(filename, width = 7.5, height = ceiling(length(list)/4)*2, unit = "in", res=res)
    }
    if (graph == "svg") {
      svg(filename, width = 7.5, height = ceiling(length(list)/4)*2)
    }
    if (graph == "png") {
      png(filename, width = 7.5, height = ceiling(length(list)/4)*2, unit = "in", res=res)
    }
    if (graph == "jpeg" | graph == "jpg") {
      jpeg(filename, width = 7.5, height = ceiling(length(list)/4)*2, unit = "in", res=res)
    }
    if (graph == "pdf") {
      pdf(filename)
    }
    if (graph == "eps") {
      setEPS()
      postscript(filename)
    }
    # plot
    plot <- 
    plot(dataID$Session,dataID$PercCorrect, pch=16, xlab="", ylab="", xlim=c(0,20), ylim=c(0,100))
    lines(dataID$Session,predict(m),lty=1,col="red",lwd=2)
    lab = paste("Lambda: ", round(lam[i],2))
    lab2 = paste("ID: ", i)
    lab3 = paste("Goodness of fit: ", round(gof[i],2))
    text(x=21,y=15, lab, pos=2)
    text(x=21,y=25, lab2, pos=2)
    text(x=21,y=5, lab3, pos=2)
    abline(h=50, lty=2)
    abline(h=80, lty=2)
    # end plot
    dev.off()
  }
  
  # create data frame with predicted values
  pd <- do.call(rbind,preddatalist)
  rownames(pd) <- c()
  
  # vector for N per group
  N <- vector(length = 0)
  
  # layout graph
  grouplist <- unique(pd$Group)
  if (any(is.na(unique(pd$Group)))==T) {
    filename2 <- paste("layout.", graph, sep="")
    gr_layout <- ggplot(pd, aes(Session, PercCorrect)) + geom_point() + geom_line(aes(Session, Predicted)) + facet_wrap(~ Animal, ncol=4) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(filename = filename2, plot = gr_layout)
  } else {
    for(i in grouplist) {
      datasub <- pd[pd[, "Group"] == i,]
      N[i] <- length(unique(datasub$Animal))
      filename3 <- paste(i, ".", graph, sep="")
      gr_layout <- ggplot(datasub, aes(Session, PercCorrect)) + geom_point() + geom_line(aes(Session, Predicted)) + facet_wrap(~ Animal, ncol=4) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      ggsave(filename = filename3, plot = gr_layout)
    }
  }
  
  # summary graph
  filename4 <- paste("summary.", graph, sep="")
  if (any(is.na(unique(pd$Group)))==T) {
    sum <- aggregate(pd$Predicted, list(pd$Session), mean)
    colnames(sum) <- c("Session", "Predicted")
    gr_sum <- ggplot(data=sum, aes(x=Session, y=Predicted)) + geom_line() + geom_point() + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(filename = filename4, plot = gr_sum)
  } else {
    sum <- aggregate(pd$Predicted, list(pd$Session, pd$Group), mean)
    colnames(sum) <- c("Session", "Group", "Predicted")
    gr_sum <- ggplot(data=sum, aes(x=Session, y=Predicted, group=Group)) + geom_line() + geom_point() + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(filename = filename4, plot = gr_sum)
  }

  # output
  output <- data.frame(subject, gr, lam, gof, init, max)
  rownames(output) <- c()
  colnames(output) <- c("Animal", "Group", "Lambda", "Goodness of fit", "Initial value", "Maximum value")
  output_sum <- output
  if (any(is.na(unique(output_sum$Group)))==T) output_sum$Group <- "No_group_information"
  output_sum$Animal <- NULL
  output_sum <- aggregate(.~Group, output_sum, FUN = function(x) mean(as.numeric(as.character(x))))
  output_sum$N <- N
  output_sum <- output_sum[,c(1,6,2:5)]
  
  # write output and predicted values to file
  setwd(wd)
  write.csv(output, file = "tsnls.csv")
  write.csv(pd, file = "tsnls_predicted.csv")
  write.xlsx(output, "tsnls.xlsx", col.names = TRUE, row.names = FALSE, append = FALSE)
  write.xlsx(pd, "tsnls_predicted.xlsx", col.names = TRUE, row.names = FALSE, append = FALSE)
  
  # statistical analysis
  if (any(is.na(unique(output$Group)))==T || length(unique(output$Group)) < 2) {
    m <- c("No group information available")
    contrasts <- c("No group information available")
  } else {
    m <- aov_ez(id = "Animal", dv = "Lambda", data = output, between = "Group")
    contrasts <- emmeans(m, pairwise ~ Group)
  }
  
  # return table in console
  outputlist <- list("Nonlinear Least Squares model" = output_sum, "Anova"= m, "Post hoc comparisons" = contrasts)
  return(outputlist)
}
