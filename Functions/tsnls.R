# version 2
# todo
# test includeGroups
# create summary graph
# create summary for output
#
# Parameters
# graph: output type, default: jpeg, options: tiff, svg, png, jpeg, eps, pdf
#
# Examples
# tsnls(dv="PercCorrect", session="Session", id="Animal", group="Group", includeGroups="all", data, lambda=10, graph="jpeg", res=600)

tsnls <- function(dv, session, id, group, includeGroups="all", data, lambda = 10, graph="jpeg", res=600){
  # create data frame
  data <- as.data.frame(data)
  
  # create readable variables from parameter input
  depvar <- eval(parse(text = paste("data$", dv, sep="")))
  ses <- eval(parse(text = paste("data$", session, sep="")))
  animal <- eval(parse(text = paste("data$", id, sep="")))
  
  # set e
  e <- exp(1)
  
  # select groups based on parameter
  if (includeGroups != "all") {
    data <- data[data[, group] == includeGroups,]
  }
  
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
  
  # for loop to save layout
  # create image
  if (graph == "tiff") {
    tiff("layout.tiff", width = 7.5, height = ceiling(length(list)/4)*2, unit = "in", res=res)
  }
  if (graph == "svg") {
    svg("layout.svg", width = 7.5, height = ceiling(length(list)/4)*2)
  }
  if (graph == "png") {
    png("layout.png", width = 7.5, height = ceiling(length(list)/4)*2, unit = "in", res=res)
  }
  if (graph == "jpeg" | graph == "jpg") {
    jpeg("layout.jpeg", width = 7.5, height = ceiling(length(list)/4)*2, unit = "in", res=res)
  }
  if (graph == "pdf") {
    pdf("layout.pdf")
  }
  if (graph == "eps") {
    setEPS()
    postscript("layout.eps")
  }
  #p <- par(mfrow=c(length(list)/4+1,4), pin=c(1.5,1.5), oma=c(1.3,1.4,1.3,1.4))
  p <- par(mfrow=c(3,4))
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
  }
  # create layout
  par(p)
  dev.off()
  
  # create data frame with predicted values
  pd <- do.call(rbind,preddatalist)
  rownames(pd) <- c()
  
  # output
  output <- data.frame(subject, gr, lam, gof, init, max)
  rownames(output) <- c()
  colnames(output) <- c("Animal", "Group", "Lambda", "Goodness of fit", "Initial value", "Maximum value")
  
  # write output and predicted values to file
  setwd(wd)
  write.csv(output, file = "tsnls.csv")
  write.csv(pd, file = "predicted.csv")
  require(xlsx) # depends on java!
  write.xlsx(output, "tsnls.xlsx", col.names = TRUE, row.names = FALSE, append = FALSE)
  write.xlsx(pd, "predicted.xlsx", col.names = TRUE, row.names = FALSE, append = FALSE)
  
  # return table in console
  return(output)
}
