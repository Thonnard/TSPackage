# version 2
# todo
# change formula paramater to actual formula instead of "FOO"
# write help to insert formula that includes changing values (value[i])
# test includeGroups
#
# Examples
# tsnls(dv="PercCorrect", session="Session", id="Animal", group="Group", includeGroups="all", formula="FOO", data, start = list(lambda=10), graph="tiff", res=600)

tsnls <- function(dv, session, id, group, includeGroups="all", formula="FOO", data, start = list(lambda = 10), graph="tiff", res=600){
  # create data frame
  data <- as.data.frame(data)
  
  # create readable variables from parameter input
  depvar <- eval(parse(text = paste("data$", dv, sep="")))
  ses <- eval(parse(text = paste("data$", session, sep="")))
  animal <- eval(parse(text = paste("data$", id, sep="")))
  form <- eval(parse(text = paste("data$", formula, sep="")))

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
    m <-nls(PercCorrect ~ max[i] - (max[i]-init[i])*e^(-lambda*Session/100), data = dataID, start = start)
    # lambda
    lam[i] <- summary(m)$coefficients[1]
    # goodness of fit
    gof[i] <- cor(dataID$PercCorrect, predict(m))
    }
  
  # output
  output <- data.frame(subject, gr, lam, gof, init, max)
  rownames(output) <- c()
  colnames(output) <- c("Animal", "Group", "Lambda", "Goodness of fit", "Initial value", "Maximum value")
  
  # write output to file
  write.csv(output, file = "tsnls.csv")
  require(xlsx) # depends on java!
  write.xlsx(output, "tsnls.xlsx", col.names = TRUE, row.names = FALSE, append = FALSE)
  
  # return table in console
  return(output)
}
