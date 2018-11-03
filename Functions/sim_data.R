# Description TEST
# sim_data() simulates score data for two groups as obtained in a rodent touchscreen visual discrimination paradigm.
# data is simulated according to the nonlinear model described by Piiponniemi et al. (DOI: 10.1016/J.NEULET.2017.04.049), which includes the estimation of general learning parameter 'lambda'
# nonlinear model: score <- max - (max - ini)*e^(-lambda*session/100)
#
# Output
# Data table with parameter info (lambda, max, ini)
# Data table with scores and values predicted by model
# Graph with scores (points) created by adding variation to the fitted model, and predicted values from the original model (lines)
#
# Parameters
# n1/n2: subjects per group (bigger groups provide more (biologically) valid data (due to lower within group variation))
# m1/m2: means for lambda
# sd1/sd2: standard deviations for lambda
# sessions: number of learning sessions
#
# Optional parameters (for obtaining maximum and initial scores for every subject)
# range_max_g1/2: range of maximum value
# max_g1/2: mean maximum value
# max_sd_g1/2: standard deviation of maximum value
# max_g1/2: mean initial value
# max_sd_g1/2: standard deviation of initial value
# range_max_g1/2: range of initial value
# These parameters are passed to the rtruncnorm (truncnorm package) function. Typical default values are provided.
#
# Parameter for random variation in data
# randvar: default 5, value passed to runif(1, -randvar,randvar). The smaller this value, the closer the ouput data is to the values predicted by the model.
#
# Parameter for reproducibility
# set_seed: value passed to set.seed()
#
# Examples
# sim_data(n1=20, m1=12, sd1=0.5, n2=15, m2=15, sd2=0.6, sessions=20)
# sim_data(n1=20, m1=15, sd1=0.5, n2=15, m2=32, sd2=0.6, sessions=20, randvar=15)
# sim_data(n1=3, m1=12, sd1=0.5, n2=4, m2=15, sd2=0.6, sessions=20, range_max_g1=c(50,80), max_g1=65, max_sd_g1=10, range_ini_g1=c(0,50), ini_g1=25, ini_sd_g1=10, range_max_g2=c(80,100), max_g2=90, max_sd_g2=10, range_ini_g2=c(0,50), ini_g2=25, ini_sd_g2=10, set_seed=1234)

sim_data <- function(n1, m1, sd1, n2, m2, sd2, sessions, randvar=5,
                     range_max_g1=c(80,100), max_g1=90,max_sd_g1=10,
                     range_ini_g1=c(0,50), ini_g1=25, ini_sd_g1=10,
                     range_max_g2=c(80,100), max_g2=90, max_sd_g2=10,
                     range_ini_g2=c(0,50),  ini_g2=25, ini_sd_g2=10, set_seed=1234){
  # libraries
  require(truncnorm)
  require(reshape2)
  require(xlsx)
  require(ggplot2)

  # reproducibility
  set.seed(set_seed)

  # e
  e <- exp(1)

  # create dir for all output
  wd <- getwd()
  dir <- paste("Simulated_data_", format(Sys.time(), "%F_%H-%M-%S"), sep="")
  dir.create(dir)

  # create vector of lambda's with mean m and standard deviation sd for each group, each lambda corresponds with 1 subject
  l_g1 <- rnorm(n1,m1,sd1)
  l_g2 <- rnorm(n2,m2,sd2)

  # max and ini values
  max_g1 <- rtruncnorm(n=n1, a=range_max_g1[1], b=range_max_g1[2], mean=max_g1, sd=max_sd_g1)
  max_g2 <- rtruncnorm(n=n2, a=range_max_g2[1], b=range_max_g2[2], mean=max_g2, sd=max_sd_g2)
  ini_g1 <- rtruncnorm(n=n1, a=range_ini_g1[1], b=range_ini_g1[2], mean=ini_g1, sd=ini_sd_g1)
  ini_g2 <- rtruncnorm(n=n2, a=range_ini_g2[1], b=range_ini_g2[2], mean=ini_g2, sd=ini_sd_g2)

  # parameters table
  parameter_temp1 <- data.frame(l_g1, max_g1, ini_g1)
  parameter_temp1$Group <- "g1"
  parameter_temp1$Animal <- paste("g1_",rep(1:n1,1),sep="")
  colnames(parameter_temp1) <- c("Lambda", "Maximum_value", "Initial_value", "Group", "Animal")
  parameter_temp2 <- data.frame(l_g2, max_g2, ini_g2)
  parameter_temp2$Group <- "g2"
  parameter_temp2$Animal <- paste("g2_",rep(1:n2,1),sep="")
  colnames(parameter_temp2) <- c("Lambda", "Maximum_value", "Initial_value", "Group", "Animal")
  parameter <- rbind(parameter_temp1,parameter_temp2)

  # create matrices
  outcome_g1 <- matrix(0, sessions,n1)
  outcome_g2 <- matrix(0, sessions,n2)

  # calculate outcome based on Piiponniemi's nonlinear model
  for (i in 1:n1) {
    for (j in 1:sessions) {
      outcome_g1[j,i] <- max_g1[i] - (max_g1[i] - ini_g1[i])*e^(-l_g1[i]*j/100)
    }
  }

  for (i in 1:n2) {
    for (j in 1:sessions) {
      outcome_g2[j,i] <- max_g2[i] - (max_g2[i] - ini_g2[i])*e^(-l_g2[i]*j/100)
    }
  }

  # create data frame
  header_g1 <- vector(length=0)
  header_g2 <- vector(length=0)
  for (k in 1:n1) {
    header_g1[k] <- paste("g1_n",k,sep="")
  }
  for (l in 1:n2) {
    header_g2[l] <- paste("g2_n",l,sep="")
  }

  outcome_g1 <- as.data.frame(outcome_g1)
  colnames(outcome_g1) <- header_g1
  outcome_g1$Session <- rep(1:sessions,1)

  outcome_g2 <- as.data.frame(outcome_g2)
  colnames(outcome_g2) <- header_g2
  outcome_g2$Session <- rep(1:sessions,1)

  # wide to long
  outcome_g1_long <- melt(outcome_g1, id.vars="Session")
  outcome_g1_long$Group <- "g1"
  colnames(outcome_g1_long) <- c("Session", "Animal", "Score", "Group")

  outcome_g2_long <- melt(outcome_g2, id.vars="Session")
  outcome_g2_long$Group <- "g2"
  colnames(outcome_g2_long) <- c("Session", "Animal", "Score", "Group")

  data <- rbind(outcome_g1_long, outcome_g2_long)

  # saving model predictions
  data$Predicted_by_Model <- data$Score

  # create data by adding random variation
  data$Score_2 <- 0
  for (m in 1:length(data$Score)){
    data$Score_2[m] <- data$Score[m] + runif(1, -(randvar), randvar)
    if (data$Score_2[m] > 100 || data$Score_2[m] < 0)
      data$Score_2[m] <- data$Score[m]
  }

  data$Score <- data$Score_2
  data$Score_2 <- NULL

  # create and set dir for graphic output
  setwd(dir)
  dir.create("Plots")
  setwd("Plots")

  # create/save graph
  sum_score <- aggregate(Score~Group*Session, data, FUN = mean)
  sum_pred <- aggregate(Predicted_by_Model~Group*Session, data, FUN = mean)
  gr_sim <- ggplot() +
    geom_hline(yintercept=50, linetype="dashed") +
    geom_hline(yintercept=80, linetype="dashed") +
    geom_point(data=sum_score, aes(x = Session, y = Score, shape = Group)) +
    scale_shape_manual(values=c(0, 19))+
    geom_line(data=sum_pred, aes(x = Session, y = Predicted_by_Model, linetype=Group)) +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  ggsave(filename = "sim_data.jpeg", plot = gr_sim)

  # create dir and save output and parameters
  setwd(wd)
  setwd(dir)
  dir.create("Data")
  setwd("Data")
  filename <- paste("simulated_data_", Sys.Date(), ".csv", sep="")
  filename2 <- paste("parameters_simulated_data_", Sys.Date(), ".csv", sep="")
  filename3 <- paste("simulated_data_", Sys.Date(), ".xlsx", sep="")
  filename4 <- paste("parameters_simulated_data_", Sys.Date(), ".xlsx", sep="")
  write.csv(data, file = filename)
  write.csv(parameter, file = filename2)
  write.xlsx(data, filename3, col.names = TRUE, row.names = FALSE, append = FALSE)
  write.xlsx(parameter, filename4, col.names = TRUE, row.names = FALSE, append = FALSE)
  setwd(wd)

  # return data but don't show it
  invisible(data)
}
