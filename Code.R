
# Student No. 2005070
# Dissertation Research Paper Code

# Contents:
# Scenario 1    line 21
# Scenario 2    line 407
# Scenario 3    line 605
# Scenario 4    line 808
# Scenario 5    line 975

################################################################################

# Loading useful libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)

set.seed(27)

################################################################################
# Scenario 1
# Illustrating a mediator (linear regression)

# Creating the mediator simulation function

sim_Med <- function(betaXMed, betaMedY, betaXY){
  n <- 1000
  errorMed <- 1     # error / random noise scaling
  errorX   <- 1
  errorY   <- 1
  X   <- errorX*rnorm(n)                             # generate X from standard normal distribution
  Med <- betaXMed*X              + errorMed*rnorm(n) # generate Mediator from X
  Y   <- betaXY*X + betaMedY*Med + errorY*rnorm(n)   # generate Y from X and Mediator
  lm1 <- lm(Y~X)          # simple regression
  lm2 <- lm(Y~X+Med)      # multiple regression
  coefXY    <- lm1$coefficients[2]  # save the estimate effect of X on Y from the simple regression
  coefXMedY <- lm2$coefficients[2]  # save the estimate effect of X on Y from the multiple regression
  coefXY.up    <- confint(lm1)[2,2] # save confidence intervals for both regressions
  coefXY.lo    <- confint(lm1)[2,1]
  coefXMedY.up <- confint(lm2)[2,2]
  coefXMedY.lo <- confint(lm2)[2,1]
  return(data.frame(coefXY, coefXMedY,            # return the two coefficients
                    coefXY.lo, coefXY.up,         # and the confidence intervals
                    coefXMedY.lo, coefXMedY.up)) 
}

# Set the parameters we will be using
betaMedY <- 0.5   # the mediator has quite a strong (0.5 coefficient) effect on Y
betaXY   <- 0.0   # this is the true effect of X on Y, here set as zero

# Test the function
sim_Med(1, betaMedY, betaXY)

# Run a loop of betaXMed values from -1.0 to +1.0 
betaXMed <- seq(-1.0, 1.0, by = 0.1)

resultsMed <- lapply(betaXMed, sim_Med, betaMedY, betaXY)

Med_df <- bind_rows(resultsMed)
# head(Med_df) 

# Now we can plot the estimated coefficient from the two regression models to show 
# the bias caused by different levels of mediator bias.
# The plot includes horizontal line which should be set at the 'true' value of the coefficient.

# Graph for simple regression, showing mediator bias
plot1 <- ggplot(Med_df, aes(x=betaXMed, y=coefXY)) +
  geom_point(shape=19, size = 1.7)+
  ylim(-0.75, 0.75)+
  geom_hline(yintercept=0.0, linetype='dashed', color='blue') +
  geom_errorbar(aes(ymin=coefXY.lo, ymax=coefXY.up), width=.02, colour = "black") +
  theme_minimal() + 
  xlab("X ~ Mediator Coefficient") + ylab("X ~ Y Coefficient")

# Graph for multiple regression, accounting for mediator
plot2 <- ggplot(Med_df, aes(x=betaXMed, y=coefXMedY)) +
  geom_point(shape=19, size = 1.7)+
  ylim(-0.75, 0.75)+
  geom_hline(yintercept=0.0, linetype='dashed', color='blue') +
  geom_errorbar(aes(ymin=coefXMedY.lo, ymax=coefXMedY.up), width=.02, colour = "black") +
  theme_minimal() + 
  xlab("X ~ Mediator Coefficient") + ylab("X ~ Y Coefficient incl. Mediator")


################################################################################
# Scenario 1
# Illustrating a mediator (logistic regression)

sim_Med_log <- function(bMX){
  n <- 10000                # the number of observations in the study
  X <- rbinom(n, 1, 0.5)   # categorical factor X
  #bMX <- log(bMX)
  intX <- 0     # intercept for the effect of mediator on X
  intY <- 0     # intercept for the effect of mediator on Y
  bMY <- 2      # coefficient for the effect of mediator on Y
  bXY <- 0      # coefficient for the effect of X on Y
  
  pM <- 1 / (1 + exp(-intX-bMX*X))         # expected probabilities of mediator
  M2 <- rbinom(n, 1, pM)                   # observed values of mediator
  pY <- 1 / (1 + exp(-intY-bXY*X-bMY*M2))  # expected probabilities of Y
  Y2 <- rbinom(n, 1, pY)                   # observed values of Y
  
  dat <- data.frame(X, M2, Y2)
  
  # Now run the logistic regressions with the 'observed' values of X, Y and Con
  glm1 <- glm(Y2 ~ X, family = binomial)     
  glm2 <- glm(Y2 ~ X + M2, family = binomial) 
  
  coefXY    <- exp(glm1$coefficients[2])  # save the estimate effect of X on Y from the simple regression
  coefXMedY <- exp(glm2$coefficients[2])  # save the estimate effect of X on Y from the multiple regression
  coefXY.up <- exp(confint(glm1)[2,2])    # save confidence intervals
  coefXY.lo <- exp(confint(glm1)[2,1])
  coefXMedY.up <- exp(confint(glm2)[2,2])
  coefXMedY.lo <- exp(confint(glm2)[2,1])
  return(data.frame(coefXY, coefXMedY, coefXY.lo, coefXY.up, coefXMedY.lo, coefXMedY.up))
}

sim_Med_log(1)
bMX <- seq(-1, 1, by = 0.1)

resultslogMed <- lapply(bMX, sim_Med_log)

Med_logdf <- bind_rows(resultslogMed)
head(Med_logdf) 

# Graph showing coefficients biased by mediator
plot3 <- ggplot(Med_logdf, aes(x=bMX, y=coefXY)) +
  geom_point(shape=19, size = 1.7)+
  geom_errorbar(aes(ymin=coefXY.lo, ymax=coefXY.up), width=.02, colour = "black") +
  ylim(0, 2)+
  geom_hline(yintercept=1, linetype='dashed', color='blue') +
  ylab("X~Y Odds Ratio") + xlab("X ~ Mediator Coefficient") +
  theme_minimal()

# Graph showing coefficients unbiased by mediator
plot4 <- ggplot(Med_logdf, aes(x=bMX, y=coefXMedY)) +
  geom_point(shape=19, size = 1.7)+
  geom_errorbar(aes(ymin=coefXMedY.lo, ymax=coefXMedY.up), width=.02, colour = "black") +
  ylim(0, 2)+
  geom_hline(yintercept=1, linetype='dashed', color='blue') +
  ylab("X~Y Odds Ratio incl. Mediator") + xlab("X ~ Mediator Coefficient") +
  theme_minimal()

# View the graphs
ggarrange(plot1, plot2, plot3, plot4, labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


################################################################################
# Scenario 1
# Illustrating a confounder (linear regression)

# Creating the confounder simulation function
sim_Con <- function(betaConX, betaConY, betaXY){
  n <- 1000
  errorCon <- 1    # error / random noise scaling
  errorX   <- 1
  errorY   <- 1
  Con <- errorCon*rnorm(n)                             # generate Confounder
  X   <- betaConX * Con              + errorX*rnorm(n) # generate X from Confounder
  Y   <- betaConY * Con + betaXY * X + errorY*rnorm(n) # generate Y from Confounder and (potentially) X
  lm1 <- lm(Y~X)         # simple regression
  lm2 <- lm(Y~X+Con)     # multiple regression
  coefXY    <- lm1$coefficients[2]  # save the estimate effect of X on Y from the simple regression
  coefXYCon <- lm2$coefficients[2]  # save the estimate effect of X on Y from the multiple regression
  coefXY.up    <- confint(lm1)[2,2] # save confidence intervals for both regressions
  coefXY.lo    <- confint(lm1)[2,1]
  coefXYCon.up <- confint(lm2)[2,2]
  coefXYCon.lo <- confint(lm2)[2,1]
  return(data.frame(coefXY, coefXYCon,            # return the two coefficients
                    coefXY.lo, coefXY.up,         # and the confidence intervals
                    coefXYCon.lo, coefXYCon.up)) 
}

# Set the parameters we will be using
betaConY <- 0.5  # the confounder has quite a strong (0.5 coefficient) effect on Y
betaXY   <- 0.0  # this is the true effect of X on Y, here set as zero

# Test the function
sim_Con(1, betaConY, betaXY) 

# Running the function in a loop using different betaConX values between -1.0 (strong confounded 
# negative correlation) and +1.0 (strong confounded positive correlation). 0 means no confounding.
betaConX <- seq(-1.0, 1.0, by = 0.1)

resultsCon <- lapply(betaConX, sim_Con, betaConY, betaXY)

Con_df <- bind_rows(resultsCon)
# head(Con_df) # examine the output

# Now we can plot the estimated coefficient from the two regression models to show 
# the bias caused by different levels of confounding.
# The plot includes horizontal line which should be set at the 'true' value of the coefficient.

# Graph for simple regression, showing confounding bias
plot5 <- ggplot(Con_df, aes(x=betaConX, y=coefXY)) +
  geom_point(shape=19, size = 1.7)+
  ylim(-0.5, 0.5)+
  geom_hline(yintercept=0.0, linetype='dashed', color='blue') +
  geom_errorbar(aes(ymin=coefXY.lo, ymax=coefXY.up), width=.02, colour = "black") +
  theme_minimal() + 
  xlab("Confounder ~ X Coefficient") + ylab("X ~ Y Coefficient")

# Graph for multiple regression, accounting for confounder
plot6 <- ggplot(Con_df, aes(x=betaConX, y=coefXYCon)) +
  geom_point(shape=19, size = 1.7)+
  ylim(-0.5, 0.5)+
  geom_hline(yintercept=0.0, linetype='dashed', color='blue') +
  geom_errorbar(aes(ymin=coefXYCon.lo, ymax=coefXYCon.up), width=.02, colour = "black") +
  theme_minimal() + 
  xlab("Confounder ~ X Coefficient") + ylab("X ~ Y Coefficient incl. Confounder")


################################################################################
# Scenario 1
# Illustrating a confounder (logistic regression)

sim_Con_log <- function(bCX){
  n <- 10000      # the number of observations in the study
  #bCX <- log(bCX)
  C <- rbinom(n, 1, 0.5)      # simulate n values of C from the binomial distribution
  
  intX <- 0     # the intercept for the logistic regressions for the effect of the confounder on X
  intY <- 0     # the intercept for the logistic regressions for the effect of the confounder on Y
  bCY <- 2      # the coefficient for the effect of the confounder on Y (the log of the odds ratio)
  
  pX <- 1 / (1 + exp(-intX-bCX*C))   # EXPECTED probabilities of X given the value of the confounder
  pY <- 1 / (1 + exp(-intY-bCY*C))   # EXPECTED probabilities of Y given the value of the confounder
  
  X2 <- rbinom(n, 1, pX)  # Use the Expected probabilities for X to generate a value either 1 or 0 for the OBSERVED value of X
  Y2 <- rbinom(n, 1, pY)  # Use the Expected probabilities for Y to generate a value either 1 or 0 for the OBSERVED value of Y
  
  dat <- data.frame(C, X2, Y2)
  
  # Now run the logistic regressions with the 'observed' values of X, Y and Con
  glm1 <- glm(Y2 ~ X2, family = binomial)     
  glm2 <- glm(Y2 ~ X2 + C, family = binomial) 
  coefXY    <- exp(glm1$coefficients[2])  # save the estimate effect of X on Y from the simple regression
  coefXYCon <- exp(glm2$coefficients[2])  # save the estimate effect of X on Y from the multiple regression
  coefXY.up <- exp(confint(glm1)[2,2])    # save confidence intervals
  coefXY.lo <- exp(confint(glm1)[2,1])
  coefXYCon.up <- exp(confint(glm2)[2,2])
  coefXYCon.lo <- exp(confint(glm2)[2,1])
  return(data.frame(coefXY, coefXYCon, coefXY.lo, coefXY.up, coefXYCon.lo, coefXYCon.up))
}

sim_Con_log(1)

bCX <- seq(-1.0, 1.0, by = 0.1)

resultslogCon <- lapply(bCX, sim_Con_log)

Con_logdf <- bind_rows(resultslogCon)
head(Con_logdf) 

# Graph showing coefficients biased by confounder
plot7 <- ggplot(Con_logdf, aes(x=bCX, y=coefXY)) +
  geom_point(shape=19, size = 1.7)+
  geom_errorbar(aes(ymin=coefXY.lo, ymax=coefXY.up), width=.02, colour = "black") +
  ylim(0, 2)+
  geom_hline(yintercept=1, linetype='dashed', color='blue') +
  ylab("X~Y Odds Ratio") + xlab("Confounder ~ X Coefficient") +
  theme_minimal()

# Graph showing coefficients unbiased by confounder
plot8 <- ggplot(Con_logdf, aes(x=bCX, y=coefXYCon)) +
  geom_point(shape=19, size = 1.7)+
  geom_errorbar(aes(ymin=coefXYCon.lo, ymax=coefXYCon.up), width=.02, colour = "black") +
  ylim(0, 2)+
  geom_hline(yintercept=1, linetype='dashed', color='blue') +
  ylab("X~Y Odds Ratio incl. Confounder") + xlab("Confounder ~ X Coefficient") +
  theme_minimal()

# View the graphs
ggarrange(plot5, plot6, plot7, plot8, labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


################################################################################
# Scenario 1
# Illustrating a collider (linear regression)

# Creating the collider simulation function
sim_Col <- function(betaXCol, betaYCol, betaXY){
  n <- 1000
  errorCol <- 1    # error / random noise scaling
  errorX   <- 1
  errorY   <- 1
  X   <- errorX*rnorm(n)                             # generate X from standard normal distribution
  Y   <- betaXY*X                + errorY*rnorm(n)   # generate Y from standard normal distribution and X
  Col <- betaXCol*X + betaYCol*Y + errorCol*rnorm(n) # generate Collider from X and Y
  lm1 <- lm(Y~X)          # simple regression
  lm2 <- lm(Y~X+Col)      # multiple regression
  coefXY    <- lm1$coefficients[2]  # save the estimate effect of X on Y from the simple regression
  coefXYCol <- lm2$coefficients[2]  # save the estimate effect of X on Y from the multiple regression 
  coefXY.up    <- confint(lm1)[2,2] # save confidence intervals for both regressions
  coefXY.lo    <- confint(lm1)[2,1]
  coefXYCol.up <- confint(lm2)[2,2]
  coefXYCol.lo <- confint(lm2)[2,1]
  return(data.frame(coefXY, coefXYCol,            # return the two coefficients
                    coefXY.lo, coefXY.up,         # and the confidence intervals
                    coefXYCol.lo, coefXYCol.up)) 
}

# Set the parameters we will be using
betaYCol <- 0.5  # Y has quite a strong (0.5 coefficient) effect on the collider
betaXY   <- 0.0  # this is the true effect of X on Y, here set as zero

# Test the function
sim_Col(1, betaYCol, betaXY)

# Run a loop of betaXCol values from -1.0 to +1.0 
betaXCol <- seq(-1.0, 1.0, by = 0.1)

resultsCol <- lapply(betaXCol, sim_Col, betaYCol, betaXY)

Col_df <- bind_rows(resultsCol)
# head(Col_df) 

# Now we can plot the estimated coefficient from the two regression models to show 
# the bias caused by different levels of collider bias.
# The plot includes horizontal line which should be set at the 'true' value of the coefficient.

# Graph for multiple regression, showing collider bias
plot9 <- ggplot(Col_df, aes(x=betaXCol, y=coefXYCol)) +
  geom_point(shape=19, size = 1.7)+
  ylim(-0.6, 0.6)+
  geom_hline(yintercept=0.0, linetype='dashed', color='blue') +
  geom_errorbar(aes(ymin=coefXYCol.lo, ymax=coefXYCol.up), width=.02, colour = "black") +
  theme_minimal() + 
  xlab("X ~ Collider Coefficient") + ylab("X ~ Y Coefficient incl. Collider")

# Graph for simple regression, unbiased by the collider
plot10 <- ggplot(Col_df, aes(x=betaXCol, y=coefXY)) +
  geom_point(shape=19, size = 1.7)+
  ylim(-0.6, 0.6)+
  geom_hline(yintercept=0.0, linetype='dashed', color='blue') +
  geom_errorbar(aes(ymin=coefXY.lo, ymax=coefXY.up), width=.02, colour = "black") +
  theme_minimal() + 
  xlab("X ~ Collider Coefficient") + ylab("X ~ Y Coefficient")


################################################################################
# Scenario 1
# Illustrating a collider (logistic regression)

sim_Col_log <- function(bC2X){
  n <- 10000                 # the number of observations in the study
  X <- rbinom(n, 1, 0.5)    # categorical factor X
  #bC2X <- log(bC2X)
  intX <- 0     # intercept for the effect of collider on X
  intY <- 0     # intercept for the effect of collider on Y
  bC2Y <- 2     # coefficient for the effect of collider on Y
  bXY  <- 0     # coefficient for the effect of X on Y
  
  pY <- 1 / (1 + exp(-intY-bXY*X))           # expected probabilities of Y
  Y2 <- rbinom(n, 1, pY)                     # observed values of Y
  pC <- 1 / (1 + exp(-intX-bC2X*X-bC2Y*Y2))  # expected probabilities of collider
  C2 <- rbinom(n, 1, pC)                     # observed values of collider
  
  dat <- data.frame(X, C2, Y2)
  
  glm1 <- glm(Y2 ~ X, family = binomial)   
  glm2 <- glm(Y2 ~ X + C2, family = binomial)  
  
  coefXY    <- exp(glm1$coefficients[2])  # save the estimate effect of X on Y from the simple regression
  coefXYCol <- exp(glm2$coefficients[2])
  coefXY.up <- exp(confint(glm1)[2,2])    # save confidence intervals
  coefXY.lo <- exp(confint(glm1)[2,1])
  coefXYCol.up <- exp(confint(glm2)[2,2])
  coefXYCol.lo <- exp(confint(glm2)[2,1])
  return(data.frame(coefXY, coefXYCol, coefXY.lo, coefXY.up, coefXYCol.lo, coefXYCol.up))
}

sim_Col_log(1)
bC2X <- seq(-1, 1, by = 0.1)

resultslogCol <- lapply(bC2X, sim_Col_log)

Col_logdf <- bind_rows(resultslogCol)
head(Col_logdf) 

# Graph showing coefficients unbiased by confounder
plot11 <- ggplot(Col_logdf, aes(x=bC2X, y=coefXYCol)) +
  geom_point()+
  geom_errorbar(aes(ymin=coefXYCol.lo, ymax=coefXYCol.up), width=.02, colour = "black") +
  ylim(0, 2)+
  geom_hline(yintercept=1, linetype='dashed', color='blue') +
  ylab("X~Y Odds Ratio incl. Collider") + xlab("X ~ Collider Coefficient") +
  theme_minimal()

# Graph showing coefficients unbiased by collider
plot12 <- ggplot(Col_logdf, aes(x=bC2X, y=coefXY)) +
  geom_point()+
  geom_errorbar(aes(ymin=coefXY.lo, ymax=coefXY.up), width=.02, colour = "black") +
  ylim(0, 2)+
  geom_hline(yintercept=1, linetype='dashed', color='blue') +
  ylab("X~Y Odds Ratio") + xlab("X ~ Collider Coefficient") +
  theme_minimal()

# View the graphs
ggarrange(plot9, plot10, plot11, plot12, labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


################################################################################
# Scenario 2
# Mediator ~ Y variation

# Set the parameters we will be using
betaXY   <- 0.0   # this is the true effect of X on Y, here set as zero

# Run a loop of betaXMed values from -1.0 to +1.0 
betaXMed <- seq(-1.0, 1.0, by = 0.1)

resultsMed1 <- lapply(betaXMed, sim_Med, betaMedY = 0.25, betaXY) %>%
  bind_rows()
# head(resultsMed1) # examine the output
resultsMed2 <- lapply(betaXMed, sim_Med, betaMedY = 0.50, betaXY) %>%
  bind_rows()
# head(resultsMed2) # examine the output
resultsMed3 <- lapply(betaXMed, sim_Med, betaMedY = 0.75, betaXY) %>%
  bind_rows()
# head(resultsMed3) # examine the output

# Now we can plot the estimated coefficient from the two regression models to show 
# the bias caused by different levels of mediator bias.
# The plot includes horizontal line which should be set at the 'true' value of the coefficient.

# Graph for simple regression, showing mediator bias
plot13 <- ggplot(NULL, aes(x=betaXMed, y=coefXY)) +
  geom_point(data=resultsMed1, shape=19, size = 1.7, aes(colour="0.25")) +
  stat_smooth(data=resultsMed1, method=loess, colour = "darkgoldenrod1", fill = "lemonchiffon") +
  geom_errorbar(data=resultsMed1,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='darkgoldenrod1') +
  geom_point(data=resultsMed2, shape=19, size = 1.7, aes(colour="0.50")) +
  stat_smooth(data=resultsMed2, method=loess, colour = "darkorange2", fill = "bisque") +
  geom_errorbar(data=resultsMed2,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='darkorange2') +
  geom_point(data=resultsMed3, shape=19, size = 1.7, aes(colour="0.75")) +
  stat_smooth(data=resultsMed3, method=loess, colour = "sienna", fill = "antiquewhite") +
  geom_errorbar(data=resultsMed3,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='sienna') +
  geom_hline(yintercept=0.0, linetype='dashed', colour='blue') +  
  scale_color_manual(name="Mediator ~ Y", 
                     breaks=c("0.25", "0.50", "0.75"),
                     values = c("darkgoldenrod1", "darkorange2", "sienna")) +
  ylim(-1, 1) +
  theme_minimal() + 
  xlab("X ~ Mediator Coefficient") + ylab("X ~ Y Coefficient") +
  theme(legend.position = "none")

# Graph for multiple regression, accounting for mediator
plot14 <- ggplot(NULL, aes(x=betaXMed, y=coefXMedY)) +
  geom_point(data=resultsMed1, shape=19, size = 1.7, aes(colour="0.25")) +
  geom_errorbar(data=resultsMed1,aes(ymin=coefXMedY.lo, ymax=coefXMedY.up), width = .02, colour='darkgoldenrod1') +
  geom_point(data=resultsMed2, shape=19, size = 1.7, aes(colour="0.50")) +
  geom_errorbar(data=resultsMed2,aes(ymin=coefXMedY.lo, ymax=coefXMedY.up), width = .02, colour='darkorange2') +
  geom_point(data=resultsMed3, shape=19, size = 1.7, aes(colour="0.75")) +
  geom_errorbar(data=resultsMed3,aes(ymin=coefXMedY.lo, ymax=coefXMedY.up), width = .02, colour='sienna') +
  geom_hline(yintercept=0.0, linetype='dashed', colour='blue') +  
  scale_color_manual(name="Mediator ~ Y", 
                     breaks=c("0.25", "0.50", "0.75"),
                     values = c("darkgoldenrod1", "darkorange2", "sienna")) +  
  ylim(-1, 1) +
  theme_minimal() + 
  xlab("X ~ Mediator Coefficient") + ylab("X ~ Y Coefficient incl. Mediator") +
  theme(legend.position = "none")

# View the graphs
ggarrange(plot13, plot14,
          ncol = 2, nrow = 1)


################################################################################
# Scenario 2
# Confounder ~ Y variation

# Set the parameters we will be using
betaXY   <- 0.0  # this is the true effect of X on Y, here set as zero

# Running the function in a loop using different betaConX values between -1.0 (strong confounded 
# negative correlation) and +1.0 (strong confounded positive correlation). 0 means no confounding.
betaConX <- seq(-1.0, 1.0, by = 0.1)

resultsCon1 <- lapply(betaConX, sim_Con, betaConY = 0.25, betaXY) %>%
  bind_rows()
# head(resultsCon1) # examine the output
resultsCon2 <- lapply(betaConX, sim_Con, betaConY = 0.50, betaXY) %>%
  bind_rows()
# head(resultsCon2) # examine the output
resultsCon3 <- lapply(betaConX, sim_Con, betaConY = 0.75, betaXY) %>%
  bind_rows()
# head(resultsCon3) # examine the output

# Now we can plot the estimated coefficient from the two regression models to show 
# the bias caused by different levels of confounding.
# The plot includes horizontal line which should be set at the 'true' value of the coefficient.

# Graph for simple regression, showing confounding bias
plot15 <- ggplot(NULL, aes(x=betaConX, y=coefXY)) +
  geom_point(data=resultsCon1, shape=19, size = 1.7, aes(colour="0.25")) +
  stat_smooth(data=resultsCon1, method=loess, colour = "darkgoldenrod1", fill = "lemonchiffon") +
  geom_errorbar(data=resultsCon1,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='darkgoldenrod1') +
  geom_point(data=resultsCon2, shape=19, size = 1.7, aes(colour="0.50")) +
  stat_smooth(data=resultsCon2, method=loess, colour = "darkorange2", fill = "bisque") +
  geom_errorbar(data=resultsCon2,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='darkorange2') +
  geom_point(data=resultsCon3, shape=19, size = 1.7, aes(colour="0.75")) +
  stat_smooth(data=resultsCon3, method=loess, colour = "sienna", fill = "antiquewhite") +
  geom_errorbar(data=resultsCon3,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='sienna') +
  geom_hline(yintercept=0.0, linetype='dashed', colour='blue') +  
  scale_color_manual(name="Confounder ~ Y", 
                     breaks=c("0.25", "0.50", "0.75"),
                     values = c("darkgoldenrod1", "darkorange2", "sienna")) +  
  ylim(-0.5, 0.5) + theme_minimal() + 
  xlab("Confounder ~ X Coefficient") + ylab("X ~ Y Coefficient") +
  theme(legend.position = "none")

# Graph for multiple regression, accounting for confounder
plot16 <- ggplot(NULL, aes(x=betaConX, y=coefXYCon)) +
  geom_point(data=resultsCon1, shape=19, size = 1.7, aes(colour="0.25")) +
  geom_errorbar(data=resultsCon1,aes(ymin=coefXYCon.lo, ymax=coefXYCon.up), width = .02, colour='darkgoldenrod1') +
  geom_point(data=resultsCon2, shape=19, size = 1.7, aes(colour="0.50")) +
  geom_errorbar(data=resultsCon2,aes(ymin=coefXYCon.lo, ymax=coefXYCon.up), width = .02, colour='darkorange2') +
  geom_point(data=resultsCon3, shape=19, size = 1.7, aes(colour="0.75")) +
  geom_errorbar(data=resultsCon3,aes(ymin=coefXYCon.lo, ymax=coefXYCon.up), width = .02, colour='sienna') +
  geom_hline(yintercept=0.0, linetype='dashed', colour='blue') +  
  scale_color_manual(name="Confounder ~ Y", 
                     breaks=c("0.25", "0.50", "0.75"),
                     values = c("darkgoldenrod1", "darkorange2", "sienna")) +  
  ylim(-0.5, 0.5) + theme_minimal() + 
  xlab("Confounder ~ X Coefficient") + ylab("X ~ Y Coefficient incl. Confounder") +
  theme(legend.position = "none")

# View the graphs
ggarrange(plot15, plot16,
          ncol = 2, nrow = 1)


################################################################################
# Scenario 2
# Y ~ Collider variation

# Set the parameters we will be using
betaXY   <- 0.0  # this is the true effect of X on Y, here set as zero

# Running the function in a loop using different betaXCol values between -1.0 (strong confounded 
# negative correlation) and +1.0 (strong confounded positive correlation). 0 means no confounding.
betaXCol <- seq(-1.0, 1.0, by = 0.1)

resultsCol1 <- lapply(betaXCol, sim_Col, betaYCol = 0.25, betaXY) %>%
  bind_rows() 
# head(resultsCol1) # examine the output
resultsCol2 <- lapply(betaXCol, sim_Col, betaYCol = 0.50, betaXY) %>%
  bind_rows()
# head(resultsCol2) # examine the output
resultsCol3 <- lapply(betaXCol, sim_Col, betaYCol = 0.75, betaXY) %>%
  bind_rows()
# head(resultsCol3) # examine the output

# Now we can plot the estimated coefficient from the two regression models to show 
# the bias caused by different levels of collider bias.
# The plot includes horizontal line which should be set at the 'true' value of the coefficient.

# Graph for multiple regression, showing collider bias
plot17 <- ggplot(NULL, aes(x=betaXCol, y=coefXYCol)) +
  geom_point(data=resultsCol1, shape=19, size = 1.7, aes(colour="0.25")) +
  stat_smooth(data=resultsCol1, method=loess, colour = "darkgoldenrod1", fill = "lemonchiffon") +
  geom_errorbar(data=resultsCol1,aes(ymin=coefXYCol.lo, ymax=coefXYCol.up), width = .02, colour='darkgoldenrod1') +
  geom_point(data=resultsCol2, shape=19, size = 1.7, aes(colour="0.50")) +
  stat_smooth(data=resultsCol2, method=loess, colour = "darkorange2", fill = "bisque") +
  geom_errorbar(data=resultsCol2,aes(ymin=coefXYCol.lo, ymax=coefXYCol.up), width = .02, colour='darkorange2') +
  geom_point(data=resultsCol3, shape=19, size = 1.7, aes(colour="0.75")) +
  stat_smooth(data=resultsCol3, method=loess, colour = "sienna", fill = "antiquewhite") +
  geom_errorbar(data=resultsCol3,aes(ymin=coefXYCol.lo, ymax=coefXYCol.up), width = .02, colour='sienna') +
  geom_hline(yintercept=0.0, linetype='dashed', colour='blue') +  
  scale_color_manual(name="Y ~ Collider", 
                     breaks=c("0.25", "0.50", "0.75"),
                     values = c("darkgoldenrod1", "darkorange2", "sienna")) +
  ylim(-0.6, 0.6) +
  theme_minimal() + 
  xlab("X ~ Collider Coefficient") + ylab("X ~ Y Coefficient incl. Collider") +
  theme(legend.position = "none")

# Graph for simple regression, unbiased by collider
plot18 <- ggplot(NULL, aes(x=betaXCol, y=coefXY)) +
  geom_point(data=resultsCol1, shape=19, size = 1.7, aes(colour="0.25")) +
  geom_errorbar(data=resultsCol1,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='darkgoldenrod1') +
  geom_point(data=resultsCol2, shape=19, size = 1.7, aes(colour="0.50")) +
  geom_errorbar(data=resultsCol2,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='darkorange2') +
  geom_point(data=resultsCol3, shape=19, size = 1.7, aes(colour="0.75")) +
  geom_errorbar(data=resultsCol3,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='sienna') +
  geom_hline(yintercept=0.0, linetype='dashed', colour='blue') +  
  scale_color_manual(name="Y ~ Collider", 
                     breaks=c("0.25", "0.50", "0.75"),
                     values = c("darkgoldenrod1", "darkorange2", "sienna")) +  
  ylim(-0.6, 0.6) +
  theme_minimal() + 
  xlab("X ~ Collider Coefficient") + ylab("X ~ Y Coefficient") +
  theme(legend.position = "none")

# View the graphs
ggarrange(plot17, plot18,
          ncol = 2, nrow = 1)


################################################################################
# Scenario 3
# X ~ Y variation (mediator)

# Set the parameters we will be using
betaMedY <- 0.5   # this is the true effect of X on Y, here set as zero

# Run a loop of betaXMed values from -1.0 to +1.0 
betaXMed <- seq(-1.0, 1.0, by = 0.1)

resultsMed1 <- lapply(betaXMed, sim_Med, betaMedY, betaXY = 0.25) %>%
  bind_rows()
# head(resultsMed1) # examine the output
resultsMed2 <- lapply(betaXMed, sim_Med, betaMedY, betaXY = 0.50) %>%
  bind_rows()
# head(resultsMed2) # examine the output
resultsMed3 <- lapply(betaXMed, sim_Med, betaMedY, betaXY = 0.75) %>%
  bind_rows()
# head(resultsMed3) # examine the output

# Now we can plot the estimated coefficient from the two regression models to show 
# the bias caused by different levels of mediator bias.
# The plot includes horizontal line which should be set at the 'true' value of the coefficient.

# Graph for simple regression, showing mediator bias
plot19 <- ggplot(NULL, aes(x=betaXMed, y=coefXY)) +
  geom_point(data=resultsMed1, shape=19, size = 1.7, aes(colour="0.25")) +
  geom_errorbar(data=resultsMed1,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='darkgoldenrod1') +
  geom_point(data=resultsMed2, shape=19, size = 1.7, aes(colour="0.50")) +
  geom_errorbar(data=resultsMed2,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='darkorange2') +
  geom_point(data=resultsMed3, shape=19, size = 1.7, aes(colour="0.75")) +
  geom_errorbar(data=resultsMed3,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='sienna') +
  geom_hline(yintercept=0.25, linetype='dashed', colour='darkgoldenrod1') +  
  geom_hline(yintercept=0.50, linetype='dashed', colour='darkorange2') +  
  geom_hline(yintercept=0.75, linetype='dashed', colour='sienna') +  
  scale_color_manual(name="True X ~ Y", 
                     breaks=c("0.25", "0.50", "0.75"),
                     values = c("darkgoldenrod1", "darkorange2", "sienna")) +
  ylim(-0.5, 1.5) +
  theme_minimal() + 
  xlab("X ~ Mediator Coefficient") + ylab("X ~ Y Coefficient") +
  theme(legend.position = "none")

# Graph for multiple regression, accounting for mediator
plot20 <- ggplot(NULL, aes(x=betaXMed, y=coefXMedY)) +
  geom_point(data=resultsMed1, shape=19, size = 1.7, aes(colour="0.25")) +
  geom_errorbar(data=resultsMed1,aes(ymin=coefXMedY.lo, ymax=coefXMedY.up), width = .02, colour='darkgoldenrod1') +
  geom_point(data=resultsMed2, shape=19, size = 1.7, aes(colour="0.50")) +
  geom_errorbar(data=resultsMed2,aes(ymin=coefXMedY.lo, ymax=coefXMedY.up), width = .02, colour='darkorange2') +
  geom_point(data=resultsMed3, shape=19, size = 1.7, aes(colour="0.75")) +
  geom_errorbar(data=resultsMed3,aes(ymin=coefXMedY.lo, ymax=coefXMedY.up), width = .02, colour='sienna') +
  geom_hline(yintercept=0.25, linetype='dashed', colour='darkgoldenrod1') +  
  geom_hline(yintercept=0.50, linetype='dashed', colour='darkorange2') +  
  geom_hline(yintercept=0.75, linetype='dashed', colour='sienna') +  
  scale_color_manual(name="True X ~ Y", 
                     breaks=c("0.25", "0.50", "0.75"),
                     values = c("darkgoldenrod1", "darkorange2", "sienna")) +  
  ylim(-0.5, 1.5) +
  theme_minimal() + 
  xlab("X ~ Mediator Coefficient") + ylab("X ~ Y Coefficient incl. Mediator") +
  theme(legend.position = "none")

# View the graphs
ggarrange(plot19, plot20,
          ncol = 2, nrow = 1)


################################################################################
# Scenario 3
# X ~ Y variation (confounder)

# Set the parameters we will be using
betaConY <- 0.5  # the confounder has quite a strong (0.5 coefficient) effect on Y

# Running the function in a loop using different betaConX values between -1.0 (strong confounded 
# negative correlation) and +1.0 (strong confounded positive correlation). 0 means no confounding.
betaConX <- seq(-1.0, 1.0, by = 0.1)

resultsCon1 <- lapply(betaConX, sim_Con, betaConY, betaXY = 0.25) %>%
  bind_rows()
# head(resultsCon1) # examine the output
resultsCon2 <- lapply(betaConX, sim_Con, betaConY, betaXY = 0.50) %>%
  bind_rows()
# head(resultsCon2) # examine the output
resultsCon3 <- lapply(betaConX, sim_Con, betaConY, betaXY = 0.75) %>%
  bind_rows()
# head(resultsCon3) # examine the output

# Now we can plot the estimated coefficient from the two regression models to show 
# the bias caused by different levels of confounding.
# The plot includes horizontal line which should be set at the 'true' value of the coefficient.

# Graph for simple regression, showing confounding bias
plot21 <- ggplot(NULL, aes(x=betaConX, y=coefXY)) +
  geom_point(data=resultsCon1, shape=19, size = 1.7, aes(colour="0.25")) +
  geom_errorbar(data=resultsCon1,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='darkgoldenrod1') +
  geom_point(data=resultsCon2, shape=19, size = 1.7, aes(colour="0.50")) +
  geom_errorbar(data=resultsCon2,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='darkorange2') +
  geom_point(data=resultsCon3, shape=19, size = 1.7, aes(colour="0.75")) +
  geom_errorbar(data=resultsCon3,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='sienna') +
  geom_hline(yintercept=0.25, linetype='dashed', colour='darkgoldenrod1') +  
  geom_hline(yintercept=0.50, linetype='dashed', colour='darkorange2') +  
  geom_hline(yintercept=0.75, linetype='dashed', colour='sienna') +  
  scale_color_manual(name="True X ~ Y", 
                     breaks=c("0.25", "0.50", "0.75"),
                     values = c("darkgoldenrod1", "darkorange2", "sienna")) +  
  ylim(-0.2, 1.2) +
  theme_minimal() + 
  xlab("Confounder ~ X Coefficient") + ylab("X ~ Y Coefficient") +
  theme(legend.position = "none")

# Graph for multiple regression, accounting for confounder
plot22 <- ggplot(NULL, aes(x=betaConX, y=coefXYCon)) +
  geom_point(data=resultsCon1, shape=19, size = 1.7, aes(colour="0.25")) +
  geom_errorbar(data=resultsCon1,aes(ymin=coefXYCon.lo, ymax=coefXYCon.up), width = .02, colour='darkgoldenrod1') +
  geom_point(data=resultsCon2, shape=19, size = 1.7, aes(colour="0.50")) +
  geom_errorbar(data=resultsCon2,aes(ymin=coefXYCon.lo, ymax=coefXYCon.up), width = .02, colour='darkorange2') +
  geom_point(data=resultsCon3, shape=19, size = 1.7, aes(colour="0.75")) +
  geom_errorbar(data=resultsCon3,aes(ymin=coefXYCon.lo, ymax=coefXYCon.up), width = .02, colour='sienna') +
  geom_hline(yintercept=0.25, linetype='dashed', colour='darkgoldenrod1') +  
  geom_hline(yintercept=0.50, linetype='dashed', colour='darkorange2') +  
  geom_hline(yintercept=0.75, linetype='dashed', colour='sienna') +  
  scale_color_manual(name="True X ~ Y", 
                     breaks=c("0.25", "0.50", "0.75"),
                     values = c("darkgoldenrod1", "darkorange2", "sienna")) +  
  ylim(-0.2, 1.2) +
  theme_minimal() + 
  xlab("Confounder ~ X Coefficient") + ylab("X ~ Y Coefficient incl. Confounder") +
  theme(legend.position = "none")

# View the graphs
ggarrange(plot21, plot22,
          ncol = 2, nrow = 1)


################################################################################
# Scenario 3
# X ~ Y variation (collider)

# Set the parameters we will be using
betaYCol <- 0.5  # this is the true effect of Y on the collider, here set as 0.5

# Running the function in a loop using different betaXCol values between -1.0 (strong confounded 
# negative correlation) and +1.0 (strong confounded positive correlation). 0 means no confounding.
betaXCol <- seq(-1.0, 1.0, by = 0.1)

resultsCol1 <- lapply(betaXCol, sim_Col, betaYCol, betaXY = 0.25) %>%
  bind_rows() 
# head(resultsCol1) # examine the output
resultsCol2 <- lapply(betaXCol, sim_Col, betaYCol, betaXY = 0.50) %>%
  bind_rows()
# head(resultsCol2) # examine the output
resultsCol3 <- lapply(betaXCol, sim_Col, betaYCol, betaXY = 0.75) %>%
  bind_rows()
# head(resultsCol3) # examine the output

# Now we can plot the estimated coefficient from the two regression models to show 
# the bias caused by different levels of collider bias.
# The plot includes horizontal line which should be set at the 'true' value of the coefficient.

# Graph for multiple regression, showing collider bias
plot23 <- ggplot(NULL, aes(x=betaXCol, y=coefXYCol)) +
  geom_point(data=resultsCol1, shape=19, size = 1.7, aes(colour="0.25")) +
  geom_errorbar(data=resultsCol1,aes(ymin=coefXYCol.lo, ymax=coefXYCol.up), width = .02, colour='darkgoldenrod1') +
  geom_point(data=resultsCol2, shape=19, size = 1.7, aes(colour="0.50")) +
  geom_errorbar(data=resultsCol2,aes(ymin=coefXYCol.lo, ymax=coefXYCol.up), width = .02, colour='darkorange2') +
  geom_point(data=resultsCol3, shape=19, size = 1.7, aes(colour="0.75")) +
  geom_errorbar(data=resultsCol3,aes(ymin=coefXYCol.lo, ymax=coefXYCol.up), width = .02, colour='sienna') +
  geom_hline(yintercept=0.25, linetype='dashed', colour='darkgoldenrod1') +  
  geom_hline(yintercept=0.50, linetype='dashed', colour='darkorange2') +  
  geom_hline(yintercept=0.75, linetype='dashed', colour='sienna') +  
  scale_color_manual(name="True X ~ Y", 
                     breaks=c("0.25", "0.50", "0.75"),
                     values = c("darkgoldenrod1", "darkorange2", "sienna")) +
  ylim(-0.3, 1.3) +
  theme_minimal() + 
  xlab("X ~ Collider Coefficient") + ylab("X ~ Y Coefficient incl. Collider") +
  theme(legend.position = "none")

# Graph for simple regression, unbiased by collider
plot24 <- ggplot(NULL, aes(x=betaXCol, y=coefXY)) +
  geom_point(data=resultsCol1, shape=19, size = 1.7, aes(colour="0.25")) +
  geom_errorbar(data=resultsCol1,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='darkgoldenrod1') +
  geom_point(data=resultsCol2, shape=19, size = 1.7, aes(colour="0.50")) +
  geom_errorbar(data=resultsCol2,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='darkorange2') +
  geom_point(data=resultsCol3, shape=19, size = 1.7, aes(colour="0.75")) +
  geom_errorbar(data=resultsCol3,aes(ymin=coefXY.lo, ymax=coefXY.up), width = .02, colour='sienna') +
  geom_hline(yintercept=0.25, linetype='dashed', colour='darkgoldenrod1') +  
  geom_hline(yintercept=0.50, linetype='dashed', colour='darkorange2') +  
  geom_hline(yintercept=0.75, linetype='dashed', colour='sienna') +  
  scale_color_manual(name="True X ~ Y", 
                     breaks=c("0.25", "0.50", "0.75"),
                     values = c("darkgoldenrod1", "darkorange2", "sienna")) +  
  ylim(-0.3, 1.3) +
  theme_minimal() + 
  xlab("X ~ Collider Coefficient") + ylab("X ~ Y Coefficient") +
  theme(legend.position = "none")

# View the graphs
ggarrange(plot23, plot24,
          ncol = 2, nrow = 1)


################################################################################
# Scenario 4
# Simple example of all third variables (linear)

# Define parameters
n <- 1000
error <- 1 
betaXY   <-  0.2 # effect of smoking history on covid severity
betaXCol <-  0.4 # effect of smoking history on hospitalisation
betaYCol <-  0.5 # effect of covid severity on hospitalisation
betaConX <- -0.6 # effect of HCW status on smoking history
betaConY <-  0.3 # effect of HCW status on covid severity
betaXMed <-  0.7 # effect of smoking history on smoking-related comorbidities
betaMedY <-  0.5 # effect of smoking-related comorbidities on covid severity

# Create variables
Con <- error*rnorm(n)                                          # generate Confounder
X   <- betaConX*Con                           + error*rnorm(n)   # generate X from Confounder
Med <- betaXMed*X                             + error*rnorm(n) # generate Mediator from X
Y   <- betaXY*X + betaConY*Con + betaMedY*Med + error*rnorm(n)   # generate Y from Confounder, Mediator and X
Col <- betaXCol*X + betaYCol*Y                + error*rnorm(n) # generate Collider from X and Y

# Run regressions
lm1 <- lm(Y~X)                     
lm2 <- lm(Y~X+Con)                   
lm3 <- lm(Y~X+Col)
lm4 <- lm(Y~X+Med)
lm5 <- lm(Y~X+Con+Col)
lm6 <- lm(Y~X+Col+Med)
lm7 <- lm(Y~X+Con+Med+Col)
lm8 <- lm(Y~X+Con+Med)
coefXY    <- lm1$coefficients[2]  # save the estimate effect of X on Y from the simple regression
coefXYCon <- lm2$coefficients[2]  # save the estimate effect of X on Y from the multiple regression
coefXYCol <- lm3$coefficients[2]
coefXMedY <- lm4$coefficients[2]
coefXYConCol <- lm5$coefficients[2]
coefXMedYCol <- lm6$coefficients[2]
coefXMedYConCol <- lm7$coefficients[2]
coefXMedYCon <- lm8$coefficients[2]
coefXY.up <- confint(lm1)[2,2]  # finally saving confidence intervals
coefXY.lo <- confint(lm1)[2,1]
coefXYCon.up <- confint(lm2)[2,2]
coefXYCon.lo <- confint(lm2)[2,1]
coefXYCol.up <- confint(lm3)[2,2] 
coefXYCol.lo <- confint(lm3)[2,1] 
coefXMedY.up <- confint(lm4)[2,2] 
coefXMedY.lo <- confint(lm4)[2,1] 
coefXYConCol.up <- confint(lm5)[2,2] 
coefXYConCol.lo <- confint(lm5)[2,1] 
coefXMedYCol.up <- confint(lm6)[2,2] 
coefXMedYCol.lo <- confint(lm6)[2,1] 
coefXMedYConCol.up <- confint(lm7)[2,2] 
coefXMedYConCol.lo <- confint(lm7)[2,1] 
coefXMedYCon.up <- confint(lm8)[2,2] 
coefXMedYCon.lo <- confint(lm8)[2,1] 

# Create data frame
model <- c("None", "Confounder", "Collider", "Mediator",
           "Collider + confounder", "Collider + mediator",
           "Collider + confounder + mediator", "Confounder + mediator")
coef  <- c(coefXY, coefXYCon, coefXYCol, coefXMedY, coefXYConCol, coefXMedYCol, 
           coefXMedYConCol, coefXMedYCon)
upper <- c(coefXY.up, coefXYCon.up, coefXYCol.up, coefXMedY.up, coefXYConCol.up,
           coefXMedYCol.up, coefXMedYConCol.up, coefXMedYCon.up)
lower <- c(coefXY.lo, coefXYCon.lo, coefXYCol.lo, coefXMedY.lo, coefXYConCol.lo,
           coefXMedYCol.lo, coefXMedYConCol.lo, coefXMedYCon.lo)
dat <- data.frame(model, coef, upper, lower)
dat$model <- factor(dat$model, levels=c("None", "Confounder", "Mediator", "Collider",
                                        "Collider + confounder", "Collider + mediator",
                                        "Collider + confounder + mediator", "Confounder + mediator"))

# Plot graph of coefficients
plot25 <- ggplot(dat, aes(x=coef, y=model)) +
  geom_point(shape=19, size = 1.7) +
  geom_vline(xintercept=0.2, linetype='dashed', color='blue') +
  geom_errorbar(aes(xmin=lower, xmax=upper), width=.02, colour = "black") +
  theme_minimal() + 
  xlab("X ~ Y Coefficients") + ylab("Covariates included")
  

################################################################################
# Scenario 4
# Simple example of all third variables (logistic)

# Define parameters (multiplied effects from linear example by 3 to get better effect sizes)
n <- 10000
betaXY   <-  0.6 # effect of smoking history on covid severity
betaXCol <-  1.2 # effect of smoking history on hospitalisation
betaYCol <-  1.5 # effect of covid severity on hospitalisation
betaConX <- -1.8 # effect of HCW status on smoking history
betaConY <-  0.9 # effect of HCW status on covid severity
betaXMed <-  2.1 # effect of smoking history on smoking-related comorbidities
betaMedY <-  1.5 # effect of smoking-related comorbidities on covid severity

# Create variables
Con <- rbinom(n, 1, 0.5)    # generate Confounder
p.X <- 1/(1+exp(-betaConX*Con))
X   <- rbinom(n, 1, p.X)    # generate X from Confounder
p.Med <- 1/(1+exp(-betaXMed*X))
Med <- rbinom(n, 1, p.Med)  # generate Mediator from X
p.Y <- 1/(1+exp(-betaXY*X -betaConY*Con -betaMedY*Med))
Y   <- rbinom(n, 1, p.Y)    # generate Y from Confounder, Mediator and X
p.Col <- 1/(1+exp(-betaXCol*X -betaYCol*Y))
Col <- rbinom(n, 1, p.Col)  # generate Collider from X and Y

# Run regressions
lm1 <- glm(Y~X, family = binomial)                     
lm2 <- glm(Y~X+Con, family = binomial)                   
lm3 <- glm(Y~X+Col, family = binomial)
lm4 <- glm(Y~X+Med, family = binomial)
lm5 <- glm(Y~X+Con+Col, family = binomial)
lm6 <- glm(Y~X+Col+Med, family = binomial)
lm7 <- glm(Y~X+Con+Med+Col, family = binomial)
lm8 <- glm(Y~X+Con+Med, family = binomial)
coefXY    <- exp(lm1$coefficients[2])  # save the estimate effect of X on Y 
coefXYCon <- exp(lm2$coefficients[2])  
coefXYCol <- exp(lm3$coefficients[2])
coefXMedY <- exp(lm4$coefficients[2])
coefXYConCol <- exp(lm5$coefficients[2])
coefXMedYCol <- exp(lm6$coefficients[2])
coefXMedYConCol <- exp(lm7$coefficients[2])
coefXMedYCon <- exp(lm8$coefficients[2])
coefXY.up <- exp(confint(lm1)[2,2])    # save confidence intervals
coefXY.lo <- exp(confint(lm1)[2,1])
coefXYCon.up <- exp(confint(lm2)[2,2])
coefXYCon.lo <- exp(confint(lm2)[2,1])
coefXYCol.up <- exp(confint(lm3)[2,2])
coefXYCol.lo <- exp(confint(lm3)[2,1])
coefXMedY.up <- exp(confint(lm4)[2,2])
coefXMedY.lo <- exp(confint(lm4)[2,1])
coefXYConCol.up <- exp(confint(lm5)[2,2])
coefXYConCol.lo <- exp(confint(lm5)[2,1])
coefXMedYCol.up <- exp(confint(lm6)[2,2])
coefXMedYCol.lo <- exp(confint(lm6)[2,1])
coefXMedYConCol.up <- exp(confint(lm7)[2,2])
coefXMedYConCol.lo <- exp(confint(lm7)[2,1])
coefXMedYCon.up <- exp(confint(lm8)[2,2])
coefXMedYCon.lo <- exp(confint(lm8)[2,1])

# Create data frame
model <- c("None", "Confounder", "Collider", "Mediator",
           "Collider + confounder", "Collider + mediator",
           "Collider + confounder + mediator", "Confounder + mediator")
coef  <- c(coefXY, coefXYCon, coefXYCol, coefXMedY, coefXYConCol, coefXMedYCol, 
           coefXMedYConCol, coefXMedYCon)
upper <- c(coefXY.up, coefXYCon.up, coefXYCol.up, coefXMedY.up, coefXYConCol.up,
           coefXMedYCol.up, coefXMedYConCol.up, coefXMedYCon.up)
lower <- c(coefXY.lo, coefXYCon.lo, coefXYCol.lo, coefXMedY.lo, coefXYConCol.lo,
           coefXMedYCol.lo, coefXMedYConCol.lo, coefXMedYCon.lo)
dat <- data.frame(model, coef, upper, lower)
dat$model <- factor(dat$model, levels=c("None", "Confounder", "Mediator", "Collider",
                                        "Collider + confounder", "Collider + mediator",
                                        "Collider + confounder + mediator", "Confounder + mediator"))

# Plot graph of coefficients
plot26 <- ggplot(dat, aes(x=coef, y=model)) +
  geom_point(shape=19, size = 1.7) +
  geom_vline(xintercept=1.8, linetype='dashed', color='blue') +
  geom_errorbar(aes(xmin=lower, xmax=upper), width=.02, colour = "black") +
  theme_minimal() + 
  xlab("X ~ Y Odds Ratio") + ylab("Covariates included")

# View the graphs
ggarrange(plot25, plot26, labels = c("A", "B"),
          ncol = 1, nrow = 2)


################################################################################
# Scenario 5
# Complex example of all third variables (linear)

# Setting parameters and creating variables
n     <- 1000
error <- 1
age       <-  error*rnorm(n)     # generate Con from standard normal distribution
ethnicity <-  error*rnorm(n)     # generate Con from standard normal distribution
hcw       <-  0.2*ethnicity + error*rnorm(n)     # generate Con from ethnicity
smoking   <- -0.4*hcw + -0.2*ethnicity + error*rnorm(n)    # generate  X  from hcw + ethnicity
comorbid  <-  0.3*smoking   + error*rnorm(n)               # generate Med from smoking 
# generate  Y  from smoking + age + comorbid + hcw + ethnicity 
covidsev  <-  0.2*smoking + 0.2*age + 0.4*comorbid + 0.3*hcw + 0.2*ethnicity + error*rnorm(n)   
# generate Col from smoking + hcw + age + comorbid + covidsev + ethnicity 
hospital  <-  0.2*smoking + 0.1*hcw + 0.4*age + 0.3*comorbid + 0.5*covidsev + 0.1*ethnicity + error*rnorm(n)    
# generate Col from smoking + hcw + age + comorbid + covidsev + hospital
testing   <-  0.1*smoking + 0.3*hcw + 0.2*age + comorbid*0.2 + covidsev*0.3 + 0.5*hospital + error*rnorm(n)   

# Running regressions
lm1 <- lm(covidsev ~ smoking)
lm2 <- lm(covidsev ~ smoking + age + ethnicity + hcw) # adjust for confounders
lm3 <- lm(covidsev ~ smoking + hospital + testing)    # adjust for colliders
lm4 <- lm(covidsev ~ smoking + comorbid)              # adjust for mediator
lm5 <- lm(covidsev ~ smoking + age + ethnicity + hcw + hospital + testing) # confounders + colliders
lm6 <- lm(covidsev ~ smoking + hospital + testing + comorbid)              # colliders + mediator
lm7 <- lm(covidsev ~ smoking + age + ethnicity + hcw + hospital + testing + comorbid)  # all
lm8 <- lm(covidsev ~ smoking + age + ethnicity + hcw + comorbid)           # confounders + mediator
coefXY    <- lm1$coefficients[2]  # save the estimate effect of X on Y from the simple regression
coefXYCon <- lm2$coefficients[2]  # save the estimate effect of X on Y from the multiple regressions
coefXYCol <- lm3$coefficients[2]
coefXMedY <- lm4$coefficients[2]
coefXYConCol <- lm5$coefficients[2]
coefXMedYCol <- lm6$coefficients[2]
coefXMedYConCol <- lm7$coefficients[2]
coefXMedYCon <- lm8$coefficients[2]
coefXY.up <- confint(lm1)[2,2]    # save confidence intervals
coefXY.lo <- confint(lm1)[2,1]
coefXYCon.up <- confint(lm2)[2,2]
coefXYCon.lo <- confint(lm2)[2,1]
coefXYCol.up <- confint(lm3)[2,2] 
coefXYCol.lo <- confint(lm3)[2,1] 
coefXMedY.up <- confint(lm4)[2,2] 
coefXMedY.lo <- confint(lm4)[2,1] 
coefXYConCol.up <- confint(lm5)[2,2] 
coefXYConCol.lo <- confint(lm5)[2,1] 
coefXMedYCol.up <- confint(lm6)[2,2] 
coefXMedYCol.lo <- confint(lm6)[2,1] 
coefXMedYConCol.up <- confint(lm7)[2,2] 
coefXMedYConCol.lo <- confint(lm7)[2,1] 
coefXMedYCon.up <- confint(lm8)[2,2] 
coefXMedYCon.lo <- confint(lm8)[2,1] 

# Create data frame
model <- c("None", "Confounders", "Colliders", "Mediator",
           "Colliders + confounders", "Colliders + mediator",
           "Colliders + confounders + mediator", "Confounders + mediator")
coef  <- c(coefXY, coefXYCon, coefXYCol, coefXMedY, coefXYConCol, coefXMedYCol, 
           coefXMedYConCol, coefXMedYCon)
upper <- c(coefXY.up, coefXYCon.up, coefXYCol.up, coefXMedY.up, coefXYConCol.up,
           coefXMedYCol.up, coefXMedYConCol.up, coefXMedYCon.up)
lower <- c(coefXY.lo, coefXYCon.lo, coefXYCol.lo, coefXMedY.lo, coefXYConCol.lo,
           coefXMedYCol.lo, coefXMedYConCol.lo, coefXMedYCon.lo)
dat <- data.frame(model, coef, upper, lower)
dat$model <- factor(dat$model, levels=c("None", "Confounders", "Mediator", "Colliders",
                                        "Colliders + confounders", "Colliders + mediator",
                                        "Colliders + confounders + mediator", "Confounders + mediator"))

# Plot graph of coefficients
plot27 <- ggplot(dat, aes(x=coef, y=model)) +
  geom_point(shape=19, size = 1.7) +
  geom_vline(xintercept=0.2, linetype='dashed', color='blue') +
  geom_errorbar(aes(xmin=lower, xmax=upper), width=.02, colour = "black") +
  theme_minimal() + 
  xlab("X ~ Y Coefficients") + ylab("Covariates included")

################################################################################
# Scenario 5
# Complex example of all third variables (logistic)

# Define parameters (effects same as linear but multiplied by 3 for better effect sizes)

n <- 10000
age        <- rbinom(n, 1, 0.5) # generate Con from standard normal distribution
ethnicity  <- rbinom(n, 1, 0.5) # generate Con from standard normal distribution
p.hcw      <- 1 / (1 + exp(-0.6*ethnicity))
hcw        <- rbinom(n, 1, p.hcw)     # generate Con from ethnicity
p.smoking  <- 1 / (1 + exp(-(-1.2*hcw) -(-0.6*ethnicity)))
smoking    <- rbinom(n, 1, p.smoking)    # generate  X  from hcw + ethnicity
p.comorbid <- 1 / (1 + exp(-0.9*smoking))
comorbid   <- rbinom(n, 1, p.comorbid)      # generate Med from smoking
# generate  Y  from smoking + age + comorbid + hcw + ethnicity
p.covidsev <- 1 / (1 + exp(-0.6*smoking -0.6*age -1.2*comorbid -0.9*hcw -0.6*ethnicity))
covidsev   <- rbinom(n, 1, p.covidsev)
# generate Col from smoking + hcw + age + comorbid + covidsev + ethnicity
p.hospital <- 1 / (1 + exp(-0.6*smoking -0.3*hcw -1.2*age -0.9*comorbid -1.5*covidsev -0.3*ethnicity))
hospital   <- rbinom(n, 1, p.hospital)
# generate Col from smoking + hcw + age + comorbid + covidsev + hospital
p.testing  <- 1 / (1 + exp(-0.3*smoking -0.9*hcw -0.6*age -0.6*comorbid -0.9*covidsev -1.5*hospital))
testing    <- rbinom(n, 1, p.testing)

dat <- data.frame(age, ethnicity, hcw, smoking, comorbid, covidsev, hospital, testing)

lm1 <- glm(data = dat, covidsev ~ smoking, family = binomial)
lm2 <- glm(data = dat, covidsev ~ smoking + age + ethnicity + hcw, family = binomial)
lm3 <- glm(data = dat, covidsev ~ smoking + hospital + testing, family = binomial)    # adjust for colliders
lm4 <- glm(data = dat, covidsev ~ smoking + comorbid, family = binomial)              # adjust for mediator
lm5 <- glm(data = dat, covidsev ~ smoking + age + ethnicity + hcw + hospital + testing, family = binomial) # confounders + colliders
lm6 <- glm(data = dat, covidsev ~ smoking + hospital + testing + comorbid, family = binomial) # colliders + mediator
lm7 <- glm(data = dat, covidsev ~ ., family = binomial)  # all
lm8 <- glm(data = dat, covidsev ~ smoking + age + ethnicity + hcw + comorbid, family = binomial)
coefXY    <- exp(lm1$coefficients[2])  # save the estimate effect of X on Y from the simple regression
coefXYCon <- exp(lm2$coefficients[2])  # save the estimate effect of X on Y from the multiple regression
coefXYCol <- exp(lm3$coefficients[2])
coefXMedY <- exp(lm4$coefficients[2])
coefXYConCol <- exp(lm5$coefficients[2])
coefXMedYCol <- exp(lm6$coefficients[2])
coefXMedYConCol <- exp(lm7$coefficients[2])
coefXMedYCon <- exp(lm8$coefficients[2])
coefXY.up <- exp(confint(lm1)[2,2])    # finally saving confidence intervals
coefXY.lo <- exp(confint(lm1)[2,1])
coefXYCon.up <- exp(confint(lm2)[2,2])
coefXYCon.lo <- exp(confint(lm2)[2,1])
coefXYCol.up <- exp(confint(lm3)[2,2])
coefXYCol.lo <- exp(confint(lm3)[2,1])
coefXMedY.up <- exp(confint(lm4)[2,2])
coefXMedY.lo <- exp(confint(lm4)[2,1])
coefXYConCol.up <- exp(confint(lm5)[2,2])
coefXYConCol.lo <- exp(confint(lm5)[2,1])
coefXMedYCol.up <- exp(confint(lm6)[2,2])
coefXMedYCol.lo <- exp(confint(lm6)[2,1])
coefXMedYConCol.up <- exp(confint(lm7)[2,2])
coefXMedYConCol.lo <- exp(confint(lm7)[2,1])
coefXMedYCon.up <- exp(confint(lm8)[2,2])
coefXMedYCon.lo <- exp(confint(lm8)[2,1])

model <- c("None", "Confounders", "Colliders", "Mediator",
           "Colliders + confounders", "Colliders + mediator",
           "Colliders + confounders + mediator", "Confounders + mediator")
coef  <- c(coefXY, coefXYCon, coefXYCol, coefXMedY, coefXYConCol, coefXMedYCol,
           coefXMedYConCol, coefXMedYCon)
upper <- c(coefXY.up, coefXYCon.up, coefXYCol.up, coefXMedY.up, coefXYConCol.up,
           coefXMedYCol.up, coefXMedYConCol.up, coefXMedYCon.up)
lower <- c(coefXY.lo, coefXYCon.lo, coefXYCol.lo, coefXMedY.lo, coefXYConCol.lo,
           coefXMedYCol.lo, coefXMedYConCol.lo, coefXMedYCon.lo)

dat2 <- data.frame(model, coef, upper, lower)
dat2$model <- factor(dat2$model, levels=c("None", "Confounders", "Mediator", "Colliders",
                                          "Colliders + confounders", "Colliders + mediator",
                                          "Colliders + confounders + mediator", "Confounders + mediator"))

# Graph showing coefficients (xintercept is exp(0.6) = ~1.8)
plot28 <- ggplot(dat2, aes(x=coef, y=model)) +
  geom_point(shape=19, size = 1.7) +
  geom_vline(xintercept=1.8, linetype='dashed', color='blue') +
  geom_errorbar(aes(xmin=lower, xmax=upper), width=.02, colour = "black") +
  theme_minimal() +
  xlab("X ~ Y Odds Ratio") + ylab("Covariates included")

# View the graphs
ggarrange(plot27, plot28, labels = c("A", "B"),
          ncol = 1, nrow = 2)
