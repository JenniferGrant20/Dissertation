
# Student No. 2005070
# Dissertation App Code

# Contents:
# User interface    line 21
# Server            line 109
# Run               line 367

################################################################################

# Loading useful libraries
library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(ggpubr)

################################################################################

# User interface
ui <- fluidPage(
  
  theme = bs_theme(preset = "zephyr"),
  
  # Title
  titlePanel("Dissertation Web App"),
  
  navset_card_underline(
    
    nav_panel(
      h4("Confounder"), br(),
      "This simulation creates a three-variable system consisting of an exposure 
      (X), outcome (Y), and confounder. Linear regression analyses are conducted
      using the models 'X ~ Y' and 'X ~ Y + Confounder'. The graphs illustrate
      these confounder-biased and confounder-adjusted results.", br(), br(),
      sidebarLayout(
        sidebarPanel(
          h4("Input data"),
          sliderInput("betaConY", "Effect of the confounder on Y", -1, 1, 0.5, step=0.05),
          sliderInput("betaXY2", "Effect of X on Y", -1, 1, 0, step=0.05)
        ),
        mainPanel(
          plotOutput("confounderplot", width="900px", height="500px")
        ))
      ),
    
    nav_panel(
      h4("Mediator"), br(),
      "This simulation creates a three-variable system consisting of an exposure 
      (X), outcome (Y), and mediator. Linear regression analyses are conducted
      using the models 'X ~ Y' and 'X ~ Y + Mediator'. The graphs illustrate
      these mediator-biased and mediator-adjusted results.", br(), br(),
      sidebarLayout(
        sidebarPanel(
          h4("Input data"),
          sliderInput("betaMedY", "Effect of the mediator on Y", -1, 1, 0.5, step=0.05),
          sliderInput("betaXY1", "Effect of X on Y", -1, 1, 0, step=0.05)
        ),
        mainPanel(
          plotOutput("mediatorplot", width="900px", height="500px"),
        ))
      ),
    
    nav_panel(
      h4("Collider"), br(),
      "This simulation creates a three-variable system consisting of an exposure 
      (X), outcome (Y), and collider. Linear regression analyses are conducted
      using the models 'X ~ Y + Collider' and 'X ~ Y'. The graphs illustrate
      these collider-biased and collider-adjusted results.", br(), br(),
      sidebarLayout(
        sidebarPanel(
          h4("Input data"),
          sliderInput("betaYCol", "Effect of Y on the collider", -1, 1, 0.5, step=0.05),
          sliderInput("betaXY3", "Effect of X on Y", -1, 1, 0, step=0.05)
        ),
        mainPanel(
          plotOutput("colliderplot", width="900px", height="500px")
        ))
      ),
    
    nav_panel(
      h4("Combined Example"), br(),
      "This simulation creates a five-variable system consisting of an exposure 
      (X), outcome (Y), a mediator, a confounder and a collider. Linear regression
      analyses are conducted using models which adjust for all combinations of 
      covariates. These results are displayed in a graph below, illustrating the
      estimated effect of X on Y when adjusting for different covariates.", br(), br(),
      sidebarLayout(
        sidebarPanel(
          h4("Input data"),
          sliderInput("betaXY4", "Effect of X on Y", -1, 1, 0.2, step=0.05),
          sliderInput("betaXCol1", "Effect of X on the collider", -1, 1, 0.4, step=0.05),
          sliderInput("betaYCol1", "Effect of Y on the collider", -1, 1, 0.5, step=0.05),
          sliderInput("betaConX1", "Effect of the confounder on X", -1, 1, -0.6, step=0.05),
          sliderInput("betaConY1", "Effect of the confounder on Y", -1, 1, 0.3, step=0.05),
          sliderInput("betaXMed1", "Effect of X on the mediator", -1, 1, 0.7, step=0.05),
          sliderInput("betaMedY1", "Effect of the mediator on Y", -1, 1, 0.5, step=0.05)
        ),
        mainPanel(
          plotOutput("allplot", width="900px", height="500px")
        ))
      )
    )
  )

################################################################################

# Server
server <- function(input, output) {
  
  # Mediator Simulation
  sim_Med <- function(betaXMed, betaMedY, betaXY){
    n <- 1000
    error <- 1     # error / random noise scaling
    X   <- error*rnorm(n)                             # generate X from standard normal distribution
    Med <- betaXMed*X              + error*rnorm(n) # generate Mediator from X
    Y   <- betaXY*X + betaMedY*Med + error*rnorm(n)   # generate Y from X and Mediator
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
  
  betaXMed <- seq(-1.0, 1.0, by = 0.1) 
  med1 <- reactive({input$betaMedY})
  med2 <- reactive({input$betaXY1})
  resultsMed <- reactive({lapply(betaXMed, sim_Med, betaMedY=med1(), betaXY=med2())})
  Med_df <- reactive({bind_rows(resultsMed())})

  output$mediatorplot <- renderPlot({
    
    plot1 <- ggplot(Med_df(), aes(x=betaXMed, y=coefXY)) +
      geom_point(shape=19, size = 1.7)+
      geom_hline(yintercept=input$betaXY1, linetype='dashed', color='blue') +
      geom_errorbar(aes(ymin=coefXY.lo, ymax=coefXY.up), width=.02, colour = "black") +
      theme_minimal() + 
      xlab("X ~ Mediator Coefficient") + ylab("X ~ Y Coefficient") +
      ggtitle("Graphs showing Linear Regression Coefficients", 
              subtitle = "Model excluding a mediator") +
      ylim(input$betaXY1-1.25, input$betaXY1+1.25) +
      theme(text = element_text(size=18))
    
    plot2 <- ggplot(Med_df(), aes(x=betaXMed, y=coefXMedY)) +
      geom_point(shape=19, size = 1.7)+
      geom_hline(yintercept=input$betaXY1, linetype='dashed', color='blue') +
      geom_errorbar(aes(ymin=coefXMedY.lo, ymax=coefXMedY.up), width=.02, colour = "black") +
      theme_minimal() + 
      xlab("X ~ Mediator Coefficient") + ylab("X ~ Y Coefficient incl. Mediator") +
      ggtitle(" ", subtitle = "Model including a mediator") +
      ylim(input$betaXY1-1.25, input$betaXY1+1.25) +
      theme(text = element_text(size=18))
    
    ggarrange(plot1, plot2, nrow=1, ncol=2)
    
  })
  
  
  # Confounder Simulation
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
  
  betaConX <- seq(-1.0, 1.0, by = 0.1)
  con1 <- reactive({input$betaConY})
  con2 <- reactive({input$betaXY2})
  resultsCon <- reactive({lapply(betaConX, sim_Con, betaConY=con1(), betaXY=con2())})
  Con_df <- reactive({bind_rows(resultsCon())})
  
  output$confounderplot <- renderPlot({
    
    plot3 <- ggplot(Con_df(), aes(x=betaConX, y=coefXY)) +
      geom_point(shape=19, size = 1.7)+
      geom_hline(yintercept=input$betaXY2, linetype='dashed', color='blue') +
      geom_errorbar(aes(ymin=coefXY.lo, ymax=coefXY.up), width=.02, colour = "black") +
      theme_minimal() + 
      xlab("Confounder ~ X Coefficient") + ylab("X ~ Y Coefficient") +
      ggtitle("Graphs showing Linear Regression Coefficients", 
              subtitle = "Model excluding a confounder") +
      ylim(input$betaXY2-1.25, input$betaXY2+1.25) +
      theme(text = element_text(size=18))
    
    plot4 <- ggplot(Con_df(), aes(x=betaConX, y=coefXYCon)) +
      geom_point(shape=19, size = 1.7)+
      geom_hline(yintercept=input$betaXY2, linetype='dashed', color='blue') +
      geom_errorbar(aes(ymin=coefXYCon.lo, ymax=coefXYCon.up), width=.02, colour = "black") +
      theme_minimal() + 
      xlab("Confounder ~ X Coefficient") + ylab("X ~ Y Coefficient incl. Confounder") +
      ggtitle(" ", subtitle = "Model including a confounder") +
      ylim(input$betaXY2-1.25, input$betaXY2+1.25) +
      theme(text = element_text(size=18))
    
    ggarrange(plot3, plot4, nrow=1, ncol=2)
    
  })
  
  
  # Collider simulation
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
  
  betaXCol <- seq(-1.0, 1.0, by = 0.1)
  col1 <- reactive({input$betaYCol})
  col2 <- reactive({input$betaXY3})
  resultsCol <- reactive({lapply(betaXCol, sim_Col, betaYCol=col1(), betaXY=col2())})
  Col_df <- reactive({bind_rows(resultsCol())})
  
  output$colliderplot <- renderPlot({
    
    plot5 <- ggplot(Col_df(), aes(x=betaXCol, y=coefXYCol)) +
      geom_point(shape=19, size = 1.7)+
      geom_hline(yintercept=input$betaXY3, linetype='dashed', color='blue') +
      geom_errorbar(aes(ymin=coefXYCol.lo, ymax=coefXYCol.up), width=.02, colour = "black") +
      theme_minimal() + 
      xlab("X ~ Collider Coefficient") + ylab("X ~ Y Coefficient incl. Collider") +
      ggtitle("Graphs showing Linear Regression Coefficients", 
              subtitle = "Model including a collider") +
      ylim(input$betaXY3-1.25, input$betaXY3+1.25) +
      theme(text = element_text(size=18))
    
    plot6 <- ggplot(Col_df(), aes(x=betaXCol, y=coefXY)) +
      geom_point(shape=19, size = 1.7)+
      geom_hline(yintercept=input$betaXY3, linetype='dashed', color='blue') +
      geom_errorbar(aes(ymin=coefXY.lo, ymax=coefXY.up), width=.02, colour = "black") +
      theme_minimal() + 
      xlab("X ~ Collider Coefficient") + ylab("X ~ Y Coefficient") +
      ggtitle(" ", subtitle = "Model excluding a collider") +
      ylim(input$betaXY3-1.25, input$betaXY3+1.25) +
      theme(text = element_text(size=18))
    
    ggarrange(plot5, plot6, nrow=1, ncol=2)
  })
    
    
    # Combined Example
    sim_all <- function(betaXY, betaXCol, betaYCol, betaConX, betaConY, betaXMed, betaMedY) {
      
      n <- 1000
      error <- 1
      
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
      
      return(dat)
        
    }
    
    dat <- reactive(sim_all(betaXY = input$betaXY4, 
                            betaXCol = input$betaXCol1, 
                            betaYCol = input$betaYCol1,
                            betaConX = input$betaConX1,
                            betaConY = input$betaConY1,
                            betaXMed = input$betaXMed1,
                            betaMedY = input$betaMedY1))
      
    output$allplot <- renderPlot({
      
      ggplot(dat(), aes(x=coef, y=model)) +
        geom_point(shape=19, size = 1.7) +
        geom_vline(xintercept=input$betaXY4, linetype='dashed', color='blue') +
        geom_errorbar(aes(xmin=lower, xmax=upper), width=.02, colour = "black") +
        theme_minimal() + theme(text = element_text(size=18)) +
        xlab("X ~ Y Coefficients") + ylab("Covariates included") +
        ggtitle("Graph of linear regression coefficients",
                subtitle = "Estimated effect of X on Y in different models")
    })
  
}

################################################################################

# Run app
shinyApp(ui = ui, server = server)

# Update app
# library(rsconnect)
# deployApp()