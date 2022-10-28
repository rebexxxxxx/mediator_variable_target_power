
require(MASS)

#--- IMPORT USER-SPECIFIED VALUES --------------------------------------------#

powReps <- input$replicationInput
mcmcReps <- input$mcdrawInput
seed <- input$seedInput
conf <- input$ciInput
input_method <- input$input_method
TarPow <- input$targetpowerInput
Nlow <- input$NlowInput
Nhigh <- input$NhighInput
Nsteps <- input$NstepsInput

#check for user-input value errors:
# CHECK: Is the number of replications > 5 and an integer?
if (powReps < 5 | !abs(powReps - round(powReps)) < .Machine$double.eps ^ 0.5) {
  stop("\"# of Replications\" must be an integer greater than 5. Please change this value.")
}

# CHECK: Is the number of MC replications > 5 and an integer?
if (mcmcReps < 5 | !abs(mcmcReps - round(mcmcReps)) < .Machine$double.eps ^ 0.5) {
  stop("\"Monte Carlo Draws per Rep\" must be an integer greater than 5. Please change this value.")
}

# CHECK: Is the seed > 5 and an integer?
if (seed < 5 | !abs(seed - round(seed)) < .Machine$double.eps ^ 0.5) {
  stop("\"Seed\" must be an integer greater than 5. Please change this value.")
}

# CHECK: Is the confidence level (%) between 0 and 100?
if (conf < 0 | conf > 100) {
  stop("\"Confidence Level (%)\" must be a number between 0 and 100. Please change this value.")
}

calc_3mediator_power <- function(powReps = 10, mcmcReps = 10, seed = 2, 
                                 conf=95, N =100,
                                 input_type = c("sc", "corr"),
                                 stdcoef=matrix(rep(0,ntimes=6)),
                                 corr = matrix(rep(0,ntimes=6)),
                                 SD=matrix(rep(0,ntimes=5))){
  

    if(input_type=="sc"){
      a1 <- as.numeric(stdcoef[1])
      a2 <- as.numeric(stdcoef[2])
      a3 <- as.numeric(stdcoef[3])
      b1 <- as.numeric(stdcoef[4])
      b2 <- as.numeric(stdcoef[5])
      b3 <- as.numeric(stdcoef[6])
      cprime <- as.numeric(stdcoef[7])
      #variables defined here
      #https://github.com/schoam4/mc_power_med/blob/master/code/three_parallel_mediators_stdcoef_ui.R
      #core32 = rM1M2 in GUI for package
      rm1m2 <- as.numeric(stdcoef[8])
      rm3m1 <- as.numeric(stdcoef[9])
      rm3m2 <- as.numeric(stdcoef[10])
      
      
      if(abs(a1)> .999 | abs(a2)> .999 | abs(a3) > .999 |
         abs(b1)> .999 | abs(b2)> .999 | abs(b3) > .999 | 
         abs(cprime)> .999 | abs(rm1m2) > .999 | abs(rm3m1) > .999 |
         abs(rm3m2) > .999
      ) {
        stop("One or more standardized coefficients are out of range (greater than 1 or less than -1)
         check your inputs and try again")
      }
      corMat <- matrix(rep(0,25),nrow=5, ncol=5)
      corMat <- diag(5)
      corMat[2,1] <- corMat[1,2] <- a1
      corMat[3,1] <- corMat[1,3] <- a2
      corMat[4,1] <- corMat[1,4] <- a3
      corMat[2,3] <- corMat[3,2] <- rm1m2
      corMat[2,4] <- corMat[4,2] <- rm3m1
      corMat[3,4] <- corMat[4,2] <- rm3m2
      
      corMat[5,1] <- corMat[1,5] <- cprime + a1*b1 + a2*b2 + a3*b3
      
      corMat[2,5] <- corMat[5,2] <- a1*cprime + b1 + b2*rm1m2 + b3*rm3m1
      corMat[3,5] <- corMat[5,3] <- a2*cprime + b2 + b1*rm1m2 + b3*rm3m1
      corMat[4,5] <- corMat[5,4] <- a3*cprime + b3 + b2*rm1m2 + b1*rm3m1
    }else{
      cor21 <- as.numeric(corr[1])
      cor31 <- as.numeric(corr[2])
      cor32 <- as.numeric(corr[3])
      cor41 <- as.numeric(corr[4])
      cor42 <- as.numeric(corr[5])
      cor43 <- as.numeric(corr[6])
      cor51 <- as.numeric(corr[7])
      cor52 <- as.numeric(corr[8])
      cor53 <- as.numeric(corr[9])
      cor54 <- as.numeric(corr[10])
      
      
      # Create correlation / covariance matrix
      corMat <- matrix(rep(0,25),nrow=5, ncol=5)
      corMat <- diag(5)
      corMat[2,1] <- corMat[1,2] <- cor21
      corMat[3,1] <- corMat[1,3] <- cor31
      corMat[2,3] <- corMat[3,2] <- cor32
      corMat[4,1] <- corMat[1,4] <- cor41
      corMat[2,4] <- corMat[4,2] <- cor42
      corMat[3,4] <- corMat[4,3] <- cor43
      corMat[5,1] <- corMat[1,5] <- cor51
      corMat[5,2] <- corMat[2,5] <- cor52
      corMat[5,3] <- corMat[3,5] <- cor53
      corMat[5,4] <- corMat[4,5] <- cor54
      
    }
    
    
    SDX <- as.numeric(SD[1])
    SDM1 <- as.numeric(SD[2])
    SDM2 <- as.numeric(SD[3])
    SDM3 <- as.numeric(SD[4])
    SDY <- as.numeric(SD[5])
    # Get diagonal matrix of SDs
    SDs <- diag(c(SDX, SDM1, SDM2, SDM3, SDY))
    
    
    # Convert to covariance matrix
    covMat <- SDs%*%corMat%*%SDs
    
  
  
  
  # Create function for 1 rep
  powRep <- function(seed = 1234, Ns = N, covMatp = corMat){
    set.seed(seed)
    require(MASS)
    
    # incProgress(1/powReps)
    
    dat <- mvrnorm(Ns, mu = c(0,0,0,0,0), Sigma = covMatp)
    # Run regressions
    m1 <- lm(dat[,2] ~ dat[,1])
    m2 <- lm(dat[,3] ~ dat[,1])
    m3 <- lm(dat[,4] ~ dat[,1])
    m4 <- lm(dat[,5] ~ dat[,2] + dat[,3] + dat[,4] + dat[,1])
    
    # Output parameter estimates and standard errors
    a1 <- rnorm(mcmcReps, coef(m1)[2], sqrt(vcov(m1)[2,2]))
    a2 <- rnorm(mcmcReps, coef(m2)[2], sqrt(vcov(m2)[2,2]))
    a3 <- rnorm(mcmcReps, coef(m3)[2], sqrt(vcov(m3)[2,2]))
    b1 <- rnorm(mcmcReps, coef(m4)[2], sqrt(vcov(m4)[2,2]))
    b2 <- rnorm(mcmcReps, coef(m4)[3], sqrt(vcov(m4)[3,3]))
    b3 <- rnorm(mcmcReps, coef(m4)[4], sqrt(vcov(m4)[4,4]))
    
    a1b1 <- a1*b1
    a2b2 <- a2*b2
    a3b3 <- a3*b3
    diff12 <- a1*b1 - a2*b2
    diff13 <- a1*b1 - a3*b3
    diff23 <- a2*b2 - a3*b3
    
    # Calculate confidence intervals
    low <- (1 - (conf / 100)) / 2
    upp <- ((1 - conf / 100) / 2) + (conf / 100)
    LL1 <- quantile(a1b1, low)
    UL1 <- quantile(a1b1, upp)
    LL2 <- quantile(a2b2, low)
    UL2 <- quantile(a2b2, upp)        
    LL3 <- quantile(a3b3, low)
    UL3 <- quantile(a3b3, upp)        
    LLd12 <- quantile(diff12, low)
    ULd12 <- quantile(diff12, upp)  
    LLd13 <- quantile(diff13, low)
    ULd13 <- quantile(diff13, upp)  
    LLd23 <- quantile(diff23, low)
    ULd23 <- quantile(diff23, upp)  
    
    # Is rep significant?
    c(LL1*UL1 > 0, LL2*UL2 > 0, LL3*UL3 > 0, LLd12*ULd12 > 0, LLd13*ULd13 > 0, LLd23*ULd23 > 0)
  }
  
  # Create vector of sample sizes
  Nused <- seq(Nlow, Nhigh, Nsteps)
  
  # Divide powReps among sample sizes; Create input vector for simulation
  Nvec <- rep(Nused, round(powReps/length(Nused)))
  
  set.seed(seed)
  # Run power analysis and logistic regression
  pow <- mapply(FUN = powRep, Ns = Nvec, seed = sample(1:50000, length(Nvec)),
                SIMPLIFY = FALSE)
  pow <- data.frame(Nvec, do.call("rbind", pow))
  names(pow) <- c("N", "a1b1", "a2b2", "a3b3", "a1a2", "a1a3", "a2a3")
  
  out <- list()
  for(i in 2:7) {
    #i <- 2
    try(mod <- glm(pow[, i] ~ pow[, "N"], family = binomial(link = "logit")), silent = TRUE)
    
    predProb <- function(newdat, glmObj, alpha = 0.05) {
      slps <- as.numeric(coef(glmObj))
      logi <- sum(newdat * slps)
      predVal <- as.matrix(newdat)
      se <- sqrt(t(predVal) %*% vcov(glmObj) %*% predVal)
      critVal <- qnorm(1 - alpha/2)
      logi <- c(logi - critVal * se, logi, logi + critVal * se)
      logi[logi > 500] <- 500
      logi[logi < -500] <- -500
      pp <- exp(logi)/(1 + exp(logi))
      if(round(pp[2], 6) == 1) pp[3] <- 1
      if(round(pp[2], 6) == 0) pp[1] <- 0
      return(pp)
    }
    
    powVal <- cbind(1, Nused)
    
    # List of power estimates with prediction intervals
    res <- apply(powVal, 1, predProb, mod)
    res <- cbind(powVal[, 2], t(as.matrix(res)))     
    colnames(res) <- c("N", "LL", "Power", "UL")
    out[[i-1]] <- res
  }
  df <- data.frame("Parameter" = rep(c("a1b1", "a2b2", "a3b3", "a1a2", "a1a3", "a2a3"),
                                     each = length(Nused)),
                   do.call("rbind", out))
  return(df)
  
}


  #convert 
  if (input_method == "corr") {
    # Import model input values
    cor21_low <- as.numeric(input$cor21_low)
    cor31_low <- as.numeric(input$cor31_low)
    cor32_low <- as.numeric(input$cor32_low)
    cor41_low <- as.numeric(input$cor41_low)
    cor42_low <- as.numeric(input$cor42_low)
    cor43_low <- as.numeric(input$cor43_low)
    cor51_low <- as.numeric(input$cor51_low)
    cor52_low <- as.numeric(input$cor52_low)
    cor53_low <- as.numeric(input$cor53_low)
    cor54_low <- as.numeric(input$cor54_low)
    
    cor21_high <- as.numeric(input$cor21_high)
    cor31_high <- as.numeric(input$cor31_high)
    cor32_high <- as.numeric(input$cor32_high)
    cor41_high <- as.numeric(input$cor41_high)
    cor42_high <- as.numeric(input$cor42_high)
    cor43_high <- as.numeric(input$cor43_high)
    cor51_high <- as.numeric(input$cor51_high)
    cor52_high <- as.numeric(input$cor52_high)
    cor53_high <- as.numeric(input$cor53_high)
    cor54_high <- as.numeric(input$cor54_high)
    
    #these variables must be in the correct order for the power calculation function
    #to use the correct inputs for the correct variables
    #if you change the order of these variables you must change the order of 
    #the variables in the main function above
    input_data <- data.frame(a1=runif(1,min= cor21_low,max = cor21_high),
                             a2=runif(1,min= cor31_low,max = cor31_high),
                             a3=runif(1,min= cor41_low,max = cor41_high),
                             b1=runif(1,min= cor52_low,max = cor52_high),
                             b2=runif(1,min= cor53_low,max = cor53_high),
                             b3=runif(1,min= cor54_low,max = cor54_high),
                             cprime=runif(1,min= cor51_low,max = cor51_high), #affect of x on y
                             rm1m2=runif(1,min= cor32_low,max = cor32_high), #affect of mediator var 1 and mv2)
                             rm3m1=runif(1,min= cor42_low,max = cor42_high), #affect of mediator var 3 on mv1
                             rm3m2=runif(1,min= cor43_low,max = cor43_high)) #affect of mediator var 3 on mv2
    
    
  } else {
    a1_low <- as.numeric(input$STa1_low)
    a1_high <- as.numeric(input$STa1_high)
    a2_low <- as.numeric(input$STa2_low)
    a2_high <- as.numeric(input$STa2_high)
    a3_low <- as.numeric(input$STa3_low)
    a3_high <- as.numeric(input$STa3_high)
    b1_low <- as.numeric(input$STb1_low)
    b1_high <- as.numeric(input$STb1_high)
    b2_low <- as.numeric(input$STb2_low)
    b2_high <- as.numeric(input$STb2_high)
    b3_low <- as.numeric(input$STb3_low)
    b3_high <- as.numeric(input$STb3_high)
    rm1m2_low <- as.numeric(input$rm1m2_low)
    rm1m2_high <- as.numeric(input$rm1m2_high)
    rm1m3_low <- as.numeric(input$rm1m3_low)
    rm1m3_high <- as.numeric(input$rm1m3_high)
    rm2m3_low <- as.numeric(input$rm2m3_low)
    rm2m3_high <- as.numeric(input$rm2m3_high)
    cprime_low <- as.numeric(input$STc_low)
    cprime_high <- as.numeric(input$STc_high)
    
    #create data from inputs
    input_data <- data.frame(a1=runif(1,min= a1_low,max = a1_high), #x to M1
                             a2=runif(1,min= a2_low,max = a2_high), #x to M2
                             a3=runif(1,min= a3_low,max = a3_high), #x to M3
                             b1=runif(1,min= b1_low,max = b1_high), 
                             b2=runif(1,min= b2_low,max = b2_high),
                             b3=runif(1,min= b3_low,max = b3_high),
                             cprime=runif(1,min=cprime_low,max=cprime_high), #x to y
                             rm1m2=runif(1,min=rm1m2_low,max=rm1m2_high), #x to y
                             rm1m3=runif(1,min=rm1m3_low,max=rm1m3_high),
                             rm2m3=runif(1,min=rm2m3_low,max=rm2m3_high))
  }
  
Nused <- seq(Nlow, Nhigh, Nsteps)
power_estimates_3mediators <- data.frame(N = rep(0,6*length(Nused)), LL = rep(0,6*length(Nused)),
                                         Power = rep(0,6*length(Nused)), UL = rep(0,6*length(Nused)))

#for(i in 1:numIterations){
#  withProgress(message = 'Running Replications', value = 0, {
#define standard deviation
SD <- c(1,1,1,1,1)
#define standard coefficients
SC <- input_data[1,]
corr <- input_data[1,]
if(input_method == "sc"){
  power_estimates_3mediators <- calc_3mediator_power(powReps=powReps, mcmcReps = mcmcReps, seed = seed,
                                                     conf = conf, N = samplesize, input_type=input_method, stdcoef = SC, SD = SD)
}else{
  power_estimates_3mediators <- calc_3mediator_power(powReps=powReps, mcmcReps = mcmcReps, seed = seed,
                                                     conf = conf, N = samplesize, input_type=input_method, corr = corr, SD = SD)}


power_estimates_3mediators

