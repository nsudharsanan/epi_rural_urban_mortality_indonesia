#Project: Rural-urban difference in mortality in Indo
#Purpose: Do the BMI, BP, and smoking decomposition
#Last edited: 10 October 2019 by NSud

  #Set the working directory
  setwd( )

  #Load my life table function
  source("analysis/code/nikkil_life_table_function.R")

  #Load MICE function
  library(mice)

  #Load the cleaned data (person-time format)
  long <- read.csv("build/output/data_for_R.csv")
  
  #Set all factor variables to be a factor
  long$urban <- as.factor(long$urban)
  long$marit <- as.factor(long$marit)
  long$job <- as.factor(long$job)
  long$schooling_abs <- as.factor(long$schooling_abs)
  long$wealth_quintile <- as.factor(long$wealth_quintile)
  long$ever_smoker <- as.factor(long$ever_smoker)
  long$province <- as.factor(long$province)

  #Make a wide dataframe
  wide <- reshape(long, idvar = c("id"), timevar = "obs_num", v.names = c("age_group","died","exposure"), direction = "wide", sep = ".")

#Bootstrap parameters

  bsize <- 5
  bs_nc <- bs_cfl_bmi <- bs_cfl_bp <- bs_cfl_smoking <- bs_cfl_bp_bmi <- matrix(NA,nrow = bsize,ncol = 4)
  colnames(bs_nc) <- colnames(bs_cfl_bmi) <- colnames(bs_cfl_bp) <- colnames(bs_cfl_smoking) <- colnames(bs_cfl_bp_bmi) <- c("M-R", "M-U", "W-R", "W-U")
  n <- length(wide$id)
  agegroups <- seq(30,85,5)

#Monte Carlo parameters (size was set based on visual inspection of convergence in MC estimates)

	mcsize <- 5
	mc_nc <- mc_cfl_bmi <- mc_cfl_bp <- mc_cfl_smoking <- mc_cfl_bp_bmi <- matrix(NA,nrow = mcsize,ncol = 4)
	colnames(mc_nc) <- colnames(mc_cfl_bmi) <- colnames(mc_cfl_bp) <- colnames(mc_cfl_smoking) <- colnames(mc_cfl_bp_bmi) <- c("M-R", "M-U", "W-R", "W-U")

#Set imputation parameters

	#Imputation method for each column
	imp_meth <- rep("",39)
	imp_meth[9] <- "logreg" # Ever smoker
	imp_meth[10] <- imp_meth[11] <- "pmm" # BMI and sysbp

	#Predictor matrix (which variables will be used in the imputation)
	predictor_matrix <- matrix(0,nrow = 39, ncol = 39)
	colnames(predictor_matrix) <- rownames(predictor_matrix) <- names(wide)
	predictor_matrix["ever_smoker",c("age_group.1","marit","schooling_abs","job","wealth_quintile","province","bmi_w3","sysbp")] <- 1
	predictor_matrix["bmi_w3",c("age_group.1","marit","schooling_abs","job","wealth_quintile","province","ever_smoker","sysbp")] <- 1
	predictor_matrix["sysbp",c("age_group.1","marit","schooling_abs","job","wealth_quintile","province","bmi_w3","ever_smoker")] <- 1

#Start the bootstrap
for(b in 1:bsize) {
  
  print(b)
  
#Draw the bootstrap sample and impute
  
  #Draw bootstrap sample
  index <- sample(n, replace = T)
  wide_bs <- wide[index,]
  
  #Do the imputation
  impute <- mice(wide_bs, method = imp_meth, predictorMatrix = predictor_matrix, m = 1)
 
  #Extract the imputed data
  wide_imp <- complete(impute,1)

  #Make long versions of the imputed data
  long_imp <- reshape(wide_imp, varying = 13:39, v.names = c("age_group","died","exposure"), timevar = "obs_num", idvar = c("id"), direction = "long", sep = ".", new.row.names = 1:2000000)
  
    #Drop person-time obs that don't exist
    long_imp<- long_imp[!is.na(long_imp$age_group) & !is.na(long_imp$died) & !is.na(long_imp$exposure),]
    
    #Sort the data back to the correct order
    long_imp <- long_imp[order(long_imp$id),]
     
#Mediator models (done on the person-level since this does not vary within ppl over time)

  #BP
  bp_model <- glm(sysbp ~ age_group.1*female*urban  + marit + job + schooling_abs + wealth_quintile + province,
                  data = wide_imp, 
                  family = gaussian,
                  weights = wt)
  
  #BMI
  bmi_model <- glm(bmi_w3 ~ age_group.1*female*urban  + marit + job + schooling_abs + wealth_quintile + province,
                   data = wide_imp, 
                   family = gaussian,
                   weights = wt)
  
  #Ever smoking
  ever_smoker_model <- glm(ever_smoker ~ age_group.1*female*urban + marit + job + schooling_abs + wealth_quintile + province,
                           data = wide_imp, 
                           family = binomial,
                           weights = wt)
  
#Save standard devations for BP and BMI
	
  #BP
	wide_imp$bp_sd <- NA
	wide_imp$bp_sd[wide_imp$female == 0 & wide_imp$urban == 0] <- sd(bp_model$residuals[wide_imp$female == 0 & wide_imp$urban == 0])
	wide_imp$bp_sd[wide_imp$female == 0 & wide_imp$urban == 1] <- sd(bp_model$residuals[wide_imp$female == 0 & wide_imp$urban == 1])
	wide_imp$bp_sd[wide_imp$female == 1 & wide_imp$urban == 0] <- sd(bp_model$residuals[wide_imp$female == 1 & wide_imp$urban == 0])
	wide_imp$bp_sd[wide_imp$female == 1 & wide_imp$urban == 1] <- sd(bp_model$residuals[wide_imp$female == 1 & wide_imp$urban == 1])
	
	#BMI
	wide_imp$bmi_sd <- NA
	wide_imp$bmi_sd[wide_imp$female == 0 & wide_imp$urban == 0] <- sd(bmi_model$residuals[wide_imp$female == 0 & wide_imp$urban == 0])
	wide_imp$bmi_sd[wide_imp$female == 0 & wide_imp$urban == 1] <- sd(bmi_model$residuals[wide_imp$female == 0 & wide_imp$urban == 1])
	wide_imp$bmi_sd[wide_imp$female == 1 & wide_imp$urban == 0] <- sd(bmi_model$residuals[wide_imp$female == 1 & wide_imp$urban == 0])
	wide_imp$bmi_sd[wide_imp$female == 1 & wide_imp$urban == 1] <- sd(bmi_model$residuals[wide_imp$female == 1 & wide_imp$urban == 1])
	
#Outcome model
  
  outcome_model <- glm(died ~ bmi_w3*female*urban + sysbp*female*urban + ever_smoker*female*urban + age_group*female*urban + marit + job + schooling_abs + wealth_quintile + province + offset(log(exposure)),
                       data = long_imp, 
                       family = poisson,
                       weights = wt)
  
#G-formula estimates: Start the MC loops
for (m in 1:mcsize) {
  
  print(paste(b,m))
  
  #Draw the mc data
  nc_wide <- wide_imp
   
  #Natural course
  
    #Draw the mediators
    nc_wide$sysbp <- rnorm(n = length(nc_wide$sysbp), mean = predict(bp_model,nc_wide,type = "response"), sd = nc_wide$bp_sd)
    nc_wide$bmi_w3 <- rnorm(n = length(nc_wide$bmi_w3), mean = predict(bmi_model,nc_wide,type = "response"), sd = nc_wide$bmi_sd)
    nc_wide$ever_smoker <- rbinom(n = length(nc_wide$ever_smoker), size = 1, prob = predict(ever_smoker_model,nc_wide,type = "response"))
    nc_wide$ever_smoker <- as.factor(nc_wide$ever_smoker)  
    
    #Reshape to long to predict the outcome
    nc_long <- reshape(nc_wide, varying = 13:39, v.names = c("age_group","died","exposure"), timevar = "obs_num", idvar = c("id"), direction = "long", sep = ".", new.row.names = 1:1000000)
   
      #Drop person-time obs that don't exist
      nc_long <- nc_long[!is.na(nc_long$age_group) & !is.na(nc_long$died) & !is.na(nc_long$exposure),]
      
      #Sort the data back to the correct order
      nc_long <- nc_long[order(nc_long$id),]
      
    #Predict mortality (predicting the rates directly)
    nc_long$died <- predict(outcome_model, nc_long, type = "response")
    
    #Form age-specific rates
    rates_mr <- rates_mu <- rates_wr <- rates_wu <- matrix(nrow = 12, ncol = 1)
    for(a in 1:length(agegroups)) {
      age = agegroups[a]
      rates_mr[a,1] <- sum(nc_long$died[nc_long$age_group == age & nc_long$female == 0 & nc_long$urban == 0]*nc_long$wt[nc_long$age_group == age & nc_long$female == 0 & nc_long$urban == 0]) / sum(nc_long$exposure[nc_long$age_group == age & nc_long$female == 0 & nc_long$urban == 0]*nc_long$wt[nc_long$age_group == age & nc_long$female == 0 & nc_long$urban == 0])
      rates_mu[a,1] <- sum(nc_long$died[nc_long$age_group == age & nc_long$female == 0 & nc_long$urban == 1]*nc_long$wt[nc_long$age_group == age & nc_long$female == 0 & nc_long$urban == 1]) / sum(nc_long$exposure[nc_long$age_group == age & nc_long$female == 0 & nc_long$urban == 1]*nc_long$wt[nc_long$age_group == age & nc_long$female == 0 & nc_long$urban == 1])
      rates_wr[a,1] <- sum(nc_long$died[nc_long$age_group == age & nc_long$female == 1 & nc_long$urban == 0]*nc_long$wt[nc_long$age_group == age & nc_long$female == 1 & nc_long$urban == 0]) / sum(nc_long$exposure[nc_long$age_group == age & nc_long$female == 1 & nc_long$urban == 0]*nc_long$wt[nc_long$age_group == age & nc_long$female == 1 & nc_long$urban == 0])
      rates_wu[a,1] <- sum(nc_long$died[nc_long$age_group == age & nc_long$female == 1 & nc_long$urban == 1]*nc_long$wt[nc_long$age_group == age & nc_long$female == 1 & nc_long$urban == 1]) / sum(nc_long$exposure[nc_long$age_group == age & nc_long$female == 1 & nc_long$urban == 1]*nc_long$wt[nc_long$age_group == age & nc_long$female == 1 & nc_long$urban == 1])
    }

    #Save life expectancies
    mc_nc[m,1] <- nikkil.life.expectancy(rates_mr,agegroups)[1,8]
    mc_nc[m,2] <- nikkil.life.expectancy(rates_mu,agegroups)[1,8]
    mc_nc[m,3] <- nikkil.life.expectancy(rates_wr,agegroups)[1,8]
    mc_nc[m,4] <- nikkil.life.expectancy(rates_wu,agegroups)[1,8]
     
  #Counterfactual 1: Just match BP
    
    #Start with the mc data (for just the urban pop)
    cfl_wide <- nc_wide[nc_wide$urban == 1,]

    #Draw counterfactual mediators
    
      #Set them to look like the rural population to estimate CFL distribution
      cfl_wide$urban <- as.factor(0)
      cfl_wide$bp_sd[cfl_wide$female == 0] <- sd(bp_model$residuals[wide_imp$female == 0 & wide_imp$urban == 0])
      cfl_wide$bp_sd[cfl_wide$female == 1] <- sd(bp_model$residuals[wide_imp$female == 1 & wide_imp$urban == 0])
      
      #Draw counterfactual sysbp
      cfl_wide$sysbp <- rnorm(n = length(cfl_wide$sysbp), mean = predict(bp_model,cfl_wide,type = "response"), sd = cfl_wide$bp_sd)
  
    #Reshape to long to predict the outcome
    cfl_long <- reshape(cfl_wide, varying = 13:39, v.names = c("age_group","died","exposure"), timevar = "obs_num", idvar = c("id"), direction = "long", sep = ".", new.row.names = 1:1000000)

      #Drop person-time obs that don't exist
      cfl_long <- cfl_long[!is.na(cfl_long$age_group) & !is.na(cfl_long$died) & !is.na(cfl_long$exposure),]

      #Sort the data back to the correct order
      cfl_long <- cfl_long[order(cfl_long$id),]

    #Predict mortality
      
      #Set them back to the urban pop
      cfl_long$urban <- as.factor(1)
      
      #Predict
      cfl_long$died <- predict(outcome_model, cfl_long, type = "response")

    #Form age-specific rates
    rates_mu <- rates_wu <- matrix(nrow = 12, ncol = 1)
    for(a in 1:length(agegroups)) {
      age = agegroups[a]
      rates_mu[a,1] <- sum(cfl_long$died[cfl_long$age_group == age & cfl_long$female == 0]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 0]) / sum(cfl_long$exposure[cfl_long$age_group == age & cfl_long$female == 0]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 0])
      rates_wu[a,1] <- sum(cfl_long$died[cfl_long$age_group == age & cfl_long$female == 1]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 1]) / sum(cfl_long$exposure[cfl_long$age_group == age & cfl_long$female == 1]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 1])
    }
    
    #Save life expectancies
    mc_cfl_bp[m,2] <- nikkil.life.expectancy(rates_mu,agegroups)[1,8]
    mc_cfl_bp[m,4] <- nikkil.life.expectancy(rates_wu,agegroups)[1,8]
   
  #Counterfactual 2: Just BMI
    
    #Start with the mc data (for just the urban pop)
    cfl_wide <- nc_wide[nc_wide$urban == 1,]
    
    #Draw counterfactual mediators
    
      #Set them to look like the rural population to estimate CFL distribution
      cfl_wide$urban <- as.factor(0)
      cfl_wide$bmi_sd[cfl_wide$female == 0] <- sd(bmi_model$residuals[wide_imp$female == 0 & wide_imp$urban == 0])
      cfl_wide$bmi_sd[cfl_wide$female == 1] <- sd(bmi_model$residuals[wide_imp$female == 1 & wide_imp$urban == 0])
      
      #Draw counterfactual bmi
      cfl_wide$bmi_w3 <- rnorm(n = length(cfl_wide$bmi_w3), mean = predict(bmi_model,cfl_wide,type = "response"), sd = cfl_wide$bmi_sd)
      
    #Reshape to long to predict the outcome
    cfl_long <- reshape(cfl_wide, varying = 13:39, v.names = c("age_group","died","exposure"), timevar = "obs_num", idvar = c("id"), direction = "long", sep = ".", new.row.names = 1:1000000)
    
      #Drop person-time obs that don't exist
      cfl_long <- cfl_long[!is.na(cfl_long$age_group) & !is.na(cfl_long$died) & !is.na(cfl_long$exposure),]
      
      #Sort the data back to the correct order
      cfl_long <- cfl_long[order(cfl_long$id),]
      
    #Predict mortality
    
      #Set them back to the urban pop
      cfl_long$urban <- as.factor(1)
      
      #Predict
      cfl_long$died <- predict(outcome_model, cfl_long, type = "response")
    
    #Form age-specific rates
    rates_mu <- rates_wu <- matrix(nrow = 12, ncol = 1)
    for(a in 1:length(agegroups)) {
      age = agegroups[a]
      rates_mu[a,1] <- sum(cfl_long$died[cfl_long$age_group == age & cfl_long$female == 0]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 0]) / sum(cfl_long$exposure[cfl_long$age_group == age & cfl_long$female == 0]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 0])
      rates_wu[a,1] <- sum(cfl_long$died[cfl_long$age_group == age & cfl_long$female == 1]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 1]) / sum(cfl_long$exposure[cfl_long$age_group == age & cfl_long$female == 1]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 1])
    }
    
    #Save life expectancies
    mc_cfl_bmi[m,2] <- nikkil.life.expectancy(rates_mu,agegroups)[1,8]
    mc_cfl_bmi[m,4] <- nikkil.life.expectancy(rates_wu,agegroups)[1,8]
    
  #Counterfactual 3: Just smoking
    
    #Start with the mc data (for just the urban pop)
    cfl_wide <- nc_wide[nc_wide$urban == 1,]
    
    #Draw counterfactual mediators
      
      #Set them to look like the rural population to estimate CFL distribution
      cfl_wide$urban <- as.factor(0)

      #Draw counterfactual smoking
      cfl_wide$ever_smoker <- as.factor(rbinom(n = length(cfl_wide$ever_smoker), size = 1, prob = predict(ever_smoker_model, cfl_wide,type = "response")))
      
    #Reshape to long to predict the outcome
    cfl_long <- reshape(cfl_wide, varying = 13:39, v.names = c("age_group","died","exposure"), timevar = "obs_num", idvar = c("id"), direction = "long", sep = ".", new.row.names = 1:1000000)
    
      #Drop person-time obs that don't exist
      cfl_long <- cfl_long[!is.na(cfl_long$age_group) & !is.na(cfl_long$died) & !is.na(cfl_long$exposure),]
      
      #Sort the data back to the correct order
      cfl_long <- cfl_long[order(cfl_long$id),]
      
    #Predict mortality
    
      #Set them back to the urban pop
      cfl_long$urban <- as.factor(1)
      
      #Predict
      cfl_long$died <- predict(outcome_model, cfl_long, type = "response")
      
    #Form age-specific rates
    rates_mu <- rates_wu <- matrix(nrow = 12, ncol = 1)
    for(a in 1:length(agegroups)) {
      age = agegroups[a]
      rates_mu[a,1] <- sum(cfl_long$died[cfl_long$age_group == age & cfl_long$female == 0]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 0]) / sum(cfl_long$exposure[cfl_long$age_group == age & cfl_long$female == 0]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 0])
      rates_wu[a,1] <- sum(cfl_long$died[cfl_long$age_group == age & cfl_long$female == 1]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 1]) / sum(cfl_long$exposure[cfl_long$age_group == age & cfl_long$female == 1]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 1])
    }
    
    #Save life expectancies
    mc_cfl_smoking[m,2] <- nikkil.life.expectancy(rates_mu,agegroups)[1,8]
    mc_cfl_smoking[m,4] <- nikkil.life.expectancy(rates_wu,agegroups)[1,8]
    
  #Counterfactual 4: BP and BMI
    
    #Start with the mc data (for just the urban pop)
    cfl_wide <- nc_wide[nc_wide$urban == 1,]
    
    #Draw counterfactual mediators
    
      #Set them to look like the rural population to estimate CFL distribution
      cfl_wide$urban <- as.factor(0)
      cfl_wide$bp_sd[cfl_wide$female == 0] <- sd(bp_model$residuals[wide_imp$female == 0 & wide_imp$urban == 0])
      cfl_wide$bp_sd[cfl_wide$female == 1] <- sd(bp_model$residuals[wide_imp$female == 1 & wide_imp$urban == 0])
      cfl_wide$bmi_sd[cfl_wide$female == 0] <- sd(bmi_model$residuals[wide_imp$female == 0 & wide_imp$urban == 0])
      cfl_wide$bmi_sd[cfl_wide$female == 1] <- sd(bmi_model$residuals[wide_imp$female == 1 & wide_imp$urban == 0])
    
      #Draw counterfactual sysbp and bmi
      cfl_wide$sysbp <- rnorm(n = length(cfl_wide$sysbp), mean = predict(bp_model,cfl_wide,type = "response"), sd = cfl_wide$bp_sd)
      cfl_wide$bmi_w3 <- rnorm(n = length(cfl_wide$bmi_w3), mean = predict(bmi_model,cfl_wide,type = "response"), sd = cfl_wide$bmi_sd)
      
    #Reshape to long to predict the outcome
    cfl_long <- reshape(cfl_wide, varying = 13:39, v.names = c("age_group","died","exposure"), timevar = "obs_num", idvar = c("id"), direction = "long", sep = ".", new.row.names = 1:1000000)
    
      #Drop person-time obs that don't exist
      cfl_long <- cfl_long[!is.na(cfl_long$age_group) & !is.na(cfl_long$died) & !is.na(cfl_long$exposure),]
      
      #Sort the data back to the correct order
      cfl_long <- cfl_long[order(cfl_long$id),]
      
    #Predict mortality
    
      #Set them back to the urban pop
      cfl_long$urban <- as.factor(1)
      
      #Predict
      cfl_long$died <- predict(outcome_model, cfl_long, type = "response")
      
    #Form age-specific rates
    rates_mu <- rates_wu <- matrix(nrow = 12, ncol = 1)
    for(a in 1:length(agegroups)) {
      age = agegroups[a]
      rates_mu[a,1] <- sum(cfl_long$died[cfl_long$age_group == age & cfl_long$female == 0]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 0]) / sum(cfl_long$exposure[cfl_long$age_group == age & cfl_long$female == 0]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 0])
      rates_wu[a,1] <- sum(cfl_long$died[cfl_long$age_group == age & cfl_long$female == 1]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 1]) / sum(cfl_long$exposure[cfl_long$age_group == age & cfl_long$female == 1]*cfl_long$wt[cfl_long$age_group == age & cfl_long$female == 1])
    }
    
    #Save life expectancies
    mc_cfl_bp_bmi[m,2] <- nikkil.life.expectancy(rates_mu,agegroups)[1,8]
    mc_cfl_bp_bmi[m,4] <- nikkil.life.expectancy(rates_wu,agegroups)[1,8]
    
}

  bs_nc[b,] <- apply(mc_nc,2,mean)
  bs_cfl_bp[b,] <- apply(mc_cfl_bp,2,mean)
  bs_cfl_bmi[b,] <- apply(mc_cfl_bmi,2,mean)
  bs_cfl_smoking[b,] <- apply(mc_cfl_smoking,2,mean)
  bs_cfl_bp_bmi[b,] <- apply(mc_cfl_bp_bmi,2,mean)

}

#Graph results
	
	#Load tidyverse (this needs to be done here because it interferes with MICE due to the "complete" function)
	library(tidyverse)
	library(gridExtra)
	
	#Create blank results matrix
	results <- tibble(scenario = rep(c("NC","Equalize BMI","Equalize BP","Equalize BMI and BP","Equalize smoking"),2),
	                  female = rep(0:1, each = 5),
	                  estimate = NA,
	                  lbound = NA,
	                  ubound = NA)
	
	#Fill the results matrix
	results[1,3] <- mean(bs_nc[,1] - bs_nc[,2])
	results[1,4:5] <- quantile(bs_nc[,1] - bs_nc[,2], probs = c(0.025,0.975))
	results[2,3] <- mean(bs_nc[,1] - bs_cfl_bmi[,2])
	results[2,4:5] <- quantile(bs_nc[,1] - bs_cfl_bmi[,2], probs = c(0.025,0.975))
	results[3,3] <- mean(bs_nc[,1] - bs_cfl_bp[,2])
	results[3,4:5] <- quantile(bs_nc[,1] - bs_cfl_bp[,2], probs = c(0.025,0.975))
	results[4,3] <- mean(bs_nc[,1] - bs_cfl_bp_bmi[,2])
	results[4,4:5] <- quantile(bs_nc[,1] - bs_cfl_bp_bmi[,2], probs = c(0.025,0.975))
	results[5,3] <- mean(bs_nc[,1] - bs_cfl_smoking[,2])
	results[5,4:5] <- quantile(bs_nc[,1] - bs_cfl_smoking[,2], probs = c(0.025,0.975))
	
	results[6,3] <- mean(bs_nc[,3] - bs_nc[,4])
	results[6,4:5] <- quantile(bs_nc[,3] - bs_nc[,4], probs = c(0.025,0.975))
	results[7,3] <- mean(bs_nc[,3] - bs_cfl_bmi[,4])
	results[7,4:5] <- quantile(bs_nc[,3] - bs_cfl_bmi[,4], probs = c(0.025,0.975))
	results[8,3] <- mean(bs_nc[,3] - bs_cfl_bp[,4])
	results[8,4:5] <- quantile(bs_nc[,3] - bs_cfl_bp[,4], probs = c(0.025,0.975))
	results[9,3] <- mean(bs_nc[,3] - bs_cfl_bp_bmi[,4])
	results[9,4:5] <- quantile(bs_nc[,3] - bs_cfl_bp_bmi[,4], probs = c(0.025,0.975))
	results[10,3] <- mean(bs_nc[,3] - bs_cfl_smoking[,4])
	results[10,4:5] <- quantile(bs_nc[,3] - bs_cfl_smoking[,4], probs = c(0.025,0.975))
	
	results$scenario <- factor(x = results$scenario, levels = c("NC","Equalize BMI","Equalize BP","Equalize BMI and BP","Equalize smoking"))
	 
	#Graph
	men <- ggplot(data = filter(results, female == 0), mapping = aes(x = scenario, y = estimate, ymin = lbound, ymax = ubound, label = round(estimate, digits = 1), color = scenario)) +
	  geom_errorbar(width = 0.5, size = 0.5) +
	  geom_hline(yintercept = round(results[1,3][[1]],digits = 1), linetype = "dotted") +
	  geom_text(size = 5) +
	  geom_label() +
	  labs(x = NULL, y = "Rural-urban e30 difference", title = "Men") +
	  scale_x_discrete(labels = c("NC","Equalize \nBMI", "Equalize \nBP", "Equalize \nBMI and BP","Equalize \nSmoking")) +
	  theme(text = element_text(size = 16),
	        plot.title = element_text(hjust = 0.5),
	        panel.background = element_blank(),
	        panel.grid.major = element_line(color = "grey95"),
	        axis.line = element_line(color = "black"),
	        legend.position = "none")
	
	women <- ggplot(data = filter(results, female == 1), mapping = aes(x = scenario, y = estimate, ymin = lbound, ymax = ubound, label = round(estimate, digits = 1), color = scenario)) +
	  geom_errorbar(width = 0.5, size = 0.5) +
	  geom_hline(yintercept = round(results[6,3][[1]],digits = 1), linetype = "dotted") +
	  geom_text(size = 5) +
	  geom_label() +
	  labs(x = NULL, y = "Rural-urban e30 difference", title = "Women") +
	  scale_x_discrete(labels = c("NC","Equalize \nBMI", "Equalize \nBP", "Equalize \nBMI and BP","Equalize \nSmoking")) +
	  theme(text = element_text(size = 16),
	        plot.title = element_text(hjust = 0.5),
	        panel.background = element_blank(),
	        panel.grid.major = element_line(color = "grey95"),
	        axis.line = element_line(color = "black"),
	        legend.position = "none")
	
	grid.arrange(men, women, nrow = 2)
	