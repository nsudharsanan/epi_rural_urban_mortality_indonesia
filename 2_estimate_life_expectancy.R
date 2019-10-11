#Project: Rural-urban differences in life expectancy in Indonesia
#Purpose: Estimate rural-urban differences in life expectancy
#Last modified: 9 October 2019 by NSud

  #Set the working directory
  setwd( )
  
  #Load my life table function
  source("analysis/code/nikkil_life_table_function.R")
  
  #Load tidyverse
  library(tidyverse)

  #Load the cleaned data (person-time format)
  ifls_pt <- read.csv("build/output/data_for_R.csv")
  
#Set up the bootstrap
  
  #Reshape the data wide to allow for resampling in the bootstrap easier
  ifls_w <- reshape(ifls_pt, idvar = c("id","female","wt","urban"), timevar = "obs_num", v.names = c("age_group","died","exposure"), direction = "wide", sep = ".")
  
  #Set boot parameters
  bsize <- 500
  boot_e30 <- matrix(NA,nrow = bsize,ncol = 4)
  colnames(boot_e30) <- c("M-R", "M-U", "W-R", "W-U")
  n <- length(ifls_w$id)
  
  #Start the bootstrap
  set.seed(12092019)
  for(b in 1:bsize) {
    
    print(b)
    
    #Draw the bootstrap sample
    index <- sample(n, replace = T)
    ifls_w_bs <- ifls_w[index,]
    
    #Reshape back into person-age
    ifls_bs <- reshape(ifls_w_bs, varying = 13:39, v.names = c("age_group","died","exposure"), timevar = "obs_num", idvar = c("id"), direction = "long", sep = ".", new.row.names = 1:1000000)
    
    #Drop person-time obs that don't exist
    ifls_bs <- ifls_bs[!is.na(ifls_bs$age_group) & !is.na(ifls_bs$died) & !is.na(ifls_bs$exposure),]
    
    #Put back into person order
    ifls_bs <- ifls_bs[order(ifls_bs$id, ifls_bs$obs_num),]                     
                       
#Estimate age-specific mortality rates
  
  rates <- matrix(NA,nrow = 12, ncol = 4)
  colnames(rates) <- c("M-R","M-U","W-R","W-U")
  agegroups<-seq(30,85,5)
  
  for(a in 1:12) {
    
    age <- agegroups[a]
    
    for(u in 0:1) {
      
      for(f in 0:1) { 
        
        num <- sum(ifls_bs$died[ifls_bs$age_group == age & ifls_bs$urban == u & ifls_bs$female == f]*ifls_bs$wt[ifls_bs$age_group == age & ifls_bs$urban == u & ifls_bs$female == f])
        den <- sum(ifls_bs$exposure[ifls_bs$age_group == age & ifls_bs$urban == u & ifls_bs$female == f]*ifls_bs$wt[ifls_bs$age_group == age & ifls_bs$urban == u & ifls_bs$female == f])

        rates[a,u+(f*2)+1] <- num/den
        
      }
    }
  }
  
#Estimate LE at 30
  
  e30<-matrix(NA,nrow = 1,ncol = 4)
  colnames(e30) <- c("M-R","M-U","W-R","W-U")
  
  for(u in 0:1) {
    
    for(f in 0:1) { 
      
      e30[u+(f*2)+1] <- nikkil.life.expectancy(nmx = rates[,u+(f*2)+1], ages = agegroups)[1,8]

    }
  }
  
  #Save the bootstrap results
  boot_e30[b,] <- e30

}

#Results for graphing
  
  #Create blank results matrix
  results <- tibble(female = c(0,0,1,1),
                    urban = c("Rural","Urban","Rural","Urban"),
                    estimate = NA,
                    lbound = NA,
                    ubound = NA)
  
  #Fill with results
  results[,3] <- apply(boot_e30,2,mean)
  ci.func <- function(input) {
    output <- quantile(input,c(0.025,0.975))
    return <- output
  }
  results[,4:5] <- t(apply(boot_e30,2,ci.func))
  results$female <- as.factor(results$female)
  results$urban <- as.factor(results$urban)
  
#Graph
  
  ggplot(data = results, mapping = aes(x = female, y = estimate, ymin = lbound, ymax = ubound, color = urban, shape = urban)) +
    geom_point(position = position_dodge(-0.6), size = 5) +
    geom_errorbar(position = position_dodge(-0.6), width = 0.5, size = 0.75) +
    labs(x = NULL, y = "e30") +
    scale_color_manual(values = c("navy","maroon")) +
    scale_x_discrete(labels = c("Men","Women")) +
    scale_y_continuous(limits = c(40,47), breaks = 40:47) +
    theme(text = element_text(size = 18),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.position = c(0.9,0.1),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey95"),
          axis.line = element_line(color = "black"))
    
#Note differences for figure caption
    
  diff_e30[1,1] <- mean(boot_e30[,1] - boot_e30[,2])
  diff_e30[2,1] <- mean(boot_e30[,3] - boot_e30[,4])
  diff_e30[1,2:3] <- ci.func(boot_e30[,1] - boot_e30[,2])
  diff_e30[2,2:3] <- ci.func(boot_e30[,3] - boot_e30[,4])
  differences <- as_tibble(round(diff_e30,digits = 1))
  