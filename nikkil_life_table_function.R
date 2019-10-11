# Write life expectancy function for this paper

nikkil.life.expectancy <- function(nmx, ages) {
  
  life_table <- matrix(nrow = length(ages), ncol = 8)
  rownames(life_table) <- ages
  colnames(life_table) <- c("mx","qx","px","lx","dx","Lx","Tx","ex")
  
  #mx
  life_table[,1] <- nmx
  
  #qx
  life_table[,2] <- (5*life_table[,1]) / (1 + 2.5*life_table[,1])
  life_table[length(ages),2] <- 1
  
  #px
  life_table[,3] <- 1 - life_table[,2]
  
  #lx
  life_table[1,4] <- 1
  for(i in 2:length(ages)) {
    
    j = i - 1
    
    life_table[i,4] <- life_table[j,4]*life_table[j,3]
  }
  
  #dx
  life_table[,5] <- life_table[,4]*life_table[,2]
  
  #Lx
  life_table[,6] <- (5*life_table[,4]*life_table[,3]) + (2.5*life_table[,5])
  life_table[length(ages),6] <- life_table[length(ages),5] / life_table[length(ages),1]
  
  #Tx
  life_table[length(ages),7] <- life_table[length(ages),6]
  for(i in (length(ages)-1):1) {
    
    j <- i + 1
    
    life_table[i,7] <- life_table[j,7] + life_table[i,6]
  }
  
  #ex
  life_table[,8] <- life_table[,7] / life_table[,4]
  
  return(life_table)
    
}