library("tidyr")
library("reshape2")
library("reshape")
library("dplyr")
library("ggplot2")
#detach("package:plyr", unload=TRUE) #conficts makes rename not functioning

convertToLog <- function(Raw_Pvalue_list){
  log_Pvalue <- c()
  for (i in (1:length(Raw_Pvalue_list))){
    log_Pvalue <- c(log_Pvalue,-1*(log10(Raw_Pvalue_list[i])))
  }
  return(log_Pvalue)
}


Pvalue_617_backExt <- read.csv("8.14BackgroundExt_617customized_genes_select72.csv")
Pvalue_mutated_backExt <- read.csv("8.14BackgroundExt_Pvalue_result_4400mutated_select72.csv")
Pvalue_minibase_backExt <- read.csv("Pvalue_result_23135minibase_select72_BackgroudExact.csv")

#make sure the order of all pathway names are the same
setdiff(Pvalue_mutated_backExt$V1,Pvalue_617_backExt$V1)
pathwayNames_InOrder <- Pvalue_617_backExt$V1
pathwayNames_InOrder <- as.data.frame(pathwayNames_InOrder)

#set pathway names as row names, and end up with 876 X 10,000 P value matrix
row.names(Pvalue_617_backExt) <- Pvalue_617_backExt$V1
Pvalue_617_backExt <- select(Pvalue_617_backExt,-V1)
ncol(Pvalue_617_backExt)

#for Pvalue_mutated table
row.names(Pvalue_mutated_backExt) <- Pvalue_mutated_backExt$V1
Pvalue_mutated_backExt <- select(Pvalue_mutated_backExt,-V1)
ncol(Pvalue_mutated_backExt)

#for Pvalue_minibase table
row.names(Pvalue_minibase_backExt) <- Pvalue_minibase_backExt$V1
Pvalue_minibase_backExt <- select(Pvalue_minibase_backExt,-V1)
ncol(Pvalue_minibase_backExt)

###use ggplot to build overlap histograms of P value distribution
for (i in (1:nrow(Pvalue_617_backExt))){
  #data: x, y, z from Pvalue_617, Pvalue_minibase,Pvalue_mutated respectively
  x <- as.numeric(Pvalue_617_backExt[i,])
  x <- convertToLog(x)
  x <- as.data.frame(x)
  
  y <- as.numeric(Pvalue_mutated_backExt[i,])
  y <- convertToLog(y)
  y <- as.data.frame(y)
  
  z <- as.numeric(Pvalue_minibase_backExt[i,])
  z <- convertToLog(z)
  z <- as.data.frame(z)
  
  #Now, combine your two dataframes into one.  First make a new column in each.
  x$background <- '617'
  y$background <- 'mutated'
  z$background <- 'minibase'
  
  x <- rename(x, pvalue = x)
  y <- rename(y, pvalue = y)
  z <- rename(z, pvalue = z)
  
  #and combine into your new data frame vegLengths
  backgrounds <- rbind(x,y,z)
  pdf(paste("Pvalue_distribution_PathwayNumber_bin_0.2_alpha0.4_",i,".pdf",sep=""))
  #png(paste("Pvalue_distribution_PathwayNumber_bin_0.2_",i,".png",sep=""))
  
  #draw overlap histogram using ggplot
  myplot <- ggplot(backgrounds, aes(pvalue, fill = background)) + 
    geom_histogram(binwidth=0.2, alpha=0.4,position="identity") + 
    labs(x="-log10(Pvalue)")
  #+ggtitle("Distribution of P values from 100 random sampled genes with different gene background")
  print(myplot)
  dev.off()
}


