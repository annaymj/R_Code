library("tidyr")
library("reshape2")
library("reshape")
library("dplyr")
library("ggplot2")
#detach("package:plyr", unload=TRUE) #conficts makes rename not functioning

Pvalue_617_select72_backExt <- read.csv("8.14BackgroundExt_617customized_genes_select72.csv")
Pvalue_4400_select72_backExt <- read.csv("8.14BackgroundExt_Pvalue_result_4400mutated_select72.csv")
pathwayNames_InOrder <- Pvalue_617_select72_backExt$V1

Result_core72_backgroundExt <- read.table("Unsorted_Core_signature_pathwayEnrichmentResult_backgroundExt.txt",sep="\t",header=TRUE)

####change Pvalue_table, Result_table, and fileName accordingly
# Pvalue_table <- Pvalue_617_select72_backExt
# Result_table <- Result_core72_backgroundExt
# fileName <- "8.16backExt_617_select72_observed_empirical_Pvalue.txt"

Pvalue_table <- Pvalue_4400_select72_backExt
Result_table <- Result_core72_backgroundExt
fileName <- "8.16backExt_4400_select72_observed_empirical_Pvalue.txt"

#write the ontology.term and empirical Pvalues
file<-file(fileName)
Firstline <- paste("Ontology.term","\t","Observed_Pvalue","\t","empirical_Pvalue")
write(Firstline,file=fileName,append=TRUE)


#####set first column as pathway names
row.names(Pvalue_table) <- Pvalue_table$V1
Pvalue_table <- select(Pvalue_table,-V1)
ncol(Pvalue_table)



#Calculate empirical Pvalue for each pathway
for (i in (1:nrow(Pvalue_table))){
  
  x <- as.numeric(Pvalue_table[i,])
  
  #empirical cutoff is the number of simulation point fall on the right side of(bigger than) the P value obtained from pathway Enrichment
  observed_Pvalue <- Result_table[i,]$pvalue
  
  ############if saved in raw Pvalue
  Pvalue <- x
  count <- length(Pvalue[Pvalue <= observed_Pvalue])
  empirical_Pvalue <- (count+1)/(ncol(Pvalue_table)+1)
  
  newline <- paste(row.names(Pvalue_table)[i],"\t",observed_Pvalue,"\t",empirical_Pvalue)
  write(newline,file=fileName,append=TRUE)
  
}
close(file)

#########################################################################################################
##Draw scatterplot
###from 617 select 72####################################################################################
empirical_Pvalue_select72From617 <- read.table("8.16backExt_617_select72_observed_empirical_Pvalue.txt",header = TRUE,sep="\t")

View(empirical_Pvalue_select72From617)

x <- empirical_Pvalue_select72From617$Observed_Pvalue
y <- empirical_Pvalue_select72From617$empirical_Pvalue

plot(x,y,col=densCols(cbind(x,y)), type="p", 
     main="Observed and empirical Pvalue using 617 panel as sampling background",pch = 20, xlab="Observed Pvalue",ylab="Empirical Pvalue")
abline(0,1,col="red",lwd=2,lty=1) 

plot(x,y,log="xy", col=densCols(cbind(x,y)), type="p", 
     main="Observed and empirical Pvalue using 617 panel as sampling background \n(log scale)",pch = 20, xlab="Observed Pvalue(log)",ylab="Empirical Pvalue(log)")
abline(0,1,col="red",lwd=2,lty=1) 

###from 4400 select 72###################################################################################
empirical_Pvalue_select72From4400 <- read.table("8.16backExt_4400_select72_observed_empirical_Pvalue.txt",header = TRUE,sep="\t")

View(empirical_Pvalue_select72From4400)

x <- empirical_Pvalue_select72From4400$Observed_Pvalue
y <- empirical_Pvalue_select72From4400$empirical_Pvalue

plot(x,y,col=densCols(cbind(x,y)), type="p", 
     main="Observed and empirical Pvalue using 4400 mutated genes as sampling background",pch = 20, xlab="Observed Pvalue",ylab="Empirical Pvalue")
abline(0,1,col="red",lwd=2,lty=1) 

plot(x,y,log="xy", col=densCols(cbind(x,y)), type="p", 
     main="Observed and empirical Pvalue using 4400 mutated genes as sampling background \n(log scale)",pch = 20, xlab="Observed Pvalue(log)",ylab="Empirical Pvalue(log)")
abline(0,1,col="red",lwd=2,lty=1) 

