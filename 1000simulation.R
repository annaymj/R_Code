##########################################################################
##This script reads the gene symbols from panel gene list, and performs###
## 10,000 times simulation using customized pathway table (876 pathways)##
##########################################################################
#get official gene list
Official_gene<-read.table("Official_gene.txt",header=TRUE)
nrow(Official_gene)
#[1] 618
geneList<-Official_gene$Official
gene_id <- recognize.gene.symbols(geneList,use.synonyms=TRUE)
#Recognized 617 gene symbols of 618 . Unrecognized: 610
geneList[593]
#[1] YWHAEP5

recognize.gene.symbols(geneList[593],use.synonyms=TRUE)
#Recognized 0 gene symbols of 1 . Unrecognized: 610, 
#character(0)

gene_id_part1<-recognize.gene.symbols(geneList[1:592],use.synonyms=TRUE)
gene_id_part2<-recognize.gene.symbols(geneList[594:618],use.synonyms=TRUE)
gene_ID <- c(gene_id_part1,gene_id_part2)
length(gene_ID)
#[1] 617

##################################################################################################
minibase_genes <- get.genes(filter=list(species="Homo sapiens"))
minibase_genes <- minibase_genes$gene
length(minibase_genes)
############################################################################
#generate n random numbers from N
#SelectGene_num = 100
SelectGene_num = 72
TotalGene_num = 617
Sample_time = 10000

#create matrix to store random gene IDs  10,000 sampling times
random_ID <- matrix(,nrow = SelectGene_num,ncol = Sample_time) 
#function to get 100 random gene IDs from 619 genes without replacement
getRandomNum_ID <- function(TotalGene_num,SelectGene_num){
  randomGene_num <- sample(TotalGene_num, SelectGene_num, replace=FALSE)
  return (gene_ID[randomGene_num])
}

#generate random gene IDs 10,000 times
for (i in 1:Sample_time){
  random_ID[,i] = getRandomNum_ID(TotalGene_num,SelectGene_num)
}
#View(random_ID)
############################################################################

#Run pathway enrichment analysis using customized pathway table
customized_map <- read.ontology("Compound X_Thomson Reuters Pathway Maps_EntrezGenes.txt")

# Loading ontology from file...
# 876 gene sets retrieved.
# 0.198 sec elapsed

#create an empty matrix to fill in log P values for 876 customized pathways, and 1000 sampling times, in the order of pathway map names order generated from read.ontology.
Number_pathway <- length(customized_map)
result_matrix <- matrix(,nrow=Number_pathway,ncol=(Sample_time +1))

#fill in the first column as pathway names 
result_matrix[,1]=names(customized_map)

#fill in all log P values in 1000 (or defined) sampling times
for (i in 1:Sample_time){
  customized_enr <- enrichment(random_ID[,i],ontology=customized_map,background.list=minibase_genes,background.exact=TRUE,alpha=1.0,minr=0,fdr=TRUE,ontology.type="gene", sort="none")
  #log_pvalue <- (-log10(customized_enr$pvalue))
  pvalue <- ((customized_enr$pvalue))
  #fill in (i+1) column in result_matrix, since the 1st column is pathway name
  result_matrix[,i+1] <- pvalue
}

View(result_matrix)
write.csv(result_matrix,"8.14BackgroundExt_617customized_genes_select72.csv",row.names=FALSE)
