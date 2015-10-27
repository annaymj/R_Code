library("dplyr")
library("reshape2")
library("tidyr")
library("gplots")

signature_VX680 <- read.csv("signature_VX680.csv")
VX_680_cor <- select(signature_VX680,Gene.symbol,cor)
VX_680_cor <- rename(VX_680_cor,VX_680=cor)
View(VX_680_cor)
# nrow(VX_680_cor)
# [1] 92
write.csv(VX_680_cor$Gene.symbol,"Pathway_VX_680_Genes.csv",row.names = FALSE,quote = FALSE)

signature_TAK632 <- read.csv("signature_TAK632.csv")
TAK632_cor <- select(signature_TAK632,Gene.symbol,cor)
TAK632_cor <- rename(TAK632_cor,TAK632=cor)
#View(TAK632_cor)
# nrow(TAK632_cor)
# [1] 65
write.csv(TAK632_cor$Gene.symbol,"Pathway_TAK632_Genes.csv",row.names = FALSE,quote = FALSE)

signature_MMAE <- read.csv("signature_MMAE.csv")
MMAE_cor <- select(signature_MMAE,Gene.symbol,cor)
MMAE_cor <- rename(MMAE_cor,MMAE=cor)
#View(MMAE_cor)
# nrow(MMAE_cor)
# [1] 52
write.csv(MMAE_cor$Gene.symbol,"Pathway_MMAE_Genes.csv",row.names = FALSE,quote = FALSE)

signature_MLN9708 <- read.csv("signature_MLN9708.csv")
MLN9708_cor <- select(signature_MLN9708,Gene.symbol,cor)
MLN9708_cor <- rename(MLN9708_cor,MLN9708=cor)
#View(MLN9708_cor)
# nrow(MLN9708_cor)
# [1] 61
write.csv(MLN9708_cor$Gene.symbol,"Pathway_MLN9708_Genes.csv",row.names = FALSE,quote = FALSE)
signature_MLN8237 <- read.csv("signature_MLN8237.csv")
MLN8237_cor <- select(signature_MLN8237,Gene.symbol,cor)
MLN8237_cor <- rename(MLN8237_cor,MLN8237=cor)
#View(MLN8237_cor)
# nrow(MLN8237_cor)
# [1] 68
write.csv(MLN8237_cor$Gene.symbol,"Pathway_MLN8237_Genes.csv",row.names = FALSE,quote = FALSE)

signature_MLN7243 <- read.csv("signature_MLN7243.csv")
MLN7243_cor <- select(signature_MLN7243,Gene_symbol,Correlation)
MLN7243_cor <- rename(MLN7243_cor,Gene.symbol=Gene_symbol)
MLN7243_cor <- rename(MLN7243_cor,MLN7243=Correlation)
#View(MLN7243_cor)
# nrow(MLN7243_cor)
# [1] 105
write.csv(MLN7243_cor$Gene.symbol,"Pathway_MLN7243_Genes.csv",row.names = FALSE,quote = FALSE)

signature_MLN4924 <- read.csv("signature_MLN4924.csv")
MLN4924_cor <- select(signature_MLN4924,Gene.symbol,cor)
MLN4924_cor <- rename(MLN4924_cor,MLN4924=cor)
#View(MLN4924_cor)
# nrow(MLN4924_cor)
# [1] 69
write.csv(MLN4924_cor$Gene.symbol,"Pathway_MLN4924_Genes.csv",row.names = FALSE,quote = FALSE)

signature_MLN0128 <- read.csv("signature_MLN0128.csv")
MLN0128_cor <- select(signature_MLN0128,Gene.symbol,cor)
MLN0128_cor <- rename(MLN0128_cor,MLN0128=cor)
#View(MLN0128_cor)
# nrow(MLN0128_cor)
# [1] 101
write.csv(MLN0128_cor$Gene.symbol,"Pathway_MLN0128_Genes.csv",row.names = FALSE,quote = FALSE)

signature_CDC7i <- read.csv("signature_CDC7i.csv")
CDC7i_cor <- select(signature_CDC7i,Gene_symbol,Correlation)
CDC7i_cor <- rename(CDC7i_cor,Gene.symbol=Gene_symbol)
CDC7i_cor <- filter(CDC7i_cor,!is.na(Correlation))
CDC7i_cor <- rename(CDC7i_cor,CDC7i=Correlation)
#View(CDC7i_cor)
# nrow(CDC7i_cor)
# [1] 102
write.csv(CDC7i_cor$Gene.symbol,"Pathway_CDC7i_Genes.csv",row.names = FALSE,quote = FALSE)


signature_Erlotinib <- read.csv("ErlotinibGenes.csv")
Erlotinib_cor <- select(signature_Erlotinib,Gene.Symbol,Correlation.coefficient)
Erlotinib_cor <- rename(Erlotinib_cor,Gene.symbol=Gene.Symbol)
Erlotinib_cor <- rename(Erlotinib_cor,Erlotinib=Correlation.coefficient)
#View(Erlotinib_cor)
# nrow(Erlotinib_cor)
# [1] 51
write.csv(Erlotinib_cor$Gene.symbol,"Pathway_Erlotinib_Genes.csv",row.names = FALSE,quote = FALSE)

signature_Sorafenib <- read.csv("SorafenibGenes.csv")
Sorafenib_cor <- select(signature_Sorafenib,Gene.Name,Correlation.coefficient)
Sorafenib_cor <- rename(Sorafenib_cor,Gene.symbol=Gene.Name)
Sorafenib_cor <- rename(Sorafenib_cor,Sorafenib=Correlation.coefficient)
#View(Sorafenib_cor)
#nrow(Sorafenib_cor)
write.csv(Sorafenib_cor$Gene.symbol,"Pathway_Sorafenib_Genes.csv",row.names = FALSE,quote = FALSE)

cor_join1_1 <- full_join(Sorafenib_cor,Erlotinib_cor,by="Gene.symbol")
sort(cor_join1_1$Gene.symbol)
CDC7i_cor$Gene.symbol
MLN4924_cor$Gene.symbol
cor_join1_2 <- full_join(CDC7i_cor,MLN4924_cor,by="Gene.symbol")
sort(cor_join1_2$Gene.symbol)
cor_join1_3 <- full_join(MLN0128_cor,MLN7243_cor,by="Gene.symbol")
cor_join1_4 <- full_join(MMAE_cor,TAK632_cor,by="Gene.symbol")
cor_join1_5 <- full_join(MLN9708_cor,MLN8237_cor,by="Gene.symbol")

cor_join2_1 <- full_join(VX_680_cor,cor_join1_1,by="Gene.symbol")
cor_join2_2 <- full_join(cor_join2_1,cor_join1_2,by="Gene.symbol")
cor_join2_3 <- full_join(cor_join2_2,cor_join1_3,by="Gene.symbol")
cor_join2_4 <- full_join(cor_join2_3,cor_join1_4,by="Gene.symbol")
cor_join2_5 <- full_join(cor_join2_4,cor_join1_5,by="Gene.symbol")

cor_matrix <- cor_join2_5
#View(cor_matrix)
geneSet <- cor_matrix$Gene.symbol

###########################################################################

#get gene name position reference table
reference <- read.csv("HG-U133_Plus_2.na32_unique_genes_v2_corrected.csv")
reference_geneSet <- filter(reference,reference$Gene.Symbol %in% geneSet)
reference_geneSet <- select(reference_geneSet,Probe.Set.ID,Gene.Symbol,Entrez.Gene)
# set geneID and probeSet
geneID <- reference_geneSet$Entrez.Gene
probeSet <- reference_geneSet$Probe.Set.ID
#View(reference_geneSet)

#get the gene and cor in the reference_geneSet
VX680_cor_geneSet <- read.csv("VX680__cor.csv")
VX680_cor_geneSet <- filter(VX680_cor_geneSet,id %in% geneID)
nrow(VX680_cor_geneSet)
#[1] 415
VX680_cor_geneSet <- rename(VX680_cor_geneSet,Entrez.Gene=id)
VX680_cor_geneSet$Entrez.Gene <- as.character(VX680_cor_geneSet$Entrez.Gene)
VX680_cor_geneSet <- inner_join(VX680_cor_geneSet,reference_geneSet,by="Entrez.Gene")
VX680_cor_geneSet <- select(VX680_cor_geneSet,Gene.Symbol,cor)
VX680_cor_geneSet <- rename(VX680_cor_geneSet,VX680=cor)
#View(VX680_cor_geneSet)

TAK632_cor_geneSet <- read.csv("TAK632comb_cor.csv")
TAK632_cor_geneSet <- filter(TAK632_cor_geneSet,id %in% geneID)
nrow(TAK632_cor_geneSet)
#[1] 429
TAK632_cor_geneSet <- rename(TAK632_cor_geneSet, Entrez.Gene=id)
TAK632_cor_geneSet$Entrez.Gene <- as.character(TAK632_cor_geneSet$Entrez.Gene)
TAK632_cor_geneSet <- inner_join(TAK632_cor_geneSet,reference_geneSet,by="Entrez.Gene")
TAK632_cor_geneSet <- select(TAK632_cor_geneSet,Gene.Symbol,cor)
TAK632_cor_geneSet <- rename(TAK632_cor_geneSet,TAK632=cor)
#View(TAK632_cor_geneSet)

CDC7i_cor_geneSet <- read.csv("CDC7i_cor.csv")
CDC7i_cor_geneSet <- filter(CDC7i_cor_geneSet,id %in% probeSet)
nrow(CDC7i_cor_geneSet)
#[1] 431
CDC7i_cor_geneSet <- rename(CDC7i_cor_geneSet,Probe.Set.ID=id)
CDC7i_cor_geneSet <- inner_join(CDC7i_cor_geneSet,reference_geneSet,by="Probe.Set.ID")
CDC7i_cor_geneSet <- select(CDC7i_cor_geneSet,Gene.Symbol,cor)
CDC7i_cor_geneSet <- rename(CDC7i_cor_geneSet,CDC7i=cor)
#View(CDC7i_cor_geneSet)

Sorafenib_cor_geneSet <- read.csv("Sorafenib__cor.csv")
Sorafenib_cor_geneSet <- filter(Sorafenib_cor_geneSet,id_ref %in% probeSet)
nrow(Sorafenib_cor_geneSet)
#[1] 424
Sorafenib_cor_geneSet <- rename(Sorafenib_cor_geneSet,Probe.Set.ID=id_ref)
Sorafenib_cor_geneSet <- inner_join(Sorafenib_cor_geneSet,reference_geneSet,by="Probe.Set.ID")
Sorafenib_cor_geneSet <- select(Sorafenib_cor_geneSet,Gene.Symbol,cor)
Sorafenib_cor_geneSet <- rename(Sorafenib_cor_geneSet,Sorafenib=cor)
#View(Sorafenib_cor_geneSet)

MMAE_cor_geneSet <- read.csv("MMAE__cor.csv")
MMAE_cor_geneSet <- filter(MMAE_cor_geneSet,id %in% geneID)
nrow(MMAE_cor_geneSet)
#[1] 428
MMAE_cor_geneSet <- rename(MMAE_cor_geneSet,Entrez.Gene=id)
MMAE_cor_geneSet$Entrez.Gene <- as.character(MMAE_cor_geneSet$Entrez.Gene)
MMAE_cor_geneSet <- inner_join(MMAE_cor_geneSet,reference_geneSet,by="Entrez.Gene")
MMAE_cor_geneSet <- select(MMAE_cor_geneSet,Gene.Symbol,cor)
MMAE_cor_geneSet <- rename(MMAE_cor_geneSet,MMAE=cor)
#View(MMAE_cor_geneSet)

MLN9708_cor_geneSet <- read.csv("MLN9708__cor.csv")
MLN9708_cor_geneSet <- filter(MLN9708_cor_geneSet,id %in% geneID)
nrow(MLN9708_cor_geneSet)
#[1] 430
MLN9708_cor_geneSet <- rename(MLN9708_cor_geneSet,Entrez.Gene=id)
MLN9708_cor_geneSet$Entrez.Gene <- as.character(MLN9708_cor_geneSet$Entrez.Gene)
MLN9708_cor_geneSet <- inner_join(MLN9708_cor_geneSet,reference_geneSet,by="Entrez.Gene")
MLN9708_cor_geneSet <- select(MLN9708_cor_geneSet,Gene.Symbol,cor)
MLN9708_cor_geneSet <- rename(MLN9708_cor_geneSet,MLN9708=cor)
#View(MLN9708_cor_geneSet)

MLN8237_cor_geneSet <- read.csv("MLN8237__cor.csv")
MLN8237_cor_geneSet <- filter(MLN8237_cor_geneSet,id %in% geneID)
nrow(MLN8237_cor_geneSet)
#[1] 426
MLN8237_cor_geneSet <- rename(MLN8237_cor_geneSet,Entrez.Gene=id)
MLN8237_cor_geneSet$Entrez.Gene <- as.character(MLN8237_cor_geneSet$Entrez.Gene)
MLN8237_cor_geneSet <- inner_join(MLN8237_cor_geneSet,reference_geneSet,by="Entrez.Gene")
MLN8237_cor_geneSet <- select(MLN8237_cor_geneSet,Gene.Symbol,cor)
MLN8237_cor_geneSet <- rename(MLN8237_cor_geneSet,MLN8237=cor)
#View(MLN8237_cor_geneSet)

MLN7243_cor_geneSet <- read.csv("MLN7243_expression_data_reduced_cor.csv")
MLN7243_cor_geneSet <- filter(MLN7243_cor_geneSet,id_ref %in% probeSet)
nrow(MLN7243_cor_geneSet)
#[1] 428
MLN7243_cor_geneSet <- rename(MLN7243_cor_geneSet,Probe.Set.ID=id_ref)
MLN7243_cor_geneSet <- inner_join(MLN7243_cor_geneSet,reference_geneSet,by="Probe.Set.ID")
MLN7243_cor_geneSet <- select(MLN7243_cor_geneSet,Gene.Symbol,cor)
MLN7243_cor_geneSet <- rename(MLN7243_cor_geneSet,MLN7243=cor)
#View(MLN7243_cor_geneSet)


MLN4924_cor_geneSet <- read.csv("MLN4924__cor.csv")
MLN4924_cor_geneSet <- filter(MLN4924_cor_geneSet,id %in% geneID)
nrow(MLN4924_cor_geneSet)
#[1] 427
MLN4924_cor_geneSet <- rename(MLN4924_cor_geneSet,Entrez.Gene=id)
MLN4924_cor_geneSet$Entrez.Gene <- as.character(MLN4924_cor_geneSet$Entrez.Gene)
MLN4924_cor_geneSet <- inner_join(MLN4924_cor_geneSet,reference_geneSet,by="Entrez.Gene")
MLN4924_cor_geneSet <- select(MLN4924_cor_geneSet,Gene.Symbol,cor)
MLN4924_cor_geneSet <- rename(MLN4924_cor_geneSet,MLN4924=cor)
#View(MLN4924_cor_geneSet)

MLN0128_cor_geneSet <- read.csv("MLN0128__cor.csv")
MLN0128_cor_geneSet <- filter(MLN0128_cor_geneSet,id %in% geneID)
nrow(MLN0128_cor_geneSet)
#[1] 428
MLN0128_cor_geneSet <- rename(MLN0128_cor_geneSet,Entrez.Gene=id)
MLN0128_cor_geneSet$Entrez.Gene <- as.character(MLN0128_cor_geneSet$Entrez.Gene)
MLN0128_cor_geneSet <- inner_join(MLN0128_cor_geneSet,reference_geneSet,by="Entrez.Gene")
MLN0128_cor_geneSet <- select(MLN0128_cor_geneSet,Gene.Symbol,cor)
MLN0128_cor_geneSet <- rename(MLN0128_cor_geneSet,MLN0128=cor)
#View(MLN0128_cor_geneSet)

Erlotinib_cor_geneSet <- read.csv("Erlotinib__cor.csv")
Erlotinib_cor_geneSet <- filter(Erlotinib_cor_geneSet,id_ref %in% probeSet)
nrow(Erlotinib_cor_geneSet)
#[1] 424
Erlotinib_cor_geneSet <- rename(Erlotinib_cor_geneSet,Probe.Set.ID=id_ref)
Erlotinib_cor_geneSet <- inner_join(Erlotinib_cor_geneSet,reference_geneSet,by="Probe.Set.ID")
Erlotinib_cor_geneSet <- select(Erlotinib_cor_geneSet,Gene.Symbol,cor)
Erlotinib_cor_geneSet <- rename(Erlotinib_cor_geneSet,Erlotinib=cor)
#View(Erlotinib_cor_geneSet)

geneSet_join1_1 <- full_join(Erlotinib_cor_geneSet,MLN0128_cor_geneSet,by="Gene.Symbol")
geneSet_join1_2 <- full_join(MLN4924_cor_geneSet,MLN7243_cor_geneSet,by="Gene.Symbol")
geneSet_join1_3 <- full_join(MLN8237_cor_geneSet,MLN9708_cor_geneSet,by="Gene.Symbol")
geneSet_join1_4 <- full_join(MMAE_cor_geneSet,Sorafenib_cor_geneSet,by="Gene.Symbol")
geneSet_join1_5 <- full_join(TAK632_cor_geneSet,VX680_cor_geneSet,by="Gene.Symbol")

geneSet_join2_1 <- full_join(CDC7i_cor_geneSet,geneSet_join1_1,by="Gene.Symbol")
geneSet_join2_2 <- full_join(geneSet_join2_1,geneSet_join1_2,by="Gene.Symbol")
geneSet_join2_3 <- full_join(geneSet_join2_2,geneSet_join1_3,by="Gene.Symbol")
geneSet_join2_4 <- full_join(geneSet_join2_3,geneSet_join1_4,by="Gene.Symbol")
geneSet_join2_5 <- full_join(geneSet_join2_4,geneSet_join1_5,by="Gene.Symbol")

geneSet_table <- geneSet_join2_5
nrow(geneSet_table)
#[1] 435
rownames(geneSet_table) <- geneSet_table$Gene.Symbol
geneSet_table <- select(geneSet_table,-Gene.Symbol)
ncol(geneSet_table)
#[1] 11

write.csv(geneSet_table,"gene_compound_table.csv")

m <- geneSet_table

#######################################################
#build heatmap, no normalizing
# heatmap related functions.
###
mean.na.rm <- function(x)
{
  return(mean(x, na.rm=T))
}

sd.na.rm <- function(x)
{
  return(sd(x, na.rm=T))
}

z.score.row <- function(m)
{
  
  avg <- apply(m, 1, mean.na.rm)
  sdv <- apply(m, 1, sd.na.rm)
  
  avg <- matrix(rep(avg, ncol(m)), ncol=ncol(m), byrow=F)
  sdv <- matrix(rep(sdv, ncol(m)), ncol=ncol(m), byrow=F)
  return((m - avg)/sdv)
}

# If you want to use correlation in computing pairwise distance.
# Hierachical clustering
hcluster.corr <- function(x)
{
  library(amap)
  return (hcluster(x, method="correlation", link="average"))
}

# Correlation computation.
dist.corr <- function(x)
{
  d <- cor(x, use="pairwise.complete.obs", method="pearson")
  return(as.dist(1 - d))
}


# Draw a heatmap
as
library(gplots)
#if run locally
pdf("gene_compound_heatmap_update7.pdf", height=10, width=10)
#if run on tmed-cbg png works
#png("test_heatmap.png", units="in", res=500, height=10, width=10)

# In case of using correlation as a distance metric.  If you want to use Euclidian distance, just delete hclustfun=hccluster.corr.

 #m[is.na(m)] <- 0 
gene_compound_ht<-heatmap.2(
          hclustfun=hcluster.corr, 
          as.matrix(m), 
          breaks=seq(-0.5, 0.5, length.out=50), 
          scale = c("none"),
          col=colorRampPalette(c("green", "black","red3"))(49), 
          Rowv=TRUE, Colv=TRUE, 
          cexRow = 0.155,
          labRow=NULL, labCol=NULL, srtCol=30,
          dendrogram="both",          
          key=TRUE, 
          main="Gene and compound heatmap", xlab="Compound", ylab="Gene", 
          na.color="grey",
          trace="none")
dev.off()


######################plot row dendrogram#############################################
pdf("try.pdf", height=15, width=10)
par(cex = 0.15)
plot(gene_compound_ht$rowDendrogram, horiz=TRUE)
dev.off()


###reset par
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
par(resetPar)

#save row dendrogram to tre file
hc <- as.hclust(gene_compound_ht$rowDendrogram)
p <- as.phylo(hc)
write.tree(p, file="RowDendrogram.tre") 

#save col dendrogram to tre file
colTree <- as.hclust(gene_compound_ht$colDendrogram)
col_P <- as.phylo(colTree)
write.tree(col_P,file = "ColDendrogram.tre")

#get gene symbols in order (bottom to top)
geneSymbols <- row.names(m)[gene_compound_ht$rowInd]
geneSymbols
#write geneSymbols to file
write.csv(geneSymbols,"geneSymbols_union.csv",quote = FALSE,row.names = FALSE)

#get genes in each clusters (divided into 7 clusters)
#Group 1
GRP1_beg <- grep("^RGS10$",geneSymbols)
GRP1_beg
GRP1_end <- grep("^PDGFRA$",geneSymbols)
GRP1_end
GRP1 <- geneSymbols[GRP1_beg:GRP1_end]
write.csv(GRP1,"GRP1.csv",quote = FALSE,row.names = FALSE)

#Group 2
GRP2_beg <- grep("^BCL2A1$",geneSymbols)
GRP2_beg
GRP2_end <- grep("^MLANA$",geneSymbols)
GRP2_end
GRP2 <- geneSymbols[GRP2_beg:GRP2_end]
write.csv(GRP2,"GRP2.csv",quote = FALSE,row.names = FALSE)

#Group 3
GRP3_beg <- grep("^CASP1$",geneSymbols)
GRP3_beg
GRP3_end <- grep("^H1F0$",geneSymbols)
GRP3_end
GRP3 <- geneSymbols[GRP3_beg:GRP3_end]
write.csv(GRP3,"GRP3.csv",quote = FALSE,row.names = FALSE)

#Group 4
GRP4_beg <- grep("^MMP1$",geneSymbols)
GRP4_beg
GRP4_end <- grep("^BAMBI$",geneSymbols)
GRP4_end
GRP4 <- geneSymbols[GRP4_beg:GRP4_end]
write.csv(GRP4,"GRP4.csv",quote = FALSE,row.names = FALSE)

#Group 5
GRP5_beg <- grep("^GJA5$",geneSymbols)
GRP5_beg
GRP5_end <- grep("^GNG11$",geneSymbols)
GRP5_end
GRP5 <- geneSymbols[GRP5_beg:GRP5_end]
write.csv(GRP5,"GRP5.csv",quote = FALSE,row.names = FALSE)

#Group 6
GRP6_beg <- grep("^COL4A5$",geneSymbols)
GRP6_beg
GRP6_end <- grep("^CLDN1$",geneSymbols)
GRP6_end
GRP6 <- geneSymbols[GRP6_beg:GRP6_end]
write.csv(GRP6,"GRP6.csv",quote = FALSE,row.names = FALSE)

#Group 7
GRP7_beg <- grep("^BIN1$",geneSymbols)
GRP7_beg
GRP7_end <- grep("^SDC4$",geneSymbols)
GRP7_end
GRP7 <- geneSymbols[GRP7_beg:GRP7_end]
write.csv(GRP7,"GRP7.csv",quote = FALSE,row.names = FALSE)






######################################################################################
#cut tree
# hc <- as.hclust(gene_compound_ht$rowDendrogram)
# #[hc$order] will put the genes in the order of dendrogram
# geneTree <- cutree(hc, h=1.0) [hc$order]
# # this will not change the order in the matrix
# geneTree_originalOrder <- cutree(hc,h=0.6)
# geneTree_originalOrder
# cluster2 <- m[geneTree_originalOrder==2,]
# cluster2
