###################################################################################
####This script is composed of two parts.##########################################
###First part: It reads the core signature genes and pathway signature gene file ##
#### and performs the pathway enrichment analysis using 876 customzied pathways.###
###Second part: It performs 10,000 simulations to check background P values.#######
###################################################################################

###import core signature genes.
core_signature <- read.csv("Core_signature.csv",header=TRUE)
nrow(core_signature)
#[1] 72
#View(core_signature)
core_signature <- core_signature$gene
length(core_signature)
#[1] 72
core_signature_id <- recognize.gene.symbols(core_signature,use.synonyms=TRUE)
length(core_signature_id)
#[1] 73

# for (i in 1:length(core_signature)){
#   ID <- recognize.gene.symbols(core_signature[i],use.synonyms=TRUE)
#   msg <-sprintf("%d,%s",i,ID)
#   print(msg)  
# }
#No.22 returned 2 IDs 
#[1] "8085" "9757"
# > describe.genes(8085)
# gene genesymbol                                gene_name                 genomic      species
# 1 8085      KMT2D lysine (K)-specific methyltransferase 2D chr12:49018974-49060146 Homo sapiens
# > describe.genes(9757)
# gene genesymbol                                gene_name                 genomic      species
# 1 9757      KMT2B lysine (K)-specific methyltransferase 2B chr19:35718018-35738877 Homo sapiens
#delete the KMT2B
# > grep("9757",core_signature_id)
# [1] 73
core_signature_id <- core_signature_id[-73]
length(core_signature_id)
#[1] 72

##import pathway signature genes
pathway_signature <- read.csv("Pathway_signature.csv",header=TRUE)
#View(pathway_signature)
pathway_signature <- pathway_signature$gene
length(pathway_signature)
#[1] 45
pathway_signature_id <- recognize.gene.symbols(pathway_signature,use.synonyms=TRUE)
length(pathway_signature_id)
#[1] 45

############################################################################
#Run pathway enrichment analysis using customized pathway table
customized_map <- read.ontology("Compound X_Thomson Reuters Pathway Maps_EntrezGenes.txt")
# Loading ontology from file...
# 876 gene sets retrieved.
# 0.198 sec elapsed

customized_enr_core <- enrichment(core_signature_id,background.list=minibase_genes,background.exact=TRUE,ontology=customized_map,alpha=1.0,minr=0,fdr=TRUE,ontology.type="gene",sort="none")
write.enrichment(customized_enr_core,file="Unsorted_Core_signature_pathwayEnrichmentResult_BackgroundExt.txt")

#The result is sorted by P value
customized_enr_core <- enrichment(core_signature_id,ontology=customized_map,alpha=1.0,minr=0,fdr=TRUE,ontology.type="gene",sort="none")
write.enrichment(customized_enr_core,file="Unsorted_Core_signature_pathwayEnrichmentResult.txt")

customized_enr_pathway <- enrichment(pathway_signature_id,ontology=customized_map,alpha=1.0,minr=0,fdr=TRUE,ontology.type="gene",sort="none")
write.enrichment(customized_enr_pathway,file="Unsorted_Pathway_signature_pathwayEnrichmentResult.txt")

#check pathway index for top 10 enriched pathways
topPathway <- c("Inhibition of WNT5A-dependent non-canonical pathway in colorectal cancer", 
                "Development_WNT signaling pathway. Part 2",
                "Development_A2A receptor signaling",
                "Development_Melanocyte development and pigmentation",
                "Development_Membrane-bound ESR1: interaction with growth factors signaling",
                "Development_TGF-beta receptor signaling",
                "Transcription_Ligand-dependent activation of the ESR1/SP pathway",
                "Development_Ligand-dependent activation of the ESR1/AP-1 pathway",
                "EGFR family signaling in pancreatic cancer",
                "Development_VEGF signaling and activation")
index_list <- c()
for (i in (1: length(topPathway))){
  index <- grep(topPathway[i],names(customized_map))
  index_list <- c(index_list,index)
}
customized_map[index_list]
index_list