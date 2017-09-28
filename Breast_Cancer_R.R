# Data Exploration and Analysis of Breast Cancer Diagnositic Data from UCI Machine Learning Repository
# Author: Mengjie Yu

setwd("~/Dropbox/2017Spring/StatisticsMSReport/Data")

# load library
library(dplyr)
library(plyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(d3heatmap)
library(knitr)
library(corrplot)
library(gridExtra)
library(ggfortify)
library(caret)
library(class)
library(ROCR)
library(MLmetrics)
library(e1071)

# load data
Breast_Cancer_table <- read.csv("data.csv",header = TRUE)
Breast_Cancer_table <- data.frame(Breast_Cancer_table)

# last column is empty, get rid of last column
Breast_Cancer_table <- Breast_Cancer_table[,-ncol(Breast_Cancer_table)]

# > nrow(Breast_Cancer_table)
# [1] 569
# > ncol(Breast_Cancer_table)
# [1] 32

# subset data into mean, se and worst
Breast_Cancer_mean <- Breast_Cancer_table[,2:12]
Breast_Cancer_SE <- Breast_Cancer_table[,c(2,13:22)]
Breast_Cancer_Worst <- Breast_Cancer_table[,c(2,23:32)]

# visualiza distribution of Breast_cancer_mean data

# smaller mean groups
mean_smallValue <- ggplot(Breast_Cancer_mean,aes(x="Smoothness",y=smoothness_mean)) + geom_boxplot(aes(colour=diagnosis)) +
  geom_boxplot(aes(x="Compactness",y=compactness_mean,colour=diagnosis)) +
  geom_boxplot(aes(x="Concavity",y=concavity_mean,colour=diagnosis)) +
  geom_boxplot(aes(x="Symmetry",y=symmetry_mean,colour=diagnosis)) +
  geom_boxplot(aes(x="Fractal dimension",y=fractal_dimension_mean,colour=diagnosis)) +
  xlab("Features") + ylab("Mean")

# larger mean groups
mean_largeValue <-ggplot(Breast_Cancer_mean,aes(x="Radius", y=radius_mean)) + geom_boxplot(aes(colour=diagnosis)) +
  geom_boxplot(aes(x="Texture", y=texture_mean,colour=diagnosis)) + 
  geom_boxplot(aes(x="Perimeter",y=perimeter_mean,colour=diagnosis)) +
  xlab("Features") + ylab("Mean")

# separate area plot by itself due to large mean value

area_mean.Category <- ddply(Breast_Cancer_mean,"diagnosis",summarise,area_mean.Category = mean(area_mean))
mean_area<- ggplot(Breast_Cancer_mean,aes(x=area_mean,fill=diagnosis)) + geom_histogram(binwidth =50,alpha=0.55) +
  geom_vline(data=area_mean.Category,aes(xintercept=area_mean.Category,colour=diagnosis),linetype="dashed",size=1)

# test correlation between variables, use default Pearson correlation
mean_cor <- cor(Breast_Cancer_mean[,2:11])
colnames(mean_cor) <- gsub("_mean","",colnames(mean_cor))
rownames(mean_cor) <- gsub("_mean","",rownames(mean_cor))

mean_cor_p <- corrplot(mean_cor,type="upper",order="hclust",tl.col="black",tl.srt=45,diag = FALSE)
write.csv(mean_cor_p,"correlation_meanValues.csv")

# visualize the relationship between those highly correlated  (rho >=0.8) variables
p1 <- ggplot(Breast_Cancer_mean,aes(x=area_mean,y=radius_mean)) + geom_point(aes(colour=diagnosis)) + 
  theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))
p2<- ggplot(Breast_Cancer_mean,aes(x=area_mean,y=perimeter_mean)) + geom_point(aes(colour=diagnosis)) + 
  theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))
p3<- ggplot(Breast_Cancer_mean,aes(x=area_mean,y=concave.points_mean)) + geom_point(aes(colour=diagnosis)) +
  theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))
p4<- ggplot(Breast_Cancer_mean,aes(x=radius_mean,y=perimeter_mean)) + geom_point(aes(colour=diagnosis)) + 
  theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))
p5<- ggplot(Breast_Cancer_mean,aes(x=radius_mean,y=concave.points_mean)) + geom_point(aes(colour=diagnosis)) +
  theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))
p6<- ggplot(Breast_Cancer_mean,aes(x=concave.points_mean,y=perimeter_mean)) + geom_point(aes(colour=diagnosis)) + 
  theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))
p7<- ggplot(Breast_Cancer_mean,aes(x=concave.points_mean,y=compactness_mean)) + geom_point(aes(colour=diagnosis)) +
  theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))
p8<- ggplot(Breast_Cancer_mean,aes(x=concave.points_mean,y=concavity_mean)) + geom_point(aes(colour=diagnosis)) + 
  theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))
p9<-ggplot(Breast_Cancer_mean,aes(x=compactness_mean,y=concavity_mean)) + geom_point(aes(colour=diagnosis))+ 
  theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))
p<-ggplot(Breast_Cancer_mean,aes(x=compactness_mean,y=concavity_mean)) + geom_point(aes(colour=diagnosis))

# Function to only get the legend of ggplot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,g_legend(p),ncol=5)

# visualize the worst values
# the horizontal line indicates 50% quantile
ggplot(Breast_Cancer_Worst,aes(x="Smoothness",y=smoothness_worst)) + 
  geom_violin(aes(colour=diagnosis),scale = "count",draw_quantiles=0.5) +
  geom_violin(aes(x="Compactness",y=compactness_worst,colour=diagnosis),scale = "count",draw_quantiles=0.5) +
  geom_violin(aes(x="Concavity",y=concavity_worst,colour=diagnosis),scale = "count",draw_quantiles=0.5) +
  geom_violin(aes(x="Symmetry",y=symmetry_worst,colour=diagnosis),scale = "count",draw_quantiles=0.5) +
  geom_violin(aes(x="Fractal dimension",y=fractal_dimension_worst,colour=diagnosis),scale = "count",draw_quantiles=0.5) +
  xlab("Features") + ylab("Worst")

worst_largeValue <-ggplot(Breast_Cancer_Worst,aes(x="Radius", y=radius_worst)) + 
  geom_violin(aes(colour=diagnosis),scale = "count",draw_quantiles=0.5) +
  geom_violin(aes(x="Texture", y=texture_worst,colour=diagnosis),scale = "count",draw_quantiles=0.5) + 
  geom_violin(aes(x="Perimeter",y=perimeter_worst,colour=diagnosis),scale = "count",draw_quantiles=0.5) +
  xlab("Features") + ylab("Worst")

worst_area <- ggplot(Breast_Cancer_Worst,aes(x="Area", y=area_worst)) + 
  geom_violin(aes(colour=diagnosis),scale = "count",draw_quantiles=0.5)

# visualizae SE
#Breast_Cancer_SE_long <- reshape2::melt(Breast_Cancer_SE)
Breast_Cancer_SE_long_largeValue <- filter(Breast_Cancer_SE_long,variable %in% c("perimeter_se","radius_se","texture_se"))

Breast_Cancer_SE_long_smallValue <- filter(Breast_Cancer_SE_long,variable %in% c("compactness_se","concavity_se","fractal_dimension_se","smoothness_se","symmetry_se"))

Breast_Cancer_SE_long_area <- filter(Breast_Cancer_SE_long,variable == "area_se")

# correlation between mean and worst
cor_mean_worst <- cor(Breast_Cancer_mean[,2:11],Breast_Cancer_Worst[,2:11])
corrplot(cor_mean_worst,method="color",type = "upper",order="hclust",addCoef.col = "black",tl.col = "black",tl.srt = 45)

# correlation between worst and se
cor_se_worst <- cor(Breast_Cancer_SE[,2:11],Breast_Cancer_Worst[,2:11])
corrplot(cor_se_worst,method="color",type = "upper",order="hclust",addCoef.col = "black",tl.col = "black",tl.srt = 45)

cor_mean_se <- cor(Breast_Cancer_mean[,2:11],Breast_Cancer_SE[,2:11])
corrplot(cor_mean_se,method="color",type = "upper",order="hclust",addCoef.col = "black",tl.col = "black",tl.srt = 45)

# use PCA to transform correlated data
PCA_Breast_Cancer <- prcomp(Breast_Cancer_table[,3:32],scale. = TRUE)
# Eigenvalue
eigenValue <- (PCA_Breast_Cancer$sdev)^2
#Variance in percentage
variance_percentage <- eigenValue*100/sum(eigenValue)

eig.data <- data.frame(eigenValue=eigenValue,variance_percentage=variance_percentage)
barplot(eig.data[,2],names.arg=1:nrow(eig.data),xlab="Principal Components",ylab="Percentage of Variance",col = "lightskyblue",ylim=c(0,50))
lines(x = 1:nrow(eig.data), eig.data[, 2], type="b", pch=19, col = "deeppink")
text(pca_barplot,par("usr")[3],labels =1:nrow(eig.data),srt=0,adj = c(1.1,3.1), xpd = TRUE, cex=.9)


autoplot(PCA_Breast_Cancer,data=Breast_Cancer_table[,2:32],colour='diagnosis',frame=TRUE)

Breast_Cancer_PCAtransformed <- predict(PCA_Breast_Cancer,newdata=Breast_Cancer_table[,2:32])


# create 10-fold cross validation 
k_foldValue =10
set.seed(100)
TenFold_CrossValidation <- createFolds(c(1:nrow(Breast_Cancer_table)),k=k_foldValue,list=TRUE,returnTrain=FALSE)

############################################# Modeling ###################################
######################## KNN Classification ##############################################
######### Experiment with a series of k values for KNN ###################################
k_values <- c(1:20)
knn_accuracy_different_Ks <- matrix(,nrow=length(k_values),ncol=k_foldValue)

for (i in k_values){
  for (j in 1:k_foldValue ){
    
    testingDataRowIndex <- TenFold_CrossValidation[[j]]
    
    # get training data
    trainingDataRaw <- Breast_Cancer_table[-testingDataRowIndex,]
    training_label <- Breast_Cancer_table[-testingDataRowIndex,2]
    
    # get testing data
    testingDataRaw <- Breast_Cancer_table[testingDataRowIndex,]
    testing_label <- Breast_Cancer_table[testingDataRowIndex,2]
    
    # Project raw data to PCA plane
    training_PCA <- prcomp(Breast_Cancer_table[-testingDataRowIndex,3:32],scale. = TRUE)
    trainingData_PCA_transformed <- predict(training_PCA,newdata=trainingDataRaw)
    testingData_PCA_transformed <- predict(training_PCA,newdata=testingDataRaw)
    
    # Build KNN model
    knn_result<- knn(trainingData_PCA_transformed,testingData_PCA_transformed,training_label,k=i,prob = TRUE)
    
    xtab <- table(knn_result,testing_label)
    confusionMat <- confusionMatrix(xtab,positive = "M")
    knn_accuracy_different_Ks[i,j] <- confusionMat$overall[[1]]
  }
}

avg_fold_accuracy_Ks <- rowMeans(knn_accuracy_different_Ks)
avg_fold_accuracy_Ks <- data.frame(avg_fold_accuracy_Ks); 
avg_fold_accuracy_Ks$K <- k_values
colnames(avg_fold_accuracy_Ks) <- c("Average_Accuracy","Ks")

# plot accuracy vs different Ks
knn_accuracy_k_p <-ggplot(avg_fold_accuracy_Ks,aes(x=Ks,y=Average_Accuracy)) + 
  geom_point(aes(colour=Average_Accuracy)) +
  xlab("Different K values in KNN") + ylab("Average Accuracy ")+
  scale_colour_gradient(low="blue",high="red") +
  geom_line(linetype=3)
  
knn_accuracy_k_p


###### Experiment with different number of top eigenvectors ###############
top_eigenvector <- c(2:30)
knn_accuracy_different_topEigenvectors <- matrix(,nrow=length(top_eigenvector),ncol=k_foldValue)

for (i in top_eigenvector){
  for (j in 1:k_foldValue ){
    
    testingDataRowIndex <- TenFold_CrossValidation[[j]]
    
    # get training data
    trainingDataRaw <- Breast_Cancer_table[-testingDataRowIndex,]
    training_label <- Breast_Cancer_table[-testingDataRowIndex,2]
    
    # get testing data
    testingDataRaw <- Breast_Cancer_table[testingDataRowIndex,]
    testing_label <- Breast_Cancer_table[testingDataRowIndex,2]
    
    # Project raw data to PCA plane
    training_PCA <- prcomp(Breast_Cancer_table[-testingDataRowIndex,3:32],scale. = TRUE)
    trainingData_PCA_transformed <- predict(training_PCA,newdata=trainingDataRaw)
    testingData_PCA_transformed <- predict(training_PCA,newdata=testingDataRaw)
    
    # Build KNN model using different number of top eigenvectors at K=7
    knn_result<- knn(trainingData_PCA_transformed[,1:i],testingData_PCA_transformed[,1:i],training_label,k=7,prob = TRUE)
    
    xtab <- table(knn_result,testing_label)
    confusionMat <- confusionMatrix(xtab,positive = "M");confusionMat
    knn_accuracy_different_topEigenvectors[i-1,j] <- confusionMat$overall[[1]]
  }
}

knn_accuracy_different_topEigenvectors

knn_avg_fold_accuracy_Eigs <- rowMeans(knn_accuracy_different_topEigenvectors)
knn_avg_fold_accuracy_Eigs <- data.frame(knn_avg_fold_accuracy_Eigs); 
knn_avg_fold_accuracy_Eigs$Number_Eigenvector <- top_eigenvector
colnames(knn_avg_fold_accuracy_Eigs) <- c("Average_Accuracy","Top_Eigenvector_Number")

# plot accuracy vs different top number of eigenvectors
knn_accuracy_eig_p <-ggplot(knn_avg_fold_accuracy_Eigs,aes(x=Top_Eigenvector_Number,y=Average_Accuracy)) + 
  geom_point(aes(colour=Average_Accuracy)) +
  xlab("Different Number of Top Eigenvectors") + ylab("Average Accuracy ")+
  scale_colour_gradient(low="blue",high="red") +
  geom_line(linetype=3) + ggtitle("KNN")

knn_accuracy_eig_p

# The Best performed KNN is k=7, topEigenvector_num=15, plot ROC curvem
KNN_AUC_list <- NULL # initiate a list to store AUC values in each fold
KNN_FPR_table <- matrix(,nrow = 59,ncol = 10)
KNN_TPR_table <- matrix(,nrow = 59,ncol=10)

KNN_10fold_ROC <- ggplot(data = NULL,aes(x=FPR,y=TPR)) + theme_bw() + xlab("False Positive Rate") + ylab("True Positive Rate")

for (j in 1:k_foldValue ){
  
  testingDataRowIndex <- TenFold_CrossValidation[[j]]
  
  # get training data
  trainingDataRaw <- Breast_Cancer_table[-testingDataRowIndex,]
  training_label <- Breast_Cancer_table[-testingDataRowIndex,2]
  
  # get testing data
  testingDataRaw <- Breast_Cancer_table[testingDataRowIndex,]
  testing_label <- Breast_Cancer_table[testingDataRowIndex,2]
  
  # Project raw data to PCA plane
  training_PCA <- prcomp(Breast_Cancer_table[-testingDataRowIndex,3:32],scale. = TRUE)
  trainingData_PCA_transformed <- predict(training_PCA,newdata=trainingDataRaw)
  testingData_PCA_transformed <- predict(training_PCA,newdata=testingDataRaw)
  
  # Build KNN model using different number of top eigenvectors at K=7
  knn_result<- knn(trainingData_PCA_transformed[,1:15],testingData_PCA_transformed[,1:15],training_label,k=7,prob = TRUE)
  
  # Calculate ROC curve table
  prob_knn <- attr(knn_result,"prob")
  prob_knn <- 2*ifelse(knn_result == "B", 1-prob, prob) - 1
  pred_knn <- prediction(prob_knn,testing_label)
  perf_knn <- performance(pred_knn,"tpr","fpr")
 
  FPR <-slot(perf_knn,"x.values")[[1]] # get False Positive Rate- x.values
  TPR <-slot(perf_knn,"y.values")[[1]] # get True Positive Rate- x.values
  
  ROC_table <- NULL
  ROC_table$FPR <- FPR
  KNN_FPR_table[1:length(FPR),j] = FPR
  
  ROC_table$TPR <- TPR
  KNN_TPR_table[1:length(TPR),j] = TPR
  
  # get AUC value
  auc_knn <- performance(pred_knn,measure = "auc")
  auc_value <- slot(auc_knn,"y.values")[[1]]
  KNN_AUC_list[j] <- auc_value
  
  ROC_table$Fold <- paste("Fold",toString(j),"(AUC",toString(round(auc_value,3)),")")
  ROC_table <- data.frame(ROC_table)
  
  KNN_10fold_ROC <- KNN_10fold_ROC + geom_line(data=ROC_table,aes(colour=Fold))
}

# plot diagnoal line
KNN_10fold_ROC + geom_abline(slope=1,linetype=3)

######################## SVM Classification ##############################################
# tune parameter for linear, polynomial and radial basis kernels using
#tune(svm, train.x=trainingData_PCA_transformed, train.y=training_label, kernel="linear", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
# For linear: cost =10, gamma = 0.5
# For Polynomial: cost =0.1, gamma = 0.5
# For Radial Basis: cost =0.1, gamma = 0.5

# Experiment to test the effect of number of top eigenvectors in SVM 
###### Experiment with different number of top eigenvectors ###############
svm_linear_accuracy_different_topEigenvectors <- matrix(,nrow=length(top_eigenvector),ncol=k_foldValue)
svm_poly_accuracy_different_topEigenvectors <- matrix(,nrow=length(top_eigenvector),ncol=k_foldValue)
svm_radial_accuracy_different_topEigenvectors <- matrix(,nrow=length(top_eigenvector),ncol=k_foldValue)

for (i in top_eigenvector){
  for (j in 1:k_foldValue ){
    
    testingDataRowIndex <- TenFold_CrossValidation[[j]]
    
    # get training data
    trainingDataRaw <- Breast_Cancer_table[-testingDataRowIndex,]
    training_label <- Breast_Cancer_table[-testingDataRowIndex,2]
    
    # get testing data
    testingDataRaw <- Breast_Cancer_table[testingDataRowIndex,]
    testing_label <- Breast_Cancer_table[testingDataRowIndex,2]
    
    # Project raw data to PCA plane
    training_PCA <- prcomp(Breast_Cancer_table[-testingDataRowIndex,3:32],scale. = TRUE)
    trainingData_PCA_transformed <- predict(training_PCA,newdata=trainingDataRaw)
    testingData_PCA_transformed <- predict(training_PCA,newdata=testingDataRaw)
    
    # Build KNN model using different number of top eigenvectors at K=7
    # test linear, polynomial and radial basis models
    svm_model <- svm(training_label ~ ., data=trainingData_PCA_transformed[,1:i],kernel = "radial", cost=0.1,gamma=0.5)
    svm_result <- predict(svm_model,testingData_PCA_transformed[,1:i],decision.values = TRUE)
    
    svm_xtab <- table(svm_result,testing_label); svm_xtab
    svm_confusionMat <- confusionMatrix(svm_xtab,positive = "M")
    #svm_linear_accuracy_different_topEigenvectors[i-1,j] <- svm_confusionMat$overall[[1]]
    #svm_poly_accuracy_different_topEigenvectors[i-1,j] <- svm_confusionMat$overall[[1]]
    svm_radial_accuracy_different_topEigenvectors[i-1,j] <- svm_confusionMat$overall[[1]]
  }
}


#View(svm_linear_accuracy_different_topEigenvectors)
#View(svm_poly_accuracy_different_topEigenvectors)
#View(svm_radial_accuracy_different_topEigenvectors)

# average accuracy in linear kernel
svm_linear_avg_fold_accuracy_Eigs <- rowMeans(svm_linear_accuracy_different_topEigenvectors)
svm_linear_avg_fold_accuracy_Eigs <- data.frame(svm_linear_avg_fold_accuracy_Eigs); 
svm_linear_avg_fold_accuracy_Eigs$Number_Eigenvector <- top_eigenvector
svm_linear_avg_fold_accuracy_Eigs$Kernel <- "Linear"
colnames(svm_linear_avg_fold_accuracy_Eigs) <- c("Average_Accuracy","Top_Eigenvector_Number","Kernel")

# average accuracy in polynomial kernel
svm_poly_avg_fold_accuracy_Eigs <- rowMeans(svm_poly_accuracy_different_topEigenvectors)
svm_poly_avg_fold_accuracy_Eigs <- data.frame(svm_poly_avg_fold_accuracy_Eigs); 
svm_poly_avg_fold_accuracy_Eigs$Number_Eigenvector <- top_eigenvector
svm_poly_avg_fold_accuracy_Eigs$Kernel <- "Polynomial"
colnames(svm_poly_avg_fold_accuracy_Eigs) <- c("Average_Accuracy","Top_Eigenvector_Number","Kernel")

# average accuracy in radial basis kernel
svm_radial_avg_fold_accuracy_Eigs <- rowMeans(svm_radial_accuracy_different_topEigenvectors)
svm_radial_avg_fold_accuracy_Eigs <- data.frame(svm_radial_avg_fold_accuracy_Eigs); 
svm_radial_avg_fold_accuracy_Eigs$Number_Eigenvector <- top_eigenvector
svm_radial_avg_fold_accuracy_Eigs$Kernel <- "Radial Basis"
colnames(svm_radial_avg_fold_accuracy_Eigs) <- c("Average_Accuracy","Top_Eigenvector_Number","Kernel")

# combine rows
svm_acc_eig_combined <- rbind(svm_linear_avg_fold_accuracy_Eigs,svm_poly_avg_fold_accuracy_Eigs,svm_radial_avg_fold_accuracy_Eigs)


# plot accuracy vs different top eigenvector numbers
svm_accuracy_eig_p <-ggplot(svm_acc_eig_combined,aes(x=Top_Eigenvector_Number,y=Average_Accuracy)) + 
  geom_line(aes(colour=Kernel)) + geom_point(aes(colour=Kernel)) +
  xlab("Different Number of Top Eigenvectors") + ylab("Average Accuracy ") + ggtitle("SVM")
svm_accuracy_eig_p

# The highest accuracy is achieved when top 9 eigenvectors were used and using linear kernel
# get corresponding average AUC list and ROC curve
SVM_AUC_list <- NULL # initiate a list to store AUC values in each fold
SVM_FPR_table <- matrix(,nrow = 60,ncol = 10)
SVM_TPR_table <- matrix(,nrow = 60,ncol=10)

SVM_10fold_ROC <- ggplot(data = NULL,aes(x=FPR,y=TPR)) + theme_bw() + xlab("False Positive Rate") + ylab("True Positive Rate")
for (j in 1:k_foldValue ){
  
  testingDataRowIndex <- TenFold_CrossValidation[[j]]
  
  # get training data
  trainingDataRaw <- Breast_Cancer_table[-testingDataRowIndex,]
  training_label <- Breast_Cancer_table[-testingDataRowIndex,2]
  
  # get testing data
  testingDataRaw <- Breast_Cancer_table[testingDataRowIndex,]
  testing_label <- Breast_Cancer_table[testingDataRowIndex,2]
  
  # Project raw data to PCA plane
  training_PCA <- prcomp(Breast_Cancer_table[-testingDataRowIndex,3:32],scale. = TRUE)
  trainingData_PCA_transformed <- predict(training_PCA,newdata=trainingDataRaw)
  testingData_PCA_transformed <- predict(training_PCA,newdata=testingDataRaw)
  
  # Build KNN model using different number of top eigenvectors at K=7
  # test linear, polynomial and radial basis models
  svm_model <- svm(training_label ~ ., data=trainingData_PCA_transformed[,1:9],kernel = "linear", cost=10,gamma=0.5)
  svm_result <- predict(svm_model,testingData_PCA_transformed[,1:9],decision.values = TRUE)
  
  svm.pred <- prediction(attributes(svm_result)$decision.values,testing_label)
  svm.roc <- performance(svm.pred, 'tpr', 'fpr') 
  
  FPR <-slot(svm.roc,"x.values")[[1]] # get False Positive Rate- x.values
  TPR <-slot(svm.roc,"y.values")[[1]] # get True Positive Rate- x.values
  
  ROC_table <- NULL
  ROC_table$FPR <- FPR
  SVM_FPR_table[1:length(FPR),j] = FPR
  
  ROC_table$TPR <- TPR
  SVM_TPR_table[1:length(TPR),j] = TPR
  
  # get AUC value
  svm.auc <- performance(svm.pred, 'auc');
  svm.auc.value <- slot(svm.auc,"y.values")[[1]]
  SVM_AUC_list[j] <- svm.auc.value
  
  ROC_table$Fold <- paste("Fold",toString(j),"(AUC",toString(round(svm.auc.value,3)),")")
  ROC_table <- data.frame(ROC_table)
  
  SVM_10fold_ROC <- SVM_10fold_ROC + geom_line(data=ROC_table,aes(colour=Fold))
}
# plot diagnoal line
SVM_10fold_ROC + geom_abline(slope=1,linetype=3)

####### Classification with Logistic Regression #################
# Experiment to test the effect of number of top eigenvectors in logistic regression

###### Experiment with different number of top eigenvectors ###############
logit_accuracy_different_topEigenvectors <- matrix(,nrow=length(top_eigenvector),ncol=k_foldValue)

for (i in top_eigenvector){
  for (j in 1:k_foldValue ){
    
    testingDataRowIndex <- TenFold_CrossValidation[[j]]
    
    # get training data
    trainingDataRaw <- Breast_Cancer_table[-testingDataRowIndex,]
    training_label <- Breast_Cancer_table[-testingDataRowIndex,2]
    
    # get testing data
    testingDataRaw <- Breast_Cancer_table[testingDataRowIndex,]
    testing_label <- Breast_Cancer_table[testingDataRowIndex,2]
    
    # Project raw data to PCA plane
    training_PCA <- prcomp(Breast_Cancer_table[-testingDataRowIndex,3:32],scale. = TRUE)
    trainingData_PCA_transformed <- predict(training_PCA,newdata=trainingDataRaw)
    testingData_PCA_transformed <- predict(training_PCA,newdata=testingDataRaw)
    
    # build logistic regression model with training data
    logit_model <-glm(training_label ~., data.frame(trainingData_PCA_transformed)[,1:i],family = binomial())
    
    # apply model to testing adata
    logit_result <- predict(logit_model,data.frame(testingData_PCA_transformed)[,1:i],type = "response")
    
    cutoff <- 0.5
    logit_predict_label <- ifelse(logit_result >= cutoff, "M","B")
    
    logit_xtab <- table(logit_predict_label,testing_label)
    logit_confusionMat <- confusionMatrix(logit_xtab,positive = "M")
    
    # store accuracy value
    logit_accuracy_different_topEigenvectors[i-1,j] <- logit_confusionMat$overall[[1]]
  }
}

View(logit_accuracy_different_topEigenvectors)

# average accuracy in logistic regression
logit_avg_fold_accuracy_Eigs <- rowMeans(logit_accuracy_different_topEigenvectors)
logit_avg_fold_accuracy_Eigs <- data.frame(logit_avg_fold_accuracy_Eigs); 
logit_avg_fold_accuracy_Eigs$Number_Eigenvector <- top_eigenvector
colnames(logit_avg_fold_accuracy_Eigs) <- c("Average_Accuracy","Top_Eigenvector_Number")

# plot 
logit_accuracy_eig_p <-ggplot(logit_avg_fold_accuracy_Eigs,aes(x=Top_Eigenvector_Number,y=Average_Accuracy)) + 
  geom_point(aes(colour=Average_Accuracy)) +
  xlab("Different Number of Top Eigenvectors") + ylab("Average Accuracy ")+
  scale_colour_gradient(low="blue",high="red") +
  geom_line(linetype=3) + ggtitle("Logistic Regression")

logit_accuracy_eig_p

# The highest accuracy is achieved when top 5 eigenvectors 
# get corresponding average AUC list and ROC curve
Logit_AUC_list <- NULL # initiate a list to store AUC values in each fold
Logit_FPR_table <- matrix(,nrow = 60,ncol = 10)
Logit_TPR_table <- matrix(,nrow = 60,ncol=10)

logit_10fold_ROC <- ggplot(data = NULL,aes(x=FPR,y=TPR)) + theme_bw() + xlab("False Positive Rate") + ylab("True Positive Rate")

for (j in 1:k_foldValue ){
  
  testingDataRowIndex <- TenFold_CrossValidation[[j]]
  
  # get training data
  trainingDataRaw <- Breast_Cancer_table[-testingDataRowIndex,]
  training_label <- Breast_Cancer_table[-testingDataRowIndex,2]
  
  # get testing data
  testingDataRaw <- Breast_Cancer_table[testingDataRowIndex,]
  testing_label <- Breast_Cancer_table[testingDataRowIndex,2]
  
  # Project raw data to PCA plane
  training_PCA <- prcomp(Breast_Cancer_table[-testingDataRowIndex,3:32],scale. = TRUE)
  trainingData_PCA_transformed <- predict(training_PCA,newdata=trainingDataRaw)
  testingData_PCA_transformed <- predict(training_PCA,newdata=testingDataRaw)
  
  # build logistic regression model with training data
  logit_model <-glm(training_label ~., data.frame(trainingData_PCA_transformed)[,1:5],family = binomial())
  
  # apply model to testing adata
  logit_result <- predict(logit_model,data.frame(testingData_PCA_transformed)[,1:5],type = "response")
  
  logit_pred <- prediction(logit_result,testing_label)
  
  # AUC
  logit_auc <- performance(logit_pred,measure = "auc")
  logit_auc_value <- slot(logit_auc,"y.values")[[1]]
  Logit_AUC_list[j] <- logit_auc_value
  
  # ROC
  logit_roc <- performance(logit_pred,"tpr","fpr")
  
  FPR <-slot(logit_roc,"x.values")[[1]] # get False Positive Rate- x.values
  TPR <-slot(logit_roc,"y.values")[[1]] # get True Positive Rate- x.values
  
  ROC_table <- NULL
  ROC_table$FPR <- FPR
  Logit_FPR_table[1:length(FPR),j] = FPR
  
  ROC_table$TPR <- TPR
  Logit_TPR_table[1:length(TPR),j] = TPR
  
  ROC_table$Fold <- paste("Fold",toString(j),"(AUC",toString(round(logit_auc_value,3)),")")
  ROC_table <- data.frame(ROC_table)
  
  logit_10fold_ROC <- logit_10fold_ROC + geom_line(data=ROC_table,aes(colour=Fold))
}
# plot diagnoal line
logit_10fold_ROC + geom_abline(slope=1,linetype=3)

###### Classification with Naive Bayes ####################################
###### Experiment with different number of top eigenvectors ###############
nb_accuracy_different_topEigenvectors <- matrix(,nrow=length(top_eigenvector),ncol=k_foldValue)

for (i in top_eigenvector){
  for (j in 1:k_foldValue ){
    
    testingDataRowIndex <- TenFold_CrossValidation[[j]]
    
    # get training data
    trainingDataRaw <- Breast_Cancer_table[-testingDataRowIndex,]
    training_label <- Breast_Cancer_table[-testingDataRowIndex,2]
    
    # get testing data
    testingDataRaw <- Breast_Cancer_table[testingDataRowIndex,]
    testing_label <- Breast_Cancer_table[testingDataRowIndex,2]
    
    # Project raw data to PCA plane
    training_PCA <- prcomp(Breast_Cancer_table[-testingDataRowIndex,3:32],scale. = TRUE)
    trainingData_PCA_transformed <- predict(training_PCA,newdata=trainingDataRaw)
    testingData_PCA_transformed <- predict(training_PCA,newdata=testingDataRaw)
    
    # Build Naive Bayes model using different number of top eigenvectors
    nb_model <- naiveBayes(training_label ~ ., data= data.frame(trainingData_PCA_transformed)[,1:i])
    nb_result <- predict(nb_model,data.frame(testingData_PCA_transformed)[,1:i])
    
    nb_xtab <- table(nb_result,testing_label)
    nb_confusionMat <- confusionMatrix(nb_xtab,positive = "M"); 
    nb_accuracy_different_topEigenvectors[i-1,j] <- nb_confusionMat$overall[[1]]
  }
}


View(nb_accuracy_different_topEigenvectors)

# average accuracy in each fold
nb_avg_fold_accuracy_Eigs <- rowMeans(nb_accuracy_different_topEigenvectors)
nb_avg_fold_accuracy_Eigs <- data.frame(nb_avg_fold_accuracy_Eigs); 
nb_avg_fold_accuracy_Eigs$Number_Eigenvector <- top_eigenvector
colnames(nb_avg_fold_accuracy_Eigs) <- c("Average_Accuracy","Top_Eigenvector_Number")

# plot accuracy vs different Ks
nb_accuracy_eig_p <-ggplot(nb_avg_fold_accuracy_Eigs,aes(x=Top_Eigenvector_Number,y=Average_Accuracy)) + 
  geom_point(aes(colour=Average_Accuracy)) +
  xlab("Different Number of Top Eigenvectors") + ylab("Average Accuracy ")+
  scale_colour_gradient(low="blue",high="red") +
  geom_line(linetype=3) + ggtitle("Naive Bayes")

nb_accuracy_eig_p

# The highest accuracy is achieved when top 6 eigenvectors 
# get corresponding average AUC list and ROC curve
NB_AUC_list <- NULL # initiate a list to store AUC values in each fold
NB_FPR_table <- matrix(,nrow = 60,ncol = 10)
NB_TPR_table <- matrix(,nrow = 60,ncol=10)

NB_10fold_ROC <- ggplot(data = NULL,aes(x=FPR,y=TPR)) + theme_bw() + xlab("False Positive Rate") + ylab("True Positive Rate")

for (j in 1:k_foldValue ){
  
  testingDataRowIndex <- TenFold_CrossValidation[[j]]
  
  # get training data
  trainingDataRaw <- Breast_Cancer_table[-testingDataRowIndex,]
  training_label <- Breast_Cancer_table[-testingDataRowIndex,2]
  
  # get testing data
  testingDataRaw <- Breast_Cancer_table[testingDataRowIndex,]
  testing_label <- Breast_Cancer_table[testingDataRowIndex,2]
  
  # Project raw data to PCA plane
  training_PCA <- prcomp(Breast_Cancer_table[-testingDataRowIndex,3:32],scale. = TRUE)
  trainingData_PCA_transformed <- predict(training_PCA,newdata=trainingDataRaw)
  testingData_PCA_transformed <- predict(training_PCA,newdata=testingDataRaw)
  
  # Build Naive Bayes model using different number of top eigenvectors
  nb_model <- naiveBayes(training_label ~ ., data= data.frame(trainingData_PCA_transformed)[,1:6])
  nb_result <- predict(nb_model,data.frame(testingData_PCA_transformed)[,1:6],type = "raw")
  
  nb_pred <- prediction(nb_result[,2],testing_label)

  # AUC
  nb_auc <- performance(nb_pred,measure = "auc")
  nb_auc_value <- slot(nb_auc,"y.values")[[1]]
  NB_AUC_list[j] <- nb_auc_value
  
  # ROC
  nb_roc <- performance(nb_pred,"tpr","fpr")
  
  FPR <-slot(nb_roc,"x.values")[[1]] # get False Positive Rate- x.values
  TPR <-slot(nb_roc,"y.values")[[1]] # get True Positive Rate- x.values
  
  ROC_table <- NULL
  ROC_table$FPR <- FPR
  NB_FPR_table[1:length(FPR),j] = FPR
  
  ROC_table$TPR <- TPR
  NB_TPR_table[1:length(TPR),j] = TPR
  
  ROC_table$Fold <- paste("Fold",toString(j),"(AUC",toString(round(nb_auc_value,3)),")")
  ROC_table <- data.frame(ROC_table)
  
  NB_10fold_ROC <- NB_10fold_ROC + geom_line(data=ROC_table,aes(colour=Fold))
}
# plot diagnoal line
NB_10fold_ROC + geom_abline(slope=1,linetype=3)

############# Model Comparison #######################################
# Accuracy comparison
model_accuracy_cmp <- data.frame(c(knn_max_avg_accuracy,svm_max_avg_accuracy,logit_max_avg_accuracy,nb_max_avg_accuracy))
model_accuracy_cmp$Model <- c("KNN","SVM","Logistic Regression","Naive Bayes")
model_accuracy_cmp <- data.frame(model_accuracy_cmp)
colnames(model_accuracy_cmp) <- c("Average_Accuracy","Model")

p_accuracy<- ggplot(model_accuracy_cmp,aes(x=Model,y=Average_Accuracy,label=round(Average_Accuracy,5))) + geom_bar(stat = "identity",aes(fill=Model)) + geom_text()+ theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))

# rearrange accuracy vs top eigenvector number plot
grid.arrange(knn_accuracy_eig_p,svm_accuracy_eig_p,logit_accuracy_eig_p,nb_accuracy_eig_p,ncol=2)

# combined average ROC plot
KNN_avg_ROC <- rowMeans(KNN_FPR_table,na.rm = TRUE); KNN_avg_ROC <- data.frame(KNN_avg_ROC)
KNN_avg_ROC$TPR <- rowMeans(KNN_TPR_table); 
KNN_avg_ROC$Model <-paste("KNN","(AUC",toString(round(mean(KNN_AUC_list),4)),")")
colnames(KNN_avg_ROC) <- c("False_Positive_Rate","True_Positive_Rate","Model")

SVM_avg_ROC <- rowMeans(SVM_FPR_table,na.rm = TRUE); SVM_avg_ROC <- data.frame(SVM_avg_ROC)
SVM_avg_ROC$TPR <- rowMeans(SVM_TPR_table,na.rm = TRUE);
SVM_avg_ROC$Model <-paste("SVM","(AUC",toString(round(mean(SVM_AUC_list),4)),")")
colnames(SVM_avg_ROC) <-c("False_Positive_Rate","True_Positive_Rate","Model")

Logit_avg_ROC <- rowMeans(Logit_FPR_table,na.rm = TRUE); Logit_avg_ROC <- data.frame(Logit_avg_ROC)
Logit_avg_ROC$TPR <- rowMeans(Logit_TPR_table,na.rm = TRUE); 
Logit_avg_ROC$Model <-paste("Logistic Regression","(AUC",toString(round(mean(Logit_AUC_list),4)),")")
colnames(Logit_avg_ROC) <- c("False_Positive_Rate","True_Positive_Rate","Model")

NB_avg_ROC <- rowMeans(NB_FPR_table,na.rm = TRUE); NB_avg_ROC <- data.frame(NB_avg_ROC)
NB_avg_ROC$TPR <- rowMeans(NB_TPR_table,na.rm = TRUE); 
NB_avg_ROC$Model <-paste("Naive Bayes","(AUC",toString(round(mean(NB_AUC_list),4)),")")
colnames(NB_avg_ROC) <- c("False_Positive_Rate","True_Positive_Rate","Model")

ggplot(data=NULL,aes(x=False_Positive_Rate,y=True_Positive_Rate)) + 
  geom_line(data=KNN_avg_ROC,aes(color=Model)) +
  geom_line(data=SVM_avg_ROC,aes(color=Model)) +
  geom_line(data=Logit_avg_ROC,aes(color=Model)) +
  geom_line(data=NB_avg_ROC,aes(color=Model)) +
  xlab("False Positive Rate") + ylab("True Positive Rate")
