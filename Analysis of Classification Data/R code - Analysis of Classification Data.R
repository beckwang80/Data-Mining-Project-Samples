# Data Preparation and Exploratory Data Analysis
Texture <- read.csv("Texture.csv")
TexturePred <- Texture[,-41]
TextureClass <- Texture[,41]
PCTexture <- princomp(TexturePred, cor=T)
screeplot(PCTexture, col=c("royal blue","blue","dark blue","light blue", "purple","grey"))
summary(PCTexture)
par(mfrow=c(1,10), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
for (i in 1:10) {
  boxplot(TexturePred[,i], pch=19, col="moccasin")
  mtext("Boxplots of A1 - A10", cex=0.8, side=1, line=0, outer=T)
}
for (i in 11:20) {
  boxplot(TexturePred[,i], pch=19, col="aliceblue")
  mtext("Boxplots of A11 - A20", cex=0.8, side=1, line=0, outer=T)
}
for (i in 21:30) {
  boxplot(TexturePred[,i], pch=19, col="bisque1")
  mtext("Boxplots of A21 - A30", cex=0.8, side=1, line=0, outer=T)
}
for (i in 31:40) {
  boxplot(TexturePred[,i], pch=19, col="lightblue1")
  mtext("Boxplots of A31 - A40", cex=0.8, side=1, line=0, outer=T)
}

DescTex <- describe(TexturePred)
Qm <- matrix(0, nrow(DescTex), 3)
DescTex <- cbind(DescTex, Qm)
for (i in 1:nrow(DescTex)){
  DescTex[i,14] <- quantile(TexturePred[,i], 0.25)
  DescTex[i,15] <- quantile(TexturePred[,i], 0.75)
  DescTex[i,16] <- DescTex[i,15]-DescTex[i,14]
}
write.csv(DescTex, "DescTex.csv")

######### Outlier rejection ##############

limoutL <- rep(0,40)
limoutU <- rep(0,40)
for (i in 1:40){
  t1 <- quantile(TexturePred[,i], 0.75)
  t2 <- quantile(TexturePred[,i], 0.25)
  t3 <- IQR(TexturePred[,i])
  limoutU[i] <- t1 + 3*t3
  limoutL[i] <- t2 - 3*t3
}
TxPrdIndexU <- matrix(0, 5500, 40)
TxPrdIndexL <- matrix(0, 5500, 40)
for (i in 1:5500)
  for (j in 1:40){
    if (TexturePred[i,j] > limoutU[j]) TxPrdIndexU[i,j] <- 1
    if (TexturePred[i,j] < limoutL[j]) TxPrdIndexL[i,j] <- 1
  }
TxIndU <- apply(TxPrdIndexU, 1, sum)
TxIndL <- apply(TxPrdIndexL, 1, sum)
TxInd <- TxIndU + TxIndL
TxPrdTemp <- cbind(TxInd, TexturePred)

Indexes <- rep(0, 128)
j <- 1
for (i in 1:5500){
  if (TxInd[i] > 0) {Indexes[j]<- i
                     j <- j + 1}
  else j <- j
}
TxPredLib <- TexturePred[-Indexes,]  # Inside of Q3+3IQR Predictors
TxClassLib <- TextureClass[-Indexes]  # Inside of Q3+3IQR the class column

############ Correlation #############
TexturePredCor <- cor(TxPredLib)
write.csv(TexturePredCor, "CorrTex.csv")

# Principal Component Analysis
PCTexture <- prcomp(TxPredLib, scale=T)
print(PCTexture)
summary(PCTexture)

par(mfrow=c(1,1), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
plot(PCTexture, type="line", col=c("dark blue"), main="", pch=19) ## Scree plot
mtext("Screeplot of Texture", side=1, line=3, cex=0.8)

biplot(PCTexture, pch=19, cex=0.6, col=c("olivedrab1", "blue"))
TextureScore <- PCTexture$x[,1:8]
TextureMasterPCScore <- cbind(TextureScore, TxClassLib)

# Used for model fitting and 10-fold cross-validation
# Cross-Validation
########### Creating Cross-validation Dataset  ###########################
CVInd <- sample.int(5372, 5372, replace=FALSE)
CVInds <- CVInd[1:4833]
dim(CVInds) <- c(537,9)
CVInds10 <- CVInd[4834:5372]

TXTrain1 <- TextureMasterPCScore[-CVInds[,1],] # Training Set 1
TXTest1  <- TextureMasterPCScore[CVInds[,1],]  # Test Set 1

TXTrain2 <- TextureMasterPCScore[-CVInds[,2],] # Training Set 2
TXTest2  <- TextureMasterPCScore[CVInds[,2],]  # Test Set 2

TXTrain3 <- TextureMasterPCScore[-CVInds[,3],] # Training Set 3
TXTest3 <- TextureMasterPCScore[CVInds[,3],]  # Test Set 3

TXTrain4<- TextureMasterPCScore[-CVInds[,4],] # Training Set 4
TXTest4 <- TextureMasterPCScore[CVInds[,4],]  # Test Set 4

TXTrain5<- TextureMasterPCScore[-CVInds[,5],] # Training Set 5
TXTest5 <- TextureMasterPCScore[CVInds[,5],]  # Test Set 5

TXTrain6<- TextureMasterPCScore[-CVInds[,6],] # Training Set 6
TXTest6 <- TextureMasterPCScore[CVInds[,6],]  # Test Set 6

TXTrain7<- TextureMasterPCScore[-CVInds[,7],] # Training Set 7
TXTest7 <- TextureMasterPCScore[CVInds[,7],]  # Test Set 7

TXTrain8 <- TextureMasterPCScore[-CVInds[,8],] # Training Set 8
TXTest8 <- TextureMasterPCScore[CVInds[,8],]  # Test Set 8

TXTrain9 <- TextureMasterPCScore[-CVInds[,9],] # Training Set 9
TXTest9 <- TextureMasterPCScore[CVInds[,9],]  # Test Set 9

TXTrain10 <- TextureMasterPCScore[-CVInds10,] # Training Set 10
TXTest10  <- TextureMasterPCScore[CVInds10,]  # Test Set 10

TextureMasterPCScore <- as.data.frame(TextureMasterPCScore)

TextureMasterPCScore$TxClassLib <- as.factor(TextureMasterPCScore$TxClassLib)
prop.table(table(TextureMasterPCScore$TxClassLib))
prop.table(table(TXTrain1$TxClassLib))
prop.table(table(TXTrain2$TxClassLib))
prop.table(table(TXTrain3$TxClassLib))
prop.table(table(TXTrain4$TxClassLib))
prop.table(table(TXTrain5$TxClassLib))
prop.table(table(TXTrain6$TxClassLib))
prop.table(table(TXTrain7$TxClassLib))
prop.table(table(TXTrain8$TxClassLib))
prop.table(table(TXTrain9$TxClassLib))
prop.table(table(TXTrain10$TxClassLib))

# Discriminant Analysis

ldafit4 <-lda(TxClassLib ~ ., data=TXTrain4)
# ldafit4

lda.pred4 <- predict(ldafit4, data=TXTrain4)
tab4 <- table(lda.pred4$class, TXTrain4$TxClassLib)
propmisTrain4 <- 1 - tr(tab4)/length(lda.pred4$class)
cat("Proportion of Misclassification in Training Set 4:", propmisTrain4)

lda.Testpred4 <- predict(ldafit4, TXTest4)
testtab4 <- table(lda.Testpred4$class, TXTest4$TxClassLib)
propmisTest4 <- 1-tr(testtab4)/length(lda.Testpred4$class)
cat("Proportion of Misclassification in Test Set 4:", propmisTest4)

# Tree Based Algorithms
TXTrain1$TxClassLib <- as.factor(TXTrain1$TxClassLib)
TXTrainTree1 <- tree(TxClassLib ~ ., data=TXTrain1, method="class")

plot(TXTrainTree1, col="dark red")
text(TXTrainTree1, pretty=0, cex=0.6, col="dark red")
mtext("Decision Tree (Unpruned) for Training Set 1", side=3, line = 2, cex=0.8, col="dark red")

m <- misclass.tree(TXTrainTree1)
propmisTrain1 <- m / length(TXTrainTree1$y)
cat("Proportion of Misclassification in Training Set 1:", propmisTrain1)

TXTest1Treefit1 <- predict(TXTrainTree1, TXTest1, type="class")
Tab1 <- table(TXTest1Treefit1, TXTest1$TxClassLib)

propmisTest1 <- 1-tr(Tab1)/length(TXTest1Treefit1)
cat("Proportion of Misclassification in Test Set 1 =", propmisTest1)

TXTrainPruneTree1 <- prune.misclass(TXTrainTree1, best=20)
m <- misclass.tree(TXTrainPruneTree1)
m / length(TXTrainPruneTree1$y)

plot(TXTrainPruneTree1, col="dark red")
text(TXTrainPruneTree1, pretty=0, cex=0.6, col="dark red")
mtext("Decision Tree for Training Set 1", side=3, line = 2, cex=0.8, col="dark red")

TXTest1PruneTreefit1 <- predict(TXTrainPruneTree1, TXTest1, type="class")
Tab1 <- table(TXTest1PruneTreefit1, TXTest1$TxClassLib)

propmisTest1 <- 1-tr(Tab1)/length(TXTest1PruneTreefit1)
cat("Proportion of Misclassification in Test Set 1 =", propmisTest1)

################### Random Forest ###################

TXTrainRF1 <- randomForest(TXTrain1[,1:8],TXTrain1[,9], ntree=100, importance=T, proximity=T)
# TXTrainRF1 <- randomForest(TXTrain1[,1:8],TXTrain1[,9], xtest=TXTest1[,1:8], ytest=TXTest1[,9], ntree=100, importance=T, proximity=T)
plot(TXTrainRF1, main="OOB Error Rate: Set 1", cex=0.4)
TXTrainRF1
varImpPlot(TXTrainRF1,  pch=19, col="dark red", main="Variable Importance: Set 1", cex=0.8)