#install.packages('mice')
library(mice)

#install.packages('DMwR')
library(DMwR)

#install.packages('missForest')
library(missForest)

#install.packages('chemometrics')
library(chemometrics)



X <- read.table('Russet_ineqdata.txt', header=T, sep='\t', row.names=1)
X

md.pattern(X)
aggr_plot <- aggr(X, col=c('blue','red'), numbers=TRUE, sortVars=TRUE, labels=names(X), cex.axis=.7, gap=3, ylab=c("Percentatge of Missing data","Combination of Missing Values"))

X.ignoreRecords <-  X[complete.cases(X),]

X.Mean <- X
X.Mean$Rent[is.na(X.Mean$Rent)] <- mean(X$Rent, na.rm = TRUE)
X.Mean$ecks[is.na(X.Mean$ecks)] <- mean(X$ecks, na.rm = TRUE)

X.RegressionImputation <- X
RentModel <- lm(Rent ~ ., X)
EcksModel <- lm(ecks ~ ., data = X)
X.RegressionImputation$Rent[is.na(X$Rent)] <- predict(RentModel, X.RegressionImputation[is.na(X$Rent),])
X.RegressionImputation$ecks[is.na(X$ecks)] <- predict(EcksModel, X.RegressionImputation[is.na(X$ecks),])

X.knn <- knnImputation(X, k = 7)

X.mice <- complete(mice(X, m = 1), 1)
row.names(X.mice) <- row.names(X)

X.missForest <- missForest(X)$ximp

X.compare <- rbind(X.Mean[is.na(X)], X.RegressionImputation[is.na(X)], X.knn[is.na(X)], X.mice[is.na(X)], X.missForest[is.na(X)] )

row.names(X.compare) <- c('Mean', 'RegressionImputation', 'KNN', 'Mice', 'MissForest')

X.compare

temp <- X
temp[,10] <- NA
names(temp)[10] <- "Index"
for (i in 1:nrow(temp))
  {
    temp[i,10] <- i
  }
#Plotting Rent data imputed to see the comparation
ggplot(data = X.knn, mapping = aes(y = X.knn$Rent, x = temp$Index)) + 
  geom_point(data=X, aes(y = X.knn$Rent, x = temp$Index, colour="Data") , size=1) +
  geom_point(data=X.Mean, aes(y = X.Mean$Rent[2], x = temp$Index[2], colour="Mean"), size=3) +
  geom_point(data=X.Mean, aes(y = X.Mean$Rent[30], x = temp$Index[30], colour="Mean"), size=3) +
  geom_point(data=X.Mean, aes(y = X.Mean$Rent[35], x = temp$Index[35], colour="Mean"), size=3) +
  geom_point(data=X.RegressionImputation, aes(y = X.RegressionImputation$Rent[2], x = temp$Index[2], colour="Regression"), size=3) +
  geom_point(data=X.RegressionImputation, aes(y = X.RegressionImputation$Rent[30], x = temp$Index[30], colour="Regression"), size=3) +
  geom_point(data=X.RegressionImputation, aes(y = X.RegressionImputation$Rent[35], x = temp$Index[35], colour="Regression"), size=3) +
  geom_point(data=X.knn, aes(y = X.knn$Rent[2], x = temp$Index[2], colour="Knn"), size=3) +
  geom_point(data=X.knn, aes(y = X.knn$Rent[30], x = temp$Index[30], colour="Knn"), size=3) +
  geom_point(data=X.knn, aes(y = X.knn$Rent[35], x = temp$Index[35], colour="Knn"), size=3) +
  geom_point(data=X.missForest, aes(y = X.missForest$Rent[2], x = temp$Index[2], colour="MissForest"), size=3) +
  geom_point(data=X.missForest, aes(y = X.missForest$Rent[30], x = temp$Index[30], colour="MissForest"), size=3) +
  geom_point(data=X.missForest, aes(y = X.missForest$Rent[35], x = temp$Index[35], colour="MissForest"), size=3) +
  geom_point(data=X.mice, aes(y = X.mice$Rent[2], x = temp$Index[2], colour="Mice"), size=3) +
  geom_point(data=X.mice, aes(y = X.mice$Rent[30], x = temp$Index[30], colour="Mice"), size=3) +
  geom_point(data=X.mice , aes(y = X.mice$Rent[35], x = temp$Index[35], colour="Mice"), size=3) +
  xlab("Index") + ylab("Rent") +
  ggtitle("Imputation of the Rent Values")

#Plotting ecks data imputed to see the comparation
ggplot(data = X.knn, mapping = aes(y = X.knn$ecks, x = temp$Index)) + 
  geom_point(data=X, aes(y = X.knn$ecks, x = temp$Index, colour="Data") , size=1) +
  geom_point(data=X.Mean, aes(y = X.Mean$ecks[31], x = temp$Index[31], colour="Mean"), size=3) +
  geom_point(data=X.RegressionImputation, aes(y = X.RegressionImputation$ecks[31], x = temp$Index[31], colour="Regression"), size=3) +
  geom_point(data=X.knn, aes(y = X.knn$ecks[31], x = temp$Index[31], colour="Knn"), size=3) +
  geom_point(data=X.missForest, aes(y = X.missForest$ecks[31], x = temp$Index[31], colour="MissForest"), size=3) +
  geom_point(data=X.mice, aes(y = X.mice$ecks[31], x = temp$Index[31], colour="Mice"), size=3) +
  xlab("Index") + ylab("ecks") +
  ggtitle("Imputation of the ecks Values")


###############################################################################################
#Exercise 2
###############################################################################################

X.outliers <- X.missForest


boxplot(X.outliers$Gini, horizontal = TRUE, col = 'red')

boxplot(X.outliers$farm, horizontal = TRUE, col = 'red')

boxplot(X.outliers$Rent, horizontal = TRUE, col = 'red')
#Repeat as many times as needed
X.outliers$Rent[X.outliers$Rent > summary(X.outliers$Rent)[5]+1.5*(summary(X.outliers$Rent)[5]- summary(X.outliers$Rent)[2])] <- NA
X.outliers <- missForest(X.outliers)$ximp
boxplot(X.outliers$Rent, horizontal = TRUE, col = 'red')

boxplot(X.outliers$Gnpr, horizontal = TRUE, col = 'red')
#Repeat as many times as needed
X.outliers$Gnpr[X.outliers$Gnpr > summary(X.outliers$Gnpr)[5]+1.5*(summary(X.outliers$Gnpr)[5]- summary(X.outliers$Gnpr)[2])] <- NA
X.outliers <- missForest(X.outliers)$ximp
boxplot(X.outliers$Gnpr, horizontal = TRUE, col = 'red')

boxplot(X.outliers$Laboagr, horizontal = TRUE, col = 'red')

boxplot(X.outliers$Instab, horizontal = TRUE, col = 'red')
X.outliers$Instab[X.outliers$Instab < summary(X.outliers$Instab)[2]-1.5*(summary(X.outliers$Instab)[5] - summary(X.outliers$Instab))[2]] <- NA
X.outliers <- missForest(X.outliers)$ximp
boxplot(X.outliers$Instab, horizontal = TRUE, col = 'red')

boxplot(X.outliers$Death, horizontal = TRUE, col = 'red')
#Repeat as many times as needed.
X.outliers$Death[X.outliers$Death > summary(X.outliers$Death)[5]+1.5*(summary(X.outliers$Death)[5]- summary(X.outliers$Death)[2])] <- NA
X.outliers <- missForest(X.outliers)$ximp
boxplot(X.outliers$Death, horizontal = TRUE, col = 'red')




X.multivariantOutliers <- X.missForest
temp2 <- Moutlier(X.multivariantOutliers, quantile = 0.995, plot = FALSE)
X.multivariantOutliers <- cbind(X.multivariantOutliers, temp2$md, temp2$rd, temp2$cutoff)
names(X.multivariantOutliers)[10] <- "Classical" 
names(X.multivariantOutliers)[11] <- "Robust" 
names(X.multivariantOutliers)[12] <- "Cutoff"




ggplot(data = X.multivariantOutliers ,mapping = aes(y = X.multivariantOutliers$Classical, x = temp$Index, label = rownames(X.multivariantOutliers))) +
  geom_point(data= X.multivariantOutliers, aes(y = X.multivariantOutliers$Classical, x = temp$Index, colour="Data") , size=1) +
  xlab("Index") + ylab("Classical") +
  geom_line(mapping = aes(y= X.multivariantOutliers$Cutoff)) +
  geom_text(hjust=0, vjust=0) +
  ggtitle("Mahalanobis Distance")


ggplot(data = X.multivariantOutliers ,mapping = aes(y = X.multivariantOutliers$Robust, x = temp$Index, label = rownames(X.multivariantOutliers) )) +
  geom_point(data= X.multivariantOutliers, aes(y = X.multivariantOutliers$Robust, x = temp$Index, colour="Data") , size=1) +
  xlab("Index") + ylab("Robust") +
  geom_line(mapping = aes(y= 1000)) +
  geom_text(hjust=0, vjust=0) +
  ggtitle("Robust Distance")



ggplot(data = X.multivariantOutliers ,mapping = aes(y = X.multivariantOutliers$Robust, x = X.multivariantOutliers$Classical, label = rownames(X.multivariantOutliers))) +
  geom_point(data= X.multivariantOutliers, aes(y = X.multivariantOutliers$Robust, x = X.multivariantOutliers$Classical, colour="Data") , size=1) +
  xlab("Classical distance") + ylab("Robust distance") +
  geom_line(mapping = aes(y= X.multivariantOutliers$Cutoff)) +
  geom_line(mapping = aes(x = X.multivariantOutliers$Cutoff)) +
  geom_text(hjust=0, vjust=0) +
  ggtitle("Mahalanobis Distance vs Robust distance")


outlier.scores <- lofactor(X.multivariantOutliers, k=5)
plot(density(outlier.scores))
# pick top 5 outliers
outliers <- order(outlier.scores, decreasing=T)[1:5]
# who are outliers
print(outliers)

nameOutliers<- list()
for (i in 1:nrow(X.multivariantOutliers))
{
  if (i %in% outliers) nameOutliers <- rbind(nameOutliers, row.names(X.multivariantOutliers)[i])
}
nameOutliers

