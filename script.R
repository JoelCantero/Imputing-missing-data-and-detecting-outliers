#install.packages('mice')
library(mice)

install.packages('DMwR')
library(DMwR)

install.packages('missForest')
library(missForest)

X <- read.table('Russet_ineqdata.txt', header=T, sep='\t', row.names=1)
X

md.pattern(X)
X.knn <- knnImputation(X, k = 7)

X.mice <- complete(mice(X, m = 1), 1)
row.names(X.mice) <- row.names(X)

X.missForest <- missForest(X)$ximp

X.compare <- rbind(X.knn[is.na(X)], X.mice[is.na(X)], X.missForest[is.na(X)])

row.names(X.compare) <- c('knn', 'mice', 'missForest')



