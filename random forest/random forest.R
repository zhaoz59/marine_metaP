library(data.table)
library(tidyverse)

ko.metaP=fread('metaP.ko.relative.abundance.csv')
ko.metaP[1:6,1:6]


library("matrixStats")

ko.metaP.matrix=ko.metaP%>%
  spread(.,ko,rel,fill=0)%>%
  tibble::column_to_rownames(.,"sample")%>%
  select(-1,-2,-3)%>%
  as.matrix()

ko.metaP.matrix%>%dim
#use KO >1% for quick run
ko.metaP.matrix[,!colMaxs(ko.metaP.matrix)<1]%>%dim

ko.metaP.matrix.trim=ko.metaP.matrix[,!colMaxs(ko.metaP.matrix)<1]

ko.metaP.fraction.trim=ko.metaP.matrix.trim%>%
  as.data.frame()%>%
  tibble::rownames_to_column('sample')%>%
  left_join(.,ko.metaP[,c(1,3)]%>%unique())%>%
  select(-sample)

ko.metaP.fraction.trim$fraction=factor(ko.metaP.fraction.trim$fraction)


library('randomForest')
library(caret)
ind <- sample(2, nrow(ko.metaP.fraction.trim), replace = TRUE, prob = c(0.7, 0.3))
train.trim <- ko.metaP.fraction.trim[ind==1,]
test.trim <- ko.metaP.fraction.trim[ind==2,]

rf.trim <- randomForest(fraction~., data=train.trim, importance=TRUE, proximity=TRUE, ntree = 1000)

print(rf.trim)

print(importance(rf.trim,type = 2)) 


result.trim = rfcv( train.trim[,-232], train.trim$fraction, cv.fold=10)
data.frame(error.cv=result.trim$error.cv)%>%tibble::rownames_to_column('var')


with(result.trim, plot(n.var, error.cv, log="x", type="o", lwd=2))
varImpPlot(rf.trim,n.var =14,type = 2 )

imp.trim= as.data.frame(importance(rf.trim))%>%tibble::rownames_to_column('ko')

ko.imp=imp.trim[order(imp.trim[,5],decreasing = T),]%>%head(n=14)%>%pull(ko)

ko.imp.abd.fraction=ko.metaP%>%filter(ko%in%ko.imp)%>%
  aggregate(data=.,rel~sample+fraction+ko,sum)

ko.imp.abd.depth=ko.metaP%>%filter(ko%in%ko.imp)%>%
  aggregate(data=.,rel~sample+depth+ko,sum)
