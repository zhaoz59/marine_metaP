library(data.table)
library(tidyverse)
library("matrixStats")

## download the data from https://figshare.com/s/51443828067e75643f61/KO_rel_abundance_table.zip
#load data
ko.metaP=fread('ko.metaP.csv')
ko.metaP[1:6,1:6]

ko.metaP.matrix=ko.metaP%>%
  tibble::column_to_rownames(.,"sample")%>%
  select(-1,-2,-3)%>%
  as.matrix()

ko.metaP.matrix%>%dim

#using KO with relative abundance >1% in at least one sample to build the model
#skip this step to use all KOs
#result is much similar

ko.metaP.matrix.trim=ko.metaP.matrix[,!colMaxs(ko.metaP.matrix)<1]

ko.metaP.fraction.trim=ko.metaP.matrix.trim%>%
  as.data.frame()%>%
  tibble::rownames_to_column('sample')%>%
  left_join(.,ko.metaP[,c(1,3)]%>%unique())%>%
  select(-sample)

ko.metaP.fraction.trim$fraction=factor(ko.metaP.fraction.trim$fraction)


library('randomForest')
library(caret)

#split the data for training (70%) and testing (30%)
set.seed(222)
ind <- sample(2, nrow(ko.metaP.fraction.trim), replace = TRUE, prob = c(0.7, 0.3))
train.trim <- ko.metaP.fraction.trim[ind==1,]
test.trim <- ko.metaP.fraction.trim[ind==2,]

#choose the number for decision tree
# Create a sequence of ntree values to test
ntree_values <- c(100, 200, 500, 1000, 1500)
# Create empty vector to store OOB errors
oob_errors <- numeric(length(ntree_values))

# Loop through ntree values
for (i in seq_along(ntree_values)) {
  model <- randomForest(fraction~., data=train.trim, importance=TRUE, proximity=TRUE, ntree = ntree_values[i])
  # Extract the OOB error from the model
  oob_errors[i] <- mean(model$err.rate[, "OOB"])
}

# Print OOB errors 
print(oob_errors)


#build the RF model
rf.trim <- randomForest(fraction~., data=train.trim, importance=TRUE, proximity=TRUE, ntree = 1000)

print(rf.trim)

#show the importance of features
print(importance(rf.trim,type = 1)) #MeanDecreaseAccuracy
print(importance(rf.trim,type = 2)) #MeanDecreaseGini

#test the cross-validated prediction error of models with reduced numbers of features (importance from low to high)
result.trim = rfcv( train.trim[,-232], train.trim$fraction, cv.fold=10)
data.frame(error.cv=result.trim$error.cv)%>%tibble::rownames_to_column('var')
with(result.trim, plot(n.var, error.cv, log="x", type="o", lwd=2))

#exam model accuracy

pred.trim=predict(rf.trim, test.trim)

confusionMatrix(pred.trim, test.trim$fraction)

#write down classification rules for each tree

# Create a directory to save the rules
dir.create("rf_rules", showWarnings = T)

# Loop through all trees
for (i in 1:1000) {
  # Extract rules from the i-th tree
  tree <- getTree(rf.trim, k = i, labelVar = TRUE)
  
  # Save rules to a text file
  rules <- capture.output(print(tree))
  #rules <- rules[grepl("if \\(", rules)]
  writeLines(rules, file.path("rf_rules", paste0("tree_", i, "_rules.txt")))
}

