library(data.table)
library(tidyverse)
library(ggpubr)

#please download the data from https://figshare.com/s/51443828067e75643f61/KO_rel_abundance_table.zip

#load data
ko.metaP=fread('ko.metaP.csv')%>%
  gather(.,'ko','rel',-1,-2,-3,-4)

#create vectors for pair-wise comparison either between size-fractions or depth

fraction.list=list(c("PA","FL"),c("FL","Exo"),c("PA","Exo"))

depth.list=list(c("Epi","Meso"),c("Meso","Bathy"),c("Epi","Bathy"))

#Pair-wise comparison between size-fractions
ko.fraction.stats=compare_means(rel~fraction,data=ko.metaP[,c(3,5,6)],
                                comparison =fraction.list,group.by ='ko')

#KOs differentially abundant between size-fractions

ko.fraction.diff=ko.fraction.stats%>%filter(p.signif!='ns')



#Pair-wise comparison between size-fractions
ko.depth.stats=compare_means(rel~depth,data=ko.metaP[,c(4,5,6)],
                             comparison =depth.list,group.by ='ko')

#KOs differentially abundant between depths

ko.depth.diff=ko.depth.stats%>%filter(p.signif!='ns')

#Show KOs differentially abundant not only between size-fractions but also between depths

intersect(unique(ko.fraction.diff$ko),unique(ko.depth.diff$ko))

#Show KOs differentially abundant  only between size-fractions or between depths

setdiff(unique(ko.fraction.diff$ko),unique(ko.depth.diff$ko))

setdiff(unique(ko.depth.diff$ko),unique(ko.fraction.diff$ko))

rm(list = ls())
