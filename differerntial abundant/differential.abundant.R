library(data.table)
library(tidyverse)

ko.metaP=fread('ko.metaP.csv')

library(ggpubr)
fraction.list=list(c("PA","FL"),c("FL","Exo"),c("PA","Exo"))
depth.list=list(c("Epi","Meso"),c("Meso","Bathy"),c("Epi","Bathy"))

ko.fraction.stats=compare_means(rel~fraction,data=ko.metaP[,c(3,5,6)],
                                comparison =fraction.list,group.by ='ko')



ko.depth.stats=compare_means(rel~depth,data=ko.metaP[,c(4,5,6)],
                             comparison =depth.list,group.by ='ko')


