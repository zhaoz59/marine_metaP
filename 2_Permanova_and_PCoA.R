library(data.table)
library(tidyverse)
library(vegan)
library(ggplot2)
## analysis for KO profile in metagenomic, metatranscriptomic and metaproteomic samples
## download the data from https://figshare.com/s/51443828067e75643f61/KO_rel_abundance_table.zip


#load data
ko.all=fread('ko.all.csv')%>%rename(sample=V1)

ko.all[1:5,1:5]  

#permanova test
permanova=adonis2(ko.all[,-c(1,2)]~ type, data = ko.all[,c(1,2)])
permanova

#build PCoA object
ko.all.pcoa=cmdscale(vegdist(ko.all[,-c(1,2)]), k=3, eig=T)
ko.all.pcoa.eig <-ko.all.pcoa$eig

#visualization
cbind(ko.all[,c(1,2)],ko.all.pcoa$points%>%as.data.frame())%>%
  ggplot(data=., aes(x=V1, y=V2,color=type,size=type))+
  geom_point(alpha=.7) +
  scale_size_manual(values = c(1,1,2,1,1))+
  scale_color_manual(values = c("#868686FF","#003C67FF","#EFC000FF","#CD534CFF","#7AA6DCFF"))+
  labs(y=paste("PCoA 1 (", format(100 * ko.all.pcoa.eig[1] / sum(ko.all.pcoa.eig), digits=4), "%)", sep=""),
       x=paste("PCoA 2 (", format(100 * ko.all.pcoa.eig[2] / sum(ko.all.pcoa.eig), digits=4), "%)", sep=""),
       title=NULL)+theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = 'none')+
  annotate('text',x=c(-0.2,-0.1,0.25,0.4,0.42),
           y=c(-0.15,0.10,0.15,0,-0.15),size=c(3,3,3,3,3),
           label = c("Metatranscriptome\n(Euk)", "Metagenome\n(Euk)",
                     "Metaproteome","Metatranscriptome\n(Prok)", "Metagenome\n(Prok)"),
           color=c("#003C67FF","#868686FF","#EFC000FF","#7AA6DCFF","#CD534CFF"),
           angle=c(320,0,0,30,30))

rm(list = ls())

## analysis for KO profile in metaproteomic samples
## download the data from https://figshare.com/s/51443828067e75643f61/KO_rel_abundance_table.zip

#load data
ko.metaP=fread('ko.metaP.csv')%>%tibble::column_to_rownames(.,"sample")

#permanova test
permanova=adonis2(ko.metaP[,-c(1,2,3)]~ fraction, data = ko.metaP[,1:3])
permanova

#build PCoA object
ko.metaP.pcoa=cmdscale(vegdist(ko.metaP[,-c(1:3)]), k=3, eig=T)

ko.metaP.pcoa.eig <-ko.metaP.pcoa$eig

#extract data from PCoA object

ko.metaP.pcoa.point=ko.metaP.pcoa$points%>%
  as.data.frame()%>%tibble::rownames_to_column(.,"sample")%>%
  left_join(.,ko.metaP[,c(1:3)]%>%as.data.frame()%>%tibble::rownames_to_column(.,"sample"),by='sample')%>%
  mutate(depth=factor(depth,levels = c("Epi","Meso","Bathy")),
         fraction=factor(fraction,levels = c("PA","FL","Exo")))

#build the polygon for each fraction
hull.pa <- ko.metaP.pcoa.point[ko.metaP.pcoa.point$fraction == "PA", ][chull(ko.metaP.pcoa.point[ko.metaP.pcoa.point$fraction == 
                                                                                                   "PA", c("V1", "V2")]), ] 

hull.fl <- ko.metaP.pcoa.point[ko.metaP.pcoa.point$fraction == "FL", ][chull(ko.metaP.pcoa.point[ko.metaP.pcoa.point$fraction == 
                                                                                                   "FL", c("V1", "V2")]), ] 

hull.exo <- ko.metaP.pcoa.point[ko.metaP.pcoa.point$fraction == "Exo", ][chull(ko.metaP.pcoa.point[ko.metaP.pcoa.point$fraction == 
                                                                                                     "Exo", c("V1", "V2")]), ] 

hull=rbind(hull.exo,hull.fl,hull.pa)


#Visualization
p.ko.metaP.pcoa=ggplot()+
  geom_point(data=ko.metaP.pcoa.point, aes(x=V1, y=V2,color=fraction,shape=depth),size=3)+
  geom_polygon(data=hull,aes(x=V1, y=V2,fill=fraction),alpha=0.3)+
  scale_fill_npg()+scale_color_npg()+theme_bw()+
  #annotate('text',x=0.2,y=0.25,label=c('PERMANOVA p<0.01'))+
  labs(y=paste("PCoA 1 (", format(100 * ko.metaP.pcoa.eig[1] / sum(ko.metaP.pcoa.eig), digits=4), "%)", sep=""),
       x=paste("PCoA 2 (", format(100 * ko.metaP.pcoa.eig[2] / sum(ko.metaP.pcoa.eig), digits=4), "%)", sep=""),
       title=NULL)+
  theme(panel.grid = element_blank())

p.ko.metaP.pcoa

rm(list=ls())