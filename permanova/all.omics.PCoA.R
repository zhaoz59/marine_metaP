library(data.table)
library(tidyverse)
library(vegan)
library(ggplot2)
ko.all=fread('ko.all.csv')%>%rename(sample=V1)

ko.all[1:5,1:5]  
permanova=adonis2(ko.all[,-c(1,2)]~ type, data = ko.all[,c(1,2)])
permanova

ko.all.pcoa=cmdscale(vegdist(ko.all[,-c(1,2)]), k=3, eig=T)
ko.all.pcoa.eig <-ko.all.pcoa$eig

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
