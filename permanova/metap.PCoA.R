library(data.table)
library(tidyverse)
library(vegan)
library(ggplot2)
library(ggsci)
ko.metaP=fread('ko.metaP.csv')%>%tibble::column_to_rownames(.,"sample")
permanova=adonis2(ko.metaP[,-c(1,2,3)]~ fraction, data = ko.metaP[,1:3])
permanova

ko.metaP.pcoa=cmdscale(vegdist(ko.metaP[,-c(1:3)]), k=3, eig=T)

ko.metaP.pcoa.eig <-ko.metaP.pcoa$eig

ko.metaP.pcoa.point=ko.metaP.pcoa$points%>%
  as.data.frame()%>%tibble::rownames_to_column(.,"sample")%>%
  left_join(.,ko.metaP[,c(1:3)]%>%as.data.frame()%>%tibble::rownames_to_column(.,"sample"),by='sample')%>%
  mutate(depth=factor(depth,levels = c("Epi","Meso","Bathy")),
         fraction=factor(fraction,levels = c("PA","FL","Exo")))

hull.pa <- ko.metaP.pcoa.point[ko.metaP.pcoa.point$fraction == "PA", ][chull(ko.metaP.pcoa.point[ko.metaP.pcoa.point$fraction == 
                                                                                                   "PA", c("V1", "V2")]), ] 

hull.fl <- ko.metaP.pcoa.point[ko.metaP.pcoa.point$fraction == "FL", ][chull(ko.metaP.pcoa.point[ko.metaP.pcoa.point$fraction == 
                                                                                                   "FL", c("V1", "V2")]), ] 

hull.exo <- ko.metaP.pcoa.point[ko.metaP.pcoa.point$fraction == "Exo", ][chull(ko.metaP.pcoa.point[ko.metaP.pcoa.point$fraction == 
                                                                                                     "Exo", c("V1", "V2")]), ] 

hull=rbind(hull.exo,hull.fl,hull.pa)

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
