zotu.rel<-  transform_sample_counts(zotu97, function(x) x / sum(x))
#SUBSET BY TIME
zotu.rel.t0<- subset_samples(zotu.rel, time== "T0")
zotu.rel.t5<- subset_samples(zotu.rel, time== "T5")
zotu.rel.t0.t5 <- subset_samples(zotu.rel, time =="T0" | time =="T5")

##### T0 + T5
t.otu <- zotu.rel.t0.t5 %>% otu_table() %>% data.frame() %>% t()
t.otu.clr <- decostand(t.otu, method = "clr", pseudocount = 1)
pca.clr <- prcomp(t.otu.clr, scale=TRUE)
scores <- data.frame(pca.clr$x)
t0.t5.pca <- data.frame(scores, sample_data(zotu.rel.t0.t5))
variance_perc <- pca.clr$sdev^2 / sum(pca.clr$sdev^2) * 100 

pca.plot.t0.t5.treat <- ggplot(t0.t5.pca, aes(PC1, PC2, col= treatment, shape= time))+
  geom_point(size= 3)+
  scale_color_manual(values= treat.colors)+
  scale_shape_manual(values=c(1,16))+
  labs(title= "T0 and T5 CLR transformed",
       x= sprintf("PC1 (%.1f%%)", variance_perc[1]), 
       y= sprintf("PC2 (%.1f%%)", variance_perc[2]))+
  stat_ellipse(aes(group =treatment, fill= treatment), level = 0.95, geom = "polygon", alpha = 0.2)+
  scale_fill_manual(values= treat.colors)
pca.plot.t0.t5.treat

pca.plot.t0.t5.time <- ggplot(t0.t5.pca, aes(PC1, PC2, col= treatment, shape= time))+
  geom_point(size= 3)+
  scale_color_manual(values= treat.colors)+
  scale_shape_manual(values=c(1,16))+
  labs(title= "T0 and T5 CLR transformed",
       x= sprintf("PC1 (%.1f%%)", variance_perc[1]), 
       y= sprintf("PC2 (%.1f%%)", variance_perc[2]))+
  stat_ellipse(aes(group =time, fill= time), level = 0.95, geom = "polygon", alpha = 0.2)
pca.plot.t0.t5.time


##### T0 
t.otu <- zotu.rel.t0 %>% otu_table() %>% data.frame() %>% t()
t.otu.clr <- decostand(t.otu, method = "clr", pseudocount = 1)
pca.clr <- prcomp(t.otu.clr, scale=TRUE)
scores <- data.frame(pca.clr$x)
t0.pca <- data.frame(scores, sample_data(zotu.rel.t0))
variance_perc <- pca.clr$sdev^2 / sum(pca.clr$sdev^2) * 100 

pca.plot.t0 <- ggplot(t0.pca, aes(PC1, PC2, col= treatment))+
  geom_point(size= 3, shape= 1)+
  scale_color_manual(values= treat.colors)+
  labs(title= "T0 CLR transformed",
       x= sprintf("PC1 (%.1f%%)", variance_perc[1]), 
       y= sprintf("PC2 (%.1f%%)", variance_perc[2]))+
  geom_polygon(aes(fill =  treatment, group = interaction(time, treatment)),alpha= 0.5)+
  scale_fill_manual(values= treat.colors)
pca.plot.t0


##### T5
t.otu <- zotu.rel.t5 %>% otu_table() %>% data.frame() %>% t()
t.otu.clr <- decostand(t.otu, method = "clr", pseudocount = 1)
pca.clr <- prcomp(t.otu.clr, scale=TRUE)
scores <- data.frame(pca.clr$x)
t5.pca <- data.frame(scores, sample_data(zotu.rel.t5))
variance_perc <- pca.clr$sdev^2 / sum(pca.clr$sdev^2) * 100 

pca.plot.t5 <- ggplot(t5.pca, aes(PC1, PC2, col= treatment))+
  geom_point(size= 3, shape= 16)+
  scale_color_manual(values= treat.colors)+
  labs(title= "T5 CLR transformed",
       x= sprintf("PC1 (%.1f%%)", variance_perc[1]), 
       y= sprintf("PC2 (%.1f%%)", variance_perc[2]))+
  geom_polygon(aes(fill =  treatment, group = interaction(time, treatment)),alpha= 0.5)+
  scale_fill_manual(values= treat.colors)
pca.plot.t5

pca.multiplot <- grid.arrange(pca.plot.t0 + guides(fill= "none", col= "none"), 
                              pca.plot.t5+ guides(fill= "none", col= "none"),
                              pca.plot.t0.t5.treat+ guides(fill= "none", col= "none")
                              ,pca.plot.t0.t5.time+guides(fill= "none", alpha = "none")+theme_classic()+theme(legend.position = "bottom"),
                              layout_matrix= rbind(c(1,2), c(3,3), c(4,4)))

ggsave(pca.multiplot, file= "ITS_results/PCA/pca_all.png")
