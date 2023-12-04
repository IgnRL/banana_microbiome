big.data <- read_delim(file= here("ITS_banana","metadata_full.csv" ),show_col_types = FALSE)


big.data <- subset(big.data, big.data$time == "T0" | big.data$time == "T2" |big.data$time == "T3") %>% data.frame()
info <- big.data[, c(1:5)]
big.data <- big.data[, -c(1:5)]

big.data$colonisation <- NULL
big.data$spore_amf <- NULL

numeric.col <-  sapply(big.data, is.numeric)
pca.data <- scale(big.data[, numeric.col])
pca_result <- prcomp(pca.data, center = TRUE, scale. = TRUE)
summary(pca_result)
pca.scores <- pca_result$x %>% data.frame()

pca <- data.frame(pca.scores,info)

pca %>% ggplot(aes(PC1, PC2, col= treatment, shape= time))+
  geom_point()+
  geom_label(aes(label= rep))+
  geom_polygon(aes(group= interaction(treatment, time), fill= treatment, alpha= time))+
  scale_fill_manual(values=treat.colors)+
  scale_color_manual(values=treat.colors)
  #stat_ellipse(aes(group= interaction(time, treatment), col= treatment))
ggbiplot(pca_result,groups = info$treatment, ellipse = TRUE, circle = TRUE)+
  scale_color_manual(values= treat.colors)




data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
print(ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = wine.class, ellipse = TRUE, circle = TRUE))
