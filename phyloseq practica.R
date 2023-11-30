import <- here("ITS_banana", "p1031_run231023_ITS_ccs_ZOTU_c97_Count_Sintax.txt")
zotu97 <- import_qiime(otufilename= import)
otu_table(zotu97)
tax_table(zotu97)

metadata <- read_delim(file= here("ITS_banana","metadata.csv" ),show_col_types = FALSE) %>% 
  column_to_rownames(var= "sample_name")

zotu97 <- merge_phyloseq(zotu97, sample_data(metadata))
otu.table <- otu_table(zotu97) %>% data.frame()

sum(otu.table$S001)
sample.data <- sample_data(zotu97) %>% data.frame()
taxa.data <- tax_table(zotu97) %>% data.frame()

order.list <- taxa.data[, "Order"] %>% data.frame()
unique(order.list$Order) %>% length()

unknown_seqs <- tax_table(zotu97) %>% as.data.frame()
unknown_seqs <-unknown_seqs[is.na(unknown_seqs$Phylum),]
nrow(unknown_seqs)
nrow(tax_table(zotu97))
unknown_OTUs <- row.names(unknown_seqs) %>% as.vector()

numFungi <- kingdom_list[kingdom_list== "Fungi"] %>% length()



dat_merge_all_tax <- tax_glom(physeq = zotu97, "Genus", NArm = TRUE)
tax_table(dat_merge_all_tax)
molten_dat_all_merge <- psmelt(dat_merge_all_tax)
p_phyla <- ggplot(molten_dat_all_merge, 
                  aes(x = Kingdom,y = Abundance/sum(Abundance)*100,fill=Genus)) + 
  geom_col() +
  xlab("Kingdom") + 
  ylab("Relative abundance (% of total sequences)") +
  theme_minimal() +
  ggtitle("Phyla representation in the total of sequences (Unassigned sequences removed)")+
  #theme(axis.text.x=element_blank(), axis.title.x=element_blank())+
  theme(legend.position="bottom")
p_phyla



trans_zotu97 <- t(otu_table(zotu97)) %>% data.frame() #transpose the otu table
rarecurve97 <- rarecurve(trans_zotu97, step=1000, xlab= "number of sequences", ylab="number of OTUs", label = FALSE, col= 2)
plot_rare <- rarecurve97 %>% 
  purrr::map(~ tibble(
    y = as.vector(.x),
    x = unname(attr(.x, "Subsample")),
    steps = names(.x))) %>%
  tibble(data = ., sample = seq_along(.)) %>%
  unnest(data)

stopifnot(identical(sort(unique(plot_rare$sample)), 1:NROW(sample.table97))) #plot_data$sample and sample_data(zotu97) must be identical
sample.table97 <- sample.table97[sample.table97$treatment != "NTC", ] %>% .[.$treatment != "CTL", ]
zotu97 <- subset_samples(zotu97, treatment != "NTC" & treatment != "CTL")
plot_rare$treat <- (sample.table97$treatment)[plot_rare$sample]
plot_rare$time <- (sample.table97$time)[plot_rare$sample]

pp_treat <- plot_rare %>% 
  ggplot() +
  geom_line(aes(x,y, color = treat, group = sample),lwd= 1) +
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
  ylab("Number of OTUs")+
  xlab("Number of sequences") +
  ggtitle("Rarefaction curve by treatment")+
  geom_textvline(xintercept = min(sample.table97$seq.depth), label = paste(min(sample.table97$seq.depth), "= min seq depth"))+
  coord_cartesian(xlim = c(0, 60000), ylim = c(0, 450))+scale_color_manual(values= treat.colors)
pp_treat
ggsave(pp_treat, file= "ITS_results/rare_treat.png")

pp_time <- plot_rare %>% 
  ggplot() +
  geom_line(aes(x,y, color = time, group = sample),lwd= 1) +
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
  ylab("Number of OTUs")+
  xlab("Number of sequences") +
  ggtitle("Rarefaction curve by time")+
  geom_textvline(xintercept = min(sample.table97$seq.depth), label = paste(min(sample.table97$seq.depth), "= min seq depth"))+
  coord_cartesian(xlim = c(0, 60000), ylim = c(0, 450))+scale_color_manual(values= time.colors)
pp_time
ggsave(pp_time, file= "ITS_results/rare_time.png")


#My own shannon function
shannon.iro <- function(x){
  fi <- x[x>0]/sum(x) #frequency of species "i" is equal to count (if > 0) divided by the sum of all counts
  -sum(fi*log(fi)) #shannon index is the negative of the sum of all the frequencies of all the species times the log of those species
}

#create empty vectors
richness_500 <- c()
diversity_500 <- c()
even_500 <- c()
for (i in 1:500){
  #generate rarefied community
  temp_500 <-  rarefy_even_depth(otu_table(zotu97), rngseed = 22081998, sample.size = min(sample.table97$seq.depth))
  #  richness is species number
  s <- specnumber(temp_500, MARGIN=2)
  richness_500 <- cbind(richness_500,s)
  
  #diverasity uses my own formula
  shannon<- apply(temp_500,2, shannon.iro)
  diversity_500 <- cbind(diversity_500,shannon)
  #evenness
  j <- shannon/s
  even_500 <- cbind(even_500,j)
} 

#calculate means of subsamples
shannon <- rowMeans(diversity_500)
richness <- rowMeans(richness_500)
evenness <- rowMeans(even_500)

# put them all in one dataframe
summary_diversity_rar <- data.frame( cbind( shannon, richness, evenness))
stopifnot(identical(rownames(sample_data(zotu97)), names(richness)))

sample.table97.500<- cbind(summary_diversity_rar, sample_data(zotu97))
save(sample.table97.500, file=here("ITS_banana", "sample.table97.500.Rdata"))
#save the results of the loop in an object to avoid wasting time in each run, if needed to re-run, change the chunk option to "eval= TRUE"


zotu.control <- subset_samples(zotu97, treatment== "control")
zotu.control <- prune_samples(zotu97, treatment== "control")
