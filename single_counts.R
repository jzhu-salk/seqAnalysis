library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)

gene = 'ENSG00000170421'
gene = row.names(gene_mat)[grep(paste0('^', gene), row.names(gene_mat))]

gene_subset_norm_counts = dds_final %>% counts(normalized=T) %>% data.frame %>%
  filter(row.names(.) %in% gene) %>%
  t %>% data.frame %>%
  cbind.data.frame(condition = row.names(.) %>% sub('[0-9].*', '',.))
colnames(gene_subset_norm_counts) = colnames(gene_subset_norm_counts) %>% gsub("\\..*", "", .)
symbols = mapIds(org.Hs.eg.db, keys = colnames(gene_subset_norm_counts), column="SYMBOL", keytype="ENSEMBL", multiVals="first")
colnames(gene_subset_norm_counts) = paste(colnames(gene_subset_norm_counts), symbols, sep='_')

single = function(index, df){
  gene = colnames(df)[index]
  p = ggplot(df, mapping = aes(x=condition_NA, y = .data[[colnames(df)[index]]])) +
    geom_boxplot(outliers = F) +
    labs(title = paste0(gene, ' Expression'),
         x = element_blank(),
         y = 'Counts') +
    # geom_point() +
    geom_jitter(width = .1) +
    geom_text(aes(label = row.names(df)), nudge_x = .25, size = 2) +
    theme_minimal()
  return(p)
}

p1 = single(1, gene_subset_norm_counts) 
p2 = single(1, gene_subset_norm_counts)
# p3 = single(3, highly_de_gene_counts) 
# p4 = single(4, highly_de_gene_counts) 
# p5 = single(5, highly_de_gene_counts) 
# p6 = single(6, highly_de_gene_counts) 
# p7 = single(7, highly_de_gene_counts) 
# p8 = single(8, highly_de_gene_counts) 
# p9 = single(9, highly_de_gene_counts) 
# p10 = single(10, highly_de_gene_counts)
ggarrange(p1, p2, nrow = 2)
