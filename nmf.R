library(RcppML)
library(pheatmap)


norm_counts = dds_final %>% counts(normalized = T) 
annotations = colnames(norm_counts) %>%
  data.frame(row.names = ., 
             Condition = sub("\\d+.*", "", .))

model = nmf(norm_counts, 8, seed=1)
distribution = model$h
colnames(distribution) = colnames(dds_final)

pheatmap(mat = distribution, 
         main = paste0("NMF ", nrow(distribution), " Ranks"),
         cluster_cols = T, 
         cluster_rows = F, 
         annotation_col = annotations,
         labels_row = seq(nrow(distribution)))