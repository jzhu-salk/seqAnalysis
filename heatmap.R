library(pheatmap)

get_heat_map <- function(df, dds){
  df_top = union(df %>% filter(Cutoff == 'dec' & !is.na(Symbol)) %>% 
                   arrange(log2FoldChange) %>% head(50) %>% dplyr::select(Symbol),
                 df %>% filter(Cutoff == 'inc' & !is.na(Symbol)) %>% 
                   arrange(log2FoldChange) %>% head(50) %>% arrange(log2FoldChange) %>% dplyr::select(Symbol))
  df_top = setNames(as.character(df_top$Symbol), row.names(df_top))
  
  df_heat_map = dds %>% counts(normalized=T) %>% data.frame %>%
    filter(row.names(.) %in% names(df_top)) %>%
    arrange(factor(row.names(.), levels = names(df_top)))
  rownames(df_heat_map) = df_top[rownames(df_heat_map)]
  
  annotations = colnames(dds) %>%
    data.frame(row.names = ., 
               Condition = sub("\\d.*", "", .))
  
  heat_map = pheatmap(mat = df_heat_map,
                      scale = 'row',
                      cluster_cols = F,
                      cluster_rows = F,
                      main = paste(substitute(df), 'Top'),    
                      annotation_col = annotations,
                      treeheight_row = 0)
  return(heat_map)
}

get_heat_map(hnat_hcp_df, dds_final)
