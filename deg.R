library(dplyr)
library(edgeR)
library(org.Hs.eg.db)
# determine values for each condition
mod_mat = model.matrix(design(dds_final), colData(dds_final))
hNAT = colMeans(mod_mat[dds_final$condition == 'hNAT',])
hCP = colMeans(mod_mat[dds_final$condition == 'hCP',])
hNP = colMeans(mod_mat[dds_final$condition == 'hNP',])


res_to_df <- function(single_res){
  df = single_res %>% data.frame %>%
    mutate(ENSG = gsub("\\..*", "", rownames(.)),
           Symbol = mapIds(org.Hs.eg.db, keys = ENSG, column="SYMBOL", keytype="ENSEMBL", multiVals="first"),
           Cutoff = dplyr::case_when(
             is.na(padj) | padj > 0.05  ~ 'ns',
             log2FoldChange >  1 ~ 'inc',
             log2FoldChange < -1 ~ 'dec',
             .default = 'nfc'))
  return(df)
}
# generate results for single comparison
hnat_hcp_res = results(dds_final, hNAT - hCP)
summary(hnat_hcp)
hnat_hcp_df = res_to_df(hnat_hcp_res)


# library(openxlsx)
# write to workbook
# wb = createWorkbook()
# addWorksheet(wb, 'hCP vs hNAT')
# addWorksheet(wb, 'hCP vs hNP')
# addWorksheet(wb, 'hNP vs hNAT')
# 
# writeData(wb, sheet = 'hCP vs hNAT', x = hcp_hnat_de, rowNames = T)
# writeData(wb, sheet = 'hCP vs hNP', x = hcp_hnp_de, rowNames=T)
# writeData(wb, sheet='hNP vs hNAT', x= hnp_hnat_de, rowNames=T)
# 
# saveWorkbook(wb = wb, file = 'DE_genes/hcp_hnp_hnat_de_genes.xlsx')

