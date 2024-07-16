library(ggplot2)
library(dplyr)
library(ggpubr)

get_volcano <- function(title){
  deseq2_df = parse(text = eval(title)) %>% eval
  # plot genes in volcano plot
  volcano <- deseq2_df %>%
    ggplot(mapping=aes(x=log2FoldChange, y=-log10(padj), color=Cutoff)) +
    geom_point(alpha=0.8, size=1) +
    scale_color_manual(values=c('dec' = 'blue', 'inc' = 'red', 'ns' = 'black', 'nfc' = 'black')) +
    scale_x_continuous(limits = c(0 - deseq2_df %>% pull(log2FoldChange) %>% abs %>% max,  
                                  deseq2_df %>% pull(log2FoldChange) %>% abs %>% max)) +
    labs(title = title,
         subtitle = paste('Increasing Genes:', sum(deseq2_df$Cutoff == 'inc'),
                          '\nDecreasing Genes:', sum(deseq2_df$Cutoff == 'dec'),
                          '\nNS/NFC Genes:', sum(deseq2_df$Cutoff == 'nfc' | deseq2_df$Cutoff == 'ns')),
         x=expression('Log'[2]*'(Fold Change)'),
         y=expression(-'Log'[10]*'(P-Adjusted Value)')) +
    geom_vline(xintercept=c(1, -1), linetype = 'dashed') +
    geom_hline(yintercept=-log10(0.05), linetype = 'dashed') +
    theme_minimal() +
    theme(legend.position = 'none',
          plot.subtitle = element_text(size = 9, hjust = 0), 
          #panel.grid.minor.y = element_blank()
    )
  
  return(volcano)
}
# hnp_hnat_volcano = get_volcano('hnp_hnat_df')
# hcp_hnp_volcano = get_volcano('hcp_hnp_df')
hcp_hnat_volcano = get_volcano('hnat_hcp_df')
# hcp_other_volcano = get_volcano('hcp_other_df')
volcano = ggarrange(hcp_hnp_volcano, hnp_hnat_volcano, hcp_hnat_volcano, hcp_other_volcano, ncol=2, nrow=2)
volcano

