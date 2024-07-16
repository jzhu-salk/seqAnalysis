library(ggplot2)
library(dplyr)
library(ggrepel)


v <- vst(dds_filtered) %>% plotPCA(intgroup = 'condition', returnData = TRUE)

percentVar <- format(round(100 * attr(v, "percentVar"), digits = 2))

pca_plot <- v %>%
  ggplot(aes(x=PC1, y=PC2, label=condition)) + 
  geom_point(aes(color=condition), size = 10, shape = 1, stroke = 2) +
  geom_text_repel(aes(label=name), nudge_y = -3)  +
  labs(title = 'PCA',
       x = paste0("PC1 - ", percentVar[1], "%"), 
       y = paste0("PC2 - ", percentVar[2], "%"),
       color = element_blank()) +
  scale_x_continuous(limits = c(min(v$PC1)* 1.1, max(v$PC1) * 1.1)) +
  scale_y_continuous(limits = c(min(v$PC2) * 1.1, max(v$PC2) * 1.1)) +
  theme_minimal() +
  theme(
    legend.position = 'inside',
    legend.justification = c(1,0),
    legend.box.background = element_rect()
  )
pca_plot