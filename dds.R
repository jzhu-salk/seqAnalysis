library(DESeq2)
library(dplyr)

# featureCounts output file
counts_file = '/osprey/osprey_gele/jz/0693_victoria/quantification/gene_counts'

# read in counts file, separating into gene count matrix and gene info 
gene_mat = read.csv(counts_file, skip=1, sep='\t', row.names=1)
gene_info = gene_mat %>% dplyr::select(c(1:5))
gene_mat = gene_mat %>% dplyr::select(-c(1:5))

# simplify sample names
colnames(gene_mat) = colnames(gene_mat) %>%
  # sub('\\.trim.*', '', .)
  # sub('_Aligned.*', '', .) 
  sub('_R1_001.*', '', .)

# metadata about samples
colData = colnames(gene_mat) %>%
  data.frame(row.names = .,
             collapsed_name = sub('_L00[1-2]', '', .),
             condition = sub('[0-9].*', '', .))

# initialize DESeq2
dds = DESeqDataSetFromMatrix(countData = gene_mat,
                             colData = colData,
                             design = ~ condition) 

# collapse technical replicates (DESeq2 adds them together)
dds_collapse_tech = collapseReplicates(dds, colData$collapsed_name)
dds = dds_collapse_tech

# filter out genes with low reads across multiple groups
dds %>% counts %>% rowSums %>% summary 
min_group_size = colData(dds)['condition'] %>% table %>% min
has_reads = rowSums(counts(dds_collapse_tech) > 10 ) > min_group_size
dds_filtered = dds_collapse_tech[has_reads,]
dds_filtered %>% counts %>% rowSums %>% summary

# calculate normalized counts
dds_final = DESeq(dds_filtered)