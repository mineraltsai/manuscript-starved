library(ggsci)
library(ggpubr)
library(readxl)

library(tidyverse)
library(microeco)
otu <- read.delim("~/Desktop/饥饿小龙虾文章/最新抽平数据/lefse数据/convsstarved.txt",header = TRUE,row.names = 1)
tax <- read.delim("~/Desktop/饥饿小龙虾文章/最新抽平数据/lefse数据/taxon.txt",header = TRUE,row.names = 1)
sampledata<-read.delim("~/Desktop/饥饿小龙虾文章/最新抽平数据/lefse数据/sampleconvsstarved.txt",header = TRUE,row.names = 1)

data<-microtable$new(
  otu_table = otu,
  sample_table = sampledata,
  tax_table = tax
)
data
data$tidy_dataset(main_data = TRUE)
data$cal_abund()
#方法
trans_diff$new(
  dataset = NULL,
  method = c("lefse", "rf", "metastat", "mseq")[1],
  group = NULL,
  lefse_subgroup = NULL,
  alpha = 0.05,
  lefse_min_subsam = 10,
  lefse_norm = 1e+06,
  nresam = 0.6667,
  boots = 30,
  rf_taxa_level = "all",
  rf_ntree = 1000,
  metastat_taxa_level = "Genus",
  group_choose_paired = NULL,
  mseq_adjustMethod = "fdr",
  mseq_count = 1
)

library(microeco)
t1 <- trans_diff$new(dataset = data, method = "lefse", group = "group"
                     #,
                     # lefse_subgroup = "type", alpha = 0.5
)
t1$plot_lefse_cladogram(use_taxa_num = 100, 
                        use_feature_num = 20, 
                        select_show_labels = NULL,
                        clade_label_level=6,
                        alpha=0.3,
                        clade_label_size=1,
                        node_size_scale = 1,
                        node_size_offset = 1,
                        annotation_shape = 22,
                        annotation_shape_size = 5,color = c("#333399","#009933")
)#+scale_color_aaas(palette = c("#333399","#FF0000"), alpha = 1)+
# scale_fill_aaas(palette = c("#333399","#FF0000"), alpha = 1)
#颜色
Accent	8
Dark2	8
Paired	12
Pastel1	9
Pastel2	8
Set1	9 #红蓝绿
Set2	8
Set3	12

#方法
trans_diff$plot_lefse_cladogram(
  color = RColorBrewer::brewer.pal(8, "Dark2"),
  use_taxa_num = 200,
  filter_taxa = NULL,
  use_feature_num = NULL,
  group_order = NULL,
  clade_label_level = 4,
  select_show_labels = NULL,
  only_select_show = FALSE,
  sep = "|",
  branch_size = 0.2,
  alpha = 0.2,
  clade_label_size = 0.7,
  node_size_scale = 1,
  node_size_offset = 1,
  annotation_shape = 22,
  annotation_shape_size = 5
)
## ------------------------------------------------
## Method `trans_diff$plot_diff_abund`
## ------------------------------------------------


t1$plot_diff_abund(use_number = 1:10)


## ------------------------------------------------
## Method `trans_diff$plot_lefse_bar`
## ------------------------------------------------


t1$plot_lefse_bar(LDA_score = 4)



## ------------------------------------------------
## Method `trans_diff$plot_metastat`
## ------------------------------------------------


t2 <- trans_diff$new(dataset = data, method = "metastat", group = "group", metastat_taxa_level = "Phylum")
t2$plot_metastat(use_number = 1:300, qvalue = 0.06, choose_group = 3)
