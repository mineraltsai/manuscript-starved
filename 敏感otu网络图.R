library(edgeR)#用于基于edgeR的组间差异OTU识别
library(indicspecies)#用于基于indicator species的组间差异OTU识别
library(igraph)#用于共现性网络的绘制
library(Hmisc)#进行共现性网络构建前的OTU两两相关性计算
library(sciplot)#用于基于模块丰度的绘图
library(reshape2)#用于长宽数据转换
library(ggpmisc)#用于其它观测指标与组间差异模块丰度关系分析

#####输入OTU表格数据 #####
otu_its <- read.table("~/Desktop/饥饿小龙虾文章/最新抽平数据/otu.txt",row.names=1,sep="\t",header=T, blank.lines.skip=F,check.names=F)
otu_its <- as.matrix(otu_its)

#####输入OTU物种分类数据 #####
tax_its <- read.table("~/Desktop/饥饿小龙虾文章/最新抽平数据/taxon.txt",row.names=1, sep="\t", header=F,stringsAsFactors=F,quote="")
colnames(tax_its) <- c("Kingdom","Phylum","Class","Order", "Family", "Genus", "Species")
#tax_its[tax_its==""] <- "Unassigned"

#####输入metadata数据 #####
design_its <- read.table("~/Desktop/饥饿小龙虾文章/最新抽平数据/Sampledata.txt", header=T, row.names=1, na.strings="NA")
design_its$group <- factor(design_its $group,c("Con","Starved","Refed"))
#design_its$stage <- factor(design_its $stage ,c("T","J","F"))
#design_its$tilstag <- factor(design_its $tilstag,c("CPTT","CPTJ","CPTF","PTT","PTJ", "PTF","ZTT","ZTJ","ZTF"))

#####输入其它性状（plant_trait）数据 #####
#plant_trait <- read.table("d4_plant_trait.txt", header=T, row.names=1, na.strings="NA")


#####以其中的T时期为例进行network的绘制#####
#tsamples <- rownames(design_its)[which(design_its$stage == "T")]
#otu_its_t <- otu_its[,tsamples]

otu_its_t <- otu_its
#去除一些低丰度OTU，在至少i个样本中具有至少j个序列
#otu_its_t <- otu_its_t[which(rowSums(otu_its_t >= j) >= 18),]
design_its_t <- droplevels(design_its)
tax_t_its <- tax_its[rownames(otu_its_t),]


#####寻找基于indicator species的组间显著差异OTU并保存结果#####
edgeR_its_t <- DGEList(counts=otu_its_t,
                       group=design_its_t$group,
                       genes=tax_t_its)
#CPM标准化
otu_norm_its_t <- cpm(edgeR_its_t, normalized.lib.sizes=T, log=F)
# 准备数据：T时期下组间的indicator species
indic_t_its <- as.data.frame(t(otu_norm_its_t))
indic_t_groups_its <- design_its_t$group
# 设置随机数种子，保证置换检验可重复
set.seed(8046)
# 鉴定各组指示种
indicatorsp_t_its <- multipatt(indic_t_its,indic_t_groups_its,func = "r.g",control=how(nperm=1000))
indic_t_df_its <- indicatorsp_t_its$sign

#按照阈值p.value < 0.05筛选各组显著的指示OTU
Con_t_its <- as.matrix(indic_t_df_its[which(indic_t_df_its$s.Con == 1 & indic_t_df_its$p.value < 0.05),])
Starved_t_its <- as.matrix(indic_t_df_its[which(indic_t_df_its$s.Starved == 1 & indic_t_df_its$p.value < 0.05),])
Refed_t_its <- as.matrix(indic_t_df_its[which(indic_t_df_its$s.Refed == 1 & indic_t_df_its$p.value < 0.05),])
#合并
t_r_values_its <- rbind(Con_t_its,Starved_t_its,Refed_t_its)
t_r_values_its
# 组名修正，删除多余的"s."
# colnames(t_r_values_its)[1:3] <- c("CPT","PT","ZT")
colnames(t_r_values_its)[1:3] <- gsub("s.","",colnames(t_r_values_its)[1:3])

#####寻找基于edgeR的组间显著差异OTU#####
model_matt_its <- model.matrix(~group, data=design_its_t)
edgeR_its_t_group <- DGEList(counts=otu_its_t, group=design_its_t$group, genes=tax_t_its)
edgeR_its_t_group <- calcNormFactors(edgeR_its_t_group)
dge_group_its <- estimateGLMRobustDisp(edgeR_its_t_group, design=model_matt_its)
fit_group_its <- glmFit(dge_group_its, design=model_matt_its)
fit_group_its
# 2，3组分别与1组比较
lrt_group_its <- glmLRT(fit_group_its, coef=2:3)
tillage_t_its <- topTags(lrt_group_its, n=Inf, p.value=0.05)
tillage_t_its <- tillage_t_its$table

##### (可选)limma分析，取indicator species、edgeR、limma的交集，但是这样可能太严格导致OTU极少
#limma_voom_t_its <- voom(edgeR_its_t_tillage, model_matt_its)
#fit_t_its <- lmFit(limma_voom_t_its, model_matt_its)
#fit_t_its <- eBayes(fit_t_its)
#limma_t_its<-topTable(fit, coef = 2:3)
#indic_edge_its_t <- Reduce(intersect,list(rownames(t_r_values_its),rownames(tillage_t_its),rownames(limma_t_its)))


######绘制能够体现组件丰度差异OTU/模块的共现性网络图co-occurence network #####
#取基于indicator species分析和edgeR分析得到的显著组间差异OTU的交集
indic_edge_its_t <- intersect(rownames(t_r_values_its),rownames(tillage_t_its))
indic_edge_its_t
#基于TMM标准化的OTU表格进行OTU的两两Spearman相关计算
t_its_otu_cor <- rcorr(t(otu_norm_its_t),type=c("spearman"))
## 邻接矩阵转化为边表
CorrDF <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    from = rownames(cormat)[col(cormat)[ut]],
    to = rownames(cormat)[row(cormat)[ut]],
    cor =(cormat)[ut],
    p = pmat[ut]
  )
}
t_its_cor_df <- CorrDF(t_its_otu_cor$r,t_its_otu_cor$P)
# p值校正
t_its_cor_df$padj <- p.adjust(t_its_cor_df$p, method = "none") #method可选c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
#取Spearman's rho > 0.7且p-value < 0.001的关系作为入选共现性网络co-occurence network的边
t_its_cor_df_padj <- t_its_cor_df[which(t_its_cor_df$cor > 0.7),]
t_its_cor_df_padj <- t_its_cor_df_padj[which(t_its_cor_df_padj$padj < 0.001),]
#生成node属性表
# 边两列合并为结点
nodeattrib_t_its <- data.frame(node = union(t_its_cor_df_padj$from,t_its_cor_df_padj$to))
# 显著的添加标签
nodeattrib_t_its$indicgroup <- 0
for (i in as.character(nodeattrib_t_its$node))
{
  if (i %in% indic_edge_its_t == TRUE)
  {nodeattrib_t_its[nodeattrib_t_its$node==i,"indicgroup"] <- paste(colnames(t_r_values_its)[which(t_r_values_its[i,1:3]==1)],collapse = "_")}
  else
  { nodeattrib_t_its[nodeattrib_t_its$node==i,"indicgroup"] <- "NA"}
}
#将OTU，即节点分类信息添加到node属性表
nodeattrib_t_its <- cbind(nodeattrib_t_its,tax_its[as.character(nodeattrib_t_its$node),])
nodeattrib_t_its
#用igraph绘制共现性网络图co-occurence network
t_net_its <- graph_from_data_frame(t_its_cor_df_padj,direct=F, vertices = nodeattrib_t_its)
## 网络中的节点相对丰度
t_ra_its <- apply(otu_norm_its_t,1,mean)
t_ra_its <- t_ra_its[V(t_net_its)$name]
#将上述显著组间差异的OTU着色，这里有CPT,ZT,PT三种处理，因此理论上有6种可能的差异丰度分布情况
cs <- c("Con","Con_Starved","Starved","Con_Refed","Starved_Refed","Refed")
unique(V(t_net_its)$indicgroup)
V(t_net_its)$color <- V(t_net_its)$indicgroup
V(t_net_its)$color[!V(t_net_its)$color %in% cs] <- "gray30"  
V(t_net_its)$color[V(t_net_its)$color == "Con"] <- "#333399"
V(t_net_its)$color[V(t_net_its)$color == "Con_Starved"] <- "#00FFFF"
V(t_net_its)$color[V(t_net_its)$color == "Starved"] <- "#FF3333"    
V(t_net_its)$color[V(t_net_its)$color == "Con_Refed"] <- "#800080"
V(t_net_its)$color[V(t_net_its)$color == "Starved_Refed"] <- "#FFFF00"
V(t_net_its)$color[V(t_net_its)$color == "Refed"] <- "#339933"
V(t_net_its)$frame.color <- V(t_net_its)$color
#上述着色信息映射到node属性表中
tits_nodes <- rownames(nodeattrib_t_its[nodeattrib_t_its$indicgroup %in% cs,])
tits_nodes

#设置节点形状
V(t_net_its)$shape <- "circle"
##设置节点大小，非显著组间差异OTU设置为"3"，显著的为"6"
V(t_net_its)$size <- V(t_net_its)$name
V(t_net_its)$size[!V(t_net_its)$size %in% tits_nodes] <- 3
V(t_net_its)$size[V(t_net_its)$size %in% tits_nodes] <- 6
tits_nodesizes <- as.numeric(V(t_net_its)$size)
#向量化各类型的显著组间差异OTU以便后续计算
CPT_nodes_t <- rownames(nodeattrib_t_its[nodeattrib_t_its$indicgroup=="Con",])
CZ_nodes_t <- rownames(nodeattrib_t_its[nodeattrib_t_its$indicgroup=="Con_Starved",])
ZT_nodes_t <- rownames(nodeattrib_t_its[nodeattrib_t_its$indicgroup=="Starved",])
CP_nodes_t <- rownames(nodeattrib_t_its[nodeattrib_t_its$indicgroup=="Con_Refed",])
ZP_nodes_t <- rownames(nodeattrib_t_its[nodeattrib_t_its$indicgroup=="Starved_Refed",])
PT_nodes_t <- rownames(nodeattrib_t_its[nodeattrib_t_its$indicgroup=="Refed",])
cs_nodes_t_all <- c(CPT_nodes_t,CZ_nodes_t,ZT_nodes_t,CP_nodes_t,ZP_nodes_t,PT_nodes_t)
cs_nodes_t_all
write.table(CPT_nodes_t,"~/Desktop/饥饿小龙虾文章/最新抽平数据/pdf/不分组-各组间差异生物模块及差异模块分析/CPT_nodes_t.txt",sep="\t",col.names=F,quote=F)
write.table(ZT_nodes_t,"~/Desktop/饥饿小龙虾文章/最新抽平数据/pdf/不分组-各组间差异生物模块及差异模块分析/ZT_nodes_t.txt",sep="\t",col.names=F,quote=F)
write.table(PT_nodes_t,"~/Desktop/饥饿小龙虾文章/最新抽平数据/pdf/不分组-各组间差异生物模块及差异模块分析/PT_nodes_t.txt",sep="\t",col.names=F,quote=F)
write.table(ZP_nodes_t,"~/Desktop/饥饿小龙虾文章/最新抽平数据/pdf/不分组-各组间差异生物模块及差异模块分析/ZP_nodes_t.txt",sep="\t",col.names=F,quote=F)

#将网络中的节点/OTU进行聚类，这里采用fast greedy法
cfg_t <- cluster_fast_greedy(as.undirected(t_net_its))
#查看包含OTU数量最多的10个模块，以进行后续的着色
t_modules <- sort(table(membership(cfg_t)),decr=T)
t_modules_10 <- t_modules[1:10]
sm10_plot <- t_modules_10
names(sm10_plot) <- as.factor(1:10)
#寻找包含OTU数量最多的10个模块中具有显著组间差异OTU的模块
t_modules_cs <- table(factor(membership(cfg_t)[cs_nodes_t_all],levels=names(t_modules)))
t_modules_cs_10 <- t_modules_cs[names(t_modules_10)]
smcs10_plot <- t_modules_cs_10
names(smcs10_plot) <- as.factor(1:10)
#将OTU数量最多的10个模块中的OTU向量化
t_modules_points <- membership(cfg_t)[membership(cfg_t) %in% names(t_modules_10)]
t_points <- NULL
for(i in t_modules_points){
  tx <- which(names(t_modules_10)==i)
  t_points <- c(t_points, tx)
}
names(t_points) <- names(t_modules_points)
#按照组间差异OTU的类型着色这些OTU
t_all_cols <- sort(t_points)
t_all_cols[!names(t_all_cols) %in% cs] <- "gray30"
t_all_cols[names(t_all_cols) %in% CPT_nodes_t] <- "#333399"
t_all_cols[names(t_all_cols) %in% CZ_nodes_t] <- "#00FFFF"
t_all_cols[names(t_all_cols) %in% ZT_nodes_t] <- "#FF3333"
t_all_cols[names(t_all_cols) %in% CP_nodes_t] <- "#800080"
t_all_cols[names(t_all_cols) %in% ZP_nodes_t] <- "#FFFF00"
t_all_cols[names(t_all_cols) %in% PT_nodes_t] <- "#339933"
#设置节点样式，1为空心圆代表普通节点，16为实心圆代表组间差异OTU
t_all_pch <- sort(t_points)
t_all_pch
t_all_pch[names(t_all_pch) %in% rownames(otu_norm_its_t)] <- 1
t_all_pch[names(t_all_pch) %in% intersect(rownames(otu_norm_its_t),cs_nodes_t_all)] <- 16
write.table(t_all_pch,"~/Desktop/饥饿小龙虾文章/最新抽平数据/pdf/不分组-各组间差异生物模块及差异模块分析/t_all_pch.txt",sep="\t",row.names=F,quote=F)

#设置节点缩放倍数，1为普通节点1倍，2为组间差异OTU2倍
t_all_cex <- sort(t_points)
t_points
t_all_cex
t_all_cex[!names(t_all_cex) %in% cs_nodes_t_all] <- 1
t_all_cex[names(t_all_cex) %in% cs_nodes_t_all] <- 2
write.table(t_points,"~/Desktop/饥饿小龙虾文章/最新抽平数据/pdf/不分组-各组间差异生物模块及差异模块分析/t_points.txt",sep="\t",col.names=F,quote=F)

#哪些模块包含有组间差异OTU
t_mods_list_cs <- list()
t_mods_list_cs
for (i in names(t_modules_cs_10)){
  x1 <- names(membership(cfg_t)[membership(cfg_t)==i])
  x2 <- x1[x1 %in% cs_nodes_t_all]
  t_mods_list_cs[[i]] <- as.numeric(V(t_net_its)[x2])
}
t_mods_list_cs$`3`
write.table(t_mods_list_cs$`3`,"~/Desktop/饥饿小龙虾文章/最新抽平数据/pdf/不分组-各组间差异生物模块及差异模块分析/t_mods_list_cs$`3`.txt",sep="\t",row.names=F,col.names=F,quote=F)

#设定layout出图，这里选择Fruchterman & Reingold
set.seed(8051)
coords_t_its <- layout_(t_net_its,with_kk())
#每次运行耗费时间，可以把文件储存
write.table(coords_t_its,"~/Desktop/饥饿小龙虾文章/最新抽平数据/不分组-各组间差异生物模块及差异模块分析/coords_t_its.txt",sep="\t",row.names=F,col.names=F,quote=F)
coords_t_its <- as.matrix(read.table("~/Desktop/饥饿小龙虾文章/最新抽平数据/不分组-各组间差异生物模块及差异模块分析/coords_t_its.txt"))
dimnames(coords_t_its) <- NULL

#出图
pdf(paste0("~/Desktop/饥饿小龙虾文章/最新抽平数据/不分组-各组间差异生物模块及差异模块分析/p1_network.pdf"),width=7,height=5)
par(mfrow=c(1,1), mar=c(0,0,0,0))
t_cols <- c("#66CCCC","#99FFCC","#99CC99")
plot(t_net_its,vertex.label=NA,vertex.size=tits_nodesizes, layout=coords_t_its,
     mark.groups=list(t_mods_list_cs$`1`,t_mods_list_cs$`2`,t_mods_list_cs$`3`
                     # ,t_mods_list_cs$`4`,t_mods_list_cs$`5`,t_mods_list_cs$`21`
                      ),
     mark.col=t_cols, mark.border=t_cols)
legend("right",legend=c("Module 1","Module 2", "Module 3"),col=t_cols,
       bty="n",fill=t_cols,border=t_cols)
dev.off()




#设定layout出图，这里选择Fruchterman & Reingold
set.seed(1024)
coords_t_its1 <- layout_(t_net_its,with_fr(niter=9999, grid="nogrid"))
#每次运行耗费时间，可以把文件储存
write.table(coords_t_its1,"~/Desktop/饥饿小龙虾文章/最新抽平数据/不分组-各组间差异生物模块及差异模块分析/coords_t_its1.txt",sep="\t",row.names=F,col.names=F,quote=F)
coords_t_its1 <- as.matrix(read.table("~/Desktop/饥饿小龙虾文章/最新抽平数据/不分组-各组间差异生物模块及差异模块分析/coords_t_its1.txt"))
dimnames(coords_t_its1) <- NULL

#出图
pdf(paste0("~/Desktop/饥饿小龙虾文章/最新抽平数据/不分组-各组间差异生物模块及差异模块分析/p4_network.pdf"),width=7,height=5)
par(mfrow=c(1,1), mar=c(0,0,0,0))
t_cols <- c("#66CCCC","#99FFCC","#99CC99")
plot(t_net_its,vertex.label=NA,vertex.size=tits_nodesizes, layout=coords_t_its1,
     mark.groups=list(t_mods_list_cs$`1`,t_mods_list_cs$`2`,t_mods_list_cs$`3`
                      # ,t_mods_list_cs$`4`,t_mods_list_cs$`5`,t_mods_list_cs$`21`
     ),
     mark.col=t_cols,
     #edge.curved=TRUE,
     mark.border=t_cols)
legend("right",legend=c("Module 1","Module 2", "Module 3"),col=t_cols,
       bty="n",fill=t_cols,border=t_cols)
dev.off()




library(edgeR)#用于基于edgeR的组间差异OTU识别
library(indicspecies)#用于基于indicator species的组间差异OTU识别
library(igraph)#用于共现性网络的绘制
library(Hmisc)#进行共现性网络构建前的OTU两两相关性计算
library(sciplot)#用于基于模块丰度的绘图
library(reshape2)#用于长宽数据转换
library(ggpmisc)#用于其它观测指标与组间差异模块丰度关系分析

library(ggsci)
#模块中组内OTU相对丰度比较

pdf(paste0("~/Desktop/饥饿小龙虾文章/最新抽平数据/不分组-各组间差异生物模块及差异模块分析/p2_module_abundance.pdf"),width=7,height=3)
par(mfrow=c(1,3), mar=c(0.5,3.5,2,0))#根据模块数调整mfrow
CS_cols <- c("#333399","#FF0000","#009933")
names(CS_cols) <- c("Con","Starved", "Refed")
#T module 1
p1<-bargraph.CI(design_its_t$group, colSums(otu_norm_its_t[cfg_t[[1]],])/1000,
            las=2, ylab="cumulative relative abundance", cex.lab=.5, cex.axis=.7, cex.names=.7,
            err.width=.025, main="T M1", col=CS_cols, border=F)
#T module 2
bargraph.CI(design_its_t$group, colSums(otu_norm_its_t[cfg_t[[2]],])/1000,
            las=2, ylab="", cex.lab=.5, cex.axis=.7, cex.names=.7,ylim=c(0,800),
            err.width=.025, main="T M2", col=CS_cols, border=F)
#T module 3
bargraph.CI(design_its_t$group, colSums(otu_norm_its_t[cfg_t[[3]],])/1000,
            las=2, ylab="", cex.lab=.5, cex.axis=.7, cex.names=.7,
            err.width=.025, main="T M3", col=CS_cols, border=F)
#T module 4
bargraph.CI(design_its_t$group, colSums(otu_norm_its_t[cfg_t[[4]],])/1000,
            las=2, ylab="", cex.lab=.5, cex.axis=.7, cex.names=.7,
            err.width=.025, main="T M4", col=CS_cols, border=F)
#T module 5
bargraph.CI(design_its_t$group, colSums(otu_norm_its_t[cfg_t[[5]],])/1000,
            las=2, ylab="", cex.lab=.5, cex.axis=.7, cex.names=.7,
            err.width=.025, main="T M5", col=CS_cols, border=F)
#T module 21
bargraph.CI(design_its_t$group, colSums(otu_norm_its_t[cfg_t[[21]],])/1000,
            las=2, ylab="", cex.lab=.5, cex.axis=.7, cex.names=.7,
            err.width=.025, main="T M21", col=CS_cols, border=F)
plot.new()
par(mar=c(0.5,0,2,0))
#legend("left", bty="n", cex=1, #x.intersp=0.1, y.intersp=1,
       #legend=names(CS_cols),
      # fill=CS_cols,
      # border=CS_cols , xpd=T)
dev.off()


#将网络图导出为"graphml"、"gml"格式，方便导入Gephi中使用；
write_graph(t_net_its , "~/Desktop/饥饿小龙虾文章/最新抽平数据/不分组-各组间差异生物模块及差异模块分析/coords_t_its .graphml", format="graphml")
write_graph(g1, "g1.gml", format="gml")


#####将包含显著差异OTU的主要模块丰度与其它观测指标进行回归#####
#提取这些模块的丰度
csmodule<-rbind(colSums(otu_norm_its_t[cfg_t[[1]],])/1000,colSums(otu_norm_its_t[cfg_t[[2]],])/1000,colSums(otu_norm_its_t[cfg_t[[3]],])/1000
                #,colSums(otu_norm_its_t[cfg_t[[4]],])/1000,colSums(otu_norm_its_t[cfg_t[[8]],])/1000
                )
row.names(csmodule)<-c("TM1","TM2","TM3")

#宽数据转化为长数据
csmodule_long<-melt(csmodule,measure.vars = c('CPT1','ZT3'))
View(csmodule_long)
names(csmodule_long)<-c("Module","Group","CPM")

#####输入其它性状（plant_trait）数据 #####
plant_trait <- read.table("~/Desktop/饥饿小龙虾文章/肝体比指数.txt", header=T, row.names=1, na.strings="NA")
plant_trait
#将性状数据与模块丰度数据对应起来，这里有5个模块所以each=5
plant_trait1 <- plant_trait[rep(1:nrow(plant_trait),each=3),]
plant_trait1
module_trait<-cbind(csmodule_long,plant_trait1)
View(module_trait)
module_trait <-module_trait[1:108,]
#去除重复的数字标号（i.e., CPT1→CPT）
module_trait$Group<-gsub("\\.\\d","",module_trait$Group)
module_trait$Group
module_trait
#绘制每个模块丰度-植物性状关系
t1<-ggplot(module_trait, aes(x = plant_trait1, y = CPM, color = Module)) +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  stat_poly_eq(aes(label = paste(..adj.rr.label.., sep = '~~~~')), formula = y ~ x, parse = T) +
  scale_linetype_manual() +
  geom_point(aes(size=5,shape=Group)) +
  scale_shape_manual(values=c(15,16,17)) +
  theme_classic()
 
ggsave(t1, file=paste0("~/Desktop/饥饿小龙虾文章/最新抽平数据/不分组-各组间差异生物模块及差异模块分析/p3_regression.pdf"), width=8, height=5)
