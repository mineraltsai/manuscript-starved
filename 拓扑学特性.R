library(ggplot2)
library(ggsci)
data<- read.delim("~/Desktop/饥饿小龙虾文章/最新抽平数据/mena分析/mena分析结果文件/拓扑学特性.txt",header = TRUE)

p<-ggplot(data, aes(x=Pi, y=Zi,shape=groups,color=Class,size=node.degree)) + 
  ggtitle("Topological roles")+
  scale_x_continuous(expand = c(0,0),limits = c(0,1))+
  scale_y_continuous(limits = c(-3,4))+
  geom_point(
    alpha=0.7,
    stroke =1.5
  )+theme_classic()+theme(legend.background = element_rect(inherit.blank = TRUE))+
  scale_color_d3()
p
p1<-p+guides(
             shape = guide_legend(order = 2),
             size = guide_legend(order = 3),
  fill= guide_legend( ncol  = 3,byrow=FALSE))+
 
  theme(legend.position = "right")

p2<-p1+
  geom_hline(aes(yintercept=2.5),linetype=5,col="red")+
  geom_vline(aes(xintercept=0.62),linetype=5,col="red")
p2
