library(ggsci)
library(ggpubr)
library(readxl)
library(tidyverse)
fcbv <- read_excel("~/Desktop/饥饿小龙虾文章/最新抽平数据/r作excel/alphadiv.xlsx")


# 统计摘要Richness
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(Richness),
    sd = sd(Richness),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("Con", "Starved", "Refed")))
summ_fcbv


# 数据转换  
fcbv_plot <- fcbv %>% 
  mutate(Species = factor(Species, levels = c("Con", "Starved", "Refed")))
head(fcbv_plot)

my_comparisons <- list( c("Con","Starved"),
                        c("Starved","Refed"),
                        c("Con","Refed")
)

p1<- ggpaired(fcbv_plot, x = 'Species', y = 'Richness',id='id',
              color = 'Species', palette = "jco", 
              line.color = "gray", line.size = 0.4,
              short.panel.labs = FALSE)+
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  geom_point(aes(x = as.numeric(Species)-0.1,
                 y = Richness,color = Species),
             position = position_jitter(width = .05),size = .25, shape = 20) +
  
  geom_point(data=summ_fcbv,
             aes(x=Species,y = mean,group = Species, color = Species),
             shape=18,
             size = 1.5,
             position = position_nudge(x = .1,y = 0)) +
  theme(legend.position = "none")+
  xlab("Species")


p1
# 统计摘要Simpson
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(Simpson),
    sd = sd(Simpson),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("Con", "Starved", "Refed")))
summ_fcbv

p2<- ggpaired(fcbv_plot, x = 'Species', y = 'Simpson',id='id',
              color = 'Species', palette = "jco", 
              line.color = "gray", line.size = 0.4,
              short.panel.labs = FALSE)+
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  geom_point(aes(x = as.numeric(Species)-0.1,
                 y = Simpson,color = Species),
             position = position_jitter(width = .05),size = .25, shape = 20) +
  
  geom_point(data=summ_fcbv,
             aes(x=Species,y = mean,group = Species, color = Species),
             shape=18,
             size = 1.5,
             position = position_nudge(x = .1,y = 0)) +
  theme(legend.position = "none")+
  xlab("Species")
p2

# 统计摘要Shannon
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(Shannon),
    sd = sd(Shannon),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("Con", "Starved", "Refed")))
summ_fcbv

p3<-  ggpaired(fcbv_plot, x = 'Species', y = 'Shannon',id='id',
               color = 'Species', palette = "jco", 
               line.color = "gray", line.size = 0.4,
               short.panel.labs = FALSE)+
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  geom_point(aes(x = as.numeric(Species)-0.1,
                 y = Shannon,color = Species),
             position = position_jitter(width = .05),size = .25, shape = 20) +
  
  geom_point(data=summ_fcbv,
             aes(x=Species,y = mean,group = Species, color = Species),
             shape=18,
             size = 1.5,
             position = position_nudge(x = .1,y = 0)) +
  theme(legend.position = "none")+
  xlab("Species")
p3



# 统计摘要Chao1
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(Chao1),
    sd = sd(Chao1),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("Con", "Starved", "Refed")))
summ_fcbv

p4<- ggpaired(fcbv_plot, x = 'Species', y = 'Chao1',id='id',
              color = 'Species', palette = "jco", 
              line.color = "gray", line.size = 0.4,
              short.panel.labs = FALSE)+
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  geom_point(aes(x = as.numeric(Species)-0.1,
                 y = Chao1,color = Species),
             position = position_jitter(width = .05),size = .25, shape = 20) +
  
  geom_point(data=summ_fcbv,
             aes(x=Species,y = mean,group = Species, color = Species),
             shape=18,
             size = 1.5,
             position = position_nudge(x = .1,y = 0)) +
  theme(legend.position = "none")+
  xlab("Species")
p4

# 统计摘要ACE
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(ACE),
    sd = sd(ACE),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("Con", "Starved", "Refed")))
summ_fcbv

p5<- ggpaired(fcbv_plot, x = 'Species', y = 'ACE',id='id',
              color = 'Species', palette = "jco", 
              line.color = "gray", line.size = 0.4,
              short.panel.labs = FALSE)+
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  geom_point(aes(x = as.numeric(Species)-0.1,
                 y = ACE,color = Species),
             position = position_jitter(width = .05),size = .25, shape = 20) +
  
  geom_point(data=summ_fcbv,
             aes(x=Species,y = mean,group = Species, color = Species),
             shape=18,
             size = 1.5,
             position = position_nudge(x = .1,y = 0)) +
  theme(legend.position = "none")+
  xlab("Species")
p5




# 统计摘要Coverage
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(Coverage),
    sd = sd(Coverage),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("Con", "Starved", "Refed")))
summ_fcbv

p6<- ggpaired(fcbv_plot, x = 'Species', y = 'Coverage',id='id',
              color = 'Species', palette = "jco", 
              line.color = "gray", line.size = 0.4,
              short.panel.labs = FALSE)+
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  geom_point(aes(x = as.numeric(Species)-0.1,
                 y = Coverage,color = Species),
             position = position_jitter(width = .05),size = .25, shape = 20) +
  
  geom_point(data=summ_fcbv,
             aes(x=Species,y = mean,group = Species, color = Species),
             shape=18,
             size = 1.5,
             position = position_nudge(x = .1,y = 0)) +
  theme(legend.position = "none")+
  xlab("Species")
p6
library(cowplot)
plot_grid(p1, p2, p3, p3,
          labels = c('A', 'B', 'C', 'D'),
          align="hv", ncol=3)

plot_grid(p4, p2,p1, p3 ,p5,p6,
          labels = c('A', 'B', 'C', 'D','E', 'F'),
          align="hv", ncol=3
)

