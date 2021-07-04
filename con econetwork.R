df = read.csv('~/Desktop/饥饿小龙虾文章/最新抽平数据/eco网络分析/con/contest.csv',header = T)
library(circlize)
circos.clear()
#circos.clear()  #这个命令用于清空画布，画错时要运行此命令重新再画。

#整体布局
gap_size <- c(rep(0.5, length(df$fc) - 1), 0.5)
circos.par(canvas.xlim =c(-1.1,1.1),canvas.ylim = c(-1.1,1.1),cell.padding = c(0,0,0,0),
           start.degree = 270, gap.degree = gap_size,track.margin = c(0,0)
)
#第一圈
fa = df$geneid
fa = factor(fa,levels = fa)
circos.initialize(factors = fa, xlim = c(0,1)) # 初始化

# 第二圈 细胞类型

circos.trackPlotRegion(
  
  ylim = c(0, 1), track.height = 0.05, bg.border = NA,
  
  panel.fun = function(x, y) {
    
    sector.index = get.cell.meta.data('sector.index')
    
    xlim = get.cell.meta.data('xlim')
    
    ylim = get.cell.meta.data('ylim')
    
  } )

# cell

highlight.sector(as.character(df$geneid[1:1]), track.index = 1,
                 
                 text = 'Methanomicrobia', niceFacing = F, font = 2, col = '#CCEBC5')

highlight.sector(as.character(df$geneid[2:5]), track.index = 1,
                 
                 text = 'Actinobacteria', niceFacing = F, font = 2, col ='#016392')

highlight.sector(as.character(df$geneid[6:107]), track.index = 1,
                 
                 text = 'Bacteroidia', niceFacing = F, font = 2,col ='#FF9900')

highlight.sector(as.character(df$geneid[108:109]), track.index = 1,
                 
                 text = 'Saprospirae', niceFacing = F, font = 2, col = '#FFCC99')

highlight.sector(as.character(df$geneid[110:110]), track.index = 1,    #FF3300
                 
                 text = 'Chloroplast', niceFacing = F, font = 2, col = '#c72e29')

highlight.sector(as.character(df$geneid[111:114]), track.index = 1,
                 
                 text = 'Deferribacteres', niceFacing = F, font = 2, col = 'palegreen')

highlight.sector(as.character(df$geneid[115:121]), track.index = 1,
                 
                 text = 'Bacilli', niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[122:174]), track.index = 1,
                 
                 text = 'Clostridia', niceFacing = F, font = 2, col = '#FFCC00')
highlight.sector(as.character(df$geneid[175:264]), track.index = 1,
                 
                 text = 'Erysipelotrichi', niceFacing = F, font = 2, col = '#FF99CC')

highlight.sector(as.character(df$geneid[265:265]), track.index = 1,
                 
                 text = 'Unclassified', niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[266:266]), track.index = 1,
                 
                 text = 'Fusobacteriia', niceFacing = F, font = 2, col = '#FF3300')
highlight.sector(as.character(df$geneid[267:267]), track.index = 1,
                 
                 text = 'Alphaproteobacteria', niceFacing = F, font = 2, col = '#FF6666')
highlight.sector(as.character(df$geneid[268:269]), track.index = 1,
                 
                 text = 'Betaproteobacteria', niceFacing = F, font = 2, col = '#FFFF33')

highlight.sector(as.character(df$geneid[270:299]), track.index = 1,
                 
                 text = 'Gammaproteobacteria', niceFacing = F, font = 2, col = '#ff00ff')


highlight.sector(as.character(df$geneid[300:300]), track.index = 1,
                 
                 text = 'Spirochaetes', niceFacing = F, font = 2, col = '#CC6699')
highlight.sector(as.character(df$geneid[301:301]), track.index = 1,
                 
                 text = 'Synergistia', niceFacing = F, font = 2, col = '#FF0099')
highlight.sector(as.character(df$geneid[302:302]), track.index = 1,
                 
                 text = 'CK-1C4-19', niceFacing = F, font = 2, col = '#CC00FF')


# 第二圈 细胞类型

circos.trackPlotRegion(
  
  ylim = c(0, 1), track.height = 0.05, bg.border = NA,
  
  panel.fun = function(x, y) {
    
    sector.index = get.cell.meta.data('sector.index')
    
    xlim = get.cell.meta.data('xlim')
    
    ylim = get.cell.meta.data('ylim')
    
  } )

# cell

highlight.sector(as.character(df$geneid[1:1]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#CCEBC5')

highlight.sector(as.character(df$geneid[2:5]), track.index = 2,
                 
             niceFacing = F, font = 2, col ='#016392')

highlight.sector(as.character(df$geneid[6:107]), track.index = 2,
                 
                 niceFacing = F, font = 2,col ='#FF9900')

highlight.sector(as.character(df$geneid[108:109]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#FFCC99')

highlight.sector(as.character(df$geneid[110:110]), track.index = 2,    #FF3300
                 
                 niceFacing = F, font = 2, col = '#c72e29')

highlight.sector(as.character(df$geneid[111:114]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = 'palegreen')

highlight.sector(as.character(df$geneid[115:121]), track.index = 2,
                 
                niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[122:174]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#FFCC00')
highlight.sector(as.character(df$geneid[175:264]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#FF99CC')

highlight.sector(as.character(df$geneid[265:265]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[266:266]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#FF3300')
highlight.sector(as.character(df$geneid[267:267]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#FF6666')
highlight.sector(as.character(df$geneid[268:268]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#FFFF33')



highlight.sector(as.character(df$geneid[269:269]), track.index = 2,
                 
                niceFacing = F, font = 2, col = '#CC0000')

highlight.sector(as.character(df$geneid[270:270]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[271:272]), track.index = 2,
                 
                niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[273:296]), track.index = 2,
                 
                niceFacing = F, font = 2, col = '#ff00ff')


highlight.sector(as.character(df$geneid[297:299]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#FF00FF')

highlight.sector(as.character(df$geneid[300:300]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#CC6699')
highlight.sector(as.character(df$geneid[301:301]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#FF0099')
highlight.sector(as.character(df$geneid[302:302]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#CC00FF')

#第三圈 配体受体

circos.trackPlotRegion(
  
  ylim = c(0, 1), track.height = 0.05, bg.border = NA,
  
  panel.fun = function(x, y) {
    
    sector.index = get.cell.meta.data('sector.index')
    
    xlim = get.cell.meta.data('xlim')
    
    ylim = get.cell.meta.data('ylim')
    
  } )
#红蓝配

highlight.sector(as.character(df$geneid[1:1]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = '#CCEBC5')
highlight.sector(as.character(df$geneid[2:3]), track.index = 3,
                 
                 niceFacing = F, font = 2, col ='#016392')

highlight.sector(as.character(df$geneid[4:4]), track.index = 3,
                 
                  niceFacing = F, font = 2, col ='#016392')
highlight.sector(as.character(df$geneid[5:5]), track.index = 3,
                 
                 niceFacing = F, font = 2, col ='#016392')

highlight.sector(as.character(df$geneid[6:80]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#FF9900')

highlight.sector(as.character(df$geneid[81:93]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = '#FFCC99')

highlight.sector(as.character(df$geneid[94:103]), track.index = 3,    #FF3300
                 
                 niceFacing = F, font = 2, col = '#c72e29')

highlight.sector(as.character(df$geneid[104:106]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = 'palegreen')

highlight.sector(as.character(df$geneid[107:107]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[108:109]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FFCC99')
highlight.sector(as.character(df$geneid[110:110]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = '#c72e29')

highlight.sector(as.character(df$geneid[111:114]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = 'palegreen')

highlight.sector(as.character(df$geneid[115:117]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = 'palegreen')
highlight.sector(as.character(df$geneid[118:118]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = 'palegreen')
highlight.sector(as.character(df$geneid[119:120]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = 'palegreen')



highlight.sector(as.character(df$geneid[121:121]), track.index = 3,
                 
                niceFacing = F, font = 2, col = 'palegreen')

highlight.sector(as.character(df$geneid[122:127]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#999900')
highlight.sector(as.character(df$geneid[128:128]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[129:160]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = '#FFCC00')


highlight.sector(as.character(df$geneid[161:166]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CCFFFF')

highlight.sector(as.character(df$geneid[167:173]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CCCC00')
highlight.sector(as.character(df$geneid[174:174]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[175:264]), track.index = 3,
                 
               niceFacing = F, font = 2, col = '#FF99CC')



highlight.sector(as.character(df$geneid[265:265]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[266:266]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = '#FF3300')
highlight.sector(as.character(df$geneid[267:267]), track.index = 3,
                 
                niceFacing = F, font = 2, col = '#FF6666')
highlight.sector(as.character(df$geneid[268:268]), track.index = 3,
                 
                niceFacing = F, font = 2, col = '#FFFF33')



highlight.sector(as.character(df$geneid[269:269]), track.index = 3,
                 
                niceFacing = F, font = 2, col = '#CC0000')

highlight.sector(as.character(df$geneid[270:270]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[271:272]), track.index = 3,
                 
                niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[273:296]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CCFF33')

highlight.sector(as.character(df$geneid[297:299]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF00FF')

highlight.sector(as.character(df$geneid[300:300]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = '#CC6699')
highlight.sector(as.character(df$geneid[301:301]), track.index = 3,
                 
               niceFacing = F, font = 2, col = '#FF0099')
highlight.sector(as.character(df$geneid[302:302]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = '#CC00FF')
#第四圈
circos.trackPlotRegion(
  
  ylim = c(0, 1), track.height = 0.05, bg.border = NA,
  
  panel.fun = function(x, y) {
    
    sector.index = get.cell.meta.data('sector.index')
    
    xlim = get.cell.meta.data('xlim')
    
    ylim = get.cell.meta.data('ylim')
    
  } )
#红蓝配

highlight.sector(as.character(df$geneid[1:1]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CCEBC5')
highlight.sector(as.character(df$geneid[2:2]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#016392')
highlight.sector(as.character(df$geneid[3:3]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#0099FF')

highlight.sector(as.character(df$geneid[4:4]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#66CCFF')
highlight.sector(as.character(df$geneid[5:5]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#99CCFF')

highlight.sector(as.character(df$geneid[6:73]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#FF9900')

highlight.sector(as.character(df$geneid[74:78]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#996600')

highlight.sector(as.character(df$geneid[79:80]), track.index = 4,    #FF3300
                 
                 niceFacing = F, font = 2, col = '#CC9933')

highlight.sector(as.character(df$geneid[81:81]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC9966')


highlight.sector(as.character(df$geneid[82:82]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC9966')
highlight.sector(as.character(df$geneid[83:91]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFCCCC')
highlight.sector(as.character(df$geneid[92:92]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#996666')
highlight.sector(as.character(df$geneid[93:93]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC6666')
highlight.sector(as.character(df$geneid[94:96]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC9909')
highlight.sector(as.character(df$geneid[97:101]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#99FF99')
highlight.sector(as.character(df$geneid[102:106]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF9933')
highlight.sector(as.character(df$geneid[107:107]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFCC99')
highlight.sector(as.character(df$geneid[108:108]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#996633')
highlight.sector(as.character(df$geneid[109:109]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFCC99')
highlight.sector(as.character(df$geneid[110:110]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#c72e29')

highlight.sector(as.character(df$geneid[111:114]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = 'palegreen')

highlight.sector(as.character(df$geneid[115:116]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[117:117]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#336600')
highlight.sector(as.character(df$geneid[118:118]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#009966')
highlight.sector(as.character(df$geneid[119:120]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFFF00')



highlight.sector(as.character(df$geneid[121:121]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')

highlight.sector(as.character(df$geneid[122:126]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[127:127]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = 'palegreen')
highlight.sector(as.character(df$geneid[128:128]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#66CCCC')
highlight.sector(as.character(df$geneid[129:130]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CCCC66')

highlight.sector(as.character(df$geneid[131:132]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#669999')
highlight.sector(as.character(df$geneid[133:158]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFCC00')
highlight.sector(as.character(df$geneid[159:160]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#99FF66')


highlight.sector(as.character(df$geneid[161:162]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#33FF99')



highlight.sector(as.character(df$geneid[163:166]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#339966')

highlight.sector(as.character(df$geneid[167:167]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#336666')
highlight.sector(as.character(df$geneid[168:171]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#33FFFF')

highlight.sector(as.character(df$geneid[172:174]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#669966')

highlight.sector(as.character(df$geneid[175:177]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#66CC99')

highlight.sector(as.character(df$geneid[178:208]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#00CC66')
highlight.sector(as.character(df$geneid[209:209]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#99FF33')

highlight.sector(as.character(df$geneid[210:261]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF99CC')
highlight.sector(as.character(df$geneid[262:265]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#003300')

highlight.sector(as.character(df$geneid[266:266]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#669900')
highlight.sector(as.character(df$geneid[267:267]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF3300')
highlight.sector(as.character(df$geneid[268:268]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF6666')



highlight.sector(as.character(df$geneid[269:269]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC0000')

highlight.sector(as.character(df$geneid[270:270]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#6600CC')
highlight.sector(as.character(df$geneid[271:272]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#0066FF')
highlight.sector(as.character(df$geneid[273:284]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#0099CC')

highlight.sector(as.character(df$geneid[285:288]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#6633FF')
highlight.sector(as.character(df$geneid[289:291]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#000099')
highlight.sector(as.character(df$geneid[292:294]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#330033')
highlight.sector(as.character(df$geneid[295:296]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#0000FF')
highlight.sector(as.character(df$geneid[297:297]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#000033')
highlight.sector(as.character(df$geneid[298:298]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#3366FF')
highlight.sector(as.character(df$geneid[299:299]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = 'palegreen')
highlight.sector(as.character(df$geneid[300:300]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#00CC66')
highlight.sector(as.character(df$geneid[301:301]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CCCC66')
highlight.sector(as.character(df$geneid[302:302]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#0033CC')


circos.trackPlotRegion(
  
  ylim = c(0, 1), track.height = 0.15, bg.border = NA,
  
  panel.fun = function(x, y) {
    
    sector.index = get.cell.meta.data('sector.index')
    
    xlim = get.cell.meta.data('xlim')
    
    ylim = get.cell.meta.data('ylim')
    
  } )

#otu
# 设置fc的大小对应的颜色，随着颜色从黑到黄到红过渡，fc值从-10至0至10
col_fun = colorRamp2(c(df$geneid), rand_color(302, luminosity = "random"))

circos.trackPlotRegion(
  
  ylim = c(0, 1), track.height = 0.05, bg.border = "black", bg.col =rand_color(302, luminosity = "random"),
  
  panel.fun = function(x, y) {
    
    sector.index = get.cell.meta.data('sector.index')
    
    xlim = get.cell.meta.data('xlim')
    
    ylim = get.cell.meta.data('ylim')
    
  } )



# 标注基因

for(i in 1:nrow(df)){
  
  circos.axis(sector.index= df[i,1], direction = "outside", labels=df[i,1],
              
              labels.facing = "reverse.clockwise",labels.cex=.33, col = 'black',
              labels.col= rand_color(302, luminosity = "random"),
              
              minor.ticks=0, major.at=seq(1, length(df$gene)))
  
}

#连线

taxonomy1 <- read.delim('~/Desktop/饥饿小龙虾文章/最新抽平数据/eco网络分析/con/ppcon.txt', sep = '\t', stringsAsFactors = FALSE)
taxonomy2 <- read.delim('~/Desktop/饥饿小龙虾文章/最新抽平数据/eco网络分析/con/npcon.txt', sep = '\t', stringsAsFactors = FALSE)
#画图
#画图
#注意源文件的空格得去掉
for(i in 1:nrow(taxonomy1)){
  
  circos.link(sector.index1 = taxonomy1[i,2], point1 = 0, sector.index2 = taxonomy1[i,1], point2 = 0 ,directional = 0,
              
              h=0.8, lwd=1, col="blue",lty=1) 
  
}

for(i in 1:nrow(taxonomy2)){
  
  circos.link(sector.index1 = taxonomy2[i,2], point1 = 0, sector.index2 = taxonomy2[i,1], point2 = 0 ,directional = 0,
              
              h=0.8, lwd=1, lty=1, col = "red") 
  
}







#第三圈 配体受体
## 读取数据库中配体-受体对关系文件

lr = read.csv('./ligand_receptors.csv',header = TRUE)



## 将配体和受体信息分开

#ldf = subset(df,lr == 'ligand')

#rdf = subset(df,lr == 'receptor')

## 创建新的数据框

lrid = lr

lrid = within(lrid,{geneid1 = NA})

lrid = within(lrid,{geneid2 = NA})

lrid = lrid[,c('Ligand','geneid1','Receptor','geneid2')]

## 添加geneid，用于后面画箭头的索引使用。

for(i in 1:nrow(rdf)){
  
  index = which(lrid$Receptor %in% rdf[i,3])
  
  for(ind in index){lrid[ind,4] = as.character(rdf[i,4])}
  
}

for(i in 1:nrow(ldf)){
  
  index = which(lrid$Ligand %in% ldf[i,3])
  
  for(ind in index){lrid[ind,2] = as.character(ldf[i,4])}
  
}

#去除NA

lrid = na.omit(lrid)
