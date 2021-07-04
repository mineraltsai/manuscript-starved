df = read.csv('~/Desktop/饥饿小龙虾文章/最新抽平数据/eco网络分析/refed/refedtest.csv',header = T)
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

highlight.sector(as.character(df$geneid[2:6]), track.index = 1,
                 
                 text = 'Actinobacteria', niceFacing = F, font = 2, col ='#016392')
highlight.sector(as.character(df$geneid[7:7]), track.index = 1,
                 
                 text = 'Coriobacteriia', niceFacing = F, font = 2, col ='#669966')

highlight.sector(as.character(df$geneid[8:118]), track.index = 1,
                 
                 text = 'Bacteroidia', niceFacing = F, font = 2,col ='#FF9900')
highlight.sector(as.character(df$geneid[119:119]), track.index = 1,
                 
                 text = 'Flavobacteriia', niceFacing = F, font = 2,col ='#33FFCC')

highlight.sector(as.character(df$geneid[120:124]), track.index = 1,
                 
                 text = 'Saprospirae', niceFacing = F, font = 2, col = '#FFCC99')

highlight.sector(as.character(df$geneid[125:125]), track.index = 1,    #FF3300
                 
                 text = 'Chloroplast', niceFacing = F, font = 2, col = '#c72e29')

highlight.sector(as.character(df$geneid[126:140]), track.index = 1,
                 
                 text = 'Deferribacteres', niceFacing = F, font = 2, col = 'palegreen')

highlight.sector(as.character(df$geneid[141:143]), track.index = 1,
                 
                 text = 'Bacilli', niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[144:256]), track.index = 1,
                 
                 text = 'Clostridia', niceFacing = F, font = 2, col = '#FFCC00')
highlight.sector(as.character(df$geneid[257:289]), track.index = 1,
                 
                 text = 'Erysipelotrichi', niceFacing = F, font = 2, col = '#FF99CC')

highlight.sector(as.character(df$geneid[290:290]), track.index = 1,
                 
                 text = 'Fusobacteriia', niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[291:292]), track.index = 1,
                 
                 text = 'Lentisphaeria', niceFacing = F, font = 2, col = '#FF3300')


highlight.sector(as.character(df$geneid[293:295]), track.index = 1,
                 
                 text = 'Alphaproteobacteria', niceFacing = F, font = 2, col = '#FFFF33')



highlight.sector(as.character(df$geneid[296:301]), track.index = 1,
                 
                 text = 'Betaproteobacteria', niceFacing = F, font = 2, col = '#ff00ff')


highlight.sector(as.character(df$geneid[302:302]), track.index = 1,
                 
                 text = 'Deltaproteobacteria', niceFacing = F, font = 2, col = '#ff00ff')

highlight.sector(as.character(df$geneid[303:322]), track.index = 1,
                 
                 text = 'Gammaproteobacteria', niceFacing = F, font = 2, col = '#FF0099')


highlight.sector(as.character(df$geneid[323:324]), track.index = 1,
                 
                 text = 'Spirochaetes', niceFacing = F, font = 2, col = '#003300')
highlight.sector(as.character(df$geneid[325:325]), track.index = 1,
                 
                 text = 'Mollicutes', niceFacing = F, font = 2, col = '#669900')


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

highlight.sector(as.character(df$geneid[2:6]), track.index = 2,
                 
                niceFacing = F, font = 2, col ='#016392')
highlight.sector(as.character(df$geneid[7:7]), track.index = 2,
                 
                 niceFacing = F, font = 2, col ='#669966')

highlight.sector(as.character(df$geneid[8:118]), track.index = 2,
                 
                 niceFacing = F, font = 2,col ='#FF9900')
highlight.sector(as.character(df$geneid[119:119]), track.index = 2,
                 
                niceFacing = F, font = 2,col ='#33FFCC')

highlight.sector(as.character(df$geneid[120:124]), track.index = 2,
                 
               niceFacing = F, font = 2, col = '#FFCC99')

highlight.sector(as.character(df$geneid[125:125]), track.index = 2,    #FF3300
                 
                niceFacing = F, font = 2, col = '#c72e29')

highlight.sector(as.character(df$geneid[126:140]), track.index = 2,
                 
                niceFacing = F, font = 2, col = 'palegreen')

highlight.sector(as.character(df$geneid[141:143]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[144:256]), track.index = 2,
                 
                niceFacing = F, font = 2, col = '#FFCC00')
highlight.sector(as.character(df$geneid[257:289]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#FF99CC')

highlight.sector(as.character(df$geneid[290:290]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[291:291]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#FF3300')
highlight.sector(as.character(df$geneid[292:292]), track.index = 2,
                 
                niceFacing = F, font = 2, col = '#FF6666')
highlight.sector(as.character(df$geneid[293:294]), track.index = 2,
                 
                niceFacing = F, font = 2, col = '#FFFF33')



highlight.sector(as.character(df$geneid[295:295]), track.index = 2,
                 
                niceFacing = F, font = 2, col = '#CC0000')

highlight.sector(as.character(df$geneid[296:299]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[300:301]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[302:302]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')


highlight.sector(as.character(df$geneid[303:303]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#FF00FF')

highlight.sector(as.character(df$geneid[304:305]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#CC6699')
highlight.sector(as.character(df$geneid[306:319]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#FF0099')
highlight.sector(as.character(df$geneid[320:322]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#CC00FF')
highlight.sector(as.character(df$geneid[323:324]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#003300')
highlight.sector(as.character(df$geneid[325:325]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#669900')

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
                 
                 niceFacing = F, font = 2, col ='#669966')
highlight.sector(as.character(df$geneid[5:5]), track.index = 3,
                 
                 niceFacing = F, font = 2, col ='#990000')
highlight.sector(as.character(df$geneid[6:6]), track.index = 3,
                 
                 niceFacing = F, font = 2, col ='#FF3333')
highlight.sector(as.character(df$geneid[7:7]), track.index = 3,
                 
                 niceFacing = F, font = 2, col ='#FF3300')

highlight.sector(as.character(df$geneid[8:72]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#FF9900')
highlight.sector(as.character(df$geneid[73:74]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#FF3333')

highlight.sector(as.character(df$geneid[75:90]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#CC3300')
highlight.sector(as.character(df$geneid[91:109]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#FF9966')
highlight.sector(as.character(df$geneid[110:116]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#663300')
highlight.sector(as.character(df$geneid[117:118]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#FFCC99')

highlight.sector(as.character(df$geneid[119:119]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#33FFCC')


highlight.sector(as.character(df$geneid[120:123]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FFCC99')
highlight.sector(as.character(df$geneid[124:124]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF0000')

highlight.sector(as.character(df$geneid[125:125]), track.index = 3,    #FF3300
                 
                 niceFacing = F, font = 2, col = '#c72e29')

highlight.sector(as.character(df$geneid[126:140]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = 'palegreen')

highlight.sector(as.character(df$geneid[141:141]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')

highlight.sector(as.character(df$geneid[142:142]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CC3300')

highlight.sector(as.character(df$geneid[143:143]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF9966')

highlight.sector(as.character(df$geneid[144:148]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FFFF33')


highlight.sector(as.character(df$geneid[149:162]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FFCC00')


highlight.sector(as.character(df$geneid[163:164]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF9933')

highlight.sector(as.character(df$geneid[165:202]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#663300')
highlight.sector(as.character(df$geneid[203:211]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CC6633')

highlight.sector(as.character(df$geneid[212:253]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FFCC66')
highlight.sector(as.character(df$geneid[254:256]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CC3333')


highlight.sector(as.character(df$geneid[257:289]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF99CC')

highlight.sector(as.character(df$geneid[290:290]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[291:291]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF3300')
highlight.sector(as.character(df$geneid[292:292]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF6666')
highlight.sector(as.character(df$geneid[293:294]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FFFF33')



highlight.sector(as.character(df$geneid[295:295]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CC0000')

highlight.sector(as.character(df$geneid[296:296]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#003333').
highlight.sector(as.character(df$geneid[297:298]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[299:299]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#006633')
highlight.sector(as.character(df$geneid[300:301]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[302:302]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')


highlight.sector(as.character(df$geneid[303:303]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF00FF')

highlight.sector(as.character(df$geneid[304:305]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CC6699')
highlight.sector(as.character(df$geneid[306:313]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF0099')

highlight.sector(as.character(df$geneid[314:316]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#33FF99')

highlight.sector(as.character(df$geneid[317:317]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#006633')
highlight.sector(as.character(df$geneid[318:319]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#003333')


highlight.sector(as.character(df$geneid[320:320]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CC00FF')
highlight.sector(as.character(df$geneid[321:321]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#669966')
highlight.sector(as.character(df$geneid[322:322]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#336633')
highlight.sector(as.character(df$geneid[323:324]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#003300')
highlight.sector(as.character(df$geneid[325:325]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#669900')
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

highlight.sector(as.character(df$geneid[2:3]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#016392')
highlight.sector(as.character(df$geneid[4:4]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#669966')
highlight.sector(as.character(df$geneid[5:5]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#990000')
highlight.sector(as.character(df$geneid[6:6]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#FF3333')
highlight.sector(as.character(df$geneid[7:7]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#FF3300')

highlight.sector(as.character(df$geneid[8:66]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#FF9900')
highlight.sector(as.character(df$geneid[67:69]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#00CCCC')
highlight.sector(as.character(df$geneid[70:72]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#999933')
highlight.sector(as.character(df$geneid[73:74]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#FF3333')

highlight.sector(as.character(df$geneid[75:90]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#CC3300')
highlight.sector(as.character(df$geneid[91:98]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#FF33CC')
highlight.sector(as.character(df$geneid[99:105]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#99FF66')
highlight.sector(as.character(df$geneid[106:116]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#663300')
highlight.sector(as.character(df$geneid[117:118]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#FFCC99')


highlight.sector(as.character(df$geneid[119:119]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#33FFCC')


highlight.sector(as.character(df$geneid[120:123]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#003333')
highlight.sector(as.character(df$geneid[124:124]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF0000')

highlight.sector(as.character(df$geneid[125:125]), track.index = 4,    #FF3300
                 
                 niceFacing = F, font = 2, col = '#c72e29')

highlight.sector(as.character(df$geneid[126:140]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = 'palegreen')

highlight.sector(as.character(df$geneid[141:141]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')

highlight.sector(as.character(df$geneid[142:142]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC3300')

highlight.sector(as.character(df$geneid[143:143]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF9966')

highlight.sector(as.character(df$geneid[144:148]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFFF33')


highlight.sector(as.character(df$geneid[149:162]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFCC00')


highlight.sector(as.character(df$geneid[163:164]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF9933')
highlight.sector(as.character(df$geneid[165:165]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#669999')
highlight.sector(as.character(df$geneid[166:168]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#66FF33')
highlight.sector(as.character(df$geneid[169:171]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#666600')

highlight.sector(as.character(df$geneid[172:200]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#663300')

highlight.sector(as.character(df$geneid[201:206]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#66CCCC')

highlight.sector(as.character(df$geneid[207:211]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC6633')

highlight.sector(as.character(df$geneid[212:212]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#99FF99')
highlight.sector(as.character(df$geneid[213:213]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[214:215]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#33FF33')


highlight.sector(as.character(df$geneid[216:230]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFCC66')

highlight.sector(as.character(df$geneid[231:241]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC6633')

highlight.sector(as.character(df$geneid[242:249]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#33FFFF')
highlight.sector(as.character(df$geneid[250:256]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#336666')

highlight.sector(as.character(df$geneid[257:260]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC3333')
highlight.sector(as.character(df$geneid[261:261]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#339966')

highlight.sector(as.character(df$geneid[262:273]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#66CC66')
highlight.sector(as.character(df$geneid[274:294]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF99CC')



highlight.sector(as.character(df$geneid[295:295]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[296:296]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF3300')
highlight.sector(as.character(df$geneid[297:297]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF6666')
highlight.sector(as.character(df$geneid[298:298]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFFF33')



highlight.sector(as.character(df$geneid[299:299]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC0000')

highlight.sector(as.character(df$geneid[300:301]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[302:302]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#336600')


highlight.sector(as.character(df$geneid[303:303]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF00FF')

highlight.sector(as.character(df$geneid[304:305]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC6699')
highlight.sector(as.character(df$geneid[306:319]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF0099')
highlight.sector(as.character(df$geneid[320:322]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC00FF')
highlight.sector(as.character(df$geneid[323:324]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#003300')

highlight.sector(as.character(df$geneid[325:325]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#669900')


circos.trackPlotRegion(
  
  ylim = c(0, 1), track.height = 0.15, bg.border = NA,
  
  panel.fun = function(x, y) {
    
    sector.index = get.cell.meta.data('sector.index')
    
    xlim = get.cell.meta.data('xlim')
    
    ylim = get.cell.meta.data('ylim')
    
  } )

#otu
# 设置fc的大小对应的颜色，随着颜色从黑到黄到红过渡，fc值从-10至0至10
col_fun = colorRamp2(c(df$geneid), rand_color(325, luminosity = "random"))

circos.trackPlotRegion(
  
  ylim = c(0, 1), track.height = 0.05, bg.border = "black", bg.col =rand_color(325, luminosity = "random"),
  
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

taxonomy1 <- read.delim('~/Desktop/饥饿小龙虾文章/最新抽平数据/eco网络分析/refed/pprefed.txt', sep = '\t', stringsAsFactors = FALSE)
taxonomy2 <- read.delim('~/Desktop/饥饿小龙虾文章/最新抽平数据/eco网络分析/refed/nprefed.txt', sep = '\t', stringsAsFactors = FALSE)
#画图
#画图
#注意源文件的空格得去掉
for(i in 1:nrow(taxonomy2)){
  
  circos.link(sector.index1 = taxonomy2[i,2], point1 = 0, sector.index2 = taxonomy2[i,1], point2 = 0 ,directional = 0,
              
              h=0.8, lwd=1, lty=1, col = "red") 
  
}
for(i in 1:nrow(taxonomy1)){

circos.link(sector.index1 = taxonomy1[i,2], point1 = 0, sector.index2 = taxonomy1[i,1], point2 = 0 ,directional = 0,

           h=0.8, lwd=1, col="blue",lty=1) 

}



