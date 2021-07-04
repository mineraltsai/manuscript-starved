df = read.csv('~/Desktop/饥饿小龙虾文章/最新抽平数据/eco网络分析/starved/starvedtest.csv',header = T)
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

highlight.sector(as.character(df$geneid[6:141]), track.index = 1,
                 
                 text = 'Bacteroidia', niceFacing = F, font = 2,col ='#FF9900')

highlight.sector(as.character(df$geneid[142:142]), track.index = 1,
                 
                 text = 'Flavobacteriia', niceFacing = F, font = 2,col ='#33FFCC')

highlight.sector(as.character(df$geneid[143:147]), track.index = 1,
                 
                 text = 'Saprospirae', niceFacing = F, font = 2, col = '#FFCC99')

highlight.sector(as.character(df$geneid[148:149]), track.index = 1,    #FF3300
                 
                 text = 'Chloroplast', niceFacing = F, font = 2, col = '#c72e29')

highlight.sector(as.character(df$geneid[150:179]), track.index = 1,
                 
                 text = 'Deferribacteres', niceFacing = F, font = 2, col = 'palegreen')

highlight.sector(as.character(df$geneid[180:182]), track.index = 1,
                 
                 text = 'Bacilli', niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[183:290]), track.index = 1,
                 
                 text = 'Clostridia', niceFacing = F, font = 2, col = '#FFCC00')
highlight.sector(as.character(df$geneid[291:315]), track.index = 1,
                 
                 text = 'Erysipelotrichi', niceFacing = F, font = 2, col = '#FF99CC')

highlight.sector(as.character(df$geneid[316:316]), track.index = 1,
                 
                 text = 'Fusobacteriia', niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[317:319]), track.index = 1,
                 
                 text = 'Lentisphaeria', niceFacing = F, font = 2, col = '#FF3300')


highlight.sector(as.character(df$geneid[320:322]), track.index = 1,
                 
                 text = 'Alphaproteobacteria', niceFacing = F, font = 2, col = '#FFFF33')




highlight.sector(as.character(df$geneid[323:328]), track.index = 1,
                 
                 text = 'Betaproteobacteria', niceFacing = F, font = 2, col = '#ff00ff')


highlight.sector(as.character(df$geneid[329:329]), track.index = 1,
                 
                 text = 'Deltaproteobacteria', niceFacing = F, font = 2, col = '#FF00FF')


highlight.sector(as.character(df$geneid[330:335]), track.index = 1,
                 
                 text = 'Gammaproteobacteria', niceFacing = F, font = 2, col = '#FF0099')

highlight.sector(as.character(df$geneid[336:337]), track.index = 1,
                 
                 text = 'Spirochaetes', niceFacing = F, font = 2, col = '#CC00FF')

highlight.sector(as.character(df$geneid[338:338]), track.index = 1,
                 
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

highlight.sector(as.character(df$geneid[2:5]), track.index = 2,
                 
                 niceFacing = F, font = 2, col ='#016392')

highlight.sector(as.character(df$geneid[6:141]), track.index = 2,
                 
                niceFacing = F, font = 2,col ='#FF9900')

highlight.sector(as.character(df$geneid[142:142]), track.index = 2,
                 
                 niceFacing = F, font = 2,col ='#33FFCC')

highlight.sector(as.character(df$geneid[143:147]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#FFCC99')

highlight.sector(as.character(df$geneid[148:148]), track.index = 2,    #FF3300
                 
                  niceFacing = F, font = 2, col = '#c72e29')
highlight.sector(as.character(df$geneid[149:149]), track.index = 2,    #FF3300
                 
                 niceFacing = F, font = 2, col = '#FF66CC')

highlight.sector(as.character(df$geneid[150:179]), track.index = 2,
                 
               niceFacing = F, font = 2, col = 'palegreen')

highlight.sector(as.character(df$geneid[180:182]), track.index = 2,
                 
                niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[183:290]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#FFCC00')
highlight.sector(as.character(df$geneid[291:315]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#FF99CC')

highlight.sector(as.character(df$geneid[316:316]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[317:318]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#FF3300')
highlight.sector(as.character(df$geneid[319:319]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#FF6666')
highlight.sector(as.character(df$geneid[320:320]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#FFFF33')



highlight.sector(as.character(df$geneid[321:322]), track.index = 2,
                 
                niceFacing = F, font = 2, col = '#CC0000')

highlight.sector(as.character(df$geneid[323:326]), track.index = 2,
                 
                niceFacing = F, font = 2, col = '#ff00ff')


highlight.sector(as.character(df$geneid[327:327]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[328:328]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#ff00ff')


highlight.sector(as.character(df$geneid[329:329]), track.index = 2,
                 
                  niceFacing = F, font = 2, col = '#FF00FF')

highlight.sector(as.character(df$geneid[330:330]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#CC6699')

highlight.sector(as.character(df$geneid[331:331]), track.index = 2,
                 
                niceFacing = F, font = 2, col = '#FFCCFF')
highlight.sector(as.character(df$geneid[332:334]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#FF0099')
highlight.sector(as.character(df$geneid[335:335]), track.index = 2,
                 
                niceFacing = F, font = 2, col = '#CC00FF')
highlight.sector(as.character(df$geneid[336:337]), track.index = 2,
                 
                 niceFacing = F, font = 2, col = '#003300')
highlight.sector(as.character(df$geneid[338:338]), track.index = 2,
                 
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

highlight.sector(as.character(df$geneid[2:2]), track.index = 3,
                 
                 niceFacing = F, font = 2, col ='#016392')
highlight.sector(as.character(df$geneid[3:3]), track.index = 3,
                 
                 niceFacing = F, font = 2, col ='#CC3366')
highlight.sector(as.character(df$geneid[4:4]), track.index = 3,
                 
                 niceFacing = F, font = 2, col ='#669966')
highlight.sector(as.character(df$geneid[5:5]), track.index = 3,
                 
                 niceFacing = F, font = 2, col ='#990000')

highlight.sector(as.character(df$geneid[6:67]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#FF9900')
highlight.sector(as.character(df$geneid[68:72]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#CC3300')
highlight.sector(as.character(df$geneid[73:89]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#FF9966')
highlight.sector(as.character(df$geneid[90:120]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#663300')

highlight.sector(as.character(df$geneid[121:121]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#FF3333')
highlight.sector(as.character(df$geneid[122:140]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#FFCC99')
highlight.sector(as.character(df$geneid[141:141]), track.index = 3,
                 
                 niceFacing = F, font = 2,col ='#33FFCC')

highlight.sector(as.character(df$geneid[142:142]), track.index = 3,
                 
                  niceFacing = F, font = 2,col ='#33FFCC')

highlight.sector(as.character(df$geneid[143:146]), track.index = 3,
                 
                niceFacing = F, font = 2, col = '#FFCC99')

highlight.sector(as.character(df$geneid[147:147]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF0000')

highlight.sector(as.character(df$geneid[148:148]), track.index = 3,    #FF3300
                 
                  niceFacing = F, font = 2, col = '#c72e29')
highlight.sector(as.character(df$geneid[149:149]), track.index = 3,    #FF3300
                 
                  niceFacing = F, font = 2, col = '#FF66CC')



highlight.sector(as.character(df$geneid[150:179]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = 'palegreen')

highlight.sector(as.character(df$geneid[180:181]), track.index = 3,
                 
                niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[182:182]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[183:188]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CC3300')

highlight.sector(as.character(df$geneid[189:193]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF9966')
highlight.sector(as.character(df$geneid[194:194]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FFFF33')

highlight.sector(as.character(df$geneid[195:196]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FFCC00')



highlight.sector(as.character(df$geneid[197:219]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF9933')


highlight.sector(as.character(df$geneid[220:220]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#663300')
highlight.sector(as.character(df$geneid[221:227]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CC6633')

highlight.sector(as.character(df$geneid[228:286]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = '#FFCC00')
highlight.sector(as.character(df$geneid[287:287]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CC3333')

highlight.sector(as.character(df$geneid[288:290]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[291:315]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF99CC')



highlight.sector(as.character(df$geneid[316:316]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[317:318]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF3300')
highlight.sector(as.character(df$geneid[319:319]), track.index = 3,
                 
                niceFacing = F, font = 2, col = '#FF6666')
highlight.sector(as.character(df$geneid[320:320]), track.index = 3,
                 
                niceFacing = F, font = 2, col = '#FFFF33')



highlight.sector(as.character(df$geneid[321:322]), track.index = 3,
                 
                niceFacing = F, font = 2, col = '#CC0000')

highlight.sector(as.character(df$geneid[323:324]), track.index = 3,
                 
                niceFacing = F, font = 2, col = '#ff00ff')

highlight.sector(as.character(df$geneid[325:326]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF00FF')


highlight.sector(as.character(df$geneid[327:327]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[328:328]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')


highlight.sector(as.character(df$geneid[329:329]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF00FF')

highlight.sector(as.character(df$geneid[330:330]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CC6699')

highlight.sector(as.character(df$geneid[331:331]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FFCCFF')
highlight.sector(as.character(df$geneid[332:334]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#FF0099')
highlight.sector(as.character(df$geneid[335:335]), track.index = 3,
                 
                 niceFacing = F, font = 2, col = '#CC00FF')
highlight.sector(as.character(df$geneid[336:337]), track.index = 3,
                 
                  niceFacing = F, font = 2, col = '#003300')
highlight.sector(as.character(df$geneid[338:338]), track.index = 3,
                 
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

highlight.sector(as.character(df$geneid[2:2]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#016392')
highlight.sector(as.character(df$geneid[3:3]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#CC3366')
highlight.sector(as.character(df$geneid[4:4]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#669966')
highlight.sector(as.character(df$geneid[5:5]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#990000')

highlight.sector(as.character(df$geneid[6:58]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#FF9900')
highlight.sector(as.character(df$geneid[59:64]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#CC99FF')
highlight.sector(as.character(df$geneid[65:67]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#993399')

highlight.sector(as.character(df$geneid[68:68]), track.index = 4,
                 
         niceFacing = F, font = 2,col ='#33FFCC')

highlight.sector(as.character(df$geneid[69:72]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#CC3300')

highlight.sector(as.character(df$geneid[73:73]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFCC99')

highlight.sector(as.character(df$geneid[74:74]), track.index = 4,
                 
                 niceFacing = F, font = 2, col ='#669966')


highlight.sector(as.character(df$geneid[75:87]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#FF9966')


highlight.sector(as.character(df$geneid[88:89]), track.index = 4,    #FF3300
                 
                  niceFacing = F, font = 2, col = '#c72e29')


highlight.sector(as.character(df$geneid[90:101]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#663300')
highlight.sector(as.character(df$geneid[102:103]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')

highlight.sector(as.character(df$geneid[104:114]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = 'palegreen')
highlight.sector(as.character(df$geneid[115:115]), track.index = 4,
                 
                niceFacing = F, font = 2, col = '#9966CC')

highlight.sector(as.character(df$geneid[116:140]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#FFCC99')

highlight.sector(as.character(df$geneid[141:141]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#33FFCC')

highlight.sector(as.character(df$geneid[142:142]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#33FFCC')

highlight.sector(as.character(df$geneid[143:144]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFCC99')


highlight.sector(as.character(df$geneid[145:145]), track.index = 4,
                 
                 niceFacing = F, font = 2,col ='#FF3333')

highlight.sector(as.character(df$geneid[146:147]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF0000')

highlight.sector(as.character(df$geneid[148:148]), track.index = 4,    #FF3300
                 
                 niceFacing = F, font = 2, col = '#c72e29')
highlight.sector(as.character(df$geneid[149:149]), track.index = 4,    #FF3300
                 
                 niceFacing = F, font = 2, col = '#FF66CC')

highlight.sector(as.character(df$geneid[150:177]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = 'palegreen')
highlight.sector(as.character(df$geneid[178:179]), track.index = 4,    #FF3300
                 
                  niceFacing = F, font = 2, col = '#c72e29')

highlight.sector(as.character(df$geneid[180:180]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[181:181]), track.index = 4,
                 
                niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[182:182]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[183:188]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC3300')

highlight.sector(as.character(df$geneid[189:193]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF9966')
highlight.sector(as.character(df$geneid[194:194]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFFF33')

highlight.sector(as.character(df$geneid[195:195]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFCC00')
highlight.sector(as.character(df$geneid[196:196]), track.index = 4,
                 
                niceFacing = F, font = 2, col = '#FFCC00')

highlight.sector(as.character(df$geneid[197:197]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CCFFFF')
highlight.sector(as.character(df$geneid[198:200]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#660099')
highlight.sector(as.character(df$geneid[201:201]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#330099')

highlight.sector(as.character(df$geneid[202:219]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF9933')


highlight.sector(as.character(df$geneid[220:220]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#663300')

highlight.sector(as.character(df$geneid[221:222]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CCCC00')


highlight.sector(as.character(df$geneid[223:227]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC6633')
highlight.sector(as.character(df$geneid[228:228]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF3300')
highlight.sector(as.character(df$geneid[229:232]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#330033')
highlight.sector(as.character(df$geneid[233:236]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#3300FF')
highlight.sector(as.character(df$geneid[237:238]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#0099CC')

highlight.sector(as.character(df$geneid[239:254]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFCC00')
highlight.sector(as.character(df$geneid[255:263]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#3300CC')
highlight.sector(as.character(df$geneid[264:272]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#9966CC')
highlight.sector(as.character(df$geneid[273:286]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CCFF33')


highlight.sector(as.character(df$geneid[287:287]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC3333')

highlight.sector(as.character(df$geneid[288:290]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[291:293]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF00FF')



highlight.sector(as.character(df$geneid[294:300]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF99CC')
highlight.sector(as.character(df$geneid[301:301]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF0099')
highlight.sector(as.character(df$geneid[302:314]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC00FF')

highlight.sector(as.character(df$geneid[315:315]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#00CCFF')


highlight.sector(as.character(df$geneid[316:316]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#009966')

highlight.sector(as.character(df$geneid[317:318]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF3300')
highlight.sector(as.character(df$geneid[319:319]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF6666')
highlight.sector(as.character(df$geneid[320:320]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFFF33')



highlight.sector(as.character(df$geneid[321:322]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC0000')

highlight.sector(as.character(df$geneid[323:324]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')

highlight.sector(as.character(df$geneid[325:326]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF00FF')


highlight.sector(as.character(df$geneid[327:327]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')
highlight.sector(as.character(df$geneid[328:328]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#ff00ff')


highlight.sector(as.character(df$geneid[329:329]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF00FF')

highlight.sector(as.character(df$geneid[330:330]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC6699')

highlight.sector(as.character(df$geneid[331:331]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FFCCFF')
highlight.sector(as.character(df$geneid[332:334]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#FF0099')
highlight.sector(as.character(df$geneid[335:335]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#CC00FF')
highlight.sector(as.character(df$geneid[336:337]), track.index = 4,
                 
                 niceFacing = F, font = 2, col = '#003300')
highlight.sector(as.character(df$geneid[338:338]), track.index = 4,
                 
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
col_fun = colorRamp2(c(df$geneid), rand_color(338, luminosity = "random"))

circos.trackPlotRegion(
  
  ylim = c(0, 1), track.height = 0.05, bg.border = "black", bg.col =rand_color(338, luminosity = "random"),
  
  panel.fun = function(x, y) {
    
    sector.index = get.cell.meta.data('sector.index')
    
    xlim = get.cell.meta.data('xlim')
    
    ylim = get.cell.meta.data('ylim')
    
  } )



# 标注基因

for(i in 1:nrow(df)){
  
  circos.axis(sector.index= df[i,1], direction = "outside", labels=df[i,1],
              
              labels.facing = "reverse.clockwise",labels.cex=.33, col = 'black',
              labels.col= rand_color(338, luminosity = "random"),
              
              minor.ticks=0, major.at=seq(1, length(df$gene)))
  
}

#连线

taxonomy1 <- read.delim('~/Desktop/饥饿小龙虾文章/最新抽平数据/eco网络分析/starved/ppstarved.txt', sep = '\t', stringsAsFactors = FALSE)
taxonomy2 <- read.delim('~/Desktop/饥饿小龙虾文章/最新抽平数据/eco网络分析/starved/npstarved.txt', sep = '\t', stringsAsFactors = FALSE)
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
