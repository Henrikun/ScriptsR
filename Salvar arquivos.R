
##    To save figures and tables using tikzDevice
library(tikzDevice) 
tikz("minhafigura.tex",width=5.5,height=4)
realgdp = ggplot(data = gdp, aes(x = Time, y = GDP)) + geom_line(aes(color = GDP))
realgdp
dev.off()

##    To save a graphic as a pdf file: 
pdf("Rgraphics.pdf",width=6,height=4,paper='special') 
plot.cycles(g,"")
dev.off()



