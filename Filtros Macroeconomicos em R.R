# Download and transform data
gdp <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?chart_type=line&recession_bars=on&log_scales=&bgcolor=%23e1e9f0&graph_bgcolor=%23ffffff&fo=Open+Sans&ts=12&tts=12&txtcolor=%23444444&show_legend=yes&show_axis_titles=yes&drp=0&cosd=1947-01-01&coed=2016-10-01&height=450&stacking=&range=&mode=fred&id=GDPC1&transformation=lin&nd=1947-01-01&ost=-99999&oet=99999&lsv=&lev=&mma=0&fml=a&fgst=lin&fgsnd=2009-06-01&fq=Quarterly&fam=avg&vintage_date=&revision_date=&line_color=%234572a7&line_style=solid&lw=2&scale=left&mark_type=none&mw=2&width=1168",colClass=c("Date","numeric")) # Download data
names(gdp) <- c("Time","GDP") # Rename variables
gdp[,"GDP"] <- log(gdp[,"GDP"]) # Take logs

library(ggplot2)
library(reshape2)
ggplot(gdp,aes(x=Time,y=GDP)) + geom_line(size=.5) + theme_classic() + labs(title="Log Real US GDP")

realgdp = ggplot(data = gdp, aes(x = Time, y = GDP)) + geom_line(aes(color = GDP))
realgdp

time.detrend <- residuals(lm(GDP ~ Time, data=gdp)) # Regress GDP on Time and obtain residuals

# Plot
dat <- data.frame("Time"=gdp[,"Time"],"Linearly.Detrended"=time.detrend)
ggplot(dat,aes(x=Time,y=Linearly.Detrended)) + geom_hline(yintercept=0,colour="grey80") + geom_line(size=.5) + theme_classic() + labs(title="Linearly Detrended",y="")

timedetrending = ggplot(dat = gdp, aes(x = Time, y = time.detrend)) + geom_hline(yintercept=0,colour="red") + geom_line(aes(color = time.detrend)) + labs(y="Linearly Detrended")
timedetrending

fd <- diff(gdp[,"GDP"]) # Take first difference

# Plot
dat <- data.frame("Time"=dat[,"Time"],"First.Difference"=c(NA,fd),"Linearly.Detrended"=dat[,"Linearly.Detrended"])
g <- melt(dat,id.vars="Time",na.rm=TRUE)
levels(g[,2]) <- c("First Difference","Linear Trend")

# Define plot function
plot.cycles <- function(d,t) {
  ggplot(g,aes(x=Time,y=value,linetype=variable)) + 
    geom_hline(yintercept=0,colour="grey80") + # Create a horizontal line with value 0
    geom_line( size=.5) + # Create line with series and specify its thickness
    labs(x="Time",y="",title=t,linetype="Legend:") + # Title of the legend
    coord_cartesian(xlim=c(min(g[,1]),max(g[,1]))) + # Set displayed area
    guides(linetype=guide_legend()) + # Set the variables contained in the legend
    theme(legend.position="bottom", # Position of the legend
          legend.key=element_rect(fill="white"), # Set background of the legend keys
          panel.background = element_rect(fill = "white"), # Set background of the graph
          axis.line=element_line(size=.3,colour="black"), # Set the size and colour of the axes
          axis.text=element_text(colour="black"), # Set the colour of the axes text
          panel.grid=element_blank()) # Set grid lines off
}

# Plot
plot.cycles(d=g,t="Linearly Detrended vs. First Difference")

diff1 <- data.frame("Time"=dat[,"Time"],"First.Difference"=c(NA,fd) )

firstdifference = ggplot(data=diff1, aes(x = Time, y = diff1$First.Difference )) + geom_hline(yintercept=0,colour="red") + geom_line(aes(color = First.Difference)) + labs(y="First Difference")
firstdifference

ggplot(data=g, aes(x=Time, y=value, group=variable)) + geom_line(aes(color=variable)) +
geom_hline(yintercept=0,colour="black") + # Create a horizontal line with value 0
labs(x="Time",y="") # Title of the legend




library(mFilter)
hp <- hpfilter(gdp[,2],freq=1600)$cycle # Apply filter and obtain data of the cycle compontent

# Plot
dat <- cbind(dat,data.frame("Hodrick.Prescott"=hp))
g <- melt(dat[,c(1,4,3)],id.vars="Time",na.rm=TRUE)
levels(g[,2]) <- c("Hodrick Prescott","Linearly Detrended")

plot.cycles(g,"Hodrick Prescott vs. Linearly Detrended")

hpff <- data.frame("Time"=dat[,"Time"],"Hodrick.Prescott"=hp )

hpfilter = ggplot(data=hpff, aes(x = Time, y = hpff$Hodrick.Prescott )) + geom_hline(yintercept=0,colour="red") + geom_line(aes(color = Hodrick.Prescott)) + labs(y="Hodrick Prescott")
hpfilter

bk <- bkfilter(gdp[,2],pl=6,pu=32,nfix=12)$cycle[,1] # Apply filter and obtain cycle component

# Plot
dat <- cbind(dat,data.frame("Baxter.King"=bk))
g <- melt(dat[,c(1,5,4)],id.vars="Time",na.rm=TRUE)
levels(g[,2]) <- c("Baxter King","Hodrick Prescott")

plot.cycles(g,"Baxter King vs. Hodrick Prescott")

bkff <- data.frame("Time"=dat[,"Time"],"Baxter.King"=bk )

bkfilter = ggplot(data=bkff, aes(x = Time, y = Baxter.King )) + geom_hline(yintercept=0,colour="red") + geom_line(aes(color = Baxter.King)) + labs(y="Baxter King")
bkfilter


library(waveslim)

wavelet <- as.data.frame(mra(diff(gdp[,2]),J=5)) # Apply filter

# Plot
dat <- cbind(dat,data.frame("Wavelet"=c(NA,cumsum(rowSums(wavelet[,3:4])))))
g <- melt(dat[,c(1,6,5)],id.vars="Time",na.rm=TRUE)
levels(g[,2]) <- c("Wavelet","Baxter King")

plot.cycles(g,"Wavelet vs. Baxter King")

wff <- data.frame("Time"=dat[,"Time"],"Wavelet.Filter"=dat$Wavelet)

waveletfilter = ggplot(data=wff, aes(x = Time, y = Wavelet.Filter )) + geom_hline(yintercept=0,colour="red") + geom_line(aes(color = Wavelet.Filter)) + labs(y="Wavelet Filter")
waveletfilter

library(EMD)
emd <- as.data.frame(emd(xt=diff(gdp[,2]),boundary="wave",stoprule="type2")$imf)

dat <- cbind(dat,data.frame("EMD"=c(NA,cumsum(rowSums(emd[,3:6])))))
g <- melt(dat[,c(1,7,4)],id.vars="Time",na.rm=TRUE)
levels(g[,2]) <- c("EMD","Hodrick Prescott")

plot.cycles(g,"EMD vs. Hodrick Prescott")


emdff <- data.frame("Time"=dat[,"Time"],"EMD.Filter"=dat$EMD)

emdfilter = ggplot(data=emdff, aes(x = Time, y = EMD.Filter )) + geom_hline(yintercept=0,colour="red") + geom_line(aes(color = EMD.Filter)) + labs(y="EMD Filter")
emdfilter



