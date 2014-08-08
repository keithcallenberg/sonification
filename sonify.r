#bouncing ball
library(diffEq)
yini <- c(height = 10, velocity = 0)
ball <- function(t, y, parms) {
  dy1 <- y[2]
  dy2 <- -9.8
  list(c(dy1, dy2))
}
rootfunc <- function(t, y, parms) y[1]
eventfunc <- function(t, y, parms) {
  y[1] <- 0
  y[2] <- -0.8*y[2]
  return(y)
}
times <- seq(from = 0, to = 12, by = 0.01)
out <- ode(times = times, y = yini, func = ball,
           parms = NULL, rootfun = rootfunc,
           events = list(func = eventfunc, root = TRUE))

plot(out, which = "height", lwd = 2,
     main = "bouncing ball", ylab = "height")

library(playitbyr)
csound::setCsoundLibrary('/usr/local/lib/libcsnd.so')


x <- sonify(as.data.frame(out), sonaes(time = time, pitch = height)) + 
  shape_scatter() + 
  scale_pitch_continuous(c(7, 12))

sonsave(x,where='ball.wav',play=F)

####
#foreclosures
library(lubridate)
fore <- read.csv('pgh/foreclosures.csv',header=T)

#function to handle 2 digit years correctly
yearfix <- function(x, year=1913){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}

fore$SaleDate.D <- mdy(fore$SaleDate.D)
fore$SaleDate.D <- yearfix(fore$SaleDate.D)
fore <- fore[order(fore$SaleDate.D),]

x <- sonify(fore, sonaes(time = SaleDate.D, pitch = NeighCode.C.8)) + 
  shape_dotplot() + 
  scale_time_continuous(c(0, 20)) +
  scale_pitch_continuous(c(7, 12))

sonsave(x,where='foreclosures.wav',play=F)



####
#anscombe's quartet

#sort anscombe by x then y so that the highlighted points appear in order
sorted.anscombe <- anscombe
sorted.anscombe[,c(1,5)] <- anscombe[order(anscombe[,1],anscombe[,5]),c(1,5)]
sorted.anscombe[,c(2,6)] <- anscombe[order(anscombe[,2],anscombe[,6]),c(2,6)]
sorted.anscombe[,c(3,7)] <- anscombe[order(anscombe[,3],anscombe[,7]),c(3,7)]
sorted.anscombe[,c(4,8)] <- anscombe[order(anscombe[,4],anscombe[,8]),c(4,8)]

file.idx <- 1 # have to do this ugly counter because of the sort
num.copies <- 9
for(k in 1:4) {
    for(j in 1:length(anscombe[,1])) {
        base.file <- paste("anscombe/ans_",file.idx,".png",sep="")
        png(base.file, width=1200, height=900) 
        op <- par(las=1, mfrow=c(2,2), mar=1.5+c(4,4,1,1), oma=c(0,0,0,0), lab=c(6,6,7), cex.lab=2.0, cex.axis=1.3, mgp=c(3,1,0)) 
        ff <- y ~ x 
        for(i in 1:4) { 
            ff[[2]] <- as.name(paste("y", i, sep="")) 
            ff[[3]] <- as.name(paste("x", i, sep="")) 
            lmi <- lm(ff, data=anscombe) 
            xl <- substitute(expression(x[i]), list(i=i)) 
            yl <- substitute(expression(y[i]), list(i=i)) 
            plot(ff, data=anscombe, col="red", pch=21, cex=2.4, bg = "orange", xlim=c(3,19), ylim=c(3,13) , xlab=eval(xl), ylab=yl) 
            abline(lmi, col="blue") 
            if (i == k) points(ff, data=sorted.anscombe[j,], col="black", pch=21, cex=2.4, bg = "black")
        } 
        par(op) 
        dev.off()
        for(copy in 1:num.copies) {
            file.copy(base.file, paste("anscombe/ans_",(file.idx+copy),".png",sep=""))
        }
        file.idx <- file.idx + num.copies + 1
    }
    for(copy in 0:(2*num.copies)) {
        file.copy(base.file, paste("anscombe/ans_",(file.idx+copy),".png",sep=""))
    }
    file.idx <- file.idx + (2* num.copies) + 1
}

ans=with(anscombe,data.frame(xVal=c(x1,x2,x3,x4), yVal=c(y1,y2,y3,y4), group=gl(4,nrow(anscombe))))

#ggplot(mydata,aes(x=xVal, y=yVal)) + geom_point() + facet_wrap(~group)

x <- sonify(ans, sonaes(time = xVal, pitch = yVal)) +
    sonfacet(group) +
    shape_scatter() + 
    scale_time_continuous(c(0, 20)) +
    scale_pitch_continuous(c(7, 12))

sonsave(x,where='anscombe.wav',play=F)


#######
#speck 12x12

library(RJSONIO)
meta <- fromJSON('speck/speck_12x12_metadata.json')

#loop over devices
speck <- data.frame(rep(NA,4141))

for(device in seq_along(meta$devices)) {
    csv <- read.csv(paste('speck/',meta$devices[[device]]$name,'.csv',sep=''))
    if (device==1) {
        speck[,1] <- csv[,1]
        names(speck)[1] <- 'EpochTime'
    }
    if (meta$devices[[device]]$numRecords != 4141) {
        speck <- merge(speck, csv,by='EpochTime',all.x=T)
    } else {
        speck[,(device+1)] <- csv[,2]
    }
    names(speck)[device+1] <- paste(meta$devices[[device]]$latitude,'.',meta$devices[[device]]$longitude,sep='')
}

#reorder data frame columns
speck.cols <- colnames(speck)[2:145]
lat <- as.numeric(sub("\\..*","",speck.cols))
long <- as.numeric(sub(".*\\.","",speck.cols))
col.order <- order(lat,long)
col.order <- col.order + 1  # account for date column
speck <- speck[,c(1,col.order)]

library(RColorBrewer)
new.palette <- colorRampPalette(rev(brewer.pal(9,"RdYlGn")))(n = 1000)
for(i in 1:length(speck[,1])) {
    png(paste('speck/heatmap/speck_',i,'.png',sep=''), width=1200, height=900) 
    heatmap(matrix(as.numeric(speck[i,2:145]),nrow=12,ncol=12,byrow=T),
            Rowv=NA,Colv=NA,scale='none',col=new.palette,labRow=NA,labCol=NA)
    dev.off()
}

x <- sonify(speck, sonaes(time = EpochTime, pitch = '0.0', pan=0)) +
    shape_scatter()

for(i in 2:145) {
    x <- x + shape_scatter(sonaes(time=EpochTime, pitch = colnames(speck)[i], pan=1,indx=1)) +
}

    x <- x +scale_pan_continuous(c(0,1)) +
    scale_time_continuous(c(0, 69)) +
    scale_pitch_continuous(c(6, 12))

sonsave(x,where='speck.wav',play=F)
