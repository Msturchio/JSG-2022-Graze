library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(nlme)
library(MuMIn); library(car)
library(sjPlot)
library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(plotrix)

library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans)

library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans) ; library(sjPlot)

# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# setwd("F:/CSU/JSG 2022/8ft grazing")
setwd("/Volumes/No Doubles/CSU/JSG 2022/8ft grazing")

env<-read.csv("Grazing EOS analysis.csv")

# env$Timepoint<-as.numeric(env$Timepoint)
# env$Plot<-as.factor(env$Plot)

tiff(file = "Grazing totals.tiff", height = 8, width = 10, res = 300, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,2.5,0.5,0.5)) 

xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,400), xlim=c(0.5,4.5))

#80cm for edges, 80 cm underneath, 280 between, 520 total
rect(xleft = -999, xright = 999, ybottom = 300, ytop = 350, col = "lavender", bty = "n", border = F)
abline(h=325, lty=3, lwd = 3)
box()
dum<-subset(env, Grazed == "JN")
dat<-subset(dum)
rect(02, 00, 03, mean(dat$ANPP..g..mean), density = 10, lwd = 3, col = "grey69" , border = "black")
ablineclip(v=2.5, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 3, col = "black")
box()

dum<-subset(env, Grazed == "JL")
dat<-subset(dum)
rect(02, 00, 03, mean(dat$ANPP..g..mean), density = 10, lwd = 3, col = "grey88" , border = "black")
ablineclip(v=2.5, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 3, col = "black")
box()

dum<-subset(env, Grazed == "N")
dat<-subset(dum)
rect(01, 00, 02, mean(dat$ANPP..g..mean), density = 10000, lwd = 3, col = "white" , border = "black")
ablineclip(v=1.5, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 3, col = "black")
box()


#####
#####
axis(2, cex.axis = 1.5, labels = T)
mtext(side = 2, expression(ANPP~(g~m^-2)), cex = 2, padj = -1.75, outer= F)
mtext(side = 1, expression(Plot), cex = 2, padj = 1.5, outer= F)

# mtext(side = 1, expression("June+re-clip  July+re-clip  End of Season"), cex = 1, padj = 1, outer= F)
# 
# legend("topright", c("First Clip", "Re-clip"), density=c(0,20), col = c("black","grey"), cex = 1.5, horiz = F, bty='n')
# 
# legend("topleft", c("June","July","EOS"), density=c(20,20,0), pch= c(15) , col=c("grey69","black","black"), cex = 1.5, horiz = F, bty='n')

text(1.5, 80, expression('June'),cex=2, col = "black")
text(1.5, 50, expression('+'),cex=3, col = "black")
text(1.5, 20, expression('regrowth'),cex=2, col = "black")

text(2.5, 80, expression('July'),cex=2, col = "black")
text(2.5, 50, expression('+'),cex=3, col = "black")
text(2.5, 20, expression('regrowth'),cex=2, col = "black")

text(3.5, 80, expression('End of'),cex=2, col = "black")
text(3.5, 50, expression('season'),cex=2, col = "black")
text(3.5, 20, expression('only'),cex=2, col = "black")
# 
# text(33, 120, expression('June'),cex=0.75, srt=90, col = "white")
# text(35, 120, expression('July'),cex=0.75, srt=90, col = "white")
# text(37, 120, expression('End of season'),cex=0.75, srt=90, col = "white")
# 
# text(43, 120, expression('June'),cex=0.75, srt=90, col = "white")
# text(45, 120, expression('July'),cex=0.75, srt=90, col = "white")
# text(47, 120, expression('End of season'),cex=0.75, srt=90, col = "white")
# 
# text(55, 120, expression('June'),cex=0.75, srt=90, col = "white")
# text(57, 120, expression('July'),cex=0.75, srt=90, col = "white")
# text(59, 120, expression('End of season'),cex=0.75, srt=90, col = "white")
# 
# 
# text(35, 585, expression('(Solar Panel)'),cex=1, col = "black")
# rect(xleft = -999, xright = 999, ybottom = -200, ytop = 0, col = "black", bty = "n", border = F)
# box()

dev.off()
