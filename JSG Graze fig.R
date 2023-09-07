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
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans) ; library(sjPlot); library(multcompView)

# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# setwd("F:/CSU/JSG 2022/8ft grazing")
setwd("/Volumes/No Doubles/CSU/JSG 2022/8ft grazing")

env<-read.csv("Means for proportions of mass.csv")

# env$Timepoint<-as.numeric(env$Timepoint)
env$Plot<-as.factor(env$Plot)

tiff(file = "JSG 2022 graze.tiff", height = 8, width = 10, res = 300, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,2.5,0.5,0.5)) 

xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,600), xlim=c(20,60))

rect(xleft = -10, xright = 100, ybottom = 300, ytop = 345, col = "lavender", bty = "n", border = F)
ablineclip(h=325,lwd = 3, lty = 3, col = "black")

rect(xleft = 34.5, xright = 35.5, ybottom = 0, ytop = 520, col = "grey52", bty = "n", border = F)
rect(xleft = 34.95, xright = 35.05, ybottom = 0, ytop = 520, col = "grey52", bty = "n", border = F)
rect(xleft = 25, xright = 45, ybottom = 505, ytop = 520, col = "grey52", bty = "n", border = F)
rect(xleft = 25, xright = 45, ybottom = 515, ytop = 518, col = "black", bty = "n", border = F)

# rect(xleft = -2, xright = 6, ybottom = -200, ytop = 1300, col = "honeydew", bty = "n", border = F)
# box()

dum<-subset(env, Treatment == "jltotal")
dat<-subset(dum, Plot == "1")
rect(56, 00, 58, mean(dat$ANPP..g..mean), col = "grey69", border = "white")

rect(56, 00, 58, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
# dum<-subset(env, Treatment == "endJL")
# dat<-subset(dum, Plot == "1")
ablineclip(v=57, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")
dum<-subset(env, Treatment == "JL")
dat<-subset(dum, Plot == "1")
rect(56, 00, 58, mean(dat$ANPP..g..mean), col = "grey69", border = "white")

rect(56, 00, 58, mean(dat$ANPP..g..mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=57, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")
# dum<-subset(env, Treatment == "endJL")
# dat<-subset(dum, Plot == "1")
# rect(56, 00, 58, mean(dat$ANPP..g..mean), col = "grey69", border = "white")
# rect(56, 00, 58, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
box()
dum<-subset(env, Treatment == "Jntotal")
dat<-subset(dum, Plot == "1")
rect(54, 00, 56, mean(dat$ANPP..g..mean), col = "grey69", border = "white")

rect(54, 00, 56, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
ablineclip(v=55, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")
dum<-subset(env, Treatment == "JN")
dat<-subset(dum, Plot == "1")
rect(54, 00, 56, mean(dat$ANPP..g..mean), col = "grey69", border = "white")

rect(54, 00, 56, mean(dat$ANPP..g..mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=55, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")
# dum<-subset(env, Treatment == "endJN")
# dat<-subset(dum, Plot == "1")
# rect(54, 00, 56, mean(dat$ANPP..g..mean), col = "grey69", border = "white")
# rect(54, 00, 56, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
box()
dum<-subset(env, Treatment == "end")
dat<-subset(dum, Plot == "1")
rect(58, 00, 60, mean(dat$ANPP..g..mean), col = "grey69", border = "white")
ablineclip(v=59, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")
########
dum<-subset(env, Treatment == "jltotal")
dat<-subset(dum, Plot == "2")
rect(44, 00, 46, mean(dat$ANPP..g..mean), col = "cornflowerblue", border = "white")

rect(44, 00, 46, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
ablineclip(v=45, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")
dum<-subset(env, Treatment == "JL")
dat<-subset(dum, Plot == "2")
rect(44, 00, 46, mean(dat$ANPP..g..mean), col = "cornflowerblue", border = "white")

rect(44, 00, 46, mean(dat$ANPP..g..mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=45.5, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")
# dum<-subset(env, Treatment == "endJL")
# dat<-subset(dum, Plot == "2")
# rect(44, 00, 46, mean(dat$ANPP..g..mean), col = "cornflowerblue", border = "white")
# rect(44, 00, 46, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
box()
dum<-subset(env, Treatment == "jntotal")
dat<-subset(dum, Plot == "2")
rect(42, 00, 44, mean(dat$ANPP..g..mean), col = "cornflowerblue", border = "white")
rect(42, 00, 44, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
ablineclip(v=43, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")

dum<-subset(env, Treatment == "JN")
dat<-subset(dum, Plot == "2")
rect(42, 00, 44, mean(dat$ANPP..g..mean), col = "cornflowerblue", border = "white")
rect(42, 00, 44, mean(dat$ANPP..g..mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=43, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")

# dum<-subset(env, Treatment == "endJN")
# dat<-subset(dum, Plot == "2")
# rect(42, 00, 44, mean(dat$ANPP..g..mean), col = "cornflowerblue", border = "white")
# rect(42, 00, 44, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
box()
dum<-subset(env, Treatment == "end")
dat<-subset(dum, Plot == "2")
rect(46, 00, 48, mean(dat$ANPP..g..mean), col = "cornflowerblue", border = "white")
ablineclip(v=47, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")

#########
dum<-subset(env, Treatment == "jltotal")
dat<-subset(dum, Plot == "3")
rect(34, 00, 36, mean(dat$ANPP..g..mean), col = "aquamarine3", border = "white")
rect(34, 00, 36, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
ablineclip(v=35, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")

dum<-subset(env, Treatment == "JL")
dat<-subset(dum, Plot == "3")
rect(34, 00, 36, mean(dat$ANPP..g..mean), col = "aquamarine3", border = "white")
rect(34, 00, 36, mean(dat$ANPP..g..mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=35.5, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")

# dum<-subset(env, Treatment == "endJL")
# dat<-subset(dum, Plot == "3")
# rect(34, 00, 36, mean(dat$ANPP..g..mean), col = "aquamarine3", border = "white")
# rect(34, 00, 36, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
box()
dum<-subset(env, Treatment == "jntotal")
dat<-subset(dum, Plot == "3")
rect(32, 00, 34, mean(dat$ANPP..g..mean), col = "aquamarine3", border = "white")
rect(32, 00, 34, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
ablineclip(v=33, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")

dum<-subset(env, Treatment == "JN")
dat<-subset(dum, Plot == "3")
rect(32, 00, 34, mean(dat$ANPP..g..mean), col = "aquamarine3", border = "white")
rect(32, 00, 34, mean(dat$ANPP..g..mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=33, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")

# dum<-subset(env, Treatment == "endJN")
# dat<-subset(dum, Plot == "3")
# rect(32, 00, 34, mean(dat$ANPP..g..mean), col = "aquamarine3", border = "white")
# rect(32, 00, 34, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
box()
dum<-subset(env, Treatment == "end")
dat<-subset(dum, Plot == "3")
rect(36, 00, 38, mean(dat$ANPP..g..mean), col = "aquamarine3", border = "white")
ablineclip(v=37, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")
box()
###########
dum<-subset(env, Treatment == "jltotal")
dat<-subset(dum, Plot == "4")
rect(24, 00, 26, mean(dat$ANPP..g..mean), col = "indianred4", border = "white")
rect(24, 00, 26, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
ablineclip(v=25, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")

dum<-subset(env, Treatment == "JL")
dat<-subset(dum, Plot == "4")
rect(24, 00, 26, mean(dat$ANPP..g..mean), col = "indianred4", border = "white")
rect(24, 00, 26, mean(dat$ANPP..g..mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=25, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")

# dum<-subset(env, Treatment == "endJL")
# dat<-subset(dum, Plot == "4")
# rect(24, 00, 26, mean(dat$ANPP..g..mean), col = "indianred4", border = "white")
# rect(24, 00, 26, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
box()
dum<-subset(env, Treatment == "jntotal")
dat<-subset(dum, Plot == "4")
rect(22, 00, 24, mean(dat$ANPP..g..mean), col = "indianred4", border = "white")
rect(22, 00, 24, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
ablineclip(v=23, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")

dum<-subset(env, Treatment == "JN")
dat<-subset(dum, Plot == "4")
rect(22, 00, 24, mean(dat$ANPP..g..mean), col = "indianred4", border = "white")
rect(22, 00, 24, mean(dat$ANPP..g..mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=23, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")

# dum<-subset(env, Treatment == "endJN")
# dat<-subset(dum, Plot == "4")
# rect(22, 00, 24, mean(dat$ANPP..g..mean), col = "indianred4", border = "white")
# rect(22, 00, 24, mean(dat$ANPP..g..mean), density = 20, lwd = 2, col = "white" , border = "white")
box()
dum<-subset(env, Treatment == "end")
dat<-subset(dum, Plot == "4")
rect(26, 00, 28, mean(dat$ANPP..g..mean), col = "indianred4", border = "white")
ablineclip(v=27, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "black")
box()
##############
# dum<-subset(env, Treatment == "end")
# dat<-subset(dum, Plot == "22")
# rect(02, 00, 04, mean(dat$ANPP..g..mean), col = "black", border = "white")
# ablineclip(v=3, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "grey69")
# box()


#####
#####
axis(2, at = seq(0,600,100), las = 2, cex.axis = 1.4)
mtext(side = 2, expression(ANPP~(g~m^-2)), cex = 2, padj = -1.75, outer= F)
mtext(side = 1, expression(Plot), cex = 2, padj = 1, outer= F)

legend("topleft", c("First sampling", "Regrowth"), density=c(0,20), cex = 1.5, horiz = F, bty='n')

legend("topright",c(expression(W [edge]),"Beneath" , expression(E [edge]), "Between"), col=c( "indianred4", "aquamarine3","cornflowerblue", "grey69"), pch= c(15) , cex = 1.5, horiz = F, bty='n')
legend("top",c("Control"), col=c("black"), cex = 1.5, lty = 3, lwd = 3, horiz = F, bty='n')


text(23, 120, expression('June'),cex=1.2, srt=90, col = "white")
text(25, 120, expression('July'),cex=1.2, srt=90, col = "white")
text(27, 120, expression('End of season'),cex=1.2, srt=90, col = "white")

text(33, 120, expression('June'),cex=1.2, srt=90, col = "white")
text(35, 120, expression('July'),cex=1.2, srt=90, col = "white")
text(37, 120, expression('End of season'),cex=1.2, srt=90, col = "white")

text(43, 120, expression('June'),cex=1.2, srt=90, col = "white")
text(45, 120, expression('July'),cex=1.2, srt=90, col = "white")
text(47, 120, expression('End of season'),cex=1.2, srt=90, col = "white")

text(55, 120, expression('June'),cex=1.2, srt=90, col = "white")
text(57, 120, expression('July'),cex=1.2, srt=90, col = "white")
text(59, 120, expression('End of season'),cex=1.2, srt=90, col = "white")


text(35, 530, expression('(Solar Panel)'),cex=1, col = "black")
# rect(xleft = -999, xright = 999, ybottom = -200, ytop = 0, col = "black", bty = "n", border = F)
# box()

dev.off()
##########
#########
#########
#######
########
tiff(file = "JSG 2022 graze N plots.tiff", height = 8, width = 10, res = 300, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,2.5,0.5,0.5)) 

xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,17), xlim=c(20,64))

rect(xleft = -10, xright = 100, ybottom = 9.1, ytop = 11.1, col = "lavender", bty = "n", border = F)
ablineclip(h=10.1,lwd = 3, lty = 3, col = "black")

# rect(xleft = 36.5, xright = 37.5, ybottom = 0, ytop = 17.8, col = "grey52", bty = "n", border = F)
# rect(xleft = 36.95, xright = 37.05, ybottom = 0, ytop = 17.8, col = "grey52", bty = "n", border = F)
# rect(xleft = 27, xright = 47, ybottom = 17.7, ytop = 17.75, col = "grey52", bty = "n", border = F)
# rect(xleft = 27, xright = 47, ybottom = 17.75, ytop = 17.85, col = "black", bty = "n", border = F)

# rect(xleft = -2, xright = 6, ybottom = -200, ytop = 1300, col = "honeydew", bty = "n", border = F)
# box()

dum<-subset(env, Treatment == "JL")
dat<-subset(dum, Plot == "1")
rect(56, 00, 58, mean(dat$CP.mean), col = "grey69", border = "white")

rect(56, 00, 58, mean(dat$CP.mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=57, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
# dum<-subset(env, Treatment == "endJL")
# dat<-subset(dum, Plot == "1")
# rect(56, 00, 58, mean(dat$CP.mean), col = "grey69", border = "white")
# rect(56, 00, 58, mean(dat$CP.mean), density = 20, lwd = 2, col = "white" , border = "white")
box()

dum<-subset(env, Treatment == "JN")
dat<-subset(dum, Plot == "1")
rect(54, 00, 56, mean(dat$CP.mean), col = "grey69", border = "white")
rect(54, 00, 56, mean(dat$CP.mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=55, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
# dum<-subset(env, Treatment == "endJN")
# dat<-subset(dum, Plot == "1")
# rect(54, 00, 56, mean(dat$CP.mean), col = "grey69", border = "white")
# rect(54, 00, 56, mean(dat$CP.mean), density = 20, lwd = 2, col = "white" , border = "white")
box()
dum<-subset(env, Treatment == "end")
dat<-subset(dum, Plot == "1")
rect(58, 00, 60, mean(dat$CP.mean), col = "grey69", border = "white")
ablineclip(v=59, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
box()

dum<-subset(env, Treatment == "endJN")
dat<-subset(dum, Plot == "1")
rect(60, 00, 61, mean(dat$CP.mean), col = "grey69", border = "white")
rect(60, 00, 61, mean(dat$CP.mean), density = 5, lwd = 2, col = "white" , border = "white")
ablineclip(v=60.5, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
box()

dum<-subset(env, Treatment == "endJL")
dat<-subset(dum, Plot == "1")
rect(61, 00, 62, mean(dat$CP.mean), col = "grey69", border = "white")
rect(61, 00, 62, mean(dat$CP.mean), density = 10, lwd = 2, col = "white" , border = "white")
ablineclip(v=61.5, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
box()
########

dum<-subset(env, Treatment == "JL")
dat<-subset(dum, Plot == "2")
rect(44, 00, 46, mean(dat$CP.mean), col = "cornflowerblue", border = "white")

rect(44, 00, 46, mean(dat$CP.mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=45, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
# dum<-subset(env, Treatment == "endJL")
# dat<-subset(dum, Plot == "2")
# rect(44, 00, 46, mean(dat$CP.mean), col = "cornflowerblue", border = "white")
# rect(44, 00, 46, mean(dat$CP.mean), density = 20, lwd = 2, col = "white" , border = "white")
box()


dum<-subset(env, Treatment == "JN")
dat<-subset(dum, Plot == "2")
rect(42, 00, 44, mean(dat$CP.mean), col = "cornflowerblue", border = "white")
rect(42, 00, 44, mean(dat$CP.mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=43, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
box()
# dum<-subset(env, Treatment == "endJN")
# dat<-subset(dum, Plot == "2")
# rect(42, 00, 44, mean(dat$CP.mean), col = "cornflowerblue", border = "white")
# rect(42, 00, 44, mean(dat$CP.mean), density = 20, lwd = 2, col = "white" , border = "white")

dum<-subset(env, Treatment == "end")
dat<-subset(dum, Plot == "2")
rect(46, 00, 48, mean(dat$CP.mean), col = "cornflowerblue", border = "white")
ablineclip(v=47, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
box()

dum<-subset(env, Treatment == "endJN")
dat<-subset(dum, Plot == "2")
rect(48, 00, 49, mean(dat$CP.mean), col = "cornflowerblue", border = "white")
rect(48, 00, 49, mean(dat$CP.mean), density = 5, lwd = 2, col = "white" , border = "white")
ablineclip(v=48.5, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
box()

dum<-subset(env, Treatment == "endJL")
dat<-subset(dum, Plot == "2")
rect(49, 00, 50, mean(dat$CP.mean), col = "cornflowerblue", border = "white")
rect(49, 00, 50, mean(dat$CP.mean), density = 10, lwd = 2, col = "white" , border = "white")
ablineclip(v=49.5, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
box()
#########


dum<-subset(env, Treatment == "JL")
dat<-subset(dum, Plot == "3")
rect(34, 00, 36, mean(dat$CP.mean), col = "aquamarine3", border = "white")
rect(34, 00, 36, mean(dat$CP.mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=35, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")

# dum<-subset(env, Treatment == "endJL")
# dat<-subset(dum, Plot == "3")
# rect(34, 00, 36, mean(dat$CP.mean), col = "aquamarine3", border = "white")
# rect(34, 00, 36, mean(dat$CP.mean), density = 20, lwd = 2, col = "white" , border = "white")
box()

dum<-subset(env, Treatment == "JN")
dat<-subset(dum, Plot == "3")
rect(32, 00, 34, mean(dat$CP.mean), col = "aquamarine3", border = "white")
rect(32, 00, 34, mean(dat$CP.mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=33, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")

# dum<-subset(env, Treatment == "endJN")
# dat<-subset(dum, Plot == "3")
# rect(32, 00, 34, mean(dat$CP.mean), col = "aquamarine3", border = "white")
# rect(32, 00, 34, mean(dat$CP.mean), density = 20, lwd = 2, col = "white" , border = "white")
box()
dum<-subset(env, Treatment == "end")
dat<-subset(dum, Plot == "3")
rect(36, 00, 38, mean(dat$CP.mean), col = "aquamarine3", border = "white")
ablineclip(v=37, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
box()

dum<-subset(env, Treatment == "endJN")
dat<-subset(dum, Plot == "3")
rect(38, 00, 39, mean(dat$CP.mean), col = "aquamarine3", border = "white")
rect(38, 00, 39, mean(dat$CP.mean), density = 5, lwd = 2, col = "white" , border = "white")
ablineclip(v=38.5, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
box()

dum<-subset(env, Treatment == "endJL")
dat<-subset(dum, Plot == "3")
rect(39, 00, 40, mean(dat$CP.mean), col = "aquamarine3", border = "white")
rect(39, 00, 40, mean(dat$CP.mean), density = 10, lwd = 2, col = "white" , border = "white")
ablineclip(v=39.5, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
box()
###########

dum<-subset(env, Treatment == "JL")
dat<-subset(dum, Plot == "4")
rect(24, 00, 26, mean(dat$CP.mean), col = "indianred4", border = "white")
rect(24, 00, 26, mean(dat$CP.mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=25, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")

# dum<-subset(env, Treatment == "endJL")
# dat<-subset(dum, Plot == "4")
# rect(24, 00, 26, mean(dat$CP.mean), col = "indianred4", border = "white")
# rect(24, 00, 26, mean(dat$CP.mean), density = 20, lwd = 2, col = "white" , border = "white")
box()

dum<-subset(env, Treatment == "JN")
dat<-subset(dum, Plot == "4")
rect(22, 00, 24, mean(dat$CP.mean), col = "indianred4", border = "white")
rect(22, 00, 24, mean(dat$CP.mean), density = 0, lwd = 2, col = "white" , border = "white")
ablineclip(v=23, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")

# dum<-subset(env, Treatment == "endJN")
# dat<-subset(dum, Plot == "4")
# rect(22, 00, 24, mean(dat$CP.mean), col = "indianred4", border = "white")
# rect(22, 00, 24, mean(dat$CP.mean), density = 20, lwd = 2, col = "white" , border = "white")
box()
dum<-subset(env, Treatment == "end")
dat<-subset(dum, Plot == "4")
rect(26, 00, 28, mean(dat$CP.mean), col = "indianred4", border = "white")
ablineclip(v=27, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
box()

dum<-subset(env, Treatment == "endJN")
dat<-subset(dum, Plot == "4")
rect(28, 00, 29, mean(dat$CP.mean), col = "indianred4", border = "white")
rect(28, 00, 29, mean(dat$CP.mean), density = 5, lwd = 2, col = "white" , border = "white")
ablineclip(v=28.5, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
box()

dum<-subset(env, Treatment == "endJL")
dat<-subset(dum, Plot == "4")
rect(29, 00, 30, mean(dat$CP.mean), col = "indianred4", border = "white")
rect(29, 00, 30, mean(dat$CP.mean), density = 10, lwd = 2, col = "white" , border = "white")
ablineclip(v=29.5, y1=as.numeric(dat$CP.mean) + (dat$CP.std.error), y2=as.numeric(dat$CP.mean) - (dat$CP.std.error),lwd = 2, col = "black")
box()
##############
dum<-subset(env, Treatment == "end")
dat<-subset(dum, Plot == "22")
# rect(02, 00, 04, mean(dat$CP.mean), col = "black", border = "white")
# ablineclip(v=3, y1=as.numeric(dat$ANPP..g..mean) + (dat$ANPP..g..std.error), y2=as.numeric(dat$ANPP..g..mean) - (dat$ANPP..g..std.error),lwd = 2, col = "grey69")
# box()


#####
#####
axis(2, at = seq(0,16,4), las = 2, cex.axis = 1.4)
mtext(side = 2, expression("% Crude Protein"), cex = 2, padj = -2.2, outer= F)
mtext(side = 1, expression(Plot), cex = 2, padj = 1, outer= F)

legend("topleft", c("June regrowth", "July regrowth"), density=c(10,20), cex = 1.5, horiz = F, bty='n')

legend("topright",c(expression(W [edge]),"Beneath" , expression(E [edge]), "Between"), col=c( "indianred4", "aquamarine3","cornflowerblue", "grey69"), pch= c(15) , cex = 1.5, horiz = F, bty='n')
legend("top",c("Control"), col=c("black"), cex = 1.5, lty = 3, lwd = 3, horiz = F, bty='n')


text(23, 2, expression('June'),cex=1.2, srt=90, col = "white")
text(25, 2, expression('July'),cex=1.2, srt=90, col = "white")
text(27, 2, expression('End of season'),cex=1.2, srt=90, col = "white")

text(33, 2, expression('June'),cex=1.2, srt=90, col = "white")
text(35, 2, expression('July'),cex=1.2, srt=90, col = "white")
text(37, 2, expression('End of season'),cex=1.2, srt=90, col = "white")

text(43, 2, expression('June'),cex=1.2, srt=90, col = "white")
text(45, 2, expression('July'),cex=1.2, srt=90, col = "white")
text(47, 2, expression('End of season'),cex=1.2, srt=90, col = "white")

text(55, 2, expression('June'),cex=1.2, srt=90, col = "white")
text(57, 2, expression('July'),cex=1.2, srt=90, col = "white")
text(59, 2, expression('End of season'),cex=1.2, srt=90, col = "white")


# text(35, 530, expression('(Solar Panel)'),cex=1, col = "black")
# rect(xleft = -999, xright = 999, ybottom = -200, ytop = 0, col = "black", bty = "n", border = F)
# box()

dev.off()


ANPP<-read.csv("Forage data to date 11-2-22.csv")
ANPP$Plot<-as.factor(ANPP$Plot)
firstsamp<-subset(ANPP, Sampling == "second" & Treatment == "endJL")
m1<-lm(ANPP..g.~Plot*Treatment, env)
anova(m1)
summary(m1)
plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Plot))


forage<-read.csv("forage data2.csv")
forage$Plot<-as.factor(forage$Plot)
dum<-subset(forage, Plot != "22" )
m1<-lm(X.CP~Plot*Treatment, forage)
anova(m1)
summary(m1)
plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Plot*Treatment))

##############################################################################

ANPP<-read.csv("Forage data to date 11-2-22.csv")
# ANPPN$CP.mean<-as.numeric(ANPPN$CP.mean)
# ANPPN$Plot<-as.factor(ANPPN$CP.mean)
# ANPPN$Treatment<-as.factor(ANPPN$Treatment)
dum<-subset(ANPPN, Treatment == "2202")
m1<-lm(CP.mean~Plot, dum)
anova(m1)
summary(m1)
plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Plot))
