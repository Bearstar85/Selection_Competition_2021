#Lets clear old objects
rm(list=ls())
getwd()
#and set the work directory in case we have moved around or opened another project
setwd("~/Documents/R/Competition_MS/03_EC")
dir()

#Load packages and data (not adapted)
library(reshape2)
library(multtest) 
library(lattice)
library(devtools)
library(ggbiplot)
library(plyr)
library(scales)
library(ggplot2)
library(devtools)
library(grid)
#library(gplots)
library(tidyverse)
library(lubridate) # useful for working with dates
library(cowplot) # useul for combining multiple plots
library(ggthemes)
library(broom)
library(ggpubr)
library(gridExtra)
#library(ggpmisc)
#Heatmap####'

#Figures

#Import data
myData2 <- read.csv2("ECvaluesAll_MStall.csv", header=TRUE)
?read.csv2
head(myData2)
myData2
sapply(myData2, class)

#Make some list for grepping data

#Change relevant data to numeric 
for(i in c(6:10: ncol(myData2))) {
  myData2[,i] <- as.numeric(as.character(myData2[,i]))
}

myData2$Strain <- as.character(as.factor(myData2$Strain))

sapply(myData2, class)
#Works!!!

#For plotting (log scales) we will change any negative error values to small possitive (0.0001) 

myData2$X95L <- (ifelse(myData2$X95L < 0, 0.0001, myData2$X95L))

#Works great

#Lets grep Ag, Cd and Cu for stats
Ag2 <- subset.data.frame(myData2, grepl("Ag", myData2$Metal))
Cd2 <- subset.data.frame(myData2, grepl("Cd", myData2$Metal))
Cu2 <- subset.data.frame(myData2, grepl("Cu", myData2$Metal))
Ag2
#for TB_11 EC95 is extrapolated hevaly and schould probably be omitted for this reason
# I also wounder why the exported confident intervalls differ from the plots and function (never negative in plots)? 
#Mainly problematic for poorly fitted curves, such as TB_11, or for EC05 

Fig2.A <- ggplot(Ag2, aes(Strain, Conc)) +
  geom_point(mapping = aes(color = EC, shape = Species, size = Species, fill = EC), size = 3, position = position_dodge(0.3)) +
  scale_color_manual(values=c("grey", "orange", "Red"), aesthetics = c("colour", "fill")) +
  scale_shape_manual(values=c(1, 2)) +
  geom_hline(yintercept = 0.046, linetype="dashed") +
  #grid.force() +  # To make the grobs visible to grid editing tools
  #grid.edit("geom_point.points", grep = TRUE, gp = gpar(lwd = 10)) + #lets see if this adjustes symbols line thickness
  #scale_size_manual(values=c(3, 3)) + #Size of symbols
  #scale_linetype_manual(values = 2) +
  geom_errorbar(aes(ymin = X95L, ymax = X95H, width=.2, color = EC), position = position_dodge(0.3)) +
  scale_y_continuous(trans = "log10") + #change the scale on y axis
  #annotation_logticks(sides = "l") + # adds linier tickmarks
  coord_cartesian(ylim=c(0.001,1), expand = F) + #changes the y axis
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #adjust text tilt and possition
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  #scale_fill_manual(values=c("#008000", "#2846FF", "#000000")) +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines 
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x="Strain", y=expression("Concentration"~(mu*M)), title = "A) Ag") +
  theme(plot.title = element_text(vjust = - 18, hjust = 0.02)) +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(legend.title=element_blank(), panel.background = element_blank()) +
  theme(legend.text=element_text(size=15)) +
  theme(text=(element_text(size=15))) +
  theme(axis.text=(element_text(size=15))) +
  theme(legend.text = element_text(face = "italic")) +
  theme(aspect.ratio=0.33) +
  #stat_cor(aes(color = scientific_name), label.x = 3) +
  theme(legend.position = c("top"),
        #legend.justification = c("right", "top"),
        #legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2),
        legend.box.background = element_rect(fill='white'),
        legend.background = element_blank(),
        legend.spacing.x=unit(0, "cm"),
        legend.spacing.y=unit(0, "cm"))


print(Fig2.A)

dev.copy(pdf, "Fig2A.pdf")
dev.off()

Fig2.B <- ggplot(Cd2, aes(Strain, Conc)) +
  geom_point(mapping = aes(color = EC, shape = Species, size = Species, fill = EC), size = 3, position = position_dodge(0.3)) +
  scale_color_manual(values=c("grey", "orange", "Red"), aesthetics = c("colour", "fill")) +
  scale_shape_manual(values=c(1, 2)) +
  geom_hline(yintercept = 5.509, linetype="dashed") +
  #grid.force() +  # To make the grobs visible to grid editing tools
  #grid.edit("geom_point.points", grep = TRUE, gp = gpar(lwd = 10)) + #lets see if this adjustes symbols line thickness
  #scale_size_manual(values=c(3, 3)) + #Size of symbols
  #scale_linetype_manual(values = 2) +
  geom_errorbar(aes(ymin = X95L, ymax = X95H, width=.2, color = EC), position = position_dodge(0.3)) +
  scale_y_continuous(trans = "log2") + #change the scale on y axis
  #annotation_logticks(sides = "l") + # adds linier tickmarks
  coord_cartesian(ylim=c(1,100), expand = F) + #changes the y axis
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #adjust text tilt and possition
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  #scale_fill_manual(values=c("#008000", "#2846FF", "#000000")) +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines 
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x="Strain", y=expression("Concentration"~(mu*M)), title = "B) Cd") +
  theme(plot.title = element_text(vjust = - 8, hjust = 0.02)) +
  theme(panel.spacing = unit(0.1, "lines")) +
  #theme(legend.title=element_blank()) +
  #theme(legend.text=element_text(size=10)) +
  theme(text=(element_text(size=15))) +
  theme(axis.text=(element_text(size=15))) +
  #theme(legend.text = element_text(face = "italic")) +
  theme(aspect.ratio=0.33) +
  #stat_cor(aes(color = scientific_name), label.x = 3) +
  theme(legend.position = "none", panel.background = element_blank())

print(Fig2.B)

dev.copy(pdf, "Fig2B.pdf")
dev.off()

Fig2.C <- ggplot(Cu2, aes(Strain, Conc)) +
  geom_point(mapping = aes(color = EC, shape = Species, size = Species, fill = EC), size = 3, position = position_dodge(0.3)) +
  scale_color_manual(values=c("grey", "orange", "Red"), aesthetics = c("colour", "fill")) +
  scale_shape_manual(values=c(1, 2)) +
  geom_hline(yintercept = 9.725, linetype="dashed") +
  #grid.force() +  # To make the grobs visible to grid editing tools
  #grid.edit("geom_point.points", grep = TRUE, gp = gpar(lwd = 10)) + #lets see if this adjustes symbols line thickness
  #scale_size_manual(values=c(3, 3)) + #Size of symbols
  #scale_linetype_manual(values = 2) +
  geom_errorbar(aes(ymin = X95L, ymax = X95H, width=.2, color = EC), position = position_dodge(0.3)) +
  scale_y_continuous(trans = "log2") + #change the scale on y axis
  #annotation_logticks(sides = "l") + # adds linier tickmarks
  coord_cartesian(ylim=c(4,16), expand = F) + #changes the y axis
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) + #adjust text tilt and possition
  #scale_fill_manual(values=c("#008000", "#2846FF", "#000000")) +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines
  #theme_classic() +
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x="Strain", y=expression("Concentration"~(mu*M)), title = "C) Cu") +
  theme(plot.title = element_text(vjust = - 8, hjust = 0.02)) +
  theme(panel.spacing = unit(0.1, "lines")) +
  #theme(legend.title=element_blank()) +
  #theme(legend.text=element_text(size=10)) +
  theme(text=(element_text(size=15))) +
  theme(axis.text=(element_text(size=15))) +
  #theme(legend.text = element_text(face = "italic")) +
  theme(aspect.ratio=0.33) +
  #stat_cor(aes(color = scientific_name), label.x = 3) +
  theme(legend.position = "none", panel.background = element_blank())

print(Fig2.C)

dev.copy(pdf, "Fig2C.pdf")
dev.off()

#MAke combined figure and export (very difficult to align figures so I gave up)
ggarrange(Fig2.A, Fig2.B, Fig2.C,  ncol = 1, nrow = 3, heights = c(4, 4, 4), align = "hv")

ggarrange(Fig2.A, Fig2.B, Fig2.C)
plot_grid(Fig2.A, Fig2.B, Fig2.C, align="v", ncol=1)

dev.copy(pdf, "Fig2.pdf")
dev.off()

widths = c(10, 10, 10)
Fig2.Zn

Zn2 <- subset.data.frame(myData2, grepl("Zn", myData2$Metal))

Fig2.Zn <- ggplot(Zn2, aes(Strain, Conc)) +
  geom_point(mapping = aes(color = EC, shape = Species, size = Species, fill = EC), size = 3, position = position_dodge(0.3)) +
  scale_color_manual(values=c("grey", "orange", "Red"), aesthetics = c("colour", "fill")) +
  scale_shape_manual(values=c(1, 2)) +
  #geom_hline(yintercept = 9.725) +
  #grid.force() +  # To make the grobs visible to grid editing tools
  #grid.edit("geom_point.points", grep = TRUE, gp = gpar(lwd = 10)) + #lets see if this adjustes symbols line thickness
  #scale_size_manual(values=c(3, 3)) + #Size of symbols
  #scale_linetype_manual(values = 2) +
  geom_errorbar(aes(ymin = X95L, ymax = X95H, width=.2, color = EC), position = position_dodge(0.3)) +
  scale_y_continuous(trans = "log2") + #change the scale on y axis
  #annotation_logticks(sides = "l") + # adds linier tickmarks
  coord_cartesian(ylim=c(4,16), expand = F) + #changes the y axis
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #adjust text tilt and possition
  #scale_fill_manual(values=c("#008000", "#2846FF", "#000000")) +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines
  #theme_classic() +
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x="Strain", y=expression("Zn concentration"~(mu*M)), title = "C") +
  theme(plot.title = element_text(vjust = - 8, hjust = 0.02)) +
  theme(panel.spacing = unit(0.1, "lines")) +
  #theme(legend.title=element_blank()) +
  #theme(legend.text=element_text(size=10)) +
  theme(text=(element_text(size=12))) +
  theme(axis.text=(element_text(size=10))) +
  #theme(legend.text = element_text(face = "italic")) +
  theme(aspect.ratio=0.33) +
  #stat_cor(aes(color = scientific_name), label.x = 3) +
  theme(legend.position = "none", panel.background = element_blank())

print(Fig2.Zn)

dev.copy(pdf, "FigZn.pdf")
dev.off()